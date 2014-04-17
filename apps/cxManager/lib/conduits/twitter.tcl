##################
## Module Name     --  twitter.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##     This is a conduit that receives objects updates and will
##     automatically post status updates at twitter.
##
## Commands Exported:
##      cmd1
##      cmd2
##################


# Implementation notes.
#
# The current implementation is bound to a twitter feed, this could be
# extended by implemented completely the OAuth authentication scheme.
# Example code for doing this is at http://wiki.tcl.tk/27607


#package require tls;  # will be done dynamically instead
package require oauth
package require http
package require uuid

namespace eval ::twitter {
    variable TWT
    if {![info exists TWT] } {
	array set TWT {
	    context          ""
	    servers          {}
	    initdone         0
	    -consumer_key    ""
	    -consumer_secret ""
	    -access_token    ""
	    -access_secret   ""
	    -timeout         10000
	    apiroot          "https://api.twitter.com/1"
	}
	::uobj::install_log twitter TWT
	::uobj::install_defaults twitter TWT
    }
}


proc ::twitter::__report { url token } {
    variable TWT
    variable log

    if { [::http::ncode $token] == 200 } {
	${log}::debug "Success in posting twitter status via $url"
    } else {
	${log}::warn "Could not post twitter status via $url: error is\
                      '[::http::error $token]', status is\
                      '[::http::status $token]', data is\
                      '[::http::data $token]'"
    }
    ::http::cleanup $token
}


proc ::twitter::tweet { str } {
    variable TWT
    variable log

    ${log}::info "Setting twitter status to '$str'"

    set url [string trimright $TWT(apiroot) "/"]/statuses/update.json

    dict set req oauth_consumer_key $TWT(-consumer_key)
    dict set req oauth_token $TWT(-access_token)
    dict set req status $str
    set oauth [oauth auth POST $url $req $TWT(-access_secret)]
    set cmd [list http::geturl $url \
		 -headers [list Authorization $oauth] \
		 -query [http::formatQuery status $str] \
		 -command [list ::twitter::__report $url]]
    if { $TWT(-timeout) > 0 } {
	lappend cmd -timeout $TWT(-timeout)
    }
    if { [catch {eval $cmd} token] } {
	${log}::error "Error while sending status to twitter: $token"
    }
}


proc ::twitter::rest:tweet { prt sock url qry } {
    variable TWT
    variable log

    array set RESULTS {
	true     "\{\"result\":true\}"
	false     "\{\"result\":false\}"
    }

    if { [dict keys $qry uuid] ne {} } {
	set r [::uobj::find [namespace current] reporter \
		   [list uuid == [dict get $qry uuid]]]
	if { $r ne "" } {
	    upvar \#0 $r RPT

	    upvar \#0 $RPT(object) OBJ
	    set map {}
	    foreach k [array names OBJ -*] {
		set k [string trimleft $k "-"]
		lappend map "%$k%" $OBJ(-$k)
	    }
	    ::twitter::tweet [string map $map $RPT(-status)]
	    return $RESULTS(true)
	}
    }
    return $RESULTS(false)

}


proc ::twitter::to_json { r } {
    variable TWT
    variable log

    if { ![::uobj::isa $r reporter] } {
	${log}::warn "$r is unknown or has wrong type"
	return -code error "$r i unknown or of wrong type"
    }
    
    upvar \#0 $r RPT
    upvar \#0 $RPT(object) OBJ
    upvar \#0 $RPT(trigger) TRIGGER
    set result "\{"
    foreach k {uuid -status} {
	append result "\"[string trimleft $k -]\":\"$RPT($k)\","
    }
    append result "\"object\":\"$OBJ(uuid)\","
    append result "\"trigger\":\"$TRIGGER(uuid)\"";  # Ugly but works...
    append result "\}"

    return $result
}


proc ::twitter::rest:reporters { prt sock url qry } {
    variable TWT
    variable log

    if { [dict keys $qry uuid] ne {} } {
	set r [::uobj::find [namespace current] reporter \
		   [list uuid == [dict get $qry uuid]]]
	if { $r eq "" } {
	    return "\{\}"
	} else {
	    return [to_json $r]
	}
    } else {
	set result "\["
	foreach r [::uobj::allof [namespace current] reporter] {
	    append result [to_json $r],
	}
	set result [string trimright $result ,]
	append result "\]"

	return $result
    }
}


proc ::twitter::rest:listen { prt sock url qry } {
    variable TWT
    variable log

    set hex "\[a-f0-9\]"
    set uuid_filter "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]"

    set uuid ""
    if { [dict keys $qry uuid] ne {} } {
	set uuid [dict get $qry uuid]
    } else {
	set uuid [lindex [split [string trimright $url "/"] "/"] end-1]
	if { ! [string match -nocase $uuid_filter $uuid] } { 
	    set uuid [lindex [split [string trimright $url "/"] "/"] end]
	    if { ! [string match -nocase $uuid_filter $uuid] } {
		set uuid ""
	    } else {
		set r [::uobj::find [namespace current] reporter \
			   [list uuid == $uuid]]
		if { $r ne "" } {
		    return [rest:reporters $prt $sock $url \
				[list uuid $uuid]]
		}
	    }
	}
    }
    
    if { $uuid ne "" && [dict keys $qry status] ne {} } {
	foreach {o c} [::find:uuid $uuid "object"] break
	if { $o ne "" } {
	    set r [::uobj::new [namespace current] reporter]
	    upvar \#0 $r RPT
	    set RPT(id) $r
	    set RPT(uuid) [::uuid::uuid generate]
	    set RPT(object) $o
	    set RPT(-status) [dict get $qry status]
	    foreach {srv root} $TWT(servers) {
		if { $srv == $prt } {
		    # Create a receiver URL, i.e. the entry point for
		    # what we have registered as /tweet under the root
		    # of the conduit.
		    set rcv \
			[::minihttpd::fullurl $srv][string trim $root /]/tweet?
		    ${log}::debug "Arranging for a trigger to call us back at\
                                   [string trimright $rcv ?] whenever $uuid\
                                   has changed"
		    append rcv uuid=$RPT(uuid)
		    # Build a query that is compatible with the API of
		    # trigger creation.  This is a bit ugly and we
		    # might want to change the API for triggers some
		    # day.
		    set tqry [list method GET \
				  receiver $rcv]
		    if { [dict keys $qry field] ne {} } {
			lappend tqry field [dict get $qry field]
		    }
		    # Create trigger and return a description of the
		    # reporter object.
		    set RPT(trigger) [::trigger:new $o $tqry]
		    return [to_json $r]
		}
	    }
	}
    }
    return "\{\}"
}


proc ::twitter::__init {} {
    variable TWT
    variable log

    if { ! $TWT(initdone) } {
	${log}::debug "First time initialisation of twitter conduit"
	if { [catch {package require tls} err] } {
	    ${log}::error "Will not be able to communicate securely!\
                           (reason: $err)"
	    # Force simple HTTP access to twitter, not recommended but
	    # no choice...
	    set TWT(apiroot) [string map [list "https:/" "http:/"] \
				  $TWT(apiroot)]
	} else {
	    ::http::register https 443 [list ::tls::socket]
	}
	
	oauth secret $TWT(-consumer_secret)
	set TWT(initdone) 1
    }
}


proc ::twitter::init { cx root srv } {
    variable TWT
    variable log

    __init

    set TWT(context) $cx
    lappend TWT(servers) $srv $root

    set hex "\[a-fA-F0-9\]"
    set uuid "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]" 

    set root [string trimright $root "/"]
    ::minihttpd::handler $srv $root/$uuid ::twitter::rest:listen \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/listen ::twitter::rest:listen \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/listen/ ::twitter::rest:listen \
	"application/json"
    ::minihttpd::handler $srv $root/tweet ::twitter::rest:tweet \
	"application/json"
    ::minihttpd::handler $srv $root ::twitter::rest:reporters \
	"application/json"
    ::minihttpd::handler $srv $root/ ::twitter::rest:reporters \
	"application/json"
    ${log}::notice "Registered REsT entry points in server at port #$srv"
}