package require uobj
package require http
package require uri

namespace eval ::cxapi {
    variable API
    if { ![info exists API] } {
	array set API {
	    -host      localhost
	    -port      8802
	    -proto     http
	    -username  ""
	    -password  ""
	    -url       ""
	    -timeout   10000
	    tls        ""
	    dft_ports  "http 8802 https 8800"
	}
	variable version 0.1
	variable libdir [file dirname [file normalize [info script]]]
	::uobj::install_log cxapi API
	::uobj::install_defaults cxapi API
    }
}


proc ::cxapi::__receive { r token } {
    variable API
    variable log

    if { ![::uobj::isa $r remote] } {
	return -code error "$r unknown or wrong type"
    }
    upvar \#0 $r REMOTE
    upvar \#0 $REMOTE(context) CXT
    
    if { [::http::ncode $token] == 200 } {
	set REMOTE(result) [::http::data $token]
	${log}::debug "Received data: $REMOTE(result)"
    } else {
	set REMOTE(result) ""
	${log}::warn "Could not get $REMOTE(url): error is\
                      '[::http::error $token]' status is\
                      '[::http::status $token]', HTTP code was:\
                      [::http::ncode $token]"
    }
    ::http::cleanup $token

    if { $REMOTE(command) eq "" } {
	set REMOTE(done) 1;    # Unblock the caller
    } else {
	# Don't do callbacks if the command is a dash, which allows to
	# asynchronously call without expecting results.
	if { [string trim $REMOTE(command) -] ne "" } {
	    if { [catch {eval [linsert $REMOTE(command) end $REMOTE(context) \
				   $REMOTE(result)]} err] } {
		${log}::warn "Could not deliver callback $REMOTE(command)\
                              for invocation of $REMOTE(url): $err"
	    }
	}
	::uobj::delete $r;  # Get rid of object here, we are done!
    }
}


proc ::cxapi::call { cx root { uuid "" } { op "" } { qry "" } { cmd "" } } {
    variable API
    variable log

    if { ![::uobj::isa $cx context] } {
	return -code error "$cx unkown or wrong type"
    }
    upvar \#0 $cx CXT

    set url [string trimright $CXT(root) "/"]/[string trimleft $root "/"]
    if { $uuid ne "" } {
	append url "/" [string trim $uuid]
    }
    if { $op ne "" } {
	append url "/" [string trim $op "/"]
    }
    if { [llength $qry] > 0 } {
	append url "?" [eval [linsert $qry 0 ::http::formatQuery]]
    }
    array set URL [::uri::split $url]

    # Arrange for password encryption in the geturl command
    set hdrs [list]
    if { [array names URL user] ne "" \
	     && $URL(user) ne "" && $URL(pwd) ne "" } {
	set auth [::base64::encode "$URL(user):$URL(pwd)"]
	lappend hdrs Authorization "Basic $auth"
    }

    # Create command for getting the URL, don't forget the timeout
    if { $uuid eq "" } {
	${log}::notice "Executing operation $op in context\
                        manager at $CXT(root)"
    } else {
	${log}::notice "Executing operation $op for object $uuid in context\
                        manager at $CXT(root)"
    }
    set r [::uobj::new [namespace current] remote [::uobj::id $cx]]
    upvar \#0 $r REMOTE
    set REMOTE(self) $r
    set REMOTE(command) $cmd
    set REMOTE(context) $cx
    set REMOTE(done) 0
    set REMOTE(url) $url
    set REMOTE(result) ""
    set gcmd [list ::http::geturl $url \
		  -headers $hdrs \
		  -command [list [namespace current]::__receive $r]]
    if { $CXT(-timeout) > 0 } {
	lappend gcmd -timeout $CXT(-timeout)
    }

    # Evaluate and return, make sure we clean things up.
    set data ""
    if { [catch {eval $gcmd} token] } {
	${log}::error "Error while getting URL at $url: $token"
	::uobj::delete $r
	return ""
    }
    # Block if no command specified
    if { $cmd eq "" } {
	vwait ${r}(done)
	set retval $REMOTE(result) 
	::uobj::delete $r
    } else {
	set retval $token
    }

    return $retval
}


proc ::cxapi::context { cx uuid { op "" } { qry "" } { cmd "" } } {
    return [::cxapi::call $cx context $uuid $op $qry $cmd]
}
proc ::cxapi::trigger { cx uuid { op "" } { qry "" } { cmd "" } } {
    return [::cxapi::call $cx trigger $uuid $op $qry $cmd]
}
proc ::cxapi::pachube { cx uuid { op "" } { qry "" } { cmd "" } } {
    return [::cxapi::call $cx conduits/cosm $uuid $op $qry $cmd]
}
proc ::cxapi::remote { cx uuid { op "" } { qry "" } { cmd "" } } {
    return [::cxapi::call $cx conduits/rcontext $uuid $op $qry $cmd]
}
proc ::cxapi::gcal { cx uuid { op "" } { qry "" } { cmd "" } } {
    return [::cxapi::call $cx conduits/gcal $uuid $op $qry $cmd]
}
proc ::cxapi::UPnP { cx uuid { op "" } { qry "" } { cmd "" } } {
    return [::cxapi::call $cx conduits/upnp $uuid $op $qry $cmd]
}


proc ::cxapi::config { cx args } {
    variable API
    variable log

    if { ![::uobj::isa $cx context] } {
	return -code error "$cx unkown or wrong type"
    }
    upvar \#0 $cx CXT

    ::uobj::inherit CXT OLD
    set result [eval ::uobj::config CXT "-*" $args]

    # If we have specified a -url in the option, the specification of
    # the URL takes over all other URL-related options of the object.
    # Override them with the ones coming from the URL.
    if { $CXT(-url) ne "" } {
	if { [catch {::uri::split $CXT(-url)} uinfo] } {
	    return -code error "URL $CXT(-url) is not valid: $uinfo"
	} else {
	    array set URL $uinfo
	    foreach {part key} [list \
				    scheme proto \
				    host host \
				    port port \
				    user username \
				    pwd password] {
		set CXT(-$key) $URL($part)
	    }
	    # Make sure we have a default port that matches the
	    # protocol to use.
	    if { $CXT(-port) eq "" } {
		foreach {proto port} $API(dft_ports) {
		    if { $proto eq $CXT(-proto) } {
			set CXT(-port) $port
		    }
		}
	    }
	}
    }

    # Deliver an error if we cannot do HTTPS
    if { $CXT(-proto) eq "https" && [string is false $API(tls)] } {
	return -code error "Cannot support https calls, could not find\
                                tls package"
    }

    # Now construct the root URL of the remote context manager based
    # on the parameters.  We should really add some sanity check
    # around the parameters here.
    if { $CXT(-username) ne "" || $CXT(-password) ne "" } {
	set CXT(root) $CXT(-proto)://$CXT(-username):$CXT(-password)@
    } else {
	set CXT(root) $CXT(-proto)://
    }
    append CXT(root) $CXT(-host)
    # When no port specified, use a default port that depends on the
    # protocol used.
    if { $CXT(-port) eq "" } {
	foreach {proto port} $API(dft_ports) {
	    if { $proto eq $CXT(-proto) } {
		set CXT(-port) $port
	    }
	}
    }
    append CXT(root) :$CXT(-port)/
}


proc ::cxapi::wsroot { cx } {
    variable API
    variable log

    if { ![::uobj::isa $cx context] } {
	return -code error "$cx unkown or wrong type"
    }
    upvar \#0 $cx CXT

    # Find the scheme and replace http by ws, to make this a viable
    # websocket URL if necessary.
    set root ""
    set colon [string first ":" $CXT(root)]
    set scheme [string range $CXT(root) 0 [expr {$colon-1}]]
    switch $scheme {
	"http" {
	    set root "ws:[string range $CXT(root) [expr {$colon+1}] end]"
	}
	"https" {
	    set root "wss:[string range $CXT(root) [expr {$colon+1}] end]"
	}
	"wss" -
	"ws" {
	    set root $CXT(root)
	}
    }

    # Make sure to append context after the root if not already
    # present.
    if { $root ne "" } {
	set root [string trimright $root "/"]
	if { [string range $root end-6 end] ne "context" } {
	    append root "/context"
	}
	return $root;  # We usually exit here, with a cool clean WS URL...
    }
    return ""
}


proc ::cxapi::__init {} {
    variable API
    variable log

    if { [catch {package require tls} ver] } {
	${log}::warn "No tls available, will default to unencrypted\
                      communication"
	set API(tls) 0
    } else {
	# Register TLS for socket connections on https and make sure
	# we have encrypted defaults.
	::http::register https 443 [list ::tls::socket]
	foreach {proto port} $API(dft_ports) {
	    if { $proto eq "https" } {
		defaults -port $port -proto https
	    }
	}
	set API(tls) 1
    }
}

proc ::cxapi::find { { ptn *:* } } {
    variable API
    variable log

    if { [string range $ptn 0 3] eq "http" } {
	set host "";  set port ""
	if { [catch {::uri::split $ptn} uinfo] } {
	    return -code error "URL $ptn is not valid: $uinfo"
	}
	foreach {k v} $uinfo {
	    if { $k eq "host" } { set host $v }
	    if { $k eq "port" } { set port $v }
	}
    } else {
	if { [string first ":" $ptn] >= 0 } {
	    foreach { host port } [split $ptn ":"] break
	} else {
	    set host $ptn
	}
    }
    if { $host eq "" } { set host * }
    if { $port eq "" } { set port * }

    set found {}
    foreach cx [::uobj::allof [namespace current] context] {
	upvar \#0 $cx CXT
	if { [string match -nocase $host $CXT(-host)] \
		 && [string match -nocase $port $CXT(-port)] } {
	    lappend found $cx
	}
    }

    return $found
}


proc ::cxapi::new { args } {
    variable API
    variable log

    if { $API(tls) eq "" } {
	__init
    }

    set cx [::uobj::new [namespace current] context]
    upvar \#0 $cx CXT

    set CXT(self) $cx;      # Ourselves
    set CXT(root) "";       # The root URL of the remote context
    
    ::uobj::inherit API CXT
    ::uobj::objectify $cx [list [list config configure] call context \
			       [list pachube cosm] remote gcal UPnP trigger \
			       wsroot]

    eval config $cx $args

    return $cx
}


package provide cxapi $::cxapi::version
