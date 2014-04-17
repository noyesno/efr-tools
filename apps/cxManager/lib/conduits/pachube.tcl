##################
## Module Name     --  pachube.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    This is a conduit designed at receiving pachube-formatted data
##    and dispatching in onto objects of the context.
##
## Commands Exported:
##      cmd1
##      cmd2
##################

#package require tls;  # Will be done dynamically instead
package require http
package require uuid
package require rest

namespace eval ::pachube {
    variable PACHUBE
    if {![info exists PACHUBE] } {
	array set PACHUBE {
	    context        ""
	    servers        {}
	    initdone       0
	    apiroot        "https://api.cosm.com/v2"
	    -frequency     180
	    -timeout       10000
	}
	::uobj::install_log pachube PACHUBE
    }
}

# XXX: Rewrite pachube posting so that it does not depend on the REST
# package, this is mostly needed to make sure we can submit the
# changes to pachube in a non blocking way, and get data from there
# similarily.  (we will also get rid of a number of dependencies that
# we don't really use at the same time, i.e. json and tdom)


# Synchronise content of pair with the data that is really available
# at the remote servers, i.e. both within the datastreams of the
# pachube feed and within the field of the context object.
proc ::pachube::pair:sync { p } {
    variable PACHUBE
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    set uuid ""; set feed ""
    switch $PAIR(-destination) {
	pachube {
	    set uuid $PAIR(-from)
	    set feed $PAIR(-to)
	}
	context {
	    set uuid $PAIR(-to)
	    set feed $PAIR(-from)
	}
    }

    set PAIR(fields:context) {}
    set PAIR(fields:pachube) {}
    if { $uuid ne "" } {
	upvar \#0 $PAIR(object) OBJ
	foreach f [array names OBJ -*] {
	    lappend PAIR(fields:context) [string trimleft $f "-"]
	}
	${log}::notice "Available fields in object $uuid in context are:\
                        [join $PAIR(fields:context) ,]"
    }
    if { $feed ne "" } {
	set PAIR(fields:pachube) {}
	set feedinfo [::pachube::feed -feed $feed -key $PAIR(-key)]
	foreach datastream [dict get $feedinfo datastreams] {
	    set f [dict get $datastream id]
	    lappend PAIR(fields:pachube) $f
	}
	${log}::notice "Available fields in pachube feed #$feed are:\
                        [join $PAIR(fields:pachube) ,]"
    }
}


proc ::pachube::pair:__extract_fields { str } {
    variable PACHUBE
    variable log

    set ids {}
    set idx 0
    while { [regexp -indices -start $idx {%\w+%} $str range] > 0 } {
	foreach {start stop} $range break
	lappend ids [string range $str [expr $start + 1] [expr $stop - 1]]
	set idx [expr $stop + 1]
    }

    return $ids
}


proc ::pachube::pair:to_json { p } {
    variable PACHUBE
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    set result "\{"
    foreach k {uuid -frequency -key -destination -to -from -translations} {
	append result "\"[string trimleft $k -]\":\"$PAIR($k)\","
    }
    set result [string trimright $result ","]
    append result "\}"

    return $result
}

proc ::pachube::pair:check { p } {
    variable PACHUBE
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    set translations {}
    foreach { lft rgt } $PAIR(-translations) {
	set valid 1

	switch $PAIR(-destination) {
	    pachube {
		foreach f [::pachube::pair:__extract_fields $lft] {
		    if { [lsearch $PAIR(fields:pachube) $f] < 0 } {
			${log}::error "Field '$f' used in translation does\
                                         not exist in feed $PAIR(-to) at\
                                         pachube!"
			set valid 0
		    }
		}
		foreach f [::pachube::pair:__extract_fields $rgt] {
		    if { [lsearch $PAIR(fields:context) $f] < 0 } {
			${log}::error "Field '$f' used in translation does\
                                         not exist in context objcet\
                                         $PAIR(-from)!"
			set valid 0
		    }
		}
	    }
	    context {
		foreach f [::pachube::pair:__extract_fields $lft] {
		    if { [lsearch $PAIR(fields:context) $f] < 0 } {
			${log}::error "Field '$f' used in translation does\
                                         not exist in context objcet\
                                         $PAIR(-from)!"
			set valid 0
		    }
		}
		foreach f [::pachube::pair:__extract_fields $rgt] {
		    if { [lsearch $PAIR(fields:pachube) $f] < 0 } {
			${log}::error "Field '$f' used in translation does\
                                         not exist in feed $PAIR(-to) at\
                                         pachube!"
			set valid 0
		    }
		}
	    }
	}
	if { $valid } {
	    lappend translations $lft $rgt
	}
    }
    set PAIR(-translations) $translations

    # Arrange to always have translations, even singular such
    if { [llength $PAIR(-translations)] == 0 } {
	foreach f $PAIR(fields:pachube) {
	    if { [lsearch $PAIR(fields:context) $f] >= 0 } {
		lappend PAIR(-translations) %$f% %$f%
	    }
	}
    }
}


proc ::pachube::pair:bind { p } {
    variable PACHUBE
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    switch $PAIR(-destination) {
	pachube {
	    set feed $PAIR(-to)
	    set uuid $PAIR(-from)
	    
	    foreach {srv root} $PACHUBE(servers) {
		# Create a receiver URL, i.e. the entry point for what
		# we have registered as /copy under the root of
		# the conduit
		set rcv \
		    [::minihttpd::fullurl $srv][string trim $root /]/copy?
		append rcv uuid=$PAIR(uuid)
		# Build a query that is compatible with the API of the
		# trigger creation.
		set tqry [list method GET \
			      always on \
			      receiver $rcv]
		set PAIR(trigger) [::trigger:new $PAIR(object) $tqry]
		${log}::notice "Installed trigger $PAIR(trigger) to return to\
                                us every time the content of $uuid changes"
		return 1
	    }
	}
	context {
	    # NYI, we will poll instead...
	}
    }
    return 0
}


proc ::pachube::pair:__copy_once { p } {
    variable PACHUBE
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    switch $PAIR(-destination) {
	pachube {
	    set feed $PAIR(-to)
	    set uuid $PAIR(-from)

	    upvar \#0 $PAIR(object) OBJ
	    # Prepare a list of JSON streams updates
	    set json ""
	    foreach {dst src} $PAIR(-translations) {
		set dst_fields [::pachube::pair:__extract_fields $dst]
		set dst_mapper {}
		foreach f $dst_fields {
		    lappend dst_mapper %$f% ""
		}
		
		set src_fields [::pachube::pair:__extract_fields $src]
		set src_mapper {}
		foreach f $src_fields {
		    lappend src_mapper %$f% $OBJ(-$f)
		}
		if { [llength $dst_fields] == 1 \
			 && [string trim \
				 [string map $dst_mapper $dst]] eq ""} {
		    set df [lindex $dst_fields 0]
		    ${log}::notice "Copying sub-content of $uuid to pachube\
                                    feed #$feed: ${df}=${src}"
		    if { [catch {expr [string map $src_mapper $src]} val]==0} {
			append json "\{"
			append json "\"id\":\"$df\","
			append json "\"current_value\":\"$val\""
			append json "\},"
		    }
		}
	    }
	    
	    # Now update the datastreams of the feed
	    set json [string trimright $json ","]
	    if { $json ne "" } {
		# we need to add the version
		set json "\{\"version\":\"1.0.0\",\"datastreams\":\[$json\]\}"
		if { [catch \
			  {::pachube::fupdate \
			       -feed $feed \
			       -key $PAIR(-key) \
			      $json} \
				  ret] } {
		    ${log}::error "Could not update value of feed #$feed at\
                                   pachube: $ret"
		} else {
		    ${log}::notice "Value of streams in feed #$feed now\
                                    updated"
		}
	    }
	}
	context {
	    set feed $PAIR(-from)
	    set uuid $PAIR(-to)
	    ${log}::debug "Getting current content of pachube feed #$feed"
	    if { [catch {::pachube::feed -feed $feed \
			     -key $PAIR(-key)} feedinfo] == 0 } {
		foreach datastream [dict get $feedinfo datastreams] {
		    # Make sure we have a current_value, newly created
		    # streams that never have had any update do NOT
		    # have a current value.
		    if { [dict exists $datastream current_value] } {
			set CURRENT([dict get $datastream id]) \
			    [dict get $datastream current_value]
		    }
		}

		upvar \#0 $PAIR(object) OBJ
		foreach {dst src} $PAIR(-translations) {
		    set dst_fields [::pachube::pair:__extract_fields $dst]
		    set dst_mapper {}
		    foreach f $dst_fields {
			lappend dst_mapper %$f% ""
		    }
		    
		    set src_fields [::pachube::pair:__extract_fields $src]
		    set src_mapper {}
		    foreach f $src_fields {
			lappend src_mapper %$f% $CURRENT($f)
		    }
		    if { [llength $dst_fields] == 1 \
			     && [string trim \
				     [string map $dst_mapper $dst]] eq ""} {
			set df [lindex $dst_fields 0]
			${log}::notice "Copying sub-content of feed #$feed into\
                                        object $uuid: ${df}=${src}"
			if { [catch {expr [string map $src_mapper $src]} \
				  val] == 0} {
			    set OBJ(-$df) $val
			}
		    }
		}
	    } else {
		${log}::warn "Could not get pachube feed #$feed content:\
                              $feedinfo"
	    }
	}
    }
}


proc ::pachube::pair:copy { p } {
    variable PACHUBE
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    if { [catch {::pachube::pair:__copy_once $p} err] } {
	${log}::error "Could not copy to $PAIR(-destination) $PAIR(-to)\
                         from $PAIR(-from) this time: $err"
    }

    set next [expr int(1000*$PAIR(-frequency))]
    set PAIR(scheduler) [after $next ::pachube::pair:copy $p]
}


proc ::pachube::pair:init { p } {
    variable PACHUBE
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    if { ! $PAIR(inited) } {
	if { [catch {pair:sync $p} err] } {
	    ${log}::error "Could not synchronise pair $p, retrying in\
                           $PAIR(-frequency) seconds"
	    set next [expr {1000*$PAIR(-frequency)}]
	    after $next ::pachube::pair:init $p
	} else {
	    pair:check $p
	    if { ! [pair:bind $p] } {
		pair:copy $p
	    } else {
		pair:__copy_once $p
	    }
	    set PAIR(inited) 1
	}
    }
}


proc ::pachube::rest:copy { prt sock url qry } {
    variable PACHUBE
    variable log

    array set RESULTS {
	true     "\{\"result\":true\}"
	false     "\{\"result\":false\}"
    }

    set hex "\[a-f0-9\]"
    set uuid_filter "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]"

    if { [dict exists $qry uuid] } {
	set uuid [dict get $qry uuid]
    } else {
	set uuid [lindex [split [string trimright $url "/"] "/"] end-1]
	if { ! [string match -nocase $uuid_filter $uuid] } { 
	    set uuid [lindex [split [string trimright $url "/"] "/"] end]
	    if { ! [string match -nocase $uuid_filter $uuid] } {
		return $RESULTS(false)
	    }
	}
    }

    set p [::uobj::find [namespace current] pair \
	       [list uuid == $uuid]]
    if { $p eq "" } { return $RESULTS(false) }

    after idle ::pachube::pair:__copy_once $p

    return $RESULTS(true)
}


proc ::pachube::rest:translation { prt sock url qry } {
    variable PACHUBE
    variable log

    set url [string trimright $url "/"]
    
    set hex "\[a-f0-9\]"
    set uuid_filter "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]"

    if { [dict exists $qry uuid] } {
	set uuid [dict get $qry uuid]
    } else {
	set uuid [lindex [split [string trimright $url "/"] "/"] end-1]
	if { ! [string match -nocase $uuid_filter $uuid] } {
	    return "\{\}"
	}
    }

    # Find the pair and return an empty pair if not found
    set p [::uobj::find [namespace current] pair \
	       [list uuid == $uuid]]
    if { $p eq "" } { return "\{\}" }
    
    set operation [string tolower [lindex [split $url "/"] end]]
    if { $operation eq "add" } {
	if { ! [dict exists $qry receive] || ! [dict exist $qry expression] } {
	    return "\{\}"
	}
	set receive "%[string trim [dict get $qry receive] %]%"
	lappend PAIR(-translations) $receive [dict get $qry expression]
	pair:check $p
    } elseif { $operation eq "delete" } {
	# Find which fields to remove, default to all
	if { [dict exists $qry filter] } {
	    set filter [dict get $qry filter]
	} else {
	    set filter "*"
	}
	set filter [string trim $filter %]

	# Remove all the translations which receiver field name
	# matches the filter (do the comparison without the % signs to
	# get this easier
	set translations {}
	foreach {rcv xpr} $PAIR(-translations) {
	    set rcv [string trim $rcv %]
	    if { ![string match $filter $rcv] } {
		lappend translations "%${rcv}%" $xpr
	    } else {
		${log}::debug "Removing translation $rcv = $xpr"
	    }
	}
	set PAIR(-translations) $translations
    }

    return [pair:to_json $p]
}

proc ::pachube::rest:pairs { prt sock url qry } {
    variable PACHUBE
    variable log

    set hex "\[a-f0-9\]"
    set uuid_filter "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]"

    if { [dict exists $qry uuid] } {
	set uuid [dict get $qry uuid]
    } else {
	set uuid [lindex [split [string trimright $url "/"] "/"] end-1]
	if { ! [string match -nocase $uuid_filter $uuid] } { 
	    set uuid [lindex [split [string trimright $url "/"] "/"] end]
	    if { ! [string match -nocase $uuid_filter $uuid] } {
		set result "\["
		foreach p [::uobj::allof [namespace current] pair] {
		    append result [pair:to_json $p],
		}
		set result [string trimright $result ","]
		append result "\]"

		return $result
	    }
	}
    }

    set p [::uobj::find [namespace current] pair \
	       [list uuid == $uuid]]
    if { $p eq "" } { return "\{\}" }

    return [pair:to_json $p]
}


proc ::pachube::rest:pair { prt sock url qry } {
    variable PACHUBE
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
		set r [::uobj::find [namespace current] pair \
			   [list uuid == $uuid]]
		if { $r ne "" } {
		    return [rest:pairs $prt $sock $url \
				[list uuid $uuid]]
		}
	    }
	}
    }
    
    if { ![dict exists $qry feed] } {
	${log}::warn "No feed id specified, aborting"
	return ""
    }
    if { ![string is integer [dict get $qry feed]] } {
	${log}::warn "Feed identifier not an integer, aborting"
	return ""
    }
    foreach {obj cls} [::find:uuid $uuid object] break
    if { $obj eq "" } {
	${log}::warn "$uuid does not to any existing object in context"
	return ""
    }

    set destination pachube
    if { [dict exists $qry destination] } {
	set destination [string tolower [dict get $qry destination]]
	if { [lsearch {pachube context} $destination] < 0 } {
	    ${log}::warn "$destination is not a recognised type for destination"
	    return ""
	}
    }

    set frequency $PACHUBE(-frequency)
    if { [dict exists $qry frequency] } {
	set frequency [dict get $qry frequency]
    }

    set key ""
    if { [dict exists $qry key] } {
	set key [dict get $qry key]
    }

    set p [::uobj::new [namespace current] pair]
    upvar \#0 $p PAIR
    set PAIR(id) $p
    set PAIR(uuid) [::uuid::uuid generate]
    set PAIR(trigger) ""
    set PAIR(inited) 0
    set PAIR(-frequency) $frequency
    set PAIR(-key) $key
    set PAIR(object) $obj
    switch $destination {
	pachube {
	    set PAIR(-destination) $destination
	    set PAIR(-to) [dict get $qry feed]
	    set PAIR(-from) $uuid
	}
	context -
	local {
	    set PAIR(-destination) "context"
	    set PAIR(-from) [dict get $qry feed]
	    set PAIR(-to) $uuid
	}
    }
    set PAIR(-translations) {}
    if { [dict exists $qry translations] } {
	foreach {rcv xpr} [dict get $qry translations] {
	    lappend PAIR(-translations) [string trim $rcv] [string trim $xpr]
	}
    }

    # Schedule initialisation of pair ASAP, do this outside of the
    # main event call since we are being called from the network
    # events of the HTTP server.
    after idle ::pachube::pair:init $p

    return [pair:to_json $p]
}


proc ::pachube::__init {} {
    variable PACHUBE
    variable log

    if { ! $PACHUBE(initdone) } {
	${log}::debug "First time initialisation of pachube conduit"
	if { [catch {package require tls} err] } {
	    ${log}::error "Will not be able to communicate securely!\
                           (reason: $err)"
	    # Force simple HTTP access to pachube, not recommended but
	    # no choice...
	    set PACHUBE(apiroot) [string map [list "https:/" "http:/"] \
				      $PACHUBE(apiroot)]
	} else {
	    ::http::register https 443 [list ::tls::socket]
	}

	set pachube(feed) \
	    [list \
		 url $PACHUBE(apiroot)/feeds/%feed%.json \
		 opt_args { key: } \
		]
	set pachube(stream) \
	    [list \
		 url $PACHUBE(apiroot)/feeds/%feed%/datastreams/%id%.json \
		 opt_args { key: } \
		]
	set pachube(update) \
	    [list \
		 url $PACHUBE(apiroot)/feeds/%feed%/datastreams/%id%.json \
		 method put \
		 body required \
		 req_args { key: } \
		]
	set pachube(fupdate) \
	    [list \
		 url $PACHUBE(apiroot)/feeds/%feed%.json \
		 method put \
		 body required \
		 req_args { key: } \
		]
	::rest::create_interface pachube

	set PACHUBE(initdone) 1
    }
}


proc ::pachube::init { cx root srv } {
    variable PACHUBE
    variable log

    __init

    set PACHUBE(context) $cx
    lappend PACHUBE(servers) $srv $root

    set hex "\[a-fA-F0-9\]"
    set uuid "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]" 

    set root [string trimright $root "/"]
    ::minihttpd::handler $srv $root/$uuid ::pachube::rest:pair \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/pair ::pachube::rest:pair \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/pair/ ::pachube::rest:pair \
	"application/json"
    ::minihttpd::handler $srv $root ::pachube::rest:pairs \
	"application/json"
    ::minihttpd::handler $srv $root/ ::pachube::rest:pairs \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/add ::pachube::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/add/ ::pachube::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/remove ::pachube::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/remove/ ::pachube::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/copy ::pachube::rest:copy \
	"application/json"
    
    ${log}::notice "Registered REsT entry points in server at port #$srv"
}