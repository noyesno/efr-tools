package require Tcl 8.5

#package require tls;  # Will be done dynamically.
package require http
package require uuid
package require rest

namespace eval ::gcal {
    variable GCAL
    if { ![info exists GCAL] } {
	array set GCAL {
	    context     ""
	    servers     {}
	    initdone    0
	    -frequency  60
	    -span       48
	    -chunk      25
	}
	::uobj::install_log gcal GCAL
	::uobj::install_defaults gcal GCAL
    }
}


proc ::gcal::pair:to_json { p } {
    variable GCAL
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    set result "\{"
    foreach k {uuid userid -frequency -translations -user -password -calendar} {
	append result "\"[string trimleft $k -]\":\"$PAIR($k)\","
    }
    set result [string trimright $result ","]
    append result "\}"

    return $result
}


# Synchronise content of pair with the data that is really available
# at the remote calendar, i.e. both with the expected field names for
# events and within the field of the context object.
proc ::gcal::pair:sync { p } {
    variable GCAL
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    ${log}::info "Authorising at gCal as $PAIR(-user):$PAIR(-password)"
    set PAIR(token) [::gcal::auth -Email $PAIR(-user) -Passwd $PAIR(-password)]
    ${log}::debug "Authorised with token: $PAIR(token)"
    ${log}::info "Getting list of accessible calendars for $PAIR(-user)"
    set res [::gcal::handle_redir \
		 ::gcal::all_calendars -token $PAIR(token) -alt jsonc]
    foreach c [dict get [dict get $res data] items] {
	set title [dict get $c title]
	if { $title eq $PAIR(-calendar) } {
	    set PAIR(userid) [lindex [split [dict get $c id] "/"] end]
	}
    }
    
    set PAIR(fields:context) {}
    set PAIR(fields:calendar) {}
    upvar \#0 $PAIR(object) OBJ
    foreach f [array names OBJ -*] {
	lappend PAIR(fields:context) [string trimleft $f "-"]
    }
    ${log}::notice "Available fields in object $OBJ(uuid) in context are:\
                    [join $PAIR(fields:context) ,]"
    if { $PAIR(userid) ne "" } {
	set PAIR(fields:calendar) [list title id where details start end]
	${log}::notice "Available fields in calendar for $PAIR(userid) are:\
                        [join $PAIR(fields:calendar) ,]"
    }
}


proc ::gcal::pair:check { p } {
    variable GCAL
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR
    upvar \#0 $PAIR(object) OBJ

    set translations {}
    foreach { lft rgt } $PAIR(-translations) {
	set valid 1

	foreach f [pair:extract $lft] {
	    if { [lsearch $PAIR(fields:context) $f] < 0 } {
		${log}::error "Field '$f' used in translation does\
                               not exist in context objcet\
                               $OBJ(uuid)!"
		set valid 0
	    }
	}
	foreach f [pair:extract $rgt] {
	    if { [lsearch $PAIR(fields:calendar) $f] < 0 } {
		${log}::error "Field '$f' used in translation does\
                               not exist in calendar events"
		set valid 0
	    }
	}

	if { $valid } {
	    lappend translations $lft $rgt
	}
    }
    set PAIR(-translations) $translations

    # Arrange to always have translations, even singular such
    if { [llength $PAIR(-translations)] == 0 } {
	foreach f $PAIR(fields:calendar) {
	    if { [lsearch $PAIR(fields:context) $f] >= 0 } {
		lappend PAIR(-translations) %$f% %$f%
	    }
	}
    }
}


proc ::gcal::get_events { p { now "" } } {
    variable GCAL
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    ${log}::debug "Getting current content of gcal $PAIR(-calendar)\
                   (span of +/- $GCAL(-span) hrs around current time)"
    set events {}
    set start 1
    set done 0
    if { $now eq "" } {
	set now [clock seconds]
    }
    set min [::schema::to_rfc3339 [expr {$now - $GCAL(-span)*60*60}] s s]
    set max [::schema::to_rfc3339 [expr {$now + $GCAL(-span)*60*60}] s s]
    while { !$done} {
	set res [::gcal::handle_redir ::gcal::all_events \
		     -token $PAIR(token) \
		     -alt jsonc \
		     -user $PAIR(userid) \
		     -start-index $start \
		     -max-results $GCAL(-chunk) \
		     -start-min $min \
		     -start-max $max]

	# Advance
	set data [dict get $res data]
	set total [dict get $data totalResults]
	set grp [dict get $data itemsPerPage]
	set nb_evts [expr $start + $grp]
	if { $nb_evts < $total } {
	    ${log}::debug "Got $nb_evts / $total events"
	    incr start $grp
	} else {
	    set done 1
	    ${log}::debug "Got all $total events"
	}

	# Get and dump events:
	foreach evt [dict get $data items] {
	    # Store and translate from Google event fields into a
	    # transient array that is almost in the format of the
	    # events that will be added to <events>
	    foreach {gkey ekey} { id id selfLink link title title \
				      details details location where } {
		if { [dict exists $evt $gkey] } {
		    set GEVT($ekey) [dict get $evt $gkey]
		} else {
		    set GEVT($ekey) ""
		}
	    }

	    # Create an event from each period found in the calendar
	    # event field "when" and append it to the events list
	    # after having copied the "global" data for the event
	    # contained in GEVT and extracted above.
	    foreach when [dict get $evt when] {
		set e [::uobj::new [namespace current] event [::uobj::id $p]]
		upvar \#0 $e EVT
		array set EVT [array get GEVT]
		set EVT(start) [::schema::from_rfc3339 [dict get $when start] s]
		set EVT(end) [::schema::from_rfc3339 [dict get $when end] s]

		lappend events $e
	    }
	}
    }

    return $events
}


proc ::gcal::pair:__copy_once { p } {
    variable GCAL
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR
    upvar \#0 $PAIR(object) OBJ

    set copied 0
    set now [clock seconds]
    set events [get_events $p $now]

    foreach e $events {
	upvar \#0 $e EVT
	${log}::debug "Testing if current date [::schema::to_rfc3339 $now s s]\
                       is between [::schema::to_rfc3339 $EVT(start) s s] and\
                       [::schema::to_rfc3339 $EVT(end) s s]\
                       (title: $EVT(title))"
	if { $now >= $EVT(start) && $now < $EVT(end) } {
	    pair:receive $PAIR(object) $e $PAIR(-translations)
	    set copied 1
	    break
	}
    }
    if { ! $copied } {
	foreach {dst src} $PAIR(-translations) {
	    set dst_fields [pair:extract $dst]
	    set dst_mapper {}
	    foreach f $dst_fields {
		lappend dst_mapper %$f% ""
	    }
		
	    if { [llength $dst_fields] == 1 \
		     && [string trim \
			     [string map $dst_mapper $dst]] eq ""} {
		set df [lindex $dst_fields 0]
		${log}::notice "Zeroing sub-content of\
                                object $OBJ(uuid): ${df}=''"
		set OBJ(-$df) ""
	    }
	}
    }

    foreach e $events {
	::uobj::delete $e
    }

    return $copied
}


proc ::gcal::pair:copy { p } {
    variable GCAL
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    if { [catch {::gcal::pair:__copy_once $p} err] } {
	${log}::error "Could not copy content of calendar into pair $PAIR(uuid)\
                       this time: $err"
    }

    set next [expr int(1000*$PAIR(-frequency))]
    set PAIR(scheduler) [after $next ::gcal::pair:copy $p]
}



proc ::gcal::pair:init { p } {
    variable GCAL
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    if { ! $PAIR(inited) } {
	if { [catch {pair:sync $p} err] } {
	    ${log}::error "Could not synchronise pair $p, retrying in\
                           $PAIR(-frequency) seconds (reason: $err)"
	    set next [expr {1000*$PAIR(-frequency)}]
	    after $next ::gcal::pair:init $p
	} else {
	    pair:check $p
	    pair:copy $p
	    set PAIR(inited) 1
	}
    }
}


proc ::gcal::rest:copy { prt sock url qry } {
    variable GCAL
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

    after idle ::gcal::pair:__copy_once $p

    return $RESULTS(true)
}


proc ::gcal::rest:translation { prt sock url qry } {
    variable GCAL
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

proc ::gcal::rest:pairs { prt sock url qry } {
    variable GCAL
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


proc ::gcal::rest:pair { prt sock url qry } {
    variable GCAL
    variable log

    set hex "\[a-f0-9\]"
    set uuid_filter "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]"

    set uuid ""
    if { [dict exists $qry uuid] } {
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
    
    if { ![dict exists $qry user] } {
	${log}::warn "No 'user' specified, aborting"
	return ""
    }

    if { ![dict exists $qry password] } {
	${log}::warn "No 'password' specified, aborting"
	return ""
    }
    if { ![dict exists $qry calendar] } {
	${log}::warn "No 'calendar' specified, aborting"
	return ""
    }

    foreach {obj cls} [::find:uuid $uuid object] break
    if { $obj eq "" } {
	${log}::warn "$uuid does not point to any existing object in context"
	return ""
    }

    set p [::uobj::new [namespace current] pair]
    upvar \#0 $p PAIR
    set PAIR(id) $p
    set PAIR(uuid) [::uuid::uuid generate]
    set PAIR(inited) 0
    set PAIR(object) $obj
    set PAIR(userid) ""
    set PAIR(token) ""
    set PAIR(scheduler) ""
    set PAIR(-frequency) $GCAL(-frequency)
    if { [dict exists $qry frequency] } {
	set frequency [dict get $qry frequency]
	if { [string is integer $frequency] } {
	    set PAIR(-frequency) $frequency
	} else {
	    ${log}::warn "Frequency $frequency not an integer, keeping default"
	}
    }
    set PAIR(-translations) {}
    if { [dict exists $qry translations] } {
	foreach {rcv xpr} [dict get $qry translations] {
	    lappend PAIR(-translations) [string trim $rcv] [string trim $xpr]
	}
    }
    set PAIR(-user) [dict get $qry user]
    set PAIR(-password) [dict get $qry password]
    set PAIR(-calendar) [dict get $qry calendar]

    # Schedule initialisation of pair ASAP, do this outside of the
    # main event call since we are being called from the network
    # events of the HTTP server.
    after idle ::gcal::pair:init $p

    return [pair:to_json $p]
}


proc ::gcal::__init {} {
    variable GCAL
    variable log

    if { ! $GCAL(initdone) } {
	${log}::debug "First time initialisation of gcal conduit"
	if { [catch {package require tls} err] } {
	    ${log}::error "Will not be able to perform secure login!\
                           (reason: $err)"
	    set scheme "http"
	} else {
	    ::http::register https 443 [list ::tls::socket]
	    set scheme "https"
	}

	set gcal(auth) {
	    method post
	    req_args { Email: Passwd: }
	    opt_args { source:tclrest }
	    static_args { service cl }
	    post_transform {
		regexp {Auth=(.*)\n} $result -> result
		return $result
	    }
	}
	lappend gcal(auth) url ${scheme}://www.google.com/accounts/ClientLogin

	set gcal(all_calendars) {
	    url https://www.google.com/calendar/feeds/default/allcalendars/%projection:full%
	    headers { Authorization {GoogleLogin auth=%token%} }
	    opt_args { gsessionid: alt: t: }
	}

	set gcal(all_events) {
	    url https://www.google.com/calendar/feeds/%user:default%/%visibility:private%/%projection:full%
	    headers { Authorization {GoogleLogin auth=%token%} }
	    opt_args { gsessionid: alt: t: start-index: max-results: start-min: start-max:}
	}

	rest::create_interface gcal

	set GCAL(initdone) 1
    }
}

proc ::gcal::handle_redir {args} {
    variable log

    if {[catch {eval $args} out]} {
        if {[lindex $out 1] == "302"} {
	    set parms [rest::parameters [lindex $out 2]]
	    if { [dict exists $parms gsessionid] } {
		eval [linsert $args 1 -gsessionid [dict get $parms gsessionid]]
	    } elseif { [dict exists $parms t] } {
		eval [linsert $args 1 -t [dict get $parms t]]
	    }
	} else {
	    return -code error $out
	}
    } else {
	return $out
    }
}



proc ::gcal::init { cx root srv } {
    variable GCAL
    variable log
    
    __init

    set GCAL(context) $cx
    lappend GCAL(servers) $srv $root

    set hex "\[a-fA-F0-9\]"
    set uuid "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]"

    set root [string trimright $root "/"]
    ::minihttpd::handler $srv $root/$uuid ::gcal::rest:pair "application/json"
    ::minihttpd::handler $srv $root/$uuid/pair ::gcal::rest:pair \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/pair/ ::gcal::rest:pair \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/add ::gcal::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/add/ ::gcal::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/remove ::gcal::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/remove/ ::gcal::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/copy ::gcal::rest:copy \
	"application/json"
    ::minihttpd::handler $srv $root ::gcal::rest:pairs \
	"application/json"
    ::minihttpd::handler $srv $root/ ::gcal::rest:pairs \
	"application/json"
    
    ${log}::notice "Registered ReST entry points in server at port #$srv"
}
