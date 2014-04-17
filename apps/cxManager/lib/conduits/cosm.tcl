##################
## Module Name     --  cosm.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    This is a conduit designed at receiving cosm-formatted data
##    and dispatching in onto objects of the context.
##
## Commands Exported:
##      cmd1
##      cmd2
##################

#package require tls;  # Will be done dynamically instead
package require http
package require uuid

namespace eval ::cosm {
    variable COSM
    if {![info exists COSM] } {
	array set COSM {
	    context        ""
	    servers        {}
	    initdone       0
	    apiroot        "https://api.xively.com/v2"
	    -frequency     180
	    -timeout       10000
	    -retries       10
	}
	::uobj::install_log cosm COSM
	::uobj::install_defaults cosm COSM
    }
}

proc ::cosm::__receive { p cmd token } {
    variable COSM
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    if { [::http::ncode $token] == 200 } {
	if { $cmd ne "" } {
	    if { [catch {eval [list $cmd $p [::http::data $token]]} err] } {
		${log}::warn "Error when calling back success API reception\
                              command '$cmd': $err"
	    }
	}
    } else {
	upvar \#0 $token USTATE
	${log}::warn "Could not get URL $USTATE(url): error id\
                      '[::http::error $token]' status is:\
                      '[::http::status $token]' code was:\
                      [::http::ncode $token]"
	pair:tellstate $p $PAIR(state) $COSM(-retries)
    }
    ::http::cleanup $token
}


proc ::cosm::cosm { p path cmd { args {} } } {
    variable COSM
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    set headers {}
    if { $PAIR(-key) ne "" } {
	lappend headers X-ApiKey $PAIR(-key)
    }

    # Construct URL and make sure we force all accesses to use JSON.
    set url [string trimright $COSM(apiroot) /]
    append url / [string trimleft $path /]
    if { [string range $url end-4 end] ne "json" } {
	append url ".json"
    }

    set gcmd [list ::http::geturl $url \
		  -command [list [namespace current]::__receive $p $cmd] \
		  -headers $headers]
    # Add global timeout
    if { $COSM(-timeout) > 0 } {
	lappend gcmd -timeout $COSM(-timeout)
    }
    # Add specific arguments that are of interest to the call
    foreach {k v} $args {
	set ck [string tolower [string trimleft $k -]]
	if { [lsearch [list method type query] $ck] >= 0 } {
	    lappend gcmd -$ck $v
	}
    }

    if { [catch {eval $gcmd} token] } {
	${log}::warn "Could not invoke API call '$path': $token"
	return ""
    }

    return $token
}


proc ::cosm::pair:__sync { p data } {
    variable COSM
    variable log

    upvar \#0 $p PAIR
    set PAIR(fields:cosm) {}
    set feedinfo [::json:to_dict $data]
    foreach datastream [dict get $feedinfo datastreams] {
	set f [dict get $datastream id]
	lappend PAIR(fields:cosm) $f
    }
    ${log}::notice "Available fields in cosm feed are:\
                    [join $PAIR(fields:cosm) ,]"
    pair:tellstate $p CHECK
}


proc ::cosm::pair:check { p } {
    variable COSM
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
	    cosm {
		foreach f [pair:extract $lft] {
		    if { [lsearch $PAIR(fields:cosm) $f] < 0 } {
			${log}::error "Field '$f' used in translation does\
                                         not exist in feed $PAIR(-to) at\
                                         cosm!"
			set valid 0
		    }
		}
		foreach f [pair:extract $rgt] {
		    if { [lsearch $PAIR(fields:context) $f] < 0 } {
			${log}::error "Field '$f' used in translation does\
                                         not exist in context objcet\
                                         $PAIR(-from)!"
			set valid 0
		    }
		}
	    }
	    context {
		foreach f [pair:extract $lft] {
		    if { [lsearch $PAIR(fields:context) $f] < 0 } {
			${log}::error "Field '$f' used in translation does\
                                         not exist in context objcet\
                                         $PAIR(-from)!"
			set valid 0
		    }
		}
		foreach f [pair:extract $rgt] {
		    if { [lsearch $PAIR(fields:cosm) $f] < 0 } {
			${log}::error "Field '$f' used in translation does\
                                         not exist in feed $PAIR(-to) at\
                                         cosm!"
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
	foreach f $PAIR(fields:cosm) {
	    if { [lsearch $PAIR(fields:context) $f] >= 0 } {
		lappend PAIR(-translations) %$f% %$f%
	    }
	}
    }
}


proc ::cosm::pair:bind { p } {
    variable COSM
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    # No binding if we were force to poll all the time!
    if { [string is true $PAIR(-polling)] } {
	return 0
    }

    switch $PAIR(-destination) {
	cosm {
	    set feed $PAIR(-to)
	    set uuid $PAIR(-from)
	    
	    foreach {srv root} $COSM(servers) {
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

proc ::cosm::pair:__insert { p data } {
    variable COSM
    variable log

    upvar \#0 $p PAIR
    set feedinfo [::json:to_dict $data]

    set uuid ""; set feed ""
    switch $PAIR(-destination) {
	cosm {
	    set uuid $PAIR(-from)
	    set feed $PAIR(-to)
	    ${log}::debug "State machine: $uuid -> $feed -- $PAIR(state)"
	}
	context {
	    set uuid $PAIR(-to)
	    set feed $PAIR(-from)
	    ${log}::debug "State machine: $feed -> $uuid -- $PAIR(state)"
	}
    }

    # Create an array <CURRENT> with the current content of the feed,
    # i.e. the latest known values from the datastreams in the feed.
    set c [::uobj::new [namespace current] cosm [::uobj::id $p]]
    ::uobj::keyword $c cacheid $PAIR(uuid)
    upvar \#0 $c CURRENT
    foreach datastream [dict get $feedinfo datastreams] {
	# Make sure we have a current_value, newly created
	# streams that never have had any update do NOT
	# have a current value.
	if { [dict exists $datastream current_value] } {
	    set sid [dict get $datastream id]
	    set CURRENT(-$sid) [dict get $datastream current_value]
	}
    }

    ::pair:receive $PAIR(object) $c $PAIR(-translations)
    ::uobj::delete $c

    if { $PAIR(state) eq "COPYING" } {
	pair:tellstate $p COPIED
    }
}


proc ::cosm::pair:__copy_once { p } {
    variable COSM
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    switch $PAIR(-destination) {
	cosm {
	    set feed $PAIR(-to)
	    set uuid $PAIR(-from)
	    
	    # Prepare a list of JSON streams updates
	    set json ""
	    set c [::uobj::new [namespace current] cosm [::uobj::id $p]]
	    ::uobj::keyword $c cacheid $PAIR(uuid)
	    upvar \#0 $c COSM
	    ::pair:receive $c $PAIR(object) $PAIR(-translations)
	    foreach k [array names COSM -*] {
		append json "\{"
		append json "\"id\":\"[string trimleft $k -]\","
		append json "\"current_value\":\"$COSM($k)\""
		append json "\},"
	    }
		
	    # Now update the datastreams of the feed
	    set json [string trimright $json ","]
	    if { $json ne "" } {
		# we need to add the version
		set json "\{\"version\":\"1.0.0\",\"datastreams\":\[$json\]\}"
		cosm $p feeds/${feed} "" -method PUT -query $json
	    }
	    ::uobj::delete $c
	    if { $PAIR(state) eq "COPYING" } {
		pair:tellstate $p COPIED
	    }
	}
	context {
	    set feed $PAIR(-from)
	    set uuid $PAIR(-to)
	    ${log}::debug "Getting current content of cosm feed #$feed"
	    cosm $p feeds/${feed} [namespace current]::pair:__insert 
	}
    }
}


proc ::cosm::pair:state { p } {
    variable COSM
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    set uuid ""; set feed ""
    switch $PAIR(-destination) {
	cosm {
	    set uuid $PAIR(-from)
	    set feed $PAIR(-to)
	    ${log}::debug "State machine: $uuid -> $feed -- $PAIR(state)"
	}
	context {
	    set uuid $PAIR(-to)
	    set feed $PAIR(-from)
	    ${log}::debug "State machine: $feed -> $uuid -- $PAIR(state)"
	}
    }

    switch $PAIR(state) {
	"CREATED" {
	    upvar \#0 $PAIR(object) OBJ
	    set PAIR(fields:context) {}
	    foreach f [array names OBJ -*] {
		lappend PAIR(fields:context) [string trimleft $f "-"]
	    }
	    ${log}::notice "Available fields in object $uuid in context:\
                            [join $PAIR(fields:context) ,]"

	    set PAIR(fields:cosm) {}
	    cosm $p feeds/${feed} [namespace current]::pair:__sync
	}
	"CHECK" {
	    pair:check $p
	    if { [pair:bind $p] } {
		set PAIR(state) BOUND
		pair:__copy_once $p
	    } else {
		pair:tellstate $p COPYING
	    }
	}
	"COPYING" {
	    pair:__copy_once $p
	    # Do not change state, since we will be changing once the
	    # copy has ended.
	}
	"COPIED" {
	    pair:tellstate $p COPYING $PAIR(-frequency)
	}
	"ERROR" {
	    ${log}::error "Dead end in state machine, something went terribly\
                           wrong..."
	}
    }
}


proc ::cosm::pair:tellstate { p { state "" } { when "idle" } } {
    variable COSM
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    if { $state ne "" } {
	catch {after cancel $PAIR(scheduler)}
	if { [string is double $when] } {
	    set next [expr int(1000*$when)]
	} else {
	    set next $when
	}
	set PAIR(state) $state
	set PAIR(scheduler) \
	    [after $next [namespace current]::pair:state $p]
    }
}



proc ::cosm::rest:copy { prt sock url qry } {
    variable COSM
    variable log

    array set RESULTS {
	true     "\{\"result\":true\}"
	false     "\{\"result\":false\}"
    }

    if { [dict exists $qry uuid] } {
	set uuid [dict get $qry uuid]
    } else {
	set uuid [lindex [split [string trimright $url "/"] "/"] end-1]
	if { ! [::schema::check UUID $uuid] } { 
	    set uuid [lindex [split [string trimright $url "/"] "/"] end]
	    if { ! [::schema::check UUID $uuid] } {
		return $RESULTS(false)
	    }
	}
    }

    set p [::uobj::find [namespace current] pair \
	       [list uuid == $uuid]]
    if { $p eq "" } { return $RESULTS(false) }

    after idle [namespace current]::pair:__copy_once $p

    return $RESULTS(true)
}


proc ::cosm::rest:translation { prt sock url qry } {
    variable COSM
    variable log

    set url [string trimright $url "/"]
    
    if { [dict exists $qry uuid] } {
	set uuid [dict get $qry uuid]
    } else {
	set uuid [lindex [split [string trimright $url "/"] "/"] end-1]
	if { ! [::schema::check UUID $uuid] } {
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
	pair:tellstate $p CHECK;   # Need to refresh the details
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

    return [json:serialize $p \
		[list uuid \
		     -frequency -key -destination -to -from -translations\
		     -polling]]
}

proc ::cosm::rest:pairs { prt sock url qry } {
    variable COSM
    variable log

    if { [dict exists $qry uuid] } {
	set uuid [dict get $qry uuid]
    } else {
	set uuid [lindex [split [string trimright $url "/"] "/"] end-1]
	if { ! [::schema::check UUID $uuid] } {
	    set uuid [lindex [split [string trimright $url "/"] "/"] end]
	    if { ! [::schema::check UUID $uuid] } {
		set result "\["
		foreach p [::uobj::allof [namespace current] pair] {
		    append result \
			[json:serialize $p \
			     [list uuid \
				  -frequency -key -destination -to -from \
				  -translations -polling]],
		}
		set result [string trimright $result ","]
		append result "\]"

		return $result
	    }
	}
    }

    set p [::uobj::find [namespace current] pair [list uuid == $uuid]]
    if { $p eq "" } { return "\{\}" }

    return [json:serialize $p \
		[list uuid \
		     -frequency -key -destination -to -from -translations \
		     -polling]]
}


proc ::cosm::rest:pair { prt sock url qry } {
    variable COSM
    variable log

    set uuid ""
    if { [dict keys $qry uuid] ne {} } {
	set uuid [dict get $qry uuid]
    } else {
	set uuid [lindex [split [string trimright $url "/"] "/"] end-1]
	if { ! [::schema::check UUID $uuid] } { 
	    set uuid [lindex [split [string trimright $url "/"] "/"] end]
	    if { ! [::schema::check UUID $uuid] } {
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

    set destination cosm
    if { [dict exists $qry destination] } {
	set destination [string tolower [dict get $qry destination]]
	if { [lsearch {cosm context local pachube} $destination] < 0 } {
	    ${log}::warn "$destination is not a recognised type for destination"
	    return ""
	}
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
    set PAIR(-key) $key
    set PAIR(object) $obj
    set PAIR(fields:context) {}
    set PAIR(fields:cosm) {}
    set PAIR(state) ""
    set PAIR(scheduler) ""
    switch $destination {
	pachube -
	cosm {
	    set PAIR(-destination) cosm
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
    set PAIR(-frequency) $COSM(-frequency)
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
    set PAIR(-polling) off
    if { [dict exists $qry polling] } {
	set PAIR(-polling) [dict get $qry polling]
    }

    # Schedule initialisation of pair ASAP, do this outside of the
    # main event call since we are being called from the network
    # events of the HTTP server.
    pair:tellstate $p "CREATED"

    return [json:serialize $p \
		[list uuid \
		     -frequency -key -destination -to -from -translations \
		     -polling]]
}


proc ::cosm::__init {} {
    variable COSM
    variable log

    if { ! $COSM(initdone) } {
	${log}::debug "First time initialisation of cosm conduit"
	if { [catch {package require tls} err] } {
	    ${log}::error "Will not be able to communicate securely!\
                           (reason: $err)"
	    # Force simple HTTP access to cosm, not recommended but
	    # no choice...
	    set COSM(apiroot) [string map [list "https:/" "http:/"] \
				      $COSM(apiroot)]
	} else {
	    ::http::register https 443 [list ::tls::socket]
	}

	set COSM(initdone) 1
    }
}


proc ::cosm::init { cx root srv } {
    variable COSM
    variable log

    __init

    set COSM(context) $cx
    lappend COSM(servers) $srv $root

    set hex "\[a-fA-F0-9\]"
    set uuid "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]" 

    set root [string trimright $root "/"]
    ::minihttpd::handler $srv $root/$uuid ::cosm::rest:pair \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/pair ::cosm::rest:pair \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/pair/ ::cosm::rest:pair \
	"application/json"
    ::minihttpd::handler $srv $root ::cosm::rest:pairs \
	"application/json"
    ::minihttpd::handler $srv $root/ ::cosm::rest:pairs \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/add ::cosm::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/add/ ::cosm::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/remove ::cosm::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/remove/ ::cosm::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/copy ::cosm::rest:copy \
	"application/json"
    
    ${log}::notice "Registered REsT entry points in server at port #$srv"
}
