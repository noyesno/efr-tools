##################
## Module Name     --  rcontext.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    This is a conduit designed at binding local objects to remote
##    objects and ensuring that they are in-sync. The conduit also
##    allows for some transformation of the data to/from the remote
##    context.
##
## Commands Exported:
##      cmd1
##      cmd2
##################

#package require tls;   # Will be done dynamically instead
package require http
package require uuid
package require cxapi

namespace eval ::rcontext {
    variable RCX
    if {![info exists RCX] } {
	array set RCX {
	    context        ""
	    servers        {}
	    initdone       0
	    https          0
	    -frequency     180
	    -timeout       30000
	}
	::uobj::install_log rcontext RCX
	::uobj::install_defaults rcontext RCX
    }
}


proc ::rcontext::pair:check { p } {
    variable RCX
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
	    remote {
		foreach f [pair:extract $lft] {
		    if { [lsearch $PAIR(fields:remote) $f] < 0 } {
			${log}::error "Field '$f' used in translation does\
                                         not exist in remote object at\
                                         $PAIR(-to)"
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
		    if { [lsearch $PAIR(fields:remote) $f] < 0 } {
			${log}::error "Field '$f' used in translation does\
                                         not exist in remote object at\
                                         $PAIR(-to)"
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
	foreach f $PAIR(fields:remote) {
	    if { [lsearch $PAIR(fields:context) $f] >= 0 } {
		lappend PAIR(-translations) %$f% %$f%
	    }
	}
    }
}


proc ::rcontext::pair:bind { p } {
    variable RCX
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
	remote {
	    set uuid $PAIR(-from)
	    
	    foreach {srv root} $RCX(servers) {
		# Create a receiver URL, i.e. the entry point for what
		# we have registered as /copy under the root of
		# the conduit
		set rcv \
		    [::minihttpd::fullurl $srv][string trim $root /]/copy?
		append rcv __uuid=$PAIR(uuid)
		# Build a query that is compatible with the API of the
		# trigger creation.
		set tqry [list method GET \
			      receiver $rcv]
		set PAIR(trigger) [::trigger:new $PAIR(object) $tqry]
		${log}::notice "Installed trigger $PAIR(trigger) to return to\
                                us every time the content of $uuid changes"
		return 1
	    }
	}
	context {
	    set uuid $PAIR(-to)
	    set remote [string trimright $PAIR(-from) "/"]
	    
	    # Try to pick up a server that is NOT on HTTPS, just
	    # because we want to get data flowing quickly (but at the
	    # price of security...)
	    set freesrv ""
	    foreach {srv root} $RCX(servers) {
		if { [::minihttpd::config $srv -pki] eq "" } {
		    set freesrv $srv
		    break
		}
	    }
	    if { $freesrv eq "" } {
		if { $RCX(https) } {
		    set freesrv [lindex $RCX(servers) 0]
		} else {
		    return 0;  # We can't do https...
		}
	    }
	    set rcv \
		[::minihttpd::fullurl $freesrv][string trim $root /]/copy?
	    append rcv __uuid=$PAIR(uuid)
	    
	    # Build a query that is compatible with the API of the
	    # trigger creation.
	    set tqry [list method GET \
			  receiver $rcv]
	    if { $PAIR(robject) eq "" } {
		${log}::notice "Installing trigger around remote object\
                                $PAIR(ruuid)"
		$PAIR(remote) context $PAIR(ruuid) listen $tqry -
	    } else {
		set PAIR(trigger) [::trigger:new $PAIR(robject) $tqry]
		${log}::notice "Installed trigger $PAIR(trigger) to return to\
                                us every time the content of $remote changes"
	    }
	    return 1
	}
    }
    return 0
}


proc ::rcontext::__receive { p cx data } {
    variable RCX
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    set r [::uobj::new [namespace current] remote [::uobj::id $p]]
    upvar \#0 $r REMOTE
    ::uobj::keyword $r cacheid $PAIR(uuid); # Use the UUID of the pair
					    # as a unique cache ID for
					    # the remote object, this
					    # is as good as it is.
    array set REMOTE [::json:to_dict $data]
    ::pair:receive $PAIR(object) $r $PAIR(-translations)
    ::uobj::delete $r

    if { $PAIR(state) eq "COPYING" } {
	pair:tellstate $p COPIED
    }
}


proc ::rcontext::pair:__copy_once { p } {
    variable RCX
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    switch $PAIR(-destination) {
	remote {
	    set remote [string trimright $PAIR(-to) "/"]
	    set uuid $PAIR(-from)

	    if { $PAIR(robject) eq "" } {
		set r [::uobj::new [namespace current] remote [::uobj::id $p]]
		upvar \#0 $r REMOTE
		::pair:receive $r $PAIR(object) $PAIR(-translations)
		set qry {}
		foreach k [array names REMOTE -*] {
		    lappend qry [string trimleft $k -] $REMOTE($k)
		}
		${log}::debug "Forwarding local content to remote object\
                               $PAIR(ruuid)"
		$PAIR(remote) context $PAIR(ruuid) set $qry "-"
		::uobj::delete $r
	    } else {
		::pair:receive $PAIR(robject) $PAIR(object) $PAIR(-translations)
	    }
	    if { $PAIR(state) eq "COPYING" } {
		pair:tellstate $p COPIED
	    }
	}
	context {
	    set remote [string trimright $PAIR(-from) "/"]
	    set uuid $PAIR(-to)
	    set rval [dict create]

	    if { $PAIR(robject) eq "" } {
		${log}::debug "Getting current content of remote object at\
                               $remote"
		$PAIR(remote) context $PAIR(ruuid) "" "" \
		    [list [namespace current]::__receive $p]
	    } else {
		upvar \#0 $PAIR(robject) ROBJ
		foreach k [array names ROBJ -*] {
		    dict set rval [string trimleft $k -] $ROBJ($k)
		}
		__receive $p "" [::json:from_dict $rval]
	    }
	}
    }
}


proc ::rcontext::pair:__sync { p cx data } {
    variable RCX
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    set rinfo [::json:to_dict $data]
    set PAIR(fields:remote) [dict keys $rinfo]
    ${log}::notice "Available fields in remote object $PAIR(ruuid) are:\
                    [join $PAIR(fields:remote) ,]"

    pair:tellstate $p CHECK
}


proc ::rcontext::pair:state { p } {
    variable RCX
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    set uuid "";
    set remote $PAIR(ruuid)
    switch $PAIR(-destination) {
	remote {
	    set uuid $PAIR(-from)
	    ${log}::debug "State machine: $uuid -> $PAIR(-to) -- $PAIR(state)"
	}
	context {
	    set uuid $PAIR(-to)
	    ${log}::debug "State machine: $PAIR(-from) -> $uuid -- $PAIR(state)"
	}
    }

    switch $PAIR(state) {
	"CREATED" {
	    upvar \#0 $PAIR(object) OBJ
	    foreach f [array names OBJ -*] {
		lappend PAIR(fields:context) [string trimleft $f "-"]
	    }
	    ${log}::notice "Available fields in object $uuid in context are:\
                            [join $PAIR(fields:context) ,]"

	    set PAIR(fields:remote) {}
	    if { $PAIR(robject) eq "" } {
		$PAIR(remote) context $remote "" "" \
		    [list [namespace current]::pair:__sync $p]
	    } else {
		upvar \#0 $PAIR(robject) ROBJ
		foreach f [array names ROBJ -*] {
		    lappend PAIR(fields:remote) [string trimleft $f "-"]
		}
		${log}::notice "Available fields in object $ROBJ(uuid) in\
                                context are: [join $PAIR(fields:remote) ,]"
		pair:tellstate $p CHECK;   # XXX: Correct??
	    }
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
	    # Do not change state, we will changed once copy is done.
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

proc ::rcontext::pair:tellstate { p { state "" } { when "idle" } } {
    variable RCX
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


proc ::rcontext::rest:copy { prt sock url qry } {
    variable RCX
    variable log

    array set RESULTS {
	true     "\{\"result\":true\}"
	false     "\{\"result\":false\}"
    }

    if { [dict exists $qry __uuid] } {
	set uuid [dict get $qry __uuid]
    } else {
	set uuid [lindex [split [string trimright $url "/"] "/"] end-1]
	if { ! [::schema::check UUID $uuid] } { 
	    set uuid [lindex [split [string trimright $url "/"] "/"] end]
	    if { ! [::schema::check UUID $uuid] } {
		return $RESULTS(false)
	    }
	}
    }

    set p [::uobj::find [namespace current] pair [list uuid == $uuid]]
    if { $p eq "" } { return $RESULTS(false) }

    after idle [namespace current]::pair:__copy_once $p

    return $RESULTS(true)
}


proc ::rcontext::rest:translation { prt sock url qry } {
    variable RCX
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

    return [json:serialize $p [list uuid -destination -to -from -translations\
				   -polling]]
}


proc ::rcontext::rest:pairs { prt sock url qry } {
    variable RCX
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
			     [list uuid -destination -to -from -translations\
				  -polling]],
		}
		set result [string trimright $result ","]
		append result "\]"

		return $result
	    }
	}
    }

    set p [::uobj::find [namespace current] pair [list uuid == $uuid]]
    if { $p eq "" } { return "\{\}" }

    return [json:serialize $p [list uuid -destination -to -from -translations\
				   -polling]]
}


proc ::rcontext::rest:pair { prt sock url qry } {
    variable RCX
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
    
    if { ![dict exists $qry remote] } {
	${log}::warn "No remote object specified, aborting"
	return ""
    }
    set remote [string trimright [dict get $qry remote] "/"]
    if { ![::schema::check UUID \
	       [lindex [split $remote "/"] end]] } {
	${log}::warn "Remote URL does not point at an UUID in context"
	return ""
    }
    # Get to the root of the remote context
    set root [join [lrange [split $remote "/"] 0 end-2] /]
    foreach {obj cls} [::find:uuid $uuid object] break
    if { $obj eq "" } {
	${log}::warn "$uuid does not to any existing object in context"
	return ""
    }

    set destination remote
    if { [dict exists $qry destination] } {
	set destination [string tolower [dict get $qry destination]]
	if { [lsearch {rcontext context local remote} $destination] < 0 } {
	    ${log}::warn "$destination is not a recognised type for destination"
	    return ""
	}
    }

    set p [::uobj::new [namespace current] pair]
    upvar \#0 $p PAIR
    set PAIR(id) $p
    set PAIR(uuid) [::uuid::uuid generate]
    set PAIR(trigger) ""
    set PAIR(object) $obj
    set PAIR(robject) ""
    set PAIR(inited) 0
    set PAIR(fields:context) {}
    set PAIR(fields:remote) {}
    set PAIR(state) ""
    set PAIR(scheduler) ""
    set PAIR(ruuid) [lindex [split $remote "/"] end]
    if { $root eq "" } {
	set PAIR(remote) "";   # Remote really is ourselves!
    } else {
	set PAIR(remote) [::cxapi::find $root]
	if { $PAIR(remote) eq "" } {
	    set PAIR(remote) [::cxapi::new -url $root -timeout $RCX(-timeout)]
	}
    }
    switch $destination {
	remote -
	rcontext {
	    set PAIR(-destination) "remote"
	    set PAIR(-to) [string trimright [dict get $qry remote] "/"]
	    set PAIR(-from) $uuid
	    if { [::schema::check UUID $PAIR(-to)] } {
		foreach {PAIR(robject) -} [::find:uuid $PAIR(-to) object] break
		${log}::debug "Remote object really is local object\
                               $PAIR(robject) with uuid $PAIR(-to)"
	    }
	}
	context -
	local {
	    set PAIR(-destination) "context"
	    set PAIR(-from) [string trimright [dict get $qry remote] "/"]
	    set PAIR(-to) $uuid
	    if { [::schema::check UUID $PAIR(-from)] } {
		foreach {PAIR(robject) -} [::find:uuid $PAIR(-from) object] \
		    break
		${log}::debug "Remote object really is local object\
                               $PAIR(robject) with uuid $PAIR(-from)"
	    }
	}
    }
    set PAIR(-frequency) $RCX(-frequency)
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

    return [json:serialize $p [list uuid -destination -to -from -translations\
				   -polling]]
}


proc ::rcontext::__init {} {
    variable RCX
    variable log

    if { ! $RCX(initdone) } {
	${log}::debug "First time initialisation of rcontext conduit"
	if { [catch {package require tls} err] } {
	    ${log}::error "Will not be able to communicate securely!\
                           (reason: $err)"
	    set RCX(https) 0
	} else {
	    ::http::register https 443 [list ::tls::socket]
	    set RCX(https) 1
	}

	set RCX(initdone) 1
    }
}


proc ::rcontext::init { cx root srv } {
    variable RCX
    variable log

    __init

    set RCX(context) $cx
    lappend RCX(servers) $srv $root

    set hex "\[a-fA-F0-9\]"
    set uuid "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]" 

    set root [string trimright $root "/"]
    ::minihttpd::handler $srv $root/$uuid ::rcontext::rest:pair \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/pair ::rcontext::rest:pair \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/pair/ ::rcontext::rest:pair \
	"application/json"
    ::minihttpd::handler $srv $root ::rcontext::rest:pairs \
	"application/json"
    ::minihttpd::handler $srv $root/ ::rcontext::rest:pairs \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/add ::rcontext::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/add/ ::rcontext::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/remove ::rcontext::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/remove/ ::rcontext::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/copy ::rcontext::rest:copy \
	"application/json"
    
    ${log}::notice "Registered REsT entry points in server at port #$srv"
}
