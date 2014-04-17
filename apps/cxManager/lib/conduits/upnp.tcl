package require http
package require uuid
package require uobj
package require ssdp
package require UPnP

namespace eval ::upnp {
    variable UPNP
    if { ![info exists UPNP] } {
	array set UPNP {
	    context      ""
	    https        0
	    UPnP         ""
	    -frequency   180
	    -timeout     10000
	    -set         Set
	    -get         Get
	    -retry       10
	}
	::uobj::install_log upnp UPNP
	::uobj::install_defaults upnp UPNP
    }
}


proc ::upnp::__receive { r f argname a args } {
    variable UPNP
    variable log

    if { ! [::uobj::isa $r remote] } {
	${log}::warn "$r unknown or wrong type"
	return -code error "$r unknown or wrong type"
    }
    upvar \#0 $r REMOTE
    upvar \#0 $REMOTE(pair) PAIR

    foreach {k v} $args {
	if { [string trimleft $k -] eq $argname } {
	    set REMOTE(-$f) $v
	}
    }

    # Count the number of received fields.
    set nb_fields 0
    foreach f $REMOTE(fields) {
	if { [array names REMOTE -$f] ne "" } {
	    incr nb_fields
	}
    }
    ${log}::debug "$nb_fields fields received out of [llength $REMOTE(fields)]"

    # If we have received all fields, perform the copy into the context
    if { $nb_fields >= [llength $REMOTE(fields)] } {
	::pair:receive $PAIR(object) $r $PAIR(-translations)
	::uobj::delete $r
	
	if { $PAIR(state) eq "COPYING" } {
	    pair:tellstate $PAIR(id) COPIED
	}
    }
}


proc ::upnp::pair:__copy_once { p } {
    variable UPNP
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    switch $PAIR(-destination) {
	upnp {
	    set upnp $PAIR(-to)
	    set uuid $PAIR(-from)

	    set u [::uobj::new [namespace current] remote [::uobj::id $p]]
	    upvar \#0 $u REMOTE
	    ::pair:receive $u $PAIR(object) $PAIR(-translations)
	    foreach k [array names REMOTE -*] {
		# Arrange for action to be the identifier of the
		# action that contains PAIR(-set) i.e. a keyword that
		# should be contained in the UPnP method to call to
		# set the variable.
		set action ""
		set srv [lindex [$PAIR(cp) get services] end]
		set k [string trimleft $k -]
		foreach a [$srv get actions] {
		    foreach arg [$a get arguments] {
			upvar \#0 $arg ARGUMENT
			if { $ARGUMENT(-relatedStateVariable) eq $k \
				 && [string first \
					 $PAIR(-set) [$a get -name]] >= 0 \
				 && $ARGUMENT(-direction) eq "in" } {
			    set action $a
			    set argname $ARGUMENT(-name)
			    break
			}
		    }
		}
		
		if { $action eq "" } {
		    ${log}::warn "Could not find any action containing the\
                                  keyword $PAIR(-set) to set '$k'"
		} else {
		    ${log}::debug "Using action [$action get -name] to set $k"
		    $action call -- -$argname $REMOTE(-$k)
		}
	    }
	    ::uobj::delete $u
	    if { $PAIR(state) eq "COPYING" } {
		pair:tellstate $p COPIED
	    }
	}
	context {
	    set upnp $PAIR(-from)
	    set uuid $PAIR(-to)
	    ${log}::debug "Getting current sub-content of UPnP device\
                           '$upnp'"

	    # Create a remote object that will contain the value of
	    # the values from the UPnP device that are necessary for
	    # this translation.
	    set r [::uobj::new [namespace current] remote [::uobj::id $p]]
	    ::uobj::keyword $r cacheid $PAIR(uuid)
	    upvar \#0 $r REMOTE

	    set REMOTE(id) $r
	    set REMOTE(pair) $p
	    set REMOTE(fields) {}
	    foreach {dst src} $PAIR(-translations) {
		set src_fields [::pair:extract $src]
		foreach f $src_fields {
		    lappend REMOTE(fields) $f
		}
	    }
	    set REMOTE(fields) [lsort -unique $REMOTE(fields)]
	    ${log}::debug "This copy requires the value of the following\
                           fields: [join $REMOTE(fields) ,]"

	    set srv [lindex [$PAIR(cp) get services] end]
	    foreach f $REMOTE(fields) {
		set action ""
		foreach a [$srv get actions] {
		    foreach arg [$a get arguments] {
			upvar \#0 $arg ARGUMENT
			if { $ARGUMENT(-relatedStateVariable) eq $f \
				 && [string first \
					 $PAIR(-get) [$a get -name]] >= 0 \
				 && $ARGUMENT(-direction) eq "out" } {
			    set action $a
			    set argname $ARGUMENT(-name)
			    break
			}
		    }
		}

		if { $action eq "" } {
		    ${log}::warn "Could not find any action containing the\
                                  keyword '$PAIR(-get)' to retrieve $f!"
		} else {
		    ${log}::debug "Using action [$action get -name] to copy $f"
		    $action call [list ::upnp::__receive $r $f $argname]
		}
	    }
	}
    }
}


proc ::upnp::pair:bind { p } {
    variable UPNP
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
	upnp {
	    set feed $PAIR(-to)
	    set uuid $PAIR(-from)
	    
	    foreach {srv root} $UPNP(servers) {
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


proc ::upnp::pair:check { p } {
    variable UPNP
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
	    upnp {
		foreach f [::pair:extract $lft] {
		    if { [lsearch $PAIR(fields:upnp) $f] < 0 } {
			${log}::error "Field '$f' used in translation does\
                                         not exist in UPnP object at\
                                         $PAIR(-to)"
			set valid 0
		    }
		}
		foreach f [::pair:extract $rgt] {
		    if { [lsearch $PAIR(fields:context) $f] < 0 } {
			${log}::error "Field '$f' used in translation does\
                                         not exist in context object\
                                         $PAIR(-from)!"
			set valid 0
		    }
		}
	    }
	    context {
		foreach f [::pair:extract $lft] {
		    if { [lsearch $PAIR(fields:context) $f] < 0 } {
			${log}::error "Field '$f' used in translation does\
                                         not exist in context objcet\
                                         $PAIR(-from)!"
			set valid 0
		    }
		}
		foreach f [::pair:extract $rgt] {
		    if { [lsearch $PAIR(fields:upnp) $f] < 0 } {
			${log}::error "Field '$f' used in translation does\
                                         not exist in UPnP object at\
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
	foreach f $PAIR(fields:upnp) {
	    if { [lsearch $PAIR(fields:context) $f] >= 0 } {
		lappend PAIR(-translations) %$f% %$f%
	    }
	}
    }
}

proc ::upnp::pair:tellstate { p { state "" } { when "idle" } } {
    variable UPNP
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


proc ::upnp::pair:state { p } {
    variable UPNP
    variable log

    if { ! [::uobj::isa $p pair] } {
	${log}::warn "$p unknown or wrong type"
	return -code error "$p unknown or wrong type"
    }
    upvar \#0 $p PAIR

    set uuid ""; set feed ""
    switch $PAIR(-destination) {
	upnp {
	    set uuid $PAIR(-from)
	    set upnp $PAIR(-to)
	    ${log}::debug "State machine: $uuid -> $upnp -- $PAIR(state)"
	}
	context {
	    set uuid $PAIR(-to)
	    set upnp $PAIR(-from)
	    ${log}::debug "State machine: $upnp -> $uuid -- $PAIR(state)"
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

	    set PAIR(cp) ""
	    if { [string range $upnp 0 3] eq "http" } {
		set PAIR(cp) [$UPNP(UPnP) add $upnp]
	    } else {
		set PAIR(cp) [$UPNP(UPnP) find device $upnp]
	    }
	    if { $PAIR(cp) eq "" } {
		pair:tellstate $p $PAIR(state) $PAIR(-retry)
	    } else {
		pair:tellstate $p SERVICE
	    }
	}
	"SERVICE" {
	    set srv [lindex [$PAIR(cp) get services] end]
	    if { $srv ne "" } {
		set PAIR(fields:upnp) {}
		foreach v [$srv get variables] {
		    upvar \#0 $v VAR
		    lappend PAIR(fields:upnp) $VAR(-name)
		}
		if { [llength $PAIR(fields:upnp)] > 0 } {
		    ${log}::notice "Available fields in UPnP device are:\
                                    [join $PAIR(fields:upnp) ,]"
		    pair:tellstate $p INIT
		    return;  # Make sure we don't reach default case below
		}
	    }
	    pair:tellstate $p $PAIR(state) $PAIR(-retry)
	}
	"INIT" {
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
	    ${log}::error "Dead end in state machine"
	}
    }
}

proc ::upnp::rest:copy { prt sock url qry } {
    variable UPNP
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


proc ::upnp::rest:translation { prt sock url qry } {
    variable UPNP
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
	pair:tellstate $p INIT;   # Need to refetch details!
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
		     -frequency -destination -to -from -tranlations -polling]]
}


proc ::upnp::rest:pairs { prt sock url qry } {
    variable UPNP
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
				  -frequency -destination -to -from \
				  -tranlations -polling]],
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

    return [json:serialize $p \
		[list uuid \
		     -frequency -destination -to -from -tranlations -polling]]
}


proc ::upnp::rest:pair { prt sock url qry } {
    variable UPNP
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
    
    if { ![dict exists $qry device] } {
	${log}::warn "No device UPnP specification, aborting"
	return ""
    }
    foreach {obj cls} [::find:uuid $uuid object] break
    if { $obj eq "" } {
	${log}::warn "$uuid does not to any existing object in context"
	return ""
    }

    set destination upnp
    if { [dict exists $qry destination] } {
	set destination [string tolower [dict get $qry destination]]
	if { [lsearch {upnp context} $destination] < 0 } {
	    ${log}::warn "$destination is not a recognised type for destination"
	    return ""
	}
    }

    set retry $UPNP(-retry)
    if { [dict exists $qry retry] } {
	set retry [dict get $qry retry]
    }
    
    set setter $UPNP(-set)
    if { [dict exists $qry set] } {
	set setter [dict get $qry set]
    }
    set getter $UPNP(-get)
    if { [dict exists $qry get] } {
	set getter [dict get $qry get]
    }
    

    set p [::uobj::new [namespace current] pair]
    upvar \#0 $p PAIR
    set PAIR(id) $p
    set PAIR(uuid) [::uuid::uuid generate]
    set PAIR(trigger) ""
    set PAIR(inited) 0
    set PAIR(scheduler) ""
    set PAIR(-set) $setter
    set PAIR(-get) $getter
    set PAIR(-retry) $retry
    set PAIR(object) $obj
    set PAIR(fields:context) {}
    set PAIR(fields:upnp) {}
    set PAIR(state) ""
    switch $destination {
	upnp {
	    set PAIR(-destination) $destination
	    set PAIR(-to) [string trim [dict get $qry device]]
	    set PAIR(-from) $uuid
	}
	context -
	local {
	    set PAIR(-destination) "context"
	    set PAIR(-from) [string trim [dict get $qry device]]
	    set PAIR(-to) $uuid
	}
    }
    set PAIR(-translations) {}
    if { [dict exists $qry translations] } {
	foreach {rcv xpr} [dict get $qry translations] {
	    lappend PAIR(-translations) [string trim $rcv] [string trim $xpr]
	}
    }
    set PAIR(-frequency) $UPNP(-frequency)
    if { [dict exists $qry frequency] } {
	set frequency [dict get $qry frequency]
	if { [string is integer $frequency] } {
	    set PAIR(-frequency) $frequency
	} else {
	    ${log}::warn "Frequency $frequency not an integer, keeping default"
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
		     -frequency -destination -to -from -tranlations -polling]]
}


proc ::upnp::__init {} {
    variable UPNP
    variable log

    if { $UPNP(UPnP) eq "" } {
	${log}::debug "First time initialisation of UPnP conduit"
	if { [catch {package require tls} err] } {
	    ${log}::error "Will not be able to communicate securely!\
                           (reason: $err)"
	} else {
	    set UPNP(https) 1
	    ::http::register https 443 [list ::tls::socket]
	}
	set UPNP(UPnP) [::UPnP::new [::ssdp::new]]
    }    
}


proc ::upnp::init { cx root srv } {
    variable UPNP
    variable log

    __init

    set UPNP(context) $cx
    lappend UPNP(servers) $srv $root

    set hex "\[a-fA-F0-9\]"
    set uuid "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]" 

    set root [string trimright $root "/"]
    ::minihttpd::handler $srv $root/$uuid ::upnp::rest:pair \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/pair ::upnp::rest:pair \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/pair/ ::upnp::rest:pair \
	"application/json"
    ::minihttpd::handler $srv $root ::upnp::rest:pairs \
	"application/json"
    ::minihttpd::handler $srv $root/ ::upnp::rest:pairs \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/add ::upnp::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/add/ ::upnp::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/remove ::upnp::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/remove/ ::upnp::rest:translation \
	"application/json"
    ::minihttpd::handler $srv $root/copy ::upnp::rest:copy \
	"application/json"
    
    ${log}::notice "Registered REsT entry points in server at port #$srv"
}
