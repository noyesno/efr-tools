##################
## Module Name     --  dna
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##      This module misuses slightly the idea of conduits in order to
##      implement device DNA, i.e. the ability for guessing which
##      devices are consuming power given a single power meter.
##
##################

# TODO
#
# Add API entries to add and remove routes dynamically
#
# Cut down main procedure to make it easier to read and more modular.

package require Tcl
package require http
package require uuid


namespace eval ::dna {
    variable DNA
    if {![info exists DNA] } {
	array set DNA {
	    context    ""
	    servers    {}
	    initdone   0
	    -routes    ""
	    -precision 10.0
	}
	::uobj::install_log dna DNA
	::uobj::install_defaults dna DNA
    }
}

proc ::dna::__allroutes { l } {
    set len [llength $l]
    if { $len == 1 } {
	return [list $l {}]
    }
    set res {}
    for {set i 0} {$i < $len} {incr i} {
	set remaining [lsort -unique [lrange $l [expr $i +1] end]]
	set elem [lindex $l $i]
	
	foreach under [__allroutes $remaining] {
	    lappend res [concat $elem $under] $under
	}
    }
    return [lsort -unique $res]
}

proc ::dna::__set { uuid field value } {
    variable DNA
    variable log

    ${log}::debug "Setting $field to $value in $uuid"
    foreach {obj cls} [::find:uuid $uuid object] break
    if { $obj ne "" } {
	upvar \#0 $obj OBJ
	if { [array names OBJ -$field] ne "" } {
	    set OBJ(-$field) $value
	} elseif { [array names OBJ $field] ne "" } {
	    set OBJ($field) $value
	} else {
	    ${log}::warn "No field $field in destination $uuid"
	}
    }
}


# ::dna::__change -- Perform DNA mapping for route
#
#       By construction, this is called whenever the value of the
#       field in the source object is changed.  The procedure performs
#       the DNA mapping intelligence to the best of its capacity and
#       arranges for the relevant destination objects to receive
#       proper values.
#
# Arguments:
#	r	Identifier of the route object
#	value	Current value, no argument means take it from source object.
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::dna::__change { r {value "-=- NoTSetVALueGETfroMobJeCtInsTEAD -=-"} } {
    variable DNA
    variable log

    # Check r is really one of our routes
    if { ![::uobj::isa $r route] } {
	${log}::warn "$r unknown or wrong type"
	return -code error "$r unknown or wrong type"
    }
    upvar \#0 $r ROUTE

    # If no value argument was specified (strange string to allow
    # empty values), get the current content of the field from the
    # source object.
    if { $value eq "-=- NoTSetVALueGETfroMobJeCtInsTEAD -=-" } {
	upvar \#0 $ROUTE(object) OBJ
	if { [array names OBJ -$ROUTE(-field)] ne "" } {
	    set value [set OBJ(-$ROUTE(-field))]
	} elseif { [array names OBJ $ROUTE(-field)] ne "" } {
	    set value [set OBJ($ROUTE(-field))]
	} else {
	    ${log}::warn "Cannot find $ROUTE(-field) in source object!"
	    return
	}
    }

    ${log}::debug "Received value $value for $ROUTE(-field) in\
                   $ROUTE(-source), detecting causes"
	
    # Pick in the list dests the UUID of the possible destinations.
    set dests {}
    foreach {uuid v} $ROUTE(-destinations) {
	lappend dests $uuid
    }

    # Arrange for possibilities to contain all possible
    # combinations of additions that would lead to the value,
    # approximately (take the precision into account).
    set possibilities {}
    set max_common 0
    foreach possibility [__allroutes $dests] {
	set route_val 0.0
	set from_prev 0
	foreach uuid $possibility {
	    set idx [lsearch $ROUTE(-destinations) $uuid]
	    set dst_val [lindex $ROUTE(-destinations) [expr {$idx + 1}]]
	    set route_val [expr {$route_val+$dst_val}]
	    if { [lsearch $ROUTE(chain) $uuid] >= 0 } {
		incr from_prev
	    }
	}
	set diff [expr {$DNA(-precision)*$route_val/100.0}]
	if { $value >= [expr {$route_val-$diff}] \
		 && $value <= [expr {$route_val+$diff}] } {
	    lappend possibilities $possibility $route_val $from_prev
	    if { $from_prev > $max_common } {
		set max_common $from_prev
	    }
	}
    }
    
    # Retain the ones that have most UUIDs in common with previous
    # state
    set retained {}
    set min_len [llength $dests]
    foreach { possibility route_val from_prev } $possibilities {
	if { $from_prev == $max_common } {
	    lappend retained $possibility $route_val
	    if { [llength $possibility] < $min_len } {
		set min_len [llength $possibility]
	    }
	}
    }

    # Retain only the smallest ones, i.e. the ones with the
    # minimum number of devices.
    set possibilities $retained
    set retained {}
    foreach { possibility route_val } $possibilities {
	if { [llength $possibility] == $min_len } {
	    lappend retained $possibility $route_val
	}
    }
    
    # And pick in <switched> the solution that is closest.
    set switched {}
    set distance [expr abs($value)]
    foreach { possibility route_val } $retained {
	if { [expr {abs($route_val-$value)}] < $distance } {
	    set switched $possibility
	}
    }

    # Remember current state for next time
    set ROUTE(chain) $switched
    
    # Now set the value of the objects that were deemed to be on, and
    # turn off (0 all the other ones).
    foreach uuid $switched {
	set idx [lsearch $ROUTE(-destinations) $uuid]
	set dst_val [lindex $ROUTE(-destinations) [expr {$idx+1}]]
	if { [::schema::check UUID $uuid] } {
	    __set $uuid $ROUTE(-field) $dst_val
	}
    }
    foreach {uuid v} $ROUTE(-destinations) {
	if { [lsearch $switched $uuid] < 0 } {
	    if { [::schema::check UUID $uuid] } {
		__set $uuid $ROUTE(-field) 0
	    }
	}
    }
}


# ::dna::__routes -- Create route objects at init
#
#       Creates all routing objects given the initial route mapping.
#
# Arguments:
#       None.
#
# Results:
#       None.
#
# Side Effects:
#       Will establish all necessary triggers, arranging for a
#       procedure to be called whenever the value of the source
#       changes.
proc ::dna::__routes {} {
    variable DNA
    variable log

    foreach {uuid field destinations} $DNA(-routes) {
	# Find source object in context, otherwise we cannot build any
	# route.
	foreach {obj cls} [::find:uuid $uuid object] break
	if { $obj eq "" } {
	    ${log}::warn "$uuid does not identify any existing obj in context"
	} else {
	    set r [::uobj::new [namespace current] route]
	    upvar \#0 $r ROUTE

	    set ROUTE(id) $r
	    set ROUTE(uuid) [::uuid::uuid generate]
	    set ROUTE(object) $obj
	    set ROUTE(-source) $uuid
	    set ROUTE(-field) $field
	    set ROUTE(-destinations) $destinations
	    set ROUTE(chain) {}
	    # Arrange for internal procedure to be called whenever the
	    # field changes.
	    set rcv "cmd:[namespace current]::__change $r %$ROUTE(-field)%"
	    set ROUTE(trigger) [::trigger:new $ROUTE(object) \
				    [list \
					 receiver $rcv]]
	    __change $r;  # Trigger once at start, we could perhaps use
	                  # ::trigger:force instead?
	}
    }
}


# ::dna::rest:copy -- Copy/Test a route
#
#       Force the execution of the DNA algorithm on a particular route
#       to detect which devices are turned on given a single power
#       input.
#
# Arguments:
#	port	Port number of web server
#	sock	Socket to client
#	url	URL being called
#	qry	Query data
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::dna::rest:copy { prt sock url qry } {
    variable DNA
    variable log

    array set RESULTS {
	true     "\{\"result\":true\}"
	false     "\{\"result\":false\}"
    }

    # Return false result if no UUID is present in the query or in the
    # URL.
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

    # Return false again if the UUID is not refering to a route,
    # otherwise perform the DNA test and return true.
    set r [::uobj::find [namespace current] route \
	       [list uuid == $uuid]]
    if { $r eq "" } { return $RESULTS(false) }

    if { [__change $r] } {
	return $RESULTS(true)
    } else {
	return $RESULTS(false)
    }
}


# ::dna::rest:routes -- Describe existing routes
#
#       Describe the set of existing routes, i.e. of existing DNA
#       mappings that can, from the power used by a device, guess
#       which devices are turned on.
#
# Arguments:
#	port	Port number of web server
#	sock	Socket to client
#	url	URL being called
#	qry	Query data
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::dna::rest:routes { prt sock url qry } {
    variable DNA
    variable log

    # Extract a UUID, either from the query, either from the URL that
    # was used to call (this is by construction, see ::dna::init).  If
    # we have return information for that route only, as long as it
    # exists.  Otherwise, return information for all existing routes.
    if { [dict exists $qry uuid] } {
	set uuid [dict get $qry uuid]
    } else {
	set uuid [lindex [split [string trimright $url "/"] "/"] end-1]
	if { ! [::schema::check UUID $uuid] } {
	    set uuid [lindex [split [string trimright $url "/"] "/"] end]
	    if { ! [::schema::check UUID $uuid] } {
		# No UUID was ever found, return info for all routes!
		set result "\["
		foreach r [::uobj::allof [namespace current] route] {
		    append result \
			[json:serialize $r \
			     [list uuid \
				  -source -field -destinations]],
		}
		set result [string trimright $result ","]
		append result "\]"

		return $result
	    }
	}
    }

    # We have a single UUID, return information for that route only.
    set r [::uobj::find [namespace current] route [list uuid == $uuid]]
    if { $r eq "" } { return "\{\}" }

    return [json:serialize $r \
		[list uuid \
		     -source -field -destinations]]
}


# ::dna::__init -- Internal initialisation, establishing linking routes
#
#       Initialise the internals of the conduit, making sure to read
#       the linking directives from either the file specified or from
#       the "command line" arguments, i.e. the -route option coming
#       from the defaults options.
#
# Arguments:
#       None.
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::dna::__init {} {
    variable DNA
    variable log

    if { !$DNA(initdone) } {
	${log}::debug "First time initialisation of dna conduit"
	
	# Read routes from file if told so, otherwise take route list
	# from argument.
	set DNA(-routes) [string trim $DNA(-routes)]
	if { [string index $DNA(-routes) 0] eq "@" } {
	    set fname [::diskutil::fname_resolv \
			   [string range $DNA(-routes) 1 end]]
	    ${log}::info "Reading routing info from $fname..."
	    set DNA(-routes) [::diskutil::lread $fname 3 "routing file"]
	}

	# Build all necessary route state objects and remember we've
	# initialised to skip it doing twice (or more!)
	__routes
	set DNA(initdone) 1
    }
}


# ::dna::init -- Initialise conduit
#
#       Initialising the conduit by creating entry points for the REsT
#       API that it supports.
#
# Arguments:
#	cx	Identifier of the context hosting the conduit
#	root	Root path/URL to where the conduit API should exist
#	srv	Identifier of the web server
#
# Results:
#       None.
#
# Side Effects:
#       Will establish the necessary links between sources and
#       destinations.
proc ::dna::init { cx root srv } {
    variable DNA
    variable log

    # Remember where we are placed into
    set DNA(context) $cx
    lappend DNA(servers) $srv $root

    # Initialise the conduits, incl. routes 
    __init
    
    # Register API entry points.
    set hex "\[a-fA-F0-9\]"
    set uuid "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]" 

    set root [string trimright $root "/"]
    ::minihttpd::handler $srv $root/$uuid ::dna::rest:routes "application/json"
    ::minihttpd::handler $srv $root/$uuid/route ::dna::rest:routes \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/route/ ::dna::rest:routes \
	"application/json"

    ::minihttpd::handler $srv $root ::dna::rest:routes "application/json"
    ::minihttpd::handler $srv $root/ ::dna::rest:routes "application/json"

    ::minihttpd::handler $srv $root/$uuid/test ::dna::rest:copy \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/test/ ::dna::rest:copy \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/copy ::dna::rest:copy \
	"application/json"
    ::minihttpd::handler $srv $root/$uuid/copy/ ::dna::rest:copy \
	"application/json"
}
