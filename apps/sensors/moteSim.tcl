# Implements a contiki me3gas mote simulator, supporting the same
# protocol as used withint the project.

set options {
    { logfile.arg "%APPDATA%/me3gas/%progname%.log" "Where to save log for every run" }
    { port.arg "9900" "Port to listen on for incoming HTTP connections" }
    { features.arg "time cpu" "List of featured (simulated) sensors" }
    { sampling.integer "1000" "Sampling rate" }
}

array set MSIM {
    logfd       ""
    debug       0
    timeout     40000
    udp_order   {udp ceptcl}
    udp         ""
    pusher      ""
}


source [file join [file dirname $argv0] lib init.tcl]
if { [string is true $MSIM(debug)] } {
    # Add debug port to something sensible and arrange to access
    # additional local libraries for when running from within a
    # dynamic debugger (RamDebugger).
    ::init::debughelper
}

# ::init:fix -- Fix packages depending on platform
#
#       This procedure is called during early initialisation and will,
#       on windows, arrange to load the registry package.  If a UI is
#       requested, it brings up a number of extra packages together
#       with a splash window that is shown until initialisation has
#       completed.
#
# Arguments:
#       glbl	Name of the global array, NP by construction
#       args	Arguments that have been passed to initialisation
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::init:fix { glbl args } {
    global MSIM
    global tcl_platform

    upvar \#0 $args ARGS
    if { $tcl_platform(platform) eq "windows" } {
	lappend ARGS(-packages) registry
    }
}


# ::log:out -- Output log to file and log window
#
#       This procedure is registered as a callback of the log module
#       and will both output to the log file and the log window,
#       whenever relevant.
#
# Arguments:
#       dt	Formated date
#       srv	Module within which the event occurs.
#       lvl	Level of severity
#       str	String description of the event
#
# Results:
#       Always return 1 so that the log module continues forwarding
#       the event to other registered procedures.
#
# Side Effects:
#       None.
proc log:out {dt srv lvl str} {
    global MSIM

    set logline "\[$dt\] \[$srv\] \[$lvl\] $str"

    # Return if no logfile specified
    if { $MSIM(logfile) eq "" } {
	return 1
    }

    if { $MSIM(logfd) eq "" } {
	set fname [file normalize [::diskutil::fname_resolv $MSIM(logfile)]]
	set d [file dirname $fname]
	if { ! [file isdirectory $d] } {
	    file mkdir $d
	}
	if { ! [catch {open $fname w+} fd] } {
	    set MSIM(logfd) $fd
	}
    }
    if { $MSIM(logfd) ne "" } {
	puts $MSIM(logfd) $logline
    }
    return 1
}


# Initialise everything in one go...
::init::init \
    -store MSIM \
    -options $options \
    -booleans [list] \
    -depends [list progver event rest udp] \
    -load [list minihttpd websocket] \
    -packages [list rest ip uri base64 http] \
    -parsed ::init:fix \
    -outlog ::log:out

# Find an appropriate UDP implementation, if possible
foreach udp $MSIM(udp_order) {
    if { [catch {package require $udp} ver] == 0 } {
	$MSIM(log)::notice "UDP support via $udp at version $ver"
	set MSIM(udp) $udp
	break
    }
}
if { $MSIM(udp) eq "" } {
    $MSIM(log)::error "No support for UDP found"
}


# ::json:to_dict -- Convert JSON expression to a Tcl dictionary
#
#       This procedure converts a JSON expression to a Tcl
#       dictionary. It is ripped off from the wiki and is simpler (and
#       quicker) than the one that exists in the tcllib.
#
# Arguments:
#       json    JSON expression
#
# Results:
#       Returns a string that can be used as a dictionary.
#
# Side Effects:
#       None.
proc ::json:to_dict {json} {
    string range [
	string trim [
	    regsub -- {^(\uFEFF)} [
		string map {\t {} \n {} \r {} , { } : { } \[ \{ \] \}} $json
		] {}
	    ]
	] 1 end-1
}



# ::json:from_dict -- Convert a dictionary to a JSON expression
#
#	Convert a dictionary to a JSON expression, this is ripped off
#	the wiki and quicker, but not as complete as the implemenation
#	that comes with the JSON library.
#
# Arguments:
#	dctnary	Dictionary to traverse for conversion
#
# Results:
#	A well-formatted JSON expression
#
# Side Effects:
#	None.
proc ::json:from_dict {dctnary} {
    dict for {key value} $dctnary {
	if {[string match {\[*\]} $value]} {
	    lappend Result "\"$key\":$value"
	} elseif {![catch {dict size value}]} {
	    lappend Result "\"$key\":[::json:from_dict $value]"
	} else {
	    lappend Result "\"$key\":\"$value\""
	}
    }
    return "\{[join $Result ","]\}"
}


proc ::to_rfc3339 { { tstamp "" } { unit us } { variant "" } } {
    # Normalise the variant so that not specifying it will mean using
    # the same unit as the one in the incoming timstamp.
    if { $variant eq "" } {
	set variant $unit
    }

    # Convert incoming timestamp to always be microseconds, arrange
    # for understanding empty timestamps as "now" to the best our
    # capability (this is backward compatible with earlier versions of
    # Tcl).
    if { $tstamp eq "" } {
	# When we have an empty timestamp, we decide ourselves the
	# unit. Try first the Tcl 8.5 way, otherwise return to the old
	# [clock clicks] from days prior to Tcl 8.5.
	if { [catch {clock microseconds} tstamp] } {
	    set tstamp [clock clicks]
	    set unit s
	} else {
	    set unit us
	}
    } else {
	switch $unit {
	    us {
	    }
	    ms {
		set us [expr {$tstamp * 1000}]
	    }
	    s {
		set us [expr {$tstamp * 1000000}]
	    }
	}
    }

    # Now convert to the RFC3339 format, making sure to support the
    # proper number of decimals in the output, i.e. following the
    # variant specification.
    set secs [expr {$tstamp / 1000000}]
    set micro [expr {$tstamp % 1000000}]
    set ts [clock format $secs -format "%Y-%m-%dT%T"]
    regexp {(...)(..)} [clock format $secs -format "%z"] matched tzh tzm
    switch $variant {
	"us" {
	    return [format "%s.%06s%s:%s" $ts $micro $tzh $tzm]
	}
	"ms" {
	    return [format "%s.%03s%s:%s" $ts [expr {$micro/1000}] $tzh $tzm]
	}
	"s" {
	    return [format "%s%s:%s" $ts $tzh $tzm]
	}
    }

    return -code error "Cannot convert incoming $tstamp to RFC3339, '$variant'\
                        is an unsupported type of fractions of seconds."
}

proc ::feature:get { f } {
    global tcl_platform
    global MSIM
    
    switch -glob -nocase -- $f {
	"t*" {
	    return [::to_rfc3339]
	}
	"c*" {
	    if { $tcl_platform(platform) eq "windows" } {
		set wmic [auto_execok wmic]
		if { [catch {exec $wmic cpu get loadpercentage} res] } {
		    $MSIM(log)::error "Cannot execute wmic: $res"
		    return 0.0
		} else {
		    set pcent [string trim [lindex [split $res "\n"] 2]]
		    return [format %.2f $pcent]
		}
	    } elseif { $tcl_platform(platform) eq "unix" } {
		if { [catch {exec top -b -d 00.50 -n 2} res] } {
		    $MSIM(log)::error "Cannot execute top: $res"
		} else {
		    foreach l [split $res "\n"] {
			if { [string match "Cpu*" $l] } {
			    foreach p [split $l ","] {
				if { [string match "*us*" $p] } {
				    set cpu [lindex [string trim $p] end]
				    set cpu [regsub %.* $cpu ""]
				}
			    }
			}
		    }
		    return $cpu
		} 
	    }
	}
	default {
	    $MSIM(log)::warn "$f is an unknown feature!"
	    return 0.0
	}
    }
}

proc ::feature:update { { filter * } } {
    global MSIM

    set updated {}
    foreach f $MSIM(features) {
	if { [string match -nocase $filter $f] } {
	    set MSIM(feature:$f) [dict create \
				      "value" [::feature:get $f]]
	    lappend updated $f
	}
    }

    return $updated
}


proc ::json:status { { filter * } } {
    global MSIM

    set res ""
    foreach f $MSIM(features) {
	if { [string match -nocase $filter $f] } {
	    append res "\"$f\":[::json:from_dict $MSIM(feature:$f)],"
	}
    }
    set res [string trimright $res ,]

    return $res
}


proc ::rest:status { prt sock url qry } {
    global MSIM
    
    ::feature:update

    set res "\{"
    append res "\"node\":[::json:from_dict $MSIM(sim:node)],"
    append res "\"cfg\":[::json:from_dict $MSIM(sim:cfg)],"
    append res "\"rsc\":{[::json:status]}"
    append res "\}"

    return $res
}

proc ::rest:node { prt sock url qry } {
    global MSIM

    return [::json:from_dict $MSIM(sim:node)]
}


proc ::net:shutdown {} {
    global MSIM

    # Cleanup live data, if any.
    if { $MSIM(ws:sock) ne "" } {
	::websocket::close $MSIM(ws:sock)
	set MSIM(ws:sock) ""
    }

    # Remove current data pusher, since we might have changed the
    # frequency
    if { $MSIM(pusher) ne "" } {
	after cancel $MSIM(pusher)
    }
}

proc ::rest:cfg { prt sock url qry } {
    global MSIM

    set changed 0
    set dta [::json:to_dict [::minihttpd::data $prt $sock]]
    dict for {key value} $MSIM(sim:cfg) {
	if { [dict exists $dta $key] } {
	    set changed 1
	    dict set MSIM(sim:cfg) $key [dict get $dta $key]
	}
    }

    if { $changed } {
	::net:shutdown

	# (re)open websocket to server if necessary.
	if { [string match -nocase "w*" [dict get $MSIM(sim:cfg) proto]] } {
	    ::ws:open
	}

	# Install new data pusher at proper interval if necessary
	if { [dict get $MSIM(sim:cfg) interval] ne "" } {
	    if { [dict get $MSIM(sim:cfg) interval] > 0 } {
		set next [expr {int([dict get $MSIM(sim:cfg) interval]*1000)}]
		set MSIM(pusher) [after $next ::net:push]
	    }
	}
    }

    return [::json:from_dict $MSIM(sim:cfg)]
}


proc ::rest:feature { prt sock url qry } {
    global MSIM

    set f [lindex [split $url "/"] end]
    if { [lsearch $MSIM(features) $f] >= 0 } {
	::feature:update $f
	return [::json:status $f]
    }
    return "\{\}"
}


proc ::ws:rcv { sock type msg } {
    global MSIM


    switch -glob -nocase -- $type {
	co* {
	    # Remember socket on connection
	    set MSIM(ws:sock) $sock
	}
	te* {
	    # What do we do??!
	}
	cl* -
	dis* {
	    ::net:shutdown
	}
    }
}


proc ::ws:open {} {
    global MSIM

    set host [dict get $MSIM(sim:cfg) host]
    if { $host ne "" } {
	set port [dict get $MSIM(sim:cfg) port]
	if { $port eq "" } { set port 80 }
	set path [dict get $MSIM(sim:cfg) path]
	set path [string trimleft $path "/"]
	set rcv "ws://${host}:${port}/${path}"
	set cmd [list ::websocket::open $rcv ::ws:rcv]
	if { $MSIM(timeout) > 0 } {
	    lappend cmd -timeout $MSIM(timeout)
	}
	if { [catch {eval $cmd} token] } {
	    $MSIM(log)::error "Error while opening WebSocket at $rcv: $token"
	} else {
	    $MSIM(log)::notice "Opening WebSocket connection to $rcv"
	}
    }
}


proc ::net:push {} {
    global MSIM

    switch -glob -nocase -- [dict get $MSIM(sim:cfg) proto] {
	"u*" {
	}
	"w*" {
	    # Open WebSocket to server, which means we'll probably
	    # miss a push... On the other hands, the connection to the
	    # server should have been opened earlier on, i.e. at
	    # (re)configuration
	    if { $MSIM(ws:sock) eq "" } {
		::ws:open
	    }

	    # Send current value to server via websocket.
	    if { $MSIM(ws:sock) ne "" } {
		::feature:update
		::websocket::send $MSIM(ws:sock) text "\{[::json:status]\}"
	    }
	}
	"h*" {
	    set host [dict get $MSIM(sim:cfg) host]
	    if { $host ne "" } {
		::feature:update
		set state "\{[::json:status]\}"

		set port [dict get $MSIM(sim:cfg) port]
		if { $port eq "" } { set port 80 }
		set path [dict get $MSIM(sim:cfg) path]
		set path [string trimleft $path "/"]
		set rcv "http://${host}:${port}/${path}"
		set cmd [list ::http::geturl $rcv \
			     -method POST \
			     -type "application/json" \
			     -query $state]
		if { $MSIM(timeout) > 0 } {
		    lappend cmd -timeout $MSIM(timeout)
		}

		if { [catch {eval $cmd} token] } {
		    $MSIM(log)::error "Error while posting result at $rcv:\
                                       $token"
		}
	    }
	}
    }

    set next [expr {int([dict get $MSIM(sim:cfg) interval]*1000)}]
    set MSIM(pusher) [after $next ::net:push]
}


proc ::net:init {} {
    global MSIM

    package require tls
    ::http::register https 443 [list ::tls::socket]

    set MSIM(srv) [::minihttpd::new "" $MSIM(port)]
    ::minihttpd::handler $MSIM(srv) "/" ::rest:status "application/json"
    ::minihttpd::handler $MSIM(srv) "/node" ::rest:node "application/json"
    ::minihttpd::handler $MSIM(srv) "/cfg" ::rest:cfg "application/json"
    foreach f $MSIM(features) {
	::minihttpd::handler $MSIM(srv) "/$f" ::rest:feature "application/json"
    }
}

set MSIM(sim:node) [dict create \
			"node-type" "Mote Simulator" \
			"version" "1.0"]
set MSIM(sim:cfg) [dict create \
		       "host" "" \
		       "port" "" \
		       "path" "" \
		       "proto" "" \
		       "interval" [expr $MSIM(sampling)/1000]]
set MSIM(ws:sock) ""
		       

# Create device listeners
::net:init

vwait forever