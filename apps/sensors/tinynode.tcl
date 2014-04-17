set options {
    { logfile.arg "%APPDATA%/me3gas/%progname%.log" "Where to save log for every run" }
    { port.integer "60000" "UDP port to listen on for incoming data" }
    { context.arg "https://localhost:8800/" "Root URL to context manager" }
    { report.arg "2 2 accdf0f3-108e-56ac-446e-9b0398eeeb7a {value value date timestamp sampling sampling} 1 1 20ecdbe4-8459-5636-6146-71c618badc71 {value value date timestamp sampling sampling}" "List of serial/type matches to UUID/fieldName" }
    { sampling.integer "60000" "Initial sampling rate, will be computed later" }
}

array set TNR {
    logfd       ""
    debug       0
    timeout     10000
}


source [file join [file dirname $argv0] lib init.tcl]
if { [string is true $TNR(debug)] } {
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
    global TNR
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
    global TNR

    set logline "\[$dt\] \[$srv\] \[$lvl\] $str"

    # Return if no logfile specified
    if { $TNR(logfile) eq "" } {
	return 1
    }

    if { $TNR(logfd) eq "" } {
	set fname [file normalize [::diskutil::fname_resolv $TNR(logfile)]]
	set d [file dirname $fname]
	if { ! [file isdirectory $d] } {
	    file mkdir $d
	}
	if { ! [catch {open $fname w+} fd] } {
	    set TNR(logfd) $fd
	}
    }
    if { $TNR(logfd) ne "" } {
	puts $TNR(logfd) $logline
    }
    return 1
}


# Initialise everything in one go...
::init::init \
    -store TNR \
    -options $options \
    -booleans [list] \
    -depends [list progver event rest udp cxapi] \
    -load [list cxapi] \
    -packages [list rest uri base64 udp] \
    -parsed ::init:fix \
    -outlog ::log:out


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


proc ::report { dev } {
    global TNR

    upvar \#0 $dev DEV

    # Parse through the known reporting directives if we find one that
    # matches the sensor serial and type that we should report for.
    foreach {serial type uuid dst} $TNR(report) {
	if { $serial == $DEV(serial) && $type == $DEV(type) } {
	    $TNR(log)::debug "Reporting sensor $DEV(serial)/$DEV(type) to\
                              $uuid: $DEV(value)"
	    # Construct a URL, based on the UUID of the receiving
	    # object and on the field specification that is contained
	    # in the report specification.
	    set qry {}
	    foreach {field sensor} $dst {
		switch -glob -- [string tolower $sensor] {
		    v* {
			lappend qry $field
			lappend qry $DEV(value)
		    }
		    t* {
			lappend qry $field
			lappend qry [::to_rfc3339 $DEV(timestamp) us]
		    }
		    s* {
			if { $DEV(sampling) ne "" } {
			    lappend qry $field
			    lappend qry $DEV(sampling)
			}
		    }
		}
	    }

	    # Send the data to the context manager using the data and
	    # the query that we have just constructed.
	    $TNR(cx) context $uuid set $qry
	}
    }
}


proc ::net:receiver { sock } {
    global TNR

    set pkt [read $sock]
    set peer [fconfigure $sock -peer]
    # Parse header
    if { [binary scan $pkt IISS serial type cmd len] < 4 } {
	$TNR(log)::warn "Could not parse header of incoming packet"
    } else {
	# Now parse data contained in packet.
	set msg [string range $pkt 12 end]; # 12 is header length
	if { [string length $msg] != $len } {
	    $TNR(log)::warn "Length mismatch: announced as $len,\
                             effective as [string length $msg]"
	    return
	}
	$TNR(log)::debug "Remote peer at [join $peer :] sent a packet;\
                          Serial:$serial Type:$type Command:$cmd Length:$len"
	if { $len == 4 } {
	    set value ""
	    switch $type {
		2 {
		    binary scan $msg I value
		}
		1 {
		    binary scan $msg I value
		    set value [expr {0.1*$value}]
		}
	    }
	    if { $value ne "" } {
		set dev [::uobj::find [namespace current] sensor \
			     [list serial == $serial type == $type]]
		if { $dev eq "" } {
		    set dev [::uobj::new [namespace current] sensor]
		    upvar \#0 $dev DEV
		    set DEV(timestamp) ""
		}
		upvar \#0 $dev DEV
		set DEV(serial) $serial
		set DEV(type) $type
		set now [clock microseconds]
		if { $DEV(timestamp) ne "" } {
		    set DEV(sampling) [expr {($now - $DEV(timestamp))/1000}]
		} else {
		    if { $TNR(sampling) > 0 } {
			set DEV(sampling) $TNR(sampling)
		    } else {
			set DEV(sampling) ""
		    }
		}
		set DEV(timestamp) $now
		set DEV(value) $value
		::report $dev
	    }
	} else {
	    $TNR(log)::warn "Supports single values at this time"
	}
    }
}

proc ::api:init {} {
    global TNR

    set TNR(cx) [::cxapi::new -url $TNR(context) -timeout $TNR(timeout)]
}

proc ::net:init {} {
    global TNR

    package require tls
    ::http::register https 443 [list ::tls::socket]

    # Register server
    set TNR(srv) [udp_open $TNR(port)]
    fconfigure $TNR(srv) -buffering none -translation binary
    fileevent $TNR(srv) readable [list ::net:receiver $TNR(srv)]
    $TNR(log)::notice "Now listening on UDP port $TNR(port) for incoming data"
}

::net:init
::api:init

vwait forever