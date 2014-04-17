# Implements a device poller, i.e. an application that will poll a
# (remote) device at regular intervals and post its data to the
# context manager.  The application supposes the device offers a JSON
# interface and was designed to support the motes from flexibility.

# We must have at least 8.6 for IPv6 support!
package require Tcl

set options {
    { logfile.arg "%APPDATA%/me3gas/%progname%.log" "Where to save log for every run" }
    { context.arg "https://localhost:8800/" "Root URL to context manager" }
    { report.arg "http://\[aaaa::250:c2a8:c55c:9cc5\]/data fc24cc87-dbfb-5243-5e9c-06ffd532a473 {temp temperature hum humidity pres pressure}" "List of serial/type matches to UUID/fieldName" }
    { sampling.integer "30000" "Sampling rate, keep it low to save batteries!" }
    { timeout.integer "40000" "HTTP timeout, in msecs" }
}

array set DVP {
    logfd       ""
    debug       0
    udp         ""
}


source [file join [file dirname $argv0] lib init.tcl]
if { [string is true $DVP(debug)] } {
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
    global DVP
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
    global DVP

    set logline "\[$dt\] \[$srv\] \[$lvl\] $str"

    # Return if no logfile specified
    if { $DVP(logfile) eq "" } {
	return 1
    }

    if { $DVP(logfd) eq "" } {
	set fname [file normalize [::diskutil::fname_resolv $DVP(logfile)]]
	set d [file dirname $fname]
	if { ! [file isdirectory $d] } {
	    file mkdir $d
	}
	if { ! [catch {open $fname w+} fd] } {
	    set DVP(logfd) $fd
	}
    }
    if { $DVP(logfd) ne "" } {
	puts $DVP(logfd) $logline
    }
    return 1
}


# Initialise everything in one go...
::init::init \
    -store DVP \
    -options $options \
    -booleans [list] \
    -depends [list progver event rest http] \
    -packages [list rest uri base64 http] \
    -parsed ::init:fix \
    -outlog ::log:out

if {![package vsatisfies [package provide Tcl] 8.6]} {
    $DVP(log)::warn "You need Tcl 8.6 and the specifically modified http\
                     package to get IPv6 access to work!"
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
	} elseif {![catch {dict size}]} {
	    lappend Result "\"$key\":\"[::json:from_dict $value]\""
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


proc ::report { dev } {
    global DVP

    upvar \#0 $dev DEV

    # Parse through the known reporting directives if we find one that
    # matches the sensor serial and type that we should report for.
    foreach {url uuid dst} $DVP(report) {
	if { $DEV(url) eq $url } {
	    $DVP(log)::debug "Reporting sensor $DEV(url) to $uuid: $DEV(value)"

	    # Construct a URL, based on the UUID of the receiving
	    # object and on the field specification that is contained
	    # in the report specification.
	    set url [string trimright $DVP(context) "/"]/context/$uuid/set?
	    set qry {}
	    foreach {sensor field} $dst {
		switch -glob -- $sensor {
		    timestamp {
			lappend qry $field
			lappend qry [::to_rfc3339 $DEV(timestamp) us]
		    }
		    sampling {
			if { $DEV(sampling) ne "" } {
			    lappend qry $field
			    lappend qry $DEV(sampling)
			}
		    }
		    * {
			if { [dict exists $DEV(value) $sensor] } {
			    lappend qry $field
			    lappend qry [dict get $DEV(value) $sensor]
			}
		    }
		}
	    }

	    # Send the data to the context manager using the data and
	    # the query that we have just constructed.
	    append url [eval [linsert $qry 0 ::http::formatQuery]]
	    set cmd [list ::http::geturl $url]
	    if { $DVP(timeout) > 0 } {
		lappend cmd -timeout $DVP(timeout)
	    }
	    if { [catch {eval $cmd} token] } {
		$DVP(log)::error "Error while getting URL at $url: $token"
	    } else {
		$DVP(log)::debug "Posted to context manager at $url"
		if { [::http::ncode $token] == 200 } {
		    set data [::http::data $token]
		}
		::http::cleanup $token
	    }
	}
    }
}


proc ::dev:__reschedule { d } {
    global DVP
    upvar \#0 $d DEV
    
    set DEV(state) IDLE
    if { $DVP(sampling) >= 0 } {
	set DEV(poll) [after $DEV(-sampling) ::dev:__get $d]
    } else {
	set devs [::uobj::allof [namespace current] device]
	set remaining [llength $devs]
	foreach dv $devs {
	    upvar \#0 $dv D
	    if { $D(state) eq "IDLE" } {
		incr remaining -1
	    }
	}
	if { $remaining <= 0 } {
	    $DVP(log)::notice "Done polling once all remote devices, exiting"
	    exit
	}
    }
}

proc ::dev:__push { d token } {
    global DVP

    upvar \#0 $token htstate
    upvar \#0 $d DEV

    set DEV(state) GOT
    set ncode [::http::ncode $token]
    if { $ncode == 200 || $ncode == 201 } {
	# Get data from sensor in JSON format.
	set data [::http::data $token]
	set DEV(value) [::json:to_dict $data]

	# Compute actual sampling rate and latest data
	set now [clock microseconds]
	if { $DEV(timestamp) ne "" } {
	    set DEV(sampling) [expr {($now - $DEV(timestamp))/1000}]
	} else {
	    if { $DEV(-sampling) > 0 } {
		set DEV(sampling) $DEV(-sampling)
	    } else {
		set DEV(sampling) ""
	    }
	}
	set DEV(timestamp) $now
	
	::report $d
    } else {
	$DVP(log)::warn "Could not get $htstate(url): error is\
                              '[::http::error $token]' status is\
                              '[::http::status $token]', HTTP code was:\
                              [::http::ncode $token]"
    }

    # Cleanup and re-schedule
    ::http::cleanup $token
    ::dev:__reschedule $d
}


proc ::dev:__progress { token total current } {
    global DVP

    upvar \#0 $token state
    $DVP(log)::debug "Read ${current}/${total} bytes for $state(url)"
}


proc ::dev:__get { dev } {
    global DVP

    upvar \#0 $dev DEV

    $DVP(log)::debug "Getting data for device at $DEV(url)"
    set DEV(state) GETTING
    set gcmd [list ::http::geturl $DEV(url) \
		  -progress ::dev:__progress \
		  -blocksize 127 \
		  -command [list ::dev:__push $dev]]
    if { $DVP(timeout) > 0 } {
	lappend gcmd -timeout $DVP(timeout)
    }
    
    if { [catch {eval $gcmd} token] } {
	$DVP(log)::error "Error while getting URL at $DEV(url): $token"
	::dev:__reschedule $dev
	set token ""
    }

    return $token
}

proc ::dev:init { url } {
    global DVP

    # Create device from URL if it does not exist.  This construct
    # will ensure that we can write several times the same device to
    # map to several objects in the context manager if necessary.
    set d [::uobj::find [namespace current] device \
	       [list url == $url]]
    if { $d eq "" } {
	set d [::uobj::new [namespace current] device]
	upvar \#0 $d DEV
	set DEV(self) $d
	set DEV(url) $url
	set DEV(poll) ""
	set DEV(timestamp) ""
	set DEV(state) IDLE
	set DEV(-sampling) $DVP(sampling)

	$DVP(log)::notice "Initialising connection to mote at $url..."
	set DEV(poll) [after idle ::dev:__get $d]
    }

    return $d
}


proc ::net:init {} {
    global DVP

    package require tls
    ::http::register https 443 [list ::tls::socket]
}

::net:init

# Create device listeners
foreach {url uuid matching} $DVP(report) {
    ::dev:init $url
}

vwait forever
