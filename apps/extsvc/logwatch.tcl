package require Tcl

set options {
    { logfile.arg "%APPDATA%/me3gas/%progname%.log" "Where to save log for every run" }
    { logs.arg "" "List of log files to watch, either as list or filename (start with @)" }
    { period.integer "500" "Poll period for peeking into log files, in ms." }
}

array set LOGWATCH {
    logfd   ""
    debug   0
}

source [file join [file dirname $argv0] lib init.tcl]
if { [string is true $LOGWATCH(debug)] } {
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
    global LOGWATCH
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
    global LOGWATCH

    set logline "\[$dt\] \[$srv\] \[$lvl\] $str"

    # Return if no logfile specified
    if { $LOGWATCH(logfile) eq "" } {
	return 1
    }

    if { $LOGWATCH(logfd) eq "" } {
	set fname [file normalize [::diskutil::fname_resolv $LOGWATCH(logfile)]]
	set d [file dirname $fname]
	if { ! [file isdirectory $d] } {
	    file mkdir $d
	}
	if { ! [catch {open $fname w+} fd] } {
	    set LOGWATCH(logfd) $fd
	}
    }
    if { $LOGWATCH(logfd) ne "" } {
	puts $LOGWATCH(logfd) $logline
    }
    return 1
}


# Initialise everything in one go...
::init::init \
    -store LOGWATCH \
    -options $options \
    -booleans [list] \
    -depends [list progver event] \
    -packages [list] \
    -load [list] \
    -parsed ::init:fix \
    -outlog ::log:out


proc ::log:check { lw } {
    global LOGWATCH

    if { ![::uobj::isa $lw watch] } {
	return -code error "$lw unknown"
    }

    upvar \#0 $lw W
    set fname [::diskutil::fname_resolv $W(-filename)]
    if { [file exists $fname] } {
	set mtime [file mtime $fname]
	set now [clock clicks -milliseconds]
	if { $W(mtime) eq "" } {
	    set W(mtime) $mtime
	    set W(checked) $now
	    $LOGWATCH(log)::debug "Last modification of $fname was\
                                   [clock format $W(mtime)]"
	} elseif { $W(mtime) == $mtime } {
	    set elapsed [expr {$now - $W(checked)}]
	    if { $elapsed > $W(-inactivity) } {
		$LOGWATCH(log)::notice "File $fname has not changed for more\
                                        than $W(-inactivity) ms!"
		if { [catch {exec $W(-action) &} res] == 0 } {
		    $LOGWATCH(log)::debug "Triggered $W(-action) successfully"
		} else {
		    $LOGWATCH(log)::warn "Triggered $W(-action) unsuccessfully:\
                                          $res"
		}
		# Reset even if we had reached inactivity period, so
		# we get some time restarting the action and making
		# sure the file gets pumped to again.
		set W(checked) $now
	    }
	} else {
	    set W(mtime) $mtime
	    set W(checked) $now
	}
    } else {
	set W(mtime) ""
	set W(checked) 0
    }
}


proc ::log:pulse { } {
    global LOGWATCH

    foreach lw [::uobj::allof [namespace current] watch] {
	::log:check $lw
    }

    after $LOGWATCH(period) ::log:pulse
}


proc ::log:init { fname inactivity action } {
    global LOGWATCH 
    
    set lw [::uobj::new [namespace current] watch]
    upvar \#0 $lw W

    # Understand floats as seconds, integers as milliseconds, convert
    # everything to milliseconds.
    if { [string is double $inactivity] && ![string is integer $inactivity] } {
	set inactivity [expr {int(1000*$inactivity)}]
    }

    set W(-inactivity) $inactivity
    set W(-filename) $fname
    set W(-action) $action
    set W(mtime) ""
}


# Read set of links from file (or get from the command line directly)
set LOGWATCH(logs) [string trim $LOGWATCH(logs)]
if { [string index $LOGWATCH(logs) 0] eq "@" } {
    set fname [::diskutil::fname_resolv [string range $LOGWATCH(logs) 1 end]]
    $LOGWATCH(log)::info "Reading log actions from $fname..."
    set LOGWATCH(logs) [::diskutil::lread $fname 3 "Log action file"]
}

# Create the log listeners.
foreach { fname inactivity action } $LOGWATCH(logs) {
    ::log:init $fname $inactivity $action
}

# Start checking
after $LOGWATCH(period) ::log:pulse

vwait forever
