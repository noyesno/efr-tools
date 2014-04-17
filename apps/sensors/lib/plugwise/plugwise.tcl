##################
## Module Name     --  plugwise.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    This library provides a high-level access to a (set of) plugwise
##    plugs.  Plugwise are ZigBee based relays that can be remotely
##    switch on and off, but also measure energy consumption.  The
##    library creates objects representing each plug that you want to
##    watch and will automatically provide a number of services:
##    # Switching on and off the plug
##    # Regular polling of current power and reporting on change.
##    # Reporting when "consequent" power change occur, i.e. new device
##      being plugged in or out of the plug.
##    # Reporting of energy log over time and reporting.
##
##    The implementation uses a library that itself is a wrapper
##    around the command line interface of the python library
##    available at https://bitbucket.org/hadara/python-plugwise/.
##    This require the command line program and library to be properly
##    installed.  Unless you change permissions yourself, you will
##    have to elevate your priviledges (sudo!) to run the Tcl script,
##    since it forks plugwise_util, which itself requires access to
##    the serial port (over USB).
##
##    The API is such as you create an object from a Plugwise MAC
##    address, an object that you will use for all further operations,
##    using a Tk-style programming interface.  This library uses the
##    services of the event library to trigger a number of events on
##    its objects.  These events are as follows:
##    # Inited is triggered when the plug is ready for use by the lib.
##    # Configure is triggered whenever the plug object is changed
##    # Switch is triggered whenever the relay changes state, incl.
##      from an external trigger.
##    # Error is triggered when too many unrecoverable errors have
##      been discovered for the plug.
##    # Demand is triggered whenever the absolute demand on the plug
##      (power difference) is greater than -react.  The event also
##      carries the current power %p, the difference %d and the time
##      %t.
##    # Change is triggered whenever the power changes. The event also
##      carries the current power %p, the previous value %d (old) and
##      the time %t.
##    # Collect is triggered whenever power data has been collected,
##      arguments are same as Change event.
##    # Energy is triggered for each (new) energy consumption log, i.e
##      every hour. The event also carries the energy in Wh %y and the
##      time %t.
##    # Delete is triggered on deletion of the plug object.
##
##################


package require uobj
package require event

namespace eval ::plugwise {
    variable PWISE
    if {![info exists PWISE] } {
	array set PWISE {
	    globals      "-controller"
	    -controller  "plugwise_util"
	    -frequency   15
	    -state       off
	    -dev         /dev/ttyUSB0
	    -react       5
	    -errors      4
	    -log         60
	    dft_mac_pfx  000D6F0000
	    debug        off
	    slots        4
	}
	variable version 0.1
	variable libdir [file dirname [file normalize [info script]]]
	::uobj::install_log plugwise PWISE
	::uobj::install_defaults plugwise PWISE
    }
}

# IMPLEMENTATION NOTES
#
# We rely on the (python-based) external binary that is able to
# control plugwise plugs via their serial interface.  Whenever a plug
# is created, an object is created and we initialise the plug from its
# current state (mainly to get the state of the relay).  Once done, we
# poll the state of the plug from time to time, both to gets its
# current power consumption and to get its state.
#
# Note that plugwise_util takes its arguments in sequence and executes
# them in sequence, which we use a number of times to minimise the
# number of forks.  For example, at initialisation time, we both
# synchronise the time of the plug to the computer time *and* get
# information from it in a single call to plugwise_util.
# 
# Ideally, we wanted to use the -c option of the plugwise_util, but
# since it does not flush, the pipe does not contain anything until
# the process has ended, so we cannot get continuous data.  Instead we
# have to poll, starting a new process every xx seconds, and this for
# each plug.  To spread the load, the initial check for status is
# delayed using a random number so as to minimise the load on the
# resources.  To ensure that the serial device is only used by one of
# our processes at a time, this library implements a (prioritised)
# queue for commands to be issued against the plugwise USB (and its
# serial interface).


# TODO
#
# Handle plugs that are of wrong fw_version, since we know that
# plugwise_util is able get information from them but perhaps not able
# to control them?


# ::plugwise::mac -- Return a plugwise MAC address
#
#       Sanitise a MAC address to return a unified MAC address of 8
#       bytes, as used within the plugwise network.  The procedure
#       accepts short 3 bytes addresses and automatically adds the
#       prefix that seems to be prevalent to all installations.
#
# Arguments:
#	mac	MAC address, separated or not by : or - (3 or 8 bytes hex)
#
# Results:
#       Return a sanitised, uppercase, no separators MAC address coded
#       on exactly 8 bytes, or an empty string when sanitisation
#       failed.
#
# Side Effects:
#       None.
proc ::plugwise::mac { mac } {
    variable PWISE
    variable log

    set hex "\[A-Fa-f0-9\]\[A-Fa-f0-9\]"
    if { [string match \
	      ${hex}:${hex}:${hex}:${hex}:${hex}:${hex}:${hex}:${hex} $mac] } {
	foreach { a b c d e f g h } [split $mac ":"] break
	return [string toupper $a][string toupper $b][string toupper $c][string toupper $d][string toupper $e][string toupper $f][string toupper $g][string toupper $h]
    } elseif { [string match \
		    ${hex}-${hex}-${hex}-${hex}-${hex}-${hex}-${hex}-${hex} $mac] } {
	foreach { a b c d e f g h } [split $mac "-"] break
	return [string toupper $a][string toupper $b][string toupper $c][string toupper $d][string toupper $e][string toupper $f][string toupper $g][string toupper $h]
    } elseif { [string match \
		    ${hex}${hex}${hex}${hex}${hex}${hex}${hex}${hex} $mac] } {
	return [string toupper $mac]
    } elseif { [string match ${hex}:${hex}:${hex} $mac] } {
	foreach {a b c} [split $mac ":"] break
	return $PWISE(dft_mac_pfx)[string toupper $a][string toupper $b][string toupper $c]
    } elseif { [string match ${hex}-${hex}-${hex} $mac] } {
	foreach {a b c} [split $mac "-"] break
	return $PWISE(dft_mac_pfx)[string toupper $a][string toupper $b][string toupper $c]
    } elseif { [string match ${hex}${hex}${hex} $mac] } {
	return $PWISE(dft_mac_pfx)[string toupper $mac]
    }

    return "";  # Parsing error
}


proc ::plugwise::__state { p state } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    ${log}::notice "Changing state of $PLUG(mac) to $state"
    # Make a command that starts by changing the state and then
    # reads the current state to check that we were successful.
    set cmd "|\"$PWISE(-controller)\" -m $PLUG(mac) -d $PLUG(-dev)\
                  -s [string is true $state] -q relay_state"
    set fd [open $cmd]
    fconfigure $fd -buffering line -blocking 1 -translation lf
    set state [string trim [read $fd]]; # Read state of plug!
    close $fd
    set PLUG(-state) $state
    ${log}::info "Plug $PLUG(mac) now in state: $state"
    ::event::generate $p Switch
}


# ::plugwise::switch -- Switch a plugwise on and off
#
#       This procedure can be used to turn on or off the relay that is
#       embedded in the plugwise, thus being able to change the state
#       of the device(s) that are connected to that switch.
#
# Arguments:
#	p	Identifier of the plugwise object
#	state	New state (should be a boolean). Empty for query only.
#       sync    Should we wait for state to be physically set at plug?
#
# Results:
#       Return the state of the plugwise.  If the state was changed,
#       the state that is returned is the state as returned by the
#       plug, i.e. the real physical state as reported by the
#       plugwise.
#
# Side Effects:
#       None.
proc ::plugwise::switch { p { state "" } { sync on } } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    if { $state ne "" } {
	${log}::debug "Requesting for plug $PLUG(mac) to be at state $state urgently"
	set id [__enqueue $PLUG(queue) -1 $p \
		    "[namespace current]::__state %plug% $state"]
	if { [string is true $sync] } {
	    __wait $PLUG(queue) $id
	}
    }

    return $PLUG(-state)
}


# ::plugwise::get -- Get (semi-)internal data from object
#
#       This procedure is able to return some extra information about
#       the plugwise.  These are the following:
#       mac     MAC address of the plug, complete
#       state   State of the state machine, ERROR can be or interest
#       hz      Number of Hertz where the plug is connected
#       hw_ver  Hardware version of the plug
#       fw_ver  Version of the firmware on the plug
#       -       Anything starting with a dash is understood as an object option
#
# Arguments:
#	p	Identifier of a plugwise object
#	what	Information to retrieve (see above).
#
# Results:
#       Returns the requested value or an error.
#
# Side Effects:
#       None.
proc ::plugwise::get { p what } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    ::switch -glob -- $what {
	mac -
	state {
	    return $PLUG($what)
	}
	usage {
	    return $PLUG(usage)
	}
	power {
	    return [lindex $PLUG(usage) end]
	}
	last_logaddr -
	slot {
	    return $PLUG(slot)
	}
	hz -
	hw_ver -
	fw_ver {
	    return $PLUG(nfo:$what)
	}
	-* {
	    return [config $p $what]
	}
    }

    return ""
}


# ::plugwise::destroy -- Destroy connection to a plug
#
#       This procedure destroys the connection to a plug and removes
#       the object from memory.
#
# Arguments:
#	p	Identifier of the plug
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::plugwise::destroy { p } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    # Last minute event trigger, just in case...
    ::event::generate $p Delete

    # Get rid of all bindings, we are dying.
    ::event::clean $p

    __pulse $p power delete
    __pulse $p energy delete
    ::uobj::delete $p
}


# ::plugwise::__pulse -- Change polling state
#
#       Each object is associated to two pulses (a frequent one for
#       the state of the relay, a less frequent one for the energy log)
#       that will periodically poll for the state of the plug.  This
#       procedure is used to control this pulse.  At initialisation,
#       the pulse will randomly wait before starting to avoid having
#       many instances of the plugwise_util program running at the
#       same time.
#
# Arguments:
#	p	Identifier of the plug
#	poller	Which poller to change state for
#	op	Operation on the pulse: delete, next or init
#
# Results:
#       Return the identifier of the after scheduler that was
#       established for the next poll, incl. an empty string if it was
#       removed.
#
# Side Effects:
#       None.
proc ::plugwise::__pulse { p { poller "power" } { op "next" } } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    ::switch $poller {
	"power" {
	    set freq $PLUG(-frequency)
	    set cmd "[namespace current]::__check:power %plug%"
	}
	"energy" {
	    set freq $PLUG(-log)
	    set cmd "[namespace current]::__check:energy %plug%"
	}
	default {
	    return -code error "$poller is not a recognised poller!"
	}
    }

    ::switch $op {
	delete -
	destroy -
	clean {
	    __dequeue $PLUG(queue) $p $cmd
	}
	next {
	    __dequeue $PLUG(queue) $p $cmd
	    set when [expr {int($freq*1000)}]
	    __enqueue $PLUG(queue) $when $p $cmd
	}
	first -
	init {
	    __dequeue $PLUG(queue) $p $cmd
	    set when [expr {int(rand()*$freq*1000)}]
	    __enqueue $PLUG(queue) $when $p $cmd
	}
    }
}


# ::plugwise::__errors -- (Ac)count for errors
#
#       Count the total number of errors in a row for a plug and put
#       the whole plug in ERROR state (as in state machine) if there
#       are too many errors.  When entering the ERROR state, an event
#       is generated.
#
# Arguments:
#	p	Identifier of the plugwise object.
#	errs	Number of errors that were detected when talking to plug
#
# Results:
#       -1 if the plug has entered the (final) ERROR state, the
#       -current number of errors count for plug so far otherwise.
#
# Side Effects:
#       None.
proc ::plugwise::__error { p errs } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    # Reset error count if communication with plug was error-free,
    # count errors otherwise.
    if { $errs == 0 } {
	if { $PLUG(errors) > 0 } {
	    ${log}::debug "Plug $PLUG(mac) is fine again"
	}
	set PLUG(errors) 0
    } else {
	incr PLUG(errors) $errs
    }

    # Put the plug in error state if we've failed getting data from it
    # for too many times.
    if { $PLUG(errors) > $PLUG(-errors) } {
	set PLUG(state) ERROR

	::event::generate $p Error

	__pulse $p power delete
	__pulse $p energy delete
	return -1;
    }

    return $PLUG(errors)
}


# ::plugwise::__check:power -- Poll plug relay state
#
#       Poll for relay the state of the plug and report via events.
#       Sometimes, error occur when accessing the current power state
#       of the plug, this procedure accounts for these errors and put
#       the plug in the ERROR state if too many have occured,
#       generating an event at the same time. Most of the time, things
#       will work and this procedure generates event every time the
#       power usage has changed, but also whenever it has
#       "drastically" changed, which is controlled by the -react
#       option.
#
# Arguments:
#	p	Identifier of the plug
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::plugwise::__check:power { p } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    set errs 0
    if { $PLUG(state) eq "INITED" } {
	${log}::debug "Checking state of plug $PLUG(mac)"
	set cmd "|\"$PWISE(-controller)\" -m $PLUG(mac) -d $PLUG(-dev)\
                  -p -q relay_state"
    
	set fd [open $cmd]
	fconfigure $fd -buffering line -blocking 1 -translation lf

	while { ! [eof $fd] } {
	    set l [string trim [string tolower [gets $fd]]]
	    if { $l ne "" } {
		if { [string is true $PWISE(debug)] } {
		    ${log}::debug "Status: $l"
		}
		if { [string match "power usage:*" $l] } {
		    foreach {- use} [split $l ":"] break
		    set use [string trim [string trim $use "w"]]
		    set old [lindex $PLUG(usage) end]
		    set now [clock seconds]
		    lappend PLUG(usage) $now $use; # XXX: Contain the length?
		    if { $old eq "" } {
			# Generate a Demand event the first time to
			# allow proper initialisation at callers, tell
			# it is the first by using a difference that
			# is zero.
			::event::generate $p Demand \
			    [list %p $use %d 0 %t $now]
		    } else {
			set diff [expr {$use-$old}]
			if { [expr {abs($diff)}] >= $PLUG(-react) } {
			    ::event::generate $p Demand \
				[list %p $use %d $diff %t $now]
			}
		    }
		    if { $old eq "" || $old != $use } {
			::event::generate $p Change \
			    [list %p $use %d "$old" %t $now]
		    }
		    ::event::generate $p Collect \
			[list %p $use %d "$old" %t $now]
		    ${log}::debug "Plug $PLUG(mac) using ${use}W"
		} elseif { [string match "error:*" $l] } {
		    ${log}::error "Error when communicating with plugwise: $l"
		    incr errs
		} else {
		    set old [string is true $PLUG(-state)]
		    set PLUG(-state) [string is true $l]
		    if { $PLUG(-state) != $old } {
			::event::generate $p Switch
		    }
		}
	    }
	}

	# Cautious close, this is where we catch raise conditions
	# where two (we and another one) processes try to access the
	# plugwise at the same time.
	if { [catch {close $fd} err] } {
	    ${log}::error "Error when communicating with plugwise: $err"
	    incr errs
	}
    }

    if { [__error $p $errs] >= 0 } {
	__pulse $p power next
    }
}


# ::plugwise::__check:energy -- Poll plug energy consumption
#
#       Poll for energy consumption of the plug and report via events.
#       Sometimes, error occur when accessing the current power state
#       of the plug, this procedure accounts for these errors and put
#       the plug in the ERROR state if too many have occured,
#       generating an event at the same time. Most of the time, things
#       will work and this procedure generates event every time the
#       power usage has changed, but also whenever it has
#       "drastically" changed, which is controlled by the -react
#       option.
#
# Arguments:
#	p	Identifier of the plug
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::plugwise::__check:energy { p } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    set errs 0
    if { $PLUG(state) eq "INITED" } {
	${log}::debug "Checking energy log of plug $PLUG(mac) at slot #$PLUG(slot)"
	set cmd "|\"$PWISE(-controller)\" -m $PLUG(mac) -d $PLUG(-dev)\
                  -l $PLUG(slot)"
    
	set fd [open $cmd]
	fconfigure $fd -buffering line -blocking 1 -translation lf

	set empty 0
	set reported 0
	set slots 0
	while { ! [eof $fd] } {
	    set l [string trim [string tolower [gets $fd]]]
	    if { $l ne "" } {
		if { [string is true $PWISE(debug)] } {
		    ${log}::debug "Status: $l"
		}
		if { [string match "power usage log:*" $l] } {
		} elseif { [string match "error:*" $l] } {
		    ${log}::error "Error when communicating with plugwise: $l"
		    incr errs
		} elseif { [string first "n/a" $l] >= 0 } {
		    incr empty
		} else {
		    incr slots
		    foreach {day hour energy unit} $l break
		    set date [clock scan "$day $hour" -format "%Y-%m-%d %H"]
		    if { [lsearch $PLUG(energy) $date] < 0 } {
			lappend PLUG(energy) $date $energy
			set strdate [clock format $date -format "%Y%m%d-%H:%M:%S"]
			${log}::debug "Found new energy log for $PLUG(mac) for\
                                       latest period: at $strdate, $energy $unit\
                                       had been used"
			::event::generate $p Energy [list %t $date %y $energy]
			incr reported
		    }
		}
	    }
	}

	# Cautious close, this is where we catch raise conditions
	# where two (we and another one) processes try to access the
	# plugwise at the same time.
	if { [catch {close $fd} err] } {
	    ${log}::error "Error when communicating with plugwise: $err"
	    incr errs
	}
    }

    # Try again if we've reached the maximum number of slots. Advance
    # to next memory slot on plug.  Give a try at getting energy data
    # from there as well to cope with startup conditions.
    if { $slots >= $PWISE(slots) } {
	incr PLUG(slot)
	__pulse $p energy next
	${log}::info "Advancing to memory slot #$PLUG(slot) for plug $PLUG(mac)"
    }

    # Try again later if we had errors or if there wasn't any value to
    # report from the plug yet.
    if { [__error $p $errs] >= 0 } {
	__pulse $p energy next
    }
}


# ::plugwise::jsdate -- Parse JavaScript date
#
#       The plugwise util is meant to be used from a JS wrapper, so
#       the dates that its writes down use the datetime object from
#       javascript.  This procedure does some simplistic parsing of
#       such JS calls and returns the timestamp that was specified.
#
# Arguments:
#	str	timestamp specification in JS
#
# Results:
#       Timestamp in seconds since the period, or an empty string on
#       errors.
#
# Side Effects:
#       None.
proc ::plugwise::jsdate { str } {
    variable PWISE
    variable log

    set str [string trim [string tolower $str]]
    if { [regexp {datetime.datetime\((.*)\)} $str - dt] } {
	foreach {Y m d H M S} [split $dt ","] break
	if { $S eq "" } { set S 00 }
	set S [string range 0[string trim $S] end-1 end]
	set M [string range 0[string trim $M] end-1 end]
	set H [string range 0[string trim $H] end-1 end]
	set d [string range 0[string trim $d] end-1 end]
	set m [string range 0[string trim $m] end-1 end]
	return [clock scan "$Y-$m-$d $H:$M:$S" -format "%Y-%m-%d %H:%M:%S"]
    }
    return [clock seconds]
}


proc ::plugwise::__init { p { force 0 } } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    if { [lsearch [list "ERROR" "NONE"] $PLUG(state)] >= 0 || [string is true $force] } {
	# Synchronise the clock with computer clock on initialisation and
	# get information about device
	set cmd "|\"$PWISE(-controller)\" -m $PLUG(mac) -d $PLUG(-dev) -t sync -i"
	
	${log}::debug "Initialising plug $PLUG(mac)"
	set fd [open $cmd]
	fconfigure $fd -buffering line -blocking 1 -translation lf
	while { ! [eof $fd] } {
	    set line [string trim [gets $fd]]
	    if { [string is true $PWISE(debug)] } {
		${log}::debug "Initialisation: $line"
	    }
	    if { [string match -nocase info* $line] } {
	    } elseif { $line eq "" } {
	    } elseif { [string match -nocase "Error:*" $line] } {
		${log}::error "Error when communicating with plugwise: $line"
		set PLUG(state) ERROR
		break
	    } elseif { [string match -nocase "*:*" $line] } {
		foreach {k v} [split $line ":"] break
		set k [string trim $k "{' \t\r\n"]
		set v [string trim $v " \t\r\n',}"]
		::switch -glob -- $k {
		    hw_ver -
		    hz {
			set PLUG(nfo:$k) $v
		    }
		    fw_ver -
		    datetime {
			set PLUG(nfo:$k) [jsdate $v]
		    }
		    last_logaddr {
			# Remember which memory slot we are at, arrange to
			# be just one before in order to better arrange
			# for resynchronisation at startup.
			set PLUG(slot) $v
			if { $v > 0 } {
			    incr PLUG(slot) -1
			}
			${log}::info "Starting to get energy history from plug $PLUG(mac)\
                                  at memory slot #$PLUG(slot)."
		    }
		    *state {
			set PLUG(-state) [string is true $v]
			${log}::info "Plug $PLUG(mac) in state: $PLUG(-state)"
		    }
		}
	    }
	}
	close $fd

	if { $PLUG(state) eq "ERROR" } {
	    ::event::generate $p Error
	} else {
	    set PLUG(state) INITED
	    set PLUG(errors) 0
	    ::event::generate $p Inited

	    __pulse $p power init
	    __pulse $p energy init
	}
    }
}


# ::plugwise::init -- Initialisation of a plug
#
#       This procedure is typically (automatically) called one a
#       plugwise object has been instantiated.  It will synchronise
#       the date and time of the computer to the plugwise and will get
#       some initial information from the plug, incl. the current
#       state of the relay.  The procedure can also be called to try
#       re-initialising connection to a plugwise once too many errors
#       have been detected and the plug has been put in the ERROR
#       state.
#
# Arguments:
#	p       Identifier of the plugwise object.
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::plugwise::init { p { sync off } } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    set id [__enqueue $PLUG(queue) -1 $p "[namespace current]::__init $p]"]
    if { [string is true $sync] } {
	__wait $PLUG(queue) $id
    }
}


proc ::plugwise::__queue { dev } {
    variable PWISE
    variable log

    set q [::uobj::find [namespace current] queue \
	       [list -dev == $dev]]
    if { $q eq "" } {
	set q [::uobj::new [namespace current] queue]
	upvar \#0 $q QUEUE

	set QUEUE(-dev) $dev;   # Serial device the queue is associated to
	set QUEUE(pending) {};  # List of pending commands in queue
	set QUEUE(idgene) 0;    # Command id generator
	set QUEUE(state) IDLE;  # Current state of queue: IDLE or EXEC
	set QUEUE(timer) "";    # After timer id for queue content check
	set QUEUE(latest) -1;   # Identifier of latest executed command
    }
    return $q
}


proc ::plugwise::__wait { q id } {
    variable PWISE
    variable log

    if { ![::uobj::isa $q queue] } {
	return -code error "$q unknown or wrong type!"
    }
    upvar \#0 $q QUEUE

    ${log}::debug "Waiting for command \#$id to end..."

    # Account for the list of command ids that we executed while
    # waiting for the completion of the command that we are waiting
    # for.
    set executed {}

    # If the latest command executed was the one that we are waiting
    # for, we are done already.  Nothing to wait for...
    if { $QUEUE(latest) == $id } {
	${log}::debug "Nothing to wait for, already executed!"
	return $executed
    }

    # Now wait for the end of the execution of commands, and for each,
    # check if this is the one that we are waiting for.
    while { 1 } {
	vwait ${q}(latest);    # Wait for finalised execution of next command
	lappend executed $QUEUE(latest)
	if { $QUEUE(latest) == $id } {
	    ${log}::debug "Executed commands with ids $executed while waiting"
	    return $executed;  # Return all commands executed in the mean time.
	}
    }
    return {};   # Never reached
}


proc ::plugwise::__peek { q } {
    variable PWISE
    variable log

    if { ![::uobj::isa $q queue] } {
	return -code error "$q unknown or wrong type!"
    }
    upvar \#0 $q QUEUE

    # Order the queue according to execution timestamps.
    set time_ordered [lsort -increasing -index 1 $QUEUE(pending)]
	
    # Access next item in queue to be executed, make sure we have
    # one, otherwise we can simply return.
    return [lindex $time_ordered 0]
}


proc ::plugwise::__tstamp { when } {
    set secs [expr int($when/1000)]
    set tstamp [clock format $secs -format "%Y:%m:%d-%H:%M:%S"]
    append tstamp ".[format %.3d [expr {$when-($secs*1000)}]]"

    return $tstamp
}


proc ::plugwise::__pqueue { q } {
    variable PWISE
    variable log

    if { ![::uobj::isa $q queue] } {
	return -code error "$q unknown or wrong type!"
    }
    upvar \#0 $q QUEUE

    # Order the queue according to execution timestamps.
    set time_ordered [lsort -increasing -index 1 $QUEUE(pending)]

    foreach spec $time_ordered {
	foreach {id when p cmd rdv} $spec break
	upvar \#0 $p PLUG
	puts "CMD\#[format %.5d $id] in plug $PLUG(mac): [__tstamp $when] --> $cmd"
    }
}


proc ::plugwise::__execute { q } {
    variable PWISE
    variable log

    if { ![::uobj::isa $q queue] } {
	return -code error "$q unknown or wrong type!"
    }
    upvar \#0 $q QUEUE

    if { $QUEUE(state) eq "IDLE" } {
	# Remove current timer, if any
	if { $QUEUE(timer) ne "" } {
	    after cancel $QUEUE(timer)
	}
	set QUEUE(timer) ""

	# Access next item in queue to be executed, make sure we have
	# one, otherwise we can simply return.
	set next [__peek $q]
	if { $next eq "" } {
	    ${log}::debug "No commands to execute in queue"
	    return
	}
	foreach {id when p cmd rdv} $next break
	
	# Decide when to execute
	set now [clock clicks -milliseconds]
	if { $now < $when } {
	    # Schedule to execute later
	    set elapsed [expr {$when-$now}]
	    ${log}::debug "$elapsed ms to next command (\#$id) in queue"
	    set QUEUE(timer) [after $elapsed [namespace current]::__execute $q]
	} else {
	    ${log}::debug "Time has come to execute top command (\#$id) in queue"
	    # We don't have time, we are already late: evaluate the
	    # command and its rendez-vous (if any).
	    set QUEUE(state) EXEC
	    set cmd [string map [list %plug% $p \
				     %queue% $q \
				     %index% $id \
				     %schedule% $when \
				     %now% $now] $cmd]
	    if { [catch {eval $cmd} res] } {
		${log}::warn "Could not execute queued command $cmd: $res"
	    } else {
		set rdv [string map [list %plug% $p \
					 %queue% $q \
					 %index% $id \
					 %schedule% $when \
					 %now% $now \
					 %result% $res] $rdv]
		if { $rdv ne "" } {
		    if { [catch {eval $rdv} err] } {
			${log}::warn "Could not execute rendez-vous command $rdv:\
                                      $err"
		    }
		}
	    }
	    __dequeue $q $id
	    set QUEUE(latest) $id
	    set QUEUE(state) IDLE
	    set QUEUE(timer) [after idle [namespace current]::__execute $q]
	}
    }
}

proc ::plugwise::__enqueue { q schedule p cmd { rdv "" } } {
    variable PWISE
    variable log

    if { ![::uobj::isa $q queue] } {
	return -code error "$q unknown or wrong type!"
    }
    upvar \#0 $q QUEUE

    # Detect current time and generate an identifier for the command.
    # This identifier can be used to remove the command from the
    # queue, for example.
    set now [clock clicks -milliseconds]
    set id [incr QUEUE(idgene)]

    # Schedule when to execute the command in the queue.  A positive
    # number means in xxx ms (from now).  Zero means ASAP.  A negative
    # number, whichever it is means prioritised ASAP, meaning that
    # this will be sooner than any command that would have been
    # scheduled ASAP.  In the end, the variable when contains the
    # timestamp at which the command should execute.
    if { $schedule < 0 } {
	# Peek next command that would be scheduled from the queue and
	# arrange for our timestamp to be BEFORE the one for that
	# command.
	set next [__peek $q]
	if { $next eq "" } {
	    set when $now
	} else {
	    foreach {- when - - -} $next break
	    if { $when > $now } {
		set when $now; # ASAP, next command was further in time
	    } else {
		incr when -1;  # 1ms before next command.
	    }
	}
    } else {
	set when [expr {$now+$schedule}]
    }

    # Add command to queue, we don't care about the order since we
    # will be peeking in order each time necessary.
    ${log}::debug "Enqueuing command \#$id for execution later than [__tstamp $when]"
    lappend QUEUE(pending) [list $id $when $p $cmd $rdv]
    __execute $q;   # Test if we should run a command

    return $id
}


proc ::plugwise::__dequeue { q what { filter "*" } } {
    variable PWISE
    variable log

    if { ![::uobj::isa $q queue] } {
	return -code error "$q unknown or wrong type!"
    }
    upvar \#0 $q QUEUE

    set newqueue {}
    foreach spec $QUEUE(pending) {
	foreach {id when p cmd rdv} $spec break
	if { [string is integer $what] } {
	    if { $id != $what || ![string match $filter $cmd] } {
		lappend newqueue $spec
	    }
	} else {
	    if { $p ne $what || ![string match $filter $cmd] } {
		lappend newqueue $spec
	    }
	}
	set QUEUE(pending) $newqueue
    }
    __execute $q
}


proc ::plugwise::config { p args } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    ::uobj::inherit PLUG OLD
    set result [eval ::uobj::config PLUG "-*" $args]
    
    # Create/get command queue for device associated to plug.
    if { $OLD(-dev) ne $PLUG(-dev) && $PLUG(queue) ne "" } {
	__dequeue $PLUG(queue) $p
    }
    if { $PLUG(-dev) ne "" } {
	set PLUG(queue) [__queue $PLUG(-dev)]
    } else {
	set PLUG(queue) ""
    }

    if { [lsearch [list "INITED" "ERROR"] $PLUG(state)] >= 0 } {
	if { $OLD(-state) ne $PLUG(-state) && $PLUG(-state) ne "" } {
	    switch $p $PLUG(-state)
	}

	if { $OLD(-frequency) ne $PLUG(-frequency) } {
	    if { $PLUG(-frequency) > 0 } {
		# (re)initialise the plug state poller whenever we change
		# the check frequency.
		__pulse $p power init
	    } else {
		set PLUG(-frequency) $OLD(-frequency)
	    }
	}
    }

    if { [llength [::uobj::diff OLD PLUG]] > 0 } {
	# Only generate a Configure event if something as changed,
	# otherwise every access to the object's properties will
	# generate a configure event!
	::event::generate $p Configure
    }

    # If we haven't initialised the plug yet, do it know.
    # Initialisation is done in a synchronised manner so that callers
    # will be able to access at least some of the details of the plug
    # at initialisation time.
    if { $PLUG(state) eq "NONE" } {
	init $p on
    }

    return $result
}


proc ::plugwise::new { mac args } {
    variable PWISE
    variable log

    set mac [mac $mac]
    if { $mac eq "" } {
	${log}::error "$mac is not a valid plugwise MAC address"
	return -code error "$mac invalid MAC address"
    }

    set p [::uobj::new [namespace current] plug]
    upvar \#0 $p PLUG

    set PLUG(self) $p
    set PLUG(mac) $mac;       # MAC address in ZigBee network
    set PLUG(state) NONE;     # State of the state machine
    set PLUG(poll:power) "";  # Identifier of polling command
    set PLUG(usage) {};       # Power usage over time
    set PLUG(poll:energy) ""; # Identifier of energy consumption polling cmd
    set PLUG(energy) {};      # Energy consumption in Wh over time
    set PLUG(errors) 0;       # Number of errors when polling state
    set PLUG(queue) "";       # Command execution queue.

    # Extract from PWISE the options which are not globals and should
    # be inherited by each object, i.e. by each plug.
    set options [list]
    foreach o [array get PWISE -*] {
	if { [lsearch $PWISE(globals) $o] < 0 } {
	    lappend options $o
	}
    }

    ::uobj::inherit PWISE PLUG $options
    ::uobj::objectify $p [list [list config configure] [list destroy delete] \
			      switch get init]
    eval config $p $args

    return $p
}


package provide plugwise $::plugwise::version
