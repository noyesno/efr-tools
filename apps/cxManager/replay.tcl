##################
## Module Name     --  replay
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    This application is able to replay data that has been captured
##    by the history facility of the context manager at a later
##    time. At present, the application only works properly if a start
##    date has been specified and if data around that date (and time!)
##    is present in the files.  The application is able to map between
##    object UUIDs when replaying from a number of sources to
##    destinations if necessary.
##
##################
package require Tcl

set  options {
    { logfile.arg "%APPDATA%/me3gas/%progname%.log" "Where to save log for every run" }
    { context.arg "http://localhost:8802/" "Root URL to context manager" }
    { dir.arg "~/Desktop/metering electricity/" "Directory containing csv files" }
    { replay.arg "" "List of UUID to replay for" }
    { filter.arg "*--*.csv" "Filter pattern when selecting UUID files" }
    { format.arg "%Y%m%d-%H:%M:%S" "Format for date specification in files" }
    { start.arg "" "Date at which to start, empty for beginning of file" }
    { speed.double "1.0" "Speed for replaying datastreams" }
    { ws "" "Use web socket when communicating with context manager" }
    { picker.integer "1" "Number of seconds between (re)connection establishments" }
    { state.arg "" "UUID of state object: replay, timestamp" }
    { sanity.integer "1814400" "Max number of seconds since last timestamp, othewise corrupt data, negative to turn off" }
}


array set REPLAY {
    logfd      ""
    debug      0
    timeout    10000
    sock       ""
    latest     ""
    controller "INIT"
}

source [file join [file dirname $argv0] lib init.tcl]
if { [string is true $REPLAY(debug)] } {
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
    global REPLAY
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
    global REPLAY

    set logline "\[$dt\] \[$srv\] \[$lvl\] $str"

    # Return if no logfile specified
    if { $REPLAY(logfile) eq "" } {
	return 1
    }

    if { $REPLAY(logfd) eq "" } {
	set fname [file normalize [::diskutil::fname_resolv $REPLAY(logfile)]]
	set d [file dirname $fname]
	if { ! [file isdirectory $d] } {
	    file mkdir $d
	}
	if { ! [catch {open $fname w+} fd] } {
	    set REPLAY(logfd) $fd
	}
    }
    if { $REPLAY(logfd) ne "" } {
	puts $REPLAY(logfd) $logline
    }
    return 1
}


# Initialise everything in one go...
::init::init \
    -store REPLAY \
    -boolean [list ws] \
    -options $options \
    -depends [list progver event cxapi] \
    -load [list cxapi websocket] \
    -packages [list http tls uri base64] \
    -parsed ::init:fix \
    -outlog ::log:out


# ::json2dict -- Convert JSON expression to a Tcl dictionary
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
proc json2dict {json} {
    string range [
	string trim [
	    regsub -- {^(\uFEFF)} [
		string map {\t {} \n {} \r {} , { } : { } \[ \{ \] \}} $json
		] {}
	    ]
	] 1 end-1
}


# ::dict2json -- Convert a dictionary to a JSON expression
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
proc dict2json {dictionary} {
    dict for {key value} $dictionary {
	if {[string match {\[*\]} $value]} {
	    lappend Result "\"$key\":$value"
	} elseif {![catch {dict size}]} {
	    lappend Result "\"$key\":\"[dict2json $value]\""
	} else {
	    lappend Result "\"$key\":\"$value\""
	}
    }
    return "\{[join $Result ","]\}"
}

# ::csv:split -- Split CSV line
#
#       Split an entry from a CSV files according to the CSV
#       specification.
#
# Arguments:
#       line    Line to split up
#
# Results:
#       Return a list of all the fields
#
# Side Effects:
#       None.
proc ::csv:split { line } {
    # XXX: This is only a goss approximation, either we start using
    # the CSV library, either we implement some sort of half-proper
    # split!
    return [split $line ","]
}


# ::replay:get -- Get next entry from CSV file
#
#       Advance within an opened CSV file (the one contained in the
#       replay object) forward to the next entry.  This procedure will
#       automatically skip entries that have the wrong timestamp,
#       i.e. a timestamp that is before the minimum specified as part
#       of the command line arguments (possibly self-discovered as the
#       timestamp from the first line).
#
# Arguments:
#       r    Identifier of a replay object
#
# Results:
#       Return the list of field and values (as a dictionary). The
#       special field __timestamp will contain the time/date of the
#       update in seconds since the epoch.
#
# Side Effects:
#       None.
proc ::replay:get { r } {
    global REPLAY
    
    upvar \#0 $r PLAY
    if { $PLAY(fd) ne "" } {
	while {![eof $PLAY(fd)] } {
	    # Get line and split it
	    set line [gets $PLAY(fd)]
	    set updt [::csv:split $line]

	    # Construct DESCR array with field content of the line
	    for {set i 0} {$i<[llength $PLAY(fields)]} {incr i} {
		set DESCR([lindex $PLAY(fields) $i]) [lindex $updt $i]
	    }

	    # Extract timestamp for update and create an UPDATE array
	    # with solely the fields that were touched by the update.
	    # Return if it is greater than min timestamp (we have to
	    # to work around time skews == 1970 at restarts sometimes!)
	    if { [catch {clock scan $DESCR(__timestamp) \
			     -format $REPLAY(format)} when] } {
		$REPLAY(log)::warn "Could not understand $DESCR(__timestamp)\
                                    according to $REPLAY(format)"
	    } else {
		if { $PLAY(prev) eq "" } {
		    set since 0
		} else {
		    set since [expr {abs($when-$PLAY(prev))}]
		}
		if { ( ($PLAY(min) ne "" && $when > $PLAY(min)) \
			   || $PLAY(min) eq "" ) } {
		    if { $REPLAY(sanity) > 0 && $since < $REPLAY(sanity) \
			     || $REPLAY(sanity) <= 0 } {
			set UPDATE(__timestamp) $when
			foreach f [split $DESCR(__hints) "|"] {
			    # Extra sanity check on the fields.
			    if { [array names DESCR $f] ne "" } {
				set UPDATE($f) $DESCR($f)
			    }
			}
			set PLAY(prev) $when
			return [array get UPDATE]
		    } else {
			$REPLAY(log)::debug "Data rejected from\
                                             $PLAY(fname): $updt"
		    }
		}
	    }
	}
    }
    return {};  # Nothing to parse or end of file
}


# ::replay:stop -- Stop replaying from a source
#
#       Stop replaying from the source associated to a replay object.
#       Replaying source file is closed so no more data can be
#       acquired from it at a later time.
#
# Arguments:
#       r    Identifier of a replay object
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::replay:stop { r } {
    global REPLAY
    
    upvar \#0 $r PLAY
    
    $REPLAY(log)::notice "Connection lost, ending replay"
    if { $PLAY(scheduler) ne "" } {
	after cancel $PLAY(scheduler)
    }
    catch {close $PLAY(fd)}
    set PLAY(fd) ""
}


# ::replay:state -- Send state 
#
#       Send current state of replaying facility to object, if that
#       was possible.
#
# Arguments:
#	updt	Replaying state, a dictionary.
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::replay:state { updt } {
    global REPLAY

    if { [llength $updt] > 0 } {
	if { $REPLAY(ws) } {
	    if { $REPLAY(sock) ne "" } {
		$REPLAY(log)::debug "Sending state update: $updt"
		::websocket::send $REPLAY(sock) text [dict2json $updt]
	    }
	} else {
	    $REPLAY(log)::debug "Pushing state update: $updt"
	    $REPLAY(cx) context $REPLAY(state) set $updt
	}
    }
}    


# ::replay:push -- Push (replayed) update back to context manager
#
#       Push the update specified in the arguments back to the context
#       manager. Current implementation uses regular web API, what
#       about using websockets instead?
#
# Arguments:
#       r    Identifier of a replay object
#       updt Dictionary specifying the update to apply (fields->values)
#
# Results:
#       Return the time/date contained in the update.  This comes from
#       the __timestamp field, a field that is internal to the CSV
#       format and thus not sent back to the context manager.
#
# Side Effects:
#       None.
proc ::replay:push { r updt } {
    global REPLAY
    
    upvar \#0 $r PLAY
    # Remove all "internal" fields, i.e. fields starting with two
    # underscore
    set pushed {}
    foreach {field val} $updt {
	if { ! [string match "__*" $field] } {
	    lappend pushed $field $val
	}
	# Save the timestamp apart so we can return it once we've
	# pushed data.
	if { $field eq "__timestamp" } {
	    set when $val
	}
    }
    
    # Send value to context manager, this is where we could use a
    # websocket, as long as we had opened against the object stream
    # instead.
    if { [llength $pushed] > 0 } {
	if { $REPLAY(ws) } {
	    if { $PLAY(sock) ne "" } {
		$REPLAY(log)::debug "Sending update for $PLAY(dst): $pushed"
		::websocket::send $PLAY(sock) text [dict2json $pushed]
	    }
	} else {
	    $REPLAY(log)::debug "Pushing update for $PLAY(dst): $pushed"
	    $REPLAY(cx) context $PLAY(dst) set $pushed
	}

	if { $when ne "" && $when > $REPLAY(latest) } {
	    ::replay:state [list "timestamp" $when]
	    set REPLAY(latest) $when
	}
    }

    # Return timestamp of update
    return $when
}


# ::replay:scheduler -- Pulse for replaying events
#
#       This is the core procedure of the module.  It arranges to push
#       back updates to the context manager at the pace specified on
#       the command line (speed argument).
#
# Arguments:
#       r    Identifier of the replay object
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::replay:scheduler { r } {
    global REPLAY
    
    upvar \#0 $r PLAY

    # If we have a pending update to push, send it to the context
    # manager.  Having a pending update is the most common case, this
    # if-statement will only fail at the beginning of the replay.
    if { [llength $PLAY(next)] > 0 } {
	set PLAY(pushed) [::replay:push $r $PLAY(next)]
    }

    # Get next update to be performed from the CSV file, jumping over
    # any update that would be out-of-range (time skews).
    set PLAY(next) [::replay:get $r]

    if { [llength $PLAY(next)] > 0 } {
	# Got an update? remember it for next pulse and count how long
	# we should sleep before pushing it back to the context
	# manager.  Make sure we take the replaying speed into
	# account.
	array set UPDATE $PLAY(next)
	set elapsed [expr {$UPDATE(__timestamp) - $PLAY(pushed)}]
	set next [expr {int($elapsed * 1000 / $REPLAY(speed))}]
	set PLAY(scheduler) [after $next ::replay:scheduler $r]
    } else {
	# No update, we are at the end of the file, close it and stop
	# pulsing.  Perhaps should we completely remove the replay
	# object at this point?
	catch {close $PLAY(fd)}
	set PLAY(fd) ""
	$REPLAY(log)::notice "End of data stream for $PLAY(dst)\
                              (at $PLAY(fname))"
    }
}


# ::replay:open -- Initialise a replay object.
#
#       Open the connection to the CSV file that is associated to a
#       replay object and arrange for replaying the historical updates
#       that it contains towards the current context manager.
#
# Arguments:
#       r    Identifier of the replay object.
#
# Results:
#       None.
#
# Side Effects:
#       Open CSV file and keep it opened until end is reached
proc ::replay:open { r } {
    global REPLAY
    
    upvar \#0 $r PLAY
    if { $PLAY(fname) ne "" } {
	# Open file for reading, will be kept opened all along
	set PLAY(fd) [open $PLAY(fname)]

	# Read header so as to discover the fields contained in the
	# object.  We do not provide for field translation at present.
	set hdr [gets $PLAY(fd)]
	set PLAY(fields) [::csv:split $hdr]

	# Read first valid update from file
	set updt [::replay:get $r]
	if { [llength $updt] > 0 } {
	    # Store timestamp of first update contained in file if no
	    # start timestamp was specified.
	    if { $PLAY(min) eq "" } {
		array set UPDATE $updt
		set PLAY(min) $UPDATE(__timestamp)
		$REPLAY(log)::notice "First update from $PLAY(fname) will be at\
                                      $UPDATE(__timestamp)"
	    }
	    # Remember first update, ready to be pushed back to
	    # context manager.
	    set PLAY(next) $updt
	}
    }
}


# ::replay:__context -- Receive state from context manager
#
#       Receive state changes from the object that is specified as to
#       the one that contains the state of the replaying context at
#       the context manager.§
#
# Arguments:
#	r	Identifier of the replay object, empty for state for all app.
#	sock	(web)socket to remote object.
#	type	Type of the message
#	msg	Content of message.
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::replay:__context { r sock type { msg "" } } {
    global REPLAY
    
    if { $r eq "" } {
	switch $type {
	    "text" {
		set dta [::json2dict $msg]
		if { [dict exists $dta "state"] } {
		    set state [string toupper [dict get $dta "state"]]
		    set replays [::uobj::allof [namespace current] replay]
		    switch $state {
			"PLAY" {
			    foreach r $replays {
				upvar \#0 $r PLAY
				if { $PLAY(scheduler) eq "" } {
				    ::replay:scheduler $r
				}
			    }
			    set REPLAY(controller) "PLAY"
			}
			"PAUSE" {
			    foreach r $replays {
				upvar \#0 $r PLAY
				if { $PLAY(scheduler) ne "" } {
				    after cancel $PLAY(scheduler)
				    set PLAY(scheduler) ""
				}
			    }
			    set REPLAY(controller) "PLAY"
			}
			"STOP" {
			}
		    }
		}
	    }
	    "close" {
	    }
	    "connect" {
		set REPLAY(sock) $sock
		$REPLAY(log)::info "Connected to context for exchanging state\
                                    data with $REPLAY(state)"
		::replay:state [list "state" $REPLAY(controller)]
	    }
	}
    } else {
	upvar \#0 $r PLAY
	switch $type {
	    "close" {
		::replay:stop $r
	    }
	    "connect" {
		set PLAY(sock) $sock
		$REPLAY(log)::info "Connected to context for sending data to\
                                $PLAY(dst)"
	    }
	}
    }
}


proc ::replay:pick {} {
    global REPLAY

    set ready 0
    if { $REPLAY(state) ne "" && $REPLAY(ws) && $REPLAY(sock) eq "" } {
	set tgt [$REPLAY(cx) wsroot]/$REPLAY(state)/stream
	$REPLAY(log)::debug "Opening WebSocket connection for state to $tgt"
	::websocket::open $tgt [list ::replay:__context ""]
    } else {
	foreach r [::uobj::allof [namespace current] replay] {
	    upvar \#0 $r PLAY
	    
	    if { $REPLAY(ws) } {
		if { $PLAY(sock) eq "" } {
		    set tgt [$REPLAY(cx) wsroot]/$PLAY(dst)/stream
		    $REPLAY(log)::debug "Opening WebSocket connection to $tgt"
		    ::websocket::open $tgt [list ::replay:__context $r]
		    break;  # Just ONE at a time!
		} else {
		    incr ready
		}
	    } else {
		incr ready
	    }
	}
    }

    # All ready, start playing!
    if { $ready >= [llength [::uobj::allof [namespace current] replay]] \
	     && $REPLAY(controller) eq "INIT" } {
	foreach r [::uobj::allof [namespace current] replay] {
	    upvar \#0 $r PLAY
	    if { $PLAY(scheduler) eq "" } {
		$REPLAY(log)::notice "Starting replaying to $PLAY(dst) now"
		::replay:scheduler $r
	    }
	}
	set REPLAY(controller) "PLAY"
	::replay:state [list "state" $REPLAY(controller)]
    }

    # Sleep until next and try picking again
    set when [expr int($REPLAY(picker)*1000)]
    after $when ::replay:pick
}


# ::replay:init -- Initialise application
#
#       For each mapping specified on the command line, create a
#       replay object that binds the historical CSV file to an object
#       in the current context manager.  Replay objects are able to
#       translate from one (past) UUID to another (present) UUID if
#       necessary.
#
# Arguments:
#       None.
#
# Results:
#       None.
#
# Side Effects:
#       Will lead to the opening of as many CSV files as necessary.
proc ::replay:init {} {
    global REPLAY

    # Access where the CSV files containing the historical updates are
    # saved and get list of files from there
    set dir [::diskutil::fname_resolv $REPLAY(dir)]
    set files [glob -directory $dir -tails -- $REPLAY(filter)]

    # Create replay objects for each pairs of UUIDs specified on the
    # command line (pairs are most of the time implicit, i.e. have a
    # similar source and destination UUID).
    foreach mapping $REPLAY(replay) {
	# Find source and destination UUIDs (usually the same)
	if { [llength $mapping] > 1 } {
	    foreach {src dst} $mapping break
	} elseif { [llength [split $mapping ":"]] > 1 } {
	    foreach {src dst} [split $mapping ":"] break
	} else {
	    set src $mapping
	    set dst $mapping
	}
	
	# Create an object and initialise
	set r [::uobj::new [namespace current] replay]
	upvar \#0 $r PLAY

	set PLAY(src) $src
	set PLAY(dst) $dst
	set PLAY(fd) ""
	set PLAY(fields) {}
	set PLAY(next) {}
	set PLAY(pushed) 0
	set PLAY(scheduler) ""
	set PLAY(prev) ""

	# Make sure we know which file to read data from
	set PLAY(fname) ""
	foreach fname $files {
	    # Try finding the UUID source from the pair in the name of
	    # the file, if we do, we are done.
	    if { [string first $src $fname] >= 0 } {
		set PLAY(fname) [file join $dir $fname]
		break
	    }
	}
	
	# No file, remove the we have so far and go to next one...
	if { $PLAY(fname) eq "" } {
	    $REPLAY(log)::warn "Cannot find any data file for $src in $dir!"
	    ::uobj::delete $r
	    break;
	}
	
	# Try opening WebSocket to remote context manager.
	if { $REPLAY(ws) } {
	    set PLAY(sock) ""
	}

	# Decide when to start, setting this will force the reading of
	# as many updates from the CSV files as necessary until the
	# specified timestamp has been found.
	set PLAY(min) ""
	if { $REPLAY(start) ne "" } {
	    set PLAY(min) [clock scan $REPLAY(start) -format $REPLAY(format)]
	}

	# Initialise replay object, i.e. read to very first start line
	# in CSV file, this might take some time...
	$REPLAY(log)::notice "Replaying data for $dst from $PLAY(fname)"
	::replay:open $r
    }
}

# Open connection to context manager and create replay objects for all
# UUID pairs specified on the command line.
set REPLAY(cx) [::cxapi::new -url $REPLAY(context) -timeout $REPLAY(timeout)]
::replay:init
::replay:pick

vwait forever
