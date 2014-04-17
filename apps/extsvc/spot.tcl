##################
## Program Name    --  Spot price watcher and binding to context
## Original Author --  Emmanuel Frécon - emmanuel@sics.se
## Description:
##
##      Controls lamp based on the price of the electricity at the
##      nordic spot market.
##
##################

set options {
    { logfile.arg "%APPDATA%/me3gas/%progname%.log" "Where to save log for every run" }
    { src.arg "http://www.nordpoolspot.com/Market-data1/Elspot/Area-Prices/ALL1/Hourly/" "URL with El-Spot price information"}
    { pause.integer "2" "How often to send data to context manager (in mins)" }
    { check.double "60" "Frequency check for data (in mins)" }
    { region.arg "SE3" "El spot region to watch" }
    { context.arg "https://localhost:8800/" "Root URL of local context manager" }
    { history.arg "%progdir%/spot" "Directory where to save history information" }
    { html.arg "" "Extra file path where to find price info (used for debug)" }
    { uuid.arg "6a00f184-2a3a-5637-6503-018fdc57493f" "UUID of destination object" }
}

array set LS {
    finished     0
    logfd        ""
    dateformat   "%Y%m%d-%H%M%S"
    state        ""
    comments    "\#!;"
    debug       0
    timeout     10000
}


source [file join [file dirname $argv0] lib init.tcl]
if { [string is true $LS(debug)] } {
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
    global LS
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
    global LS

    set logline "\[$dt\] \[$srv\] \[$lvl\] $str"

    # Return if no logfile specified
    if { $LS(logfile) eq "" } {
	return 1
    }

    if { $LS(logfd) eq "" } {
	set fname [file normalize [::diskutil::fname_resolv $LS(logfile)]]
	set d [file dirname $fname]
	if { ! [file isdirectory $d] } {
	    file mkdir $d
	}
	if { ! [catch {open $fname w+} fd] } {
	    set LS(logfd) $fd
	}
    }
    if { $LS(logfd) ne "" } {
	puts $LS(logfd) $logline
    }    
    return 1
}


# Initialise everything in one go...
::init::init \
    -store LS \
    -options $options \
    -booleans [list] \
    -depends [list progver rest event htmlutil] \
    -load [list] \
    -packages [list rest http uri uri::urn htmlutil] \
    -parsed ::init:fix \
    -outlog ::log:out

# Migrate the necessary elements from the local PP context into the
# one of the context manager... This is horribly ugly, should move to
# real modules very soon instead!
foreach k [list log {context rcontext} comments timeout] {
    if { [llength $k] > 1 } {
	foreach {sk dk} $k break
	set CM($dk) $LS($sk)
    } else {
	set CM($k) $LS($k)
    }
}

# Source unpackaged, but mostly well separated modules.
foreach module [list api] {
    source [file join $::init::libdir .. .. cxManager lib ${module}.tcl]
}


proc ::spot:__save { html } {
    global LS

    # Remove all tags from the HTML, keeping only table data and headers
    # for a little while.
    set raw [string map {&nbsp; " "} \
		 [::htmlutil::tagsremove $html [list "td" "th"]]]

    # Insert some spaces in inside the table data to ease parsing further
    # down, and get rid of the remaining HTML tags.
    set txt [::htmlutil::tagsremove \
		 [string map {"</td>" " </td>" "</th>" " </th>"} $raw]]

    set state INIT
    set currency ""
    set regions ""
    set date ""

    foreach ln [split $txt "\n"] {
	set ln [string trim $ln]
	if { $ln ne "" } {
	    switch $state {
		INIT {
		    # Guess the currency, which we also use as a
		    # marker for where we start parsing better
		    if { [regexp "(.*)/MWh" $ln - currency] } {
			$LS(log)::debug "Currency is $currency"
			set state CURRENCY
		    }
		}
		CURRENCY {
		    if { [string equal -nocase "SYS" [lindex $ln 0]] } {
			set regions $ln
			$LS(log)::debug "Available regions are $regions"
			foreach r $regions {
			    set values.$r ""
			}
			set state REGIONS
		    }
		}
		REGIONS {
		    set date [clock scan $ln -format "%d-%m-%Y"]
		    $LS(log)::debug "Parsing data for\
                                     [clock format $date -format %Y%m%d]"
		    set state DATE
		}
		DATE {
		    if { [regexp {(\d\d)\s+\-\s+(\d\d)\s+(.*)} $ln \
			      - from to vals] } {
			for { set i 0 } { $i < [llength $vals] } { incr i } {
			    set r [lindex $regions $i]
			    set v [string map {"," "."} [lindex $vals $i]]
			    lappend values.$r $from $to $v
			}
			if { $from eq "23" } {
			    $LS(log)::debug "Found all hourly prices"
			    set state SKIP
			}
		    }
		}
		SKIP {
		}
	    }
	}
    }

    # Create directory if necessary
    set dir [::diskutil::fname_resolv $LS(history)]
    if { ! [file isdirectory $dir] } {
	$LS(log)::notice "Creating directory for historical data: $dir"
	if { [catch {file mkdir $dir}] } {
	    $LS(log)::error "Could not make directory for historical data"
	}
    }

    # Write into CSV format.
    set f_temp [file join $dir [clock format $date -format "%Y%m%d.tmp"]]
    if { [catch {open $f_temp "w"} fd] == 0 } {
	$LS(log)::debug "Creating temporary file: $f_temp"
	puts $fd ",[join $regions ,]"
	# Traverse across the only region we certainly know of to get the from
	# and to right.
	foreach { from to vals } [set values.SYS] {
	    puts -nonewline $fd "${from}-${to},"
	    foreach r $regions {
		foreach { f t v } [set values.$r] {
		    if { $f eq $from && $t eq $to } {
			puts -nonewline $fd "$v"
		    }
		}
		if { $r eq [lindex $regions end] } {
		    puts $fd ""
		} else {
		    puts -nonewline $fd ","
		}
	    }
	}
	close $fd
    }

    # Now update or create final file.
    set f_name [file join $dir [clock format $date -format "%Y%m%d.csv"]]
    if { [file exists $f_name] } {
	if { [::diskutil::equal_files $f_temp $f_name] } {
	    file delete $f_temp
	    $LS(log)::debug "No change, keeping previous file!"
	} else {
	    file rename -force -- $f_temp $f_name
	    $LS(log)::notice "Updated historical data into $f_name"
	}
    } else {
	file rename $f_temp $f_name
	$LS(log)::notice "Created historical data at $f_name"
    }
}

# Read prices for given date and make these the current ones in the
# program.
proc ::spot:__read { { now "" } } {
    global LS

    if { $now eq "" } {
	set now [clock seconds]
    }

    set dir [::diskutil::fname_resolv $LS(history)]
    set f_name [file join $dir [clock format $now -format "%Y%m%d.csv"]]
    if { [catch {open $f_name "r"} fd] == 0 } {
	set regions [lrange [split [gets $fd] ","] 1 end]
	set LS(state) ""
	while { ! [eof $fd] } {
	    set ln [split [gets $fd] ","]
	    if { [llength $ln] > 0 } {
		foreach { from to } [split [lindex $ln 0] "-"] break
		set idx [lsearch $regions $LS(region)]
		set v [lindex $ln [expr $idx + 1]]
		lappend LS(state) $from $to $v
	    }
	}
	close $fd
    } else {
	$LS(log)::error "Could not open file $f_name for given date: $fd"
    }
}


proc ::spot:__analyse { token } {
    global LS

    if { [::http::ncode $token] == 200 } {
	::spot:__save [::http::data $token]
    } else {
	$LS(log)::warn "Could not get $LS(src): error is\
                        '[::http::error $token]' status is\
                        '[::http::status $token]', HTTP code was:\
                        [::http::ncode $token]"
    }
}
    

proc ::spot:__report { } {
    global LS

    if { $LS(state) ne "" } {
	set min ""
	set max ""
	set current ""

	set h [clock format [clock seconds] -format "%H"]

	foreach { from to val } $LS(state) {
	    if { $max eq "" || $val > $max } {
		set max $val
	    }
	    if { $min eq "" || $val < $min } {
		set min $val
	    }
	    if { $h == $from } {
		set current $val
	    }
	}

	if { $current ne "" } {
	    $LS(log)::debug "Current price is $current <${min},${max}>"
	    ::api:context $LS(uuid) set \
		[list min $min max $max current $current]
	}
	set next [expr int($LS(pause)*60*1000)]
    } else {
	$LS(log)::notice "No price yet, trying to report sooner"
	set next $LS(timeout)
    }

    after $next ::spot:__report
}


# Check for spot prices, save updated prices to historical directory
# and actualise state of program to show current (as of today's)
# prices.
proc ::spot:check {} {
    global LS

    $LS(log)::debug "Getting El spot information from $LS(src)"
    http::geturl $LS(src) -timeout $LS(timeout) -command ::spot:__analyse

    # Prices arrive long before the date change, so we can safely
    # install the state from the (priorly created) file.
    ::spot:__read
    
    set next [expr int($LS(check)*60*1000)]
    after $next ::spot:check
}


proc ::net:init {} {
    global LS

    # Arrange for a nice fallback in case we haven't got access to
    # TLS, thus no https connections.
    if { [catch {package require tls} err] } {
	$LS(log)::error "Will not be able to provide secured connections!\
                         (reason: $err)"
	if { [string tolower [string range $LS(context) 0 4]] eq "https" } {
	    $LS(log)::critical "You have specified a secured context at\
                                $LS(context), restart with unsecured HTTP\
                                instead"
	    exit
	}
    } else {
	::http::register https 443 [list ::tls::socket]
    }
}



if { $LS(html) ne "" } {
    if { [catch {open $LS(html) "r"} fd] == 0 } {
	::spot:__save [read $fd]
	close $fd
	exit
    } else {
	$LS(log)::error "Cannot open extra HTML info at $LS(html): $fd"
    }
}

::net:init
after idle ::spot:check
after idle ::spot:__report


vwait LS(finished)