# Implements a device poller, i.e. an application that will poll a
# (remote) device at regular intervals and post its data to the
# context manager.  The application is derived from the pump
# application, except that it was adapted to the snooping experiment
# by Liam, i.e. using wireshark to detect incoming data from motes.

# The implemation is rather generic since it has a way to describe the
# content of the lines of the data that is acquired.  timestamp, value
# and unit description are described by regular expressions, which
# should contain a sub-expression in parenthesis and be the timestamp,
# value, unit to extract.  Empty regular expression means no parsing
# for this type of value, all empty means single lines containing a
# single value.  This scheme allows to support almost any kind of
# line-by-line (including one line only!) storage.

# We must have at least 8.6 for IPv6 support!

set options {
    { logfile.arg "%APPDATA%/me3gas/%progname%.log" "Where to save log for every run" }
    { feed.integer "82599" "Pachube feed to update" }
    { key.arg "" "pachube security key" }
    { refresh.integer "2" "Refresh rate for data acquisition (in mins.)" }
    { root.arg "http://86.41.92.251:8888/" "Root URL or directory to acquire data from" }
    { once "" "Should we only run once and then exit?" }
    { context.arg "https://localhost:8800/" "Root URL of context manager" }
    { targets.arg "gas1 7e985c23-7e2a-5174-1eb7-737ac2efbda3 gas2 565d5c8f-018c-53d2-ce32-1eaf5d5c6a18 gas3 31c2a75d-4ff9-595c-f40a-83e000ca5cdc gas4 2cfd19c4-74fb-5270-cb0e-9444d440d260 pump 55851044-b290-56a5-3c88-d64ffbfa75e9 temp 003be15d-e3d5-5939-18a6-c492d1cf631e" "Target objects in context manager" }
    { matcher.arg "%gasdata% gas1:energy %gasdata3% gas3:energy %gasdata4% gas4:energy" "Matcher for sensor id to field name" }
    { timeout.integer "30000" "Timeout when waiting for sensor value" }
    { extension.arg ".txt" "Extension for document containing values" }
    { timestamp.arg {\[(.*)\]} "Regular expression to extract timestamp from lines" }
    { value.arg {\[.*\]\s*([\.\d]+)} "Regular expression to extract value from lines" }
    { unit.arg {\[.*\]\s*[\.\d]+\s+(\w*)} "Regular expression to extract unit from lines" }
    { convert.arg "" "Conversion to perform on value, if any" }
    { format.arg "" "Format of the timestamp, empty for free format scan" }
    { caching.double "0.2" "Caching factor for device data retention (rel. to refresh period)" }
}

array set RPT {
    logfd       ""
    debug       0
    apiroot     "http://api.cosm.com/v2"
    conversions {boolean float integer}
    cx          ""
}

## TODO
#
# Optimise so we don't get the value of the sensors several times (one
# for the context and one for the direct cosm posting).
#

source [file join [file dirname $argv0] lib init.tcl]
if { [string is true $RPT(debug)] } {
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
    global RPT
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
    global RPT

    set logline "\[$dt\] \[$srv\] \[$lvl\] $str"

    # Return if no logfile specified
    if { $RPT(logfile) eq "" } {
	return 1
    }

    if { $RPT(logfd) eq "" } {
	set fname [file normalize [::diskutil::fname_resolv $RPT(logfile)]]
	set d [file dirname $fname]
	if { ! [file isdirectory $d] } {
	    file mkdir $d
	}
	if { ! [catch {open $fname w+} fd] } {
	    set RPT(logfd) $fd
	}
    }
    if { $RPT(logfd) ne "" } {
	puts $RPT(logfd) $logline
    }
    return 1
}


# Initialise everything in one go...
::init::init \
    -store RPT \
    -options $options \
    -booleans [list once] \
    -depends [list event rest cxapi http] \
    -load [list cxapi] \
    -packages [list rest uri base64 http] \
    -parsed ::init:fix \
    -outlog ::log:out

if {![package vsatisfies [package provide Tcl] 8.6]} {
    $RPT(log)::warn "You need Tcl 8.6 and the specifically modified http\
                     package to get IPv6 access to work!"
}

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
    return "\{[join $Result ",\n"]\}"
}


proc ::convert { value { convert "" } } {
    global RPT

    if { $convert eq "" } {
	set convert $RPT(convert)
    }

    $RPT(log)::debug "Converting '$value' to $convert"
    if { $convert ne "" && $value ne "" } {
	switch -glob -nocase -- $convert {
	    b* {
		if { [string is true $value] } {
		    return 1
		} else {
		    return 0
		}
	    }
	    i* {
		return [expr int($value)]
	    }
	    d* -
	    f* {
		return [expr double($value)]
	    }
	    default {
		$RPT(log)::error "$convert is not a known conversion!"
	    }
	}
    } else {
	return $value
    }
}


proc ::extract { data { convert "" } } {
    global RPT

    set latest 0
    set value ""
    set unit ""

    $RPT(log)::debug "Extracting latest value for sensor"

    # Parse data in a line-by-line manner and analyse each trimmed
    # non-empty line.
    foreach line [split $data "\n"] {
	set line [string trim $line]
	if { $line ne "" } {
	    # Extract timestamp from line, if possible (defaulting to
	    # "now").  This uses the regular expression contained in
	    # -timestamp option to the program (and which should have
	    # a sub-expression in parenthesis matching the exact
	    # string containing the timestamp).  Once we have a
	    # string, we scan it using the clock scanning format
	    # specified by the option -format to the program, or using
	    # free format scan if it was empty.
	    set timestamp [clock seconds]
	    if { $RPT(timestamp) ne "" } {
		if { [catch {regexp $RPT(timestamp) $line mtch val} res] } {
		    $RPT(log)::warn \
			"Could not extract timestamp from line $line: $res"
		} else {
		    if { $res } {
			if { $RPT(format) eq "" } {
			    if { [catch { clock scan $val } ts] } {
				$RPT(log)::warn \
				    "Cannot understand time value $val: $ts"
			    } else {
				set timestamp $ts
			    }
			} else {
			    if { [catch { clock scan $val \
					      -format $RPT(format) } ts] } {
				$RPT(log)::warn \
				    "Cannot understand time value $val: $ts"
			    } else {
				set timestamp $ts
			    }
			}
		    }
		}
	    }

	    # If the analysed timestamp was greater than previous
	    # stored one (greater or equal to make sure that next line
	    # replaces previous line in case no timestamp found), we
	    # extract the value from the line. Special note: if the
	    # value regular expression is empty, we enter a special
	    # case, where we consider the value as being the whole
	    # content of the line.  This is to ease parsing of files
	    # that only contain one value per line...
	    if { $timestamp >= $latest } {
		set latest $timestamp;   # Store latest timestamp
		if { $RPT(value) eq "" } {
		    set value $line;  # Special case: no regexp for value!
		} else {
		    if { [catch {regexp $RPT(value) $line mtch val} res] } {
			$RPT(log)::warn \
			    "Could not extract value from line $line: $res"
		    } else {
			if { $res } {
			    set value $val
			}
		    }
		}

		# Extract unit specification from line, as specified
		# by the option -unit to the program and if this
		# wasn't empty.
		if { $RPT(unit) ne "" } {
		    if { [catch {regexp $RPT(unit) $line mtch val} res] } {
			$RPT(log)::warn \
			    "Could not extract unit from line $line: $res"
		    } else {
			if { $res } {
			    set unit $val
			}
		    }
		}
	    }
	}
    }

    # Once here, latest contains the timestamp of the latest value in
    # the file.  value contains the data value at that time and unit
    # contains a specification of the unit for that data.  We continue
    # as long as we have a non-empty value.

    # Do conversion on the latest value, as specified by the convert
    # argument.
    if { $value ne "" } {
	if { [catch {::convert $value $convert} val] } {
	    $RPT(log)::warn "Could not convert $value to $convert: $val"
	} else {
	    return $val
	}
    }

    return ""
}

proc ::getdata { dev } {
    global RPT

    set root [string trimright $RPT(root) "/"]
    $RPT(log)::debug "Getting latest value for device $dev at $root"

    set data ""
    set url ${root}/${dev}$RPT(extension)
    array set URL [::uri::split $url]
    if { [string range $URL(scheme) 0 3] eq "http" && $URL(path) ne $url } {
	# Arrange for password encryption in the geturl command
	set hdrs [list]
	if { [array names URL user] ne "" \
		 && $URL(user) ne "" && $URL(pwd) ne "" } {
	    set auth [::base64::encode "$URL(user):$URL(pwd)"]
	    lappend hdrs Authorization "Basic $auth"
	}
	
	$RPT(log)::notice "Getting latest value for sensor $dev at $url"
	set cmd [list ::http::geturl $url \
		     -headers $hdrs]
	if { $RPT(timeout) > 0 } {
	    lappend cmd -timeout $RPT(timeout)
	}
	if { [catch {eval $cmd} token] } {
	    $RPT(log)::error "Error while getting URL at $url: $token"
	} else {
	    if { [::http::ncode $token] == 200 } {
		set data [::http::data $token]
	    }
	    ::http::cleanup $token
	}
    } else {
	$RPT(log)::notice "Reading latest value for sensor $dev from $url"
	if { [catch {open $url} fd] } {
	    $RPT(log)::error "Could not open file at $url: $fd"
	} else {
	    set data [read $fd]
	    close $fd
	}
    }

    return $data
}


# Get latest value, implement a cache to avoid fetching values too
# often.
proc ::latest { dev { lbl "" } } {
    global RPT

    set root [string trimright $RPT(root) "/"]
    set url ${root}/${dev}$RPT(extension)
    set now [clock seconds]

    set d [::uobj::find [namespace current] device \
	       [list name == $dev]]
    if { $d eq "" } {
	set d [::uobj::new [namespace current] device]
	upvar \#0 $d DEV
	set DEV(name) $dev
	set DEV(data) ""
	set DEV(latest) 0
    } else {
	upvar \#0 $d DEV
    }

    if { $now - $DEV(latest) > [expr int($RPT(caching)*$RPT(refresh)*60)] } {
	set DEV(latest) $now
	set DEV(data) [::getdata $dev]
    } else {
	$RPT(log)::debug "Using cached value for sensor $dev at $url"
    }

    return [::extract $DEV(data) $lbl]
}


proc ::streams {} {
    global RPT

    set streams {}
    if { [catch {::pachube::feed -feed $RPT(feed)} descr] } {
	$RPT(log)::error "Could not get list of feeds at pachube.com: $descr"
    } else {
	foreach stream [dict get $descr datastreams] {
	    if { [catch {dict get $stream id} id] } {
		$RPT(log)::warn "Datastream does not have any identifier!"
		set id ""
	    }
	    if { [catch {dict get [dict get $stream unit] label} label] } {
		$RPT(log)::warn "No label for unit!"
		set label ""
	    }
	    if { $id ne "" } {
		lappend streams $id "$label"
	    }
	}
    }

    return $streams
}


proc ::update:context {} {
    global RPT

    array set DEVICES {}
    foreach {xp dst} $RPT(matcher) {
	set ids {}
	set idx 0
	while { [regexp -indices -start $idx {%[\w_/.]+%} $xp range] > 0 } {
	    foreach {start stop} $range break
	    lappend ids [string range $xp [expr $start + 1] [expr $stop - 1]]
	    set idx [expr $stop + 1]
	}

	foreach id $ids {
	    set xp [string map [list %$id% [::latest $id]] $xp]
	}
	if { $xp ne "" } {
	    if { [catch {expr $xp} val] == 0 } {
		foreach {dev field} [split $dst ":"] break
		lappend DEVICES($dev) $field $val
	    } else {
		$RPT(log)::error "Could not compute current value from $xp:\
                                  $val"
	    }
	}
    }

    foreach dev [array names DEVICES] {
	foreach {d uuid} $RPT(targets) {
	    if { $d eq $dev } {
		$RPT(cx) context $uuid set $DEVICES($dev)
	    }
	}
    }
}


proc ::update:pachube {} {
    global RPT


    foreach {id lbl} [::streams] {
	if { [lsearch -nocase $RPT(conversions) $lbl] >= 0 } {
	    set val [::latest $id $lbl]
	} else {
	    set val [::latest $id]
	}
	if { $val eq "" } {
	    $RPT(log)::warn "Could not get latest value for sensor $id"
	} else {
	    $RPT(log)::debug "Latest value for sensor $id is $val, posting at\
                              remote site $RPT(apiroot)"
	    if { [catch {::pachube::update -feed $RPT(feed) -id $id \
			     [dict2json [list current_value $val]]} ret] } {
		$RPT(log)::error "Could not update value for sensor $id to\
                                  $val: $ret"
	    }
	}
    }
}

proc ::update {} {
    global RPT

    $RPT(log)::info "Getting data and updating targets"
    if { $RPT(feed) > 0 } {
	update:pachube
    }
    if { $RPT(context) ne "" } {
	update:context
    }

    if { $RPT(once) } {
	exit
    }
    
    set next [expr $RPT(refresh)*60*1000]
    after $next ::update
}


proc ::net:init {} {
    global RPT

    package require tls
    ::http::register https 443 [list ::tls::socket]
}


# This creates an API for pachube, it is far from being complete though.
proc ::api:init {} {
    global RPT

    if { $RPT(feed) > 0 } {
	set pachube(feed) \
	    [list \
		 url $RPT(apiroot)/feeds/%feed%.json \
		 headers [list X-PachubeApiKey $RPT(key)] \
		]
	set pachube(stream) \
	    [list \
		 url $RPT(apiroot)/feeds/%feed%/datastreams/%id%.json \
		 headers [list X-PachubeApiKey $RPT(key)] \
		]
	set pachube(update) \
	    [list \
		 url $RPT(apiroot)/feeds/%feed%/datastreams/%id%.json \
		 method put \
		 body required \
		 headers [list X-PachubeApiKey $RPT(key)] \
		]
	::rest::create_interface pachube
    }

    set RPT(cx) [::cxapi::new -url $RPT(context) -timeout $RPT(timeout)]
}


::net:init
::api:init
after idle ::update

vwait forever
