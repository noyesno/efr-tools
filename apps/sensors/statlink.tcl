set options {
    { logfile.arg "%APPDATA%/me3gas/%progname%.log" "Where to save log for every run" }
    { feed.integer "52002" "Pachube feed to update" }
    { key.arg "" "pachube security key" }
    { refresh.integer "2" "Refresh rate for data acquisition" }
    { root.arg "http://efrecon.homedns.org:8808/live" "Root URL or directory to acquire pump data from" }
    { once "" "Should we only run once and then exit?" }
    { context.arg "https://localhost:8800/" "Root URL of context manager" }
    { targets.arg "pump 55851044-b290-56a5-3c88-d64ffbfa75e9 temp 003be15d-e3d5-5939-18a6-c492d1cf631e" "Target objects in context manager" }
    { matcher.arg "%1% pump:radiatorIn %2% temp:value %2% pump:outside %3% pump:hotWater %6% pump:compressor %7% pump:heatFluidOut %8% pump:heatFluidIn %9% pump:coldFluidIn %10% pump:coldFluidOut int(%13%)!=0 pump:compressing int(%14%*3000+%15%*6000) pump:resistance" "Matcher for sensor id to field name" }
    { timeout.integer "30000" "Timeout when waiting for sensor value" }
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
# Remove pachube security key from default arguments, make this a
# "statlink.arg" file instead, not on SVN.

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
    -depends [list event rest cxapi] \
    -load [list cxapi] \
    -packages [list rest uri base64] \
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


proc ::extract { data { convert "" } } {
    global RPT

    set latest ""
    foreach line [split $data "\n"] {
	if { $line ne "" } {
	    set latest $line
	}
    }

    if { $latest ne "" } {
	foreach {tm val} [split $latest ","] break
	if { $convert ne "" && $val ne "" } {
	    switch -glob -nocase -- $convert {
		b* {
		    if { $val } {
			return 1
		    } else {
			return 0
		    }
		}
		i* {
		    return [expr int($val)]
		}
		f* {
		    return [expr double($val)]
		}
		default {
		    $RPT(log)::error "$convert is not a known conversion!"
		}
	    }
	} else {
	    return $val
	}
    }

    return ""
}


proc ::latest { dev { lbl "" } } {
    global RPT

    set now [clock seconds]
    set root [string trimright $RPT(root) "/"]/[clock format $now -format "%Y"]/[clock format $now -format "%m"]/[clock format $now -format "%d"]

    set data ""
    set url ${root}/${dev}.txt
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

    return [::extract $data $lbl]
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
	while { [regexp -indices -start $idx {%\w+%} $xp range] > 0 } {
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


    if { $RPT(feed) > 0 } {
	update:pachube
    }
    update:context

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
    set RPT(cx) [::cxapi::new -url $RPT(context) -timeout $RPT(timeout)]
}


::net:init
::api:init
after idle ::update

vwait forever