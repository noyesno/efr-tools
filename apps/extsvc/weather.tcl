##################
## Program Name    --  Weather Forecast
## Original Author --  Emmanuel Frécon - emmanuel@sics.se
## Description:
##
##      This program will continuously (or once) update an object from
##      the context manager to contain the forecasted weather from the
##      coming day.  The application gets it information from the
##      weather underground API and supposes that the fields of the
##      object that it tries to update are compatible with the
##      WeatherStation object class that is currently
##      specified. Actually, it tries to set more, but the
##      WeatherStation object does not implement all the fields.  The
##      list of fields is as follows:
##
##      temperature    Forecast temperature in Celcius.
##      windSpeed      Wind speed in m/s
##      windDirection  Wind direction in degrees.
##      UVIndex        The UV index.
##      humidity       Degree of humidity in percent.
##      pressure       Pressure in HPa
##      rain           millimeters of rain
##      snow           millimeters of snow.
##
##################

set options {
    { logfile.arg "%APPDATA%/me3gas/%progname%.log" "Where to save log for every run" }
    { wunderground.arg "http://api.wunderground.com/api/%key%/hourly10day/%settings%/q/%location%.json" "Wunderground API call, content of % surrounded strings taken from options map"}
    { options.arg "key 0868b8200c6eab89 settings \"\" location Sweden/Stockholm" "Map of options for query" }
    { freq.double "60" "Frequency check for data (in mins), negative or zero for once" }
    { context.arg "https://localhost:8800/" "Root URL of local context manager" }
    { uuid.arg "684c4e19-c4ed-5861-f127-59109a41bb56" "UUID of destination object" }
}

array set WU {
    finished     0
    logfd        ""
    dateformat   "%Y%m%d-%H%M%S"
    state        ""
    comments    "\#!;"
    debug       0
    timeout     30000
}


source [file join [file dirname $argv0] lib init.tcl]
if { [string is true $WU(debug)] } {
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
    global WU
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
    global WU

    set logline "\[$dt\] \[$srv\] \[$lvl\] $str"

    # Return if no logfile specified
    if { $WU(logfile) eq "" } {
	return 1
    }

    if { $WU(logfd) eq "" } {
	set fname [file normalize [::diskutil::fname_resolv $WU(logfile)]]
	set d [file dirname $fname]
	if { ! [file isdirectory $d] } {
	    file mkdir $d
	}
	if { ! [catch {open $fname w+} fd] } {
	    set WU(logfd) $fd
	}
    }
    if { $WU(logfd) ne "" } {
	puts $WU(logfd) $logline
    }    
    return 1
}


# Initialise everything in one go...
::init::init \
    -store WU \
    -options $options \
    -booleans [list] \
    -depends [list progver] \
    -load [list cxapi] \
    -packages [list http uri] \
    -parsed ::init:fix \
    -outlog ::log:out


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


proc ::wu:__store { forecast } {
    global WU

    set datespec [dict get $forecast "FCTTIME"]
    set date [dict get $datespec "epoch"]

    puts "Dealing with [clock format $date] : $forecast"

    set temp [dict get [dict get $forecast "temp"] "metric"]
    set wspeed [dict get [dict get $forecast "wspd"] "metric"]
    set wdir [dict get [dict get $forecast "wdir"] "degrees"]
    set uvi [dict get $forecast "uvi"]
    set humidity [dict get $forecast "humidity"]
    set pressure [dict get [dict get $forecast "mslp"] "metric"]
    set rain [dict get [dict get $forecast "qpf"] "metric"]
    if { $rain eq "" } { set rain 0 }
    set snow [dict get [dict get $forecast "snow"] "metric"]
    if { $snow eq "" } { set snow 0 }
    
    $WU(log)::notice "Storing forecast data for [clock format $date]"
    $WU(cx) context $WU(uuid) set \
	[list __when $date \
	    temperature $temp \
	    windSpeed $wspeed \
	    windDirection $wdir \
	    UVIndex $uvi \
	    humidity $humidity \
	    pressure $pressure \
	    rain $rain \
	    snow $snow]
}


proc ::wu:__receive { t } {
    global WU

    if { [::http::ncode $t] == 200 } {
	set dta [::http::data $t]
	set response [::json:to_dict $dta]
	if { [dict exists $response "hourly_forecast"] } {
	    $WU(log)::debug "Received data for\
                             [llength [dict get $response hourly_forecast]]\
                             hours forward"
	    foreach hspec [dict get $response "hourly_forecast"] {
		::wu:__store $hspec
	    }
	} else {
	    $WU(log)::warn "No properly formatted forecast data available"
	}
    } else {
	$WU(log)::warn "Could not get response from weather underground\
                        error: '[::http::error $t]',\
                        status: '[::http::status $t]',\
                        response code: '[::http::ncode $t]'"
    }

    ::http::cleanup $t
    ::wu:next
}


proc ::wu:next {} {
    global WU

    if { $WU(freq) > 0 } {
	set next [expr int($WU(freq)*60*1000)]
	after $next ::wu:check
    } else {
	exit
    }
}


proc ::wu:check {} {
    global WU

    # Use content of options to create proper URL for the API call.
    $WU(log)::debug "Getting forecast from Weather Underground"
    set mapper [list]
    foreach {k v} $WU(options) {
	set k [string trim $k %]
	lappend mapper %${k}% $v
    }
    set url [string map $mapper $WU(wunderground)]

    $WU(log)::debug "Forecast query string is $url"
    array set URL [::uri::split $url]

    # Arrange for password encryption in the geturl command
    set hdrs [list]
    if { [array names URL user] ne "" \
	     && $URL(user) ne "" && $URL(pwd) ne "" } {
	set auth [::base64::encode "$URL(user):$URL(pwd)"]
	lappend hdrs Authorization "Basic $auth"
    }

    set gcmd [list ::http::geturl $url \
		  -headers $hdrs \
		  -command [list ::wu:__receive]]
    if { $WU(timeout) > 0 } {
	lappend gcmd -timeout $WU(timeout)
    }

    if { [catch {eval $gcmd} token] } {
	$WU(log)::error "Error while getting URL at $url: $token"
	::wu:next
    }
}


proc ::net:init {} {
    global WU

    # Arrange for a nice fallback in case we haven't got access to
    # TWU, thus no https connections.
    if { [catch {package require tls} err] } {
	$WU(log)::error "Will not be able to provide secured connections!\
                         (reason: $err)"
	if { [string tolower [string range $WU(context) 0 4]] eq "https" } {
	    $WU(log)::critical "You have specified a secured context at\
                                $WU(context), restart with unsecured HTTP\
                                instead"
	    exit
	}
    } else {
	::http::register https 443 [list ::tls::socket]
    }
    set WU(cx) [::cxapi::new -url $WU(context) -timeout $WU(timeout)]
}



::net:init
after idle ::wu:check

vwait WU(finished)