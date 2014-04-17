##################
## Module Name     --  encontrol.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##     Implements a connection to enControl
##
##################


set options {
    { logfile.arg "%APPDATA%/me3gas/%progname%.log" "Where to save log for every run" }
    { port.integer "8899" "Port number of the web server that we implement" }
    { hostname.arg "" "Our real hostname as from the external world" }
    { mappings.arg {http://localhost:8802/ 55851044-b290-56a5-3c88-d64ffbfa75e9 %outside% mote4.temp0 http://localhost:8802/ fc24cc87-dbfb-5243-5e9c-06ffd532a473 %temperature% mote7.temp0} "Available mappings" }
    { credentials.arg "" "Credentials when accessing enControl, separated by colon sign" }
}

# The format of the mappings is based on triplets, where the first
# item is the UUID of an object within the context manager, the second
# is a (unique within the selection) string identifying the object and
# the last describes how to decide the colour (see next).  Colour
# decision is built on a list of pairs where the first item is an
# expression and the second item is the resulting colour when that
# expression is true.  The logic is that as soon as an expression in
# the list validates, that colour is selected.  The expression can
# contain some of the fields of the object, enclosed in % signs, so
# that if an object as a field called value, the string %value% in the
# expression will be dynamically replaced by the value when executing
# the test expression.


array set ENCTRL {
    logfd       ""
    debug       1
    timeout     5000
    triggers    ""
    current     ""
    soap        https://www.encontrol.es/API/DataService.svc
    soaproot    encontrol.api.service/DataService
}


source [file join [file dirname $argv0] lib init.tcl]
if { [string is true $ENCTRL(debug)] } {
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
    global ENCTRL
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
    global ENCTRL

    set logline "\[$dt\] \[$srv\] \[$lvl\] $str"

    # Return if no logfile specified
    if { $ENCTRL(logfile) eq "" } {
	return 1
    }

    if { $ENCTRL(logfd) eq "" } {
	set fname [file normalize [::diskutil::fname_resolv $ENCTRL(logfile)]]
	set d [file dirname $fname]
	if { ! [file isdirectory $d] } {
	    file mkdir $d
	}
	if { ! [catch {open $fname w+} fd] } {
	    set ENCTRL(logfd) $fd
	}
    }
    if { $ENCTRL(logfd) ne "" } {
	puts $ENCTRL(logfd) $logline
    }
    return 1
}


# Initialise everything in one go...
::init::init \
    -store ENCTRL \
    -options $options \
    -depends [list progver event rest cxapi] \
    -load [list minihttpd cxapi] \
    -packages [list http tls rest uri base64] \
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


proc ::soap:call { action args { hdrs {} } } {
    global ENCTRL

    set URL https://www.encontrol.es/API/DataService.svc
    lappend hdrs Content-Type text/xml
    lappend hdrs SOAPAction $action

    set dta "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<soap:Envelope soap:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"><soap:Body>"
    append dta "<"
    append dta [lindex [split $action "/"] end]
    append dta " xmlns=\""
    append dta [lindex [split $action "/"] 0]
    append dta "\">"
    foreach {k v} $args {
	append dta "<${k}>"
	append dta $v
	append dta "</${k}>"
    }
    append dta "</"
    append dta [lindex [split $action "/"] end]
    append dta ">"
    append dta "</soap:Body></soap:Envelope>"
    set tok [::http::geturl $ENCTRL(soap) \
		 -query $dta \
		 -method POST \
		 -headers $hdrs]
    return $tok
}


proc ::soap:result { token tag } {
    global ENCTRL

    if { [::http::ncode $token] == 200 } {
	set res [::http::data $token]
	regexp <$tag>(.*)</$tag> $res - result
    } else {
	upvar \#0 $token RES
	$ENCTRL(log)::error "Could not get $RES(url): error is\
                             '[::http::error $token]', status is\
                             '[::http::status $token]', HTTP code was:\
                             '[::http::ncode $token]'"
	parray RES
	set result ""
    }

    ::http::cleanup $token
    return $result
}


proc ::net:init {} {
    global ENCTRL

    # Start serving on port number and sharing proper directory.
    set srv [::minihttpd::new "" $ENCTRL(port) -externhost $ENCTRL(hostname)]
    if { $srv < 0 } {
	$ENCTRL(log)::error "Cannot start serving on port $ENCTRL(port)"
	exit
    }

    # Remember the server that we implement
    set ENCTRL(srv) $srv

    # Make sure we implement a little ReST ourselves to select among
    # the different sources.
    #::minihttpd::handler $srv /select ::rest:select "application/json"
    #::minihttpd::handler $srv /selectors ::rest:selectors "application/json"
    ::minihttpd::handler $srv /callback ::rest:callback "application/json"

    # Bring in TLS, just in case.
    package require tls
    ::http::register https 443 [list ::tls::socket]

    # Login at enControl
    foreach {usr pwd} [split $ENCTRL(credentials) ":"] break
    set ret [::soap:call $ENCTRL(soaproot)/LoginCookie \
		 [list login $usr password $pwd]]
    set cookie [::soap:result $ret LoginCookieResult]
    if { $cookie eq "" } {
	$ENCTRL(log)::error "Cannot login at encontrol!"
	exit
    }

    foreach { c1 c2 } [split $cookie "\#"] break
    set ENCTRL(cookie) "ASP.NET_SessionId=$c1; "
    append ENCTRL(cookie) ".ASPXAUTH=$c2"
    $ENCTRL(log)::notice "Logged in as $usr at $ENCTRL(soap)"
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
    }
    switch $unit {
	us {
	    set us $tstamp
	}
	ms {
	    set us [expr {$tstamp * 1000}]
	}
	s {
	    set us [expr {$tstamp * 1000000}]
	}
    }

    # Now convert to the RFC3339 format, making sure to support the
    # proper number of decimals in the output, i.e. following the
    # variant specification.
    set secs [expr {$us / 1000000}]
    set micro [expr {$us % 1000000}]
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

proc ::extract { str } {
    set ids {}
    set idx 0
    while { [regexp -indices -start $idx {%\w+%} $str range] > 0 } {
	foreach {start stop} $range break
	lappend ids [string range $str [expr $start + 1] [expr $stop - 1]]
	set idx [expr $stop + 1]
    }

    return $ids
}


# ::rest:callback -- Callback receiving data whenever value in object changes
#
#	This procedure is the REsT callback that is registered at the
#	context manager and that will be triggered (read: called back)
#	whenever the value of a field in the object changes.  It finds
#	the matching selector in our mappings, and, if found,
#	changes the colour of the lamp according to the selection
#	specification.
#
# Arguments:
#	arg1	descr
#	arg2	descr
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::rest:callback { prt sock url qry } {
    global ENCTRL

    set clr ""
    set q_idx [dict get $qry __index]

    # Look for object and selection name within our known selection,
    # if found forward this to the procedure select which will select
    # the proper colour.
    set idx 0
    foreach { root uid xpr sensor } $ENCTRL(mappings) {
	if { $q_idx == $idx } {
	    set mapping {}
	    foreach name [lsort -unique [::extract $xpr]] {
		if { [dict exists $qry $name] } {
		    lappend mapping %$name% [dict get $qry $name]
		}
	    }
	    set value [string map $mapping $xpr]
	    if { [catch {expr $value} result] == 0 } {
		$ENCTRL(log)::debug "$xpr=$result -- sending to enControl"
		set ret [::soap:call \
			      $ENCTRL(soaproot)/UpdateSensorData \
			     [list sensorName $sensor \
				  value $result \
				  date [::to_rfc3339 "" s]] \
			     [list Cookie $ENCTRL(cookie)]]
		set result [::soap:result $ret UpdateSensorDataResult]
		if { $result eq "01" } {
		    $ENCTRL(log)::debug "Updated sensor $sensor to $result at\
                                         enControl"
		} else {
		    $ENCTRL(log)::warn "Error when updating sensor $sensor:\
                                        $result"
		}
		break
	    } else {
		$ENCTRL(log)::error "Could not compute result or $xpr: $result"
	    }
	}
	incr idx
    }

    return "\{\"colour\":\"$clr\"\}"
}


proc ::bind {} {
    global ENCTRL

    set idx 0
    foreach {root uuid xpr sensor} $ENCTRL(mappings) {
	# Getting data for object to check that it exists
	set cx [::cxapi::find $root]
	if { $cx eq "" } {
	    set cx [::cxapi::new -url $root -timeout $ENCTRL(timeout)]
	}

	set obj [$cx context $uuid]
	if { $obj ne "" } {
	    # Build a query string that will arrange for us to get all
	    # the details about the field in that object
	    set qry {}
	    foreach name [lsort -unique [::extract $xpr]] {
		lappend qry $name %$name%
	    }
	    # Append index to query so we can know who this is for on
	    # return.
	    lappend qry __index $idx
	
	    # Install a callback that will get back to us when
	    # anything happen on the object.
	    set bdy [eval [linsert $qry 0 ::http::formatQuery]]
	    set rcv [::minihttpd::fullurl $ENCTRL(srv) /callback]
	    $ENCTRL(log)::debug "Installing a trigger to call us back at $rcv"
	    set cbinfo \
		[$cx context $uuid listen \
		     [list \
			  always on \
			  receiver $rcv \
			  body $bdy]]
	    set cb [json2dict $cbinfo]
	    set tuid [dict get $cb uuid]
	    lappend ENCTRL(triggers) $root $uuid $xpr $tuid
	    $ENCTRL(log)::notice "Added trigger $tuid at remote context\
                                  manager at $root"
	} else {
	    $ENCTRL(log)::warn "$uuid does not exist within context manager!"
	}
	incr idx
    }
}


::net:init
::bind

vwait forever