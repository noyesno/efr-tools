# We must have at least 8.6 for IPv6 support!
package require Tcl

# Implements a contiki me3gas listener that subscribes to a mote,
# listen to its content (on UDP and/or HTTP) and forwards data to a
# context manager.

# JE IPv6 for the mote should be: 2001:5c0:1508:8d00:212:7400:101:7 it
# offers temperature as a feature.

set options {
    { logfile.arg "%APPDATA%/me3gas/%progname%.log" "Where to save log for every run" }
    { ports.arg "http:9090 udp:6050 ws:9099" "Ports to listen on for incoming data, format is protocol followed by colon followed by port numer. Known protocols are udp, http and ws (WebSocket)" }
    { context.arg "https://localhost:8800/" "Root URL to context manager" }
    { report.arg "localhost:9900 cpu accdf0f3-108e-56ac-446e-9b0398eeeb7a {value value date timestamp sampling sampling}" "List of serial/type matches to UUID/fieldName" }
    { sampling.integer "120000" "Initial sampling rate, will be computed later" }
    { myaddr.arg "" "My address, to select network interfaces. Empty to pick" }
}

array set CTKI {
    logfd       ""
    debug       0
    timeout     40000
    udp_order   {udp ceptcl}
    udp         ""
}


source [file join [file dirname $argv0] lib init.tcl]
if { [string is true $CTKI(debug)] } {
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
    global CTKI
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
    global CTKI

    set logline "\[$dt\] \[$srv\] \[$lvl\] $str"

    # Return if no logfile specified
    if { $CTKI(logfile) eq "" } {
	return 1
    }

    if { $CTKI(logfd) eq "" } {
	set fname [file normalize [::diskutil::fname_resolv $CTKI(logfile)]]
	set d [file dirname $fname]
	if { ! [file isdirectory $d] } {
	    file mkdir $d
	}
	if { ! [catch {open $fname w+} fd] } {
	    set CTKI(logfd) $fd
	}
    }
    if { $CTKI(logfd) ne "" } {
	puts $CTKI(logfd) $logline
    }
    return 1
}


# Initialise everything in one go...
::init::init \
    -store CTKI \
    -options $options \
    -booleans [list] \
    -depends [list progver event rest udp] \
    -load [list minihttpd] \
    -packages [list rest ip uri base64 {http 2.8.5}] \
    -parsed ::init:fix \
    -outlog ::log:out

# Find an appropriate UDP implementation
foreach udp $CTKI(udp_order) {
    if { [catch {package require $udp} ver] == 0 } {
	$CTKI(log)::notice "UDP support via $udp at version $ver"
	set CTKI(udp) $udp
	break
    }
}
if { $CTKI(udp) eq "" } {
    $CTKI(log)::critical "No support for UDP found."
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
    global CTKI

    upvar \#0 $dev DEV

    # Parse through the known reporting directives if we find one that
    # matches the sensor serial and type that we should report for.
    foreach {ip key uuid dst} $CTKI(report) {
	foreach {ip port} [::net:ip $ip] break
	if { $ip == $DEV(ip) && $key == $DEV(key) } {
	    $CTKI(log)::debug "Reporting sensor $DEV(ip)/$DEV(key) to\
                               $uuid: $DEV(value)"
	    # Construct a URL, based on the UUID of the receiving
	    # object and on the field specification that is contained
	    # in the report specification.
	    set url [string trimright $CTKI(context) "/"]/context/$uuid/set?
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
			# Check value of sampling, since, at least the
			# first time, we don't have a sampling
			# frequence available all the time.
			if { $DEV(sampling) ne "" } {
			    lappend qry $field
			    lappend qry $DEV(sampling)
			}
		    }
		}
	    }

	    # Send the data to the context manager using the data and
	    # the query that we have just constructed.
	    append url [eval [linsert $qry 0 ::http::formatQuery]]
	    set cmd [list ::http::geturl $url]
	    if { $CTKI(timeout) > 0 } {
		lappend cmd -timeout $CTKI(timeout)
	    }
	    if { [catch {eval $cmd} token] } {
		$CTKI(log)::error "Error while getting URL at $url: $token"
	    } else {
		$CTKI(log)::debug "Posting to context manager at $url"
		if { [::http::ncode $token] == 200 } {
		    set data [::http::data $token]
		}
		::http::cleanup $token
	    }
	}
    }
}


proc ::net:receiver { peer pkt } {
    global CTKI

    $CTKI(log)::debug "Received raw packet from [join $peer :]: $pkt"

    # Converts value to dictionary
    set data [::json:to_dict $pkt]

    set devs [::uobj::find [namespace current] sensor \
		 [list ip == [lindex $peer 0]]]
    foreach dev $devs {
	upvar \#0 $dev DEV
	$CTKI(log)::debug "Sender is $DEV(ip)/$DEV(key)"
	if { [catch {dict get [dict get $data $DEV(key)] "value"} value] } {
	    $CTKI(log)::error "Could not understand data coming from device:\
                               $value"
	} else {
	    set now [clock microseconds]
	    if { $DEV(timestamp) ne "" } {
		set DEV(sampling) [expr {($now - $DEV(timestamp))/1000}]
	    } else {
		if { $CTKI(sampling) > 0 } {
		    set DEV(sampling) $CTKI(sampling)
		} else {
		    set DEV(sampling) ""
		}
	    }
	    set DEV(timestamp) $now
	    set DEV(value) $value
	    ::report $dev
	}
    }
}


proc ::udp:receiver { sock } {
    global CTKI

    set peer [fconfigure $sock -peer]
    ::net:receiver $peer [read $sock]

}

proc ::cep:receiver { sock addr port } {
    ::net:receiver [list $addr $port] [read $sock]
}


proc ::htreceive { prt sock url qry } {
    global CTKI

    set peer [fconfigure $sock -peer]
    set data [::minihttpd::data $prt $sock]
    ::net:receiver [lreplace $peer 0 0] $data
}


proc ::wsreceive { sock type msg } {
    global CTKI

    if { $type eq "text" } {
	set peer [fconfigure $sock -peer]
	::net:receiver [lreplace $peer 0 0] $msg
    }
}


proc ::dev:__subscribed { dev token } {
    global CTKI

    upvar \#0 $dev DEV

    if { [::http::ncode $token] == 200 } {
	$CTKI(log)::debug "Subscribed to dev at $DEV(ip)/$DEV(key)"
    } else {
	$CTKI(log)::warn "Could not subscribe at $DEV(ip)/$DEV(key): error is\
                              '[::http::error $token]' status is\
                              '[::http::status $token]', HTTP code was:\
                              [::http::ncode $token]"
    }
    ::http::cleanup $token
}


proc ::dev:subscribe { dev } {
    global CTKI

    upvar \#0 $dev DEV
    set srv [lindex [::uobj::allof [namespace current] server] 0]
    upvar \#0 $srv SERVER
    $CTKI(log)::debug "Binding device $DEV(ip):$DEV(port)/$DEV(key) to server\
                       $SERVER(protocol):$SERVER(port)"

    # Build a dictionary that will form our request, i.e. asking the
    # mote to send back data to us on the UDP port specified in the
    # arguments.
    set registration "\{"
    append registration "\"proto\":\"$SERVER(protocol)\","
    append registration "\"host\":\"$CTKI(myaddr)\","
    append registration "\"port\":$SERVER(port),"
    if { [string match "w*" $SERVER(protocol)] } {
	append registration "\"path\":\"/push![::uobj::id $dev]\","
    } else {
	append registration "\"path\":\"/receive![::uobj::id $dev]\","
    }
    append registration "\"interval\":[expr $CTKI(sampling)/1000]"
    append registration "\}"
    
    # Establish a subscription at the mote.
    if { [::ip::version $DEV(ip)] == 6 } {
	set rooturl "http://\[$DEV(ip)\]:$DEV(port)/cfg"
    } else {
	set rooturl "http://$DEV(ip):$DEV(port)/cfg"
    }
    set cmd [list ::http::geturl $rooturl \
		 -myaddr $CTKI(myaddr) \
		 -method POST \
		 -type "application/json" \
		 -query $registration \
		 -progress ::dev:__progress \
		 -command [list ::dev:__subscribed $dev]]
    if { $CTKI(timeout) > 0 } {
	lappend cmd -timeout $CTKI(timeout)
    }

    if { [catch {eval $cmd} token] } {
	$CTKI(log)::error "Error while getting URL at $rooturl: $token"
	set token ""
    }

    return $token
}


proc ::dev:__create { ip port key token } {
    global CTKI

    upvar \#0 $token htstate
    if { [::http::ncode $token] == 200 } {
	$CTKI(log)::info "Analysing resources available from $htstate(url)"
	set dev ""

	set data [::http::data $token]
	set data [::json:to_dict $data]

	set node_type "<unknown>"
	if { [dict exists $data "node"] } {
	    set node [dict get $data "node"]
	    set node_type [dict get $node "node-type"]
	}
	
	if { [dict exists $data "rsc"] } {
	    set rsc [dict get $data "rsc"]
	    if { [dict exists $rsc $key] } {
		set dev [::uobj::new [namespace current] sensor]
		upvar \#0 $dev DEV
		set DEV(type) $node_type
		set DEV(timestamp) ""
		set DEV(ip) $ip
		set DEV(port) $port
		set DEV(key) $key
		set DEV(value) [dict get [dict get $rsc $key] "value"]
		set DEV(sampling) ""
		$CTKI(log)::debug "Initialised connection to\
                                   $DEV(type): $DEV(ip):$DEV(port)/$DEV(key)\
                                   = $DEV(value)"

		# Report current value at once, otherwise we will have
		# to wait for a whole sampling period for reporting.
		::report $dev; 
	    } else {
		$CTKI(log)::notice "Device at $ip has no resource called $key:\
                                    available are [dict keys $rsc]"
	    }
	} else {
	    $CTKI(log)::notice "Device at $ip has no resource descriptions"
	}

	if { $dev ne "" } {
	    ::dev:subscribe $dev
	}

    } else {
	$CTKI(log)::warn "Could not get $htstate(url): error is\
                              '[::http::error $token]' status is\
                              '[::http::status $token]', HTTP code was:\
                              [::http::ncode $token]"
    }
    ::http::cleanup $token
}


proc ::dev:__progress { token total current } {
    global CTKI

    upvar \#0 $token state
    $CTKI(log)::debug "Read ${current}/${total} bytes from $state(url)"
}


proc ::net:ip { spec } {
    global CTKI

    # Try understanding all combinations of hostname/ip and port
    # number with a colon as a separator.
    if { [string match {\[[0-9a-fA-F:]*\]:[0-9]*} $spec] } {
	set colon [string last ":" $ip]
	set port [string trim [string range $spec $colon end] ":"]
	set ip [string trim [string range $spec 0 $colon] "\[\]:"]
    } elseif { [::ip::version $spec] == 6 } {
	set ip $spec
	set port 80
    } elseif { [string match {*:[0-9]*} $spec] } {
	foreach {ip port} [split $spec ":"] break
    } else {
	set ip $spec
	set port 80
    }

    return [list $ip $port]
}

proc ::dev:init { ip key } {
    global CTKI

    foreach {ip port} [::net:ip $ip] break

    # Open a socket to the remote web server to guess our own IP
    # address.
    if { $CTKI(myaddr) eq "" } {
	$CTKI(log)::info "Detecting our own IP address"
	if { [catch {socket $ip $port} sock] } {
	    $CTKI(log)::error "Could not open connection to $ip: $sock"
	} else {
	    foreach {myip hst prt} [fconfigure $sock -sockname] break
	    set CTKI(myaddr) $myip
	    $CTKI(log)::notice "Our IP address is $CTKI(myaddr) (DNS: $hst)"
	    close $sock
	}
    } 

    # Now get the complete description of the mote via http to make
    # sure that it has the specified key among its resources and to be
    # able to create an object encapsulating it.
    set dev ""
    if { [::ip::version $ip] == 6 } {
	set rooturl "http://\[${ip}\]:${port}/"
    } else {
	set rooturl "http://${ip}:${port}/"
    }
    $CTKI(log)::notice "Initialising connection to mote at $rooturl..."
    set gcmd [list ::http::geturl $rooturl \
		  -progress ::dev:__progress \
		  -blocksize 127 \
		  -command [list ::dev:__create $ip $port $key]]
    if { $CTKI(timeout) > 0 } {
	lappend gcmd -timeout $CTKI(timeout)
    }
    
    if { [catch {eval $gcmd} token] } {
	$CTKI(log)::error "Error while getting URL at $rooturl: $token"
	set token ""
    }
    return $token
}


proc ::net:init {} {
    global CTKI

    package require tls
    ::http::register https 443 [list ::tls::socket]

    foreach spec $CTKI(ports) {
	foreach {proto port} [split $spec ":"] break
	set srv [::uobj::new [namespace current] server]
	upvar \#0 $srv SERVER
	set SERVER(id) $srv
	set SERVER(port) $port
	set SERVER(protocol) $proto
	set SERVER(sock) ""
	
	switch -nocase -- $SERVER(protocol) {
	    "udp" {
		# Register server
		switch $CTKI(udp) {
		    "ceptcl" {
			set SERVER(sock) [cep -type datagram -domain inet6 \
					      -server ::cep:receiver \
					      $SERVER(port)] 
			fconfigure $SERVER(sock) \
			    -buffering none \
			    -translation binary
		    }
		    "udp" {
			set SERVER(sock) [udp_open $SERVER(port) reuse]
			fconfigure $SERVER(sock) \
			    -buffering none \
			    -translation binary
			fileevent $SERVER(sock) readable \
			    [list ::udp:receiver $SERVER(sock)]
		    }
		}
		if { $SERVER(sock) eq "" } {
		    $CTKI(log)::notice "Cannot listen on UDP port $SERVER(port)\
                                        no support for UDP found"
		    ::uobj::delete $srv
		} else {
		    $CTKI(log)::notice "Now listening on UDP port $SERVER(port)\
                                        for incoming data"
		}
	    }
	    "http" {
		set myaddr $CTKI(myaddr)
		if { $myaddr ne "" && [::ip::version $myaddr] == 6 } {
		    set myaddr "\[$myaddr\]"
		}
		set port [::minihttpd::new "" $SERVER(port) \
			      -externhost $myaddr \
			      -selfvalidate ""]
		::minihttpd::handler $port /receive!* ::htreceive \
		    "application/json"
	    }
	    "ws" -
	    "websocket" {
		set myaddr $CTKI(myaddr)
		set port [::minihttpd::new "" $SERVER(port) \
			      -externhost $myaddr \
			      -selfvalidate ""]
		::minihttpd::live $port /push!* ::wsreceive
	    }
	}
    }
}

# Create device listeners
foreach {ip key uuid matching} $CTKI(report) {
    ::dev:init $ip $key
}
::net:init

vwait forever
