#######################
##
## This program aims at transforming data from the context manager
## into another web service. Its original goal is to push into sensor
## networks in order to achieve actuation in satisfatory ways.
##
## The program offers an alternative to the triggers concept proponed
## by the context engine.  However, we need 8.6 to make sure we can
## talk IPv6 to sensors.  First, it offers ways to represent more
## complex transformations of data. Secondly, it releases some of the
## clutter of virtual objects and complex triggers that might populate
## the context manager for the representation of transformation.
##
## Finally, the program contains a mechanism for the use of wget
## whenever data is to be pushed to the remote web service.  This is
## to work around a bug when running Tcl 8.6 and IPv6 in Windows.
##
#######################

# TODO:
# 
# Fix GET and PUT of data over wget.
#
# Put wget in the background and provide a way to detect that it is
# has worked properly and to parse the data that is sent back.
#
# Express ways of parsing the results of the transformation operations
# to analyse if they have been successful. Implement on top of regular
# http::geturl and on top of wget conduit.
#
# Make cross transformations posting, i.e. use the value of variables
# from several transformations when posting to a remote web service.
# Since we store the value of variables, we are very close to being
# able to do this.


# NOTES:
# 
# This has only been tested against the windows binaries found at the
# following location: http://nebm.ist.utl.pt/~glopes/wget/. These are
# self-contained binaries with both SSL and IPv6 support.


# At least 8.5, but preferrably 8.6 for proper IPv6 support
package require Tcl 8.5

set options {
    { logfile.arg "%APPDATA%/me3gas/%progname%.log" "Where to save log for every run" }
    { port.integer "9900" "Port to listen to for incoming changes" }
    { hostname.arg "" "Hostname as seen from external clients (used for NAT)" }
    { transforms.arg "@%progdir%/%progname%.tsl" "Translations from objects in context manager, either as list or filename  (start with @)" }
    { timeout.integer "10000" "Timeout when talking HTTP (in msecs)" }
    { wget.arg "" "Path to WGET for web operations, empty for internal HTTP lib" }
}


array set TSF {
    logfd      ""
    debug      0
    triggers   ""
    comments   "\#!;"
    NaN        "-=(No&tSe@t/Ye%t)=-"
}


source [file join [file dirname $argv0] lib init.tcl]
if { [string is true $TSF(debug)] } {
    ::init::debughelper
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
    global TSF

    set logline "\[$dt\] \[$srv\] \[$lvl\] $str"

    # Return if no logfile specified
    if { $TSF(logfile) eq "" } {
	return 1
    }

    if { $TSF(logfd) eq "" } {
	set fname [file normalize [::diskutil::fname_resolv $TSF(logfile)]]
	set d [file dirname $fname]
	if { ! [file isdirectory $d] } {
	    file mkdir $d
	}
	if { ! [catch {open $fname w+} fd] } {
	    set TSF(logfd) $fd
	}
    }
    if { $TSF(logfd) ne "" } {
	puts $TSF(logfd) $logline
    }
    return 1
}


# Initialise everything in one go...
::init::init \
    -store TSF \
    -options $options \
    -depends [list progver event cxapi http] \
    -load [list minihttpd cxapi websocket] \
    -packages [list http ip tls uri base64 uuid] \
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


# ::net:init -- Initialise network connections
#
#       Initialises the network connections by creating a web server
#       for the reception of notifications from the context engine and
#       make sure to register for a REsT entry point.
#
# Arguments:
#       None.
#
# Results:
#       None.
#
# Side Effects:
#       Will listen for incoming web connections on the port specified
#       as an argument.  The server is rootless as only the REsT entry
#       point will respond to incoming requests.
proc ::net:init {} {
    global TSF

    set srv [::minihttpd::new "" $TSF(port) -externhost $TSF(hostname)]
    if { $srv < 0 } {
	$TSF(log)::error "Cannot start serving on port $TSF(port)"
	exit
    }

    # Remember the server we implement
    set TSF(srv) $srv

    # Make sure we implement ReST
    ::minihttpd::handler $srv /push ::rest:push "application/json"
    
    # Bring in TLS, just in case
    package require tls
    ::http::register https 443 [list ::tls::socket]    
}


# ::net:report -- Cleanup states after HTTP request
#
#	This procedure is registered to be called once requests to the
#	remote web server registered within a transformation have
#	ended.  It cleans up states, tracing results in the log files.
#
# Arguments:
#	t	Identifier of the transformation
#	url	URL that was requested
#	token	Token from the HTTP request
#
# Results:
#	None.
#
# Side Effects:
#	Destroys all HTTP request data as contained in token
proc ::net:report { t url token } {
    global TSF

    if { ![::uobj::isa $t transform] } {
	$TSF(log)::error "$t unknown or wrong type"
	return -code error "$t unknown or wrong type"
    }
    upvar \#0 $t TRF
    if { [::http::ncode $token] == 200 } {
	$TSF(log)::debug "Success in calling back $url"
    } else {
	$TSF(log)::warn "Could not callback $url: error is\
                         '[::http::error $token]', status is\
                         '[::http::status $token]'"
    }
    ::http::cleanup $token
}


# ::push:data -- Compute current data value
#
#	Given the current values of the "variables" that have been
#	declared for a tranformation, perform the tokenisation
#	necessary to compute the value of the data that should be sent
#	further to the remote server.
#
# Arguments:
#	t	Identifier of the transformation
#
# Results:
#	Return the current value of the data in the transformation.
#
# Side Effects:
#	None.
proc ::push:data { t } {
    global TSF

    upvar \#0 $t TRF

    # Compute data for HTTP operation
    set map {}
    foreach v [::uobj::allof [namespace current] variable [::uobj::id $t]] {
	upvar \#0 $v VAR
	if { $VAR(value) ne $TSF(NaN) } {
	    lappend map %$VAR(-name)% $VAR(value)
	}
    }
    set dta [string map $map $TRF(-data)]

    return $dta
}


# ::push:wget -- Push data to remote service with wget.
#
#	For a given transformation, actualise the value of the data
#	and push it to the remote service using the external tool
#	wget. Current implementation does not place wget in the
#	background, which we might want to do.
#
# Arguments:
#	t	Identifier of the transformation
#
# Results:
#	None.
#
# Side Effects:
#	Call wget to perform the external web request, using most of
#	the defaults.  Ensure that wget only performs one attempt to
#	avoid blocking too long.
proc ::push:wget { t } {
    global TSF

    upvar \#0 $t TRF
    array set URL [::uri::split $TRF(-destination)]

    # Prepare call to wget: Output to stdout, just try once, quiet
    # output to make sure we only display the response in the log
    # file.
    set bin [::diskutil::fname_resolv $TSF(wget)]
    set cmd [list $bin -O - -t 1 -q]

    # Add Basic Authentication headers
    set hdrs [list]
    if { [array names URL user] ne "" \
	     && $URL(user) ne "" && $URL(pwd) ne "" } {
	lappend cmd --http-user="$URL(user)" --http-password="$URL(pwd)"
    }
    
    if { $TRF(-type) ne "" } {
	lappend cmd --header="Content-Type: $TRF(-type)"
    }
    lappend cmd --no-check-certificate
    if { $TRF(-operation) eq "POST" } {
	lappend cmd --post-data=[::push:data $t]
    } else {
	$TSF(log)::error "$TRF(-operation) not supported with WGET yet!"
	return
    }
    lappend cmd $TRF(-destination)

    $TSF(log)::debug "Executing: $cmd"
    if { [catch {exec {*}$cmd} err] } {
	$TSF(log)::error "Could not execute external wget: $err"
    } else {
	$TSF(log)::debug "Requested for $TRF(-destination) returned: $err"
    }
}


# ::push:internal -- Push data to remote service.
#
#	For a given transformation, actualise the value of the data
#	and push it to the remote service using the internal http
#	library.  Arrange for a callback to be placed once the
#	callback has ended in order not to block.
#
# Arguments:
#	t	Identifier of the transformation
#
# Results:
#	None.
#
# Side Effects:
#	Do a web call, which might block during when the connection is
#	established (and when hostname is resolved).
proc ::push:internal { t } {
    global TSF

    upvar \#0 $t TRF
    array set URL [::uri::split $TRF(-destination)]

    # Add Basic Authentication headers
    set hdrs [list]
    if { [array names URL user] ne "" \
	     && $URL(user) ne "" && $URL(pwd) ne "" } {
	set auth [::base64::encode "$URL(user):$URL(pwd)"]
	lappend hdrs Authorization "Basic $auth"
    }

    # Prepare the HTTP operation
    set url $TRF(-destination)
    set cmd [list ::http::geturl $url -headers $hdrs]
    if { $TSF(timeout) > 0 } {
	lappend cmd -timeout $TSF(timeout)
    }
    if { $TRF(-type) ne "" } {
	lappend cmd -type $TRF(-type)
    }
    if { $TRF(-operation) ne "" } {
	lappend cmd -method [string toupper $TRF(-operation)]
    }
    if { $TRF(myaddr) ne "" } {
	lappend cmd -myaddr $TRF(myaddr)
    }
    lappend cmd -query [::push:data $t]
    lappend cmd -command [list ::net:report $t $url]
    if { [catch {eval $cmd} token] } {
	$TSF(log)::error "Error while posting callback: $token"
    }
}


# ::rest:push -- Receive trigger from context engine
#
#	Receive trigger from context manager, comput value of
#	transformation variables and push to relevant web service.
#
# Arguments:
#	prt	Port of web server
#	sock	Socket to client
#	url	Local URL being requested
#	qry	Content of query.
#
# Results:
#	None.
#
# Side Effects:
#	Perform a web call to the remote service!
proc ::rest:push { prt sock url qry } {
    global TSF

    # Get object id from transform and back to the object itself, if
    # possible.  Happily ignore unknown transforms, they are probably
    # from a prior instance of ourselves.
    set id [dict get $qry __transform]
    set t [::uobj::find [namespace current] transform \
	       [list tuid == $id]]
    if { $t eq "" } {
	$TSF(log)::debug "$id not a transform anymore (old instance?), ignoring"
	return
    }
    upvar \#0 $t TRF

    # Compute current value of all variables, do this so we don't
    # overide previous value if we had problems with the mathematical
    # expression.
    foreach v [::uobj::allof [namespace current] variable [::uobj::id $t]] {
	upvar \#0 $v VAR
	set map {}
	foreach f [dict keys $qry] {
	    if { $f ne "__transform" } {
		lappend map %$f% [dict get $qry $f]
	    }
	}
	set xp [string map $map $VAR(-expr)]
	if { [catch {expr $xp} val] == 0 } {
	    set VAR(value) $val
	}
    }
    
    set colon [string first ":" $TRF(-destination)]
    set scheme [string tolower [string range $TRF(-destination) 0 $colon]]
    if { $scheme eq "http:" || $scheme eq "https:" } {
	if { $TSF(wget) eq "" } {
	    ::push:internal $t
	} else {
	    ::push:wget $t
	}
    }
}


# ::net:ip -- Extract IP and port from specification
#
#	Try extracting IP address and port number from a specification
#	that can be written as an address and a port number separated
#	by a colon.  Supports default ports, IPv6 addresses, etc.
#
# Arguments:
#	spec	Incoming spec, preferrably addess:port format.
#	dft	Default port (80 by default)
#
# Results:
#	Return a list of the address and port number
#
# Side Effects:
#	None
proc ::net:ip { spec { dft 80 } } {
    global TSF

    # Try understanding all combinations of hostname/ip and port
    # number with a colon as a separator.
    if { [string match {\[[0-9a-fA-F:]*\]:[0-9]*} $spec] } {
	set colon [string last ":" $ip]
	set port [string trim [string range $spec $colon end] ":"]
	set ip [string trim [string range $spec 0 $colon] "\[\]:"]
    } elseif { [::ip::version $spec] == 6 } {
	set ip $spec
	set port $dft
    } elseif { [::ip::version [string trim $spec "\[\]"]] == 6 } {
	set ip [string trim $spec "\[\]"]
	set port $dft
    } elseif { [string match {*:[0-9]*} $spec] } {
	foreach {ip port} [split $spec ":"] break
    } else {
	set ip $spec
	set port $dft
    }

    return [list $ip $port]
}


# ::init:transforms -- Initialise all transformations
#
#	Create transformation objects from the list of registered
#	transformations, registering triggers for them at the context
#	manager.  Each variable will also lead to an object.
#
# Arguments:
#	None.
#
# Results:
#	Return the list of transforms objects that were created
#
# Side Effects:
#	Register a trigger for the specified field at the context
#	manager for each transform that is declared.
proc ::init:transforms {} {
    global TSF

    set transforms [list]
    foreach {root uuid translations dst op data type} $TSF(transforms) {
	# Create an object containing most of the parameters.
	set t [::uobj::new [namespace current] transform]
	upvar \#0 $t TRF
	set TRF(-root) $root
	set TRF(-uuid) $uuid
	set TRF(-destination) $dst
	set TRF(-operation) [string toupper $op]
	set TRF(-data) $data
	set TRF(-type) $type
	set TRF(fields) {}
	set TRF(myaddr) ""
	set TRF(tuid) [::uuid::uuid generate]

	# Represent each variable that the transformation as a sub
	# object of the transform.
	foreach {var expr} $translations {
	    set v [::uobj::new [namespace current] variable [::uobj::id $t]]
	    upvar \#0 $v VAR
	    set VAR(-name) $var
	    set VAR(-expr) $expr
	    set VAR(value) $TSF(NaN)
	    set VAR(transform) [::uobj::id $t]

	    set idx 0
	    while { [regexp -indices -start $idx {%\w+%} $expr range] > 0 } {
		foreach {start stop} $range break
		lappend TRF(fields) \
		    [string range $expr [expr $start+1] [expr $stop-1]]
		set idx [expr $stop+1]
	    }
	    set TRF(fields) [lsort -unique $TRF(fields)]
	    $TSF(log)::debug "Fields necessary for $uuid at $root:\
                              [join $TRF(fields) ,]"
	}

	# Establish a context for communicating with the (remote)
	# context manager, making sure we reuse contexts if possible.
	set cx [::cxapi::find $TRF(-root)]
	if { $cx eq "" } {
	    set cx [::cxapi::new -url $TRF(-root) -timeout $TSF(timeout)]
	}
	set TRF(cx) $cx
	
	# Build a query to make sure we only are notified upon the
	# proper set of fields and we know which transform this refers
	# to.
	set qry {}
	foreach f $TRF(fields) {
	    lappend qry $f %$f%
	}

	# Add the unique identifier of the transformation to the query
	# so we can find ourselves back when the trigger fire within
	# the context engine.  This identifier is a UUID, which
	# ensures that we can start the transformer any number of
	# times and happily ignore old triggers that would still
	# remain within the context manager and lead to us (from prior
	# instances).
	lappend qry __transform $TRF(tuid)

	# Install a callback so we are notified any time the value of
	# the fields for this transformation will change.
	set bdy [eval [linsert $qry 0 ::http::formatQuery]]
	set rcv [::minihttpd::fullurl $TSF(srv) /push]
	$TSF(log)::debug "Installing a trigger to call us back at $rcv"
	set cbinfo \
	    [$TRF(cx) context $TRF(-uuid) listen \
		 [list \
		      receiver $rcv \
		      body $bdy]]
	set cb [json2dict $cbinfo]
	set TRF(trigger) [dict get $cb uuid]
	$TSF(log)::notice "Added trigger $TRF(trigger) at remote context\
                           manager"

	# Open a socket to remote server to guess our own IP to that
	# address.  We only need to do this when using the services of
	# the internal (hacked) HTTP library.
	if { $TRF(myaddr) eq "" && $TSF(wget) eq "" } {
	    # Extract destination.
	    set idx [string first "://" $TRF(-destination)]
	    incr idx [string length "://"]
	    set end [string first "/" $TRF(-destination) $idx]
	    incr end -1
	    foreach {ip port} \
		[::net:ip [string range $TRF(-destination) $idx $end]] break

	    $TSF(log)::info "Detecting our own IP address towards $ip"
	    if { [catch {socket $ip $port} sock] } {
		$TSF(log)::error "Could not open connection to $ip: $sock"
	    } else {
		foreach {myip hst prt} [fconfigure $sock -sockname] break
		set TRF(myaddr) $myip
		$TSF(log)::notice "Our IP address is $TRF(myaddr) (DNS: $hst)"
		close $sock
	    }
	}

	lappend transforms $t;  # Remember identifier of new transform
    }

    return $transforms
}

# Read set of transforms from the file (or get it from the command line).
set TSF(transforms) [string trim $TSF(transforms)]
if { [string index $TSF(transforms) 0] eq "@" } {
    set fname [::diskutil::fname_resolv [string range $TSF(transforms) 1 end]]
    $TSF(log)::info "Reading transformations from $fname..."
    set TSF(transforms) [::diskutil::lread $fname 7 "transformations file"]
}

# Initialise network and create and register transformations.
::net:init
set transforms [::init:transforms]
$TSF(log)::info "Created and registered [llength $transforms] transformations"
vwait forever
