set options {
    { logfile.arg "%APPDATA%/me3gas/%progname%.log" "Where to save log for every run" }
    { port.arg "https:8800 http:8802" "Port number(s) where to listen for REsT request. To liste to secure and unsecure, give http:XXX https:YYY as an argument, default decided by value of pki" }
    { schema.arg "http://localhost:8802/model.sha" "Schema of objects we support" }
    { context.arg "http://localhost:8802/myhouse.mdl" "Context of objects we support" }
    { pki.arg "%progdir%/cfg/cxManager-public.pem %progdir%/cfg/cxManager-private.pem" "Path to public and private keys to run with HTTPS, empty for HTTP" }
    { auths.arg "" "List of colon separated pairs of usernames and passwords" }
    { timeout.integer "10000" "Timeout when triggering callbacks via HTTP" }
    { www.arg "%progdir%/models" "Directory we serve with HTTP" }
    { hostname.arg "" "Our real hostname as from the external world" }
    { pachkey.arg "" "Key when talking to pachube" }
    { pairing.arg "" "Path to pairing configuration file" }
    { redis.arg "localhost:6379" "Hostname and port of REDIS server, empty to disable" }
    { history.arg "%progdir%/history" "Directory for CSV output, empty to disable" }
    { cache.integer "5" "Number of historical values to keep in internal memory cache, negative to disable" }
}

array set CM {
    logfd       ""
    debug       0
    cx          ""
    realm       "me3gas Context Engine"
    comments    "\#!;"
    conduits    {}
    https       0
    permanent   ""
    rest        {}
    pertain     1000
    idgene      0
}


source [file join [file dirname $argv0] lib init.tcl]
if { [string is true $CM(debug)] } {
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
    global CM
    global tcl_platform

    upvar \#0 $args ARGS
    if { $tcl_platform(platform) eq "windows" } {
	lappend ARGS(-packages) registry
    }

}

proc ::init:tls { glbl args } {
    global CM

    # Arrange for a nice fallback in case we haven't got access to TLS,
    # thus no https connections.
    if { [catch {package require tls} err] } {
	$CM(log)::error "Will not be able to provide secured connections!\
                         (reason: $err)"
	set CM(port) [string map [list "https:" "http:"] $CM(port)]
	$CM(log)::warn "Forcedly replaced all HTTPS serving ports to HTTP!"
	set CM(https) 0
    } else {
	set CM(https) 1
	$CM(log)::notice "HTTPS supported, all green!"
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
    global CM

    set logline "\[$dt\] \[$srv\] \[$lvl\] $str"

    # Return if no logfile specified
    if { $CM(logfile) eq "" } {
	return 1
    }

    if { $CM(logfd) eq "" } {
	set fname [file normalize [::diskutil::fname_resolv $CM(logfile)]]
	set d [file dirname $fname]
	if { ! [file isdirectory $d] } {
	    file mkdir $d
	}
	if { ! [catch {open $fname w+} fd] } {
	    set CM(logfd) $fd
	}
    }
    if { $CM(logfd) ne "" } {
	puts $CM(logfd) $logline
    }
    return 1
}


# Initialise everything in one go...
::init::init \
    -store CM \
    -options $options \
    -booleans [list] \
    -depends [list event uuidhash oauth rest redis udp] \
    -load [list minihttpd websocket schema model db ssdp UPnP cxapi csvout] \
    -packages [list struct::tree http uuid] \
    -sources [list json find trigger rest tree api cache pairing] \
    -parsed ::init:fix \
    -loaded ::init:tls \
    -outlog ::log:out

set dir [file join $::init::libdir conduits]
foreach f [glob -directory $dir -nocomplain -tails -- *.tcl] {
    source [file join $dir $f]
    lappend CM(conduits) [file rootname $f]
}
::init::configuration;   # Re-configure again to configure the conduits!


proc ::net:httpd { port { pki "" } } {
    global CM

    set www [::diskutil::fname_resolv $CM(www)]
    set srv [::minihttpd::new $www $port -pki $pki \
		 -authorization [list "*" $CM(realm) $CM(auths)] \
		 -externhost $CM(hostname)]
    if { $srv < 0 } {
	return $srv
    }

    # Handler for query of current context
    ::minihttpd::handler $srv /context ::rest:context "application/json"
    ::minihttpd::handler $srv /context/ ::rest:context "application/json"
    # Handler for queries for current objects and classes.
    set hex "\[a-fA-F0-9\]"
    set uuid "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]" 
    ::minihttpd::handler $srv /context/$uuid ::rest:uuid "application/json"
    ::minihttpd::handler $srv /context/$uuid/ ::rest:uuid "application/json"
    # Handlers for modifications of current objects, this get verbose
    # since we are trying to support several API-styles, and to make
    # sure we can have a trailing (unecessary) slash for lazy callers.
    ::minihttpd::handler $srv /context/$uuid/set ::rest:set "application/json"
    ::minihttpd::handler $srv /context/$uuid/set/ ::rest:set "application/json"
    ::minihttpd::handler $srv /context/set ::rest:set "application/json"
    ::minihttpd::handler $srv /context/set/ ::rest:set "application/json"
    ::minihttpd::handler $srv /context/$uuid/append ::rest:append \
	"application/json"
    ::minihttpd::handler $srv /context/append ::rest:append "application/json"
    ::minihttpd::handler $srv /context/append/ ::rest:append "application/json"
    ::minihttpd::handler $srv /context/$uuid/listen ::rest:listen \
	"application/json"
    ::minihttpd::handler $srv /context/listen ::rest:listen "application/json"
    ::minihttpd::handler $srv /context/listen/ ::rest:listen "application/json"
    ::minihttpd::live $srv /context/$uuid/stream ::rest:stream
    ::minihttpd::live $srv /context/stream ::rest:stream
    ::minihttpd::live $srv /context/stream/ ::rest:stream
    ::minihttpd::handler $srv /context/$uuid/destroy ::rest:destroy \
	"application/json"
    ::minihttpd::handler $srv /context/destroy ::rest:destroy "application/json"
    ::minihttpd::handler $srv /context/destroy/ ::rest:destroy \
	"application/json"
    ::minihttpd::handler $srv /context/add ::rest:add "application/json"
    ::minihttpd::handler $srv /context/add/ ::rest:add "application/json"
    ::minihttpd::handler $srv /find ::rest:find "application/json"
    ::minihttpd::handler $srv /find/ ::rest:find "application/json"
    ::minihttpd::handler $srv /locate ::rest:locate "application/json"
    ::minihttpd::handler $srv /locate/ ::rest:locate "application/json"
    ::minihttpd::handler $srv /topmost ::rest:topmost "application/json"
    ::minihttpd::handler $srv /topmost/ ::rest:topmost "application/json"
    ::minihttpd::handler $srv /trigger ::rest:triggers "application/json"
    ::minihttpd::handler $srv /trigger/ ::rest:triggers "application/json"
    ::minihttpd::handler $srv /trigger/$uuid ::rest:uuid "application/json"
    ::minihttpd::handler $srv /trigger/$uuid/ ::rest:uuid "application/json"
    ::minihttpd::handler $srv /trigger/test ::rest:test "application/json"
    ::minihttpd::handler $srv /trigger/test/ ::rest:test "application/json"
    ::minihttpd::handler $srv /trigger/$uuid/test ::rest:test "application/json"
    ::minihttpd::handler $srv /trigger/$uuid/test/ ::rest:test \
	"application/json"
    ::minihttpd::handler $srv /trigger/$uuid/destroy ::rest:destroy \
	"application/json"
    ::minihttpd::handler $srv /trigger/destroy ::rest:destroy "application/json"
    ::minihttpd::handler $srv /trigger/destroy/ ::rest:destroy \
	"application/json"

    return $srv
}


# Initialise network, creating a web server
proc ::net:init {} {
    global CM

    foreach p $CM(port) {
	set srv -1
	if { [string is integer $p] } {
	    set srv [::net:httpd $p $CM(pki)]
	} elseif { [string first ":" $p] >= 0 } {
	    foreach {proto port} [split $p ":"] break
	    switch -nocase -- $proto {
		HTTP {
		    set srv [::net:httpd $port]
		}
		HTTPS {
		    set srv [::net:httpd $port $CM(pki)]
		}
		default {
		    $CM(log)::error "$proto is not a recognised protocol\
                                     specification for server port"
		}
	    }
	}

	if { $srv > 0 } {
	    lappend CM(rest) $srv
	}
    }

    # Initialise HTTP module
    if { $CM(https) } {
	::http::register https 443 ::tls::socket
    }
}


::net:init
set s [::schema::new [::diskutil::fname_resolv $CM(schema)]]
set CM(cx) [::model::new $s]
$CM(cx) create [::diskutil::fname_resolv $CM(context)] name
if { $CM(redis) ne "" } {
    set CM(permanent) [::db::new $CM(cx) -redis $CM(redis)]
}
if { $CM(history) ne "" } {
    set CM(csv) [::csvout::new $CM(cx) -dir $CM(history)]
}

# Initialise all conduits for all supported servers
foreach c $CM(conduits) {
    foreach srv $CM(rest) {
	$CM(log)::notice "Initialising conduit '$c' at /conduits/$c in\
                          server at port #$srv"
	# Set the loglevel of the conduit to the same as the main
	# program, this supposes that it uses log facilities, but make
	# this an option by catching the call.
	catch {::${c}::loglevel $CM(verbose)}
	::argutil::fix_outlog
	# Tell conduit to install its entry points and perform its
	# initialisation for this server.
	::${c}::init $CM(cx) /conduits/$c $srv
    }
}

# Try to pick up a server that is NOT on HTTPS, just
# because we want to get data flowing quickly (but at the
# price of security...)
set freesrv ""
foreach srv $CM(rest) {
    if { [::minihttpd::config $srv -pki] eq "" } {
	set freesrv $srv
	break
    }
}
if { $freesrv eq "" } {
    if { $CM(https) } {
	set freesrv [lindex $CM(rest) 0]
    } else {
	return 0;  # We can't do https...
    }
}

# Register ourselves as the default remote context for API calls to
# remote contexts.
set CM(rcontext) [::minihttpd::fullurl $freesrv]

# Read pairs from configuration file and bind them to the context.
# The current code does not remove the pairing information from memory
# for debugging reasons.
if { $CM(pairing) ne "" } {
    set pairs [::pair:init [::diskutil::fname_resolv $CM(pairing)] $CM(pachkey)]
    foreach p $pairs {
	after idle ::pair:register $p
    }
}

vwait forever
