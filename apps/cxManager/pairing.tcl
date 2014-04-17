##################
## Module Name     --  Pairing to pachube or remote contexts
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##     This program automatically pairs an object from the context
##     manager so that it will report its data to pachube or a remote
##     context (or vice versa).  Pairing relies on the conduits that
##     are built in the context manager.
##
## Commands Exported:
##      cmd1
##      cmd2
##################


set options {
    { logfile.arg "%APPDATA%/me3gas/%progname%.log" "Where to save log for every run" }
    { pairing.arg "%progdir%/cfg/pairing.cfg" "Pairing configuration" }
    { key.arg "Gqpl91eGQHGiUW7wj9FreurApl6SAKxTWG5oRlgvM2dPVT0g" "pachube security key" }
    { context.arg "https://localhost:8800/" "Root URL of local context manager" }
}

array set PP {
    comments    "\#!;"
    logfd       ""
    debug       0
    timeout     10000
}


source [file join [file dirname $argv0] lib init.tcl]
if { [string is true $PP(debug)] } {
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
    global PP
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
    global PP

    set logline "\[$dt\] \[$srv\] \[$lvl\] $str"

    # Return if no logfile specified
    if { $PP(logfile) eq "" } {
	return 1
    }

    if { $PP(logfd) eq "" } {
	set fname [file normalize [::diskutil::fname_resolv $PP(logfile)]]
	set d [file dirname $fname]
	if { ! [file isdirectory $d] } {
	    file mkdir $d
	}
	if { ! [catch {open $fname w+} fd] } {
	    set PP(logfd) $fd
	}
    }
    if { $PP(logfd) ne "" } {
	puts $PP(logfd) $logline
    }
    return 1
}


# Initialise everything in one go...
::init::init \
    -store PP \
    -options $options \
    -booleans [list once] \
    -depends [list progver event rest] \
    -load [list] \
    -packages [list http rest uri base64 uri::urn] \
    -parsed ::init:fix \
    -outlog ::log:out

# Migrate the necessary elements from the local PP context into the
# one of the context manager... This is horribly ugly, should move to
# real modules very soon instead!
foreach k [list log {context rcontext} comments timeout] {
    if { [llength $k] > 1 } {
	foreach {sk dk} $k break
	set CM($dk) $PP($sk)
    } else {
	set CM($k) $PP($k)
    }
}

# Source unpackaged, but mostly well separated modules.
foreach module [list api pairing] {
    source [file join $::init::libdir ${module}.tcl]
}

proc ::net:init {} {
    global PP

    # Arrange for a nice fallback in case we haven't got access to
    # TLS, thus no https connections.
    if { [catch {package require tls} err] } {
	$PP(log)::error "Will not be able to provide secured connections!\
                         (reason: $err)"
	if { [string tolower [string range $PP(context) 0 4]] eq "https" } {
	    $PP(log)::critical "You have specified a secured context at\
                                $PP(context), restart with unsecured HTTP\
                                instead"
	    exit
	}
    } else {
	foreach proto {ssl2 ssl3 tls1} {
	    if { [catch {::tls::ciphers $proto} ciphers] } {
		::tls::init -$proto 0
		$PP(log)::warn "No support for $proto under HTTPS"
	    } else {
		if { [llength $ciphers] > 0 } {
		    ::tls::init -$proto 1
		    $PP(log)::notice "HTTPS will have support for $proto"
		} else {
		    ::tls::init -$proto 0
		    $PP(log)::warn "No support for $proto under HTTPS"
		}
	    }
	}
	::http::register https 443 [list ::tls::socket]
    }
}

::net:init
set pairs [::pair:init [::diskutil::fname_resolv $PP(pairing)] $PP(key)]
foreach p $pairs {
    ::pair:register $p
}

exit
