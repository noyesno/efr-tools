set options {
    { logfile.arg "%APPDATA%/me3gas/%progname%.log" "Where to save log for every run" }
    { context.arg "https://localhost:8800/" "Root URL of context manager" }
    { dst.arg "7650af8a-f919-56df-156d-8257aceb44f3" "Destination UUID" }
    { src.arg "003be15d-e3d5-5939-18a6-c492d1cf631e" "Source UUID" }
}

array set TWT {
    comments    "\#!;"
    logfd       ""
    debug       0
    timeout     5000
}


source [file join [file dirname $argv0] lib init.tcl]
if { [string is true $TWT(debug)] } {
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
    global TWT
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
    global TWT

    set logline "\[$dt\] \[$srv\] \[$lvl\] $str"

    # Return if no logfile specified
    if { $TWT(logfile) eq "" } {
	return 1
    }

    if { $TWT(logfd) eq "" } {
	set fname [file normalize [::diskutil::fname_resolv $TWT(logfile)]]
	set d [file dirname $fname]
	if { ! [file isdirectory $d] } {
	    file mkdir $d
	}
	if { ! [catch {open $fname w+} fd] } {
	    set TWT(logfd) $fd
	}
    }
    if { $TWT(logfd) ne "" } {
	puts $TWT(logfd) $logline
    }
    return 1
}


# Initialise everything in one go...
::init::init \
    -store TWT \
    -options $options \
    -booleans [list] \
    -depends [list progver event] \
    -load [list] \
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


# ::api:cxmgr -- Call operation on object in context
#
#	This procedure will call an operation on an object in the
#	context manager.  It takes an addition list of pairs that will
#	be pushed to the server as part of the query to further
#	specify the arguments of the operation.
#
# Arguments:
#	root	Root, i.e. directory behind UUID in path.
#	uuid	Identifier of the object, class, trigger, etc.
#	op	Operation to execute, there will not be any check
#	qry	List or key and values to use as arguments to operation
#
# Results:
#	Return the JSON from the server, empty on errors.
#
# Side Effects:
#	None.
proc ::api:cxmgr { root { uuid "" } { op "" } { qry "" } } {
    global TWT

    if { $uuid eq "" } {
	return ""
    }

    # Construct the URL that will contain the API call at the context.
    set url [string trimright $TWT(context) "/"]/[string trimleft $root "/"]
    if { $uuid ne "" } {
	append url "/" [string trim $uuid]
    }
    if { $op ne "" } {
	append url "/" [string trim $op "/"]
    }
    if { [llength $qry] > 0 } {
	append url "?" [eval [linsert $qry 0 ::http::formatQuery]]
    }
    array set URL [::uri::split $url]

    # Arrange for password encryption in the geturl command
    set hdrs [list]
    if { [array names URL user] ne "" \
	     && $URL(user) ne "" && $URL(pwd) ne "" } {
	set auth [::base64::encode "$URL(user):$URL(pwd)"]
	lappend hdrs Authorization "Basic $auth"
    }

    # Create command for getting the URL, don't forget the timeout
    $TWT(log)::notice "Executing operation $op for object $uuid in context\
                      manager"
    set cmd [list ::http::geturl $url -headers $hdrs]
    if { $TWT(timeout) > 0 } {
	lappend cmd -timeout $TWT(timeout)
    }

    # Evaluate and return, make sure we clean things up.
    set data ""
    if { [catch {eval $cmd} token] } {
	$TWT(log)::error "Error while getting URL at $url: $token"
    } else {
	if { [::http::ncode $token] == 200 } {
	    set data [::http::data $token]
	}
	::http::cleanup $token
    }

    return $data
}


proc ::api:context { uuid { op "" } { qry {} } } {
    return [::api:cxmgr context $uuid $op $qry]
}

proc ::api:twitter { uuid { op "" } { qry {} } } {
    return [::api:cxmgr conduits/twitter $uuid $op $qry]
}


proc ::net:init {} {
    global TWT

    package require tls
    foreach proto {ssl2 ssl3 tls1} {
	if { [catch {::tls::ciphers $proto} ciphers] } {
	    ::tls::init -$proto 0
	    $TWT(log)::warn "No support for $proto under HTTPS"
	} else {
	    if { [llength $ciphers] > 0 } {
		::tls::init -$proto 1
		$TWT(log)::notice "HTTPS will have support for $proto"
	    } else {
		::tls::init -$proto 0
		$TWT(log)::warn "No support for $proto under HTTPS"
	    }
	}
    }
    ::http::register https 443 [list ::tls::socket]
}


proc ::bind { } {
    global TWT

    set seturl \
	[string trimright $TWT(context) "/"]/context/$TWT(dst)/set?temperature=
    ::api:context $TWT(src) listen \
	[list \
	     method GET \
	     expression "%value%<-5" \
	     receiver ${seturl}FREEZING]
    ::api:context $TWT(src) listen \
	[list \
	     method GET \
	     expression "%value%>=-5 && %value%<10" \
	     receiver ${seturl}COLD]
    ::api:context $TWT(src) listen \
	[list \
	     method GET \
	     expression "%value%>=10 && %value%<25" \
	     receiver ${seturl}WARM]
    ::api:context $TWT(src) listen \
	[list \
	     method GET \
	     expression "%value%>=25" \
	     receiver ${seturl}HOT]

    ::api:twitter $TWT(dst) listen \
	[list \
	     status "%name%: It is %temperature% outside."]
}


::net:init
::bind
