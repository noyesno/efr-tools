# ::api:cxmgr -- Call operation on object in context
#
#	This procedure will call an operation on an object in the
#	context manager.  It takes an addition list of pairs that will
#	be pushed to the server as part of the query to further
#	specify the arguments of the operation.
#
# Arguments:
#	root	Root, i.e. directory behind UUID in path or root URL to context
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
    global CM

    if { $uuid eq "" } {
	$CM(log)::warn "Empty UUID specified, call not possible"
	return ""
    }

    # Construct the URL that will contain the API call at the context.
    if { [string range $root 0 3] eq "http" } {
	set url [string trimleft $root "/"]
    } else {
	set url [string trimright $CM(rcontext) "/"]/[string trimleft $root "/"]
    }
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
    $CM(log)::notice "Executing operation $op for object $uuid in context\
                      manager"
    set cmd [list ::http::geturl $url -headers $hdrs]
    if { $CM(timeout) > 0 } {
	lappend cmd -timeout $CM(timeout)
    }

    # Evaluate and return, make sure we clean things up.
    set data ""
    if { [catch {eval $cmd} token] } {
	$CM(log)::error "Error while getting URL at $url: $token"
    } else {
	if { [::http::ncode $token] == 200 } {
	    set data [::http::data $token]
	    $CM(log)::debug "Received data: $data"
	} else {
	    $CM(log)::warn "Could not get $url: error is\
                            '[::http::error $token]' status is\
                            '[::http::status $token]'"
	}
	::http::cleanup $token
    }

    return $data
}


proc ::api:context { uuid { op "" } { qry {} } } {
    return [::api:cxmgr context $uuid $op $qry]
}

proc ::api:pachube { uuid { op "" } { qry {} } } {
    return [::api:cxmgr conduits/cosm $uuid $op $qry]
}

proc ::api:remote { uuid { op "" } { qry {} } } {
    return [::api:cxmgr conduits/rcontext $uuid $op $qry]
}

proc ::api:gcal { uuid { op "" } { qry {} } } {
    return [::api:cxmgr conduits/gcal $uuid $op $qry]
}

proc ::api:UPnP { uuid { op "" } { qry {} } } {
    return [::api:cxmgr conduits/upnp $uuid $op $qry]
}
