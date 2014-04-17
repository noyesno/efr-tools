##################
## Module Name     --  trigger.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##     This module, not a library in itself, deals with trigger that
##     can be associated to objects and their value.  Triggers are
##     basically HTTP-based events; a trigger will perform an HTTP
##     operation whenever a number of conditions are met on an object.
##     HTTP callbacking is as general as possible and contains enough
##     hooks to suits most of the needs. In particular, it contains
##     ways to send the current value of some (or all) of the fields
##     of the object.  Triggers can also be associated to a condition
##     which needs to be met for the trigger to fire.
##
## Commands Exported:
##      Everything not starting with double __ after ::trigger: (note
##      the single double colon!).
##################


# ::trigger:__report -- Log reporting over trigger execution.
#
#	Report to the log if the trigger could be fired or not. 
#
# Arguments:
#	t	Identifier of the trigger
#	url	URL that was called as part of the trigger.
#	token	Token from the http module
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::trigger:__report { t url token } {
    global CM

    if { ! [::uobj::isa $t trigger] } {
	return -code error "$t unknown or wrong type"
    }
    upvar \#0 $t TRIGGER
    
    if { [::http::ncode $token] == 200 } {
	$CM(log)::debug "Success in calling back $url"
    } else {
	$CM(log)::warn "Could not callback $url: error is\
                        '[::http::error $token]', status is\
                        '[::http::status $token]'"
    }
    ::http::cleanup $token
}


# ::trigger:deliver -- Deliver a trigger
#
#	Deliver the trigger by performing the callback to the remote
#	server.  This is executed once the jittering period has
#	expired.
#
# Arguments:
#	t	Identifier of the trigger
#
# Results:
#	1 if the HTTP callback was sent, 0 otherwise. This does not
#	provide you with a complete information about how well the
#	trigger fired.
#
# Side Effects:
#	None.
proc ::trigger:deliver { t } {
    global CM

    if { ! [::uobj::isa $t trigger] } {
	return -code error "$t unknown or wrong type"
    }
    upvar \#0 $t TRIGGER

    # Replace any occurrence of the name of a field of the object,
    # enclosed by % signs with the current value.
    upvar \#0 $TRIGGER(-object) OBJ
    set map {}
    foreach k [array names OBJ -*] {
	set k [string trimleft $k "-"]
	lappend map %$k% $OBJ(-$k)
    }
    set url [string map -nocase $map $TRIGGER(-receiver)]
    set body [string map -nocase $map $TRIGGER(-body)]

    $CM(log)::debug "Mediating changes on $TRIGGER(-object):\
                     [join [lsort -unique $TRIGGER(changes)] , ]"
    set TRIGGER(scheduled) ""
    set TRIGGER(changes) {}

    set colon [string first ":" $url]
    set scheme [string tolower [string range $url 0 $colon]]

    if { $TRIGGER(-method) eq "WS" } {
	set sock [string range $url [expr {$colon+1}] end]
	::websocket::send $sock text [::json:object $TRIGGER(-object)]
    } elseif { $TRIGGER(-method) eq "CMD" } {
	set cmd [string range $url [expr {$colon+1}] end]
	# url contains the command to callback
	if { [catch {eval $cmd} res] } {
	    $CM(log)::warn "Could not execute internal command $cmd: $res"
	} else {
	    $CM(log)::debug "Executed internal command $cmd"
	}
    } elseif { $scheme eq "http:" || $scheme eq "https:" } {
	array set URL [::uri::split $url]

	# Add Basic authentication headers.
	set hdrs [list]
	if { [array names URL user] ne "" \
		 && $URL(user) ne "" && $URL(pwd) ne "" } {
	    set auth [::base64::encode "$URL(user):$URL(pwd)"]
	    lappend hdrs Authorization "Basic $auth"
	}

	# Add any header that we registered with the callback.
	if { $TRIGGER(-header) ne {} } {
	    foreach hdr $TRIGGER(-header) {
		foreach {k v} [split $hdr ":"] break
		lappend hdrs $k $v
	    }
	}

	# Prepare the HTTP operation.
	set cmd [list ::http::geturl $url -headers $hdrs]
	if { $CM(timeout) > 0 } {
	    # Append timeout if any.
	    lappend cmd -timeout $CM(timeout)
	}
	if { $TRIGGER(-type) ne "" } {
	    lappend cmd -type $TRIGGER(-type)
	}
	if { $TRIGGER(-method) ne "" } {
	    lappend cmd -method $TRIGGER(-method)
	}
	if { $body ne "" } {
	    lappend cmd -query $body
	}
	lappend cmd -command [list ::trigger:__report $t $url]

	$CM(log)::debug "Requesting with $cmd"
	if { [catch {eval $cmd} token] } {
	    $CM(log)::error "Error while posting trigger callback: $token"
	    return 0
	}
	return 1
    }

    return 0
}


# ::trigger:__check -- Check momentaneous value of trigger expression
#
#	Each trigger can be associated to an expression, i.e. an extra
#	check that will permit or inhibits its triggering at a given
#	moment.  This procedure replaces all instances of the fields
#	within the object associated to the trigger and enclosed by %
#	signs by their momentaneous value and test the resulting
#	mathemtical expression to detect if the trigger should fire
#	(return 1) or not.
#
# Arguments:
#	t	Identifier of a trigger
#	arg2	descr
#
# Results:
#	Positive boolean if the trigger should be fired given the
#	current value of the fields in the object and the control
#	expression, negative otherwise.  Empty expressions always
#	allow the trigger, which is the default.
#
# Side Effects:
#	None.
proc ::trigger:__check { t } {
    global CM

    if { ! [::uobj::isa $t trigger] } {
	return -code error "$t unknown or wrong type"
    }
    upvar \#0 $t TRIGGER

    # Empty expression, allow the trigger to fire.
    if { $TRIGGER(-expression) eq "" } {
	$CM(log)::debug "Trigger $TRIGGER(uuid) should fire"
	return 1
    }

    # Replace any occurrence of the name of a field of the object,
    # enclosed by % signs with the current expression.
    upvar \#0 $TRIGGER(-object) OBJ
    set map {}
    foreach k [array names OBJ -*] {
	set k [string trimleft $k "-"]
	lappend map %$k% $OBJ(-$k)
    }
    set expression [string map -nocase $map $TRIGGER(-expression)]
    
    # Execute the mathematical expression with care and allow firing
    # if it evaluates to true.
    if { [catch {expr $expression} result] } {
	$CM(log)::error "Could not execute trigger expression\
                         '$TRIGGER(-expression)': $result"
    } else {
	if { $result } {
	    $CM(log)::debug "Trigger $TRIGGER(uuid) should fire"
	} else {
	    $CM(log)::debug "Trigger $TRIGGER(uuid) refrained firing by expr"
	}
	return $result
    }
    return 0;  # Will catch errors on the expression
}

proc ::trigger:find { by key } {
    global CM

    switch -glob -- $by {
	ur* -
	r* {
	    # url or receiver are synonyms
	    return [::uobj::find [namespace current] trigger \
			[list -receiver == $key]]
	}
	o* {
	    return [::uobj::find [namespace current] trigger \
			[list -object == $key]]
	}
	i* -
	uu* {
	    # id and uuid are synonyms
	    return [::uobj::find [namespace current] trigger \
			[list uuid == $key]]
	}
    }
    return ""
}


# ::trigger:callback -- Callback for watched objects
#
#	This is the implementation of the write trace callback for
#	(parts of) objects that are being watched using the listen
#	ReST mechanism.
#
# Arguments:
#	t	Identifier of the trigger object
#	name1	Name of the variable (array of object)
#	name2	Index in object (array) being written
#	op	Operation being performed (write by construction)
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::trigger:callback { t name1 name2 op } {
    global CM

    if { ! [::uobj::isa $t trigger] } {
	return ""; # Gracefully return
    }

    # Access the trigger and remember which change (i.e. which field
    # from the object) that we have just written to.
    upvar \#0 $t TRIGGER
    lappend TRIGGER(changes) $name2

    # Arrange for <consider> to be true if we should consider the
    # change of value, also make sure that we copy the current value
    # of the field from the object into the trigger copy.
    if { $TRIGGER(-always) } {
	set consider 1
    } else {
	upvar \#0 $TRIGGER(copy) COPY
	upvar \#0 $TRIGGER(-object) OBJECT
	if { [catch {expr {$COPY($name2) != $OBJECT($name2)}} consider] == 0 } {
	    if { $consider } {
		$CM(log)::debug "Object $OBJECT(uuid) field $name2 has changed"
	    } else {
		$CM(log)::debug "Object $OBJECT(uuid) field $name2 has not\
                                 changed, ignoring"
	    }
	} else {
	    $CM(log)::error "Could not check equality of old copy against new\
                             value: $consider"
	    set consider 1
	}
	set COPY($name2) $OBJECT($name2)
    }

    # React in jitter ms from now if we haven't already scheduled such
    # a reaction.  We are also doing the check for the mathematical
    # expression here to make sure we are able to catch ALL changes
    # that would evaluate to allowing the trigger to fire.  If we only
    # do this in "xxx" ms (the jitter), we might miss some of these
    # events!
    if { $TRIGGER(scheduled) eq "" && $consider && [::trigger:__check $t] } {
	set TRIGGER(scheduled) [after $TRIGGER(-jitter) ::trigger:deliver $t]
    }
}



# ::trigger:force -- Force trigger check and firing.
#
#	Check if a trigger should be fired according to the expression
#	that it is associated to.  If it is, attempt to fire it.
#
# Arguments:
#	t	Identifier of the trigger.
#
# Results:
#	1 if success in attempting to fire, 0 in all other cases.
#
# Side Effects:
#	None.
proc ::trigger:force { t } {
    global CM

    # Fire at once if we should according to the expression, do not
    # care if the copy is equal or not to the current object.
    if { [::trigger:__check $t] } {
	return [::trigger:deliver $t]
    }
    return 0
}


# ::trigger:destroy -- Destroy a trigger
#
#	This procedure destroys a trigger, including all the variable
#	traces that were associated to the trigger.
#
# Arguments:
#	t	Identifier of the trigger.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::trigger:destroy { t } {
    global CM

    if { ! [::uobj::isa $t trigger] } {
	return -code error "$t unknown or wrong type"
    }
    
    upvar \#0 $t TRIGGER
    if { $TRIGGER(scheduled) ne "" } {
	after cancel $TRIGGER(scheduled)
    }
    
    set o $TRIGGER(-object)
    upvar \#0 $o OBJ
    foreach f [array names OBJ -$TRIGGER(-field)] {
	$CM(log)::debug "Removing trace on writes for ${o}($f)"
	trace remove variable ${o}($f) write [list ::trigger:callback $t]
    }

    $CM(log)::info "Removed trigger $TRIGGER(uuid) for object $o,\
                    pushing back ($TRIGGER(-method)) to $TRIGGER(-receiver)"

    ::uobj::delete $t
}


proc ::trigger:new { o qry } {
    global CM

    upvar \#0 $o OBJ

    set t [::uobj::new [namespace current] trigger]
    upvar \#0 $t TRIGGER
    set TRIGGER(id) $t
    set TRIGGER(scheduled) ""
    set TRIGGER(changes) {}
    set TRIGGER(uuid) [::uuid::uuid generate]
    set c [::uobj::new [namespace current] tcopy [::uobj::id $t]]
    upvar \#0 $c COPY
    set TRIGGER(copy) $c
	    
    set TRIGGER(-object) $o
    set TRIGGER(-receiver) [dict get $qry receiver]

	    
    # Decide which field to listen to, 
    if { [dict keys $qry field] eq {} } {
	set TRIGGER(-field) "*"
    } else {
	set TRIGGER(-field) [string trimleft [dict get $qry field] "-"]
    }
    
    # How to send back value
    set colon [string first ":" $TRIGGER(-receiver)]
    if { $colon >= 0 } {
	set scheme [string tolower [string range $TRIGGER(-receiver) 0 $colon]]
	if { $scheme eq "sock:" || $scheme eq "ws:" } {
	    set TRIGGER(-method) WS
	} elseif { $scheme eq "proc:" || $scheme eq "cmd:" } {
	    set TRIGGER(-method) CMD
	} else {
	    set TRIGGER(-method) POST
	    if { [dict keys $qry method] ne {} } {
		set meth [string toupper [dict get $qry method]]
		if { [lsearch {GET POST PUT DELETE} $meth] >= 0 } {
		    set TRIGGER(-method) $meth
		}
	    }
	}
    } else {
	return -code error "Malformed URL for trigger: $TRIGGER(-receiver)"
    }
    
    $CM(log)::info "Creating trigger $TRIGGER(uuid) for object $o,\
                    pushing back ($TRIGGER(-method)) to $TRIGGER(-receiver)"

    # Jitter
    set TRIGGER(-jitter) 100
    if { [dict keys $qry jitter] ne {} } {
	if { [string is integer [dict get $qry jitter]] } {
	    set TRIGGER(-jitter) [dict get $qry jitter]
	}
    }

    # Should we react on value changes only (the default) or everytime
    set TRIGGER(-always) 0
    if { [dict keys $qry always] ne {} } {
	set TRIGGER(-always) [string is true [dict get $qry always]]
    }

    # Extra header, content type and body. Also the expression that
    # the trigger should match to occur; this is dangerous so maybe we
    # should add some syntactic checking on this one!
    foreach k [list header type expression] {
	set TRIGGER(-$k) ""
	if { [dict keys $qry $k] ne {} } {
	    set TRIGGER(-$k) [dict get $qry $k]
	}
    }

    # Body should be URL decoded once more, the following is UGLY
    # since it uses a hidden procedure in the web server.  But it does
    # the work... :(
    set TRIGGER(-body) ""
    if { [dict exists $qry body] } {
	set TRIGGER(-body) [::minihttpd::__URLtoString [dict get $qry body]]
    }
	    
    # Setup traces and make initial copy
    foreach f [array names OBJ -$TRIGGER(-field)] {
	set COPY($f) $OBJ($f)
	$CM(log)::debug "Listening for writes on ${o}($f)"
	trace add variable ${o}($f) write [list ::trigger:callback $t]
    }
    
    return $t
}
