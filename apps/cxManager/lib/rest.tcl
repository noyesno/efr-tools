##################
## Module Name     --  rest.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    This is the implementation of the core of the context manager,
##    i.e. the various ReST entry points that it represents.
##
## Commands Exported:
##      ::rest:* (all other commands are deprecated or internal
##################

proc ::rest:when { when } {
    global CM

    if { ! [string is integer $when] } {
	if { [catch {::schema::from_rfc3339 $when} dt] == 0 } {
	    set when $dt
	} else {
	    if { [catch {clock scan $when} dt] == 0 } {
		set when $dt
	    } else {
		$CM(log)::warn "Could not understand timestamp '$when'.\
                                Tried RFC3339, raw timestamp and even\
                                date formatting"
		set when ""
	    }
	}
    }

    return $when
}


# ::rest:uuid -- ReST UUID content access.
#
#	Find an object, a class or a trigger via its UUID in the
#	context and returns a JSON representation for it.  The UUID is
#	either taken from the URL or from the "uuid" field from the
#	query.
#
# Arguments:
#	prt	Port number on which the request is received
#	sock	Socket to client
#	url	URL being requested at server
#	qry	List of pairs containing the keys and values of the query
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::rest:uuid { prt sock url qry } {
    global CM
 
    set result ""
    set uuid ""
    if { [dict keys $qry uuid] ne {} } {
	set uuid [dict get $qry uuid]
    } else {
	set uuid [lindex [split [string trimright $url "/"] "/"] end]
    }

    set when ""
    if { [dict exists $qry when] } {
	set when [::rest:when [dict get $qry when]]
    }

    if { $uuid ne "" } {
	set find [::find:uuid $uuid]
	if { [llength $find] > 0 } {
	    foreach {type id cls} $find break
	    if { $type eq "class" } {
		append result [::json:class $id]
	    } else {
		append result [::json:object $id $cls $when]
	    }
	} else {
	    set id [::uobj::find [namespace current] trigger \
			[list uuid == $uuid]]
	    if { $id ne "" } {
		append result [::json:trigger $id]
	    }
	}
    }

    return $result
}


proc ::object:update { o f val } {
    global CM

    if { $o ne "" } {
	upvar \#0 $o OBJ
    }
    if { [$f get builtin] || [$f get constraint] ne "" } {
	if { [$f get -multi] } {
	    set OBJ(-[$f get -name]) {}
	    foreach v [::json:to_dict $val] {
		lappend OBJ(-[$f get -name]) $v
	    }
	} else {
	    set OBJ(-[$f get -name]) $val
	}
    } else {
	if { [$f get -multi] } {
	    set OBJ(-[$f get -name]) {}
	    foreach v [::json:to_dict $val] {
		foreach {pv c} [::find:uuid $v object] break
		if { $pv ne "" } {
		    lappend OBJ(-[$f get -name]) $pv
		}
	    }
	} else {
	    foreach {v c} [::find:uuid $val object] break
	    if { $v ne "" } {
		set OBJ(-[$f get -name]) $val
	    }
	}
    }

    return [array get OBJ]
}


proc ::object:append { o f val } {
    global CM

    upvar \#0 $o OBJ
    if { [$f get -multi] } {
	if { [$f get builtin] || [$f get constraint] ne "" } {
	    foreach v [::json:to_dict $val] {
		lappend OBJ(-[$f get -name]) $v
	    }
	} else {
	    foreach v [::json:to_dict $val] {
		foreach {pv c} [::find:uuid $v object] break
		if { $pv ne "" } {
		    lappend OBJ(-[$f get -name]) $pv
		}
	    }
	}
    }
}


proc ::object:push { o c qry } {
    global CM

    set updates {};    # List of key values with updated fields.

    upvar \#0 $o OBJ
    set origins [$c inheritance on]
    foreach s $origins {
	foreach f [$s get fields] {
	    set fname [$f get -name]
	    if { [dict keys $qry $fname] ne "" } {
		set val [dict get $qry $fname]
		if { [catch {::object:update $o $f $val} err] == 0 } {
		    lappend updates $fname $val
		} else {
		    $CM(log)::warn "Could not update field $f in $o to\
                                    '$val': $err"
		}
	    }
	}
    }

    return $updates
}


proc ::object:set { o c qry } {
    global CM

    set result ""
    set when ""
    if { [dict exists $qry __when] } {
	set when [::rest:when [dict get $qry __when]]
	$CM(log)::debug "Setting values for object in future or past:\
                         [::schema::to_rfc3339 $when s]"
    }

    # Setting values in the future (or the past). We create a
    # temporary instance of the object, and insert the values from the
    # request into the temporary object (instead of the current
    # copy). We hint the db package with the time of the insert
    # (i.e. the timestamp contained in __when) so that it will be able
    # to store in the database at the proper time.
    if { $when ne "" } {
	upvar \#0 $o SRC
	set o [$c new]
	upvar \#0 $o OBJ
	# Copy the UUID and reference since this is the same object,
	# later/earlier in time.
	set OBJ(uuid) $SRC(uuid)
	set OBJ(name) $SRC(name)
	# Hint at what time the set operations are occuring,
	# this will be used by the db library.
	::uobj::keyword $o when $when
    }

    upvar \#0 $o OBJ
    foreach { fname val } [::object:push $o $c $qry] {
	if { $when eq "" } {
	    $CM(log)::debug "Updated field '$fname' in $o to $val"
	} else {
	    $CM(log)::debug "Field '$fname' in ${o}<-$SRC(id)\
                             scheduled to be $val at\
                             [::schema::to_rfc3339 $when s]"
	}
    }

    # Remove the keys that were not set by the operation when
    # we specify a value in future or past, this will allow
    # the db module to work smoothly, but is also conceptually
    # proper since we have just described an update (an
    # nothing else).
    if { $when ne "" } {
	foreach k [array names OBJ -*] {
	    if { ![dict exists $qry [string trimleft $k "-"]] } {
		unset ${o}($k)
	    }
	}
    }

    # Return a JSON expression describing either the full
    # (current) state of the object, or the value of the
    # fields that were set for future or past operations.
    append result [::json:object $o $c]

    # Remove copy of object source when we had specified a
    # timestamp, since we don't need it anymore (it will
    # hopefully be replayed into our dataspace later). We wait
    # some time before the removal since the db module flushes
    # to the database only after a while. This is a bit ugly
    # though.
    if { $when ne "" } {
	after $CM(pertain) ::uobj::delete $o
    }

    return $result
}


# ::rest:set -- Set value of field(s)
#
#	Set the value(s) of a (number of) field(s) in an object, given
#	its UUID.  The UUID is either implicitely set from the URL or
#	via the uuid field in the query.  Except "uuid" all other key
#	and values contained in the query are understood as being
#	field(s) and their value(s) for the object and the object is
#	updated.  Note that the schema constrains the values being
#	set.  A JSON respresentation of the object is returned.  It is
#	possible to specify a date in the futur or the past using the
#	field __when. That date should be in the RFC3339 format and in
#	that case the JSON representation of the update will be
#	returned only (as opposed to a full object representation).
#
# Arguments:
#	prt	Port number on which the request is received
#	sock	Socket to client
#	url	URL being requested at server
#	qry	List of pairs containing the keys and values of the query
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::rest:set { prt sock url qry } {
    global CM

    set result ""
    set uuid ""
    if { [dict keys $qry uuid] ne {} } {
	set uuid [dict get $qry uuid]
    } else {
	set uuid [lindex [split [string trimright $url "/"] "/"] end-1]
    }

    if { $uuid ne "" } {
	foreach {o c} [::find:uuid $uuid object] break
	if { $o ne "" } {
	    set result [::object:set $o $c $qry]
	}
    }

    return $result
}


# ::rest:append -- Append value to multi-field(s)
#
#	Append to the value(s) of a (number of) field(s) in an object,
#	given its UUID.  The UUID is either implicitely set from the
#	URL or via the uuid field in the query.  Except "uuid" all
#	other key and values contained in the query are understood as
#	being multi-field(s) of the object and the value(s) that
#	should be appended to the field(s).  Note that the schema
#	constrains the values being set.  A JSON respresentation of
#	the object is returned.
#
# Arguments:
#	prt	Port number on which the request is received
#	sock	Socket to client
#	url	URL being requested at server
#	qry	List of pairs containing the keys and values of the query
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::rest:append { prt sock url qry } {
    global CM
 
    set result ""
    set uuid ""
    if { [dict keys $qry uuid] ne {} } {
	set uuid [dict get $qry uuid]
    } else {
	set uuid [lindex [split [string trimright $url "/"] "/"] end-1]
    }

    if { $uuid ne "" } {
	foreach {o c} [::find:uuid $uuid object] break
	if { $o ne "" } {
	    upvar \#0 $o OBJ
	    set origins [$c inheritance on]
	    foreach s $origins {
		foreach f [$s get fields] {
		    set fname [$f get -name]
		    if { [dict keys $qry $fname] ne "" } {
			if { [catch {::object:append $o $f \
					 [dict get $qry $fname]} err] == 0 } {
			    $CM(log)::debug "Appended to field '$fname' in $o:\
                                             [dict get $qry $fname]"
			} else {
			    $CM(log)::warn "Could not append to field '$fname'\
                                            in $o: $err"
			} 
		    }
		}
	    }
	    append result [::json:object $o $c]
	}
    }

    return $result
}


proc ::rest:destroy { prt sock url qry } {
    global CM
 
    array set RESULTS {
	true     "\{\"result\":true\}"
	false     "\{\"result\":false\}"
    }

    set uuid ""
    if { [dict keys $qry uuid] ne {} } {
	set uuid [dict get $qry uuid]
    } else {
	set uuid [lindex [split [string trimright $url "/"] "/"] end-1]
    }

    if { $uuid ne "" } {
	foreach {o c} [::find:uuid $uuid object] break
	if { $o ne "" } {
	    ::tree:clean $o
	    ::uobj::delete $o
	    return $RESULTS(true)
	} else {
	    set id [::uobj::find [namespace current] trigger \
			[list uuid == $uuid]]
	    if { $id ne "" } {
		::trigger:destroy $id
		return $RESULTS(true)
	    }
	}
    }
    return $RESULTS(false)
}




# ::rest:listen -- ReST interface to trigger on object modifications
#
#	Implement the core of a trigger mechanism that automatically
#	performs HTTP-based callbacks whenever (parts of) objects from
#	the context are modified, either by a program, but mostly
#	using the ReST interface to set and append values to objects.
#	In essence, this implements an HTTP-based event system.  The
#	arguments to the query that are recognised are the following:
#
#	"receiver" is the URL that will received the callback and be
#	notified whenever the object changes.  In the URL, any string
#	that is surrounded by % signs will be replaced by the current
#	value of the field with the same name (sans % signs) in the
#	object.
#
#	"field" is a pattern matching one or more of the fields of the
#	object.  This can be used to only react on parts of the
#	object.  Not specifying the field will lead to callback for
#	any modification to the fields in the objects.
#
#	"header" is a list of key and values, separated by colon
#	signs. These will be automatically added to the HTTP request
#	at the time of the callback, permitting to transmit API keys
#	or similar.  It defaults to an empty string, i.e. no special
#	headers.
#
#	"method" can be one of GET, POST, PUT or DELETE and specifies
#	which type of HTTP method will be used at the time of the
#	callback.  It defaults to GET and will automatically become a
#	POST if query arguments are specified.
#
#	"type" can be used to specify the content type of the data
#	that will be sent as part of the request.  It will default to
#	some sensible values, but can be used to drive the
#	"Content-Type" part of the HTTP header better whenever
#	necessary.
#
#	"body" can be used to put or post data via the HTTP request
#	made to the server.  In the body, which is a textual string,
#	strings surrounded by % signs will be substituted as in the
#	receiving URL.
#
#	"jitter" is expressed in milliseconds and guarantees that
#	callbacks are not made more often than the jitter period.  It
#	defaults to 100, so as to allow to easily set several fields
#	in an object simultaneously without triggering several
#	callbacks.
#
#       So, if you have a pachube feed #53818, the following
#       registration would automatically update the "radiatorOut"
#       datastream of your feed whenever you change the field called
#       radiatorOut in the object.
#       https://localhost:8800/context/b88b194c-4d5b-15b4-1824-3ef87a3b0b24/listen?header=X-PachubeApiKey%3AGqpl91eGQHGiUW7wj9FreurApl6SAKxTWG5oRlgvM2dPVT0g&method=PUT&receiver=http%3A%2F%2Fapi.pachube.com%2Fv2%2Ffeeds%2F53818%2Fdatastreams%2FradiatorOut.csv&body=%25radiatorOut%25
#
# Arguments:
#	prt	Port number on which the request is received
#	sock	Socket to client
#	url	URL being requested at server
#	qry	List of pairs containing the keys and values of the query
#
# Results:
#	Returns a JSON string describing the trigger object that has
#	been created.  This string is for information only and there
#	are no ways to modify a trigger at this point.
#
# Side Effects:
#	None.
proc ::rest:listen { prt sock url qry } {
    global CM

    set result ""
    set uuid ""
    if { [dict keys $qry uuid] ne {} } {
	set uuid [dict get $qry uuid]
    } else {
	set uuid [lindex [split [string trimright $url "/"] "/"] end-1]
    }

    if { $uuid ne "" } {
	$CM(log)::notice "Setting up a triggering callback for $uuid"

	# XXXX: Can we handle classes also??

	# receiver URL, a must have
	if { [dict keys $qry receiver] eq {} } {
	    return $result
	}

	foreach {o c} [::find:uuid $uuid object] break
	if { $o ne "" } {
	    set t [::trigger:new $o $qry]
	    set result [::json:trigger $t]
	}
    }

    return $result
}


proc ::rest:stream { sock type msg } {
    global CM

    switch -glob -- $type {
	"con*" -
	"re*" {
	    foreach {url qry} $msg break
	    set result ""
	    set uuid ""
	    if { [dict keys $qry uuid] ne {} } {
		set uuid [dict get $qry uuid]
	    } else {
		set uuid [lindex [split [string trimright $url "/"] "/"] end-1]
	    }
	    
	    if { $uuid ne "" } {
		$CM(log)::notice "Setting up a streaming callback for $uuid"
		
		# Misuse the receiver to be a "socket" URL, since we
		# are not going to close things down.  Remembering the
		# socket will help us finding back the trigger, thus
		# the object, when receiving JSON formated data from
		# the client.
		dict set qry receiver "sock:$sock"
		
		foreach {o c} [::find:uuid $uuid object] break
		if { $o ne "" } {
		    set t [::trigger:new $o $qry]
		    # Force output of object state at websocket
		    # opening to ease interfacing from external
		    # programs.
		    ::trigger:force $t
		} else {
		    $CM(log)::warn "Object with UUID $uuid does not exist,\
                                    closing stream socket"
		    ::websocket::close $sock 1000 "Cannot find object $uuid"
		}
	    }
	}
	"te*" {
	    # Receive JSON-formated data.  We look for the trigger
	    # that is associated to the pseudo URL that have
	    # previously built using the name of the socket and push
	    # data from the JSON into the object.
	    set t [::trigger:find receiver "sock:$sock"]
	    if { $t ne "" } {
		if { [catch {::json:to_dict $msg} dta] } {
		    $CM(log)::error "Error when parsing JSON $msg: $dta"
		} else {
		    upvar \#0 $t TRIGGER
		    upvar \#0 $TRIGGER(-object) OBJ
		    set c [[$CM(cx) get schema] find [::uobj::type $OBJ(id)]]
		    ::object:set $OBJ(id) $c $dta
		}
	    } else {
		$CM(log)::warn "Cannot find trigger associated to websocket\
                                $sock!"
	    }
	}
	"cl*" {
	    # Cleanup trigger when websocket is closed.
	    set t [::trigger:find receiver "sock:$sock"]
	    if { $t ne "" } {
		::trigger:destroy $t
	    } else {
		$CM(log)::warn "Cannot find trigger associated to websocket\
                                $sock!"
	    }
	}
    }
}



proc ::location:fix { o c classes relation val } {
    global CM

    upvar \#0 $o OBJ

    if { $OBJ(-$relation) eq "" } {
	::uobj::keyword $o $relation $val
    }

    set origins [$c inheritance on]
    foreach cls $origins {
	foreach f [$cls get fields] {
	    if { ! [$f get builtin] } {
		if { [lsearch $classes [$f get class]] >= 0 } {
		    if { [$f get -multi] } {
			foreach obj $OBJ(-[$f get -name]) {
			    ::location:fix $obj \
				[$f get class] $classes $relation $val
			}
		    } else {
			if { $OBJ(-[$f get -name]) ne "" } {
			    ::location:fix $OBJ(-[$f get -name]) \
				[$f get class] $classes $relation $val
			}
		    }
		}
	    }
	}
    }
}


# Recursively arrange so that objects that do not have a value for the
# location relationship are inheriting it from those who have one.
proc ::location:update {} {
    global CM

    foreach {contained relation} $CM(locating) {
	set relation [string trimleft $relation "-"]
	set contained_class [[$CM(cx) get schema] find $contained]
	set contained_classes [concat [$contained_class inherited] \
				   $contained_class]
	foreach o [$CM(cx) get objects] {
	    set c [[$CM(cx) get schema] find [::uobj::type $o]]
	    if { [lsearch $contained_classes $c] >= 0 } {
		upvar \#0 $o OBJ
		if { $OBJ(-$relation) ne "" } {
		    ::location:fix $o $c $contained_classes $relation \
			$OBJ(-$relation)
		}
	    }
	}
    }
}



# ::rest:locate -- Locate matching objects in context hierarchies
#
#       This procedure implements that a REST-compatible hierarchy
#       search in a context.  Since the context is variable and since
#       we cannot know how the hierarchy is describe, the
#       implementation only assumes that hierarchies are described
#       using multi-field that contains the instances of the
#       "children".
#
#       The idea is to traverse the hierarchy, starting from a
#       container object, and using of its multi-fields to traverse.
#       During the traversal, what we are looking for are objects
#       contained in (another) multi-field, and of a given class type.
#       Further restrictions can be applied, so that we only select
#       objects that have a given field with a given value (or
#       patterns of such).  For more details, see the query arguments
#       parameters below:
#
#       "traverse" specifies the name of the multi-field that we will be
#       using for traversing through the hierarchy.  This is mandatory
#       for a successful query.
#
#       "within" specifies within which multi-field we should be
#       looking for objects.  Not specifying this, means looking
#       within the same multi-field as for traversal, thus allowing to
#       selectively find children of a given top object.
#
#       "container" should be the UUID of a class or an object.  If
#       pointing at an object, this will be the top object at which we
#       start the traversal. If pointing at a class, the traversal
#       will select all topmost objects of that given class,
#       respecting the "traverse" specification.
#
#       "type" selects the type of the object that we will be looking
#       for.  If not specified, the query will be looking for the same
#       type as the type of the "within" field.
#
#       "field" is a pattern matching the name of a field in the
#       objects that we are looking for, i.e. of the type specified
#       implicitely or explicitely by "type".
#
#       "filter" is a pattern matching the name of the field above,
#       and only objects whose field matches the value of the filter
#       will be returned.
#
# Arguments:
#	prt	Port number on which the request is received
#	sock	Socket to client
#	url	URL being requested at server
#	qry	List of pairs containing the keys and values of the query
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::rest:locate { prt sock url qry } {
    global CM

    #::location:update

    # We must at least know how to traverse the hierarchy!!
    if { [dict keys $qry traverse] eq {} } {
	    return "\[\]"
    }

    # If no "within" is specified, take the same as the relation used
    # for traversal, which will enable to locate for matching objects
    # within a single tree structure.
    set within ""
    if { [dict keys $qry within] eq {} } {
	set within [dict get $qry traverse]
    } else {
	set within [dict get $qry within]
    }
    set within [string trimleft $within "-"]

    # Find where to start looking at in the hierarchy, this should
    # resolved to an object. If we specified a class instead, pick-up
    # the topmost objects that inherits from the class, if any.
    set tops ""
    if { [dict keys $qry container] ne "" } {
	foreach {tops cls} [::find:uuid [dict get $qry container] object] break
	if { $tops eq "" } {
	    set cls [::find:uuid [dict get $qry container] class]
	    set tops [::find:topmosts $cls [dict get $qry traverse]]
	}
    }

    # Filter some of the classes or all of the classes, depending on
    # the value of the "type" field.  If no field type is specified,
    # we base this on the type of the field of that we use for
    # looking within.
    if { [dict keys $qry type] ne "" } {
	set all [::find:class [dict get $qry type]]
    } else {
	set type ""
	if { $tops ne {} } {
	    set all {}
	    foreach top $tops {
		# Get to the class $top is an instance of
		set cls [::uobj::keyword $top class]
		# Then to the field specification that leads to what
		# we are looking for.
		set f [$cls field $within]
		set type [[$f get class] get -name]
		set all [concat $all [::find:class $type]]
	    }
	} else {
	    set all [[$CM(cx) get schema] get classes]
	}
    }

    # Select in the list objects those of the classes selected above
    # that have a field which name matches the filter and which
    # content matches the field.
    set filter "*"
    set field "*"
    if { [dict keys $qry filter] ne "" } {
	set filter [dict get $qry filter]
    }
    if { [dict keys $qry field] ne "" } {
	set field [dict get $qry field]
    }
    set objects [::find:objects $field $filter $all]
    set objects [lsort -unique $objects]

    # Finally traverse the hierarchy, starting at the container and
    # performing the traversal on the (multi-)field which name is
    # specified by "traverse".  While traversing, look for the objects
    # that matches those filtered previously and that are contained in
    # the (multi-)field which name is specified by "within".
    set filtered {}
    if { $tops ne "" } {
	foreach top $tops {
	    foreach o [concat $top [::find:descendants $top \
					[dict get $qry traverse]]] {
		upvar \#0 $o OBJ
		if { [array names OBJ -$within] ne "" } {
		    foreach f $objects {
			if { [lsearch $OBJ(-$within) $f] >= 0 } {
			    lappend filtered $f
			}
		    }
		}
	    }
	}
    }
    
    return [::json:context [lsort -unique $filtered]]
}



# ::rest:topmost -- Return topmost objects given a class and relation
#
#       This procedure implements that a REST-compatible hierarchy
#       search in a context.  Since the context is variable and since
#       we cannot know how the hierarchy is describe, the
#       implementation only assumes that hierarchies are described
#       using multi-field that contains the instances of the
#       "children".  The procedure returns the part of the context
#       that contains the topmost objects for a given class and given
#       a relation.
#
#       "traverse" specifies the name of the multi-field that we will be
#       using for traversing through the hierarchy.  This is mandatory
#       for a successful query.
#
#       "type" selects the (base) type of the object that we will be
#       looking for.
#
# Arguments:
#	prt	Port number on which the request is received
#	sock	Socket to client
#	url	URL being requested at server
#	qry	List of pairs containing the keys and values of the query
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::rest:topmost { prt sock url qry } {
    global CM

    #::location:update

    # We must at least know how to traverse the hierarchy!!
    if { [dict keys $qry traverse] eq {} || [dict keys $qry type] eq {} } {
	return "\[\]"
    }

    set cls [[$CM(cx) get schema] find [dict get $qry type]]
    if { $cls ne "" } {
	set tops [::find:topmosts $cls [dict get $qry traverse]]
	return [::json:context $tops]
    }

    return "\[\]"
}


proc ::rest:add { prt sock url qry } {
    global CM

    # We must have a type, i.e. a class for the object to be created
    if { [dict keys $qry type] eq {} } {
	return "\[\]"
    }

    # Find the class of the object
    set sha [$CM(cx) get schema]
    set cls [lindex [find:class [dict get $qry type]] 0]
    if { $cls eq "" } {
	return "\[\]"
    }
    if { [dict keys $qry reference] ne {} } {
	set ref [dict get $qry reference]
    } else {
	set ref ""
    }

    # Add object to context and return UUID
    set o [$CM(cx) add $cls name $ref]

    return [::json:context [list $o]]
    
}


# ::rest:find -- Find within context
#
#       Finds objects matching which class is a sub-class of a given
#       class and which carry a field which name matches a given
#       pattern and which value matches a gen pattern.
#
#	"field" is a pattern matching one or more of the fields of the
#	object.  Not specifying the field will lead to selecting all
#	objects matching the class specification.
#
#	"filter" is a pattern matching the value of the fields
#	selected by the value of the "field" in the query.  Not
#	specifying it will match objects that contain the fields only.
#
#	"type" specifies the class and inherited sub-classes within
#	which to look for.  Not specifying the type selects all
#	classes, thus indirectly all objects from the context.
#
#	Without any arguments to the query, this procedure returns a
#	specification of the whole context.
#
# Arguments:
#	prt	Port number on which the request is received
#	sock	Socket to client
#	url	URL being requested at server
#	qry	List of pairs containing the keys and values of the query
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::rest:find { prt sock url qry } {
    global CM

    #::location:update

    # Filter some of the classes or all of the classes, depending on
    # the value of the "type" field.
    if { [dict keys $qry type] ne "" } {
	set all [::find:class [dict get $qry type]]
    } else {
	set all [[$CM(cx) get schema] get classes]
    }

    # Select in the list objects those of the classes selected above
    # that have a field which name matches the filter and which
    # content matches the field.
    set filter "*"
    set field "*"
    if { [dict keys $qry filter] ne "" } {
	set filter [dict get $qry filter]
    }
    if { [dict keys $qry field] ne "" } {
	set field [dict get $qry field]
    }
    set objects [::find:objects $field $filter $all]

    return [::json:context $objects]
}


# ::rest:context -- Return all objects in context
#
#       Without any parameter in the query, return a JSON expression
#       describing all objects known to the context.  When called with
#       a field called uuid, behaves as the get rest:uuid procedure,
#       i.e. return information about the object that is identified by
#       the UUID, i.e. a class, an object or a trigger.
#
# Arguments:
#	prt	Port number on which the request is received
#	sock	Socket to client
#	url	URL being requested at server
#	qry	List of pairs containing the keys and values of the query
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::rest:context { prt sock url qry } {
    global CM

    set uuid ""
    if { [dict keys $qry uuid] ne {} } {
	set uuid [dict get $qry uuid]
    }

    # If UUID provided, then what we meant is getting the content of a
    # specific class or object, forward this to this very
    # implementation.  Otherwise, send back a complete description of
    # the current context.
    if { $uuid ne "" } {
	return [rest:uuid $prt $sock $url $qry]
    } else {
	return [::json:context]
    }
}


# ::rest:triggers -- Return all existing triggers in context
#
#       Without any parameter in the query, return a JSON expression
#       describing all triggers known to the context.  When called with
#       a field called uuid, behaves as the get rest:uuid procedure,
#       i.e. return information about the object that is identified by
#       the UUID, i.e. a class, an object or a trigger.
#
# Arguments:
#	prt	Port number on which the request is received
#	sock	Socket to client
#	url	URL being requested at server
#	qry	List of pairs containing the keys and values of the query
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::rest:triggers { prt sock url qry } {
    global CM

    set uuid ""
    if { [dict keys $qry uuid] ne {} } {
	set uuid [dict get $qry uuid]
    }

    # If UUID provided, then what we meant is getting the content of a
    # specific class or object, forward this to this very
    # implementation.  Otherwise, send back a complete description of
    # the current context.
    if { $uuid ne "" } {
	return [rest:uuid $prt $sock $url $qry]
    } else {
	set result "\["
	foreach t [::uobj::allof [namespace current] trigger] {
	    append result [::json:trigger $t]
	    append result ","
	}
	set result [string trimright $result ","]
	append result "\]"
	return $result
    }
}


# ::rest:test -- Fire a trigger on request, if possible
#
#       Test the value of the trigger and fire it if relevant, i.e. if
#       the conditions that are associated to the trigger are met.
#       This allows to fire trigger before a value is changed or
#       within the jitter period that they carry.  The only argument
#       recognised in the query is uuid, which specifies the uuid of
#       the trigger.  This can be omitted when it can be extracted
#       from the URL path.
#
# Arguments:
#	prt	Port number on which the request is received
#	sock	Socket to client
#	url	URL being requested at server
#	qry	List of pairs containing the keys and values of the query
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::rest:test { prt sock url qry } {
    global CM

    array set RESULTS {
	true     "\{\"result\":true\}"
	false     "\{\"result\":false\}"
    }
    set uuid ""
    if { [dict keys $qry uuid] ne {} } {
	set uuid [dict get $qry uuid]
    } else {
	set uuid [lindex [split [string trimright $url "/"] "/"] end-1]
    }

    if { $uuid ne "" } {
	# Look for matching trigger and try firing it.
	foreach t [::uobj::allof [namespace current] trigger] {
	    upvar \#0 $t TRIGGER
	    if { $TRIGGER(uuid) eq $uuid } {
		if { [::trigger:force $t] } {
		    return $RESULTS(true)
		}
	    }
	}
    }
    return $RESULTS(false); # Catch all errors
}


