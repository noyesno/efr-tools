if { [info vars ::__json_pkg] eq "" } {
    set ::__json_pkg ""
}
if { [catch {package require json} ver] == 0 } {
    set ::__json_pkg $ver
} else {
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
    if { $::__json_pkg eq "" } {
    string range [
	string trim [
	    regsub -- {^(\uFEFF)} [
		string map {\t {} \n {} \r {} , { } : { } \[ \{ \] \}} $json
		] {}
	    ]
	] 1 end-1
    } else {
	return [::json::json2dict $json]
    }
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


# ::json:serialize -- Serializes an array to JSON
#
#	This procedure serialises the content of any object to JSON.
#
# Arguments:
#	o	Name of the object
#	indices	List of index patterns that should be serialised to JSON
#
# Results:
#	A valid JSON representation of the object.
#
# Side Effects:
#	None.
proc ::json:serialize { o { indices "-*" } } {
    global CM

    upvar \#0 $o OBJ

    set result "\{"
    foreach ptn $indices {
	foreach k [array names OBJ $ptn] {
	    append result "\"[string trimleft $k -]\":"
	    if { [string is integer $OBJ($k)] || [string is double $OBJ($k)]} {
		append result "$OBJ($k)"
	    } elseif { [string is boolean $OBJ($k)] } {
		if { [string is true $OBJ($k)] } {
		    append result "true"
		} else {
		    append result "false"
		}
	    } else {
		append result "\"$OBJ($k)\""
	    }
	    append result ","
	}
    }
    set result [string trimright $result ","]
    append result "\}"

    return $result
}


# ::json:__dumpfield -- Convert the content of a field for use in JSON
#
#	Convert the single content of a class field for use in a JSON
#	expression.  The conversion takes into account the type of the
#	field for its conversion to the string, enclosing necessary
#	fields by quotes.  This does not provide support for
#	multi-fields, since this is only a support procedure for the
#	complete caller (see below).
#
# Arguments:
#	f	Identifier of the field
#	val	Value to convert to string
#
# Results:
#	A well-formatted string, ready for use in a JSON expression
#
# Side Effects:
#	None.
proc ::json:__dumpfield { f val } {
    global CM

    if { [$f get builtin] || [$f get constraint] ne "" } {
	switch -nocase -- [$f type] {
	    String -
	    UUID -
	    Timestamp {
		return "\"$val\""
	    }
	    Boolean {
		return [expr [string is true $val]?"true":"false"]
	    }
	    default {
		return $val
	    }
	}
    } else {
	if { $val eq "" } {
	    return "\"\""
	} else {
	    upvar \#0 $val OBJ
	    return "\"$OBJ(uuid)\""
	}
    }
}



# ::json:field -- Convert field for use in JSON expression
#
#	Converts the content of a multi- or single- field for use
#	within a JSON expression.  The procedure converts multi-field
#	to arrays and handle properly builtin fields as part of the
#	conversion process.
#
# Arguments:
#	f	Identifier of the field
#	val	Content of field
#
# Results:
#	A well-formatted representation of the field
#
# Side Effects:
#	None.
proc ::json:field { f val } {
    global CM

    set result ""
    if { [$f get -multi] } {
	append result "\["
	foreach v $val {
	    append result "[::json:__dumpfield $f $v],"
	}
	set result [string trimright $result ","]
	append result "\]"
    } else {
	append result [::json:__dumpfield $f $val]
    }
    return $result
}



# ::json:object -- Convert object to JSON
#
#	Converts an object from the context to a JSON formatted
#	string.  The conversion process is aware of multi- and single-
#	fields and converts properly builtin types.  When no class is
#	specified, the procedure looks up the class of the object in
#	the context.
#
# Arguments:
#	o	Identifier of the object
#	c	Identifier of the class the object is an instance of.
#	when	Timestamp at which to get the data from, empty for latest
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::json:object { o { c "" } { when "" } } {
    global CM

    if { $when eq "" || $CM(permanent) eq "" } {
	upvar \#0 $o OBJ
    } else {
	array set OBJ [$CM(permanent) get $o $when]
    }

    # No class specified? Lookup for the object type in the
    # context. Maybe could we use ::uobj::keyword $o class instead
    # here?
    if { $c eq "" } {
	set c [[$CM(cx) get schema] find [::uobj::type $o]]
    }

    # Converts all fields of object, doing this from the class
    # description (and not the content of the object) to guarantee
    # than only the dash led fields that are coming from the context
    # are taken into account during the conversion.
    set result ""
    if { $c ne "" } {
	append result "\{"
	set origins [$c inheritance on]
	foreach s $origins {
	    foreach f [$s get fields] {
		if { [array names OBJ -[$f get -name]] ne "" } {
		    append result "\"[$f get -name]\": "
		    append result [::json:field $f $OBJ(-[$f get -name])]
		    append result ","
		}
	    }
	}
	set result [string trimright $result ","]
	append result "\}"
    }
    return $result
}



# ::json:class -- Convert a class description to JSON
#
#	Convert a class description to a JSON string that completely
#	describes the class, its content, its type and its
#	inheritance.  The serialisation can be used to reconstruct a
#	programmatical representation of the class in another (client)
#	system.  It starts with a header containing the user-edited
#	reference (name) for the class in the schema, its UUID and the
#	UUIDs of the classes that it inherits from.  Finally, comes
#	a table with the fields, each as an extra construct.
#
# Arguments:
#	c	Identifier of the class
#
# Results:
#	A well-formatted JSON representation of the class.
#
# Side Effects:
#	None.
proc ::json:class { c } {
    global CM
 
    set result ""
    set supers [$c inheritance]
    append result "\{"
    append result "\"name\":\"[$c get -name]\","
    append result "\"uuid\":\"[$c get uuid]\","
    if { $supers ne "" } {
	append result "\"supers\":\["
	foreach s $supers {
	    append result "\"[$s get uuid]\","
	}
	set result [string trimright $result ","]
	append result "\],"
    }
    append result "\"fields\":\["
    foreach s [concat $supers $c] {
	foreach f [$s get fields] {
	    append result "\{"
	    append result "\"origin\":\"[$s get uuid]\","
	    append result "\"name\":\"[$f get -name]\","
	    append result "\"type\":\"[$f get -type]\","
	    append result "\"multiple\":[$f get -multi]"
	    append result "\},"
	}
    }
    set result [string trimright $result ","]
    append result "\]"
    append result "\}"

    return $result
}


# ::json:context -- Convert (some) objects from the context to JSON
#
#	Converts a list of objects to a JSON string representation.
#	The serialisation contains a complete description of the
#	object, together with a reference to its class and can
#	therefore be used to recreate a programmatical represenation
#	of the object on a remote client.
#
# Arguments:
#	objs	List of objects to convert, empty for all objects.
#	when	Timestamp at which we want the data from.
#
# Results:
#	A well-formatted JSON representation of the context
#
# Side Effects:
#	None.
proc ::json:context { { objs {} } { when "" } } {
    global CM

    # No objects? Take them all from the context!
    if { $objs eq {} } {
	set objs [$CM(cx) get objects]
    }

    set result ""
    append result "\["
    foreach o $objs {
	upvar \#0 $o OBJ
	set class [[$CM(cx) get schema] find [::uobj::type $o]]
	append result "\{"
	append result "\"reference\":\"$OBJ(name)\","
	append result "\"uuid\":\"$OBJ(uuid)\","
	append result "\"class\":\"[$class get uuid]\","
	append result "\"value\":[::json:object $o $class $when]"
	append result "\},"
    }
    set result [string trimright $result ","]; # Remove last comma
    append result "\]"
    return $result
}


# ::json:trigger -- Convert a trigger to JSON
#
#	Converts a trigger to a JSON representation.
#
# Arguments:
#	t	Identifier of the trigger
#
# Results:
#	A well-formatted JSON representation of the trigger
#
# Side Effects:
#	None.
proc ::json:trigger { t } {
    global CM

    upvar \#0 $t TRIGGER
    upvar \#0 $TRIGGER(-object) OBJ
    set result ""
    append result "\{"
    append result "\"uuid\":\"$TRIGGER(uuid)\","
    append result "\"object\":\"$OBJ(uuid)\","
    foreach k [array names TRIGGER -*] {
	set k [string trimleft $k "-"]
	if { $k ne "object" } {
	    append result "\"$k\":\"$TRIGGER(-$k)\","
	}
    }
    set result [string trimright $result ","]
    append result "\}"

    return $result
}
