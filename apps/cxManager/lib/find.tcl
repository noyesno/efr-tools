# ::find:uuid -- Find a UUID in context.
#
#	UUIDs are used to uniquely identify classes, constraints and
#	objects in the context.  This procedure looks for the item
#	that is identified by a given UUID, while being able to
#	restrict itselfs on a given type only.  The result string is
#	heavily dependant on the type being requested.
#
# Arguments:
#	uuid	Unique identifier 
#	type	Can be class (look only for class), object or all (anything)
#
# Results:
#	If all is specified as a type, returns a list of 2 or 3
#	elements where the first is either class or object, the second
#	element is the identifier of the class or object and the
#	third, when relevant is the identifier of the class that the
#	object is an instance of.  When called with any other type,
#	the type specification is removed from the list above.  Return
#	an empty list if not found.
#
# Side Effects:
#	None.
proc ::find:uuid { uuid { type "all" } } {
    global CM

    set schema [$CM(cx) get schema]

    # Look among the known classes if we have one that match the UUID
    # implicitely passed as a parameter.
    foreach c [$schema get classes] {
	if { $type eq "all" || $type eq "class" } {
	    if { [$c get uuid] eq $uuid } {
		if { $type eq "all" } {
		    return [list class $c]
		} else {
		    return $c
		}
	    }
	}

	if { $type eq "all" || $type eq "object" } {
	    # Look among the known objects if we have one that match
	    # the UUID implicitely passed as a parameter.  We go via
	    # the list of classes that are known to the schema, which
	    # would allow us to support several schemas later, but
	    # mostly to guarantee that we only access that objects
	    # that were instanciated via the schema (and thus the
	    # model).
	    foreach o [::uobj::allof [namespace current] [$c get -name]] {
		upvar \#0 $o OBJ
		if { $OBJ(uuid) eq $uuid } {
		    if { $type eq "all" } {
			return [list object $o $c]
		    } else {
			return [list $o $c]
		    }
		}
	    }
	}
	
    }
    
    return [list ""]
}

# ::find:topmosts -- Find topmost objects that inherit from a given class
#
#	Find the topmosts objects that inherit from a given class.
#	The procedure supposes that an object hierarchy is formed
#	using a multi-field called a relation.  It returns the objects
#	of the context that are at the top of this hierarchy.
#
# Arguments:
#	cls	Root class that the hierarchy is based on
#	rel	Relation (i.e. multi-field) used to express the hierarchy
#
# Results:
#	A(n empty) list of the topmost object identifiers.
#
# Side Effects:
#	None.
proc ::find:topmosts { cls rel } {
    global CM

    set tree [::tree:construct $cls $rel]
    set topmosts {}
    foreach n [$tree children root] {
	lappend topmosts [$tree get $n object]
    }

    return $topmosts
}


# ::find:class -- Find after specific classes
#
#       Find among the known classes (or a subset of these) after
#       classes matching the "class" argument.  If class is a UUID,
#       only classes that inherits from the class with that UUID will
#       be returned.  If class is a string filter, class is understood
#       as a pattern for the class name and all classes with name
#       matching the filter will be taken into account instead.
#
# Arguments:
#       class   Class UUID or class name filter.
#       subset  Subset of classes to restrict search from, empty for all
#
# Results:
#       Return the list of matching class identifiers
#
# Side Effects:
#       None.
proc ::find:class { { class "*" } { subset "" } } {
    global CM

    set hex "\[a-fA-F0-9\]"
    set uuid "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]"

    if { $subset eq "" } {
	set subset [[$CM(cx) get schema] get classes]
    }

    set all {}
    foreach c $subset {
	if { ( [string match $uuid $class] && [$c get uuid] eq $class ) \
		 || [string match $class [$c get -name]] } {
	    set all [concat $all [$c inherited] $c]
	}
    }

    return $all
}



# ::find:objects -- Find after specific objects
#
#	Find among the known objects (or a subset of these) after
#	objects that have a field and the value for that field that
#	match the pattern passed as arguments.  The name of the field
#	and the filter for its value are expressed as patterns as in
#	"string match".
#
# Arguments:
#	field	Pattern that should match the name of the field
#	filter	Pattern that should match the value of the field
#	classes	List of classes among which to find, empty means all classes
#
# Results:
#	Return the list of object identifiers.
#
# Side Effects:
#	None.
proc ::find:objects { {field "*"} {filter "*"} {classes ""} } {
    global CM

    if { $classes eq "" } {
	set classes [[$CM(cx) get schema] get classes]
    }

    set objects {}
    foreach c $classes {
	foreach o [::uobj::allof [namespace current] [$c get -name]] {
	    upvar \#0 $o OBJ
	    foreach k [array names OBJ -$field] {
		if { [string match $filter $OBJ($k)] } {
		    lappend objects $o
		    break
		}
	    }
	}
    }

    return $objects
}


# ::find:descendants -- Return all descendants of an object
#
#	Return return all descendants of an object by recursing along
#	a given relation (a multi-field, i.e. a list of identifiers to
#	objects).
#
# Arguments:
#	o	Object to start recursion at
#	rel	Relation for traversal.
#
# Results:
#	Return all descendants.
#
# Side Effects:
#	None.
proc ::find:descendants { o rel } {
    global CM

    set children {}
    set rel [string trimleft $rel "-"]
    upvar \#0 $o OBJ
    if { [array names OBJ -$rel] ne "" } {
	foreach o $OBJ(-$rel) {
	    set children [concat $children $o \
			      [::find:descendants $o $rel]]
	}
    }

    return $children
}
