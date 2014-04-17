##################
## Module Name     --  schema.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##     This library provides a strong typing facility on top of the
##     uobj object and the regular Tcl language.  The idea is to read
##     a schema, which will describe the class inheritance and the
##     types of the variables in the classes.  The schema is meant to
##     be in a human-readable format with very little syntactical
##     sugar.  Once it has been read, the core operation on a schema
##     is the instantiation of a class.  This will result in a uobj,
##     i.e. basically a Tcl array where all the variables of the
##     classes will lead to dash led names in the array.  Every write
##     operation on the class instance (on its fields) will check for
##     validity.  For example, arrays in the schema will be checked
##     against lists of identifiers to objects of the proper instance,
##     etc.
##
##     Apart from classes and a class hierarchy, the library also
##     supports constraints, which should inherit from the built in
##     types, i.e. from String, Boolean, Integer or Float.  Since the
##     library is aimed at the IoT domain, constraints also contain a
##     unit, i.e. a string in the standard format of the BIPM.
##     Constraints and classes are all associated to a UUID which
##     depends on their origin (location on disk or URL) and their
##     name in the hierarchy.
##
##     The library follows the Tk-programming style.  You should
##     create a schema using ::schema::new, which will return an
##     identifier that is also a command.  The combo command/object
##     identifier host all other options.  The library also contains a
##     wide number of operation to explore the class hierarchy, the
##     fields, constraints, etc.  The library supports the following
##     types of objects: schema, class, constraint and field.
##
## Commands Exported:
##      ::schema::new
##################

package require http
#package require tls;   # Will be done dynamically
package require uri
package require base64

package require uobj
package require diskutil
package require event
package require uuid
package require uuidhash

# TODO:
#
# Add something else than "unit" in order to provide information for
# units that are not official by that should be present in the model.
#
# Add some sort of regexp check on the values as part of the
# constraint.  This would help defining things such as a URI/URL and
# use them as part of the model.

namespace eval ::schema {
    variable SCHEMA
    if {![info exists SCHEMA] } {
	array set SCHEMA {
            comments         "\#!;"
	    units            {}
	    prefixes         {}
	    builtin          {String Boolean Integer Float Timestamp UUID}
	    inited           0
	    -timeout         5000
	}
	variable version 0.1
	variable libdir [file dirname [file normalize [info script]]]
	::uobj::install_log schema SCHEMA
	::uobj::install_defaults schema SCHEMA
    }
}


proc ::schema::from_rfc3339 { str { unit s } } {
    # Parse incoming timestamp into seconds (since the period) and
    # microseconds
    set usecs ""
    # Fix Zulu so that it's understood as +00.00, meaning UTC
    if { [string index $str end] eq "Z" } {
	set str "[string range $str 0 end-1]+00:00"
    }
    if { [regexp {(\d+)-(\d+)-(\d+)T(\d{2}):(\d{2}):(\d{2}).(\d+)([+-])(\d{2}):(\d{2})} $str match y m d H M S fs offset tzh tzm] } {
	set secs [clock scan "$y $m $d $H:$M:$S ${offset}$tzh$tzm" \
		      -format "%Y %m %d %T %z"]
	set usecs [expr 1000000*$secs+int(1000000*0.${fs})]
    } elseif { [regexp {(\d+)-(\d+)-(\d+)T(\d{2}):(\d{2}):(\d{2})([+-])(\d{2}):(\d{2})} $str match y m d H M S offset tzh tzm] } {
	set secs [clock scan "$y $m $d $H:$M:$S ${offset}$tzh$tzm" \
		      -format "%Y %m %d %T %z"]
	set usecs [expr $secs*1000000]
    }

    # Return according to the time unit that was requested.
    if { $usecs ne "" } {
	switch $unit {
	    us {
		return $usecs
	    }
	    ms {
		return [expr {$usecs/1000}]
	    }
	    s {
		return [expr {$usecs/1000000}]
	    }
	}
    }
    return -code error "Cannot convert incoming string $str to $unit"
}


proc ::schema::to_rfc3339 { { tstamp "" } { unit us } { variant "" } } {
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
    }
    switch $unit {
	us {
	    set us $tstamp
	}
	ms {
	    set us [expr {$tstamp * 1000}]
	}
	s {
	    set us [expr {$tstamp * 1000000}]
	}
    }

    # Now convert to the RFC3339 format, making sure to support the
    # proper number of decimals in the output, i.e. following the
    # variant specification.
    set secs [expr {$us / 1000000}]
    set micro [expr {$us % 1000000}]
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


# ::schema::get -- Get options and semi-private data for objects
#
#	Return options, but also semi-private data for the various
#	objects types that are supported by the library,
#	i.e. constraint, schema, class and field.  If the argument is
#	dash-led, it is understood as an option and the procedure will
#	act as getting the configuration for that object.  Read below
#	for the remaining data items that can be got.
#
# Arguments:
#	o	Identifier of the object
#	what	The data to get
#
# Results:
#	Return the value for that data, or an empty string.
#
# Side Effects:
#	None.
proc ::schema::get { o what } {
    variable SCHEMA
    variable log

    set type [::uobj::type $o]
    switch $type {
	field {
	    upvar \#0 $o FIELD
	    switch -glob -nocase -- $what {
		class {
		    # Return the identifier of the class of the field,
		    # if the field is typed by a class.
		    if { [::uobj::type $FIELD(definition)] eq "class" } {
			return $FIELD(definition)
		    }
		}
		constraint {
		    # Return the identifer of the constraint of the
		    # field, if the field is typed by a constraint.
		    if { [::uobj::type $FIELD(definition)] eq "constraint" } {
			return $FIELD(definition)
		    }
		}
		definition {
		    # Return either the constraint or the class
		    # identifier that types the field.
		    return $FIELD(definition)
		}
		super {
		    # Return the identifier of the class that carries
		    # the field.
		    return $FIELD($what)
		}
		builtin {
		    # Return a boolean telling if the field is a
		    # built-in type
		    return [expr [lsearch $SCHEMA(builtin) $FIELD(-type)] >= 0]
		}
		-* {
		    return [config $o $what]
		}
		default {
		    return -code error "$what cannot be got from a $type"
		}
	    }
	}
	class {
	    upvar \#0 $o CLASS
	    switch -glob -nocase -- $what {
		uuid -
		schema {
		    # Return the UUID of the class or the identifier
		    # of the schema where the class is originating
		    # from.
		    return $CLASS($what)
		}
		fields {
		    # Return the list of fields that are contained in
		    # that class, this does not return the fields that
		    # come from inherited classes.
		    upvar \#0 $o CLASS
		    return [::uobj::allof [namespace current] field \
				[list [::uobj::id $CLASS(schema)] \
				     [::uobj::id $o]]]
		}
		-* {
		    return [config $o $what]
		}
		default {
		    return -code error "$what cannot be got from a $type"
		}
	    }
	}
	constraint {
	    upvar \#0 $o CONSTRAINT
	    switch -glob -nocase -- $what {
		uuid -
		schema {
		    # Return the UUID of the constraint or the
		    # identifier of the schema where the constraint is
		    # originating from.
		    return $CONSTRAINT($what)
		}
		intervals -
		unit {
		    # Return the intervals of the constraint. Return
		    # the unit (in official BIPM format) of the
		    # constraint.
		    return $CONSTRAINT($what)
		}
		oneOf -
		switch {
		    # Return the list of values the constraint can
		    # only switch between.
		    return $CONSTRAINT(switch)
		}
		-* {
		    return [config $o $what]
		}
		default {
		    return -code error "$what cannot be got from a $type"
		}
	    }
	}
	schema {
	    switch -glob -nocase -- $what {
		classes {
		    # Return the list of class identifiers that are
		    # owned by this schema.
		    return [::uobj::allof [namespace current] class \
				[::uobj::id $o]]
		}
		constraints {
		    # Return the list of constraint identifiers that
		    # are owned by this schema.
		    return [::uobj::allof [namespace current] constraint \
				[::uobj::id $o]]
		}
		-* {
		    return [config $o $what]
		}
	    }
	}
    }
    return ""
}


# ::schema::__check_field -- Check if value is an instance of a class
#
#	Check if the value v passed as a parameter is an instance of
#	the class within the schema.  The test is recursive since
#	fields can originate from inherited classes.
#
# Arguments:
#	s	Identifier of the schema
#	v	Value to test, an object identifier
#	class	Class to test against, will be recursive.
#
# Results:
#	1 if no value or on success, 0 otherwise.
#
# Side Effects:
#	None.
proc ::schema::__check_field { s v class } {
    # Empty values always match!
    if { $v eq "" } {
	return 1
    }

    # Find class of value, provided it has been instantiated via the
    # instance operation.
    set vclass [find $s [::uobj::type $v]]
    if { $vclass eq "" } {
	return 0
    }

    # Check if the type of the object is among the classes the class
    # inherits from.
    set inheritance [inheritance $vclass on]
    if { [lsearch $inheritance $class] >= 0 } {
	return 1
    }
    return 0
}


# ::schema::__check_constraint -- Check if value satisfies constraint
#
#	Check that the value passed as a parameter satisfies the
#	constraint within the schema.  If the constraint contains a
#	number of switches, the value must be one of them.  If the
#	constaint contains a number of intervals, the value must be
#	within one of them.
#
# Arguments:
#	s	Identifier of schema
#	v	Value to test against constraint
#	ctnt	Identifier of the constraint
#
# Results:
#	1 if validation was successful, 0 otherwise.
#
# Side Effects:
#	None.
proc ::schema::__check_constraint { s v ctnt } {
    upvar \#0 $ctnt CONSTRAINT

    # Check if the value is one of the few that are allowed
    if { [llength $CONSTRAINT(switch)] > 0 } {
	foreach sv $CONSTRAINT(switch) {
	    if { $sv != $v } {
		return 0
	    }
	}
    }

    # Check if the value is within the boundaries of all the
    # intervals.
    if { [llength $CONSTRAINT(intervals)] > 0 } {
	set inside 0
	foreach {opening min closing max} $CONSTRAINT(intervals) {
	    # Minimum and maximum are tested unless they are empty.
	    # Arrange for <inside> to be 1 if we are inside one of the
	    # range, making sure we only use bounds that can be used
	    # (empty means no bounds).
	    if { $min ne "" } {
		if { $opening eq "\[" } {
		    if { $v >= $min } {
			set inside 1
		    }
		} else {
		    if { $v > $min } {
			set inside 1
		    }
		}
	    }
	    if { $max ne "" } {
		if { $closing eq "\]" } {
		    if { $v <= $max } {
			set inside 1
		    }
		} else {
		    if { $v < $max } {
			set inside 1
		    }
		}
	    }
	}
	return $inside
    }

    return 1
}


proc ::schema::check { type val } {
    variable SCHEMA
    variable log

    switch -nocase -- $type {
	float -
	double {
	    return [string is double $val]
	}
	boolean -
	integer {
	    return [string is [string tolower $type] $val]
	}
	timestamp {
	    set rx {(\d\d\d\d)(-)?(\d\d)(-)?(\d\d)(T)?(\d\d)(:)?(\d\d)(:)?(\d\d)(\.\d+)?(Z|([+-])(\d\d)(:)?(\d\d))}
	    return [regexp $rx $val]
	}
	string {
	    return 1
	}
	UUID {
	    set hex "\[a-f0-9\]"
	    set uuid_filter "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]"
	    return [string match -nocase $uuid_filter $val]
	}
	"" {
	    return 1;  # Anything matches an empty type.
	}
    }
    return 0
}


# ::schema::__verify -- Object property write validation
#
#	This is the core of the object property validation and is
#	registered as a trace on all the necessary indices of the
#	array that represent the instance of the object.  It will
#	check that object identifiers in fields or multi-fields are of
#	the proper type, that values placed into constrained field
#	match the constraint and that values placed in builtin fields
#	match the type.
#
# Arguments:
#	f	Identifier of the origin field
#	name1	Name of the array that implements the object.
#	name2	Name of the index in the array, match name of field.
#	op	Operation is always "write" by construction.
#
# Results:
#	Return an error when mismatch, to permit proper stop and error
#	handling in calling scripts.
#
# Side Effects:
#	None.
proc ::schema::__verify { f name1 name2 op } {
    variable SCHEMA
    variable log

    if {![::uobj::isa $f field]} {
	return -code error "$f unkown or wrong type"
    }
    upvar \#0 $f FIELD
    
    upvar $name1 v
    set type [type $f]
    if { [lsearch $SCHEMA(builtin) $type] >= 0 } {
	if { $FIELD(-multi) } {
	    foreach i $v($name2) {
		if { ![check $type $i] } {
		    return -code error "$i, one of the values set to\
                                        ${name1}($name2) is not a $type"
		}
	    }
	} else {
	    if { ![check $type $v($name2)] } {
		return -code error "$v($name2), value set to ${name1}($name2)\
                                    is not a $type"
	    }
	}
	if { $FIELD(definition) ne "" } {
	    upvar \#0 $FIELD(definition) CONSTRAINT
	    if { $FIELD(-multi) } {
		foreach i $v($name2) {
		    if { ! [__check_constraint $CONSTRAINT(schema) $i\
				$FIELD(definition)] } {
			return -code error "$i, one of the values set to \
                                            ${name1}($name2) does not respect \
                                            constraint $CONSTRAINT(-name)"
		    }
		}
	    } else {
		if { ! [__check_constraint $CONSTRAINT(schema) $v($name2) \
			    $FIELD(definition)] } {
		    return -code error "$v($name2), value set to \
                                        ${name1}($name2) does not respect \
                                        constraint $CONSTRAINT(-name)"
		}
	    }
	}
    } else {
	upvar \#0 $FIELD(definition) CLASS
	if { $FIELD(-multi) } {
	    foreach i $v($name2) {
		if { ! [__check_field $CLASS(schema) $i $FIELD(definition)] } {
		    return -code error "$i, one of the values set to\
                                        ${name1}($name2) does not inherit from\
                                        $FIELD(-type)"
		}
	    }
	} else {
	    if { ! [__check_field $CLASS(schema) $v($name2) \
			$FIELD(definition)] } {
		return -code error "$v($name2), none of the super classes of\
                                    ${name1}($name2) is\
                                    a $FIELD(-type)"
	    }
	}
    }
}



# ::schema::type -- Return the raw type of field, if possible.
#
#	Returns the raw type of the field when the field is a builtin
#	type or a constraint.  When it is a constraint, return the
#	builtin type that it extends.  The type returned can only be
#	one of Boolean, String, Float or Integer.
#
# Arguments:
#	f	Identifier of the field.
#
# Results:
#	Builtin type of field, if appropriate, otherwise empty string.
#
# Side Effects:
#	None.
proc ::schema::type { f } {
    variable SCHEMA
    variable log

    if {![::uobj::isa $f field]} {
	return -code error "$f unkown or wrong type"
    }
    upvar \#0 $f FIELD

    # Determine builtin type of field, if appropriate
    set type ""
    if { $FIELD(definition) ne "" \
	     && [::uobj::type $FIELD(definition)] eq "constraint" } {
	upvar \#0 $FIELD(definition) CONSTRAINT
	return $CONSTRAINT(-extends)
    }
    if { $FIELD(definition) eq "" } {
	return $FIELD(-type)
    }
    return ""
}



# ::schema::origin -- Origin of a field in class
#
#       Lookup the origin of a field and return an identifier for it.
#       In other words, return an empty string for builtin types, the
#       identifier of the constraint if the field is defined as such,
#       or the topmost class in the class hierarchy that the field
#       originates from (as opposed the the class of the object that
#       the field originates from).
#
# Arguments:
#       f       Identifier of field
#
# Results:
#       See description above.
#
# Side Effects:
#       None.
proc ::schema::origin { f } {
    variable SCHEMA
    variable log

    if {![::uobj::isa $f field]} {
	return -code error "$f unkown or wrong type"
    }
    upvar \#0 $f FIELD

    # Builtin types have an empty definition
    if { $FIELD(definition) eq "" } {
	return ""
    }

    # Constraint? Return it
    if { [::uobj::type $FIELD(definition)] eq "constraint" } {
	return $FIELD(definition)
    }

    # Otherwise, field is from a class, traverse upwards until we find
    # where the field originates from.
    foreach c [inheritance $FIELD(definition) on] {
	if { [lsearch [get $c fields] $f] >= 0 } {
	    return $c
	}
    }

    return ""; # Catch all for errors of various sorts.
}



# ::schema::instance -- Create an instance of a class
#
#	This is the major reason that this whole library exists: the
#	procedure instantiate an object, given a class.  The "object"
#	will be an array, under the very loose control of the uobj
#	library, where all fields of the class are represented as
#	dash-led indices in the array.  The procedure installs a
#	number of traces onto the array, so as to strictly control
#	what is being written into the fields of the array.
#
#	The instance of the class that is created is an array in a
#	given namespace.  By default, the namespace will be the one of
#	the first namespace that is not "schema" at the time of the
#	calling.  This is in most cases what you want, since arrays
#	will usually be created in the namespace of the caller.  If
#	another behaviour was necessary, just specify a namespace
#	after the identifier of the class when calling the procedure.
#
#	The identifier of the object is annotated with the identifier
#	of the class via the "class" keyword and the uobj library.
#	The object automatically gets two extra fields set: "id"
#	contains its identifier, the name of the array itself; "uuid"
#	is a generated UUID.
#
# Arguments:
#	c	Identifier of the class
#	ns	Namespace in which to create the array (optional)
#	args	List of dash-led key and values, for initial values to object,
#		the keys should be the same as the field names from class!
#
# Results:
#	An identifier of the object, which is also the name of the
#	global variable that holds object information.
#
# Side Effects:
#	None.
proc ::schema::instance { c args } {
    variable SCHEMA
    variable log

    if {![::uobj::isa $c class]} {
	return -code error "$c unkown or wrong type"
    }
    upvar \#0 $c CLASS

    # Detect in which namespace the object will be created.  Unless
    # explicitely specified, this will be the namespace of the caller
    # function.
    if { [string index [lindex $args 0] 0] ne "-" } {
	set ns [lindex $args 0]
	set args [lrange $args 1 end]
    } else {
	# Climb stack hierarchy until we found the real namespace of
	# the caller, i.e. a namespace that is not ours.  We have to
	# climb stepwise since the tk-calling convention implies the
	# dynamic creation of commands in this namespace.  In other
	# words, uplevel 1 does not suffices in all cases.
	set this [namespace current]
	for { set lvl 1 } { 1 } { incr lvl } {
	    # Get current namespace at upper level, stop on errors,
	    # i.e. at end of calling stack.
	    if { [catch {uplevel $lvl namespace current} ns] } {
		set ns ""
		break
	    } elseif { $ns ne $this } {
		break
	    }
	}
    }
    set ns [string trim $ns ":"]

    # Access and create object, anotate the object so we can easily
    # recover to the class that it is an instance of.
    set o [::uobj::new $ns $CLASS(-name)]
    upvar \#0 $o OBJ
    set OBJ(id) $o
    set OBJ(uuid) [::uuid::uuid generate]
    ::uobj::keyword $o class $c

    # Arrange so that object will have all dash led options that are
    # coming from the inherited classes
    set origins [inheritance $c on]
    foreach s $origins {
	foreach f [::uobj::allof [namespace current] field \
		       [list [::uobj::id $CLASS(schema)] [::uobj::id $s]]] {
	    upvar \#0 $f FIELD
 
	    set type [type $f]
	    if { [lsearch $SCHEMA(builtin) $type] >= 0 } {
		if { $FIELD(-multi) } {
		    set OBJ(-$FIELD(-name)) {}
		} else {
		    switch -nocase -- $type {
			Boolean {
			    set OBJ(-$FIELD(-name)) false
			}
			Integer {
			    set OBJ(-$FIELD(-name)) 0
			}
			Float -
			Double {
			    set OBJ(-$FIELD(-name)) 0.0
			}
			Timestamp {
			    set OBJ(-$FIELD(-name)) [to_rfc3339]
			}
			UUID {
			    set OBJ(-$FIELD(-name)) [::uuid::uuid generate]
			}
			default {
			    set OBJ(-$FIELD(-name)) ""
			}
		    }
		}
	    } else {
		if { $FIELD(-multi) } {
		    set OBJ(-$FIELD(-name)) {}
		} else {
		    set OBJ(-$FIELD(-name)) ""
		}
	    }
	    trace add variable ${o}(-$FIELD(-name)) write \
		[list [namespace current]::__verify $f]
	    # XXX: Add a trace on the array that test if the field
	    # (starting with dash) is actually allowed as one of the
	    # fields of the object or not.

	    # Set initial value of field in object to what comes from
	    # the arguments, if any.
	    foreach {k v} $args {
		set k [string trimleft $k "-"]
		if { $FIELD(-name) eq $k } {
		    set OBJ(-$FIELD(-name)) $v
		}
	    }

	    ::event::generate $CLASS(schema) New \
		[list %c $c \
		    %i $o \
		    %n $ns]
	}
    }

    return $o
}


# ::schema::inheritance -- List of classes a class inherits from
#
#       Return the list of the classes that a given class inherits
#       from. The topmost class in the inheritance hierarchy is at the
#       end of the list, which means that te class that it directly
#       inherits from is at the beginning of the list.  If myself is
#       true, the identifier of the class itself will be prepended to
#       the list.
#
# Arguments:
#       c       Identifier of class in schema
#       myself  Prepend class identifier or not
#
# Results:
#       Return list of classes, topmost class last.
#
# Side Effects:
#       None.
proc ::schema::inheritance { c { myself off } } {
    variable SCHEMA
    variable log

    if {![::uobj::isa $c class]} {
	return -code error "$c unkown or wrong type"
    }
    upvar \#0 $c CLASS

    # Initialise inheritance list
    if { [string is true $myself] } {
	set inheritance [list $c]
    } else {
	set inheritance {}
    }

    # Climb up until we don't have any super and have reached the top
    # of the inheritance hierarchy.
    while { $CLASS(-super) ne "" } {
	lappend inheritance $CLASS(-super)
	upvar \#0 $CLASS(-super) CLASS
    }
    return $inheritance
}



# ::schema::inherited -- Inherited classes
#
#	Return the list of class identifiers of the classes that are
#	inherited by a given class.
#
# Arguments:
#	c	Identifier of the class
#
# Results:
#	List of classes, empty list if none.
#
# Side Effects:
#	None.
proc ::schema::inherited { c } {
    variable SCHEMA
    variable log

    if {![::uobj::isa $c class]} {
	return -code error "$c unkown or wrong type"
    }
    upvar \#0 $c CLASS
    
    set children {}
    foreach cls [::uobj::allof [namespace current] class \
		     [::uobj::id $CLASS(schema)]] {
	if { [lsearch [inheritance $cls] $c] >= 0 } {
	    lappend children $cls
	}
    }
    return $children
}



# ::schema::field -- Return field matching filter
#
#	Looks in a class after field(s) that match a filter and return
#	theirs identifiers.  The filter is a pattern that follows the
#	rules of the string match command.
#
# Arguments:
#	c	Identifier of the class
#	filter	String filter to match against fields' name
#
# Results:
#	Return the list of identifier for fields, empty if none matched.
#
# Side Effects:
#	None.
proc ::schema::field { c filter } {
    variable SCHEMA
    variable log

    if {![::uobj::isa $c class]} {
	return -code error "$c unkown or wrong type"
    }
    upvar \#0 $c CLASS

    set filter [string trimleft $filter "-"]

    set origins [inheritance $c on]

    foreach s $origins {
	foreach f [::uobj::allof [namespace current] field \
		       [list [::uobj::id $CLASS(schema)] [::uobj::id $s]]] {
	    upvar \#0 $f FIELD
	    if { [string match $filter $FIELD(-name)] } {
		return $f
	    }
	}
    }

    return ""
}


proc ::schema::find { o filter } {
    variable SCHEMA
    variable log

    switch [::uobj::type $o] {
	"schema" {
	    foreach c [::uobj::allof [namespace current] class \
			   [::uobj::id $o]] {
		upvar \#0 $c CLASS
		if { [string match $filter $CLASS(-name)] } {
		    return $c
		}
	    }
	    foreach c [::uobj::allof [namespace current] constraint \
			   [::uobj::id $o]] {
		upvar \#0 $c CONSTRAINT
		if { [string match $filter $CONSTRAINT(-name)] } {
		    return $c
		}
	    }
	    return ""
	}
	"class" {
	    # As compared to the operation <field> (see above), this
	    # only looks in the fields that are solely carried by a
	    # class definition, i.e. not the ones that are inherited
	    # from super classes.
	    upvar \#0 $o 
	    foreach f [::uobj::allof [namespace current] field \
			   [list [::uobj::id [get $o schema]] \
				[::uobj::id $o]]] {
		upvar \#0 $f FIELD
		if { [string match $filter $FIELD(-name)] } {
		    return $f
		}
	    }
	    return ""
	}
    }
    return -code error "$o unknown of wrong type"
}


proc ::schema::__dumpfields { s c { fd stdout } { ident 0 } } {
    variable SCHEMA
    variable log

    if {![::uobj::isa $c class]} {
	return -code error "$c unkown or wrong type"
    }
    upvar \#0 $c CLASS

    foreach f [::uobj::allof [namespace current] field \
		   [list [::uobj::id $s] [::uobj::id $c]]] {
	upvar \#0 $f FIELD
	set out [string repeat \t $ident]
	append out $FIELD(-name)
	if { $FIELD(-optional) } {
	    append out "-"
	}
	append out " " $FIELD(-type)
	if { $FIELD(-multi) } {
	    append out "\[\]"
	}
	puts $fd $out
    }
}


proc ::schema::__dumpclasses { s { fd stdout } { super "" } { ident 0 }} {
    variable SCHEMA
    variable log

    if {![::uobj::isa $s schema]} {
	return -code error "$s unkown or wrong type"
    }
    upvar \#0 $s SMA

    foreach c [::uobj::allof [namespace current] class [::uobj::id $s]] {
	upvar \#0 $c CLASS
	if { $CLASS(-super) eq $super } {
	    set hdr [string repeat "\t" $ident]
	    puts $fd "$hdr$CLASS(-name) \{ \# $CLASS(uuid)"
	    __dumpfields $s $c $fd [expr $ident + 1]
	    __dumpclasses $s $fd $c [expr $ident + 1]
	    puts $fd "$hdr\}"
	}
    }
}


proc ::schema::dump { s { fd stdout } { super "" }} {
    variable SCHEMA
    variable log

    if {![::uobj::isa $s schema]} {
	return -code error "$s unkown or wrong type"
    }
    upvar \#0 $s SMA

    foreach c [::uobj::allof [namespace current] constraint [::uobj::id $s]] {
	upvar \#0 $c CONSTRAINT
	puts $fd \
	    "$CONSTRAINT(-name):$CONSTRAINT(-extends) \{ \# $CONSTRAINT(uuid)"
	if { [llength $CONSTRAINT(switch)] > 0 } {
	    puts $fd "\toneOf \{$CONSTRAINT(switch)\}"
	}
	if { $CONSTRAINT(unit) ne "" } {
	    puts $fd "\tunit \"$CONSTRAINT(unit)\""
	}
	if { [llength $CONSTRAINT(intervals)] > 0 } {
	    set intervals "\tintervals \{"
	    foreach {opening min closing max} $CONSTRAINT(intervals) {
	        append intervals "${opening}${min},${max}${closing} "
	    }
	    set intervals [string trimright $intervals]
	    puts $fd "${intervals}\}"
	}
	puts $fd "\}"
    }

    __dumpclasses $s $fd $super
}


proc ::schema::__known { s { filter "*" } } {
    variable SCHEMA
    variable log

    if {![::uobj::isa $s schema]} {
	return -code error "$s unkown or wrong type"
    }
    upvar \#0 $s SMA

    set all $SCHEMA(builtin)
    foreach c [::uobj::allof [namespace current] class [::uobj::id $s]] {
	upvar \#0 $c CLASS
	lappend all $CLASS(-name)
    }
    foreach c [::uobj::allof [namespace current] constraint [::uobj::id $s]] {
	upvar \#0 $c CONSTRAINT
	lappend all $CONSTRAINT(-name)
    }
    

    set return {}
    foreach c $all {
	if { [string match $filter $c] } {
	    lappend return $c
	}
    }
    return $return
}


proc ::schema::__class { s name { super "" } } {
    variable SCHEMA
    variable log

    if {![::uobj::isa $s schema]} {
	return -code error "$s unkown or wrong type"
    }
    upvar \#0 $s SMA

    set c [find $s $name]
    if { $c eq "" } {
	set c [::uobj::new [namespace current] class [::uobj::id $s]]
	upvar \#0 $c CLASS
	${log}::debug "Creating new class $name, child of $super"
	set CLASS(schema) $s
	set CLASS(uuid) ""
	set CLASS(-name) $name
	set CLASS(-super) $super
	::uobj::objectify $c [list [list config configure] [list instance new] \
				  inheritance field get inherited find]
    } else {
	${log}::debug "Updating class $name, child of $super"
    }
    if { $super eq "" } {
	config $c -name $name
    } else {
	config $c -name $name -super $super
    }
    
    return $c
}

proc ::schema::__constraint { s name primary } {
    variable SCHEMA
    variable log

    if {![::uobj::isa $s schema]} {
	return -code error "$s unkown or wrong type"
    }
    upvar \#0 $s SMA

    set c [find $s $name]
    if { $c ne "" && [::uobj::isa $c class] } {
	${log}::debug "Converting class $name to constraint"
	::uobj::delete $c
	set c ""
    }

    if { $c eq "" } {
	set c [::uobj::new [namespace current] constraint [::uobj::id $s]]
	upvar \#0 $c CONSTRAINT
	${log}::debug "Creating new constraint $name, extending $primary"
	set CONSTRAINT(schema) $s
	set CONSTRAINT(uuid) ""
	set CONSTRAINT(-name) $name
	set CONSTRAINT(-extends) $primary
	set CONSTRAINT(unit) ""
	set CONSTRAINT(switch) {}
	set CONSTRAINT(intervals) {}
	::uobj::objectify $c [list [list config configure] get]
    } else {
	${log}::debug "Updating constraint $name, extending $primary"
    }
    config $c -name $name -extends $primary

    return $c
}


proc ::schema::__analyse_constraint { c def } {
    variable SCHEMA
    variable log

    if {![::uobj::isa $c constraint]} {
	return -code error "$c unkown or wrong type"
    }
    upvar \#0 $c CONSTRAINT

    foreach { k v } $def {
	switch -nocase -glob -- $k {
	    o -
	    s {
		# oneOf or switch
		
		# Force understanding of the switch as a list to
		# trigger possible errors in Tcl, errors that would
		# then occur during parsing.
		set CONSTRAINT(switch) {}
		set switches 0
		foreach sw $v {
		    incr switches
		    if { ![check $CONSTRAINT(-extends) $sw]} {
			${log}::error "In $CONSTRAINT(-name), within switch\
                                       type of value #$switches\
                                       $sw does not match\
                                       $CONSTRAINT(-extends)"
		    } else {
			lappend CONSTRAINT(switch) $sw
		    }
		}
	    }
	    i* {
		# intervals
		set CONSTRAINT(intervals) {}
		set intervals 1
		while { [regexp -indices {[\[\]].*?,.*?[\[\]]} $v range] } {
		    # Extract minimum, maximum and boundary types of range
		    foreach {i j} $range break
		    set interval [string trim \
				      [string range $v $i $j]]
		    set opening [string index $interval 0]
		    set closing [string index $interval end]
		    set interval [string trim \
				      [string range $interval 1 end-1]]
		    foreach { min max } [split $interval ","] break
		    # Check that minimum and maximum match the type of
		    # the builtin type that the constraint extends.
		    set append 1
		    if { $min ne "" && ![check $CONSTRAINT(-extends) $min] } {
			${log}::error "In $CONSTRAINT(-name), at interval\
                                       #$intervals, type of minimum\
                                       $min does not match\
                                       $CONSTRAINT(-extends)"
			set append 0
		    }
		    if { $max ne "" && ![check $CONSTRAINT(-extends) $max] } {
			${log}::error "In $CONSTRAINT(-name), at interval\
                                       #$intervals, type of maximum\
                                       $max does not match\
                                       $CONSTRAINT(-extends)"
			set append 0
		    }
		    # Append range to list of intervals and advance to next
		    if { $append } {
			lappend CONSTRAINT(intervals) \
			    $opening $min $closing $max
		    }
		    set v [string range $v [expr $j + 1] end]
		    if { $v eq "" } {
			break
		    }
		    incr intervals; # Count intervals
		}
	    }
	    u* {
		# unit
		
		# Set the unit, maybe we should make sure this is one
		# of the units allowed by the BIPM
		set v [string trim $v]
		if { $v ne "" && [lsearch -nocase [units] $v] >= 0 } {
		    set CONSTRAINT(unit) $v
		} else {
		    ${log}::error "Unit '$v' is not an official unit at the\
                                   BIPM, ignoring it"
		}
	    }
	}
    }
}


proc ::schema::__analyse { s lst { queue ""} } {
    variable SCHEMA
    variable log

    if {![::uobj::isa $s schema]} {
	return -code error "$s unkown or wrong type"
    }
    upvar \#0 $s SMA

    foreach {k v} $lst {
	if { [string eq -nocase $k INCLUDE] } {
	    __fetch $s $v
	} elseif { [string match "\[A-Z\]*:\[A-Z\]*" $k] } {
	    set super [lindex $queue end]
	    foreach {name primary} [split $k ":"] break
	    if { $super ne "" } {
		${log}::error "When declaring constraint $name, constraints\
                               can only be declared at top levels."
	    } elseif { [lsearch $SCHEMA(builtin) $primary] < 0 } {
		${log}::error "Constraint $name extends $primary, which is not\
                               a builtin type"
	    } else {
		set c [__constraint $s $name $primary]
		__analyse_constraint $c $v
	    }
	} elseif { [string match "\[A-Z\]*" $k] } {
	    set super [lindex $queue end]
	    set c [__class $s $k $super]
	    
	    lappend queue $c
	    __analyse $s $v $queue
	    set queue [lrange $queue 0 end-1]
	} else {
	    set c [lindex $queue end]
	    if { $c ne "" } {
		# Detect if array and take away "sugar" string for
		# array definition
		set multi 0
		if { [regexp {\[\s*\]$} $v] } {
		    set multi 1
		    set v [regsub {\[\s*\]$} $v ""]
		}

		upvar \#0 $c CLASS
		set f [::uobj::new [namespace current] field \
			   [list [::uobj::id $s] [::uobj::id $c]]]
		upvar \#0 $f FIELD
		set FIELD(super) $c
		set FIELD(definition) ""
		if { [string index $k end] eq "-" } {
		    set k [string trim [string range $k 0 end-1]]
		    set FIELD(-optional) 1
		} else {
		    set FIELD(-optional) 0
		}
		set FIELD(-name) $k;     # Name of field
		set FIELD(-type) $v;     # Textual name of field
					 # class/constraint
		set FIELD(-multi) $multi;# Array or not
		config $f;               # Initialise field
		::uobj::objectify $f [list [list config configure] get type \
					 origin]
	    } else {
		${log}::warn "Ignoring $k -> $v no class to attach it to!"
	    }
	}
    }
}


proc ::schema::__read { s fd_or_n {inline ""}} {
    variable SCHEMA
    variable log

    if {![::uobj::isa $s schema]} {
	return -code error "$s unkown or wrong type"
    }
    upvar \#0 $s SMA

    if { $inline eq "" } {
	# Guess if second argument is an opened file descriptor or a file
	# name.  If it is a file name, open it.  Always make sure fd is a
	# file descriptor to which we will write.
	if { [catch {fconfigure $fd_or_n}] } {
	    ${log}::info "Reading schema from $fd_or_n"
	    if { [catch {open $fd_or_n} fd] } {
		${log}::warn "Could not open $fd_or_n: $fd"
		return -code error "Could not open $fd_or_n: $fd"
	    }
	} else {
	    set fd $fd_or_n
	}
    }
    
    set done 0
    set lineno 0
    set data [split $inline "\n"]
    set stripped ""
    while {!$done} {
	if { $inline eq "" } {
	    set line [string trim [gets $fd]]
	} else {
	    set line [lindex $data $lineno]
	}
	incr lineno; # Keep track of line numbers for error spotting.
	if { $line ne "" } {
	    foreach c [split $SCHEMA(comments) ""] {
		set idx [string first $c $line]
		if { $idx >= 0 } {
		    set line [string range $line 0 [expr $idx - 1]]
		}
	    }
	    
	    set line [string trim $line]
	    append stripped $line "\n"
	}
	if {$inline eq "" && [eof $fd]} {
	    set done 1
	}
	if {$inline ne "" && $lineno >= [llength $data] } {
	    set done 1
	}
    }
 
    # Close the file if the second parameter was a file name.
    if { $inline eq "" && $fd ne $fd_or_n } {
	close $fd
    }

    return [string trim $stripped]
}


proc ::schema::__done { s url token } {
    variable SCHEMA
    variable log

    if {![::uobj::isa $s schema]} {
	return -code error "$s unkown or wrong type"
    }
    upvar \#0 $s SMA

    # If we had proper data for the remote URL, analyse it into the
    # schema.
    if { [::http::ncode $token] == 200 \
	     && [::http::status $token] ne "cancelled" } {
	set dta [::http::data $token]
	set def [__read $s "" $dta]
	__analyse $s $def
    } else {
	${log}::warn "Could not get $url: error is\
                      '[::http::error $token]' status is\
                      '[::http::status $token]'"
    }
    ::http::cleanup $token
}


proc ::schema::__fetch { s { url "" } } {
    variable SCHEMA
    variable log

    if {![::uobj::isa $s schema]} {
	return -code error "$s unkown or wrong type"
    }
    upvar \#0 $s SMA

    if { $url eq "" } {
	set url $SMA(url)
    }

    array set URL [::uri::split $url]
    if { [string range $URL(scheme) 0 3] eq "http" && $URL(path) ne $url} {
	# Arrange for password encryption in the geturl command
	set hdrs [list]
	if { [array names URL user] ne "" \
		 && $URL(user) ne "" && $URL(pwd) ne "" } {
	    set auth [::base64::encode "$URL(user):$URL(pwd)"]
	    lappend hdrs Authorization "Basic $auth"
	}

	set cmd [list ::http::geturl $url \
		     -headers $hdrs \
		     -command [list [namespace current]::__done $s $url]]
	if { $SMA(-timeout) > 0 } {
	    lappend cmd -timeout $SMA(-timeout)
	}
	if { [catch {eval $cmd} token] } {
	    ${log}::error "Error while getting URL at $url: $token"
	}
    } else {
	set def [__read $s $url]
	__analyse $s $def
    }
}


proc ::schema::config { o args } {
    variable SCHEMA
    variable log

    if { [lsearch [list schema class field constraint] \
	      [::uobj::type $o]] < 0 } {
	return -code error "$o unkown or wrong type"
    }
    upvar \#0 $o OBJ

    #Save prior object content in OLD and do configuration
    ::uobj::inherit OBJ OLD
    set result [eval ::uobj::config OBJ "-*" $args]

    switch [::uobj::type $o] {
	schema {
	    upvar \#0 $o SMA
	    if { !$SMA(inited) } {
		__fetch $o
		set SMA(inited) 1
	    }
	}
	class {
	    upvar \#0 $o CLASS
	    if { $CLASS(-name) ne $OLD(-name) || $CLASS(uuid) eq "" } {
		upvar \#0 $CLASS(schema) SMA
		# Construct a UUID based on the name of the type and
		# on the source of the schema, this ensures that the
		# same type from the same source will always get the
		# same UUID.
		set CLASS(uuid) [::uuidhash::uuid "$SMA(url)/$CLASS(-name)"]
	    }
	}
	constraint {
	    upvar \#0 $o CONSTRAINT
	    if { $CONSTRAINT(-name) ne $OLD(-name) \
		     || $CONSTRAINT(uuid) eq "" } {
		upvar \#0 $CONSTRAINT(schema) SMA
		# Construct a UUID based on the name of the type and
		# on the source of the schema, this ensures that the
		# same type from the same source will always get the
		# same UUID.
		set CONSTRAINT(uuid) \
		    [::uuidhash::uuid "$SMA(url)/$CONSTRAINT(-name)"]
	    }
	}
	field {
	    upvar \#0 $o FIELD
	    if { $FIELD(-type) ne $OLD(-type) || $FIELD(definition) eq "" } {
		upvar \#0 $FIELD(super) CLS;  # Get to the class def that
					      # carries the field.

		# Detect the class of the field, arrange to
		# auto-declare classes so they can be used before they
		# are declared.
		if { [lsearch $SCHEMA(builtin) $FIELD(-type)] < 0 } {
		    set fc [find $CLS(schema) $FIELD(-type)]
		    if { $fc eq "" } {
			${log}::debug "$FIELD(-type) is an unknown\
                                       class/constraint, forward declaring it"
			set fc [__class $CLS(schema) $FIELD(-type)]
		    }
		} else {
		    set fc ""
		}
		set FIELD(definition) $fc
	    }
	}
    }

    return $result
}


proc ::schema::__init { } {
    variable SCHEMA
    variable log
    variable libdir

    # Sources for the unit description and prefixes are:
    # http://dx.doi.org/10.3247/SL1Phys06.002
    # http://en.wikipedia.org/wiki/International_System_of_Units

    if { ! $SCHEMA(inited) } {
	set fname [file join $libdir units.txt]
	if { [catch {open $fname} fd] } {
	    ${log}::error "Could not open list of official BIPM units at\
                           $fname: $fd"
	} else {
	    set SCHEMA(units) [read $fd]
	    close $fd
	    ${log}::debug "Read official BIPM units from $fname"
	}

	set fname [file join $libdir prefixes.txt]
	if { [catch {open $fname} fd] } {
	    ${log}::error "Could not open list of official BIPM prefixes at\
                           $fname: $fd"
	} else {
	    set SCHEMA(prefixes) [read $fd]
	    close $fd
	    ${log}::debug "Read official BIPM prefixes from $fname"
	}

	set SCHEMA(inited) 1
    }
}


proc ::schema::units {} {
    variable SCHEMA

    __init
    set units {}
    foreach {unt usym} $SCHEMA(units) {
	foreach {pfx psym} $SCHEMA(prefixes) {
	    lappend units ${pfx}${unt} ${psym}${usym}
	}
	lappend units $unt $usym
    }

    return $units
}


proc ::schema::new { url args } {
    variable SCHEMA
    variable log

    __init

    set s [::uobj::new [namespace current] schema]
    upvar \#0 $s SMA

    if { [catch {package require tls} err] } {
	${log}::error "Will not be able to include/read secure remote schemas!\
                       (reason: $err)"
    } else {
	::http::register https 443 ::tls::socket
    }

    set SMA(self) $s;       # Ourselves
    set SMA(url) $url;      # URL to read from
    set SMA(inited) 0;      # We have not perform init yet

    ::uobj::inherit SCHEMA SMA
    ::uobj::objectify $s [list [list config configure] find dump get]
    
    eval config $s $args

    return $s
}

package provide schema $::schema::version
