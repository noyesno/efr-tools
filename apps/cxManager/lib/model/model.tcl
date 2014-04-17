package require Tcl 8.4

package require http
#package require tls;   # Will be done dynamically
package require uri
package require base64
package require uuidhash

package require uobj
package require diskutil
package require event

package require schema

namespace eval ::model {
    variable MODEL
    if { ![info exists MODEL] } {
	array set MODEL {
	    comments         "\#!;"
	    -timeout         5000
	}
	variable version 0.1
	variable libdir [file dirname [file normalize [info script]]]
	::uobj::install_log model MODEL
	::uobj::install_defaults model MODEL
    }
}

proc ::model::__reference { f name ns } {
    variable MODEL
    variable log

    set r [::uobj::find [namespace current] reference\
		[list -name == $name]]
    if { $r eq "" } {
	# Dynamically create a reference for further use
	# (i.e. definition!)
	set r [::uobj::new [namespace current] reference]
	upvar \#0 $r REF
	set cls [$f get class]
	set REF(-class) $cls
	set REF(-object) [$cls new $ns]
	set REF(-name) $name
	set REF(-uuid) ""
	::uobj::objectify $r [list get]
	::event::generate $r Reference [list %c $cls %i $REF(-object) %r $name]
    }
    
    return $r
}


proc ::model::__specialise { r class ns } {
    variable MODEL
    variable log

    upvar \#0 $r REF

    # Already a specialisation or of the same class, do nothing.
    if { $class eq "" || $REF(-class) eq $class \
	     || [lsearch [$REF(-class) inheritance] $class] >= 0 } {
	return $REF(-object)
    }

    # Check that the reference was from a class that is actually in
    # the super classes of the class of the target.
    set inherits 0
    foreach inherited [$class inheritance] {
	if { $REF(-class) eq $inherited } {
	    set inherits 1
	    break;
	}
    }
    # Fix the class of o, since it has been dynamically
    # declared previously.  The following code actually
    # manages class type modification, which is necessary
    # when super classes are used in class definitions.
    if { ! $inherits } {
	return ""
    }

    # Create an instance of the class
    set o [$class new $ns]
    upvar \#0 $o OBJ
    
    # Copy old referred object into a new instance, this time of the
    # proper class (i.e. deeper down in the class hierarchy), get rid
    # of the previous object.
    upvar \#0 $REF(-object) SRC
    ${log}::debug "Specialising $REF(-name) to become a\
		   [$class get -name] (was a [$REF(-class) get -name])"
    array set OBJ [array get SRC -*]
    ::uobj::delete $REF(-object)
    
    # Update the objects that referred to this one to reflect the
    # class change.
    foreach referee [::uobj::allof [namespace current] reference] {
	upvar \#0 $referee REFEREE
	if { [info exists $REFEREE(-object)] } {
	    upvar \#0 $REFEREE(-object) ROBJ
	    foreach f [array names ROBJ -*] {
		for {set i [lsearch $ROBJ($f) $REF(-object)]} {$i>=0} \
		    {set i [lsearch $ROBJ($f) $REF(-object)]} {
			set ROBJ($f) \
			    [lreplace $ROBJ($f) $i $i $o]
		    }
	    }
	}
    }
    
    # Update the reference to reflect the class and object that it now
    # points at.
    set REF(-object) $o
    set REF(-class) $class
    
    return $o
}


proc ::model::__set { r descr { ns "" } } {
    variable MODEL
    variable log

    if { ![::uobj::isa $r reference]} {
	return -code error "$r unknown or wrong type"
    }
    set class [$r get -class]
    set o [$r get -object]
    if { $o eq "" } {
	${log}::error "Reference $r does not point to existing object!"
	return
    }
    upvar \#0 $o OBJ

    foreach {k v} $descr {
	set f [$class field $k]
	if { $f ne "" } {
	    if { [$f get builtin] || [$f get constraint] ne "" } {
		set OBJ(-$k) $v;  # No extra work, leave type checking
				  # to the verification mechanisms in
				  # schema.
	    } else {
		set fclass [$f get class]
		if { [$f get -multi] } {
		    set OBJ(-$k) {}
		    foreach val $v {
			# Get to reference and get to object from there
			set fr [__reference $f $val $ns]
			upvar \#0 $fr FREF
			
			set o [__specialise $fr $fclass $ns]
			if { $o ne "" } {
			    lappend OBJ(-$k) $o
			} else {
			    return -code error \
				"Cannot specialise $FREF(-name) to\
		                 a [$fclass get -name]"
			}
		    }
		} else {
		    # Get to reference and get to object from there
		    set fr [__reference $f $v $ns]
		    upvar \#0 $fr FREF
		    
		    set o [__specialise $fr $fclass $ns]
		    if { $o ne "" } {
			set OBJ(-$k) $o
		    } else {
			return -code error \
			    "Cannot specialise $FREF(-name) to a\
		             [$fclass get -name]"
		    }
		}
	    }
	} else {
	    return -code error "Field $k is unknown to class $type"
	}
    }
}


proc ::model::__analyse { m lst url refstore ns } {
    variable MODEL
    variable log

    if {![::uobj::isa $m model]} {
	return -code error "$m unkown or wrong type"
    }
    upvar \#0 $m MDL

    set hex "\[a-fA-F0-9\]"
    set uuid_filter "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]" 

    foreach {type instance descr} $lst {
	if { [string match "*=$uuid_filter" $instance] } {
	    foreach {instance uuid} [split $instance "="] break
	    ${log}::debug "Forcing $instance to UUID $uuid"
	} else {
	    set uuid ""
	}
	puts "$instance : $uuid"
	set class [$MDL(schema) find $type]
	set r ""
	if { $class eq "" } {
	    ${log}::error "$type is not a class known from schema!"
	} else {
	    set r [__add $m $class $instance $ns]
	}
	if { $r ne "" } {
	    upvar \#0 $r REF
	    set REF(-uuid) $uuid
	    #set o [$r get -object]
	    __set $r $descr $ns
	} else {
	    return -code error "Cannot instantiate constraints at top level"
	}
    }

    # Gather all objects that we have created under the parsing
    # process and return them.  Arrange for setting their uuid to
    # something that is uniquely dependent on the context.  Make sure
    # that we get rid of the references now that we have parsed.
    set created {}
    foreach r [::uobj::allof [namespace current] reference] {
	upvar \#0 $r REF
	if { [info exists $REF(-object)] } {
	    upvar \#0 $REF(-object) OBJ
	    if { $REF(-uuid) ne "" } {
		set OBJ(uuid) $REF(-uuid)
	    } else {
		set OBJ(uuid) \
		    [::uuidhash::uuid "$url/[$REF(-class) get uuid]/$REF(-name)"]
	    }
	    if { $refstore ne "" } {
		set OBJ($refstore) $REF(-name)
	    }
	    lappend created $REF(-object)
	}
	::uobj::delete $r
    }
    set MDL(objects) [concat $MDL(objects) $created]

    return $created
}


proc ::model::__read { m fd_or_n {inline ""}} {
    variable MODEL
    variable log

    if {![::uobj::isa $m model]} {
	return -code error "$m unkown or wrong type"
    }
    upvar \#0 $m MDL

    if { $inline eq "" } {
	# Guess if second argument is an opened file descriptor or a file
	# name.  If it is a file name, open it.  Always make sure fd is a
	# file descriptor to which we will write.
	if { [catch {fconfigure $fd_or_n}] } {
	    ${log}::info "Reading model from $fd_or_n"
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
    while {! $done} {
	if { $inline eq "" } {
	    set line [string trim [gets $fd]]
	} else {
	    set line [lindex $data $lineno]
	}
	incr lineno; # Keep track of line numbers for error spotting.
	if { $line ne "" } {
	    foreach c [split $MODEL(comments) ""] {
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


proc ::model::__dumpval { fd field val references } {
    variable MODEL
    variable log

    if { [$field type] ne "" } {
	# If the field is a builtin or extends one, dump intelligently
	# with quotes whenever necessary.
	if { [$field type] eq "String" } {
	    puts -nonewline $fd "\"$val\""
	} else {
	    if { $val eq "" } {
		puts -nonewline $fd "\"$val\""
	    } else {
		puts -nonewline $fd "$val"
	    }
	}
    } else {
	if { $val eq "" } {
	    puts -nonewline $fd "\"\""
	} else {
	    foreach {o ref} $references {
		if { $o eq $val } {
		    puts -nonewline $fd "$ref"
		}
	    }
	}
    }
}


proc ::model::dump { m { refstore "" } { fd stdout } } {
    variable MODEL
    variable log

    if {![::uobj::isa $m model] } {
	return -code error "$m unknown or wrong type"
    }
    upvar \#0 $m MDL
    
    # Create textual references for all the objects that we should
    # dump, since we might have forward references
    set references {}
    set cnt 0
    foreach o $MDL(objects) {
	if { [info exists $o] } {
	    set class [$MDL(schema) find [::uobj::type $o]]
	    upvar \#0 $o OBJ

	    # Decide upon object name
	    if { $refstore eq "" || [array names OBJ $refstore] eq "" \
		     || $OBJ($refstore) eq "" } {
		set reference "[string tolower [$class get -name]]!$cnt"
	    } else {
		set reference $OBJ($refstore)
	    }
	    lappend references $o $reference
	}
    }

    # Now dump all the objects that have been passed as an
    # argument. Dump is dependent on the schema that is associated to
    # the model.
    set cnt 0
    foreach o $MDL(objects) {
	if { [info exists $o] } {
	    set class [$MDL(schema) find [::uobj::type $o]]
	    upvar \#0 $o OBJ

	    foreach {ro ref} $references {
		if { $ro eq $o } {
		    puts $fd "[$class get -name] ${ref}!$OBJ(uuid) { "
		    foreach f [array names OBJ -*] {
			set field [$class field $f]
			if { $OBJ($f) ne "" } {
			    puts -nonewline $fd "\t[string trimleft $f "-"]\t"
			    if { [$field get -multi] } {
				puts -nonewline $fd "{"
				set len [llength $OBJ($f)]
				for {set i 0} {$i < $len} {incr i} {
				    __dumpval $fd $field [lindex $OBJ($f) $i] \
					$references
				    if { $i < [expr $len-1] } {
					puts -nonewline $fd " "
				    }
				}
				puts $fd "}"
			    } else {
				__dumpval $fd $field $OBJ($f) $references
				puts $fd ""
			    }
			}
		    }
		    puts $fd "}"
		}
	    }
	}
    }
}


proc ::model::find { m uuid } {
    variable MODEL
    variable log
    
    if {![::uobj::isa $m model] } {
	return -code error "$m unknown or wrong type"
    }
    upvar \#0 $m MDL

    foreach o $MDL(objects) {
	upvar \#0 $o OBJ
	if { $OBJ(uuid) eq $uuid } {
	    return $o
	}
    }
    return ""
}


proc ::model::get { o what } {
    variable MODEL
    variable log

    set type [::uobj::type $o]
    switch $type {
	model {
	    upvar \#0 $o MDL
	    switch -glob -nocase -- $what {
		objects -
		schema {
		    return $MDL($what)
		}
		-* {
		    return [config $o $what]
		}
		default {
		    return -code error "$what cannot be got from a $type"
		}
	    }
	}
	reference {
	    upvar \#0 $o REF
	    switch -glob -nocase -- $what {
		upvar \#0 $r REF
		-* {
		    if { [array names REF $what] ne "" } {
			return $REF($what)
		    } else {
			return ""
		    }
		}
		default {
		    return -code error "$what cannot be got from a $type"
		}
	    }
	}
    }

    return ""; #Never reached.
}


proc ::model::add { m cls {refstore ""} {ref ""} {ns ""}} {
    variable MODEL
    variable log

    if {![::uobj::isa $m model] } {
	return -code error "$m unknown or wrong type"
    }
    upvar \#0 $m MDL

    # Add object to model, giving it a reference
    set o ""
    set r [__add $m $cls $ref $ns]
    
    # Save reference in refstore, if relevant and return created
    # object.  Dump the reference object as this is only transient.
    if { $r ne "" } {
	upvar \#0 $r REF
	set o $REF(-object)
	if { [info exists $o] } {
	    upvar \#0 $o OBJ
	    set OBJ(uuid) \
		[::uuidhash::uuid "[$REF(-class) get uuid]/$REF(-name)"]
	    if { $refstore ne "" } {
		set OBJ($refstore) $REF(-name)
	    }
	    lappend MDL(objects) $o
	}
	::uobj::delete $r
    }
    return $o
}


proc ::model::__add { m cls { ref "" } { ns "" } } {
    variable MODEL
    variable log
    
    if {![::uobj::isa $m model] } {
	return -code error "$m unknown or wrong type"
    }
    if {![::uobj::isa $cls class]} {
	return -code error "$cls unknown or wrong type"
    }
    upvar \#0 $m MDL
    
    # Generate a reference if we don't provide one.
    if { $ref eq "" } {
	set ref "a"
	append ref [$cls get -name]
	append ref [clock clicks -milliseconds]
    }

    # Create a reference to the object or get to the one that
    # already exist for this name.
    set r [::uobj::find [namespace current] reference \
	       [list -name == $ref]]
    if { $r eq "" } {
	# Create an instance of the class
	set o [$cls new $ns]
	# Create a reference to the object and remember it
	set r [::uobj::new [namespace current] reference]
	upvar \#0 $r REF
	set REF(-name) $ref
	set REF(-class) $cls
	set REF(-object) $o
	set REF(-uuid) ""
	::uobj::objectify $r [list get]
	::event::generate $m Create [list %i $o %c $cls %r $ref %n $ns]
	::event::generate $m Add [list %i $o %c $cls %r $ref %n $ns]
    } else {
	upvar \#0 $r REF
	set o [__specialise $r $cls $ns]
	if { $o eq "" } {
	    return -code error "$REF(-name) previously declared as\
		                a [$REF(-class) get -name] which\
		                is not a class inherited by [$cls get -name]"
	} else {
	    ::event::generate $m Specialise [list %i $o %c $cls %r $ref %n $ns]
	    ::event::generate $m Add [list %i $o %c $cls %r $ref %n $ns]
	}
    }

    return $r
}


proc ::model::create { m url { refstore "" } { ns "" } } {
    variable MODEL
    variable log
    
    if {![::uobj::isa $m model] } {
	return -code error "$m unknown or wrong type"
    }
    upvar \#0 $m MDL
    
    array set URL [::uri::split $url]
    set def ""
    if { [string range $URL(scheme) 0 3] eq "http" && $URL(path) ne $url } {
	# Arrange for password encryption in the geturl command
	set hdrs [list]
	if { [array names URL user] ne "" \
		 && $URL(user) ne "" && $URL(pwd) ne "" } {
	    set auth [::base64::encode "$URL(user):$URL(pwd)"]
	    lappend hdrs Authorization "Basic $auth"
	}

	set cmd [list ::http::geturl $url \
		     -headers $hdrs]
	if { $MDL(-timeout) > 0 } {
	    lappend cmd -timeout $MDL(-timeout)
	}
	if { [catch {eval $cmd} token] } {
	    ${log}::error "Error while getting URL at $url: $token"
	} else {
	    if { [::http::ncode $token] == 200 } {
		set def [__read $m "" [::http::data $token]]
	    }
	    ::http::cleanup $token
	}
    } else {
	set def [__read $m $url]
    }
    return [__analyse $m $def $url $refstore "$ns"]
}


proc ::model::config { m args } {
    variable MODEL
    variable log
    
    if {![::uobj::isa $m model] } {
	return -code error "$m unknown or wrong type"
    }
    upvar \#0 $m MDL
    
    # Save prior object content in OLD and do configuration.
    ::uobj::inherit MDL OLD
    set result [eval ::uobj::config MDL "-*" $args]
    
    return $result
}


proc ::model::new { schema args } {
    variable MODEL
    variable log
    
    set m [::uobj::new [namespace current] model]
    upvar \#0 $m MDL

    if { [catch {package require tls} err] } {
	${log}::error "Will not be able to include/read secure remote models!\
                       (reason: $err)"
    } else {
	::http::register https 443 ::tls::socket
    }
    
    set MDL(self) $m;        # Ourselves
    set MDL(schema) $schema; # Schema to create objects from
    set MDL(objects) {};     # List of objects in model

    ::uobj::inherit MODEL MDL
    ::uobj::objectify $m [list [list config configure] add create dump get find]
    eval config $m $args
    return $m
}

package provide model $::model::version
