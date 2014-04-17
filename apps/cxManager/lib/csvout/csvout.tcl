##################
## Module Name     --  csvout
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    The purpose of this module is to store the successive values of
##    objects into CSV files so historical data can easily be imported
##    in graphing tools or similar.
##
##################

# IMPLEMENTATION NOTES
#
# At initialisation, the module register a write trace on all the
# objects that have been created within the model. Every time a write
# occur, the trace will give the object a respit period (controlled by
# -flush, in milliseconds) and write that version of the object to the
# database (see below).  The respit allows for a number of successive
# operations within that (short!) time frame so as to try minimising
# the number of versions in the database.
#
# A timestamp and a description of what fields have been changed is
# automatically added to the CSV files in order to facilitate their
# import.  Future or past updates are ignored.

package require uobj
package require event

namespace eval ::csvout {
    variable CSVOUT
    if { ![info exists CSVOUT] } {
	array set CSVOUT {
	    -dir       ""
	    -flush     200
	    -fname     "%name%--%uuid%.csv"
	    -separator ","
	    -quote     "\""
	    -cutter    "|"
	    -format    "%Y%m%d-%H:%M:%S"
	    -header    on
	    -past      off
	    -future    off
	    -seeker    8192
	    -jumper    0.9
	}
	variable version 0.1
	variable libdir [file dirname [file normalize [info script]]]
	::uobj::install_log csvout CSVOUT
	::uobj::install_defaults csvout CSVOUT
    }
}


# ::csvout::__dumpfield -- Convert value for CSV storage
#
#	Format a value, issuing from a field so that it can be stored
#	in the CSV database.  The procedure will replace object
#	identifiers by their UUIDs and booleans by 0 or 1.
#
# Arguments:
#	f	Identifier of the field spec in schema
#	val	Value to convert
#
# Results:
#	Return a string ready for insertion in a CSV file.
#
# Side Effects:
#	None.
proc ::csvout::__dumpfield { f val } {
    if { [$f get builtin] || [$f get constraint] ne "" } {
	# Can we convert UUID better to save memory?
	switch -nocase -- [$f type] {
	    String -
	    UUID -
	    Timestamp {
		return $val
	    }
	    Boolean {
		return [expr [string is true $val]?1:0]
	    }
	    default {
		return $val
	    }
	}
    } else {
	if { $val eq "" } {
	    return ""
	} else {
	    upvar \#0 $val OBJ
	    return $OBJ(uuid)
	}
    }
}



# ::csvout::__field -- Format field value for insertion in CSV file
#
#	Convert the content of a field for insertion in a CSV file.
#	This procedure understands properly multi-fields, i.e. arrays
#	of values.  Conversion of single values is delegated to
#	__dumpfield.
#
# Arguments:
#	f	Field specification from schema
#	val	Value to convert
#
# Results:
#	Return a string ready for insertion into CSV file
#
# Side Effects:
#	None.
proc ::csvout::__field { f val } {
    set result ""
    if { [$f get -multi] } {
	foreach v $val {
	    lappend result [__dumpfield $f $v]
	}
    } else {
	set result [__dumpfield $f $val]
    }
    return $result
}


# ::csvout::__open -- Open CSV file to append data
#
#	Open (possibly creating) a CSV file so as to be able to append
#	data to the file.  At creating, a header is automatically
#	added to the CSV file as specified in RFC4180.
#
# Arguments:
#	csvout	CSV context as created by new
#	o	Identifier of object to open file for data output
#	mode	Opening mode: dump for appending data, read for recap
#
# Results:
#	Return a file descriptor ready for appending data, empty
#	string on file/directory creation errors.
#
# Side Effects:
#	None.
proc ::csvout::__open { csvout o { mode "dump" } } {
    variable CSVOUT
    variable log

    if { ![::uobj::isa $csvout csvout] } {
	return -code error "$csvout unkown or wrong type"
    }
    upvar \#0 $csvout COUT

    upvar \#0 $o OBJ
    set RESOLVER(name) $OBJ(name)
    set RESOLVER(ref) $OBJ(name)
    set RESOLVER(uuid) $OBJ(uuid)
    set RESOLVER(class) [::uobj::type $o]

    # Create directory for output, if it does not exist.
    set dir [::uobj::resolve $o $COUT(-dir) [array get RESOLVER]]
    if { [catch {file mkdir $dir} err] } {
	${log}::error "Cannot create/access directory $dir: $err"
	return ""
    }

    # When in dumping mode, pen file for appending, testing its
    # existence before opening so as to be able to automatically
    # append a header at creation time.  When in reading mode only,
    # simply open the file if it exists and return the file desciptor.
    set fname [file join $dir \
		   [::uobj::resolve $o $COUT(-fname) [array get RESOLVER]]]
    if { $mode eq "dump" } {
	set opener "a+"
    } else {
	set opener "r"
    }
    set header [expr ![file exists $fname]]
    if { [catch {open $fname $opener} fd] } {
	if { $mode eq "dump" } {
	    # It's ok not to have any file to read when opening for
	    # reading, since this is how we tell the caller that there
	    # isn't anything to read right now.
	    ${log}::warn "Cannot open file at $fname for CSV output: $fd"
	}
	set fd "";  # Nothing to read from or write to!
    } else {
	if { $mode eq "dump" && $header && [string is true $COUT(-header)] } {
	    # Access the class directing the content of the object.
	    set c [[$COUT(model) get schema] find [::uobj::type $o]]
	    set fields [list __timestamp]
	    foreach s [$c inheritance on] {
		foreach f [$s get fields] {
		    lappend fields [__quoted $csvout [$f get -name]]
		}
	    }
	    lappend fields __hints
	    puts $fd "[join $fields $COUT(-separator)]"
	}
    }

    # Return file descriptor, caller should close
    return $fd
}


# ::csvout::__quoted -- Handle quotation of field contents
#
#	Quote a value as specified in RFC4180.
#
# Arguments:
#	csvout	CSV context as created by new
#	val	Value to quote
#
# Results:
#	Return quoted value.
#
# Side Effects:
#	None.
proc ::csvout::__quoted { csvout val } {
    variable CSVOUT
    variable log

    if { ![::uobj::isa $csvout csvout] } {
	return -code error "$csvout unkown or wrong type"
    }
    upvar \#0 $csvout COUT

    # Replace quotes by double quotes if any
    if { [string first $COUT(-quote) $val] >= 0 } {
	set val [string map $val \
		     [list $COUT(-quote) [string repeat $COUT(-quote) 2]]]
    }
    # Force quoting if we have separators or quotes
    if { [string first $COUT(-separator) $val] >= 0 \
	     || [string first $COUT(-quote) $val] >= 0 } {
	set val "$COUT(-quote)${val}$COUT(-quote)"
    }

    return $val
}


# ::csvout::__flush -- Flush a version to CSV file
#
#	Writes the current content of the object to the CSV file
#	implicitely under the current time as the version.
#
# Arguments:
#	csvout	CSV context created by <new>
#	o	Identfier of object.
#
# Results:
#	Return the list of fields that were found in the update.
#
# Side Effects:
#	Append textual representation of the update to the CSV file.
proc ::csvout::__flush { csvout o } {
    variable CSVOUT
    variable log

    if { ![::uobj::isa $csvout csvout] } {
	return -code error "$csvout unkown or wrong type"
    }
    upvar \#0 $csvout COUT

    # Get the current time
    set now [clock seconds]

    # Only consider scheduled writes if we've been told so by the
    # options.  Arrange in all cases for the variable <when> to
    # contain the exact timestamp of the event to dump.
    set when [::uobj::keyword $o "when"]
    if { $when eq "" } {
	set when $now
    } else {
	if { [string is true $COUT(-past)] && $when < $now } {
	    # Let it pass happily, we'll have unordered and past
	    # timestamps in the CSV file.
	    ${log}::debug "Keeping past update"
	} elseif { [string is true $COUT(-future)] && $when > $now } {
	    # Let it pass happily, we'll have unordered and future
	    # timestamps in the CSV file.
	    ${log}::debug "Keeping future update"
	} else {
	    # Most of the time we want to stop when not dumping "live"
	    # data, but rather data that will occur in the past or
	    # future.
	    ::uobj::keyword $o csv/output ""
	    ::uobj::keyword $o csv/hints ""
	    ${log}::debug "Discarding past or future update"
	    return {}
	}
    }

    # Open file for output, creating if necessary
    set fd [__open $csvout $o "dump"]

    # Access the class directing the content of the object.
    set c [[$COUT(model) get schema] find [::uobj::type $o]]

    # For all the fields coming from all the classes that we inherit
    # from, store the value in the file.  Make sure to prepend current
    # timestamp for the operation, and to append an hint over which
    # fields were modified by the operation, even though we actually
    # store the whole content of the object each time.
    set hints {}
    if { $c ne "" && $fd ne "" } {
	upvar \#0 $o OBJ

	# Prepend timestamp
	set values [list [__quoted $csvout \
			      [clock format $when -format $COUT(-format)]]]
	
	# Continue with quoted content of all fields.
	foreach s [$c inheritance on] {
	    foreach f [$s get fields] {
		set fname [$f get -name]
		if { [array names OBJ -$fname] ne "" } {
		    lappend values [__quoted $csvout [__field $f $OBJ(-$fname)]]
		}
	    }
	}
	
	# Append a hint describing which fields were modified.
	foreach h [::uobj::keyword $o csv/hints] {
	    lappend hints [string trimleft $h "-"]
	}
	lappend values [__quoted $csvout [join $hints $COUT(-cutter)]]

	# Dump single line to CSV file
	puts $fd [join $values $COUT(-separator)]
    }
    
    # Close file, we will reopen next time if necessary.
    if { $fd ne "" } {
	close $fd
    }

    # Empty the output timer and the hints. 
    ::uobj::keyword $o csv/output ""
    ::uobj::keyword $o csv/hints ""

    return $hints
}


# ::csvout::__write -- Remember write and schedule versioning dump
#
#	This procedure is registered to be called each time an object
#	of the model is written to.  It schedule a version dump of the
#	object to the database in no more than -flush milliseconds.
#
# Arguments:
#	csvout	CSV context, as created by <new>
#	varname	Name of variable being changed.
#	idx	Index in array, (always relevant in our case)
#	op	Operation on object (always write in our case)
#
# Results:
#	None.
#
# Side Effects:
#	Schedule a version dump of the object to the relevant CSV file.
proc ::csvout::__write { csvout varname idx op } {
    variable CSVOUT
    variable log

    if { ![::uobj::isa $csvout csvout] } {
	return -code error "$csvout unkown or wrong type"
    }
    upvar \#0 $csvout COUT
    upvar $varname V; # Variable name is in the context of the calling
		      # procedure, i.e. where the write operation
		      # occurs.  We utilise the fact that we always
		      # store the identifier of the object under the
		      # index id, which is an undocumented feature.

    # Arrange to push data to the database within -flush milliseconds.
    set delay [::uobj::keyword $V(id) csv/output]
    if { $delay eq "" } {
	set delay [after $COUT(-flush) \
		       [namespace current]::__flush $csvout $V(id)]
	::uobj::keyword $V(id) csv/output $delay
    }

    # Remember what was changed in the object, make sure we do this
    # only once, no point otherwise...
    set hints [::uobj::keyword $V(id) csv/hints]
    lappend hints $idx
    ::uobj::keyword $V(id) csv/hints [lsort -unique $hints]
}


# ::csvout::__trace -- Setup traces on object write
#
#       Arrange to have internal traces on write on an object (of the
#       model) so that data from the object will be written to the
#       database automatically whenever the object is modified.
#
# Arguments:
#	csvout	CSV context, as created by <new>
#       o       Identifier of the object
#
# Results:
#       Return the identifier of the object if a trace was setup,
#       empty string if no trace was installed, which means that we
#       detected that there was already a trace from us on that
#       object.
#
# Side Effects:
#       Will arrange for content of the object updates to be written
#       to the CSV file that is associated to the object.
proc ::csvout::__trace { csvout o } {
    variable CSVOUT
    variable log

    if { ![::uobj::isa $csvout csvout] } {
	return -code error "$csvout unkown or wrong type"
    }
    upvar \#0 $csvout COUT

    if { ![followed $csvout $o] } {
	trace add variable $o write [list [namespace current]::__write $csvout]
	return $o
    }

    return ""
}


# ::csvout::follow -- Start following an object.
#
#       Arrange to bind an object to a CSV file: the state of the
#       object will be read from the latest available state in the CSV
#       file, then write traces will be created so as to record all
#       present and future updates on the object.
#
# Arguments:
#	csvout	CSV context, as created by <new>
#       o       Identifier of the object
#
# Results:
#       Return the identifier of the object if a trace was setup,
#       empty string if no trace was installed, which means that we
#       detected that there was already a trace from us on that
#       object.
#
# Side Effects:
#       Will arrange for content of the object updates to be written
#       to the CSV file that is associated to the object.
proc ::csvout::follow { csvout o } {
    variable CSVOUT
    variable log

    if { ![::uobj::isa $csvout csvout] } {
	return -code error "$csvout unkown or wrong type"
    }
    upvar \#0 $csvout COUT

    if { ! [followed $csvout $o] } {
	__recap $csvout $o
	__trace $csvout $o

	return $o
    }
    return ""
}


# ::csvout::followed -- Check if we are following an object
#
#       Check if this module is following a given object, detection is
#       made through looking for matching write traces for the object.
#
# Arguments:
#	csvout	CSV context, as created by <new>
#       o       Identifier of the object
#
# Results:
#       Return a boolean describing the following state of the object.
#
# Side Effects:
#       None.
proc ::csvout::followed { csvout o } {
    variable CSVOUT
    variable log

    if { ![::uobj::isa $csvout csvout] } {
	return -code error "$csvout unkown or wrong type"
    }
    upvar \#0 $csvout COUT

    set found 0
    foreach nfo [trace info variable $o] {
	foreach {op pfx} $nfo break
	if { [string first [namespace current]::__write $pfx] >= 0 } {
	    set found 1
	}
    }
    return $found
}


# ::csvout::__split -- Split CSV line
#
#       Split a CSV line into the fields that it contains, paying
#       proper attention to separators and quoting.  This code
#       originates from the code available at the wiki:
#       http://wiki.tcl.tk/2215 (scroll down!).  The current
#       implementation probably does not work against lines that
#       contain carriage returns or other end-of-line markers.
#
# Arguments:
#	csvout	CSV context, as created by <new>
#       line    Input line to split.
#
# Results:
#       Return a list of all fields contained in the line, including
#       empty fields.
#
# Side Effects:
#       None.
proc ::csvout::__split { csvout line } {
    variable CSVOUT
    variable log

    upvar \#0 $csvout COUT

    # Process each input character.
    set commaRX [string map [list "%separator%" $COUT(-separator)] \
		     {.*?(?=%separator%|$)}]
    set result [list]
    set beg 0
    while {$beg < [string length $line]} {
       if {[string index $line $beg] eq "\""} {
          incr beg
          set quote false
          set word ""
          foreach char [concat [split [string range $line $beg end] ""] \
			    [list ""]] {
             # Search forward for the closing quote, one character at a time.
             incr beg
             if {$quote} {
		 if {[lsearch [list $COUT(-separator) ""] $char] >= 0} {
                   # Quote followed by comma or end-of-line indicates the end of
                   # the word.
                   break
                } elseif {$char eq $COUT(-quote)} {
                   # Pair of quotes is valid.
                   append word $char
                } else {
                   # No other characters can legally follow quote.  I think.
                   return -code error \
		       "extra characters after close-quote"
                }
                set quote false
             } elseif {$char eq ""} {
                # End-of-line inside quotes indicates embedded newline.
                return -code error \
		    "embedded newlines not supported"
             } elseif {$char eq $COUT(-quote)} {
                # Treat the next character specially.
                set quote true
             } else {
                # All other characters pass through directly.
                append word $char
             }
          }
          lappend result $word
       } else {
          # Use all characters up to the comma or line ending.
          regexp -start $beg $commaRX $line word
          lappend result $word
          set beg [expr {$beg + [string length $word] + 1}]
       }
    }

    # If the line ends in a comma, add a blank word to the result.
    if {[string index $line end] eq $COUT(-separator)} {
       lappend result ""
    }

    # Done.  Return the result list.
    return $result    
}


# ::csvout::__update -- Update field in object
#
#       Update the content of a field in an object using a string
#       value, as read from the CSV file.  This procedure will
#       properly translate back from UUIDs to object identifier in the
#       internal data space.
#
# Arguments:
#	o	Identifier of object to update
#       f       Identifier of field (from class)
#	val	Value to write in field.
#
# Results:
#       None.
#
# Side Effects:
#       Change value of field in object.
proc ::csvout::__update { o f val } {
    variable CSVOUT
    variable log
    
    upvar \#0 $o OBJ

    if { [$f get builtin] || [$f get constraint] ne "" } {
	set OBJ(-[$f get -name]) $val
    } else {
	if { [$f get -multi] } {
	    set OBJ(-[$f get -name]) {}
	    foreach v $val {
		foreach {pv c} [::find:uuid $v object] break
		if { $pv ne "" } {
		    lappend OBJ(-[$f get -name]) $pv
		}
	    }
	} else {
	    foreach {v c} [::find:uuid $val object] break
	    if { $v ne "" } {
		set OBJ(-[$f get -name]) $v
	    }
	}
    }
}


# ::csvout::__recap -- Revert to latest available state from CSV
#
#       Revert to the latest available state of the object, given
#       content is available in the associated CSV file and the
#       structure of the object has not changed.  This procedure is
#       also able to take into account future timestamps if such had
#       been used, i.e. it will get to the state that was wished at
#       the time of the future update as long as this update appears
#       at the end of the file.
#
# Arguments:
#	csvout	CSV context, as created by <new>
#	o	Identifier of object to update
#
# Results:
#       Return the list of field names that were updated, an empty
#       list in all other casese.
#
# Side Effects:
#       Change value of fields in object according to CSV content.
proc ::csvout::__recap { csvout o } {
    variable CSVOUT
    variable log

    if { ![::uobj::isa $csvout csvout] } {
	return -code error "$csvout unkown or wrong type"
    }
    upvar \#0 $csvout COUT

    set updated {};   # List of fields updated by procedure

    # Open CSV file, if any and find class for object.
    set fd [__open $csvout $o "read"]
    set c [[$COUT(model) get schema] find [::uobj::type $o]]

    if { $fd eq "" || $c eq "" } {
	${log}::debug "No history file or no class available for $o"
    } else {
	${log}::info "Reading old state for object $o from CSV history"

	# Access all fields that are in the object, prepend the
	# __timestamp and the __hints that are specific to the module
	# and arrange for ofields to be that list and FMAPPER an array
	# that will map from the name to the identifier of the fields.
	array set FMAPPER {}
	set ofields [list __timestamp]
	upvar \#0 $o OBJ
	    
	# Continue with quoted content of all fields.
	foreach s [$c inheritance on] {
	    foreach f [$s get fields] {
		lappend ofields [$f get -name]
		set FMAPPER([$f get -name]) $f
	    }
	}
	lappend ofields __hints

	# Arrange for the variable fields to be the list of the fields
	# that we will be finding in the CSV file.  We prefer reading
	# from the header of the file, but can do with the fields
	# currently in the object.  If we read from the file, scream
	# if we have a mismatch, meaning we probably have an object
	# that have changed description along its life.
	set fields {}
	if { [string is true $COUT(-header)] } {
	    # Go to start of file and read header
	    seek $fd 0
	    set hdr [gets $fd]
	    set fields [__split $csvout $hdr]
	    
	    # Now check file fields against current object and
	    # vice-versa, screaming when we have a mismatch.
	    foreach f $fields {
		if { [lsearch $ofields $f] < 0 } {
		    ${log}::critical \
			"File field $f has been removed from object!"
		    return {}
		}
	    }
	    foreach f $ofields {
		if { [lsearch $fields $f] < 0 } {
		    ${log}::critical \
			"Object field $f cannot be found in file!"
		    return {}
		}
	    }
	} else {
	    # If we didn't have header in the file, trust the
	    # object...
	    set fields $ofields
	}

	# Go towards the end of the file in clever ways.  For small
	# files, we can afford reading everything. For bigger files,
	# jump to the end of the file to minimise CSV parsing overhead
	# (at the risk of missing the real latest update, but this is
	# unlikely by construction).
	seek $fd 0 end
	set size [tell $fd]
	if { $size > $COUT(-seeker) } {
	    # If the file is big, jump forward towards the end of it,
	    # and read a junk line since we have no idea where we've
	    # arrived.
	    set jumper [expr int($COUT(-jumper)*$size)]
	    if { $size - $jumper > $COUT(-seeker) } {
		${log}::debug "File size 'enormous', skipping to\
                               $COUT(-seeker) from its end."
		seek $fd [expr -$COUT(-seeker)] end
	    } else {
		${log}::debug "File size greater than $COUT(-seeker) bytes,\
                               skipping first $jumper bytes of file for quicker\
                               access."
		seek $fd $jumper start
	    }
	    gets $fd;  # Skip junk line, which ensures we'll never get
		       # the header either.
	} else {
	    ${log}::debug "File size is $size bytes, parsing it all to look\
                           for latest update"
	    seek $fd 0 start; # Start from the very beginning of the file
	    # skip header, we've already parsed that information anyway...
	    if { [string is true $COUT(-header)] } {
		set hdr [gets $fd]
	    }
	}

	# Now read data stepwise and look for the one that is either
	# the latest timestamp available, as long as it's not a future
	# timestamp that would be placed ahead of current time.  This
	# means that the present algorithm is able to actually recap
	# "future" timestamps from CSV files in a number of cases.
	set now [clock seconds]
	set max 0
	set latest {}
	while { ![eof $fd] } {
	    # Get line and split
	    set line [string trim [gets $fd]]
	    if { $line ne "" } {
		set updt [__split $csvout $line]

		# Construct DESCR array with field content of the line
		for {set i 0} {$i<[llength $fields]} {incr i} {
		    set DESCR([lindex $fields $i]) [lindex $updt $i]
		}

		if { [catch {clock scan $DESCR(__timestamp) \
				 -format $COUT(-format)} when] } {
		    ${log}::warn "Could not understand $DESCR(__timestamp) as\
                                  a timestamp in format $COUT(-format)"
		} else {
		    if { $when <= $now && $when > $max } {
			set latest $updt
		    }
		}
	    }
	}

	# We have found the latest update for the object, update
	# current object with latest value, performing the proper
	# translations between how we wrote to the CSV file and how
	# data is represented in memory space.
	if { [llength $latest] > 0 } {
	    for {set i 0} {$i<[llength $fields]} {incr i} {
		set fname [lindex $fields $i]
		if { $fname ne "__timestamp" && $fname ne "__hints" } {
		    set value [lindex $latest $i]
		    set f $FMAPPER($fname)
		    if { [catch {__update $o $f $value} err] } {
			${log}::warn "Could not update field $fname in $o to\
                                      '$value': $err"
		    } else {
			lappend updated $fname
		    }
		}
	    }
	}
    }

    return $updated
}


# ::csvout::config -- Configure CSV context object.
#
#	Configure or access values for a CSV output context object.
#
# Arguments:
#	csvout	CSV context, as created by <new>
#	args	List of dash-led options with values.
#
# Results:
#	Return the current value of an option if only one option was
#	given as an argument.
#
# Side Effects:
#	None.
proc ::csvout::config { csvout args } {
    variable CSVOUT
    variable log

    if { ![::uobj::isa $csvout csvout] } {
	return -code error "$csvout unkown or wrong type"
    }
    upvar \#0 $csvout COUT

    # Save prior content
    ::uobj::inherit COUT OLD
    set result [eval ::uobj::config COUT "-*" $args]

    # For all objects in the context, see if we have setup the traces
    # on write and arrange for setting them up if they are missing.
    set traced [list]
    foreach o [$COUT(model) get objects] {
	if { [follow $csvout $o] ne "" } {
	    lappend traced $o
	}
    }
    ${log}::debug "Installed write traces on [join $traced , ]"

    return $result
}


# ::csvout::new -- Create new context for historical data storage
#
#	Create a new context for storage of object versions at within
#	a directory.  The context is bound to a model and the objects
#	that have been created within the model.  The current
#	implementation does not automatically detect new objects, but
#	this can be manually done by just calling the config
#	procedure.  The options (in args) that are accepted are the
#	following:
#       -dir     Directory (%-enclosed variables are allowed as of diskutil)
#       -flush   Time in milliseconds before actually attempting file write
#
# Arguments:
#	mdl	Identifier of a model
#	args	List of dash-led options and their values, see above.
#
# Results:
#	Return an identifier that is also a command, for Tk-style
#	access to the library.
#
# Side Effects:
#	None.
proc ::csvout::new { mdl args } {
    variable CSVOUT
    variable log

    set csvout [::uobj::new [namespace current] csvout]
    upvar \#0 $csvout COUT

    set COUT(self) $csvout;    # Ourselves
    set COUT(model) $mdl;      # The model that we are bound to

    # Listen to events on the schema to discover new objects that
    # would be created later.  This is essential to arrange for the
    # content of updates to be pushed into the database.
    ::event::bind [$mdl get schema] New \
	[list [namespace current]::__trace $csvout %i]

    ::uobj::inherit CSVOUT COUT
    ::uobj::objectify $csvout [list [list config configure] followed follow]
    
    eval config $csvout $args

    return $csvout
}

package provide csvout $::csvout::version
