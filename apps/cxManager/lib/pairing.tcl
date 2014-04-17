package require uri::urn


proc ::pair:extract { str { indices false } } {
    set ids {}
    set idx 0
    while { [regexp -indices -start $idx {%[\w|]+%} $str range] > 0 } {
	# Extract where the field specification starts and stops
	foreach {start stop} $range break

	# Extract field name itself and see if there is an index
	# specification in the field name, i.e. an integer following a
	# pipe character.
	set f [string range $str [expr $start + 1] [expr $stop - 1]]
	set i [string last "|" $f]

	# Depending on what was asked, either add the field name and
	# its index, or just the field name to the <ids> list which
	# will be returned.
	if { $i >= 0 } {
	    if { [string is false $indices] } {
		lappend ids [string range $f 0 [expr {$i - 1}]]
	    } else {
		lappend ids \
		    [string range $f 0 [expr {$i - 1}]] \
		    [string range $f [expr {$i + 1}] end]
	    }
	} else {
	    # No index specification for field, assume it is 0,
	    # i.e. the latest from the cache.
	    if { [string is false $indices] } {
		lappend ids $f
	    } else {
		lappend ids $f 0
	    }
	}

	# Advance to next specification
	set idx [expr $stop + 1]
    }

    return $ids
}


proc ::pair:receive { o r translations } {
    global CM

    upvar \#0 $o OBJ
    upvar \#0 $r REMOTE

    # Automatically push latest (i.e. current) version of remote
    # object into cache.
    ::cache:push $r

    # Remember what we will be logging about (which object we are
    # talking about).
    set uuid "local object"
    if { [array names OBJ uuid] ne "" } {
	append uuid " " $OBJ(uuid)
    }

    set touched_fields {};  # List of fields touched by translations.
    # Now perform translations, i.e. compute what the destination
    # fields in the incoming object identified by <o> will be, given
    # current values of remote object in <r> and given the fields
    # transformations described in <translations>.
    foreach {dst src} $translations {
	# Arrange for a string mapping directive describing receiving
	# fields in <o>
	set dst_fields [pair:extract $dst]
	set dst_mapper {}
	foreach f $dst_fields {
	    lappend dst_mapper %$f% ""
	}
	
	# Arrange for a string mapping directive for the values of the
	# remote fields in the transformations.  Make sure that we
	# take care of all cases, including accessing historical
	# fields.  Remember where we would fail if caching is
	# insufficient.
	set src_fields [pair:extract $src true]
	set src_mapper {}
	set skip ""
	foreach {f i} $src_fields {
	    if { $i == 0 } {
		if { [array names REMOTE -$f] ne "" } {
		    lappend src_mapper %$f% $REMOTE(-$f)
		    lappend src_mapper %${f}|0% $REMOTE(-$f)
		}
		if { [array names REMOTE $f] ne "" } {
		    lappend src_mapper %$f% $REMOTE($f)
		    lappend src_mapper %${f}|0% $REMOTE($f)
		}
	    } elseif { [string is integer $i] && $i < [::cache:versions $r] } {
		array set HIST [::cache:peek $r $i]
		if { [array names HIST -$f] ne "" } {
		    lappend src_mapper %${f}|${i}% $HIST(-$f)
		}
		if { [array names HIST $f] ne "" } {
		    lappend src_mapper %${f}|${i}% $HIST($f)
		}
	    } else {
		set skip ${f}|${i}
		break
	    }
	}

	# If we really have one destination field to receive the
	# result of the computation, perform the string mapping
	# replacement according to the map above and do the maths!
	# Arrange for string copies to work if the expression fails.
	if { $skip ne "" } {
	    $CM(log)::notice "Bad historical index or insufficient number\
                              of versions for '$skip'"
	} elseif { [llength $dst_fields] == 1 \
		       && [string trim [string map $dst_mapper $dst]] eq ""} {
	    set df [lindex $dst_fields 0]
	    $CM(log)::notice "Copying sub-content of remote object into\
                              $uuid: ${df}=${src}"
	    if { [catch {expr [string map $src_mapper $src]} val] == 0} {
		set OBJ(-$df) $val
	    } else {
		set OBJ(-$df) [string map $src_mapper $src]
	    }
	    $CM(log)::debug "Copied sub-content from remote object into\
                             $uuid: ${df} = $OBJ(-$df)"
	    lappend touched_fields $df
	}
    }

    return $touched_fields
}


proc ::pair:init { fname pachkey } {
    global CM
    
    $CM(log)::notice "Opening $fname for pairing configuration"
    if { [catch {open $fname} fd] } {
	$CM(log)::error "Could not open $fname: $fd"
	return {}
    }

    set hex "\[a-f0-9\]"
    set uuid_filter "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]"
    
    set pairs {}
    set lineno 0
    while { ![eof $fd] } {
	set line [gets $fd]
	incr lineno
	if { [string trim $line] ne "" } {
	    set first [string index $line 0]
	    if { [string first $first $CM(comments)] < 0 } {
		if { [string trimleft $line] ne $line } {
		    set equal [string first "=" $line]
		    set lft [string range $line 0 [expr {$equal-1}]]
		    set rgt [string range $line [expr {$equal+1}] end]
		    lappend PAIR(-translations) \
			[string trim $lft] [string trim $rgt]
		} else {
		    set p [::uobj::new [namespace current] pair]
		    upvar \#0 $p PAIR
		    foreach {from direction to} [string trim $line] break
		    set PAIR(-frequency) ""
		    regexp {\d+} $direction PAIR(-frequency)
		    set PAIR(id) $p
		    set PAIR(scheduler) ""
		
		    # Detect direction of pairing, we like writing an
		    # "arrow" in the file, e.g. --> but really only
		    # need the > or the < to detect the direction.
		    if { [string first ">" $direction] >= 0 } {
			set PAIR(-from) $from
			set PAIR(-to) $to
		    } else {
			set PAIR(-from) $to
			set PAIR(-to) $from
		    }
		    # Detect if we should force polling
		    if { [string first "!" $direction] >= 0 } {
			set PAIR(-polling) on
		    } else {
			set PAIR(-polling) off
		    }
		    set PAIR(-translations) {}

		    if { [string is integer $PAIR(-to)] \
			     || [string is integer $PAIR(-from)] } {
			set PAIR(-conduit) pachube
		    } elseif { [string range $PAIR(-to) 0 3] eq "gcal" \
				   || [string range $PAIR(-from) 0 3] \
				   eq "gcal"} {
			set PAIR(-conduit) gcal
		    } elseif { [string toupper [string range $PAIR(-to) 0 3]] eq "UPNP" \
				   || [string toupper [string range $PAIR(-from) 0 3]] \
				   eq "UPNP"} {
			set PAIR(-conduit) UPnP
		    } else {
			set PAIR(-conduit) remote
		    }

		    switch $PAIR(-conduit) {
			"pachube" {
			    if { [string is integer $PAIR(-to)] } {
				set PAIR(-destination) pachube
			    } else {
				set PAIR(-destination) context
			    }
			    set PAIR(-key) $pachkey
			}
			"UPnP" -
			"remote" -
			"gcal" {
			    if { [string match -nocase $uuid_filter \
				      $PAIR(-to)] } {
				set PAIR(-destination) context
			    } else {
				set PAIR(-destination) $PAIR(-conduit)
			    }
			}
		    }

		    lappend pairs $p
		}
	    }
	}
    }
    close $fd
    
    return $pairs
}


proc ::pair:register { p } {
    global CM

    upvar \#0 $p PAIR

    switch $PAIR(-conduit) {
	"pachube" {
	    if { $PAIR(-destination) eq "pachube" } {
		set feed $PAIR(-to)
		set uuid $PAIR(-from)
	    } else {
		set feed $PAIR(-from)
		set uuid $PAIR(-to)
	    }
	    set args \
		[list \
		     destination $PAIR(-destination) \
		     key $PAIR(-key) \
		     feed $feed \
		     translations $PAIR(-translations) \
		     polling $PAIR(-polling)]
	    if { $PAIR(-frequency) ne "" } {
		lappend args \
		    frequency $PAIR(-frequency)
	    }
	    ::api:pachube $uuid pair $args
	}
	"remote" {
	    if { $PAIR(-destination) eq "remote" } {
		set remote $PAIR(-to)
		set uuid $PAIR(-from)
	    } else {
		set remote $PAIR(-from)
		set uuid $PAIR(-to)
	    }
	    set args \
		[list \
		     destination $PAIR(-destination) \
		     remote $remote \
		     translations $PAIR(-translations) \
		     polling $PAIR(-polling)]
	    if { $PAIR(-frequency) ne "" } {
		lappend args \
		    frequency $PAIR(-frequency)
	    }
	    ::api:remote $uuid pair $args
	}
	"gcal" {
	    if { $PAIR(-destination) ne "context" } {
		$CM(log)::error "Cannot copy into gcal yet"
	    } else {
		if { [regexp {gcal://([a-zA-Z0-9\.\-%]+(\:[a-zA-Z0-9\.&%\$\-]+)*@)?(.*)(/)?} $PAIR(-from) match userpass pass cal] } {
		    set user [::uri::urn::unquote \
				  [lindex [split $userpass ":"] 0]]
		    set password [::uri::urn::unquote \
				      [string trimleft $pass ":"]]
		    set cal [::uri::urn::unquote \
				 [string trimright $cal "/"]]
		    set args [list \
				  user $user \
				  password $password \
				  calendar $cal]
		    if { $PAIR(-frequency) ne "" } {
			lappend args frequency $PAIR(-frequency)
		    }
		    ::api:gcal $PAIR(-to) pair $args
		} else {
		    $CM(log)::error "$PAIR(-from) is not a valid gcal URL spec"
		}
	    }
	}
	"UPnP" {
	    if { $PAIR(-destination) eq "UPnP" } {
		set UPnP $PAIR(-to)
		set uuid $PAIR(-from)
	    } else {
		set UPnP $PAIR(-from)
		set uuid $PAIR(-to)
	    }
	    # Get rid of "UPnP:" at the beginning and URI unquote to
	    # be able to support spaces.
	    set UPnP [string trimleft [string range $UPnP 4 end] ":"]
	    set UPnP [::uri::urn::unquote $UPnP]
	    set args [list \
			  device $UPnP \
			  destination $PAIR(-destination) \
			  translations $PAIR(-translations) \
			  polling $PAIR(-polling)]
	    if { $PAIR(-frequency) ne "" } {
		lappend args frequency $PAIR(-frequency)
	    }
	    ::api:UPnP $uuid pair $args
	}
    }
}
