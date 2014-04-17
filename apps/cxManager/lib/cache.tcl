# This array will contain lists of versions for the objects that are
# placed in the cache.  Indices for the array is object dependent and
# decided by the ::cache:id procedure.
array set __Cache {}

proc ::cache:id { o } {
    global CM

    # Look for possibly associated identifiers.
    foreach id [list "cacheid" "id" "uuid"] {
	if { [::uobj::keyword $o $id] ne "" } {
	    return [::uobj::keyword $o $id]
	}
    }

    # Look for "good" members uniquely identifying the object.
    upvar \#0 $o OBJ
    foreach id [list "uuid" "id"] {
	if { [llength [array names OBJ $id]] > 0 } {
	    return $OBJ($id)
	}
    }

    # Revert to the identifier of the object, which is probably a
    # bad'ish id, but the only thing that we have at this point.
    return $o
}


proc ::cache:push { o } {
    global CM
    global __Cache

    if { $CM(cache) <= 0 } {
	return 
    }
    set id [::cache:id $o]
    upvar \#0 $o OBJ
    if { [llength [array names __Cache $id]] > 0 } {
	set __Cache($id) [lrange [linsert $__Cache($id) 0 [array get OBJ]] \
			      0 [expr {$CM(cache)-1}]]
    } else {
	set __Cache($id) [list [array get OBJ]]
    }
}


proc ::cache:peek { o { idx 0 } } {
    global CM
    global __Cache

    if { $CM(cache) <= 0 } {
	return {}
    }
    if { $idx < 0 || $idx >= $CM(cache) } {
	return {}
    }
    set id [::cache:id $o]
    if { [llength [array names __Cache $id]] > 0 } {
	return [lindex $__Cache($id) $idx]
    } else {
	return {}
    }
}


proc ::cache:versions { o } {
    global CM
    global __Cache

    if { $CM(cache) <= 0 } {
	return 0
    }
    set id [::cache:id $o]
    if { [llength [array names __Cache $id]] > 0 } {
	return [llength $__Cache($id)]
    } else {
	return 0
    }
}


proc ::cache:empty { { objs {} } } {
    global CM
    global __Cache

    if { [llength $objs] == 0 } {
	unset __Cache
	array set __Cache {}
    }

    foreach o $objs {
	set id [::cache:id $o]
	unset __Cache($id)
    }
}
