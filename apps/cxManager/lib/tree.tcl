# ::tree:__find -- Find after object in tree
#
#	This is designed to be a callback procedure for tree traversal
#	and will return 1 if the object which identifier passed as an
#	argument is associated to the tree node under the association
#	"object.
#
# Arguments:
#	o	Identifier of the object to look for
#	tree	Identifier of the tree
#	node	Identifier of the node within tree
#
# Results:
#	1 if found, 0 otherwise.
#
# Side Effects:
#	None.
proc ::tree:__find { o tree node } {
    if { [$tree keys $node object] ne {} } {
	if { [$tree get $node object] eq $o } {
	    return 1
	}
    }
    return 0
}



# ::tree:construct -- Construct a tree representing an object hierarchy
#
#	Construct a tree taking into consideration all the objects of
#	the context that inherits (directly or indirectly) from a
#	given class and a "relation", i.e. a multi-field that will
#	contain instances of objects of the same class.  This tree
#	will be used as a cache in a number of other operations.  The
#	tree is constructed only if it does not exist, unless forced
#	to (see <force> below).
#
# Arguments:
#	cls	Root class to use for tree construction
#	rel	Relation (i.e. multi-field) used to express the hierarchy
#	force	Force re-construction of tree.
#
# Results:
#	An identifier for the tree.
#
# Side Effects:
#	None.
proc ::tree:construct { cls rel { force off } } {
    global CM

    # Create an identifier for the tree, an identifier that will be
    # unique across classes and relations.
    set rel [string trimleft $rel "-"]
    set tree [namespace current]:tree:[$cls get uuid]:$rel

    # Destroy tree if requested
    if { [string is true $force] } {
	catch {$tree destroy}
    }

    if { [info commands $tree] eq {} } {
	# Create tree.
	::struct::tree $tree

	# Traverse through all the objects and only take into
	# consideration those that inherits from the specified class.
	foreach o [$CM(cx) get objects] {
	    set c [::uobj::keyword $o class]
	    set classes [$c inheritance on]
	    if { [lsearch $classes $cls] >= 0 } {
		# Construct the tree, nodes are created and associated
		# to the identifier of an object, their children being
		# the nodes associated to the object identifiers that
		# are contained in the relation field.

		# Find node for object.
		set node [$tree descendants root filter \
			      [list ::tree:__find $o]]
		
		# No node, create it under the root of the tree
		if { $node eq "" } {
		    set node [$tree insert root end]
		    $tree set $node object $o
		}
		upvar \#0 $o OBJ

		# For all children, as expressed by the relation, find
		# the object and hook under the node created or found
		# above.
		foreach s $OBJ(-$rel) {
		    set snode [$tree descendants root filter \
				   [list ::tree:__find $s]]
		    if { $snode eq "" } {
			set snode [$tree insert $node end]
			$tree set $snode object $s
		    } else {
			$tree insert $node end $snode; # Reparent at end of node
		    }
		}
	    }
	}
    }

    return $tree; # Return identifier for tree, maybe the cached one.
}

proc ::tree:clean { { o "" } } {
    global CM

    if { $o eq "" } {
	foreach tree [info commands [namespace current]:tree:*] {
	    catch {$tree destroy}
	}
    } else {
	set c [::uobj::keyword $o class]
	set classes 
	foreach cls [$c inheritance on] {
	    foreach tree [info commands \
			      [namespace current]:tree:[$cls get uuid]:*] {
		catch {$tree destroy}
	    }
	}
    }
}

