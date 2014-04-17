#package require udp
package require uobj

namespace eval ::ssdp {
    variable SSDP
    if { ![info exists SSDP] } {
	array set SSDP {
	    -agent        ""
	    -frequency    180
	    -port         1900
	    -mcast        239.255.255.250
	    -delay        3
	    -ttl          2
	    udp           ""
	}
	variable version 0.2
	variable libdir [file dirname [file normalize [info script]]]
	::uobj::install_log ssdp SSDP
	::uobj::install_defaults ssdp SSDP
    }
}



# ::ssdp::__vanish -- Automatic device object expiration
#
#	Implement expiration after the respit time, as specified by
#	the ssdp:alive message.  Device object will simply disapear.
#
# Arguments:
#	d	Identifier of the device.
#
# Results:
#	None
#
# Side Effects:
#	None
proc ::ssdp::__vanish { d } {
    variable SSDP
    variable log

    if { ![::uobj::isa $d device] } {
	return -code error "$d unknown or wrong type"
    }
    upvar \#0 $d DEVICE

    ::uobj::delete $d
}


# ::ssdp::__receive -- Receive SSDP messages and handle
#
#	This procedure forms the core of the library. It receives UDP
#	messages fand analyse them to re-route them to registered
#	callbacks, but also to keep count of existing devices and
#	services on the network.
#
# Arguments:
#	cp	Identifier of the SSDP listener returned by <new>
#
# Results:
#	None
#
# Side Effects:
#	None
proc ::ssdp::__receive { cp } {
    variable SSDP
    variable log

    if { ![::uobj::isa $cp controlpoint] } {
	return -code error "$cp unknown or wrong type"
    }
    upvar \#0 $cp CP

    if { $CP(sock) eq "" } {
	${log}::warn "No socket to receive data on!"
	return
    }

    # Read from socket and discover who is this coming from
    set message [read $CP(sock)]
    set peer [fconfigure $CP(sock) -peer]
    if { [string trim $message] eq "" } return

    # Split message to ease parsing and analyse tag line
    set message [split $message \n]
    foreach {method url version} [lindex $message 0] break

    # Fill in the list <headers> with some easy digest of the content
    # of the HTTPU header.
    set headers {}
    foreach line [lreplace $message 0 0] {
	if { $line eq "" } break
	regexp {^([^:]+):(.*)$} $line all header value
	lappend headers [string tolower $header] [string trim $value]
    }

    #puts "==> MTH:$method URL:$url VRS:$version HDRS:$headers"

    # React depending on the method.
    switch -exact -- $method {
	"M-SEARCH" {
	}
	"HTTP/1.1" -
	"NOTIFY" {
	    array set MSG $headers
	    # Only do something on the ssdp:alive which contains the
	    # announcements. Maybe should we react more and have a
	    # chance to discover services and devices earlier on
	    # instead.
	    if { [array names MSG usn] ne "" } {
		# Make sure we have all details for announcements.
		foreach k [list cache-control] {
		    if { [array names MSG $k] eq "" } {
			${log}::warn "No '$k' available in announce,\
                                      cannot parse"
			return 
		    }
		}

		# Extract notification type (if any) and UUID from the
		# composite identifier of the advertisement.
		set nt ""
		set dcolon [string first "::" $MSG(usn)]
		if { $dcolon >= 0 } {
		    set nt \
			[string trim [string range $MSG(usn) $dcolon end] :]
		    set MSG(uuid) \
			[string trim [string range $MSG(usn) 0 $dcolon] :]
		} else {
		    set MSG(uuid) $MSG(usn)
		}
		set MSG(uuid) [regsub {^uuid:} $MSG(uuid) ""]

		# Create or update the device object, now that we know
		# its UUID
		set d [::uobj::find [namespace current] device \
			   [list -uuid == $MSG(uuid)] [::uobj::id $cp]]
		if { $d eq "" } {
		    set d [::uobj::new [namespace current] device \
			       [::uobj::id $cp]]
		    upvar \#0 $d DEVICE
		    set DEVICE(self) $d
		    set DEVICE(vanish) ""
		    set DEVICE(devices) {}
		    set DEVICE(services) {}
		    ${log}::notice "Discovered new UPnP root device: $MSG(uuid)"
		    ::uobj::objectify $d [list [list config configure]]
		} else {
		    upvar \#0 $d DEVICE
		}

		# Copy interesting data into device object.
		foreach k {location uuid server} {
		    if { [array names MSG $k] ne "" } {
			if { $MSG($k) ne "" || [array names DEVICE -$k] == ""} {
			    set DEVICE(-$k) $MSG($k)
			}
		    }
		}
		if { [string first ":device:" $nt] >= 0 } {
		    lappend DEVICE(devices) $nt
		    set DEVICE(devices) [lsort -unique $DEVICE(devices)]
		}
		if { [string first ":service:" $nt] >= 0 } {
		    lappend DEVICE(services) $nt
		    set DEVICE(services) [lsort -unique $DEVICE(services)]
		}

		# Implement automatic expiration of object.  We should
		# really start listening to byebye messages to make
		# the implementation complete.
		if { $DEVICE(vanish) ne "" } {
		    after cancel $DEVICE(vanish)
		    set DEVICE(vanish) ""
		}
		foreach {mage age} [split $MSG(cache-control) "="] break
		set next [expr {[string trim $age]*1000}]
		set DEVICE(vanish) \
		    [after $next [namespace current]::__vanish $d]
		#${log}::debug "Auto-removal of $MSG(uuid) in $age seconds"

		# Deliver callback for the notification type that has
		# just arrived.
		foreach {ptn cb} $CP(alive) {
		    if { [string match $ptn $nt] } {
			if { [catch {eval $cb $cp $d "$nt"} err] } {
			    ${log}::warn "Could not deliver callback $cb: $err"
			}
		    }
		}
	    }
	}
	default {
	    ${log}::warn "Received $method, which is not implemented"
	}
    }
}


# ::ssdp::__search -- Issue a search for devices
#
#	This procedure issues a search for devices and services on the
#	multicast group that is associated to this SSDP listener.  The
#	search will lead to notifications that are accounted for in
#	<__receive>
#
# Arguments:
#	cp	Identifier of the SSDP listener returned by <new>
#
# Results:
#	None
#
# Side Effects:
#	None
proc ::ssdp::__search { cp } {
    variable SSDP
    variable log

    if { ![::uobj::isa $cp controlpoint] } {
	return -code error "$cp unknown or wrong type"
    }
    upvar \#0 $cp CP

    # XX: While this is "supposed" to work, I have actually never been
    # able to witness any unicast answers on the socket. This is OK,
    # since all services are required to multicast a notify from time
    # to time, but it is much slower than it should be to discover
    # services.
    set search "M-SEARCH * HTTP/1.1
Host:$CP(-mcast):$CP(-port)
Man:\"ssdp:discover\"
MX:$SSDP(-delay)
ST:ssdp:all
User-Agent:$CP(-agent)

"
    if { $CP(sock) ne "" } {
	puts $CP(sock) $search
	flush $CP(sock)
    }
    
    set next [expr {int($CP(-frequency)*1000)}]
    set CP(searcher) [after $next [namespace current]::__search $cp]
}


# ::ssdp::devices -- Return all known devices
#
#	Returns all devices that have been discovered so far,
#	independantly if their discovery has led to a callback via the
#	callback mechanism described in <register> or not.
#
# Arguments:
#	cp	Identifier of the SSDP listener returned by <new>
#
# Results:
#	Return a list of devices, these are objects as described in
#	<register>
#
# Side Effects:
#	None
proc ::ssdp::devices { cp } {
    variable SSDP
    variable log

    if { ![::uobj::isa $cp controlpoint] } {
	return -code error "$cp unknown or wrong type"
    }
    return [::uobj::allof [namespace current] device [::uobj::id $cp]]
}


# ::ssdp::register -- Register a command for service discovery
#
#	This procedure registers a command that will be called each
#	time a service/device matching the pattern is discovered.
#	Relevant patterns, except * that matches all announcements are
#	urn:*:service:* or urn:*:device:*.  The command will be called
#	back with the following arguments, in order:
#
#       The identifier of the SSDP listener (as of <new>)
#       The identifier of a device object (see below)
#       The type of the notification that matched
#
#       The identifier of the device that was found is an object
#       (similar to the one returned by <new> which only has a
#       command, configure, and which has the following options:
#       -location -usn and -server.
#
# Arguments:
#	cp	Identifier of the SSDP listener
#	cmd	Command to callback on matches
#	ptn	Pattern of notification types to match.
#
# Results:
#	None
#
# Side Effects:
#	None
proc ::ssdp::register { cp cmd { ptn "*" } } {
    variable SSDP
    variable log

    if { ![::uobj::isa $cp controlpoint] } {
	return -code error "$cp unknown or wrong type"
    }
    upvar \#0 $cp CP
    lappend CP(alive) $ptn $cmd
}


# ::ssdp::config -- (re)configure an SSDP listener
#
#	This procedure either (re)configure an SSDP listener or
#	returns the current value of a configuration variable.  When
#	called with dash-led options with values, it will
#	(re)configure the listener.  See <new> for the list of valid
#	options and their meaning.  When called with a single dash-led
#	option, it will return the current value of that option.
#
# Arguments:
#	See above
#
# Results:
#	Return the current value if relevant
#
# Side Effects:
#	Start/stop listening to multicast and searching for existing
#	devices, depending on the options.
proc ::ssdp::config { o args } {
    variable SSDP
    variable log
    variable version
    global tcl_platform

    if { ![::uobj::isa $o [list "controlpoint" "device"]] } {
	return -code error "$o unknown or wrong type"
    }
    upvar \#0 $o OBJ

    # Save prior content
    ::uobj::inherit OBJ OLD
    set result [eval ::uobj::config OBJ "-*" $args]

    switch [::uobj::type $o] {
	"controlpoint" {
	    # Close old socket if port number has changed or another multicast
	    # group.
	    if { $OBJ(sock) ne "" \
		     && ( $OLD(-port) != $OBJ(-port) \
			      || $OLD(-mcast) ne $OBJ(-mcast)) } {
		close $OBJ(sock)
		set OBJ(sock) ""
	    }
	    
	    if { $OBJ(sock) eq "" && $SSDP(udp) ne "" } {
		if { [catch {udp_open $OBJ(-port) reuse} sock] } {
		    ${log}::error "Could not bind to SSDP port\
                                   $OBJ(-port)!: $sock"
		} else {
		    set OBJ(sock) $sock
		    fconfigure $OBJ(sock) -translation crlf -buffering none
		    if { [catch {fconfigure $OBJ(sock) \
				     -mcastadd $OBJ(-mcast) \
				     -ttl $SSDP(-ttl) \
				     -remote [list $OBJ(-mcast) $OBJ(-port)]} \
			      err] } {
			# Discovered while running without network
			${log}::error "Could not listen to multicast group\
                                   $OBJ(-mcast): $err"
		    } else {
			fileevent $OBJ(sock) readable \
			    [list [namespace current]::__receive $o]
		    }
		}
	    }

	    # Make up a proper agent string if we had an empty one,
	    # this does not use the platform package to make sure the
	    # code can work on older versions of Tcl.
	    if { $OBJ(-agent) eq "" } {
		set OBJ(-agent) \
		    "$tcl_platform(os)/$tcl_platform(osVersion) UPnP/1.1"
		append OBJ(-agent) " " "Tcl/$version"
	    }

	    # Turn on active search for existing services on the network
	    if { $OBJ(searcher) eq "" && $OBJ(-frequency) > 0 \
		     && $SSDP(udp) ne "" } {
		set OBJ(searcher) [after idle [namespace current]::__search $o]
	    }
	    # Turn off active search when frequency is negative.
	    if { $OBJ(searcher) ne "" && $OBJ(-frequency) <= 0 } {
		after cancel $OBJ(searcher)
	    }
	}
    }

    return $result
}


# ::ssdp::new -- Create new SSDP listener
#
#	This procedure creates a new SSDP listener object.  This will
#	result in the caller to start listening for control point
#	annouces on the multicast SSDP multicast group.  The command
#	return an object which also is a command with which all
#	further operations with the library can be performed.  The
#	command accepts a number of dash led options, all of which can
#	be further changed using the configure command of the returned
#	object. These options are:
#	-mcast     The multicast group to listen on, defaults to standard.
#	-port      The port number to listen on, defaults to standard.
#	-agent     User agent, leave empty for good default
#	-frequency Frequency for service discoveries, in seconds, negative
#                  to turn off.
#
# Arguments:
#	A series of dash-led options, see above.
#
# Results:
#	The identifier of an object that is also a command with which
#	all further operations should be performed.
#
# Side Effects:
#	Will start listening for service announcements on the
#	multicast group and will actively request for existing
#	services on the network.
proc ::ssdp::new { args } {
    variable SSDP
    variable log

    # Lazy load of udp package.
    if { $SSDP(udp) eq "" } {
	if { [catch {package require udp} ver] } {
	    ${log}::error "Could not load UDP package, cannot discover\
                           services: $ver"
	} else {
	    set SSDP(udp) $ver
	}
    }

    set cp [::uobj::new [namespace current] controlpoint]
    upvar \#0 $cp CP

    set CP(self) $cp;     # Ourselves
    set CP(searcher) "";  # Search output
    set CP(sock) "";      # UDP socket
    set CP(alive) {};     # Callbacks for alive services.

    ::uobj::inherit SSDP CP
    ::uobj::objectify $cp [list [list config configure] devices register]

    eval config $cp $args

    return $cp
}

package provide ssdp $::ssdp::version