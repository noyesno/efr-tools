package require uobj
package require ssdp
package require http
package require tax
package require uri

namespace eval ::UPnP {
    variable UPnP
    if { ![info exists UPnP] } {
	array set UPnP {
	    -timeout    10000
	    -maxwait    5
	    soap:hdr    "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<s:Envelope s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\"><s:Body><u:%action% xmlns:u=\"%serviceType%\">"
	    soap:ftr     "</u:%action%></s:Body></s:Envelope>"
	}
	variable version 0.1
	variable libdir [file dirname [file normalize [info script]]]
	::uobj::install_log UPnP UPnP
	::uobj::install_defaults UPnP UPnP
    }
}

## XX: Make variables and arguments objects via objectify?

# ::UPnP::__parseSOAP -- Parse SOAP response
#
#	This procedure is registered as a callback of the tax XML
#	parser and will fill in the remote object with the SOAP
#	answer.
#
# Arguments:
#	r	Identifier of the remote object (temporary)
#	tag	Name of tag
#	type	O or C or OC (O=open, C=close)
#	props	XML properties of tag, if any
#	bdy	Body of tag
#	tree	List of tags preceeding tag in XML tree.
#
# Results:
#	None
#
# Side Effects:
#	None
proc ::UPnP::__parseSOAP { r tag type props bdy tree } {
    variable UPnP
    variable log

    upvar \#0 $r REMOTE
    upvar \#0 $REMOTE(action) ACTION

    # Don't do anything on closing tags, since we don't have any data
    # at that time.
    if { [string first "O" $type] < 0 } {
	return
    }

    # If we are receiving one of the OUT arguments, store it in the
    # REMOTE object.
    foreach arg $ACTION(arguments) {
	upvar \#0 $arg ARGUMENT
	if { $ARGUMENT(-direction) eq "out" && $ARGUMENT(-name) eq $tag } {
	    set REMOTE(-$tag) [string trim $bdy]
	}
    }
}


# ::UPnP::__receive -- Receive result of (SOAP) call
#
#	This procedure will receive the result of the SOAP call,
#	i.e. the call that is performed when calling an action (see
#	<call>).  The command will gather results in a temporary
#	object via __parseSOAP and will, once all results have been
#	gathered either callback the command or unblock the caller.
#
# Arguments:
#	r	Identifier to the temporary remote object
#	token	HTTP token.
#
# Results:
#	None
#
# Side Effects:
#	None
proc ::UPnP::__receive { r token } {
    variable UPnP
    variable log

    if { ! [::uobj::isa $r remote] } {
	${log}::warn "$r unknown or wrong type"
	return -code error "$r unknown or wrong type"
    }
    upvar \#0 $r REMOTE
    upvar \#0 $REMOTE(action) ACTION

    if { [::http::ncode $token] == 200 } {
	set xml [::http::data $token]
	::tax::parse [::tax::new [list [namespace current]::__parseSOAP $r]] \
	    $xml
    } else {
	${log}::warn "Could not get URL: error id\
                      '[::http::error $token]' status is:\
                      '[::http::status $token]' code was:\
                      [::http::ncode $token]"
    }
    ::http::cleanup $token

    if { $REMOTE(command) eq "" } {
	set REMOTE(done) 1;   # Unblock caller
    } else {
	# Don't do callbacks if the command is a dash, which allows to
	# asynchronously call methods that do not send back any
	# results.
	if { [string trim $REMOTE(command) -] ne "" } {
	    set hdr [linsert $REMOTE(command) end $REMOTE(action)]
	    if { [catch {eval [concat $hdr [array get REMOTE -*]]} err] } {
		${log}::warn "Could not deliver callback for invocation of\
                              $ACTION(-name): $err"
	    }
	}
	::uobj::delete $r;  # Get rid of object here, we are done!
    }
}


# ::UPnP::call -- Call an action, synchronously or asynchronously
#
#	Call an action, therefor within a given service within a given
#	control point, passing further all arguments that are
#	necessary for the call.  The command will either return a
#	description of the result (synchronous call) or take a command
#	that will be called back with the result upon success.
#
# Arguments:
#	a	Identifier of the action
#	(cmd)	Command to call upon result, empty for synchronous. The
#               special command - will perform an asynchronous call but do
#               not take specific care of the result
#	args	List of dash-led options and their values.  The name of
#               these options (sans dash) should be the same as the "in"
#               arguments specified in the service description URL. 
#
# Results:
#	On synchronous calls, returns a list of dash-led options and
#	values that represent the values got from the call to the
#	action (i.e. the argument specified as "out" in the service
#	description URL.  On asynchronous calls, return the token of
#	the HTTP call.
#
# Side Effects:
#	Performs an HTTP (SOAP!) call.
proc ::UPnP::call { a args } {
    variable UPnP
    variable log

    if { ![::uobj::isa $a action] } {
	return -code error "$a unknown or wrong type"
    }

    # Either we have a command for reception, or we block until
    # reception of reply.
    set first [lindex $args 0]
    set cmd ""
    if { [string index $first 0] ne "-" \
	     || ( [string index $first 0] eq "-" \
		      && [string trim $first -] eq "" ) } {
	set cmd [lindex $args 0]
	set args [lrange $args 1 end]
    }

    upvar \#0 $a ACTION
    upvar \#0 $ACTION(service) SERVICE
    upvar \#0 $SERVICE(device) DEVICE
    upvar \#0 $DEVICE(control) CP
    upvar \#0 $CP(client) CLIENT
    
    # Prepare a mapping list to use all items from action
    set mapper [list "%action%" $ACTION(-name)]
    foreach k [array names SERVICE -*] {
	lappend mapper "%[string trimleft $k -]%" $SERVICE($k)
    }

    # Construct call based on the arguments
    set call [string map $mapper $UPnP(soap:hdr)]
    foreach {k v} $args {
	foreach arg $ACTION(arguments) {
	    upvar \#0 $arg ARGUMENT
	    parray ARGUMENT
	    if { $ARGUMENT(-direction) eq "in" \
		     && $ARGUMENT(-name) eq [string trimleft $k -] } {
		append call "<$ARGUMENT(-name)>${v}</$ARGUMENT(-name)>"
	    }
	}
    }
    append call [string map $mapper $UPnP(soap:ftr)]
    
    # Now make the call.
    set soapaction "$SERVICE(-serviceType)\#$ACTION(-name)"
    set headers [list Content-Type text/xml \
		     SoapAction $soapaction]
    set r [::uobj::new [namespace current] remote [::uobj::id $a]]
    upvar \#0 $r REMOTE
    set REMOTE(action) $a
    set REMOTE(self) $r
    set REMOTE(command) $cmd
    set REMOTE(done) 0
    set gcmd [list ::http::geturl $SERVICE(-controlURL) \
		  -query $call \
		  -method POST \
		  -command [list [namespace current]::__receive $r] \
		  -headers $headers]
    if { $CLIENT(-timeout) > 0 } {
	lappend gcmd -timeout $CLIENT(-timeout)
    }

    if { [catch {eval $gcmd} token] } {
	${log}::warn "Could not invoke action $ACTION(-name) at\
                      $SERVICE(-controlURL): $token"
	::uobj::delete $r
	return ""
    }
    # Block caller if no command was specified
    if { $cmd eq "" } {
	vwait ${r}(done)
	set retval [array get REMOTE -*]
	::uobj::delete $r
    } else {
	set retval $token
    }

    return $retval
}


# ::UPnP::__parseCP -- Control point parsing callback
#
#	This procedure is called back by the tax XML parser for each
#	tag (opening and closing) that are discovered.  It creates all
#	the necessary objects hierarchically under the control point,
#	i.e. devices and services.  Currently, it flattens any
#	hierarchy that could happen there.
#
# Arguments:
#	cp	Identifier of the control point
#	tag	Name of tag
#	type	O or C or OC (O=open, C=close)
#	props	XML properties of tag, if any
#	bdy	Body of tag
#	tree	List of tags preceeding tag in XML tree.
#
# Results:
#	None
#
# Side Effects:
#	None
proc ::UPnP::__parseCP { cp tag type props bdy tree } {
    variable UPnP
    variable log

    if { ![::uobj::isa $cp controlpoint] } {
	return -code error "$cp unknown or wrong type"
    }
    upvar \#0 $cp CP
    
    # Don't do anything on closing tags, since we don't have any data
    # at that time.
    if { [string first "O" $type] < 0 } {
	return
    }

    upvar \#0 $CP(client) CLIENT
    switch -exact $tag {
	"device" {
	    set d [::uobj::new [namespace current] device \
		       [list [::uobj::id $CLIENT(self)] [::uobj::id $cp]]]
	    upvar \#0 $d DEVICE
	    set DEVICE(self) $d
	    set DEVICE(control) $cp
	    set DEVICE(services) {}
	    ::uobj::objectify $d [list [list config configure] get find\
				      [list delete destroy]]
	    lappend CP(devices) $d
	}
	"deviceType" -
	"friendlyName" -
	"manufacturer" -
	"manufacturerURL" -
	"modelDescription" -
	"modelName" -
	"modelNumber" -
	"modelURL" -
	"serialNumber" -
	"UDN" -
	"UPC" {
	    set d [lindex $CP(devices) end]
	    upvar \#0 $d DEVICE
	    set DEVICE(-$tag) [string trim $bdy]
	    if { $tag eq "friendlyName" } {
		${log}::info "Discovered device '$DEVICE(-$tag)'"
	    }
	}
	"service" {
	    set d [lindex $CP(devices) end]
	    upvar \#0 $d DEVICE
	    set s [::uobj::new [namespace current] service \
		       [list [::uobj::id $CLIENT(self)] \
			    [::uobj::id $cp] \
			    [::uobj::id $d]]]
	    upvar \#0 $s SERVICE
	    set SERVICE(self) $s
	    set SERVICE(device) $d
	    set SERVICE(actions) {}
	    set SERVICE(variables) {}
	    ::uobj::objectify $s [list [list config configure] get find\
				      [list delete destroy]]
	    lappend DEVICE(services) $s
	}
	"serviceType" -
	"serviceId" -
	"SCPDURL" -
	"controlURL" -
	"eventSubURL" {
	    set d [lindex $CP(devices) end]
	    upvar \#0 $d DEVICE
	    set s [lindex $DEVICE(services) end]
	    upvar \#0 $s SERVICE
	    if { [string first "URL" $tag] >= 0 } {
		set SERVICE(-$tag) \
		    [::uri::resolve $CP(-location) [string trim $bdy]]
	    } else {
		set SERVICE(-$tag) [string trim $bdy]
	    }
	}
    }
}


# ::UPnP::__parseSRV -- Service parsing callback
#
#	This procedure is called back by the tax XML parser for each
#	tag (opening and closing) that are discovered.  It creates all
#	the necessary objects hierarchically under the service,
#	i.e. actions, variables, arguments.
#
# Arguments:
#	s	Identifier of the service
#	tag	Name of tag
#	type	O or C or OC (O=open, C=close)
#	props	XML properties of tag, if any
#	bdy	Body of tag
#	tree	List of tags preceeding tag in XML tree.
#
# Results:
#	None
#
# Side Effects:
#	None
proc ::UPnP::__parseSRV { s tag type props bdy tree } {
    variable UPnP
    variable log

    if { ![::uobj::isa $s service] } {
	return -code error "$s unknown or wrong type"
    }
    upvar \#0 $s SERVICE

    # Don't do anything on closing tags, since we don't have any data
    # at that time.
    if { [string first "O" $type] < 0 } {
	return
    }
    
    upvar \#0 $SERVICE(device) DEVICE
    upvar \#0 $DEVICE(control) CP
    upvar \#0 $CP(client) CLIENT
    set inheritance [list [::uobj::id $CLIENT(self)] \
			 [::uobj::id $CP(self)] \
			 [::uobj::id $DEVICE(self)] \
			 [::uobj::id $SERVICE(self)]]
    switch $tag {
	"name" {
	    set ancestor [lindex $tree end]
	    if { $ancestor eq "action" } {
		set a [::uobj::new [namespace current] action $inheritance]
		upvar \#0 $a ACTION
		
		set ACTION(self) $a
		set ACTION(service) $s
		set ACTION(-name) [string trim $bdy]
		set ACTION(arguments) {}
		::uobj::objectify $a [list [list config configure] get \
					  call [list delete destroy]]
		lappend SERVICE(actions) $a
	    } elseif { $ancestor eq "stateVariable" } {
		set v [::uobj::new [namespace current] variable $inheritance]
		upvar \#0 $v VARIABLE
		
		set VARIABLE(self) $v
		set VARIABLE(service) $s
		set VARIABLE(-name) [string trim $bdy]
		set VARIABLE(-dataType) ""
		lappend SERVICE(variables) $v
	    } elseif { $ancestor eq "argument" } {
		set action [lindex $SERVICE(actions) end]
		upvar \#0 $action ACTION
		set arg [::uobj::new [namespace current] argument \
			     [linsert $inheritance end [::uobj::id $action]]]
		upvar \#0 $arg ARGUMENT
		set ARGUMENT(self) $arg
		set ARGUMENT(action) $action
		set ARGUMENT(service) $s
		set ARGUMENT(-name) [string trim $bdy]
		set ARGUMENT(-direction) ""
		set ARGUMENT(-retval) 0
		set ARGUMENT(-relatedStateVariable) ""
		lappend ACTION(arguments) $arg
	    }
	}
	"retval" {
	    set action [lindex $SERVICE(actions) end]
	    upvar \#0 $action ACTION
	    set arg [lindex $ACTION(arguments) end]
	    upvar \#0 $arg ARGUMENT
	    set ARGUMENT(-$tag) 1
	}
	"relatedStateVariable" -
	"direction" {
	    set action [lindex $SERVICE(actions) end]
	    upvar \#0 $action ACTION
	    set arg [lindex $ACTION(arguments) end]
	    upvar \#0 $arg ARGUMENT
	    set ARGUMENT(-$tag) [string trim $bdy]
	}
	"dataType" {
	    set v [lindex $SERVICE(variables) end]
	    upvar \#0 $v VARIABLE
	    set VARIABLE(-$tag) [string trim $bdy]
	}
    }
}


# ::UPnP::__service_get -- Discover service details
#
#	This procedure arranges to get the URL that describes a
#	service and for the actions and variables that are present
#	within the service to be represented as object within this
#	library.
#
# Arguments:
#	s	Identifier of the service
#
# Results:
#	Return the token of the HTTP call
#
# Side Effects:
#	Will create actions, arguments and variables as a result of
#	the URL content parsing.
proc ::UPnP::__service_get { s } {
    variable UPnP
    variable log

    if { ![::uobj::isa $s service] } {
	return -code error "$s unknown or wrong type"
    }
    upvar \#0 $s SERVICE
    upvar \#0 $SERVICE(device) DEVICE
    upvar \#0 $DEVICE(control) CP
    upvar \#0 $CP(client) CLIENT

    ${log}::info "Getting service information for $SERVICE(-serviceId)\
                  from $SERVICE(-SCPDURL)"
    
    set cmd [list ::http::geturl $SERVICE(-SCPDURL) \
		 -command [list [namespace current]::__xml \
			       $CLIENT(self) \
			       [namespace current]::__parseSRV \
			       [namespace current]::__service \
			       $s]]
    if { $CLIENT(-timeout) > 0 } {
	lappend cmd -timeout $CLIENT(-timeout)
    }
    if { [catch {eval $cmd} token] } {
	${log}::error "Error while getting service description at\
                       $SERVICE(-SCPDURL): $token"
	set token ""
	## What do we do, remove the service object entirely??
    }

    return $token
}

 
# ::UPnP::__controlpoint -- Trigger services discovery for control point
#
#	This procedure is called back the description of a control
#	point has been parsed and understood.  It arranges for all the
#	services to be further discovered, i.e. for all the URL that
#	contains their description to be got and parsed.
#
# Arguments:
#	cp	Identifier of a control point
#
# Results:
#	None
#
# Side Effects:
#	Triggers as many URL get as their are services in the control
#	point, these are spread using the -maxwait option of the
#	client to minimise the load.
proc ::UPnP::__controlpoint { cp } {
    variable UPnP
    variable log

    if { ![::uobj::isa $cp controlpoint] } {
	return -code error "$cp unknown or wrong type"
    }
    upvar \#0 $cp CP
    upvar \#0 $CP(client) CLIENT

    foreach d $CP(devices) {
	upvar \#0 $d DEVICE
	${log}::notice "Querying all services for '$DEVICE(-friendlyName)'\
                        $DEVICE(-UDN)"
	foreach s $DEVICE(services) {
	    upvar \#0 $s SERVICE
	    set when [expr {int(rand()*$CLIENT(-maxwait))*1000}]
	    after $when [namespace current]::__service_get $s
	    ${log}::info "Getting service information for $SERVICE(-serviceId)\
                          in $when ms"
	}
    }
}


# ::UPnP::__service -- Service creation callback
#
#	Procedure to be called once a service has been called and its
#	XML desciption parsed.  At present, the procedure does little
#	less than verbosely describing the parsed service in the log.
#
# Arguments:
#	s	Identifier of the service
#
# Results:
#	None
#
# Side Effects:
#	None
proc ::UPnP::__service { s } {
    variable UPnP
    variable log

    if { ![::uobj::isa $s service] } {
	return -code error "$s unknown or wrong type"
    }
    upvar \#0 $s SERVICE

    set actions {}
    set vars {}
    foreach a $SERVICE(actions) {
	upvar \#0 $a ACTION
	lappend actions $ACTION(-name)
    }
    foreach v $SERVICE(variables) {
	upvar \#0 $v VAR
	lappend vars $VAR(-name)
    }
    ${log}::notice "Service has the following action: [join $actions ,] and\
                    the following variables [join $vars ,]"
}


# ::UPnP::__xml -- XML parsing wrapper
#
#	XML parsing wrapper designed to be called as a result of an
#	HTTP get call (from http library).  The wrapper triggers a
#	parsing command on HTTP success and arranges for a command to
#	be called once the parsing has ended.  Both commands are
#	passed the identifier of an object as a first argument, but
#	the parsing command should otherwise comply to the API from
#	the XML parser in tax.
#
# Arguments:
#	c	Identifier of the UPnP client
#	parser	Command to call for XML parsing on HTTP success
#	done	Command to call once XML parsing has ended.
#	o	Identifier of object concerned by the parsing
#	token	HTTP token
#
# Results:
#	None
#
# Side Effects:
#	None
proc ::UPnP::__xml { c parser done o token } {
    variable UPnP
    variable log

    if { ![::uobj::isa $c client] } {
	return -code error "$c unknown or wrong type"
    }
    upvar \#0 $c CLIENT

    if { [::http::ncode $token] == 200 } {
	set xml [::http::data $token]
	::tax::parse [::tax::new [list $parser $o]] $xml
	if { $done ne "" } {
	    if { [catch {eval $done $o} err] } {
		${log}::warn "Error when calling back '$done' after parsing:\
                              $err"
	    }
	}
    } else {
	${log}::warn "Could not get URL: error id\
                      '[::http::error $token]' status is:\
                      '[::http::status $token]'"
    }
    ::http::cleanup $token
}


# ::UPnP::discover -- Connect to control point for service/device discovery
#
#	Triggers the discovery of devices and services within a
#	control point by triggering the fetching of its URL
#	description and arranging to parse its XML description.
#
# Arguments:
#	cp	Identifier of control point, as returned by <add>
#
# Results:
#	Return the token of the HTTP call
#
# Side Effects:
#	Will create devices and services, as well as trigger service
#	discoveries.
proc ::UPnP::discover { cp } {
    variable UPnP
    variable log

    if { ![::uobj::isa $cp controlpoint] } {
	return -code error "$cp unknown or wrong type"
    }
    upvar \#0 $cp CP
    upvar \#0 $CP(client) CLIENT

    if { $CP(scheduler) ne "" } {
	catch {after cancel $CP(scheduler)}
	set CP(scheduler) ""
    }

    set cmd [list ::http::geturl $CP(-location) \
		 -command [list [namespace current]::__xml \
			       $CP(client) \
			       [namespace current]::__parseCP \
			       [namespace current]::__controlpoint \
			       $cp]]
    if { $CLIENT(-timeout) > 0 } {
	lappend cmd -timeout $CLIENT(-timeout)
    }
    if { [catch {eval $cmd} token] } {
	${log}::error "Error while getting control point description at\
                       $CP(-location): $token"
	::uobj::delete $cp
	set token ""
    }

    return $token
}


# ::UPnP::add -- Add remote control point by URL
#
#	Add a control point to a known UPnP client, given the URL of
#	the remote control point.  Control points that have been added
#	to the client will later on be polled for their devices and
#	services.
#
# Arguments:
#	c	Identifier of the UPnP client (as of <new>)
#	url	URL of remote control point
#	when	In how many seconds to start polling for content, negative
#               for default from client, i.e. as specified by -maxwait.
#
# Results:
#	Return the identifier of the new control point
#
# Side Effects:
#	Add control point to list of known control points, which will
#	result in future lazy polling of the services and devices that
#	the control points contains.
proc ::UPnP::add { c url { when -1 } } {
    variable UPnP
    variable log

    if { ![::uobj::isa $c client] } {
	return -code error "$c unknown or wrong type"
    }
    upvar \#0 $c CLIENT

    set control [::uobj::new [namespace current] controlpoint \
		     [::uobj::id $c]]
    upvar \#0 $control CP
    set CP(self) $control
    set CP(client) $c
    set CP(devices) {}
    set CP(scheduler) ""
    set CP(-location) $url
    ::uobj::objectify $control [list [list config configure] get find \
				    [list delete destroy] discover]

    if { $when < 0 } {
	set when [expr {int(rand()*$CLIENT(-maxwait))*1000}]
	${log}::info "Getting control point information for $CP(-location)\
                      in $when ms"
	set CP(scheduler) [after $when [namespace current]::discover $control]
    }

    return $control
}


# ::UPnP::__discover -- Auto-discovery of control points
#
#	Automatically discover control point as a result of the
#	binding onto the SSDP listener.  Discovered (and new) control
#	points will be polled for their services and devices in a lazy
#	fashion, i.e. as controlled by the -maxwait option of the UPnP
#	client object.
#
# Arguments:
#	c	Identifier of the client (as returned by <new>)
#	cp	Identifier of the control point (as of SSDP module)
#	d	Identifier of the device within the SSDP module
#	type	SSDP notification type.
#
# Results:
#	Return identifier of the control point
#
# Side Effects:
#	Add control point to list of known control points, which will
#	result in future lazy polling of the services and devices that
#	the control points contains.
proc ::UPnP::__discover { c cp d type } {
    variable UPnP
    variable log

    if { ![::uobj::isa $c client] } {
	return -code error "$c unknown or wrong type"
    }
    upvar \#0 $c CLIENT

    set url [$d config -location]
    set control [::uobj::find [namespace current] controlpoint \
		     [list -location == $url] [::uobj::id $c]]
    if { $control eq "" } {
	${log}::notice "Discovered new control point at $url"
	set control [add $c $url]
    }

    return $control
}


# ::UPnP::find -- Search for matching objects
#
#	Search for objects which name match a given pattern under a
#	given object.  Only variables, actions and devices can be
#	searched for.
#
# Arguments:
#	o	Identifier of the top object at which to start search
#	what	What to search for, can be "device", "action", "variable"
#	ptn	Pattern to match against
#
# Results:
#	Return the list of objects which name match the pattern, empty
#	if none matched.
#
# Side Effects:
#	None
proc ::UPnP::find { o what ptn } {
    variable UPnP
    variable log

    if { ![::uobj::isa $o [list client controlpoint device service]] } {
	return -code error "$o unknown or wrong type"
    }
    switch -glob -- $what {
	v* -
	a* {
	    set res {}
	    foreach a [$o get $what] {
		if { [string match $ptn [$a config -name]] } {
		    lappend res $a
		}
	    }
	    return $res
	}
	d* {
	    if { [lsearch [list client controlpoint] [::uobj::type $o]] >= 0 } {
		set res {}
		foreach a [$o get $what] {
		    if { [string match $ptn [$a config -friendlyName]] } {
			lappend res $a
		    }
		}
		return $res
	    }
	}
    }
}


# ::UPnP::dump -- Dump object's data
#
#	This procedure, not exported as a command to objects on
#	purpose, can be used to recursively dump all information known
#	about an object.  The procedure is meant for debugging
#	purposes.
#
# Arguments:
#	o	Identifier of the object to dump information for.
#	chan	Channel to dump information to
#	indent	Indentation level (will increase with recursion)
#
# Results:
#	None
#
# Side Effects:
#	Writes to the file descriptor passed as an argument.
proc ::UPnP::dump { o { chan stdout } { indent 0 } } {
    variable UPnP
    variable log

    if { ![::uobj::isa $o [list client controlpoint device service action \
			      variable argument]] } {
	return -code error "$o unknown or wrong type"
    }

    set lead [string repeat " " $indent]
    switch [::uobj::type $o] {
	client {
	    upvar \#0 $o CLIENT
	    puts "${lead}Port:\t[$CLIENT(cp) config -port]"
	    puts "${lead}Mcast:\t[$CLIENT(cp) config -mcast]"
	    puts "${lead}Control Points:"
	    incr indent 2
	    foreach c [$o get controlpoints] {
		dump $c $chan $indent
	    }
	}
	controlpoint {
	    upvar \#0 $o CP
	    foreach k [array names DEVICE -*] {
		puts "${lead}[string trimleft $k -]:\t$CP($k)"
	    }
	    puts "${lead}Devices:"
	    incr indent 2
	    foreach d [$o get devices] {
		dump $d $chan $indent
	    }
	}
	device {
	    upvar \#0 $o DEVICE
	    foreach k [array names DEVICE -*] {
		puts "${lead}[string trimleft $k -]:\t$DEVICE($k)"
	    }
	    puts "${lead}Services:"
	    incr indent 2
	    foreach d [$o get services] {
		dump $d $chan $indent
	    }
	}
	service {
	    upvar \#0 $o SERVICE
	    foreach k [array names SERVICE -*] {
		puts "${lead}[string trimleft $k -]:\t$SERVICE($k)"
	    }
	    puts "${lead}Variables:"
	    foreach d [$o get variables] {
		dump $d $chan [expr {2+$indent}]
	    }
	    puts "${lead}Actions:"
	    foreach d [$o get actions] {
		dump $d $chan [expr {2+$indent}]
	    }
	}
	variable {
	    upvar \#0 $o VARIABLE
	    puts "${lead}Variable:"
	    incr indent 2
	    set lead [string repeat " " $indent]
	    foreach k [array names VARIABLE -*] {
		puts "${lead}[string trimleft $k -]:\t$VARIABLE($k)"
	    }
	}
	action {
	    upvar \#0 $o ACTION
	    foreach k [array names ACTION -*] {
		puts "${lead}[string trimleft $k -]:\t$ACTION($k)"
	    }
	    puts "${lead}Arguments:"
	    incr indent 2
	    foreach d [$o get arguments] {
		dump $d $chan $indent
	    }
	}
	argument {
	    upvar \#0 $o ARGUMENT
	    puts "${lead}Argument:"
	    incr indent 2
	    set lead [string repeat " " $indent]
	    foreach k [array names ARGUMENT -*] {
		puts "${lead}[string trimleft $k -]:\t$ARGUMENT($k)"
	    }
	}
    }
}

# ::UPnP::delete -- Delete an object (and its descendants) 
#
#	Delete an objects and all objects that might have been
#	recursively created from the object.  For example, deleting a
#	control point will delete all devices and services under the
#	control point, including all actions, variables and arguments.
#	Note that since we do SSDP discovery, control points that are
#	removed will reappear, though with a new and different
#	identifier.
#
# Arguments:
#	o	Identifier of the object
#
# Results:
#	None
#
# Side Effects:
#	Recursively deletes all under objects.
proc ::UPnP::delete { o } {
    variable UPnP
    variable log

    if { ![::uobj::isa $o [list client controlpoint device service action \
			      variable argument]] } {
	return -code error "$o unknown or wrong type"
    }

    switch [::uobj::type $o] {
	client {
	    upvar \#0 $o CLIENT
	    foreach c [get $o controlpoints] {
		delete $c
	    }
	    ::uobj::delete $o
	}
	controlpoint {
	    upvar \#0 $o CP
	    foreach d $DEVICE(devices) {
		delete $d
	    }
	    ::uobj::delete $o
	}
	device {
	    upvar \#0 $o DEVICE
	    upvar \#0 $DEVICE(control) CP
	    set idx [lsearch $CP(devices) $o]
	    if { $idx >= 0 } {
		set CP(devices) [lreplace $CP(devices) $idx $idx]
	    }
	    foreach s $DEVICE(services) {
		delete $s
	    }
	    ::uobj::delete $o
	}
	service {
	    upvar \#0 $o SERVICE
	    upvar \#0 $SERVICE(device) DEVICE
	    set idx [lsearch $DEVICE(services) $o]
	    if { $idx >= 0 } {
		set DEVICE(services) [lreplace $DEVICE(services) $idx $idx]
	    }
	    foreach a $SERVICE(actions) {
		delete $a
	    }
	    foreach v $SERVICE(variables) {
		delete $v
	    }
	    ::uobj::delete $o
	}
	action {
	    upvar \#0 $o ACTION
	    upvar \#0 $ACTION(service) SERVICE
	    set idx [lsearch $SERVICE(actions) $o]
	    if { $idx >= 0 } {
		set SERVICE(actions) [lreplace $SERVICE(actions) $idx $idx]
	    }
	    # No recursion here, unecessary
	    foreach a $ACTION(arguments) {
		::uobj::delete $a
	    }
	    ::uobj::delete $o
	}
	variable {
	    upvar \#0 $o VARIABLE
	    upvar \#0 $VARIABLE(service) SERVICE
	    set idx [lsearch $SERVICE(variables) $o]
	    if { $idx >= 0 } {
		set SERVICE(variables) [lreplace $SERVICE(variables) $idx $idx]
	    }
	    ::uobj::delete $o
	}
	argument {
	    upvar \#0 $o ARGUMENT
	    upvar \#0 $ARGUMENT(action) ACTION
	    set idx [lsearch $ACTION(arguments) $o]
	    if { $idx >= 0 } {
		set ACTION(arguments) [lreplace $ACTION(arguments) $idx $idx]
	    }
	    ::uobj::delete $o
	}
    }
}


# ::UPnP::get -- Get (semi-)internal details for objects
#
#	This procedure will return (semi-)internal details for the
#	objects that are created via this library.  Any dash-led
#	options is blindly passed to the <config> command for the
#	object.  All remaining types of information depend on the type
#	of the object, and can be abbreviated to their minima.  These
#	are (recursive most of the time):
#
#	For clients:
#       controlpoints Return the list of discovered control points
#       services      Return the list of discovered services
#       devices       Return the list of discovered devices
#       variables     Return the list of discovered variables
#       actions       Return the list of discovered actions
#
#	For clients:
#       controlpoints Return the list of discovered control points
#       services      Return the list of discovered services
#       devices       Return the list of discovered devices
#       variables     Return the list of discovered variables
#       actions       Return the list of discovered actions
#
#	For clients:
#       controlpoints Return the list of discovered control points
#       services      Return the list of discovered services
#       devices       Return the list of discovered devices
#       variables     Return the list of discovered variables
#       actions       Return the list of discovered actions
#
#	For control points:
#       services      Return the list of discovered services
#       devices       Return the list of discovered devices
#       variables     Return the list of discovered variables
#       actions       Return the list of discovered actions
#
#	For devices:
#       services      Return the list of discovered services
#
#	For services:
#       variables     Return the list of discovered variables
#       actions       Return the list of discovered actions
#       
#	For actions:
#       arguments     Return the list of discovered arguments
#       
#
# Arguments:
#	o	Identifier of the object
#	what	Type of the information to retrieve (see above)
#
# Results:
#	Return the information, or an empty string.
#
# Side Effects:
#	None
proc ::UPnP::get { o what } {
    variable UPnP
    variable log

    if { ![::uobj::isa $o [list client controlpoint device service action]] } {
	return -code error "$o unknown or wrong type"
    }

    switch [::uobj::type $o] {
	"client" {
	    switch -glob -- $what {
		c* {
		    return [::uobj::allof [namespace current] controlpoint \
				[::uobj::id $o]]
		}
		s* -
		d* -
		v* -
		a* {
		    set objs {}
		    foreach c [get $o controlpoints] {
			set objs [concat $objs [get $c $what]]
		    }
		    return $objs
		}
		-* {
		    return [config $o $what]
		}
	    }
	}
	"controlpoint" {
	    upvar \#0 $o CP
	    switch -glob -- $what {
		d* {
		    return $CP(devices)
		}
		s* {
		    set services {}
		    foreach d $CP(devices) {
			set services [concat $services [get $d services]]
		    }
		    return $services
		}
		a* {
		    set actions {}
		    foreach d $CP(devices) {
			foreach s [get $d services] {
			    set actions [concat $actions [get $s actions]]
			}
		    }
		    return $actions
		}
		v* {
		    set vars {}
		    foreach d $CP(devices) {
			foreach s [get $d variables] {
			    set vars [concat $vars [get $s actions]]
			}
		    }
		    return $vars
		}
		-* {
		    return [config $o $what]
		}
	    }
	}
	"device" {
	    upvar \#0 $o DEVICE
	    switch -glob -- $what {
		s* {
		    return $DEVICE(services)
		}
		-* {
		    return [config $o $what]
		}
	    }
	}
	"service" {
	    upvar \#0 $o SERVICE
	    switch -glob -- $what {
		a* {
		    return $SERVICE(actions)
		}
		v* {
		    return $SERVICE(variables)
		}
		-* {
		    return [config $o $what]
		}
	    }
	}
	"action" {
	    upvar \#0 $o ACTION
	    switch -glob -- $what {
		a* {
		    return $ACTION(arguments)
		}
		-* {
		    return [config $o $what]
		}
	    }
	}
    }

    return ""
}


# ::UPnP::config -- (re)configure an object
#
#	This procedure either (re)configure an object or returns the
#	current value of a configuration variable.  Only UPnP client
#	objects can be reconfigured, but all other objects created via
#	the library can be requested for their configuration values.
#
# Arguments:
#	o	Identifier of the object
#	args	Aither dash-led options and values or one dash-led option
#
# Results:
#	Return the current value of an option, whenever relevant.
#
# Side Effects:
#	None
proc ::UPnP::config { o args } {
    variable UPnP
    variable log

    if { ![::uobj::isa $o [list client controlpoint device service action]] } {
	return -code error "$o unknown or wrong type"
    }
    upvar \#0 $o OBJ

    # Save prior content
    ::uobj::inherit OBJ OLD
    set result [eval ::uobj::config OBJ "-*" $args]

    switch [::uobj::type $o] {
	"client" {
	    upvar \#0 $o CLIENT
	    if { ! $CLIENT(bound) && $CLIENT(cp) ne "" } {
		$CLIENT(cp) register [list [namespace current]::__discover $o] \
		    "urn:*:service:*"
		set CLIENT(bound) 1
	    }
	}
    }

    return $result
}


# ::UPnP::new -- Create a new UPnP client
#
#	This procedure will create a new UPnP client. It will register
#	a callback into an SSDP listener and will automatically
#	inquire for the UPnP devices and services that are available
#	on the network.  Available actions within services will be
#	represented with constructs that encapsulate all necessary
#	code to call the method and analyse returned answers.
#
#       The procedure returns an identifier for the client object.
#       This identifier is also a command with which all further
#       operations on the library can be called.  The command accepts
#       a number of dash led options, all of which can be further
#       changed using the configure command of the returned object.
#       These options are:
#       -timeout   Number of millseconds before timing out on web calls.
#       -maxwait   Max number of seconds to wait before requesting services'
#                  descriptions (used to reduce load on the services).
#
# Arguments:
#	cp	Identifier of an SSDP listener, can be empty to disable
#               auto-discovery of control points.
#	args	List of dash-led options and their values, see above.
#
# Results:
#	The identifier of an object that is also a command with which
#	all further operations should be performed.
#
# Side Effects:
#	Will start polling remote services for their capabilities, as
#	soon as they are discovered.
proc ::UPnP::new { cp args } {
    variable UPnP
    variable log

    if { ![::uobj::isa $cp controlpoint] } {
	return -code error "Need an SSDP control point connection"
    }

    set c [::uobj::new [namespace current] client]
    upvar \#0 $c CLIENT

    set CLIENT(self) $c;      # Ourselves
    set CLIENT(cp) $cp;       # SSDP control point connection
    set CLIENT(bound) 0;      # Have we bound?

    ::uobj::inherit UPnP CLIENT
    ::uobj::objectify $c [list [list config configure] get add find\
			      [list delete destroy]]

    eval config $c $args

    return $c
}

# Provide the version last to make sure we catch errors whenever we
# generate the index of the package.
package provide UPnP $::UPnP::version