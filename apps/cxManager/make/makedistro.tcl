# makedistro.tcl -- Distribution ZIP file maker
#
#	Script to automate the generation of a ZIP file that contains
#	all the files necessary for the viewr application bundle.
#
# Copyright (c) 2004-2006 by the Swedish Institute of Computer Science.
#
# See the file 'license.terms' for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
array set MD {
    complevel      9
    implementation mkzip
}

set topdir [file normalize [file dirname [info script]]]

# Source bootstrapping routine.
source [file join $topdir .. lib bootstrap.tcl]
::argutil::accesslib tcllib

# Now parse the options
package require cmdline

set options {
    { verbose.alpha "info" "Verbosity Level" }
    { rootdir.arg  "nightpatrol" "Name of root directory in distribution file" }
    { zip.arg "nightpatrol.zip" "Name of the ZIP file that we generate" }
}

set inited [::argutil::initargs MD $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}

array set MD $optlist
foreach key $inited {
    ::argutil::makelist MD($key)
}
    
# Initialise local logging facility
package require logger
set MD(log) [::logger::init makedistro]
$MD(log)::setlevel $MD(verbose)
::argutil::fix_outlog

if { ! [info exists ::starkit::topdir] } {
    ::argutil::accesslib lib
}
if { $MD(implementation) eq "zipper" } {
    ::argutil::accesslib critlib
} else {
    ::argutil::accesslib mkZiplib
}
::argutil::fix_outlog

if { $MD(implementation) eq "zipper" } {
    package require zlib
    package require zipper
} else {
    package require mkZiplib
}


##################
## Module Name     -- zip_
## Original Author --  Emmanuel Frécon - emmanuel.frecon@myjoice.com
## Description:
##
##      Emulation layer to write ZIP files using either the zipper or
##      the mkZiplib extension.
##
## Commands Exported:
##      zip_open
##      zip_copy
##      zip_close
##################

proc zip_open { fpath } {
    global MD

    if { $MD(implementation) eq "zipper" } {
	set zfd [open $fpath w]
	::zipper::initialize $zfd
	return $zfd
    } else {
	set zfd [zip open $fpath w]
	return $zfd
    }
}

proc zip_copy { zfd src dst } {
    global MD

    set fd [open $src]
    fconfigure $fd -translation binary -encoding binary
    if { $MD(implementation) eq "zipper" } {
	::zipper::addentry $dst [read $fd] [file mtime $src]
    } else {
	zip set $zfd $dst -level $MD(complevel) -time [file mtime $src]
	zip write $zfd [read $fd]
    }
    close $fd
}

proc zip_close { zfd } {
    global MD

    if { $MD(implementation) eq "zipper" } {
	close [::zipper::finalize]
    } else {
	zip close $zfd
    }
}

##############  Real stuff starts here...

$MD(log)::notice "Creating distribution archive $MD(zip), root directory\
                  $MD(rootdir)"
set zfd [zip_open $MD(zip)]

proc addfile { zfd fname } {
    global MD

    if { [file isdirectory $fname] } {
	foreach f [glob -directory $fname -nocomplain -- *] {
	    addfile $zfd $f
	}
    } else {
	zip_copy $zfd $fname [file join $MD(rootdir) $fname]
    }
}

foreach fname [list powerwarn.cfg  \
		   powerwarn.exe powerwarn.vsn \
		   pics] {
    $MD(log)::info "Adding ${fname}..."
    addfile $zfd $fname
}

zip_close $zfd
