# md5write.tcl -- Creates MD5SUM files
#
#	This program will create a MD5SUMS file that will host the MD5
#	sums for a given number of file and that can be used when
#	releasing new binaries on a web server.  The program is
#	constructed so as to update the MD5SUM file by default.  All
#	files which names are passed on the command line will be
#	scanned for their MD5 sum and appended/updated into the MD5
#	sum file.
#
# Copyright (c) 2004-2006 by the Swedish Institute of Computer Science.
#
# See the file 'license.terms' for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

array set M5W {
    finished      0
    comments      "\#!;"
}

set options {
    { wipe "" "Should we wipe out the content of the MD5 sum file?" }
    { out.arg "%progdir%/../MD5SUMS" "File containing MD5 sums" }
}

source [file join [file dirname $argv0] .. lib init.tcl]
::init::init \
    -store M5W \
    -options $options \
    -booleans {wipe} \
    -load [list diskutil] \
    -packages [list md5]


set fileinfo [list]
set md5fname [::diskutil::fname_resolv $M5W(out)]
if { ! $M5W(wipe) } {
    if { [catch {open $md5fname} fd] == 0 } {
	$M5W(log)::info "Reading content of MD5 sum file at $md5fname"
	while { ! [eof $fd] } {
	    set line [gets $fd]
	    if { $line ne "" } {
		set firstchar [string index $line 0]
		# Skip all lines that are commented.
		if { [string first $firstchar $M5W(comments)] < 0 } {
		    set sum [lindex $line 0]
		    set fname [lrange $line 1 end]
		    lappend fileinfo [file tail $fname] $sum
		}
	    }
	}
	close $fd
    }
}

foreach fname $argv {
    set fname [::diskutil::fname_resolv $fname]
    $M5W(log)::info "Computing MD5 for $fname..."
    set md5 [::md5::md5 -hex -file $fname]
    set idx [lsearch $fileinfo $fname]
    if { $idx >= 0 } {
	incr idx
	set fileinfo [lreplace $fileinfo $idx $idx $md5]
    } else {
	lappend fileinfo [file tail $fname] $md5
    }
}

if { [catch {open $md5fname "w"} fd] == 0 } {
    $M5W(log)::info "Writing MD5 sum file at $md5fname"
    foreach {fname sum} $fileinfo {
	puts $fd "$sum $fname"
    }
    close $fd
}
