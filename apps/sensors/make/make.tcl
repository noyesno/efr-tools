# make.tcl -- Binaries and application bundle construction
#
#	This script is a make-like facility to construct wrapped
#	binaries for the viewr application bundle.  The script also
#	provides support for the automated construction of associated
#	file (ZIP file for distribution, MD5SUMS, etc.).
#
# Copyright (c) 2004-2006 by the Swedish Institute of Computer Science.
#
# See the file 'license.terms' for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

set topdir [file join [file normalize [file dirname [info script]]] ..]

# Source bootstrapping routine.
source [file join $topdir lib bootstrap.tcl]

# Now source make-like rule system
set mkdir [::bootstrap::resolve_links [file join $topdir lib make]]
source [file join $mkdir lib make.tcl]
source [file join $mkdir lib binmake.tcl]
source [file join $mkdir lib translate.tcl]

# Source program versioning fixes.
set verdir [::bootstrap::resolve_links [file join $topdir lib progver]]
lappend auto_path $verdir
package require progver


proc clean_imagemagick { rootdir } {
    set im_dir [file join $rootdir lib imgop ImageMagick Windows-x86]
    puts "Cleaning ImageMagick installation at $im_dir to a minimum"
    set allfiles [glob -nocomplain -tails -directory $im_dir *]
    foreach f $allfiles {
	if { ! [string match convert* $f] } {
	    file delete -force -- [file join $im_dir $f]
	}
    }
}

proc clean_process { rootdir } {
    set processdir [file join $rootdir lib til process bin windows]
    puts "Cleaning process installation at $processdir to a minimum"
    if { [file exists [file join $processdir pdh.dll]] } {
	file delete -force -- [file join $processdir pdh.dll]
    }
    if { [file exists [file join $processdir pv]] } {
	file delete -force -- [file join $processdir pv]
    }
    set allfiles [glob -nocomplain -tails \
		      -directory [file join $processdir pslist] *]
    foreach f $allfiles {
	if { ! [string match *list* $f] && ! [string match *kill* $f] \
		 && ! [string match *.dll $f] } {
	    file delete -force -- [file join $processdir pslist $f]
	}
    }
}


make.force messages from [list] using {
    puts "Translating applications"
    ::translate::update [file join $::topdir msgs] \
	-files [list [file join $::topdir lampcontroller.tcl]] \
	-languages [list en sv]
}

# lampcontroller
# XXX: We should find a way to automatically turn the debughelper
# facility off everytime we make the kit/exe so as to get rid of
# problems when Tcl is installed on the same machine as the .exe is
# running.
make statlink.kit from [list [file join $topdir statlink.tcl] \
			       messages] using {
    ::binmake::kit $::topdir statlink -extras [list msgs] \
	-tcllib [list log cmdline fileutil json base64 uri] \
	-tclbi [list fileutil json base64 uri tdom] \
	-tclpot [list fileutil json base64 uri tdom tls tcom] \
	-tclcore [list reg] \
	-lib [list bootstrap.tcl init.tcl \
		  tkconclient progver event rest cxapi] \
	-til [list uobj diskutil errhan]
}
make statlink.exe from statlink.kit using {
    set vsn [::progver::guess statlink]
    ::binmake::executable $::topdir statlink console \
        appicons [file join pics statlink.ico] \
        version $vsn \
	descr "Periodically reports upon pump sensor values to pachube.com and ContextManager" \
        company "SICS" \
        copyright "Emmanuel Frécon" \
        product "PumpReporter"
}
make statlink from statlink.exe using ""


make tinynode.kit from [list [file join $topdir tinynode.tcl] \
                               messages] using {
    ::binmake::kit $::topdir tinynode -extras [list msgs] \
        -tcllib [list log cmdline fileutil json base64 uri] \
        -tclbi [list fileutil json base64 Trf uri tdom] \
        -tclpot [list fileutil json base64 uri tdom tls Trf tcom] \
        -tclcore [list reg] \
        -lib [list bootstrap.tcl init.tcl \
                  udp1.0.9 tkconclient progver event rest cxapi] \
        -til [list uobj diskutil errhan]
}
make tinynode.exe from tinynode.kit using {
    set vsn [::progver::guess tinynode]
    ::binmake::executable $::topdir tinynode console \
        appicons [file join pics tinynode.ico] \
        version $vsn \
        descr "Sends TinyNode measures to context manager" \
        company "SICS" \
        copyright "Emmanuel Frécon" \
        product "TinyReceiver"
}
make tinynode from tinynode.exe using ""


make snoop_motes.kit from [list [file join $topdir snoop_motes.tcl] \
                               messages] using {
    ::binmake::kit $::topdir snoop_motes -extras [list msgs] \
	-version 8.6b \
        -tcllib [list log cmdline fileutil json base64 uri] \
        -tclbi [list fileutil json base64 Trf uri tdom tcom] \
        -tclpot [list fileutil json base64 uri tdom tls Trf tcom] \
        -tclcore [list reg] \
        -lib [list bootstrap.tcl init.tcl tkconclient progver \
		  event rest http cxapi] \
        -til [list uobj diskutil errhan]
}
make snoop_motes.exe from snoop_motes.kit using {
    set vsn [::progver::guess snoop_motes]
    ::binmake::executable $::topdir snoop_motes console:8.6b \
        appicons [file join pics tinynode.ico] \
        version $vsn \
        descr "Polls remote server and send measures to context manager" \
        company "SICS" \
        copyright "Emmanuel Frécon" \
        product "SnoopMotes"
}
make snoop_motes from snoop_motes.exe using ""

make transformer.kit from [list [file join $topdir transformer.tcl] \
                               messages] using {
    ::binmake::kit $::topdir transformer -extras [list msgs] \
	-version 8.6b \
        -tcllib [list log cmdline fileutil html ncgi textutil json md5 \
		     base64 uri dns uuid] \
        -tclbi [list fileutil json base64 Trf uri tdom tcom] \
        -tclpot [list fileutil json base64 md5 sha1 uri tdom tls Trf tcom] \
        -tclcore [list reg] \
        -lib [list bootstrap.tcl init.tcl tkconclient progver \
		  event http cxapi] \
        -til [list uobj diskutil errhan minihttpd websocket mimetype]
}
make transformer.exe from transformer.kit using {
    set vsn [::progver::guess transformer]
    ::binmake::executable $::topdir transformer console:8.6b \
        appicons [file join pics transformer.ico] \
        version $vsn \
        descr "Sink for context object, sending tranformed data to sensors." \
        company "SICS" \
        copyright "Emmanuel Frécon" \
        product "Transformer"
}
make transformer from transformer.exe using ""


# Automate constuction of distribution ZIP file, use the version
# contained in the version file.
set pver [join [split [::progver::guess lampcontroller] "."] ""]
set pver [string trimright $pver "0"]
make lampcontroller${pver}.zip from [list lampcontroller.exe] \
    using {
    mexec [info nameofexecutable] make/makedistro.tcl \
	-zip lampcontroller_v${pver}.zip -rootdir lampcontroller_v$pver
}
make distro from lampcontroller${pver}.zip using ""

# AUtomate construction of MD5 sums
make MD5SUMS from [list lampcontroller.exe lampcontroller.cfg lampcontroller.pwd] using {
    mexec [info nameofexecutable] make/md5write.tcl \
	lampcontroller.exe lampcontroller.cfg lampcontroller.pwd
}

# cleanup Rule
make.force clean from [list] using {
    foreach a [list statlink tinynode snoop_motes transformer] {
	puts "Removing $a"
	catch {file delete ${a}.exe}
	catch {file delete ${a}.kit}
	foreach f [glob -nocomplain tmp*_${a}] {
	    puts "Removing directory $f"
	    catch {file delete -force $f}
	}
	foreach f [glob -nocomplain ${a}*.zip] {
	    puts "Removing $f"
	    catch {file delete $f}
	}
    }
    foreach f [list MD5SUMS distro.zip] {
	puts "Removing $f"
	catch {file delete $f}
    }
}

# Make everything
make all from [list statlink tinynode] using ""

# Glue for default rules and parameters.
proc main {} {
    if {$::argc > 0} {
	foreach a $::argv {
	    eval [lindex $a 0]
	}
    } else {
	eval all
    }
}
main
