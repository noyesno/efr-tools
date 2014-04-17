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
make contextmanager.kit from [list [file join $topdir contextmanager.tcl] \
			       messages] using {
    ::binmake::kit $::topdir contextmanager -extras [list msgs] \
	-tcllib [list log cmdline fileutil dns uri html textutil ncgi \
		    base64 md5 uuid struct sha1 json] \
	-tclbi [list fileutil uri Trf json tdom] \
	-tclpot [list fileutil uri tls md5 sha1 Trf json tdom tcom] \
	-tclcore [list reg] \
	-lib [list *.tcl \
		  uuidhash tkconclient progver event schema model \
		  oauth conduits rest udp1.0.9 db ssdp UPnP cxapi \
		  redis csvout] \
	-til [list uobj diskutil errhan minihttpd mimetype tax websocket]
}
make contextmanager.exe from contextmanager.kit using {
    set vsn [::progver::guess contextmanager]
    ::binmake::executable $::topdir contextmanager console \
        appicons [file join pics contextmanager.ico] \
        version $vsn \
	descr "me3gas Context Manager" \
        company "SICS" \
        copyright "Emmanuel Frécon" \
        product "me3gas project"
}
make contextmanager from contextmanager.exe using ""

make pachube.kit from [list [file join $topdir pachube.tcl] \
			       messages] using {
    ::binmake::kit $::topdir pachube -extras [list msgs] \
	-tcllib [list log cmdline fileutil \
		    base64 json uri] \
	-tclbi [list fileutil uri Trf json base64 tdom] \
	-tclpot [list fileutil uri tls Trf json base64 tdom] \
	-tclcore [list reg] \
	-lib [list bootstrap.tcl init.tcl \
		  tkconclient progver event rest] \
	-til [list uobj diskutil errhan]
}
make pachube.exe from pachube.kit using {
    set vsn [::progver::guess pachube]
    ::binmake::executable $::topdir pachube console \
        appicons [file join pics pachube.ico] \
        version $vsn \
	descr "me3gas Pachube connector" \
        company "SICS" \
        copyright "Emmanuel Frécon" \
        product "me3gas project"
}
make pachube from pachube.exe using ""

make pairing.kit from [list [file join $topdir pairing.tcl] \
			       messages] using {
    ::binmake::kit $::topdir pairing -extras [list msgs] \
	-tcllib [list log cmdline fileutil \
		    base64 json uri] \
	-tclbi [list fileutil uri Trf json base64 tdom] \
	-tclpot [list fileutil uri tls Trf json base64 tdom] \
	-tclcore [list reg] \
	-lib [list bootstrap.tcl init.tcl pairing.tcl api.tcl \
		  tkconclient progver event rest] \
	-til [list uobj diskutil errhan]
}
make pairing.exe from pairing.kit using {
    set vsn [::progver::guess pairing]
    ::binmake::executable $::topdir pairing console \
        appicons [file join pics pairing.ico] \
        version $vsn \
	descr "me3gas pairing connector for pachube and remote contexts
" \
        company "SICS" \
        copyright "Emmanuel Frécon" \
        product "me3gas project"
}
make pairing from pairing.exe using ""


make twitter.kit from [list [file join $topdir twitter.tcl] \
			       messages] using {
    ::binmake::kit $::topdir twitter -extras [list msgs] \
	-tcllib [list log cmdline fileutil dns uri base64] \
	-tclbi [list fileutil uri Trf] \
	-tclpot [list fileutil uri tls Trf] \
	-tclcore [list reg] \
	-lib [list *.tcl \
		  tkconclient progver event] \
	-til [list uobj diskutil errhan]
}
make twitter.exe from twitter.kit using {
    set vsn [::progver::guess twitter]
    ::binmake::executable $::topdir twitter console \
        appicons [file join pics twitter.ico] \
        version $vsn \
	descr "me3gas auto twitter" \
        company "SICS" \
        copyright "Emmanuel Frécon" \
        product "me3gas project"
}
make twitter from twitter.exe using ""


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
    foreach a [list contextmanager twitter pachube pairing] {
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
make all from [list contextmanager] using ""

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
