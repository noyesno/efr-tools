package require platform

array set ML {
    ln      "ln"
    lnopts  "-s -f"
}


proc makelink { root dir } {
    global mkdir
    global ML
    global tcl_platform

    set target [file join $root $dir]
    if { ![file exists $target] } {
	puts "$target does not exist!!!"
	return 0
    }

    set cmd [list exec $ML(ln)]
    set cmd [concat $cmd $ML(lnopts)]
    lappend cmd $target
    lappend cmd [file tail $target]
    if { [catch {eval $cmd} err] } {
	puts "Could not link to $target: $err"
    } else {
	puts "Link created: $dir -> $target"
    }
}


# On windows, rely on the ln.exe from
# http://www.flexhex.com/docs/articles/hard-links.phtml for making the
# link instead, this avoids any dependency on cygwin or msys.  Note
# that ln -s on msys copies data instead of creating a shortcut, which
# is why we had to rely on an external tool.
set mkdir [file dirname [info script]]
foreach {os cpu} [split [::platform::generic] "-"] break
if { $os eq "windows" } {
    set ML(ln) [file join [file normalize $mkdir] "ln.exe"]
    set ML(lnopts) "-s"
}
set libdir [file join $mkdir .. lib]

cd $libdir
set common ../../../../common
set lamp ../../lampcontroller/lib
set cx ../../cxManager/lib

foreach src [list make progver event uuidhash] {
    makelink [file join $common tcldev/lib $src] $src
}
foreach src [list tkconclient send rest redis oauth] {
    makelink [file join $common tcldev/contrib] $src
}
# Following to ensure we access tcllib under debugging sessions.
foreach src [list tcllib1.11.1] {
    makelink [file join $common] $src
}
# Link from external, but related programs.
foreach src [list htmlutil] {
    makelink [file join $lamp] $src
}
foreach src [list cxapi] {
    makelink [file join $cx] $src
}

makelink $common til
foreach src [list bootstrap.tcl init.tcl] {
    file copy -force -- [file join $common tcldev lib $src] $src
    puts "Copied $src from [file join $common tcldev lib]"
}
