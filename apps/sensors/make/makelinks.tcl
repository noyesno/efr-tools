set mkdir [file dirname [info script]]
source [file join $mkdir .. .. .. .. common tcldev lib make make lib links.tcl]

set libdir [file join $mkdir .. lib]
cd $libdir
set common ../../../../common
set cx ../../cxManager/lib
set lamp ../../lampcontroller/lib

foreach src [list make progver event] {
    ::linker::symdir [file join $common tcldev/lib $src] $src
}
foreach src [list tkconclient send rest] {
    ::linker::symdir [file join $common tcldev/contrib] $src
}
if { $tcl_platform(platform) eq "windows" } {
    foreach src [list iocpsock3.0 udp1.0.9] {
	::linker::symdir [file join $common tcldev/contrib] $src
    }
}
# Following to ensure we access tcllib under debugging sessions.
foreach src [list tcllib1.11.1] {
    ::linker::symdir [file join $common] $src
}
# Bring in some source code from related packages.
foreach src [list cxapi http] {
    ::linker::symdir [file join $cx] $src
}

::linker::symdir $common til
foreach src [list bootstrap.tcl init.tcl] {
    file copy -force -- [file join $common tcldev lib $src] $src
    puts "Copied $src from [file join $common tcldev lib]"
}
