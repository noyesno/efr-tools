# Tcl package index file, version 1.1
# This file is generated by the "pkg_mkIndex" command
# and sourced either when an application starts up or
# by a "package unknown" script.  It invokes the
# "package ifneeded" command to set up package-related
# information so that packages will be loaded automatically
# in response to "package require" commands.  When this
# script is sourced, the variable $dir must contain the
# full path name of this file's directory.

package ifneeded winapi 0.2 [list source [file join $dir winapi.tcl]]\n[list source [file join $dir wa_actacc.tcl]]\n[list source [file join $dir wa_gdi.tcl]]\n[list source [file join $dir wa_messages.tcl]]\n[list source [file join $dir wa_mixer.tcl]]\n[list source [file join $dir wa_process_thread.tcl]]\n[list source [file join $dir wa_user_input.tcl]]\n[list source [file join $dir wa_windows.tcl]]\n[list source [file join $dir wa_power.tcl]]
package ifneeded winapi::core 0.2 [list source [file join $dir core.tcl]]
package ifneeded winapix 0.3 [list source [file join $dir winapix.tcl]]
