# Copyright (c) 2000, Red Hat, Inc.
# 
# This file is part of Source-Navigator.
# 
# Source-Navigator is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2, or (at your option)
# any later version.
# 
# Source-Navigator is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along
# with Source-Navigator; see the file COPYING.  If not, write to
# the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
# MA 02111-1307, USA.
# 
# error.tcl - Routines for error handling.
# Copyright (C) 1998 Cygnus Solutions.

proc sn_error_dialog {TextString {ititle ""} {wait "wait"} {cmd ""}} {
    if {[sn_batch_mode]} {
        if {${ititle} != ""} {
            set err "${ititle}: ${TextString}"
        } else {
            set err ${TextString}
        }
        puts stdout ${err}
        return
    }

    set TextString [string range ${TextString} 0 999]
    sn_log "Error <${TextString}>"

    if {${ititle} == ""} {
        set title [get_indep String Error]
    } else {
        set title ${ititle}
    }

    # FIXME: is cmd actually used anywhere?
    # When should we run it? Would it ever not get run?

    # FIXME : the regular msg box might not be perfect but
    # at least it works, improve it later!

    return [tk_messageBox -message $TextString -icon error -title $title]
}

proc sn_wait_dialog {parent TextString ititle {cmd ""} {ok_str ""}} {
    global sn_debug tkPriv

    set TextString [string range ${TextString} 0 999]

    set focus [focus]
    if {${focus} == ""} {
        set focus "."
    }

    if {${ititle} == ""} {
        set title [get_indep String Error]
    } else {
        set title ${ititle}
    }

    set w ${parent}.wait_dlg
    if {[itcl::find object ${w}] == ${w}} {
        itcl::delete object ${w}
    }
    sourcenav::Window ${w}

    ${w} withdraw
    ${w} title ${title}
    ${w} minsize 100 100
    set ctrl [winfo toplevel ${focus}]

    if {[wm state ${ctrl}] == "normal"} {
        ${w} transient ${ctrl}
    }
    ${w} on_close "itcl::delete object ${w} ; set tkPriv(${w},button) -1"

    frame ${w}.msg_bitmap

    if {${ititle} != ""} {
        label ${w}.msg_bitmap.bitmap -image info_image
    } else {
        label ${w}.msg_bitmap.bitmap -image error_image
    }

    label ${w}.msg_bitmap.msg -text ${TextString} -relief flat

    pack ${w}.msg_bitmap.bitmap -side left -padx 5m
    pack ${w}.msg_bitmap.msg -side right -expand y -fill both -padx 5 -pady 5

    pack ${w}.msg_bitmap -expand y -fill both -pady 5

    if {${ok_str} == ""} {
        set ok_str [get_indep String Cancel]
    }
    sn_motif_buttons ${w} bottom 0 ${ok_str}

    if {${cmd} == ""} {
        ${w}.button_0 configure -command "
                itcl::delete object ${w}
                set tkPriv(${w},button) -1
            "
    } else {
        ${w}.button_0 configure -command "
                eval ${cmd}
                itcl::delete object ${w}
                set tkPriv(${w},button) -1
            "
    }

    if {[catch {set save_grab [${w} grab current]}]} {
        set save_grab ""
    }

    ${w} bind_tk <Return> "${w}.button_0 invoke"
    ${w} bind_tk <Escape> "${w}.button_0 invoke"

    catch {${w} grab set}
    ${w} focus_tk
    ${w} centerOnScreen
}

