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
# bindxtnd.tcl
# Copyright 1995 by Paul Raines (raines&slac.stanford.edu)

# This file defines code shared by all widget bindings int the
# BindExtended package

#-------------------------------------------------------------------------
# Elements of tkBind used by all widgets. These can be set in
# a user's ~/.tkBindrc file.
#
# notWord -		Regular expression saying what characters are not
#			to be considered a word
# fillBreak -		String containing the characters upon which paragraph
#			filling is allowed to break on
# meta -		What should be considered meta for emacs bindings,
#			either Meta or Alt
# killRing -		List storing kill buffers
# killLen -		Length of kill ring
# killPtr -		Index of buffer in kill ring to use for next yank
# killMax -		Maximum number of buffers to store in kill ring
# undoMax -		Maximum number of buffers to store in undo list
# bindUndo -		Whether undo ring should be on by default
# bell -		Command to use instead of ring bell for errors
# modKeys -		List of keysyms for your keyboards modifier keys
#			Any keysyms listed in 'xmodmap -pm' should go here
# delSel -		If set true, any current selection is deleted
#			an a character insertion or character deletion
# insertAtClick -	Whether a mouse insert should be done at the position
#			of the mouse click or current insert mark
# noCase -		Set to 1 to make search case insensitive, 0 for not
# path -		List of paths to search for tkBind packages
# required -		List of packages already required
#-------------------------------------------------------------------------
# Widget specific elements of tkBind for internal use only.
#
# bindtags -		Bindtags saved for a text widget when in state key
# mesg -		A variable that these procedure write informational
#			messages to. Good to use for -textvariable.
#-------------------------------------------------------------------------
global tkBind

# tkBindDefVar --
# Set the element 'elem' in the tkBind array to 'def' only if 
# it does not already exist. Useful to allow developer to override
# defaults before this file is sourced

proc tkBindDefVar {elem def} {
    global tkBind
    if {![info exists tkBind(${elem})]} {
        set tkBind(${elem}) ${def}
    }
}

tkBindDefVar notWord {[^a-zA-Z_0-9]}
tkBindDefVar fillBreak " \t-"
tkBindDefVar meta Meta
tkBindDefVar undoMax 150
tkBindDefVar killMax 25
tkBindDefVar killRing {}
tkBindDefVar killLen 0
tkBindDefVar killPtr 0
tkBindDefVar bindUndo 0
tkBindDefVar bell bell
tkBindDefVar delSel 1
tkBindDefVar insertAtClick 0
tkBindDefVar noCase 1
tkBindDefVar required {}
# Meta_L is needed on HP.
#tkBindDefVar modKeys [list Control_L Control_R Meta_R Meta_L Alt_R Alt_L \
			  Shift_L Shift_R Caps_Lock Multi_key]
tkBindDefVar modKeys [list Control_L Control_R Meta_R Alt_R Alt_L Shift_L\
  Shift_R Caps_Lock Multi_key]

# tkBindRequire --

proc tkBindRequire {pkg {nocomplain 0} {bind 1}} {
    global tkBind

    set indir [file dirname ${pkg}]
    set pkg [file tail ${pkg}]

    if {![string length [file extension ${pkg}]]} {
        append pkg .tcl
    }
    if {[lsearch -exact $tkBind(required) ${pkg}] > -1} {
        return 2
    }

    set tkBind([file rootname ${pkg}],bind) ${bind}

    foreach dir [concat ${indir} [glob -nocomplain ~/tk/tkBind] $tkBind(path)] {
        if {[string length ${dir}] && [file exists [file join ${dir} ${pkg}]]} {
            source [file join ${dir} ${pkg}]
            lappend tkBind(required) ${pkg}
            return 1
        }
    }
    if {!${nocomplain}} {
        error "Cannot find tkBindExtend package ${pkg}."
    }
    return 0
}

# tkBindNoBind -- 
# If not a modifier key, signal a non-bound key
proc tkBindNoBind {w k s} {
    global tkBind
    if {[lsearch $tkBind(modKeys) ${k}] < 0} {
        set tkBind(${w},mesg) "[tkBindGetMod ${s}]${k} not bound."
        eval $tkBind(bell)
    }
}

# tkBindGetMod --

proc tkBindGetMod s {
    set mod {}
    if {${s} & 1} {
        append mod "Shift-"
    }
    if {${s} & 2} {
        append mod "Lock-"
    }
    if {${s} & 4} {
        append mod "Control-"
    }
    if {${s} & 8} {
        append mod "Mod1-"
    }
    if {${s} & 16} {
        append mod "Mod2-"
    }
    if {${s} & 32} {
        append mod "Mod3-"
    }
    if {${s} & 64} {
        append mod "Mod4-"
    }
    return ${mod}
}

# tkBindCancelStateKey --
# Cancel the current state key in widget 'w'

proc tkBindCancelStateKey w {
    global tkBind
    if {[llength $tkBind(${w},bindtags)]} {
        bindtags ${w} $tkBind(${w},bindtags)
        set tkBind(${w},bindtags) {}
    }
}

# tkBindSetStateKey --
# Arm the state key 's' in widget 'w' echoing 'd' to message area

proc tkBindSetStateKey {w s d} {
    global tkBind

    catch {
        if {![llength $tkBind(${w},bindtags)]} {
            set tkBind(${w},bindtags) [bindtags ${w}]
        }
        bindtags ${w} [concat ${s} BindState $tkBind(${w},bindtags)]
        set tkBind(${w},mesg) ${d}
    }
}

bind BindState <KeyPress> {
  tkBindCancelStateKey %W; break
}
bind BindState <ButtonPress> {
  tkBindCancelStateKey %W; break
}

# tkBindDefArg --
# Default handler for modifying a repeat count by the current buffer
# arg count. The repeat count will only be modified if it is a plus
# or minus sign.
#
# Arguments:
# w -		The window in which to modify count
# n -		The repeat count to be modified

proc tkBindDefArg {w n {def 1}} {
    global tkBind

    if {![string length $tkBind(${w},arg)]} {
        set tkBind(${w},arg) ${def}
    }\
    elseif {$tkBind(${w},arg) == "-"} {
        set tkBind(${w},arg) -1
    }\
    elseif {$tkBind(${w},arg) == "+"} {
        set tkBind(${w},arg) 1
    }
    if {${n} == "+"} {
        set n $tkBind(${w},arg)
    }\
    elseif {${n} == "-"} {
        set n [expr -1*$tkBind(${w},arg)]
    }
    set tkBind(${w},arg) {}
    return ${n}
}

# tkBindArgKey --
#
# Arguments:
# w -		The window in which to yank
# a -		The ascii character of key ( a minus sign or decimal number)

proc tkBindArgKey {w a} {
    global tkBind
    if {${a} == "-"} {
        if {$tkBind(${w},arg) == "-"} {
            set tkBind(${w},arg) "+"
        }\
        elseif {$tkBind(${w},arg) == "+"} {
            set tkBind(${w},arg) "-"
        }\
        elseif [string length $tkBind(${w},arg)] {
            set tkBind(${w},arg) [expr -1*$tkBind(${w},arg)]
        } else {
            set tkBind(${w},arg) "-"
        }
        set tkBind(${w},mesg) "arg: $tkBind(${w},arg)"
        return
    }
    if {![string length $tkBind(${w},arg)]} {
        set tkBind(${w},mesg) "arg: "
    }
    append tkBind(${w},arg) ${a}
    append tkBind(${w},mesg) ${a}
}


