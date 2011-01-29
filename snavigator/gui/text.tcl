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
# text.tcl
# Copyright (c) 1992-1994 The Regents of the University of California.
# Copyright (c) 1994-1995 Sun Microsystems, Inc.
#
# This file defines the default bindings for Tk text widgets and is a
# highly extended version of the original distributed with Tk4.0. It
# is intended as a drop in replacement for the original. If you do not
# have the permission or desire to do that, then you run the following
# lines in your applicaiton to achieve the efficitively same result
#
# foreach key [bind Text] { bind Text $key {} }
# source text.tcl
#
# This is beta software so beware. It will work only with Tk4.0.
# Release tkBindExtended

# ORIGINAL COPYRIGHT INFORMATION
#
# Copyright (c) 1992-1994 The Regents of the University of California.
# Copyright (c) 1994-1995 Sun Microsystems, Inc.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

# ADDITIONAL COPYRIGHT INFORMATION
#
# Copyright 1995 by Paul Raines (raines&slac.stanford.edu)
# with the same DISCLAIMER as above 

#-------------------------------------------------------------------------
# Elements of tkPriv that are used in this file:
#
# afterId -		If non-null, it means that auto-scanning is underway
#			and it gives the "after" id for the next auto-scan
#			command to be executed.
# mouseMoved -		Non-zero means the mouse has moved a significant
#			amount since the button went down (so, for example,
#			start dragging out a selection).
# selectMode -		The style of selection currently underway:
#			char, word, or line.
# x, y -		Last known mouse coordinates for scanning
#			and auto-scanning.
#-------------------------------------------------------------------------
global tkPriv

#-------------------------------------------------------------------------
# Elements of tkBind used by all text widgets.
#
# fillCol -		Default fill column for initializing text widgets
#-------------------------------------------------------------------------
global tkBind

tkBindDefVar fillCol 0

#-------------------------------------------------------------------------
# Elements of tkText specific to each text widget
#
# char -		Character position on the line; kept in order
#			to allow moving up or down past short lines while
#			still remembering the desired position.
# prevPos -		Used when moving up or down lines via the keyboard.
#			Keeps track of the previous insert position, so
#			we can distinguish a series of ups and downs, all
#			in a row, from a new up or down.
# arg -			current argument count for repeating next command
# prevCmd -		The last tkText command run
# markActive -		Whether transient mark mode is active
# ovwrt - 		Whether overwrite mode is active
# killLast -		Text index of last kill used to determine whether
#			the next kill can be appended to the last kill
# undoList -		List storing undo info for each step
# undoCnt - 		Length of the undo list
# undoCut -		Info of last operation that can be undone
# undoFirst - 		Starting text index of last undoable operation
# undoLast - 		Ending text index of last undoable operation
# markRing -		List as history of emacs marks in buffer
# fillCol -		Character column at which to "hard" wrap text
# fillWidth -		Best guess at pixel width to wrap lines at
#-------------------------------------------------------------------------
global tkText

# tkTextSetup --
# Set up private variables for the specified text widget 'w'.

proc tkTextSetup w {
    global tkText tkBind

    if [info exists tkText(${w},char)] return

    set tkText(${w},char) {}
    set tkText(${w},prevPos) {}
    set tkText(${w},prevCmd) Setup
    set tkText(${w},markActive) 0
    set tkText(${w},ovwrt) 0
    set tkText(${w},killLast) 0.0
    set tkText(${w},markRing) {}
    set tkText(${w},fillCol) $tkBind(fillCol)
    set tkText(${w},fillWidth) [tkTextGetFillWidth ${w} $tkText(${w},fillCol)]

    set tkBind(${w},bindtags) {}
    set tkBind(${w},arg) {}
    set tkBind(${w},toplevel) [winfo toplevel ${w}]

    tkBindDefVar "${w},mesg" {}

    if $tkBind(bindUndo) {
        tkTextUndoSetup ${w}
    }

    if {[info proc tkTextWidgetHook]!=""} {
        tkTextWidgetHook ${w}
    }
    ${w} mark set emacs 0.0
}

# tkTextDestroy --
# Free memory of private variables for the specified text widget 'w'.

proc tkTextDestroy w {
    global tkText tkBind

    catch {unset tkText(${w},char)}
    catch {unset tkText(${w},prevPos)}
    catch {unset tkText(${w},prevCmd)}
    catch {unset tkText(${w},markActive)}
    catch {unset tkText(${w},ovwrt)}
    catch {unset tkText(${w},killLast)}
    catch {unset tkText(${w},markRing)}
    catch {unset tkText(${w},fillCol)}
    catch {unset tkText(${w},fillWidth)}

    catch {unset tkBind(${w},bindtags)}
    catch {unset tkBind(${w},mesg)}
    catch {unset tkBind(${w},arg)}

    catch {tkTextUndoFree ${w}}
}

# Setup/Cleanup bindings
bind Text <FocusIn> {tkTextSetup %W}
bind Text <Destroy> {tkTextDestroy %W}

# Mouse bindings

bind Text <Button> {
	if {!(%b == 1 || %b == 3)} break
	tkTextButton%b %W %x %y
	%W tag remove sel 0.0 end
	set tkPriv(x) %x
	set tkPriv(y) %y
}
bind Text <B1-Motion> {
	set tkPriv(x) %x
	set tkPriv(y) %y
	tkTextSelectTo %W %x %y
}
bind Text <B3-Motion> {
	set tkPriv(x) %x
	set tkPriv(y) %y
	tkTextSelectTo %W %x %y
}
bind Text <Double-Button> {
	if {!(%b == 1 || %b == 3)} break
	set tkPriv(selectMode) word
	tkTextSelectTo %W %x %y
# if {%b == 1} { catch {%W mark set insert sel.first} }
}
bind Text <Triple-Button> {
	if {!(%b == 1 || %b == 3)} break
	set tkPriv(selectMode) line
	tkTextSelectTo %W %x %y
	if {%b == 1} { catch {%W mark set insert sel.first} }
}
bind Text <Shift-Button> {
	if {!(%b == 1 || %b == 3)} break
	tkTextResetAnchor %W @%x,%y
	set tkPriv(selectMode) char
	tkTextSelectTo %W %x %y
}
bind Text <Double-Shift-Button> {
	if {!(%b == 1 || %b == 3)} break
	set tkPriv(selectMode) word
	tkTextSelectTo %W %x %y
}
bind Text <Triple-Shift-Button> {
	if {!(%b == 1 || %b == 3)} break
	set tkPriv(selectMode) line
	tkTextSelectTo %W %x %y
}
bind Text <B1-Leave> {
	set tkPriv(x) %x
	set tkPriv(y) %y
	tkTextAutoScan %W
}
bind Text <B1-Enter> {
	tkCancelRepeat
}
bind Text <B3-Leave> {
}
bind Text <B3-Enter> {
}
bind Text <ButtonRelease> {
	if {!(%b == 1 || %b == 3)} break
	tkCancelRepeat
	if {[selection own -displayof %W] == "%W" &&
			[string length [%W tag nextrange sel 1.0 end]]} {
#		clipboard clear -displayof %W
#		clipboard append -displayof %W [selection get -displayof %W]
	}
}
bind Text <Control-1> {
	%W mark set insert @%x,%y
}

bind Text <3> {
	%W scan mark %x %y
	set tkPriv(x) %x
	set tkPriv(y) %y
	set tkPriv(mouseMoved) 1
}

bind Text <B3-Motion> {
	%W scan dragto %x %y
}

bind Text <B2-Motion> { }

bind Text <2> {
	%W mark set insert @%x,%y
	%W scan mark %x %y
	set tkPriv(x) %x
	set tkPriv(y) %y
	set tkPriv(mouseMoved) 1
}

bind Text <ButtonRelease-2> {
	tkTextSetup %W
	tkTextButtonInsert %W %x %y
	focus %W;		# Very important for Undo support.
}

# Standard key bindings

bind Text <Left> {
	tkTextSetCursor %W [tkTextPlaceChar %W -]
}
bind Text <Right> {
	tkTextSetCursor %W [tkTextPlaceChar %W +]
}
bind Text <Up> {
	set pos [tkTextPlaceLine %W -]
	tkTextSetCursor %W $pos
	set tkText(%W,prevPos) $pos
}
bind Text <Down> {
	set pos [tkTextPlaceLine %W +]
	tkTextSetCursor %W $pos
	set tkText(%W,prevPos) $pos
}
bind Text <Shift-Left> {
	tkTextKeySelect %W [tkTextPlaceChar %W -]
}
bind Text <Shift-Right> {
	tkTextKeySelect %W [tkTextPlaceChar %W +]
}
bind Text <Shift-Up> {
	set pos [tkTextPlaceLine %W -]
	tkTextKeySelect %W $pos
	set tkText(%W,prevPos) $pos
}
bind Text <Shift-Down> {
	set pos [tkTextPlaceLine %W +]
	tkTextKeySelect %W $pos
	set tkText(%W,prevPos) $pos
}
bind Text <Control-KeyPress> { }
bind Text <Control-Left> {
	tkTextSetCursor %W [tkTextPlaceWord %W -]
}
bind Text <Control-Right> {
	tkTextSetCursor %W [tkTextPlaceWord %W +]
}
bind Text <Control-Up> {
	tkTextSetCursor %W [tkTextPlacePara %W -]
}
bind Text <Control-Down> {
	tkTextSetCursor %W [tkTextPlacePara %W +]
}
bind Text <Shift-Control-Left> {
	tkTextKeySelect %W [tkTextPlaceWord %W -]
}
bind Text <Shift-Control-Right> {
	tkTextKeySelect %W [tkTextPlaceWord %W +]
}
bind Text <Shift-Control-Up> {
	tkTextKeySelect %W [tkTextPlacePara %W -]
}
bind Text <Shift-Control-Down> {
	tkTextKeySelect %W [tkTextPlacePara %W +]
}
bind Text <Prior> {
	tkTextSetCursor %W [tkTextScrollPages %W -]
}
bind Text <Shift-Prior> {
	tkTextKeySelect %W [tkTextScrollPages %W -]
}
bind Text <Next> {
	tkTextSetCursor %W [tkTextScrollPages %W +]
}
bind Text <Shift-Next> {
	tkTextKeySelect %W [tkTextScrollPages %W +]
}
bind Text <Control-Prior> {
	%W xview scroll [tkBindDefArg %W -] page
}
bind Text <Control-Next> {
	%W xview scroll [tkBindDefArg %W +] page
}

bind Text <Home> {
	tkTextSetCursor %W [tkTextPlaceHome %W +]
}
bind Text <Shift-Home> {
	tkTextKeySelect %W [tkTextPlaceHome %W +]
}
bind Text <End> {
	tkTextSetCursor %W [tkTextPlaceEnd %W +]
}
bind Text <Shift-End> {
	tkTextKeySelect %W [tkTextPlaceEnd %W +]
}
bind Text <Control-Home> {
	tkTextSetCursor %W 1.0
}
bind Text <Control-Shift-Home> {
	tkTextKeySelect %W 1.0
}
bind Text <Control-End> {
	tkTextSetCursor %W {end - 1 char}
}
bind Text <Control-Shift-End> {
	tkTextKeySelect %W {end - 1 char}
}

bind Text <Tab> {
	tkTextInsertChar %W \t
	focus %W
	break
}
bind Text <Shift-Tab> {
	# Needed only to keep <Tab> binding from triggering; doesn't
	# have to actually do anything.
}
bind Text <Control-Tab> {
	focus [tk_focusNext %W]
}
bind Text <Control-Shift-Tab> {
	focus [tk_focusPrev %W]
}
bind Text <Control-i> {
	tkTextInsertChar %W \t
}
bind Text <Return> {
	tkTextInsertChar %W \n
	set tkText(%W,prevCmd) NewLine
}

catch {
    bind Text <apLineDel> {
		tkTextDelete %W insert [tkTextPlaceChar %W +] $tkBind(delSel) 0
	}
}

bind Text <Delete> {
	tkTextDelete %W insert [tkTextPlaceChar %W +] $tkBind(delSel) 0
}
bind Text <BackSpace> {
	tkTextDelete %W insert [tkTextPlaceChar %W -] $tkBind(delSel) 0
}

bind Text <Control-backslash> {tkTextKeyCancel %W}

bind Text <Insert> {
	set tkText(%W,ovwrt) [expr !$tkText(%W,ovwrt)]
	if $tkText(%W,ovwrt) {
		set tkBind(%W,mesg) "Entering overwrite mode."
	} else {
		set tkBind(%W,mesg) "Leaving overwrite mode."
	}
}

#key bindings for (printable) keys
bind Text <Any-KeyPress> {
		tkTextInsertChar %W %A
	}
# Special bindings to numeric keys for arguments
for {set n 0} {${n} < 10} {incr n} {
    bind Text <KeyPress-${n}> {tkTextNumKey %W %A}
}
#those bindings are important, it works without this
#bindings on a specified platform _BUT_ don't work
#on another platform.
bind Text <at> {tkTextInsertChar %W %A}
bind Text <numbersign> {tkTextInsertChar %W %A}
bind Text <bracketleft> {tkTextInsertChar %W %A}
bind Text <bracketright> {tkTextInsertChar %W %A}
bind Text <braceleft> {tkTextInsertChar %W %A}
bind Text <braceright> {tkTextInsertChar %W %A}
bind Text <backslash> {tkTextInsertChar %W %A}
bind Text <brokenbar> {tkTextInsertChar %W %A}
bind Text <greater> {tkTextInsertChar %W %A}
bind Text <quotedbl> {tkTextInsertChar %W %A}
bind Text <parenright> {tkTextInsertChar %W %A}
catch {bind Text <dead_tilde> {tkTextInsertChar %W %A}}
catch {bind Text <dead_circumflex> {tkTextInsertChar %W %A}}
catch {bind Text <dead_grave> {tkTextInsertChar %W %A}}

#this binding is needed for the french keyboard to
#insert "|", it's bound on <AltGr-minus>
bind Text <minus> {tkTextInsertChar %W %A}

# Ignore all Alt, Meta, and Control keypresses unless explicitly bound.
# Otherwise, if a widget binding for one of these is defined, the
# <KeyPress> class binding will also fire and insert the character,
# which is wrong. Ditto for <Escape>.

bind Text <Alt-KeyPress> {# nothing}
bind Text <Meta-KeyPress> {# nothing}
bind Text <Escape> {# nothing}

# Additional emacs-like bindings:
bind Text <Control-a> {
	tkTextSetCursor %W [tkTextPlaceHome %W +]
}
bind Text <Control-b> {
	tkTextSetCursor %W [tkTextPlaceChar %W -]
}
bind Text <Control-d> {
	tkTextDelete %W insert [tkTextPlaceChar %W +] $tkBind(delSel) 0
}
bind Text <Control-e> {
	tkTextSetCursor %W [tkTextPlaceEnd %W +]
}
bind Text <Control-f> {
	tkTextSetCursor %W [tkTextPlaceChar %W +]
}
bind Text <Control-g> {tkTextKeyCancel %W}
bind Text <Control-h> {
	tkTextDelete %W insert [tkTextPlaceChar %W -] $tkBind(delSel) 0
}
bind Text <Control-k> {
	if [string length $tkBind(%W,arg)] {
		if {$tkBind(%W,arg) > -1} {
			set sign "+1"
		} else {
			set sign -1
		}
	} else { set sign "+1" }
		set ndx [tkTextPlaceEnd %W +]
		if [%W compare $ndx == insert] {
		set ndx [%W index "$ndx $sign chars"]
	} 
	tkTextDelete %W insert $ndx 0 1
}
bind Text <Control-n> {
	set pos [tkTextPlaceLine %W +]
	tkTextSetCursor %W $pos
	set tkText(%W,prevPos) $pos
}
bind Text <Control-t> {tkTextTranspose %W}
bind Text <Control-v> {
	tkTextSetCursor %W [tkTextScrollPages %W +]
}
#Control-w is reserved for closing window
#bind Text <Control-w> { tkTextCut %W }
bind Text <Control-y> { tkTextYank %W }
bind Text <Control-slash> {tkTextUndo %W}
bind Text <Control-underscore> {tkTextUndo %W}
bind Text <Control-space> {tkTextSetMark %W insert}
bind Text <Select> {tkTextSetMark %W insert}

bind Text <F16> { tkTextCopy %W }
bind Text <F20> { tkTextCut %W }
bind Text <F18> { tkTextPaste %W }

upvar #0 sn_control_x ctrlx
if {![info exists ctrlx]} {
    set ctrlx "g"
}
bind Text <Control-${ctrlx}> {
	tkBindSetStateKey %W TextCX C-x
}
bind TextCX <KeyPress> {
	if {[lsearch $tkBind(modKeys) %K] > -1} {
		break
	}
	set tkBind(%W,mesg) "C-x [tkBindGetMod %s]%K not bound."
	eval $tkBind(bell)
}
bind TextCX <ButtonPress> {
	set tkBind(%W,mesg) "C-x [tkBindGetMod %s]mouse-%b not bound."
	eval $tkBind(bell)
}
bind TextCX <KeyPress-f> { tkTextSetFillCol %W }
bind TextCX <KeyPress-h> { tkTextSelectAll %W }
bind TextCX <KeyPress-u> { tkTextUndo %W }
bind TextCX <KeyPress-w> { set tkBind(%W,mesg) "BBox: [%W bbox insert]" }
bind TextCX <Control-g> {tkTextKeyCancel %W}
bind TextCX <Control-o> { tkTextEatLines %W }
bind TextCX <Control-x> { tkTextExchangeMark %W }
bind TextCX <Control-e> { tkTextEvalSel %W }

bind Text <Escape> {
	tkBindSetStateKey %W TextEsc Esc-
}
bind TextEsc <KeyPress> {
	if {[lsearch $tkBind(modKeys) %K] > -1} break
	set tkBind(%W,mesg) "ESC [tkBindGetMod %s]%K not bound."
	eval $tkBind(bell)
}
bind TextEsc <ButtonPress> {
	set tkBind(%W,mesg) "ESC [tkBindGetMod %s]mouse-%b not bound."
	eval $tkBind(bell)
}

bind TextEsc <KeyPress-b> {
	tkTextSetCursor %W [tkTextPlaceWord %W -]
}
bind TextEsc <KeyPress-d> {
	tkTextDelete %W insert [tkTextPlaceWord %W +] 0 1
}
bind TextEsc <KeyPress-f> {
	tkTextSetCursor %W [tkTextPlaceWord %W +]
}
bind TextEsc <KeyPress-h> {tkTextMarkPara %W}
bind TextEsc <KeyPress-q> {tkTextFormatPara %W}
bind TextEsc <KeyPress-v> {
	tkTextSetCursor %W [tkTextScrollPages %W -]
}
bind TextEsc <KeyPress-w> { tkTextCopy %W }
bind TextEsc <KeyPress-y> {tkTextYankPop %W}
bind TextEsc <KeyPress-backslash> {tkTextEatSpace %W}
bind TextEsc <KeyPress-braceright> {
	tkTextSetCursor %W [tkTextPlacePara %W +]
}
bind TextEsc <KeyPress-braceleft> {
	tkTextSetCursor %W [tkTextPlacePara %W -]
}
bind TextEsc <KeyPress-less> {
	tkTextSetCursor %W 1.0
}
bind TextEsc <KeyPress-greater> {
	tkTextSetCursor %W end-1c
}
bind TextEsc <KeyPress-BackSpace> {
	tkTextDelete %W insert [tkTextPlaceWord %W -] 0 1
}
bind TextEsc <KeyPress-Delete> {
	tkTextDelete %W insert [tkTextPlaceWord %W -] 0 1
}
bind TextEsc <Control-g> {tkTextKeyCancel %W}

# Special bindings to numeric keys for arguments
for {set n 0} {${n} < 10} {incr n} {
    bind TextEsc <KeyPress-${n}> {tkBindArgKey %W %A}
}
bind TextEsc <KeyPress-minus> {tkBindArgKey %W %A}

# Meta key bindings
if {![catch "bind Text <$tkBind(meta)-b>"]} {
    bind Text <$tkBind(meta)-b> {
		tkTextSetCursor %W [tkTextPlaceWord %W -]
	}
    bind Text <$tkBind(meta)-d> {
		tkTextDelete %W insert [tkTextPlaceWord %W +] 0 1
	}
    bind Text <$tkBind(meta)-f> {
		tkTextSetCursor %W [tkTextPlaceWord %W +]
	}
    bind Text <$tkBind(meta)-h> {tkTextMarkPara %W}
    bind Text <$tkBind(meta)-q> {tkTextFormatPara %W}
    bind Text <$tkBind(meta)-v> {
		tkTextSetCursor %W [tkTextScrollPages %W -]
	}
    bind Text <$tkBind(meta)-w> {tkTextCopy %W}
    bind Text <$tkBind(meta)-y> {tkTextYankPop %W}
    bind Text <$tkBind(meta)-backslash> {tkTextEatSpace %W}
    bind Text <$tkBind(meta)-braceright> {
		tkTextSetCursor %W [tkTextPlacePara %W +]
	}
    bind Text <$tkBind(meta)-braceleft> {
		tkTextSetCursor %W [tkTextPlacePara %W -]
	}
    bind Text <$tkBind(meta)-less> {
		tkTextSetCursor %W 1.0
	}
    bind Text <$tkBind(meta)-greater> {
		tkTextSetCursor %W end-1c
	}
    bind Text <$tkBind(meta)-BackSpace> {
		tkTextDelete %W insert [tkTextPlaceWord %W -] 0 1
	}
    bind Text <$tkBind(meta)-Delete> {
		tkTextDelete %W insert [tkTextPlaceWord %W -] 0 1
	}
    # Special bindings to numeric keys for arguments
    for {set n 0} {${n} < 10} {incr n} {
        bind Text <$tkBind(meta)-KeyPress-${n}> {tkBindArgKey %W %A}
    }
    bind Text <$tkBind(meta)-minus> {tkBindArgKey %W %A}
}

bind Text <Control-z> {tkTextUndo %W}
bind Text <Control-y> {set tkText(%W,prevCmd) "Redo"; tkTextUndo %W}
bind Text <Control-c> { 
	if {[selection own -displayof %W] == "%W"} { tkTextCopy %W }
}
bind Text <Control-x> { 
	if {[selection own -displayof %W] == "%W"} {
		catch {tkTextDelete %W sel.first sel.last 1 1}
	}
}
bind Text <Control-v> {tkTextPaste %W}

proc tkTextPaste w {
    if {![catch {${w} get sel.first sel.last}]} {
        if {![catch {set str [selection get -displayof ${w}\
          -selection CLIPBOARD]}]} {

            if {[${w} compare insert >= sel.first] && [${w} compare insert <=\
              sel.last]} {
                tkTextReplace ${w} [${w} index sel.first] [${w} index\
                  sel.last] ${str}
            } else {
                tkTextInsertChar ${w} ${str}
            }
        }
    }\
    elseif {![catch {set str [selection get -displayof ${w}]}]} {
        tkTextInsert ${w} insert ${str}
    }\
    elseif {![catch {set str [selection get -displayof ${w}\
      -selection CLIPBOARD]}]} {
        tkTextInsert ${w} insert ${str}
    }
}

# tkTextKeyCancel --
# Cancels everything

proc tkTextKeyCancel w {
    global tkText tkBind
    ${w} tag remove sel 1.0 end
    tkBindCancelStateKey ${w}
    set tkText(${w},markActive) 0
    set tkBind(${w},arg) {}
    set tkText(${w},prevCmd) KeyCancel
    set tkBind(${w},mesg) Cancel.
}

# tkTextButtonInsert --
# Do the mouse button insertion in XTerm fashion, looking to the
# clipboard if no PRIMARY selection is found.
#
# Arguments:
# w -		The text window in which to insert.
# x -		The x-coordinate of the button press.
# y -		The x-coordinate of the button press.

proc tkTextButtonInsert {w x y} {
    global tkText tkBind

    if {[${w} cget -state] != "normal"} {
        return
    }
    if $tkBind(insertAtClick) {
        ${w} mark set insert @${x},${y}
        ${w} mark set anchor insert
    }
    set sv $tkBind(delSel)
    set tkBind(delSel) 0

    if {[catch {set str [selection get -displayof ${w}]}]} {
        catch {tkTextInsertChar ${w} [selection get -displayof ${w}\
          -selection CLIPBOARD]}
    } else {
        tkTextInsertChar ${w} ${str}
        catch {
            clipboard clear -displayof ${w}
            clipboard append -displayof ${w} -- ${str}
        }
    }
    set tkBind(delSel) ${sv}
}

# tkTextButton1 --
# This procedure is invoked to handle button-1 presses in text
# widgets. It moves the insertion cursor, sets the selection anchor,
# and claims the input focus.
#
# Arguments:
# w -		The text window in which the button was pressed.
# x -		The x-coordinate of the button press.
# y -		The x-coordinate of the button press.

proc tkTextButton1 {w x y} {
    global tkText tkPriv

    tkTextButton3 ${w} ${x} ${y}
    ${w} mark set insert @${x},${y}
    set tkText(${w},prevCmd) Button1
}

# tkTextButton3 --
# This procedure is invoked to handle button-3 presses in text
# widgets. It sets the selection anchor and claims the input focus.
#
# Arguments:
# w -		The text window in which the button was pressed.
# x -		The x-coordinate of the button press.
# y -		The x-coordinate of the button press.

proc tkTextButton3 {w x y} {
    global tkText tkPriv tkBind

    set tkPriv(selectMode) char
    set tkPriv(mouseMoved) 0
    set tkPriv(pressX) ${x}
    ${w} mark set anchor @${x},${y}
    if {[${w} cget -state] == "normal"} {
        focus ${w}
    }
    set tkText(${w},markActive) 0
    set tkBind(${w},arg) {}
    set tkText(${w},prevCmd) Button3
    set tkBind(${w},mesg) {}
}

# tkTextSelectTo --
# This procedure is invoked to extend the selection, typically when
# dragging it with the mouse. Depending on the selection mode (character,
# word, line) it selects in different-sized units. This procedure
# ignores mouse motions initially until the mouse has moved from
# one character to another or until there have been multiple clicks.
#
# Arguments:
# w -		The text window in which the button was pressed.
# x -		Mouse x position.
# y - 		Mouse y position.

proc tkTextSelectTo {w x y} {
    global tkText tkPriv tkBind

    set cur [${w} index @${x},${y}]
    if [catch {${w} index anchor}] {
        ${w} mark set anchor ${cur}
    }
    set anchor [${w} index anchor]
    if {[${w} compare ${cur} != ${anchor}] ||(abs($tkPriv(pressX) - ${x}) >=\
      3)} {
        set tkPriv(mouseMoved) 1
    }
    switch $tkPriv(selectMode) {
        char {
                if [${w} compare ${cur} < anchor] {
                    set first ${cur}
                    set last anchor
                } else {
                    set first anchor
                    set last [${w} index "${cur} + 1c"]
                }
            }
        word {
                if [${w} compare ${cur} < anchor] {
                    set first [${w} index "${cur} wordstart"]
                    set last [${w} index "anchor - 1c wordend"]
                } else {
                    set first [${w} index "anchor wordstart"]
                    set last [${w} index "${cur} wordend"]
                }
            }
        line {
                if [${w} compare ${cur} < anchor] {
                    set first [${w} index "${cur} linestart"]
                    set last [${w} index "anchor - 1c lineend + 1c"]
                } else {
                    set first [${w} index "anchor linestart"]
                    set last [${w} index "${cur} lineend + 1c"]
                }
            }
    }
    if {$tkPriv(mouseMoved) ||($tkPriv(selectMode) != "char")} {
        ${w} tag remove sel 0.0 ${first}
        ${w} tag add sel ${first} ${last}
        ${w} tag raise sel
        ${w} tag remove sel ${last} end
        update idletasks
    }

    set tkBind(${w},arg) {}
    set tkText(${w},prevCmd) SelectTo
    set tkBind(${w},mesg) {}
}

proc tkTextWordIndex {w i n} {
    global tkText tkBind

    set ndx [${w} index ${i}]
    if {${n} > 0} {
        for {} {${n} > 0} {incr n -1} {
            while {[regexp $tkBind(notWord) [${w} get ${ndx}]] &&\
              [${w} compare ${ndx} < end]} {
                set ndx [${w} index "${ndx}+1c"]
            }
            while {![regexp $tkBind(notWord) [${w} get ${ndx}]] &&\
              [${w} compare ${ndx} < end]} {
                set ndx [${w} index "${ndx}+1c"]
            }
        }
    } else {
        for {set i 0} {${i} > ${n}} {incr i -1} {
            set ndx [${w} index "${ndx}-1c"]
            while {[regexp $tkBind(notWord) [${w} get ${ndx}]] &&\
              [${w} compare insert > 1.0]} {
                set ndx [${w} index "${ndx}-1c"]
            }
            while {![regexp $tkBind(notWord) [${w} get ${ndx}]] &&\
              [${w} compare insert > 1.0]} {
                set ndx [${w} index "${ndx}-1c"]
            }
        }
        set ndx [${w} index "${ndx}+1c"]
    }

    return ${ndx}
}


# tkTextAutoScan --
# This procedure is invoked when the mouse leaves a text window
# with button 1 down. It scrolls the window up, down, left, or right,
# depending on where the mouse is (this information was saved in
# tkPriv(x) and tkPriv(y)), and reschedules itself as an "after"
# command so that the window continues to scroll until the mouse
# moves back into the window or the mouse button is released.
#
# Arguments:
# w -		The text window.

proc tkTextAutoScan {w} {
    global tkText tkPriv
    if {$tkPriv(y) >= [winfo height ${w}]} {
        ${w} yview scroll 2 units
    }\
    elseif {$tkPriv(y) < 0} {
        ${w} yview scroll -2 units
    }\
    elseif {$tkPriv(x) >= [winfo width ${w}]} {
        ${w} xview scroll 2 units
    }\
    elseif {$tkPriv(x) < 0} {
        ${w} xview scroll -2 units
    } else {
        return
    }
    tkTextSelectTo ${w} $tkPriv(x) $tkPriv(y)
    set tkPriv(afterId) [after 50 tkTextAutoScan ${w}]
}

# tkTextSetCursor --
# Select whole text buffer
#
# Arguments:
# w -		The text window.

proc tkTextSelectAll w {
    global tkText tkBind

    ${w} tag add sel 1.0 end
    ${w} tag raise sel
    ${w} mark set emacs "end -1c"
    ${w} mark set anchor emacs
    ${w} mark set insert 1.0

    set tkBind(${w},arg) {}
    set tkText(${w},prevCmd) SelectAll
    set tkBind(${w},mesg) {}
}

# tkTextSetCursor
# Move the insertion cursor to a given position in a text. Also
# clears the selection, if there is one in the text, and makes sure
# that the insertion cursor is visible. Also, don't let the insertion
# cursor appear on the dummy last line of the text.
#
# Arguments:
# w -		The text window.
# pos -		The desired new position for the cursor in the window.

proc tkTextSetCursor {w pos} {
    global tkText tkBind

    if $tkText(${w},markActive) {
        return [tkTextKeySelect ${w} ${pos}]
    }

    if [${w} compare ${pos} == end] {
        set pos {end - 1 chars}
    }
    ${w} mark set insert ${pos}

    ${w} tag remove sel 1.0 end
    ${w} see insert

    set tkText(${w},prevPos) {}
    set tkBind(${w},arg) {}
    set tkText(${w},prevCmd) SetCursor
    set tkBind(${w},mesg) {}
}

# tkTextKeySelect
# This procedure is invoked when stroking out selections using the
# keyboard. It moves the cursor to a new position, then extends
# the selection to that position.
#
# Arguments:
# w -		The text window.
# new -		A new position for the insertion cursor (the cursor hasn't
#		actually been moved to this position yet).

proc tkTextKeySelect {w new} {
    global tkText tkBind

    if {[${w} tag nextrange sel 1.0 end] == ""} {
        if [${w} compare ${new} < insert] {
            ${w} tag add sel ${new} insert
        } else {
            ${w} tag add sel insert ${new}
        }
        ${w} mark set anchor insert
    } else {
        if [${w} compare ${new} < anchor] {
            set first ${new}
            set last anchor
        } else {
            set first anchor
            set last ${new}
        }
        ${w} tag remove sel 1.0 ${first}
        ${w} tag add sel ${first} ${last}
        ${w} tag remove sel ${last} end
    }
    ${w} tag raise sel
    ${w} mark set insert ${new}
    ${w} see insert
    update idletasks

    set tkBind(${w},arg) {}
    set tkText(${w},prevCmd) KeySelect
    set tkBind(${w},mesg) {}
}

# tkTextResetAnchor --
# Set the selection anchor to whichever end is farthest from the
# index argument. One special trick: if the selection has two or
# fewer characters, just leave the anchor where it is. In this
# case it doesn't matter which point gets chosen for the anchor,
# and for the things like Shift-Left and Shift-Right this produces
# better behavior when the cursor moves back and forth across the
# anchor.
#
# Arguments:
# w -		The text widget.
# index -	Position at which mouse button was pressed, which determines
#		which end of selection should be used as anchor point.

proc tkTextResetAnchor {w index} {
    global tkText tkBind

    if {[${w} tag ranges sel] == ""} {
        ${w} mark set anchor ${index}
        return
    }
    set a [${w} index ${index}]
    set b [${w} index sel.first]
    set c [${w} index sel.last]
    if [${w} compare ${a} < ${b}] {
        ${w} mark set anchor sel.last
        return
    }
    if [${w} compare ${a} > ${c}] {
        ${w} mark set anchor sel.first
        return
    }
    scan ${a} "%d.%d" lineA chA
    scan ${b} "%d.%d" lineB chB
    scan ${c} "%d.%d" lineC chC
    if {${lineB} < ${lineC}+2} {
        set total [string length [${w} get ${b} ${c}]]
        if {${total} <= 2} {
            return
        }
        if {[string length [${w} get ${b} ${a}]] <(${total}/2)} {
            ${w} mark set anchor sel.last
        } else {
            ${w} mark set anchor sel.first
        }
        return
    }
    if {(${lineA}-${lineB}) <(${lineC}-${lineA})} {
        ${w} mark set anchor sel.last
    } else {
        ${w} mark set anchor sel.first
    }

    set tkText(${w},markActive) 0
    set tkBind(${w},arg) {}
    set tkText(${w},prevCmd) ResetAnchor
    set tkBind(${w},mesg) {}
}

# tkTextInsertChar --
# Insert a string into a text at the point of the insertion cursor.
# If there is a selection in the text, and it covers the point of the
# insertion cursor, then delete the selection before inserting.
#
# Arguments:
# w -		The text window in which to insert the string
# s -		The string to insert (usually just a single character)

proc tkTextInsertChar {w s} {
    global tkText tkBind

    if {${s} == "" || ${s} == "{}" || [${w} cget -state] == "disabled"} {
        return "no"
    }

    set txt {}
    set cutbuf {}
    for {set n [tkBindDefArg ${w} +]} {${n} > 0} {incr n -1} {
        append txt ${s}
    }
    if {$tkBind(delSel) && [${w} tag nextrange sel 1.0 end] != "" &&\
      [${w} compare sel.first <= insert] && [${w} compare sel.last >= insert]} {
        set cutbuf [tkTextCopyTagBuffer ${w} sel.first sel.last]
        SyncEditors $w delete sel.first sel.last
        catch {${w} delete sel.first sel.last}
    }\
    elseif $tkText(${w},ovwrt) {
        set ndx [${w} index "insert + [string length ${txt}] chars"]
        if [${w} compare ${ndx} > "insert lineend"] {
            set ndx "insert lineend"
        }
        set cutbuf [tkTextCopyTagBuffer ${w} insert ${ndx}]
        ${w} delete insert ${ndx}
    }

    set start [${w} index insert]
    set high_idx ${start}
    SyncEditors $w insert insert $txt
    ${w} insert insert ${txt}
    ${w} see insert
    #update idletasks
    set y [lindex [${w} bbox insert] 1]
    if {$tkText(${w},fillCol) && [lindex [${w} bbox insert] 0] >\
      $tkText(${w},fillWidth)} {
        tkTextUndoBeginGroup ${w} wrap
        set wrap 1
    } else {
        set wrap 0
    }

    if {[info exists tkText(${w},undoCnt)]} {
        if {$tkText(${w},ovwrt) || $tkText(${w},prevCmd) != "InsertChar" ||\
          $tkText(${w},undoLast) != ${start} || [string length\
          $tkText(${w},undoCut)]} {
            tkTextUndoPush ${w} ${cutbuf} ${start} insert
        } else {
            set tkText(${w},undoLast) [${w} index insert]
        }
    }

    if ${wrap} {
        ${w} mark set fillstart insert
        ${w} mark set insert "@$tkText(${w},fillWidth),${y}"
        tkTextWrapWord ${w}
        tkTextUndoEndGroup ${w} wrap
        ${w} mark set insert fillstart
    }

    uplevel #0 "set tkText(${w},toplevel).linenum \[${w} index insert\]"

    ${w} see insert
    ${w} tag remove sel 1.0 end
    set tkText(${w},markActive) 0
    set tkText(${w},prevCmd) InsertChar
    set tkBind(${w},mesg) {}

    synch_highlight ${w} ${high_idx}
    return ""
}

# tkTextInsert --
# Wrapper around normal text insert to handle undo and stuff
#
# Arguments:
# w -		The text window in which to insert the string
# ndx -		Point to insert text at.
# args -	As in 'text insert' -> string taglist ...

proc tkTextInsert {w ndx args} {
    global tkText tkBind

    if {([${w} cget -state] == "disabled") || ![llength ${args}]} return
    set ins [${w} index insert]
    set ndx [${w} index ${ndx}]
    ${w} mark set insert ${ndx}

    eval "SyncEditors $w insert insert ${args}"
    eval "${w} insert insert ${args}"
    synch_highlight ${w} ${ndx}

    tkTextUndoPush ${w} {} ${ndx} insert

    ${w} see insert

    #	$w mark set insert $ins
    set tkText(${w},markActive) 0
    set tkBind(${w},arg) {}
    set tkText(${w},prevCmd) Insert
}

# tkTextReplace --
# Wrapper around normal text insert & delete to handle undo and stuff
#
# Arguments:
# w -		The text window in which to insert the string
# ndx1 -	Start point of text to replace.
# ndx2 -	End point of text to replace.
# s -		The string to insert
# t -		List of tags to apply to inserted text

proc tkTextReplace {w ndx1 ndx2 s {t {}}} {
    global tkText tkBind

    if {[${w} cget -state] == "disabled"} return
    if [${w} compare ${ndx1} < ${ndx2}] {
        set cutbuf [tkTextCopyTagBuffer ${w} ${ndx1} ${ndx2}]
	SyncEditors $w delete ${ndx1} ${ndx2}
        ${w} delete ${ndx1} ${ndx2}
    } else {
        set cutbuf [tkTextCopyTagBuffer ${w} ${ndx2} ${ndx1}]
	SyncEditors $w delete ${ndx2} ${ndx1}
        ${w} delete ${ndx2} ${ndx1}
    }

    set ndx1 [${w} index ${ndx1}]
    ${w} mark set insert ${ndx1}
    SyncEditors $w insert ${ndx1} $s $t
    ${w} insert ${ndx1} ${s} ${t}
    tkTextUndoPush ${w} ${cutbuf} ${ndx1} insert
    ${w} see insert

    ${w} tag remove sel 1.0 end
    set tkText(${w},markActive) 0
    set tkBind(${w},arg) {}
    set tkText(${w},prevCmd) Replace
    set tkBind(${w},mesg) {}
    synch_highlight ${w} ${ndx1}
}

# tkTextDelete --
# Deletes characters between text indices 'ndx1' and 'ndx2'.
#
# Arguments:
# w -		The text window in which to delete
# ndx1,ndx2 -	Text indices surrounding text to delete
# dsel		If true, delete selection instead if it exists
# cut -		If true, add deletion to kill ri

proc tkTextDelete {w ndx1 {ndx2 {}} {dsel 0} {cut 0}} {
    global tkText tkBind

    if {[${w} cget -state] == "disabled"} return
    if {![string length ${ndx2}]} {
        set ndx2 ${ndx1}
    }

    if {$tkText(${w},prevCmd) == "Delete" && [${w} compare\
      $tkText(${w},killLast) == insert]} {
        set where 1
    } else {
        set where 0
    }

    if {${dsel} && ![catch {set cuttxt [${w} get sel.first sel.last]}] &&\
      [${w} compare sel.first <= insert] && [${w} compare sel.last >= insert]} {
        set start [${w} index sel.first]
        set cutbuf [tkTextCopyTagBuffer ${w} sel.first sel.last]
        SyncEditors $w delete sel.first sel.last
        ${w} delete sel.first sel.last
    } else {
        if [${w} compare ${ndx1} < ${ndx2}] {
            set start [${w} index ${ndx1}]
            set cuttxt [${w} get ${ndx1} ${ndx2}]
            set cutbuf [tkTextCopyTagBuffer ${w} ${ndx1} ${ndx2}]
            SyncEditors $w delete $ndx1 $ndx2
            ${w} delete ${ndx1} ${ndx2}
        } else {
            set start [${w} index ${ndx2}]
            set cuttxt [${w} get ${ndx1} ${ndx2}]

            set cutbuf [tkTextCopyTagBuffer ${w} ${ndx2} ${ndx1}]
            SyncEditors $w delete $ndx2 $ndx1
            ${w} delete ${ndx2} ${ndx1}
        }
        ${w} see insert
    }
    synch_highlight ${w} [${w} index insert]

    tkTextUndoPush ${w} ${cutbuf} ${start} ${start}
    if ${cut} {
        tkTextPushTagBuffer ${cutbuf} ${where}
        set tkText(${w},killLast) [${w} index insert]
        clipboard clear -displayof ${w}
        clipboard append -displayof ${w} ${cuttxt}
    }
    set tkText(${w},markActive) 0
    set tkBind(${w},arg) {}
    set tkText(${w},prevCmd) Delete
    set tkBind(${w},mesg) {}

    uplevel #0 "set tkText(${w},toplevel).linenum \[${w} index insert\]"
}

# tkTextEatSpace --
# Deletes whitespace characters around insert mark
#
# Arguments:
# w -		The text window in which to eat space

proc tkTextEatSpace {w} {
    global tkText

    if {[${w} cget -state] == "disabled"} return
    set ndx [${w} index insert]
    while {[string match "\[ \t\]" [${w} get ${ndx}]]} {
        set ndx [${w} index "${ndx} +1 char"]
    }
    while {[string match "\[ \t\]" [${w} get "insert -1 char"]]} {
        ${w} mark set insert "insert -1 char"
    }
    if {[${w} compare insert < ${ndx}]} {
        tkTextDelete ${w} insert ${ndx} 0 0
    }
}

# tkTextEatLines --
# Deletes blank lines around current line
#
# Arguments:
# w -		The text window in which to eat lines

proc tkTextEatLines {w} {
    global tkText

    if {[${w} cget -state] == "disabled"} return
    set ndx [${w} index insert]
    ${w} mark set insert "insert linestart"
    while {[${w} get "insert +1 line"] == "\n"} {
        ${w} mark set insert "insert +1 line"
    }
    set last [${w} index "insert + 1 line"]
    ${w} mark set insert "${ndx} linestart"
    if {[${w} get insert] == "\n"} {
        while {[${w} get "insert -1 line"] == "\n"} {
            ${w} mark set insert "insert -1 line"
        }
        set start [${w} index insert]
    } else {
        set start [${w} index "insert +1 line"]
    }

    ${w} mark set insert ${ndx}
    if {[${w} compare ${start} < ${last}]} {
        tkTextDelete ${w} ${start} ${last} 0 0
    }
}

# tkTextYank --
# Paste contents from kill buffer stack in to text widget
#
# Arguments:
# w -		The text window in which to yank
# n -		Depth in to kill buffer stack

proc tkTextYank {w {n 1}} {
    global tkText tkBind

    if {[${w} cget -state] == "disabled"} return

    set n [tkBindDefArg ${w} ${n}]
    set rng [tkTextInsertTagBuffer ${w} insert [tkTextGetTagBuffer ${n}]]
    set ndx [lindex ${rng} 0]
    ${w} mark set emacs ${ndx}
    ${w} mark set anchor ${ndx}
    tkTextUndoPush ${w} {} ${ndx} [lindex ${rng} 1]
    ${w} see insert

    ${w} tag remove sel 1.0 end
    set tkText(${w},markActive) 0
    set tkText(${w},prevCmd) Yank
    set tkBind(${w},mesg) {}
}

# tkTextYank --
# Replace previous yank with next contents of kill buffer stack
#
# Arguments:
# w -		The text window in which to yank
# n -		Depth in to kill buffer stack

proc tkTextYankPop {w {n 1}} {
    global tkText tkBind

    if {$tkText(${w},prevCmd) != "Yank"} {
        eval $tkBind(bell)
        return
    }
    set n [tkBindDefArg ${w} ${n}]

    ${w} delete emacs insert

    set rng [tkTextInsertTagBuffer ${w} insert [tkTextGetTagBuffer [incr n]]]
    set ndx [lindex ${rng} 0]
    ${w} mark set emacs ${ndx}
    ${w} mark set anchor ${ndx}

    if {[info exists tkText(${w},undoCnt)]} {
        set tkText(${w},undoFirst) ${ndx}
        set tkText(${w},undoLast) [lindex ${rng} 1]
    }

    set tkText(${w},prevCmd) Yank
    set tkBind(${w},mesg) {}
}

# tkTextCopy --
# Place currently marked region onto kill buffer stack
#
# Arguments:
# w -		The text window in which to copy

proc tkTextCopy w {
    global tkText tkBind

    if {![catch {set str [${w} get sel.first sel.last]}]} {
        clipboard clear -displayof ${w}
        catch {
            clipboard append -displayof ${w} -- ${str}
        }
        tkTextPushTagBuffer [tkTextCopyTagBuffer ${w} sel.first sel.last] 0

        # $w tag remove sel 1.0 end
        set tkBind(${w},arg) {}
        set tkText(${w},prevCmd) Copy
        set tkBind(${w},mesg) {}
    }
}

# tkTextCut --
# Cut currently marked region onto kill buffer stack
#
# Arguments:
# w -		The text window in which to cut

proc tkTextCut {w} {
    if {[selection own -displayof ${w}] != ${w}} {
        tkTextCheckMark ${w} 1
    }
    tkTextDelete ${w} emacs insert 1 1
}

# tkTextReTag --
# Wrapper around tag add/remove commands to handle undo.
#
# Arguments:
# w -		The text window in which to modify count
# ndx1,ndx2 -	Text indices surrounding text to retag
# rlist - 	List of Tags to remove
# alist - 	List of Tags to add

proc tkTextReTag {w ndx1 ndx2 rlist alist} {
    global tkText tkBind

    set cutbuf [tkTextCopyTagBuffer ${w} ${ndx1} ${ndx2}]
    foreach tagname ${rlist} {
        ${w} tag remove ${tagname} ${ndx1} ${ndx2}
    }
    foreach tagname ${alist} {
        ${w} tag add ${tagname} ${ndx1} ${ndx2}
    }
    tkTextUndoPush ${w} ${cutbuf} ${ndx1} ${ndx2}

    set tkText(${w},markActive) 0
    set tkBind(${w},arg) {}
    set tkText(${w},prevCmd) ReTag
    set tkBind(${w},mesg) {}
}

# tkTextNumKey --
# Check if currenlty building a number argument and if so, append to
# the argument. Otherwise, insert the number in the text.
#
# Arguments:
# w -		The text window in which to yank
# a -		The ascii character of key (decimal number)

proc tkTextNumKey {w a} {
    global tkText tkBind
    if {![string length $tkBind(${w},arg)]} {
        tkTextInsertChar ${w} ${a}
    } else {
        tkBindArgKey ${w} ${a}
    }
}

# tkTextPlaceChar --
# Returns the index of the character that is 'n' characters
# away from the current index
#
# Arguments:
# w -		The text window in which the cursor is to move.
# n -		The number of chars to move: -1 for left one char,
#		+1 for right one char.

proc tkTextPlaceChar {w n {ndx insert}} {
    global tkText

    set n [tkBindDefArg ${w} ${n}]
    if {${n} > -1} {
        set n "+${n}"
    }
    return [${w} index "${ndx} ${n} char"]
}

# tkTextPlaceWord --
# Returns the index of the character that is 'n' words
# away from the current index
#
# Arguments:
# w -		The text window in which the cursor is to move.
# n -		The number of words to move: -1 for left one word,
#		+1 for right one word.

proc tkTextPlaceWord {w n {ndx insert}} {
    global tkText

    set n [tkBindDefArg ${w} ${n}]
    return [tkTextWordIndex ${w} ${ndx} ${n}]
}

# tkTextPlaceHome --
# Moves cursor to beginning of current line and then 'n-1' lines
#
# Arguments:
# w -		The text window in which the cursor is to move.
# n -		The number of lines to move: 0 for up one line,
#		+2 for down one line.

proc tkTextPlaceHome {w n {ndx insert}} {
    global tkText

    set n [tkBindDefArg ${w} ${n}]
    incr n -1
    set ndx [tkTextPlaceLine ${w} ${n} ${ndx}]
    return [${w} index "${ndx} linestart"]
}

# tkTextPlaceEnd --
# Moves cursor to end of current line and then 'n-1' lines
#
# Arguments:
# w -		The text window in which the cursor is to move.
# n -		The number of lines to move: 0 for up one line,
#		+2 for down one line.

proc tkTextPlaceEnd {w n {ndx insert}} {
    global tkText

    set n [tkBindDefArg ${w} ${n}]
    incr n -1
    set ndx [tkTextPlaceLine ${w} ${n} ${ndx}]
    return [${w} index "${ndx} lineend"]
}

# tkTextPlaceLine --
# Returns the index of the character one line above or below the
# insertion cursor. There are two tricky things here. First,
# we want to maintain the original column across repeated operations,
# even though some lines that will get passed through don't have
# enough characters to cover the original column. Second, don't
# try to scroll past the beginning or end of the text.
#
# Arguments:
# w -		The text window in which the cursor is to move.
# n -		The number of lines to move: -1 for up one line,
#		+1 for down one line.

proc tkTextPlaceLine {w n {ndx insert}} {
    global tkText

    set i [${w} index ${ndx}]
    scan ${i} "%d.%d" line char
    if {[string compare $tkText(${w},prevPos) ${i}] != 0 ||\
      $tkText(${w},prevCmd) != "SetCursor"} {
        set tkText(${w},char) [lindex [${w} bbox ${i}] 0]
    }
    set n [tkBindDefArg ${w} ${n}]
    set new [${w} index [expr ${line} + ${n}].0]
    ${w} see ${new}
    set y [lindex [${w} bbox ${new}] 1]
    if {[catch {set new [${w} index "@$tkText(${w},char),${y}"]}]} {
        set new "insert"
    }
    if {[${w} compare ${new} == end] || [${w} compare ${new} ==\
      "${ndx} linestart"]} {
        set new ${i}
    }
    return ${new}
}

# tkTextPlacePara --
# Returns the index of the beginning of the 'n'th paragraph just
# before or after a given position in the text (the beginning of a
# paragraph is the first non-blank character after a blank line).
#
# Arguments:
# w -		The text window in which the cursor is to move.
# n -		Number of paragraphs to move: -1 for up paragraphs,
#		+1 for down paragrap

proc tkTextPlacePara {w n {ndx insert}} {
    global tkText

    set n [tkBindDefArg ${w} ${n}]
    set ndx [${w} index ${ndx}]
    if {${n} > -1} {
        for {} {${n} > 0} {incr n -1} {
            set ndx [tkTextNextPara ${w} ${ndx}]
        }
    } else {
        for {} {${n} < 0} {incr n} {
            set ndx [tkTextPrevPara ${w} ${ndx}]
        }
    }
    return ${ndx}
}

# tkTextPrevPara --
# Returns the index of the beginning of the paragraph just before a given
# position in the text (the beginning of a paragraph is the first non-blank
# character after a blank line).
#
# Arguments:
# w -		The text window in which the cursor is to move.
# pos -		Position at which to start search.

proc tkTextPrevPara {w pos} {
    set pos [${w} index "${pos} linestart"]
    while 1 {
        if {(([${w} get "${pos} - 1 line"] == "\n") &&([${w} get ${pos}] !=\
          "\n")) ||(${pos} == "1.0")} {
            if [regexp -indices {^[ 	]+(.)} [${w} get ${pos} "${pos} lineend"]\
              dummy index] {
                set pos [${w} index "${pos} + [lindex ${index} 0] chars"]
            }
            if {[${w} compare ${pos} != insert] ||(${pos} == "1.0")} {
                return ${pos}
            }
        }
        set pos [${w} index "${pos} - 1 line"]
    }
}

# tkTextNextPara --
# Returns the index of the beginning of the paragraph just after a given
# position in the text (the beginning of a paragraph is the first non-blank
# character after a blank line).
#
# Arguments:
# w -		The text window in which the cursor is to move.
# start -	Position at which to start search.

proc tkTextNextPara {w start} {
    set pos [${w} index "${start} linestart + 1 line"]
    while {[${w} get ${pos}] != "\n"} {
        if [${w} compare ${pos} == end] {
            return [${w} index "end - 1c"]
        }
        set pos [${w} index "${pos} + 1 line"]
    }
    while {[${w} get ${pos}] == "\n"} {
        set pos [${w} index "${pos} + 1 line"]
        if [${w} compare ${pos} == end] {
            return [${w} index "end - 1c"]
        }
    }
    if [regexp -indices {^[ 	]+(.)} [${w} get ${pos} "${pos} lineend"] dummy\
      index] {
        return [${w} index "${pos} + [lindex ${index} 0] chars"]
    }
    return ${pos}
}

# tkTextScrollPages --
# This is a utility procedure used in bindings for moving up and down
# pages and possibly extending the selection along the way. It scrolls
# the view in the widget by the number of pages, and it returns the
# index of the character that is at the same position in the new view
# as the insertion cursor used to be in the old view.
#
# Arguments:
# w -		The text window in which the cursor is to move.
# count -	Number of pages forward to scroll; may be negative
#		to scroll backwards.

proc tkTextScrollPages {w count} {
    set count [tkBindDefArg ${w} ${count}]
    set bbox [${w} bbox insert]
    ${w} yview scroll ${count} pages
    if {${bbox} == ""} {
        return [${w} index @[expr [winfo height ${w}]/2],0]
    }
    return [${w} index @[lindex ${bbox} 0],[lindex ${bbox} 1]]
}

# tkTextTranspose --
# This procedure implements the "transpose" function for text widgets.
# It tranposes the characters on either side of the insertion cursor,
# unless the cursor is at the end of the line. In this case it
# transposes the two characters to the left of the cursor. In either
# case, the cursor ends up to the right of the transposed characters.
#
# Arguments:
# w -		Text window in which to transpose.

proc tkTextTranspose w {
    global tkText tkBind

    ${w} tag remove sel 1.0 end
    set pos [${w} index insert]
    set cutbuf [tkTextCopyTagBuffer ${w} "insert - 1 c" "insert + 1 c"]

    if [${w} compare ${pos} != "${pos} lineend"] {
        set pos [${w} index "${pos} + 1 char"]
    }
    set new [${w} get "${pos} - 1 char"][${w} get "${pos} - 2 char"]
    if [${w} compare "${pos} - 1 char" == 1.0] {
        return
    }
    ${w} delete "${pos} - 2 char" ${pos}
    ${w} insert insert ${new}
    tkTextUndoPush ${w} ${cutbuf} "${pos} - 2 char" ${pos}
    ${w} see insert

    set tkText(${w},markActive) 0
    set tkBind(${w},arg) {}
    set tkText(${w},prevCmd) Transpose
    set tkBind(${w},mesg) {}
}

proc tkTextEvalSel w {
    global tkText tkBind

    if {[${w} tag nextrange sel 1.0 end] != ""} {
        set txt [uplevel #0 "eval \[${w} get sel.first sel.last\]"]
    } else {
        if [${w} compare emacs < insert] {
            set txt [uplevel #0 "eval \[${w} get emacs insert\]"]
        } else {
            set txt [uplevel #0 "eval \[${w} get insert emacs\]"]
        }
    }
    regsub -all \n ${txt} "^J" mtxt
    set tkBind(${w},mesg) "Eval Result: ${mtxt}"

    set tkText(${w},markActive) 0
    set tkBind(${w},arg) {}
    set tkText(${w},prevCmd) EvalSel
    return ${txt}
}

######################################################################
# EMACS MARK manipulation routines
######################################################################

# tkTextCheckMark --
# Returns 1 if no emacs mark is present in text widget

proc tkTextCheckMark {w {chksel 0}} {
    global tkText tkBind
    if {[catch "${w} index emacs"]} {
        if ${chksel} {
            if {![catch "${w} index sel.first"]} return
        }
        eval $tkBind(bell)
        #		error "No emacs mark present!"
    }
}

# tkTextSetMark --
# Set the emacs mark to the given text index on 0 argument,
# else pop off the given mark in the mark ring
#
# Arguments:
# w -		Text window in which to set mark.
# ndx -		Text index to place mark
# n - 		Index of mark to pop off, if non-zero

proc tkTextSetMark {w {ndx insert} {n 0}} {
    global tkText tkBind

    ${w} mark set emacs ${ndx}
    ${w} mark set anchor ${ndx}
}

# tkTextExchangeMark --
# Exchange index positon of insert cursor and emacs mark
#
# Arguments:
# w -		Text window in which to exchange mark.

proc tkTextExchangeMark {w} {
    global tkText
    tkTextCheckMark ${w}
    set t_m_p [${w} index insert]
    ${w} mark set insert emacs
    tkTextSetMark ${w} ${t_m_p}

    ${w} see insert
    update idletasks
}

proc tkTextGetParaBounds {w {ndx insert}} {
    global tkText tkBind

    set ndx [tkTextNextPara ${w} ${ndx}]
    if {[${w} compare "${ndx} lineend" < end]} {
        set ndx [${w} index "${ndx} -1 line"]
    }
    while {[string trim [${w} get ${ndx} "${ndx} lineend"]] == "" &&\
      [${w} compare ${ndx} > 1.0]} {
        set ndx [${w} index "${ndx} -1 line"]
    }
    set last [${w} index "${ndx} lineend +1c"]
    set first [${w} index "[tkTextPrevPara ${w} ${ndx}] linestart"]
    return [list ${first} ${last}]
}

proc tkTextMarkPara {w {dosel 1}} {
    global tkText tkBind

    #lassign [tkTextGetParaBounds $w] first last
    set bb [tkTextGetParaBounds ${w}]
    set first [lindex ${bb} 0]
    set last [lindex ${bb} 1]

    ${w} mark set insert ${first}
    ${w} mark set emacs ${last}
    ${w} mark set anchor emacs
    if ${dosel} {
        ${w} tag remove sel 1.0 end
        ${w} tag add sel insert emacs
        ${w} tag raise sel
        set tkText(${w},markActive) 1
    }

    ${w} see insert
    set tkBind(${w},arg) {}
    set tkText(${w},prevCmd) MarkPara
    set tkBind(${w},mesg) {}
}

######################################################################
# KILL BUFFER manipulation routines
######################################################################
proc tkTextCopyTagBuffer {w start stop} {
    set tagspecs {}
    set start [${w} index ${start}]
    set stop [${w} index ${stop}]

    return [list [${w} get ${start} ${stop}] ${tagspecs}]
    # Zsolt Koppany

    set ctag [${w} tag names ${start}]
    if {${ctag} == ""} {
        return [list [${w} get ${start} ${stop}] ${tagspecs}]
    }

    if {[lsearch -exact ${ctag} "rem"] != -1} {
        set beg [expr ${start} - 30]
    } else {
        set beg [expr int(${start})].0
    }

    foreach tagname [${w} tag names] {
        if {[lsearch -exact [${w} tag names ${start}] ${tagname}] > -1} {
            set rng [${w} tag nextrange ${tagname} ${beg} ${stop}]
        } else {
            set rng [${w} tag nextrange ${tagname} ${start} ${stop}]
        }
        while {[llength ${rng}]} {
            if {[${w} compare [lindex ${rng} 1] >= ${start}]} {
                lappend tagspecs [tkTextGetTagOffsets ${w} ${tagname} ${start}\
                  ${stop} ${rng}]
            }
            set rng [${w} tag nextrange ${tagname} [lindex ${rng} 1] ${stop}]
        }
    }

    return [list [${w} get ${start} ${stop}] ${tagspecs}]
}

proc tkTextGetTagOffsets {w tagname anchor stop rng} {
    set rngbeg [lindex ${rng} 0]
    set rngend [lindex ${rng} 1]
    if [${w} compare ${rngend} < ${anchor}] return
    if [${w} compare ${rngend} > ${stop}] {
        set rngend ${stop}
    }
    if [${w} compare ${rngbeg} < ${anchor}] {
        set rngbeg ${anchor}
    }

    scan ${anchor} "%d.%d" aline achar
    scan ${rngbeg} "%d.%d" bline bchar
    scan ${rngend} "%d.%d" eline echar

    if {${aline} == ${bline}} {
        set boffset "+ [expr ${bchar}-${achar}] c"
    } else {
        set boffset "+ [expr ${bline}-${aline}] l linestart + ${bchar} c"
    }

    if {${aline} == ${eline}} {
        set eoffset "+ [expr ${echar}-${achar}] c"
    } else {
        set eoffset "+ [expr ${eline}-${aline}] l linestart + ${echar} c"
    }
    return [list ${tagname} ${boffset} ${eoffset}]
}

proc tkTextInsertTagBuffer {w ndx cutbuf {tlist {}}} {

    if {[${w} cget -state] == "disabled"} return
    set ndx [${w} index ${ndx}]
    ${w} mark set insert ${ndx}
    ${w} tag remove sel 1.0 end

    while {[llength ${cutbuf}]} {

        set start [${w} index insert]

        #$w insert insert [lvarpop cutbuf]
        SyncEditors $w insert insert [lindex ${cutbuf} 0]
        ${w} insert insert [lindex ${cutbuf} 0]
        set cutbuf [lrange ${cutbuf} 1 end]

        foreach tagname [${w} tag names] {
            ${w} tag remove ${tagname} ${start} insert
        }

        #set tagspecs [lvarpop cutbuf]
        set tagspecs [lindex ${cutbuf} 0]
        set cutbuf [lrange ${cutbuf} 1 end]

        foreach tagspec ${tagspecs} {
            set tagname [lindex ${tagspec} 0]
            if {[llength ${tlist}] && [lsearch -exact ${tlist} ${tagname}] <\
              0} continue
            set boffset [lindex ${tagspec} 1]
            set eoffset [lindex ${tagspec} 2]
            ${w} tag add ${tagname} "${start} ${boffset}" "${start} ${eoffset}"
        }
    }

    return [list ${ndx} [${w} index insert]]
}

proc tkTextPushTagBuffer {thiscut {where 0}} {
    global tkText tkBind

    if ${where} {
        #set lastcut [lvarpop tkBind(killRing) end]
        set len [llength $tkBind(killRing)]
        set lastcut [lindex $tkBind(killRing) end]
        set tkBind(killRing) [lrange $tkBind(killRing) 0 [expr ${len} - 2]]

        if {${where} < 0} {
            set thiscut [concat ${thiscut} ${lastcut}]
        } else {
            set thiscut [concat ${lastcut} ${thiscut}]
        }
        lappend tkBind(killRing) ${thiscut}
    } else {
        set tkBind(killLen) [llength [lappend tkBind(killRing) ${thiscut}]]
        if {$tkBind(killLen) > $tkBind(killMax)} {
            incr tkBind(killLen) -1

            #lvarpop tkBind(killRing)
            set tkBind(killRing) [lrange $tkBind(killRing) 1 end]
        }
    }
    set tkBind(killPtr) 0
}

proc tkTextPopTagBuffer {} {
    global tkText tkBind

    if {$tkBind(killLen) == 0} {
        return ""
    }

    #set lastcut [lvarpop tkBind(killRing) end]
    set len [llength $tkBind(killRing)]
    set lastcut [lindex $tkBind(killRing) end]
    set tkBind(killRing) [lrange $tkBind(killRing) 0 [expr ${len} - 2]]

    incr tkBind(killLen) -1
    set tkBind(killPtr) 0
    return ${lastcut}
}

proc tkTextGetTagBuffer {{ndx 1}} {
    global tkText tkBind

    if {$tkBind(killLen) == 0} {
        return ""
    }
    set ndx [expr ${ndx}+$tkBind(killPtr)]
    set tkBind(killPtr) [expr ${ndx}-1]
    set ndx [expr ${ndx}%$tkBind(killLen)]
    if {${ndx} == 0} {
        set ndx $tkBind(killLen)
    }
    return [lindex $tkBind(killRing) [expr $tkBind(killLen)-${ndx}]]
}

proc tkTextGetBufferText {{ndx 1}} {
    set cutbuf [tkTextGetTagBuffer ${ndx}]
    set txt {}
    #while {[llength $cutbuf]} {
    #	append txt [lvarpop cutbuf]
    #	lvarpop cutbuf
    #}
    set i 0
    foreach item ${cutbuf} {
        if {${i} > 0} {
            set i 0
            continue
        }
        append txt ${item}
        incr i
    }
    return ${txt}
}

######################################################################
# UNDO ROUTINES
######################################################################

# tkTextUndoSetup --
# Initialize globals for handling undo ring in given widget
#
# Arguments:
# w -		The text window 

proc tkTextUndoSetup {w} {
    global tkText

    set tkText(${w},undoCnt) 0
    set tkText(${w},undoPtr) 0
    set tkText(${w},undoCut) {}
    set tkText(${w},undoFirst) -1.0
    set tkText(${w},undoLast) -1.0
    set tkText(${w},undoList) {}
    set tkText(${w},undoSafe) 1
}

# tkTextUndoFree --
# Free globals for handling undo ring in given widget
#
# Arguments:
# w -		The text window 

proc tkTextUndoFree {w} {
    global tkText

    if {![info exists tkText(${w},undoCnt)]} return
    unset tkText(${w},undoCnt)
    unset tkText(${w},undoPtr)
    unset tkText(${w},undoCut)
    unset tkText(${w},undoFirst)
    unset tkText(${w},undoLast)
    unset tkText(${w},undoList)
    unset tkText(${w},undoSafe)
}

# tkTextUndo --
# Undo the last modification to the given text widget and
# reset text state globals
#
# Arguments:
# w -		The text window 

proc tkTextUndo {w} {
    global tkText tkBind

    set high_idx [${w} index insert]
    if {![info exists tkText(${w},undoCnt)] || ![info exists\
      tkText(${w},prevCmd)]} {
        return "no"
    }

    ${w} tag remove sel 1.0 end
    set tkBind(${w},mesg) {Undo.}
    if {$tkText(${w},prevCmd) != "Undo"} {
        set tkText(${w},undoPtr) $tkText(${w},undoCnt)
    }
    set tkText(${w},undoSafe) 0
    tkTextUndoPop ${w}
    set tkText(${w},undoSafe) 1
    set tkText(${w},markActive) 0
    set tkBind(${w},arg) {}
    set tkText(${w},prevCmd) Undo

    synch_highlight ${w} ${high_idx}
    return ""
}

# tkTextUndoPush --
# Push information on undo ring on how to undo a modification
#
# Arguments:
# w -		The text window 
# cutbuf -	Cut buffer of text that used to be between the
#			text indices first and last
# first, last - The start and ending text indices of affected
#			region of text widget

proc tkTextUndoPush {w cutbuf first last} {
    global tkText tkBind

    if {![info exists tkText(${w},undoCnt)]} {
        return "no"
    }

    if [${w} compare ${first} == ${last}] {
        if {![string length ${cutbuf}]} {
            return "no"
        }
    }

    set first [${w} index ${first}]
    set last [${w} index ${last}]

    lappend tkText(${w},undoList) [list $tkText(${w},undoCut)\
      $tkText(${w},undoFirst) $tkText(${w},undoLast)]
    set tkText(${w},undoCut) ${cutbuf}
    set tkText(${w},undoFirst) ${first}
    set tkText(${w},undoLast) ${last}

    incr tkText(${w},undoCnt)
    if {$tkText(${w},undoCnt) > $tkBind(undoMax) && $tkText(${w},undoSafe)} {
        set num [expr $tkText(${w},undoCnt)-$tkBind(undoMax)]
        set tkText(${w},undoList) [lreplace $tkText(${w},undoList) 1 ${num}]
        set tkText(${w},undoCnt) $tkBind(undoMax)
    }

    return ""
}

# tkTextUndoPop --
# Undo the last modification to the given text widget
#
# Arguments:
# w -		The text window 

proc tkTextUndoPop {w} {
    global tkText tkBind

    if {![info exists tkText(${w},undoCnt)]} {
        eval $tkBind(bell)
        error "Undo tracking is not turned on for this text widget!"
    }

    if {$tkText(${w},undoPtr) < 1} {
        eval $tkBind(bell)
        set tkBind(${w},mesg) {No more to undo.}
        tkTextUndoSetup ${w}
        return
    }

    if {$tkText(${w},undoPtr) == $tkText(${w},undoCnt)} {
        set undoCut $tkText(${w},undoCut)
        set undoFirst $tkText(${w},undoFirst)
        set undoLast $tkText(${w},undoLast)
    } else {
        #lassign [lindex $tkText($w,undoList) $tkText($w,undoPtr)] undoCut\
          undoFirst undoLast
        set bb [lindex $tkText(${w},undoList) $tkText(${w},undoPtr)]
        set undoCut [lindex ${bb} 0]
        set undoFirst [lindex ${bb} 1]
        set undoLast [lindex ${bb} 2]
    }

    set loop 0
    set retval {}
    if {${undoFirst} == "grp"} {
        set grp [lindex ${undoCut} 0]
        if {${undoLast} == -2.0} {
            set loop 1
        } else {
            ${w} mark set insert [lindex ${undoCut} 1]
            set retval ${grp}
        }
    } else {
        if {[${w} compare ${undoFirst} < ${undoLast}]} {
            set cutbuf [tkTextCopyTagBuffer ${w} ${undoFirst} ${undoLast}]
        } else {
            set cutbuf {}
        }

        ${w} mark set insert ${undoFirst}
        SyncEditors $w delete insert $undoLast
        ${w} delete insert ${undoLast}
        tkTextInsertTagBuffer ${w} insert ${undoCut}

        lappend tkText(${w},undoList) [list $tkText(${w},undoCut)\
          $tkText(${w},undoFirst) $tkText(${w},undoLast)]
        set tkText(${w},undoCut) ${cutbuf}
        set tkText(${w},undoFirst) ${undoFirst}
        set tkText(${w},undoLast) [${w} index insert]
        incr tkText(${w},undoCnt)
    }

    incr tkText(${w},undoPtr) -1
    if ${loop} {
        tkTextUndoBeginGroup ${w} ${grp} -1.0
        while {$tkText(${w},undoPtr) > 0 && [tkTextUndoPop ${w}] != ${grp}} { }
        tkTextUndoBeginGroup ${w} ${grp} -2.0
    }
    ${w} see insert

    # If this is the last undo we can tell the editor that
    # the file is no longer modified.
    if {$tkText($w,undoPtr) == 0} {
        # Find editor
        set undone_editor ""
        foreach editor_object [itcl::find objects -class Editor&] {
            if {[$editor_object editor]==$w} {
                $editor_object setmodified 0
                $editor_object SetTitle
                break
            }
        }
    }

    return ${retval}
}

# tkTextUndoBeginGroup --
# Signals to the undo ring that each subsequent undo push until an
# end group marker is found should be considered part of "one"
# operation when popped later
#
# Arguments:
# w -		The text window 
# grp -		A arbritrary but unique identifier for the group
#		which must match the one eventually given to 
#		tkTextUndoEndGroup
# signal -	-1.0 means begin group, -2.0 means end group

proc tkTextUndoBeginGroup {w grp {signal -1.0}} {
    global tkText tkBind

    if {![info exists tkText(${w},undoCnt)]} return
    lappend tkText(${w},undoList) [list $tkText(${w},undoCut)\
      $tkText(${w},undoFirst) $tkText(${w},undoLast)]
    set tkText(${w},undoCut) [list ${grp} [${w} index insert]]
    set tkText(${w},undoFirst) grp
    set tkText(${w},undoLast) ${signal}

    incr tkText(${w},undoCnt)
}

# tkTextUndoEndGroup --
# Signals to the undo ring that a grouping is to end
#
# Arguments:
# w -		The text window 
# grp -		A arbritrary but unique identifier for the group
#		which must match the one that was given to 
#		tkTextUndoBeginGroup to start the group

proc tkTextUndoEndGroup {w grp} {
    tkTextUndoBeginGroup ${w} ${grp} -2.0
}

######################################################################
# EXTRAS
######################################################################

proc tkTextGetFillWidth {w c} {

    #KHAMIS, very important, disable synch
    global SyncEditors_Disabled

    catch {incr SyncEditors_Disabled}

    set ndx [${w} index "@0,0 linestart"]
    ${w} see ${ndx}
    ${w} insert ${ndx} a
    set fw [expr ([lindex [${w} bbox ${ndx}] 2]*${c})+[lindex [${w} bbox\
      ${ndx}] 0]]
    ${w} delete ${ndx}

    #KHAMIS, reenable synch
    catch {incr SyncEditors_Disabled -1}

    return ${fw}
}

proc tkTextSetFillCol {w {n +}} {
    global tkText tkBind
    set n [tkBindDefArg ${w} ${n} -1]

    if {${n} < 0} {
        ${w} see insert
        set tkText(${w},fillWidth) [lindex [${w} bbox insert] 0]
        set margin [lindex [${w} bbox "insert linestart"] 0]
        set cw [expr [tkTextGetFillWidth ${w} 1]-${margin}]
        set tkText(${w},fillCol)\
          [expr round(($tkText(${w},fillWidth)-${margin})*1.0/${cw})]
    } else {
        set tkText(${w},fillCol) ${n}
        set tkText(${w},fillWidth) [tkTextGetFillWidth ${w} ${n}]
    }

    set tkBind(${w},arg) {}
    set tkText(${w},prevCmd) SetFillCol
    set tkBind(${w},mesg) "Fill column set to $tkText(${w},fillCol).\
      {$tkText(${w},fillWidth)}"
}

proc tkTextWrapWord {w {push 1}} {
    global tkText tkBind

    if {[${w} cget -state] == "disabled"} return
    if {[string first [${w} get "insert -1 c"] " \t"] < 0} {
        ${w} mark set insert insert-1c
        while {![regexp $tkBind(notWord) [${w} get insert]] && [${w} compare\
          insert > 1.0]} {
            ${w} mark set insert insert-1c
        }
        ${w} mark set insert insert+1c
    }
    if {[${w} compare insert > "insert linestart"]} {
        set ndx [${w} index insert]
        ${w} insert insert \n
        if ${push} {
            tkTextUndoPush ${w} {} ${ndx} [${w} index insert]
        }
    }
}

proc tkTextFormatPara {w} {
    global tkText tkBind

    set tkBind(${w},arg) {}

    #lassign [tkTextGetParaBounds $w] first last
    set bb [tkTextGetParaBounds ${w}]
    set first [lindex ${bb} 0]
    set last [lindex ${bb} 1]

    tkTextFormatRegion ${w} ${first} ${last}
    set tkText(${w},prevCmd) FormatPara
    set tkBind(${w},mesg) {}
}

proc tkTextFormatRegion {w first last} {
    global tkText tkBind

    if {[${w} cget -state] == "disabled"} return
    if {$tkText(${w},fillCol) > 2} {
        set fwidth $tkText(${w},fillWidth)
    } else {
        set fwidth [tkTextGetFillWidth ${w} 70]
    }

    set first [${w} index "${first} linestart"]
    set cutbuf [tkTextCopyTagBuffer ${w} ${first} ${last}]

    ${w} mark set insert ${first}
    ${w} mark set formatend ${last}

    # determine prefix
    while {[string match "\[ \t\]" [${w} get insert]] && [${w} compare insert\
      < end]} {
        ${w} mark set insert "insert +1 char"
    }
    if {[${w} compare insert != ${first}]} {
        set prefix [${w} get ${first} insert]
    } else {
        set prefix ""
    }

    #### go through stripping extra space and newlines
    set lastspace 0
    while {[${w} compare insert < formatend]} {
        if {[${w} get insert] == " "} {
            if ${lastspace} {
                ${w} delete insert
            } else {
                ${w} mark set insert "insert +1c"
            }
            set lastspace 1
        }\
        elseif {[${w} get insert] == "\n" || [${w} get insert] == "\t"} {
            ${w} delete insert
            if {!${lastspace}} {
                ${w} insert insert { }
            }
            set lastspace 1
        } else {
            set lastspace 0
            ${w} mark set insert "insert +1c"
        }
    }
    ${w} mark set insert ${first}

    ### go through and put in line breaks
    while {[${w} compare insert < end]} {
        ${w} see insert

        #lassign [$w bbox insert] x y
        set bb [${w} bbox insert]
        set x [lindex ${bb} 0]
        set y [lindex ${bb} 1]

        ${w} mark set insert "@${fwidth},${y}"
        if [${w} compare insert >= formatend] break

        if {[${w} get insert] == " "} {
            ${w} mark set insert "insert +1c"
        } else {
            while {[string first [${w} get "insert -1c"] $tkBind(fillBreak)] <\
              0 && [${w} compare insert > 1.0]} {
                ${w} mark set insert "insert -1c"
            }
        }
        ${w} insert insert "\n${prefix}"
    }
    ${w} mark set insert formatend
    ${w} insert insert \n

    tkTextUndoPush ${w} ${cutbuf} ${first} [${w} index insert]
    ${w} see insert
}

if {[info proc tkTextInitHook]!=""} {
    tkTextInitHook
}


