# Copyright (c) 2000, 2001, Red Hat, Inc.
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
# tk_dialog:
#
# This procedure displays a dialog box, waits for a button in the dialog
# to be invoked, then returns the index of the selected button.
#
# Arguments:
# w -		Window to use for dialog top-level.
# title -	Title to display in dialog's decorative frame.
# text -	Message to display in dialog.
# bitmap -	Bitmap to display in dialog (empty string means none).
# default -	Index of button that is to display the default ring
#		(-1 means none).
# args -	One or more strings to display in buttons across the
#		bottom of the dialog box.

proc tk_dialog {w title text bitmap default args} {
    global tkPriv tkeWinNumber

    if {[sn_batch_mode]} {
        if {${title} != ""} {
            set msg "${title}: ${text}"
        } else {
            set msg ${text}
        }
        puts stderr "WARNING: ${msg} <[lindex ${args} 0]>"
        return 0
    }

    # 1. Create the top-level window and divide it into top
    # and bottom parts.
    if {${w} == "auto"} {
        incr tkeWinNumber
        set w ".info-${tkeWinNumber}"
    }

    if {[catch {sourcenav::Window ${w}}]} {
        return -1
    }
    ${w} withdraw
    ${w} on_close "set tkPriv(${w},button) -1"
    ${w} configure -title ${title}

    set focus [focus]
    if {${focus} == ""} {
        set focus "."
    } else {
        set focus [winfo toplevel ${focus}]
    }
    if {[wm state ${focus}] == "normal"} {
        ${w} transient ${focus}
    }

    frame ${w}.top -relief groove -bd 2
    pack ${w}.top -side top -fill both

    # 2. Fill the top part with bitmap and message.

    label ${w}.msg -text ${text}

    pack ${w}.msg -in ${w}.top -side right -expand 1 -fill both -padx 5m\
      -pady 5m
    if {${bitmap} != ""} {
        label ${w}.bitmap -image ${bitmap}
        pack ${w}.bitmap -in ${w}.top -side left -padx 5m -pady 5m
    }

    # 3. Create a row of buttons at the bottom of the dialog.

    eval sn_motif_buttons ${w} bottom ${default} ${args}

    set i 0
    foreach but ${args} {
        ${w}.button_${i} configure -command " set tkPriv(${w},button) ${i} "
        if {${i} == ${default}} {
            bind ${w} <Return> "
				catch {${w}.button_${i} flash}
				${w}.button_${i} invoke
			"
        }
        incr i
    }
    set lastnum [expr ${i} - 1]

    catch {${w} grab set}

    catch {${w} resizable yes no}
    catch {${w} grab set}

    #update idletasks
    set geom [split [lindex [split [${w} geometry] "+"] 0] "x"]
    ${w} minsize [lindex ${geom} 0] [lindex ${geom} 1]

    ${w} move_to_mouse
    ${w} take_focus ${w}.button_0

    vwait tkPriv(${w},button)

    itcl::delete object ${w}

    catch {focus ${focus}}

    #don't return -1
    if {$tkPriv(${w},button) == -1} {
        return ${lastnum}
    } else {
        return $tkPriv(${w},button)
    }
}

#
# This procedure creates buttons in a frame.
#
# Arguments:
# w -		Frame for buttons.
# default -	Index of button that is to display the default ring
#		(-1 means none).
# pos -		top, bottom ...
# args -	One or more strings to display in buttons across the
#		bottom of the dialog box.
proc sn_motif_buttons {frm pos default args} {
    global tcl_platform
    if {${frm} == "."} {
        set w ""
    } else {
        set w ${frm}
    }

    if {$tcl_platform(platform) == "windows"} {
        frame ${w}.button -relief raised -bd 0
        pack ${w}.button -side ${pos} -fill both -padx 5 -pady 5
    } else {
        frame ${w}.button -relief raised -bd 1
        pack ${w}.button -side ${pos} -fill both
    }

    #expand the width of the buttons
    set len 10
    foreach but ${args} {
        set ll [string length ${but}]
        if {${ll} < 14 && ${len} < ${ll}} {
            set len ${ll}
        }
    }

    set i 0
    foreach but ${args} {
        set cmd [list button ${w}.button_${i}]
        if {[string index ${but} 0] == "@"} {
            lappend cmd -image [string range ${but} 1 end]
        } else {
            lappend cmd -text ${but}
            set len [string length ${but}]
            if {${len} <= 6 && ${len} != 0} {
                lappend cmd -width 7
            }
        }
        if {${i} == ${default}} {
            lappend cmd -default active
        }
        if {[string length ${but}] < ${len}} {
            lappend cmd -width ${len}
        }

        eval ${cmd}
        pack ${w}.button_${i} -in ${w}.button -side left -expand 1 -padx 3\
          -pady 2 -ipadx 1m

        incr i
    }
}

#
# tk_dialog:
#
# This procedure displays a dialog box, waits for a button in the dialog
# to be invoked, then returns the index of the selected button.  If the
# dialog somehow gets destroyed, -1 is returned.
#
# Arguments:
# w             -	Window to use for dialog top-level.
# title         -	Title to display in dialog's decorative frame.
# text          -	Message to display in dialog.
# bitmap        -	Bitmap to display in dialog (empty string means none).
# default       -	Index of button that is to display the default ring
#		          (-1 means none).
# create_widgets- command to be called, to create new addional widgets or
#                 to manipulate existing behavior
# args          -	One or more strings to display in buttons across the
#		          bottom of the dialog box.
proc tk_dialog_with_widgets {w title text bitmap default create_widgets args} {
    global tkPriv tcl_platform

    set oldFocus [focus]
    # FIXME: this is a hack to work around an error. We used to use our own
    # custom "grab" command. The Tk grab seems to have changed WRT the results
    # you get when the passed in window does not exist. The "real" fix will be
    # to totally revamp our use of grab.
    if {[winfo exists ${w}]} {
        set oldGrab [grab current ${w}]
    } else {
        set oldGrab [grab current]
    }
    if {${oldGrab} != ""} {
        set grabStatus [grab status ${oldGrab}]
    }

    # 1. Create the top-level window and divide it into top
    # and bottom parts.
# FIXME: This needs to use sourcenav::Window
    #catch {$w delete}
    catch {destroy ${w}}
    toplevel ${w} -class Dialog
    wm title ${w} ${title}
    wm iconname ${w} Dialog

    #bind escape/close event
    on_close ${w} "set tkPriv(${w},button) -1"

    frame ${w}.bot
    frame ${w}.top
    if {$tcl_platform(platform) == "unix"} {
        ${w}.bot configure -relief raised -bd 1
        ${w}.top configure -relief raised -bd 1
    }
    pack ${w}.bot -side bottom -fill both
    pack ${w}.top -side top -fill both -expand 1

    # 2.1 Add client procedure to create widgets
    eval ${create_widgets} ${w}

    # 2. Fill the top part with bitmap and message (use the option
    # database for -wraplength so that it can be overridden by
    # the caller).

    #option add *Dialog.msg.wrapLength 3i widgetDefault
    label ${w}.msg -justify left -text ${text}
    pack ${w}.msg -in ${w}.top -side right -expand 1 -fill both -padx 3m\
      -pady 3m
    if {${bitmap} != ""} {
        if {($tcl_platform(platform) == "macintosh") &&(${bitmap} == "error")} {
            set bitmap "stop"
        }
        label ${w}.bitmap -image ${bitmap}
        pack ${w}.bitmap -in ${w}.top -side left -padx 3m -pady 3m
    }

    # 3. Create a row of buttons at the bottom of the dialog.

    set len 10
    foreach but ${args} {
        set ll [string length ${but}]
        if {${ll} < 14 && ${len} < ${ll}} {
            set len ${ll}
        }
    }
    set i 0
    foreach but ${args} {
        set cmd [list button ${w}.button${i} -text ${but} -command " set\
          tkPriv(${w},button) ${i} "]
        if {[string length ${but}] < ${len}} {
            lappend cmd -width ${len}
        }

        if {${i} == ${default}} {
            lappend cmd -default active
        } else {
            lappend cmd -default normal
        }

        #create button
        eval ${cmd}

        grid ${w}.button${i} -in ${w}.bot -column ${i} -row 0 -sticky ew\
          -padx 10 -pady 5
        grid columnconfigure ${w}.bot ${i}
        # We boost the size of some Mac buttons for l&f
        if {$tcl_platform(platform) == "macintosh"} {
            set t_m_p [string tolower ${but}]
            if {(${t_m_p} == "ok") ||(${t_m_p} == "cancel")} {
                grid columnconfigure ${w}.bot ${i} -minsize [expr 59 + 20]
            }
        }
        incr i
    }
    set lastnum [expr ${i} - 1]

    # 4. Create a binding for <Return> on the dialog if there is a
    # default button.

    if {${default} >= 0} {
        bind ${w} <Return> "
	    ${w}.button${default} configure -state active -relief sunken
	    update idletasks
	    after 100
	    set tkPriv(${w},button) ${default}
	"
    }

    # 5. Create a <Destroy> binding for the window that sets the
    # button variable to -1;  this is needed in case something happens
    # that destroys the window, such as its parent window being destroyed.
    bind ${w} <Destroy> "set tkPriv(${w},button) -1"

    # 6. Withdraw the window, then update all the geometry information
    # so we know how big it wants to be, then center the window in the
    # display and de-iconify it.

    wm withdraw ${w}

    update idletasks

    set x [expr [winfo screenwidth ${w}]/2 - [winfo reqwidth ${w}]/2 -\
      [winfo vrootx [winfo parent ${w}]]]
    set y [expr [winfo screenheight ${w}]/2 - [winfo reqheight ${w}]/2\
      - [winfo vrooty [winfo parent ${w}]]]
    wm geom ${w} +${x}+${y}

    wm deiconify ${w}

    # 7. Set a grab and claim the focus too.

    grab ${w}

    if {$default >= 0} {
	window_configure ${w} deiconify ${w}.button${default}
	focus ${w}.button${default}
    }

    update idle

    # 8. Wait for the user to respond, then restore the focus and
    # return the index of the selected button.  Restore the focus
    # before deleting the window, since otherwise the window manager
    # may take the focus away so we can't redirect it.  Finally,
    # restore any grab that was in effect.

    vwait tkPriv(${w},button)
    catch {focus ${oldFocus}}
    if {[winfo exists ${w}]} {
        # It's possible that the window has already been destroyed,
        # Delete the Destroy handler so that
        # tkPriv($w,button) doesn't get reset by it.

        bind ${w} <Destroy> {}
        destroy ${w}
    }
    if {${oldGrab} != ""} {
        if {${grabStatus} == "global"} {
            grab -global ${oldGrab}
        } else {
            grab ${oldGrab}
        }
    }

    #don't return -1
    if {$tkPriv(${w},button) == -1} {
        return ${lastnum}
    } else {
        return $tkPriv(${w},button)
    }
}


itcl::class sourcenav::Dialog {
    inherit sourcenav::Window

    constructor { args } {}
    destructor {}

    public method activate {}
    public method deactivate { args }

    # Hide the on_close method so that it can't be invoked!
    private method on_close { {cmd ""} }

    itk_option define -modality modality Modality application {
        if {$_active} {
            error "Cannot change -modality while Dialog is active."
        }

	switch $itk_option(-modality) {
	    none -
	    application -
	    global {
	    }
	    
	    default {
		error "bad modality option \"$itk_option(-modality)\":\
			should be none, application, or global"
	    }
	}
    }

    private common grabstack {}

    private variable _result ""
    private variable _active 0
}

itcl::body sourcenav::Dialog::constructor { args } {
    # Maintain a withdrawn state until activated.  
    $this withdraw

    on_close [itcl::code $this deactivate]

    eval itk_initialize $args
}

itcl::body sourcenav::Dialog::destructor { } {
    # If the dialog is currently being displayed,
    # we need to clean it up before we die.
    if {$_active} {
        sn_log "Dialog was active in dtor, calling deactivate"
        $this deactivate
    }
}

# This method is overloaded here just to make sure that
# nobody calls on_close for a Dialog class.

itcl::body sourcenav::Dialog::on_close { {cmd ""} } {
    sourcenav::Window::on_close $cmd
}

itcl::body sourcenav::Dialog::activate { } {
    if {$_active} {
        error "Called activate method when Dialog was already active."
    }
    if {[winfo ismapped $itk_component(hull)]} {
        error "Called activate method when Dialog window was already mapped."
    }
    set _active 1

    $this PushModalStack $itk_component(hull)

    ${this} centerOnScreen
    ${this} deiconify
    ${this} raise
    tkwait visibility $itk_component(hull)

    ${this} focusmodel active

    if {$grabstack != {}} {
        ::grab release [lindex $grabstack end]
    }

    set err 1

    if {$itk_option(-modality) == "application"} {
        while {$err == 1} {
            set err [catch [list ::grab $itk_component(hull)]]
            if {$err == 1} {
                after 1000 "set pause 1"
                vwait pause
            }
        }

        lappend grabstack [list ::grab $itk_component(hull)]
    } elseif {$itk_option(-modality) == "global" }  {
        while {$err == 1} {
            set err [catch [list ::grab -global $itk_component(hull)]]
            if {$err == 1} {
                after 1000 "set pause 1"
                vwait pause
            }
        }
	    
        lappend grabstack [list ::grab -global $itk_component(hull)]
    }

    sn_log "Dialog::activate waiting for _result"
    vwait [itcl::scope _result]
    sn_log "Dialog::activate returning _result = \"$_result\""
    return $_result
}

itcl::body sourcenav::Dialog::deactivate { args } {
    if {!$_active} {
        error "Called deactivate method when Dialog was not active."
    }
    set _active 0

    if {$itk_option(-modality) == "none"} {
        wm withdraw $itk_component(hull)
    } elseif {$itk_option(-modality) == "application"} {
        ::grab release $itk_component(hull)
        if {$grabstack != {}} {
            if {[set grabstack [lreplace $grabstack end end]] != {}} {
                eval [lindex $grabstack end]
            }
	}

        $this PopModalStack
        wm focusmodel $itk_component(hull) passive
        wm withdraw $itk_component(hull)
        
    } elseif {$itk_option(-modality) == "global"} {
        ::grab release $itk_component(hull)
        if {$grabstack != {}} {
            if {[set grabstack [lreplace $grabstack end end]] != {}} {
                eval [lindex $grabstack end]
            }
        }

        wm withdraw $itk_component(hull)
    }

    # Set the result to what the user passed in.
    # This will release the vwait inside activate.
    set _result $args
    return
}
