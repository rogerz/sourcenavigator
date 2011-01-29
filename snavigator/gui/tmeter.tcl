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
##
## tmeter.tcl
##
## Implements termometers used in SN to show progress.


## by batch mode, display the files to the standard
## output
proc display_scale_window {sc line} {
    global tcl_platform
    if {[winfo exists ${sc}]} {
        global sn_progress_value
        ${sc}.i.filename config -text ${line}
        incr sn_progress_value

        if {$tcl_platform(platform) == "windows"} {
            update idletasks
            update idletasks
        }
        update idletasks
    }\
    elseif {[sn_batch_mode]} {
        puts stdout [format [get_indep String ScanningFile] ${line}]
        flush stdout
    }
}

proc delete_scale_window {wdg} {
    global sn_scale_oldfocus
    if {[itcl::find object ${wdg}] == ${wdg}} {
        itcl::delete object ${wdg}
    }

    #restore old focus
    if {[info exists sn_scale_oldfocus] && [winfo exists\
      ${sn_scale_oldfocus}]} {
        take_focus [winfo toplevel ${sn_scale_oldfocus}] ${sn_scale_oldfocus}
    }
    
    # Update so that any windows the scale window was over
    # get to redraw before a possibly long wait.
    update
}

proc make_scale_window {filenum {enable_cancel 0}} {
    global sn_options
    global sn_progress_value
    global sn_scale_oldfocus

    if {[sn_batch_mode]} {
        return
    }

    set curs "watch"

    set sn_scale_oldfocus [focus]

    set t [sourcenav::Window [sourcenav::Window::next_window_name] -cursor ${curs}]
    set thull [$t component hull]
    ${t} withdraw
    ${t} on_close "${thull}.cancel invoke"
    ${t} configure -title [get_indep String Scanning]

    frame ${thull}.i
    label ${thull}.i.run -image company_image -bg black

    label ${thull}.i.filename -width 50 -anchor w

    #use a normal termometer (not bad unix scaler)
    set sn_progress_value 0
    ProgressBar ${thull}.scale -maxvalue ${filenum} -orientation horizontal\
      -showvalue 1 -variable sn_progress_value

    button ${thull}.cancel -text [get_indep String Cancel] -command "sn_processing_canceled 2"

    pack ${thull}.i.run -side left -padx 2 -pady 1 -anchor w
    pack ${thull}.i.filename -side left -fill x -anchor w -padx 10 -pady 20
    pack ${thull}.i -fill x -anchor w

    pack ${thull}.scale -fill x -padx 20 -pady 20
    if {${enable_cancel}} {
        pack ${thull}.cancel -side top -pady 2
    }

    catch {${thull} resizable yes no}

    ${thull} move_to_mouse

    update idletasks
    set geom [split [lindex [split [${t} geometry] "+"] 0] "x"]
    ${t} minsize [expr int([lindex ${geom} 0] / 3)] [lindex ${geom} 1]

    pack propagate ${thull}.i 0
    ${thull}.i.filename config -width 0

    catch {${thull} grab set}

    window_configure ${thull} deiconify ${thull}.scale

    # Ugh! Why does the window get mapped without
    # having the contents drawn under Windows?
    if {$::tcl_platform(platform) == "windows"} {
        sn_wait 300
    }

    return ${thull}
}

############################################################
# Own Implementation of the ProgressBar
############################################################
itcl::class ProgressBar {

    inherit itk::Widget

    constructor {args} {
        global sn_options

        itk_component add label {
            label $itk_component(hull).label
        } {
            keep -font
            rename -text -labeltext labeltext Labeltext
        }

        itk_component add canvas {
            canvas $itk_component(hull).canvas \
                -highlightthickness 0 \
                -bd 1 \
                -relief ridge \
                -height [winfo reqheight $itk_component(label)]
        } {
            keep -width
        }

        eval itk_initialize $args

        # FIXME: I don't understand how we could set
        # the -width -height properties int the canvas ctor
        # when we need to call itk_initialize after adding it?
        # This should work but the options are not getting
        # set for some reason.

        #$itk_component(canvas) configure \
        #    -width $itk_option(-width) \
        #    -height $itk_option(-height)

        if {$itk_option(-labeltext) != ""} {
            grid $itk_component(label) -row 0 -column 0
        }

        grid $itk_component(canvas) -row 0 -column 1 -sticky ew

        grid columnconfigure $itk_component(hull) 1 -weight 1

        $itk_component(canvas) create rect -1 0 0 25 \
            -fill ${indicatorcolor} \
            -tags bar \
            -outline {}

        $itk_component(canvas) create text 0 0 \
            -fill ${foreground} \
            -tags text \
            -anchor c \
            -font $sn_options(def,layout-font)

        $itk_component(canvas) xview moveto 0
        $itk_component(canvas) yview moveto 0

        bind $itk_component(canvas) <Configure> [itcl::code ${this} resize %w %h]
    }
    destructor {
        # delete the bound tracing script if one exists
        if {$itk_option(-variable) != ""} {
            # upvar can fail if the var scope no longer exits, just ignore
            if {![catch {upvar #0 $itk_option(-variable) local}]} {
                trace vdelete local w [itcl::code $this trace_callback]
            }
        }
    }

    # Should this be keyed to a configure of -width and -height ?

    method resize {W H} {

        sn_log "ProgressBar resize $W $H, current value is \"$itk_option(-value)\""

# FIXME : not sure if this logic is correct!
        # steped display
        if {${displayed_value} > 0 && (($itk_option(-value) - ${displayed_value}) <
            ${step}) && (${displayed_value} < $itk_option(-value))} {
            return
        }

        # Don't display values >100%
        if {$itk_option(-value) > $itk_option(-maxvalue)} {
            set itk_option(-value) $itk_option(-maxvalue)
        }

        set pcnt [expr {$itk_option(-value) / double($itk_option(-maxvalue))}]

        if {[string match "h*" ${orientation}]} {
            $itk_component(canvas) coords bar -1 0 [expr {int(${pcnt}*${W})}] ${H}
        } else {
            # Vertical orientation needs testing
            $itk_component(canvas) coords bar -1 ${H} ${W} [expr {int(${pcnt}*${H})}]
        }

        if {${showvalue}} {
            $itk_component(canvas) coords text [expr {${W}/2}] [expr {${H}/2-2}]
            $itk_component(canvas) itemconfigure text -text \
		"${beforetext}[expr {int(${pcnt}*100.0)}]${aftertext}"
        } else {
            $itk_component(canvas) coords text ${W} ${H}
        }

        set displayed_value $itk_option(-value)
    }

    method __bind args {
        eval bind $itk_component(canvas) ${args}
        eval bind $itk_component(label) ${args}
    }
    method balloon {txt} {
        balloon_bind_info $itk_component(canvas) ${txt}
        balloon_bind_info $itk_component(label) ${txt}
    }

    private method trace_callback {name name2 op} {
        upvar #0 $itk_option(-variable) local
        set itk_option(-value) $local
        sn_log "ProgressBar trace_callback \"$name\" \"$name2\" is $itk_option(-value)"
        $this resize \
            [winfo width  $itk_component(canvas)] \
            [winfo height $itk_component(canvas)]
    }

    itk_option define -value value Value 1
    itk_option define -fromvalue fromvalue Fromvalue 1

    itk_option define -variable variable Variable "" {
        trace vdelete $itk_option(-variable) w [itcl::code $this trace_callback]

        if {$itk_option(-variable) == ""} {
            return
        }

        sn_log "ProgressBar $this tracing on variable \"$itk_option(-variable)\""
        upvar #0 $itk_option(-variable) local
        trace variable local w [itcl::code $this trace_callback]
    }

    itk_option define -maxvalue maxvalue Maxvalue 100 {
        if {$itk_option(-maxvalue) <= 0} {
            set itk_option(-maxvalue) 1
        }
    }

    itk_option define -labeltext labeltext Labeltext "" {
        if {$itk_option(-labeltext) == ""} {
            grid forget $itk_component(label)
        } else {
            grid $itk_component(label) -row 0 -column 0
        }
    }

    protected variable displayed_value 0
    public variable beforetext ""
    public variable aftertext "%"
    public variable canvas ""
    public variable labelw ""
    public variable orientation "horizontal"
    public variable showvalue 1

    itk_option define -width width Width 100

    public variable foreground "black"
    public variable indicatorcolor $sn_options(def,progress-bg)
    public variable step 1
}



###########################################################
## Procedures for displaying the loading window on startup.
###########################################################
proc hide_loading_message {{wdg ".loading"}} {
    destroy ${wdg}
    update idletasks
}

proc sn_loading_message {{str ""} {title ""} {first_interp 1}\
  {hide_toplevel 0}} {
    global sn_options
    global sn_product_name
    global older_sn_loading_message_title
    global tcl_platform

    #interactive mode
    if {[sn_batch_mode]} {
        puts stdout ${str}
        return
    }

    if {${title} == ""} {
        set title ${sn_product_name}
    }
    if {${str} == ""} {
        set str [get_indep String Loading]
    }

    set w .loading

    if {![winfo exists ${w}]} {
        if {${hide_toplevel}} {
            wm withdraw .
        }
        if {![info exists sn_options(def,layout-font)]} {
            init_some_font_attributes
        }
        set bg #c0c0c0
        set cursor "watch"

        #delete loading message window
        hide_loading_message

        # changed by Freek 2007: dont show ridiculous copyright message
	# again and again when working with the app
	#
	# original format
        # ---------------------
        #|     COPY-RIGHT      |
        #| IMAGE TEXT          |
        #| IMAGE               |
        #|                     |
        #|        Stop         |
        # ---------------------
        toplevel ${w} -bg ${bg}
        wm transient ${w} .
        wm title ${w} ${title}

        frame ${w}.m -bg ${bg}
        pack ${w}.m -fill both -expand y

        label ${w}.m.img -bg black -cursor ${cursor} -image company_image \
            -anchor w
        pack ${w}.m.img -padx 5 -pady 5 -side left -anchor w -side left


        frame ${w}.m.cfr -bg ${bg}
        pack ${w}.m.cfr -side left -fill both -expand y -side left -pady 5

        set cmd [list label ${w}.m.cfr.lbl -bg ${bg} -cursor ${cursor}\
          -anchor w -width 40 -font [list $sn_options(def,layout-font)]\
          -text [get_indep String Loading]]
        if {$sn_options(iscolor)} {
            lappend cmd -fg blue
        }
        eval ${cmd}
        pack ${w}.m.cfr.lbl -side top -fill x -anchor w -expand y

        button ${w}.btn -bg ${bg} -font $sn_options(def,layout-font)\
          -text [get_indep String Cancel] -command " sn_loading_cancel "\
          -state disabled

        set x [expr [winfo screenwidth .] / 6 * 2]
        set y [expr [winfo screenheight .] / 6 * 2]
        wm geometry ${w} "+${x}+${y}"

        global sn_force_flag
        eval focus ${sn_force_flag} [focus -lastfor ${w}]
        #focus $img

        set older_sn_loading_message_title ${title}
    } else {
        # Don't call raise here since it can cause a two second
        # timeout with some broken window managers (kde, gnome).
        #raise ${w}
    }

    if {${older_sn_loading_message_title} != ${title}} {
        wm title ${w} ${title}
        set older_sn_loading_message_title ${title}
    }

    ${w}.m.cfr.lbl config -text ${str}

    #wait to let the window be drawn (needed for windows)
    if {$tcl_platform(platform) == "windows"} {
        sn_wait 150
    }
    update idletasks
}
proc sn_loading_cancel {} {
    global SN_Loading_Cancel_Var
    if {! [info exists SN_Loading_Cancel_Var]} {
        return
    }
    upvar #0 ${SN_Loading_Cancel_Var} var
    set var cancel
}
proc sn_loading_enable_cancel {var} {
    global SN_Loading_Cancel_Var
    set SN_Loading_Cancel_Var ${var}
    pack .loading.btn -pady 5 -side bottom -anchor c
    .loading.btn config -state normal
    grab set .loading
}

proc sn_loading_disable_cancel {} {
    global SN_Loading_Cancel_Var
    set SN_Loading_Cancel_Var ""
    .loading.btn config -state disabled
    pack forget .loading.btn
    grab release .loading
}

proc sn_wait {{msec 10}} {
    global sn_str_wait
    update idletasks
    set sn_str_wait nomore
    after ${msec} "set sn_str_wait nomore"
    #make sure that 'tkwait' terminates!
    after [expr {${msec} + 2000}] "set sn_str_wait nomore"
    tkwait variable sn_str_wait
    unset sn_str_wait
    update idletasks
}


