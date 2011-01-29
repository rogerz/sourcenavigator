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
# window.tcl - Toplevel replacement and routines for handling
#              and creating windows?
# Copyright (C) 1998 Cygnus Solutions.

#FIXME:
# This Window class is based on the old pre itcl1.5 code
# from Toplevel&.  Most of the methods need a big tidy up.


namespace eval sourcenav {}
 
itcl::class sourcenav::Window {

    inherit itk::Toplevel

    constructor {args} {
        global sn_options

        # We need extra options from itk::Toplevel.
        itk_option add hull.menu hull.relief hull.borderwidth

# FIXME : Why would itk::Toplevel not do this by default?
        #close
        on_close "itcl::delete object ${this}"

        # What is this leader stuff???
        # configure -leader

        # Make sure that the tag class "Toplevel" is included
        # in the event tag list, also "all", but Toplevel
        # must be placed before all
        sn_add_tags $itk_component(hull) {Toplevel all} end

	wm protocol $itk_component(hull) WM_TAKE_FOCUS \
            [itcl::code $this CheckTakeFocus $itk_component(hull)]

        eval itk_initialize $args

    }

    destructor {
# FIXME: Remove this grab stuff, it should be in Dialog class only!
        if {${old_grab} != ""} {
            if {[catch {${old_grab} grab}]} {
                ::grab set ${old_grab}
                # It might be not a Toplevel& object.
            }
        }
        bind_tk <Unmap> { }

        sn_input_check_in_window $itk_component(hull) 0

        if {[winfo exists ${leader}] && [winfo toplevel ${leader}] ==\
          ${leader}} {
            wm deiconify ${leader}
        }

        if {[winfo exists ${leader}]} {
            bind ${leader} <Map> {}
            bind ${leader} <Unmap> {}
            bind ${leader} <Destroy> {}
        }
    }

    # Widget Tk-methods.

    # Used only to query the current geometry since
    # itcl/itk does not support a cget query method.
    public method geometry {} {
       return [wm geometry $itk_component(hull)]
    }


    public method aspect {args} {
        return [eval wm aspect $itk_component(hull) ${args}]
    }

    public method centerOnScreen {} {
        global tcl_platform

	update idletasks

	# Use reqwidth/reqheight unless user explicitly set
	# a width and height for the window via -geometry.
	if {$explicit_width != -1} {
	    set wd $explicit_width
	    set explicit_width -1
	} else {
	    set wd [winfo reqwidth $itk_component(hull)]
	}
	if {$explicit_height != -1} {
	    set ht $explicit_height
	    set explicit_height -1
	} else {
	    set ht [winfo reqheight $itk_component(hull)]
	}
        set sw [winfo screenwidth $itk_component(hull)]
        set sh [winfo screenheight $itk_component(hull)]
	set x [expr {($sw-$wd)/2}]
	set y [expr {($sh-$ht)/2}]
	wm geometry $itk_component(hull) +$x+$y
    }

    public method client args {
        return [eval wm client $itk_component(hull) ${args}]
    }

    public method colormapwindows args {
        return [eval wm colormapwindows $itk_component(hull) ${args}]
    }

    public method command args {
        return [eval wm command $itk_component(hull) ${args}]
    }

    public method config_tk args {
        return [::eval $itk_component(hull) configure ${args}]
    }

    public method deiconify {} {
        return [wm deiconify $itk_component(hull)]
    }

    public method focusmodel args {
        return [eval wm focusmodel $itk_component(hull) ${args}]
    }

    # we cannot used the method name 'frame' because it would
    # conflict with the 'frame' command in Tk.
    public method frame_tk {} {
        return [wm frame $itk_component(hull)]
    }

    public method __grid args {
        return [eval wm grid $itk_component(hull) ${args}]
    }

    public method group args {
        return [eval wm group $itk_component(hull) ${args}]
    }

    public method iconify {} {
        return [eval wm iconify $itk_component(hull)]
    }

    public method iconposition args {
        return [eval wm iconposition $itk_component(hull) ${args}]
    }

    public method icontwindow args {
        return [eval wm icontwindow $itk_component(hull) ${args}]
    }

    public method maxsize args {
        return [eval wm maxsize $itk_component(hull) ${args}]
    }

    public method minsize args {
        return [eval wm minsize $itk_component(hull) ${args}]
    }

    public method overrideredirect args {
        return [eval wm overrideredirect $itk_component(hull) ${args}]
    }

    public method positionfrom args {
        return [eval wm positionfrom $itk_component(hull) ${args}]
    }

    public method protocol args {
        return [eval wm protocol $itk_component(hull) ${args}]
    }

    public method resizable args {
        return [eval wm resizable $itk_component(hull) ${args}]
    }

    public method sizefrom args {
        return [eval wm sizefrom $itk_component(hull) ${args}]
    }

    public method state {} {
        return [wm state $itk_component(hull)]
    }

    # FIXME : Can't we remove this completely and just use -title?
    public method title {{argl ""} {umlaut_map ""}} {
        set argl [join ${argl}]

        # FIXME: should this be != "" ?
        if {${umlaut_map} == ""} {
            set argl [map_umlauts ${argl}]
        }

        if {[string compare ${argl} ""] == 0} {
            #return [set itk_option(-title)]
	    return [wm title $itk_component(hull)]
        } else {
            #return [set itk_option(-title) ${argl}]
            return [wm title $itk_component(hull) ${argl}]
        }
    }

    public method transient {{window ""}} {
        global tcl_platform

        if {${window} == {}} {
            return [wm transient $itk_component(hull)]
        } else {
            if {! [winfo exists ${window}]} {
                error "window ${window} does not exist!"
            }
            # If the window is not a toplevel, find
            # the toplevel it lives in.
            set toplevel [winfo toplevel ${window}]
 
            # A bug in Tk 8.1 will remap a withdrawn
            # window when it becomes a transient.
            # We work around it here by unmapping
            # the window again. It is fixed in 8.3.

            if {$tcl_platform(platform) == "windows"} {
                set ismapped [winfo ismapped $itk_component(hull)]
            }

            wm transient $itk_component(hull) ${toplevel}

            if {$tcl_platform(platform) == "windows"
                    && !$ismapped} {
                ${this} withdraw

                # This update call is required under
                # windows, otherwise the window
                # will appear for a split second
                # before being withdrawn.
                update
            }
        }
    }

    public method withdraw {} {
        return [wm withdraw $itk_component(hull)]
    }

    # This function is useful to replace umlauts in window titles, because
    # the window managers (normaly) do not use iso8859-1 fonts.

    public proc map_umlauts {str} {
        regsub -all "\xfc" ${str} "ue" str
        regsub -all "\xe4" ${str} "ae" str
        regsub -all "\xf6" ${str} "oe" str
        regsub -all "\xdc" ${str} "Ue" str
        regsub -all "\xc4" ${str} "Ae" str
        regsub -all "\xd6" ${str} "Oe" str

        return ${str}
    }

    public method grab {args} {
        if {${args} == "set"} {
            set old_grab [lindex [::grab current] 0]
            ${this} bind_tk <1> "if {\"%W\" == \"$itk_component(hull)\"} {
					${this} move_to_mouse 0
					${this} raise
					bell -displayof %W
				}
				break
				"
            ${this} bind_tk <2> [${this} bind_tk <1>]
            ${this} bind_tk <3> [${this} bind_tk <1>]
        }\
        elseif {${args} == "release"} {
            ${this} bind_tk <1> { }
            ${this} bind_tk <2> [${this} bind_tk <1>]
            ${this} bind_tk <3> [${this} bind_tk <1>]
        }
        # FIXME: This seems to generate the error "grab failed: window not viewable"
        return [eval ::grab ${args} $itk_component(hull)]
    }

    public method raise {args} {
        global sn_force_flag tcl_platform

        if {[state] != "normal"} {
            set ret [deiconify]
        } else {
            #don't steal focus on UNIX
            if {$tcl_platform(platform) == "windows"} {
                set wdg [::focus -lastfor $itk_component(hull)]
                eval focus ${sn_force_flag} $itk_component(hull)
                focus ${wdg}
            }
            set ret [eval ::raise $itk_component(hull) ${args}]
        }
        return ${ret}
    }

    public method take_focus {{wdg ""}} {
        ::take_focus $itk_component(hull) ${wdg}
    }

    #bind Escape and WM-close hint to the window
    public method on_close {{cmd ""}} {
        global tcl_platform

        if {${cmd} == ""} {
            set cmd [list itcl::delete object ${this}]
        }

        ::wm protocol $itk_component(hull) WM_DELETE_WINDOW ${cmd}
        bind $itk_component(hull) <Escape> ${cmd}

        #on windows Alt-F4 closes window
        if {$tcl_platform(platform) == "windows"} {
            bind $itk_component(hull) <Alt-F4> ${cmd}
        }
    }

    public method bind_tk args {
        return [eval bind  $itk_component(hull) ${args}]
    }

    public method focus_tk {{args ""}} {
        return [eval focus ${args}  $itk_component(hull)]
    }

    public proc next_window_name {} {
        set wn "${internal_name}${seq}"
        incr seq
        return ${wn}
    }

    public method window_name {} {
        return $itk_component(hull)
    }

    public method move_to_mouse {{swap 1}} {
        #we never move the window to the mouse location
        after idle "[itcl::code ${this} centerOnScreen]; ${this} deiconify ; ${this} take_focus"
    }

    # This procedure checks to make sure it's not stealing
    # focus from a modal dialog.
    public method CheckTakeFocus {window} {

	set focusHolder [$this TopModalStack]

	if {$focusHolder == $window} {
	    return
	}

	if {$focusHolder == "" || ![winfo exists $focusHolder]} {
	    # Remove bogus entry from stack.
	    $this PopModalStack
	    return
	}

	# Get the last widget focus.
	set widgetfocus [::focus -lastfor $focusHolder]

	::focus -force $focusHolder

	# Raise focus holder window.
	::raise $focusHolder
	bell -displayof $focusHolder

	# Raise focus holder window again, after idle
	# so that if we lost focus we can get it back
	# after everything has settled down.
	after idle "::raise $focusHolder"

	# Set focus to the last widget that was active.
	::focus $widgetfocus
    }

    public method PopModalStack {} {

	if {$modalstack == ""} {
	    return
	}
	return [lvarpop modalstack]
    }

    public method PushModalStack {window} {
	lvarpush modalstack $window
    }

    public method TopModalStack {} {
	if {$modalstack != ""} {
	    return [lindex $modalstack 0]
	}
    }

    itk_option define -leader leader Leader "" {
        global tcl_platform

        if {$tcl_platform(platform) == "windows"} {
            set leader ""
        }
        if {${leader} != "" && [winfo exists ${leader}]} {
            set leader [winfo toplevel ${leader}]
        }
        if {${leader} != "" && [winfo exists ${leader}] && [winfo exists\
          $itk_component(hull)]} {
            bind ${leader} <Map> "catch {${this} deiconify}"
            bind ${leader} <Unmap> "catch {${this} withdraw}"
# FIXME: This <Destroy> would not work, and it really should not be used since
# itk does a Destroy binding for us.
            #bind ${leader} <Destroy> "catch {${this} delete}"
        }
    }

    itk_option define -input_check input_check Input_Check "1" {
        sn_input_check_in_window $itk_component(hull) $itk_option(-input_check)
    }

    itk_option define -iconbitmap iconbitmap IconBitmap "" {
        wm iconbitmap $itk_component(hull) $itk_option(-iconbitmap)
    }

    itk_option define -geometry geometry Geometry "" {
        # Full: 200x200+144+144
        # Short: 200x200
        # XY:    +144+144 (ignore)
        set geom $itk_option(-geometry)
        if {[regexp {([0-9]+)x([0-9]+)} $geom whole width height]} {
            set explicit_width $width
            set explicit_height $height
        }
        wm geometry $itk_component(hull) $geom
    }

    itk_option define -iconmask iconmask IconMask "" {
        wm iconmask $itk_component(hull) $itk_option(-iconmask)
    }

    itk_option define -iconname iconname IconName "" {
        wm iconname $itk_component(hull) $itk_option(-iconname)
    }

# FIXME: The -width and -height options don't work
# because the Toplevel class does not keep them.

    itk::usual Window {
        keep -background -bd -bg -borderwidth -container
        keep -colormap -cursor -height -menu -relief -title
        keep -screen -use -visual -width
    }

    protected variable leader ""

    protected variable old_grab ""

    protected variable explicit_width -1
    protected variable explicit_height -1

    common modalstack ""

    common internal_name .sn_

    # Currenltly, wm iconbitmap only support xbm. Either we need a b&w
    # version of the ide icon, or we need to add xpm support.

    set iconfile paf.xbm

    global sn_path
    common iconbitmap @[file join $sn_path(bitmapdir) ${iconfile}]

    common seq 0
}

proc take_focus {win {wdg ""}} {
    global sn_force_flag tcl_platform
    if {![winfo exists ${win}]} {
        return -1
    }
    if {$tcl_platform(platform) == "windows"} {
        eval focus ${sn_force_flag} [winfo toplevel ${win}]
    }
    if {${wdg} != "" && [winfo exists ${wdg}] && [winfo toplevel ${wdg}] !=\
      ${wdg}} {
        eval focus ${sn_force_flag} ${wdg}
    }
    return 0
}

proc window_configure {win {cmd normal} {focus_wdg ""}} {
    if {![winfo exists ${win}]} {
        return
    }
    switch -- ${cmd} {
        "normal" -
        "deiconify" {
                global sn_force_flag
                eval focus ${sn_force_flag} [focus -lastfor ${win}]
                wm deiconify ${win}
                eval focus ${sn_force_flag} [focus -lastfor ${win}]
                raise ${win}
                if {${focus_wdg} != ""} {
                    focus ${focus_wdg}
                }
            }
        "withdraw" {
                wm withdraw ${win}
            }
        default {
                error "unrecognized key: ${cmd}"
            }
    }
}

#bind Escape and WM-close hint to the window
proc on_close {w {cmd ""}} {
    if {${cmd} == ""} {
        set cmd "${this} delete"
    }
    wm protocol ${w} WM_DELETE_WINDOW ${cmd}
    bind ${w} <Escape> ${cmd}
}

proc sn_create_window {w {add_msg 1}} {
    global sn_options
    global sn_path sn_product_name sn_ide

    set m ${w}.menu
    set exp ${w}.exp
    set date ${w}.status.date
    set bitpath $sn_path(bitmapdir)

    if {![winfo exists ${w}]} {
        sourcenav::Window ${w} -menu ${m}
    } else {
        # FIXME: this should be part of the snavigator::Window class?
        set iconfile paf.xbm
        ${w} configure -iconbitmap @[file join ${bitpath} ${iconfile}]
        ${w} configure -menu ${m}
    }
    ${w} minsize 20 1

    menu ${m} -tearoff 0

    ## File menu
    ####################
    menu ${m}.file -tearoff 0 -postcommand "ProjectMenuEntries_post ${m}.file\
      subwindow"
    ${m} add cascade -label [get_indep String EditFile] -menu ${m}.file\
      -underline [get_indep Pos EditFile]

    #project menu entries
    AddProjectMenuEntries ${m}.file ${w}

    ${m}.file add separator

    #close window
    ${m}.file add command -command " eval \[wm protocol ${w}\
      WM_DELETE_WINDOW\] " -label [get_indep String WindowsClose]\
      -underline [get_indep Pos WindowsClose]

    ${m}.file add command -label [get_indep String Exit] -underline\
      [get_indep Pos Exit] -command " sn_exit "

    ## Edit menu
    menu ${m}.edit -tearoff 0

    ${m} add cascade -label [get_indep String EditEdit] -menu ${m}.edit\
      -underline [get_indep Pos EditEdit]

    #project preferences
    ${m}.edit add command -label [get_indep String ProjectPreferencesMenu]\
      -underline [get_indep Pos ProjectPreferencesMenu]\
      -command sn_project_preferences

    ## History
    AddHistMenu ${m}

    ##Windows
    AddWindowsMenu ${m} ${w}

    ##Help
    AddHelpMenu ${m} ${w}
    ${w} config -menu ${m}

    frame ${exp} -relief groove -bd 2

    pack ${exp} -side top -fill x

    frame ${w}.status
    pack ${w}.status -side bottom -fill x

    global ${w}.reusable
    set ${w}.reusable 1
    checkbutton ${w}.status.reuse -relief groove -text [get_indep String\
      Reuse] -variable ${w}.reusable -font $sn_options(def,layout-font)

    pack ${w}.status.reuse -side left

    if {${add_msg}} {
        set info ${w}.status.msg
        pack [label ${info} -font $sn_options(def,layout-font) -anchor w\
          -relief groove -bd 2] -side left -anchor w -fill both -expand y
        pack propagate ${info} 0
    }
}

proc sn_window_add_icons {w buttons} {
    set exp ${w}.exp

    #Hierarchy
    if {[lsearch -exact ${buttons} "ctree"] != -1} {
        button ${exp}.tree -takefocus 0 -image tree_image -command "sn_classtree" -state disabled
        bind_history ${exp}.tree tree
        balloon_bind_info ${exp}.tree [get_indep String INFOHierarchy]
        pack ${exp}.tree -side left
    }

    #Class
    if {[lsearch -exact ${buttons} "class"] != -1} {
        button ${exp}.class -takefocus 0 -image watch_image -command "sn_classbrowser" -state disabled
        bind_history ${exp}.class browse
        balloon_bind_info ${exp}.class [get_indep String INFOStartClassBrw]
        pack ${exp}.class -side left
    }

    #Xref
    if {[lsearch -exact ${buttons} "xref"] != -1} {
        button ${exp}.cross -takefocus 0 -image crossref_image -command "sn_xref"
        bind_history ${exp}.cross xref
        balloon_bind_info ${exp}.cross [get_indep String PafCrossRef]
        pack ${exp}.cross -side left
    }

    #Include
    if {[lsearch -exact ${buttons} "inc"] != -1} {
        button ${exp}.inc -takefocus 0 -image include_image -command "sn_include"
        bind_history ${exp}.inc xref
        balloon_bind_info ${exp}.inc [get_indep String IncludeLaunch]
        pack ${exp}.inc -side left
    }
}

proc sn_window_append_print {w cmd} {
    set m ${w}.menu
    set exp ${w}.exp
    set info ${w}.status.msg

    ${m}.edit insert 0 separator
    ${m}.edit insert 0 command -label [get_indep String Print]\
      -underline [get_indep Pos Print] -command ${cmd}
}
