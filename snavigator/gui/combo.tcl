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
# combo.tcl - A combo box widget.
# Copyright (C) 2000 Red Hat Inc.
# Copyright (C) 1998, 1999 Cygnus Solutions.

itcl::class Combo& {

    inherit itk::Widget

    constructor {args} {
        global sn_path

        if {${balloon} != "" && ${entryballoon} == ""} {
            set entryballoon ${balloon}
        }

        if {${balloon} != "" && ${buttonballoon} == ""} {
            set buttonballoon ${balloon}
        }

        itk_component add label {
            label $itk_component(hull).label
        } {
            rename -text -label text Text
            rename -width -labelwidth width Width
            keep -underline -anchor -font
        }

        itk_component add entry {
            entry $itk_component(hull).entry
        } {
            keep -width -exportselection -font
        }

        itk_component add arrow {
            menubutton $itk_component(hull).arrow \
               -relief raised \
               -image arrow_image \
               -menu $itk_component(hull).arrow.menu
        } {
            keep -font
        }

        if {! [info exists label_disabled_color]} {
            set label_disabled_color \
                [$itk_component(arrow) cget -disabledforeground]
        }

        # This is a really nasty hack, but under Windows
        # menus will not be mutually exclusive unless
        # you pass -type tearoff.

        global tcl_platform
        set type normal

        if {$tcl_platform(platform) == "windows"} {
            set type tearoff
        }

        itk_component add menu {
            menu $itk_component(arrow).menu \
                -tearoff 0 \
                -postcommand "${this} menu_place" \
                -type $type
        } {
            keep -font
        }

        wm overrideredirect $itk_component(menu) 1

# FIXME: What the heck is a tree widget doing inside a Combo box? This should be a listbox!

        # Passing -sort "" to the Tree is required, otherwise the
        # entries will be sorted alpha numerically by default!

        itk_component add tree {
	    Tree $itk_component(menu).tree \
                -sort ""
        } {
        }

        itk_component add treew {
            $itk_component(tree) tree
        } {}

        # The extended widget is used by the client.
        # It is a frame that will appear to the
        # right of the drop down list.

        itk_component add extended {
            frame $itk_component(menu).extended
        } {}

        # init the args.
        eval itk_initialize $args

        # Link the entry widget to the -entrytext property
        $itk_component(entry) configure -textvariable \
            [itcl::scope itk_option(-entrytext)]

        # Geometry management

        pack $itk_component(label) -side left -padx 1 -pady 1
        pack $itk_component(entry) -side left -fill both -expand true
        pack $itk_component(arrow) -side right -fill y

        # Event bindings

        bind $itk_component(entry) <Return> [itcl::code ${this} entry_selection]
        bind $itk_component(entry) <Return> +break

        bind $itk_component(entry) <Tab> {focus [tk_focusNext %W]}
        bind $itk_component(entry) <Shift-Tab> {focus [tk_focusPrev %W]}

        if {${entryballoon} != ""} {
            balloon_bind_info $itk_component(entry) ${entryballoon}
        }

        bind $itk_component(arrow) <Tab> {focus [tk_focusNext %W]}
        bind $itk_component(arrow) <Shift-Tab> {focus [tk_focusPrev %W]}
        if {${buttonballoon} != ""} {
            balloon_bind_info $itk_component(arrow) ${buttonballoon}
        }

        $itk_component(tree) treebind <B1-ButtonRelease> [itcl::code ${this} menu_selection]
        $itk_component(tree) treebind <space> "+${this} entry_travers; ${this} menu_unpost"
        $itk_component(tree) treebind <Return> [$itk_component(tree) treebind <space>]
        $itk_component(tree) treebind <Escape> "${this} menu_unpost"

        # Bind traversal keys
        bind $itk_component(entry) <Up> "tkListboxUpDown $itk_component(treew) -1;    ${this}\
          entry_travers"
        bind $itk_component(entry) <Down> "tkListboxUpDown $itk_component(treew) 1;     ${this}\
          entry_travers"
        bind $itk_component(entry) <Prior> "$itk_component(treew) yview scroll -1 pages; $itk_component(treew)\
          activate @0,0; ${this} entry_travers"
        bind $itk_component(entry) <Next> "$itk_component(treew) yview scroll 1 pages;  $itk_component(treew)\
          activate @0,0; ${this} entry_travers"

        # Remove the Menu binding from the popup, since we don't
        # want the default Menu bindings to mess with the focus.

        set tags [bindtags $itk_component(menu)]
        set ind [lsearch -exact ${tags} Menu]
        if {${ind} == -1} {
            error "Menu tag not found in bindtags list \{$tags\}"
        }
        set tags [lreplace ${tags} ${ind} ${ind}]
        bindtags $itk_component(menu) $tags

        # The default <ButtonRelease-1> binding for a MenuButton
        # will reset the focus. This steals the focus away
        # from the widget in the menu, so we need to reset
        # the focus again after an upclick.

        bind $itk_component(arrow) <ButtonRelease-1> [itcl::code $this menu_focus]
    }

    destructor {
        sourcenav::OptionTrace::deleteOptionTrace -entryvariable -entrytext \
            [itcl::scope itk_option]
    }

    method menu_popup {} {
        tkMbPost $itk_component(arrow) 0 0
    }

    method menu_place {} {
        global tcl_platform

        wm withdraw $itk_component(menu)
        set x [winfo rootx $itk_component(entry)]
        set y [expr {[winfo rooty $itk_component(entry)] +
            [winfo height $itk_component(entry)]}]

        set ww [expr {[winfo width $itk_component(entry)] +
            [winfo width $itk_component(arrow)]}]
        set hh [winfo height $itk_component(menu)]

        if {${hh} < 200} {
            set hh 200
        }

        # Manage extended widget if user has packed into it.

        if {[winfo children $itk_component(extended)] != {}} {
            set extw [winfo reqwidth $itk_component(extended)]
            set exth [winfo reqheight $itk_component(extended)]
            if {${hh} < ${exth}} {
                set hh [expr {${exth} + ${bd}*2}]
            }

            # Position extended widget to the right of drop down list
            place $itk_component(extended) -x [expr {${ww}-${bd}}] -y ${bd} \
                -width ${extw} -height [expr {${hh} - ${bd}*2}]
        } else {
            set extw 0
            set exth 0
            place forget $itk_component(extended)
        }

        # Position tree widget directly under the entry widget.
        # Note that we need to use place because the menu code
        # uses place and you can't mix geometry managers.

        place $itk_component(tree) -x ${bd} -y ${bd} \
            -width [expr {${ww} - ${bd}*2}] -height [expr {${hh} - ${bd}*2}]

        # Add the extended window width to the final menu width
        set ww [expr {${ww} + ${extw}}]

        # Figure out the screen width and height every time
        # because the user might have changed the screen resolution
        set screenw [winfo screenwidth $itk_component(hull)]
        set screenh [winfo screenheight $itk_component(hull)]

        #don't display a part of the window outside the screen
        if {(${x} + ${ww}) > $screenw} {
            set x [expr {$screenw - ${ww}}]
        }

        if {(${y} + ${hh}) > $screenh} {
            set y [expr {$screenh - ${hh}}]
        }

        if {$itk_option(-postcommand) != ""} {
            eval $itk_option(-postcommand)
        }

        #when we don't raise the menu window, it isn't viewed
        #in MS Windows.
        if {$tcl_platform(platform) == "windows"} {
            after idle "
				wm overrideredirect $itk_component(menu) 1
				wm deiconify $itk_component(menu)
				wm geometry $itk_component(menu) ${ww}x${hh}+${x}+${y} 
				raise $itk_component(menu)
				[itcl::code $this menu_focus]
                       "
        } else {
            after idle "
				wm overrideredirect $itk_component(menu) 1
				wm geometry $itk_component(menu) ${ww}x${hh}+${x}+${y} 
				wm deiconify $itk_component(menu)
				raise $itk_component(menu)
				[itcl::code $this menu_focus]
                       "
        }
    }

    # Called when a menu has been displayed, we assign
    # the focus to a widget in the menu during this callback

    private method menu_focus {} {
        #sn_log "refocused to $itk_component(treew)"
        after idle "focus -force $itk_component(treew)"
        set sel [$itk_component(treew) curselection]
        if {$sel != ""} {
            after idle "$itk_component(treew) see ${sel}"
        }
    }

    # This callback is invoked when the user selects
    # an item out of the drop down list.

    private method menu_selection {} {
        set sel [lindex [$itk_component(treew) curselection] 0]
        if {${sel} != ""} {
            set txt [$itk_component(treew) get ${sel}]
            $this configure -entrytext ${txt}
        }
        tkMenuUnpost $itk_component(menu)
        invoke_command
    }

    method menu_unpost {} {
        tkMenuUnpost $itk_component(menu)
    }

    # This callback is invoked when the user
    # hits Enter in the entry box

    private method entry_selection {} {
        if {$itk_option(-entrytext) == ""} {
            return
        }

        invoke_command
    }

    # This method will underline the first element
    # of the list in the popup so that it can
    # be selected by a key press shortcut.

    method entry_travers {} {
        $itk_component(treew) selection clear 0 end
        $itk_component(treew) selection set active
        set sel [lindex [$itk_component(treew) curselection] 0]
        if {${sel} == ""} {
            if {[$itk_component(treew) size] > 0} {
                set sel 0
            } else {
                set sel -1
            }
        }
        if {${sel} != -1} {
            $this configure -entrytext [$itk_component(treew) get ${sel}]
        }
        invoke_command
    }

    method invoke_command {} {
        if {$itk_option(-selectcommand) != ""} {
            eval $itk_option(-selectcommand) [list $itk_option(-entrytext)]
        }
    }

    #select combobox entry that match a given text
    #if offset is greater than 0 the item with this
    #offset is selected
    method selecttext {txt {off 0}} {
        set idx [$itk_component(treew) search -exact -- ${txt} 0]
        if {${idx} == ""} {
            set idx 0
        }
        set idx [expr {${idx} + ${off}}]
        if {${idx} != "" && ${idx} != -1} {
            $itk_component(treew) selection clear 0 end
            $itk_component(treew) selection set ${idx}
            $itk_component(treew) see ${idx}
        }
        $this configure -entrytext ${txt}
        return $idx
    }

    #return the offset of the selected text if there
    #is more than one entry with the same text
    method offset {} {
        set sel [$itk_component(treew) curselection]
        if {${sel} == ""} {
            return 0
        }
        set txt [$itk_component(treew) get ${sel}]
        set idx [$itk_component(treew) search -exact -- ${txt} 0]
        if {${sel} != "" && ${idx} != ""} {
            return [expr {${sel} - ${idx}}]
        } else {
            return 0
        }
    }

    # Variables and options

    private variable bd 1

    # This user callback is invoked when the user types a value
    # into the text box and hits enter or selects a value from
    # the drop down list.

    itk_option define -selectcommand selectCommand SelectCommand ""

    # This user callback is invoked when the menu window appears on screen
    itk_option define -postcommand postCommand PostCommand ""

    itk_option define -filter filter Filter ""

    public variable entryballoon ""
    public variable buttonballoon ""
    public variable balloon ""

    # The -state and -readonly options are
    # a bit tricky. We don't pass -state
    # as a keep option to the widgets since
    # we need to configure the entry and
    # keep track of the -readonly option.
    # The -state readonly is also not available
    # in Tk 8.3, it only supports normal
    # and disabled.

    itk_option define -state state State normal {
        if {$itk_option(-state) != "normal" &&
            $itk_option(-state) != "disabled"} {
            error "bad state \"$itk_option(-state)\":\
                must be disabled, or normal"
        }

        $itk_component(label) configure -state $itk_option(-state)
        $itk_component(arrow) configure -state $itk_option(-state)
        if {$itk_option(-state) == "disabled"} {
            $itk_component(entry) configure -state disabled
        } else {
            $itk_component(entry) configure -state normal

            if {$itk_option(-readonly)} {
                bind $itk_component(entry) <KeyPress> break
            } else {
                bind $itk_component(entry) <KeyPress> {}
            }
        }
    }

    itk_option define -readonly readOnly ReadOnly 0 {
        if {$itk_option(-readonly)} {
            if {[string equal $itk_option(-state) "normal"]} {
                bind $itk_component(entry) <KeyPress> break
            }
        } else {
            bind $itk_component(entry) <KeyPress> {}
        }
    }

    # FIXME: this wouldn't be needed if you could
    #   ${tree} configure -contents ...
    itk_option define -contents contents Contents "" {
        if {[winfo exists $itk_component(tree)]} {
            $itk_component(tree) setcontents $itk_option(-contents)
            $itk_component(tree) contents
        }
    }

    # The variable for the entry box text is set with this option

    itk_option define -entrytext entrytext Entrytext ""

    # The variable for the entry box text is set with this option

    itk_option define -entryvariable entryvariable Entryvariable "" {
        sourcenav::OptionTrace::configureOptionTrace -entryvariable -entrytext \
            [itcl::scope itk_option]
    }
}


