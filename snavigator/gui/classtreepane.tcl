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
#####################################################
##
## Class hierarchy that can be integrated in any subwindow.
##
#####################################################
itcl::class ClassTree& {
    inherit sourcenav::MultiChild
    constructor {args} {
        global sn_options

        set topw [winfo toplevel $itk_component(hull)]

        # Set default layout and order for classtree.
        global ${this}-layoutstyle
        global ${this}-order
        set ${this}-order $sn_options(def,ctree-view-order)
        set ${this}-layoutstyle $sn_options(def,ctree-layout)

	itk_component add canvas {
	    canvas $itk_component(hull).canvas\
		    -borderwidth 2\
		    -xscrollcommand "${this}.scrollx set"\
		    -yscrollcommand "${this}.scrolly set"\
		    -xscrollincrement 20\
		    -yscrollincrement 20
	} { }

	itk_component add scrollx {
	    scrollbar $itk_component(hull).scrollx\
		    -orient horiz\
		    -command "$itk_component(canvas) xview"\
		    -jump $sn_options(def,canvas-tree-jump)
	}

	itk_component add scrolly {
	    scrollbar $itk_component(hull).scrolly\
		    -command "$itk_component(canvas) yview"\
		    -jump $sn_options(def,canvas-tree-jump)
	}

        grid $itk_component(canvas) -row 0 -column 0 -sticky news
        grid $itk_component(scrollx) -row 1 -column 0 -sticky ew
        grid $itk_component(scrolly) -row 0 -column 1 -sticky ns

        grid rowconfigure $itk_component(hull) 0 -weight 1
        grid columnconfigure $itk_component(hull) 0 -weight 1

        $itk_component(canvas) bind cl <1> "${this} mark_class"
        $itk_component(canvas) bind cl <Control-1>\
		"${this} Focus_On current 0"
        $itk_component(canvas) bind cl <Shift-1>\
		"${this} Focus_On current 0"
        $itk_component(canvas) bind cl <Double-1> "${this} edit_it \[%W itemcget current -text\]"

        # Display filenames with the classes.
        global ${this}-disp-files
        set ${this}-disp-files ${display_filenames}

	eval itk_initialize $args

        # Add menu entries for the class hierarchy.
        if {$itk_option(-menu) != "" && [winfo exists $itk_option(-menu)]} {

            $itk_option(-menu) add command\
		    -label [get_indep String MultiHierDef]\
		    -underline [get_indep Pos MultiHierDef]\
		    -command "${this} Focus_On"\
                    -state disabled

            $itk_option(-menu) add command\
		    -label [get_indep String MultiHierRel]\
		    -underline [get_indep Pos MultiHierRel]\
		    -command "${this} Focus_On \[$itk_component(canvas) select item\] 0"\
	            -state disabled

            $itk_option(-menu) add command\
		    -label [get_indep String MultiHierAll]\
		    -underline [get_indep Pos MultiHierAll]\
		    -command "${this} DrawAll"\
                    -state disabled

            $itk_option(-menu) add separator
            $itk_option(-menu) add checkbutton\
		    -label [get_indep String HierDispFiles]\
		    -underline [get_indep Pos HierDispFiles]\
		    -variable ${this}-disp-files\
		    -command " ${this} toggle_disp_files "
        }

        # View icons on the toolbar.
        if {$itk_option(-toolbar) != ""} {

	    itk_component add toolbar {
		frame $itk_option(-toolbar).classtreetoolbar
	    }

            pack $itk_component(toolbar) -side left

	    itk_component add showall {
		button $itk_component(toolbar).tree\
			-image tree_image\
			-takefocus 0 -command ""\
			-state normal
	    }

            bind $itk_component(showall) <Any-ButtonPress>\
		    "if {%b == 1} {
		         ${this} DrawAll
	             }"

            balloon_bind_info $itk_component(showall)\
		    [get_indep String MultiHierAllINFO]

	    itk_component add showrelated {
		button $itk_component(toolbar).deftree\
			-image stree_image -takefocus 0\
			-command "" -state disabled
	    }

            bind $itk_component(showrelated) <Any-ButtonPress>\
		    "if {%b == 1} {${this} Focus_On}"

            balloon_bind_info $itk_component(showrelated)\
		    [get_indep String MultiHierDefINFO]

	    itk_component add showderived {
		button $itk_component(toolbar).down\
			-image down_image -takefocus 0 -command "  "\
			-state disabled
	    }

            bind $itk_component(showderived) <Any-ButtonPress>\
		    "if {%b == 1} {
		         ${this} Focus_On \[$itk_component(canvas) select item\] 0
                     }"


	    balloon_bind_info $itk_component(showderived)\
		    [get_indep String MultiHierRelINFO]

            pack $itk_component(showall) -side left
            pack $itk_component(showrelated) -side left
            pack $itk_component(showderived) -side left
        }

        # Option menu for Class Tree items.
	itk_component add menu {
	    menu $itk_component(canvas).menu\
		    -tearoff 0 -postcommand\
		     "${this} update_post_menu $itk_component(canvas).menu"
	} {}
	
	wm overrideredirect $itk_component(menu) 1

        # See super- and baseclasses.
        $itk_component(menu) add command\
		-label [get_indep String MultiHierDef]\
		-underline [get_indep Pos MultiHierDef]\
		-command "${this} Focus_On"\
		-state disabled

        # See superclasses.
        $itk_component(menu) add command\
		-label [get_indep String MultiHierRel]\
		-underline [get_indep Pos MultiHierRel]\
		-command "${this} Focus_On \[$itk_component(canvas) select item\] 0"\
		-state disabled

        $itk_component(menu) add separator
        $itk_component(menu) add checkbutton\
		-label [get_indep String HierDispFiles]\
		-underline [get_indep Pos HierDispFiles]\
		-variable ${this}-disp-files\
		-command "${this} toggle_disp_files"

        $itk_component(menu) add separator

        # Bind option menu.
        $itk_component(canvas) bind cl <ButtonPress-3>\
		"${this} mark_class; tk_popup $itk_component(canvas).menu %X %Y"

        # Init some variables.
        FillValues

        Update_Layout

        # As first step load all availiable classes and build the
        # hierarchy.
        if {[info commands paf_db_cl] == ""} {
            reset
        }

        # Is a class selected?
        goto "" $itk_option(-classname)

        # Call user defined procedure.
        catch {sn_rc_classhierarchy ${this} $itk_component(canvas)}
    }

    destructor {
        # If this is the last object, reset the common variables.
        if {[itcl::find objects -class [info class]] == ${this}} {
            reset
        }
        foreach v [::info globals "${this}-*"] {
            catch {uplevel #0 unset ${v}}
        }
    }

    method update_post_menu {m} {
        set clname [currentclass current]

        set state normal
        ${m} entryconfig [get_indep String MultiHierDef] -state ${state}
        ${m} entryconfig [get_indep String MultiHierRel] -state ${state}

        set num [${m} index end]
        if {${num} >= 5} {
            ${m} delete 5 end
        }

        set vw ""

        set lbl " [get_indep String Of] '${clname}'"

        # Editor
        ${m} add command -label "${vw}[get_indep String MultiEditor]${lbl}"\
          -underline [get_indep Pos MultiEditor] -command "${this} edit_it ${clname}" -state normal

        #Class
        ${m} add command -label "${vw}[get_indep String MultiClass]${lbl}"\
          -underline [get_indep Pos MultiClass] -command "sn_classbrowser ${clname}" -state normal

        #Xref
        ${m} add command -label "${vw}[get_indep String MultiXRef]${lbl}"\
          -underline [get_indep Pos MultiXRef] -command " sn_xref both\
          ${clname}(cl) " -state normal
    }

    method print {} {
        global sn_options tcl_platform

        if {$tcl_platform(platform)=="windows"} {

            ide_print_canvas $itk_component(canvas)

        } else {
            if {${print_dialog} == "" || [itcl_info objects ${print_dialog}]\
              == ""} {
                set print_dialog [PrintDialog $itk_component(hull).printdialog \
                  -leader ${topw} \
                  -modality application \
                  -canvas $itk_component(canvas)\
                  -file [file join $sn_options(profile_dir)\
                  tree.ps]]

	        $print_dialog transient ${topw}
	        $print_dialog activate
	        itcl::delete object $print_dialog
            } else {
                ${print_dialog} raise
            }
        }
    }

    #returns a list of all classes that inherit the specified
    #class
    proc parent_inh {cls} {
        global sn_sep
        if {! [info exists parent_classes(${cls})]} {
            if {[::info commands paf_db_in] != ""} {
                set parent_classes(${cls}) [::lunique [::lsort\
                  -dictionary [paf_db_in seq -data -col 1 "${cls}${sn_sep}"]]]
            } else {
                set parent_classes(${cls}) ""
            }
        }
        return $parent_classes(${cls})
    }

    method get_parent {id} {
        set found 0
        set CanDraw(${id}) 1
        set prnts ""
        foreach a [parent_inh ${id}] {
            set found 1
            #set CanDraw($a) 1
            if {! [get_parent ${a}]} {
                lappend tag_level ${a}
            }
        }
        return ${found}
    }

    method Draw_Classes {rot {all 0} {base ""}} {
        global sn_options
        upvar #0 $itk_component(canvas)-SupNum SupNum
        upvar #0 $itk_component(canvas)-BasNum BasNum

        if {[info exists class_drawn(${rot})]} {
            if {${base} != ""} {
                if {![info exists SupNum(${base})]} {
                    set SupNum(${base}) 1
                } else {
                    incr SupNum(${base})
                }
                if {![info exists BasNum(${rot})]} {
                    set BasNum(${rot}) 1
                } else {
                    incr BasNum(${rot})
                }
            }
            return
        }

        foreach entry [all_inheritences ${rot}] {
            if {${rot} != ${entry}} {
                Draw_Classes ${entry} ${all} ${rot}
            }
        }

        if {${all} || ![info exists CanDraw] || [info exists CanDraw(${rot})]\
          && $CanDraw(${rot})} {
            set class_drawn(${rot}) 1

            if {${base} != ""} {
                if {![info exists SupNum(${base})]} {
                    set SupNum(${base}) 1
                } else {
                    incr SupNum(${base})
                }
                if {![info exists BasNum(${rot})]} {
                    set BasNum(${rot}) 1
                } else {
                    incr BasNum(${rot})
                }
            }
            if {[is_abstract ${rot}]} {
                set fnt $sn_options(def,abstract-font)
            } else {
                set fnt $sn_options(def,ctree-font)
            }
            if {${display_filenames}} {
                set cnt [read_matched_from_db "" cl -exact ${rot} "" "" "" -1\
                  -1 0 0]
                set file [lindex [split ${cnt} \t] 4]
                if {${file} != ""} {
                    set file "\n${file}"
                }
                $itk_component(canvas) create text -1000 -1000 \
			-text ${rot}${file}\
			-tags [list ${rot} cl] -anchor nw -font ${fnt}
            } else {
                $itk_component(canvas) create text -1000 -1000\
			-text ${rot}\
			-tags [list ${rot} cl]\
			-anchor nw -font ${fnt}
            }
        }
    }

    method Draw_Links {} {
        global sn_options
        foreach cls [all_sorted_cls] {
            if {![info exists class_drawn(${cls})]} {
                continue
            }
            foreach child [all_inheritences ${cls}] {
                if {[info exists class_drawn(${child})]} {
                    #refer itself, unended loop, catch it
                    if {${child} != ${cls}} {
                        $itk_component(canvas) create edge -1m -1m -1m -1m -tag\
                          [list ch:${child} e:${child} e:${cls}] -arrow last\
                          -fill $sn_options(def,canv-line-fg) -from ${cls}\
                          -to ${child}
                    }
                }
            }
        }
    }

    #invoked to view the super- or subclasses of a specified class
    method Focus_On {{tag ""} {lev -1}} {
        if {${tag} == ""} {
            set id [$itk_component(canvas) select item]
            if {${id} == ""} {
                return
            }
            #set tag [$can itemcget $id -text]
            set tag [currentclass ${id}]
        }\
        elseif {${tag} == "current"} {
            #set tag [$can itemcget $tag -text]
            set tag [currentclass ${tag}]
        }

        if {${tag} == ""} {
            return
        }

        catch {unset CanDraw}

        if {${lev} == -1} {
            set RelativClass ${tag}

            #get the first base class of the specified class
            #view super- and subcalsses
            set tag_level ""
            get_parent ${tag}

            if {${tag_level} == ""} {
                set show_tags ${tag}
            } else {
                set show_tags ${tag_level}
            }
        } else {
            #make the specified class to be root (view subclasses)
            #set show_tags [$can itemcget $tag -text]
            set show_tags [currentclass ${tag}]
            set tag ${show_tags}
        }
        if {${show_tags} != ""} {
            set base_root ${show_tags}
            Redraw
            see_item ${tag}
        }
    }

    method edit_it {name} {
        if {${name} == ""} {
            return
        }
        if {$itk_option(-doubleclickcommand) != ""} {
            ::eval $itk_option(-doubleclickcommand) [Selection]
        }
    }

    method mark_class {{cname ""}} {
        global sn_options
        upvar #0 $itk_component(canvas)-marked_edges m_edges
        upvar #0 $itk_component(canvas)-SupNum SupNum
        upvar #0 $itk_component(canvas)-BasNum BasNum

        if {${cname} == ""} {
            set id current
            set cname [currentclass ${id}]
        } else {
            set id [$itk_component(canvas) find withtag ${cname}]
        }

        if {${cname} == "" && $itk_option(-message_var) != ""} {
            set $itk_option(-message_var) " "
            return
        }
        catch {$itk_component(canvas) select from ${id} 0}
        catch {$itk_component(canvas) select to ${id} end}

        # If the selection is not owned by us we do not
	# want the marking to disappear.
        selection own $itk_component(canvas) " "

        # Unmark old selection.
        catch {$itk_component(canvas) itemconfig "e:${m_edges}"\
          -fill $sn_options(def,canv-line-fg)}
        set m_edges ${cname}

        $itk_component(canvas) itemconfig e:${cname}\
		-fill $sn_options(def,tree-select-line)
        $itk_component(canvas) raise e:${cname}

        control_buttons normal

        # Display number of super- and baseclasses of the marked class.
        if {[info exists SupNum(${cname})]} {
            set supnum $SupNum(${cname})
        } else {
            set supnum 0
        }
        if {[info exists BasNum(${cname})]} {
            set basnum $BasNum(${cname})
        } else {
            set basnum 0
        }

        if {$itk_option(-message_var) != ""} {
            if {[expr ${basnum} + ${supnum}] == 0} {
                set $itk_option(-message_var) ""
            } else {
                set $itk_option(-message_var) "Baseclasses: ${basnum}, Superclasses: ${supnum}"
            }
        }

        #execute assigned command, if availiable
        if {$itk_option(-selectcommand) != ""} {
            ::eval $itk_option(-selectcommand) [Selection]
        }

        if {$itk_option(-symbols) != ""} {
            $itk_option(-symbols) selecttext ${cname}
        }
        return [list ${cname} ${id}]
    }

    # This function assures that the item will be on the screen.
    method see_item {{cname ""}} {
        if {[string match {[0-9]*} ${cname}] == 0} {
            set opts [mark_class ${cname}]
            set cname [lindex ${opts} 0]
            set id [lindex ${opts} 1]
        } else {
            set id ${cname}
        }

        #selected class isn't drawn
        if {"${id}" == "" || ${id} == -1} {
            return "error"
        }

        set coords [$itk_component(canvas) bbox ${id}]
        set x1 [$itk_component(canvas) canvasx 0]
        set x2 [$itk_component(canvas) canvasx \
		[winfo width $itk_component(canvas)]]
        set y1 [$itk_component(canvas) canvasy 0]
        set y2 [$itk_component(canvas) canvasy \
		[winfo height $itk_component(canvas)]]
        set enclosed [$itk_component(canvas) find enclosed\
		${x1} ${y1} ${x2} ${y2}]

        # If the item is not fully on the screen, we scroll the canvas.
        if {[lsearch -exact ${enclosed} ${id}] == -1} {
            set scr_reg [lindex [$itk_component(canvas) configure\
		    -scrollregion] 4]

            if {${scr_reg} == ""} {
                return ""
            }

            set wid [expr [lindex ${scr_reg} 2] - [lindex ${scr_reg} 0]]
            set hei [expr [lindex ${scr_reg} 3] - [lindex ${scr_reg} 1]]

            set xoff [expr double([winfo width $itk_component(canvas)]) / 3]
            set pos [expr double([lindex ${coords} 0] - ${xoff}) / ${wid}]
            if {${pos} < 0.0} {
                set pos 0.0
            }
            $itk_component(canvas) xview moveto ${pos}

            set yoff [expr double([winfo height $itk_component(canvas)]) / 3]
            set pos [expr double([lindex ${coords} 1] - ${yoff}) / ${hei}]
            $itk_component(canvas) yview moveto ${pos}
        }
        return ""
    }

    method toggle_disp_files {} {
        upvar #0 ${this}-disp-files dispfiles
        set display_filenames ${dispfiles}
        Redraw
    }

    method DrawAll {} {
        set old_cls [lindex [Selection] 1]
        set base_root ""

        Redraw

        if {${old_cls} != ""} {
            goto "" ${old_cls}
        } else {
            control_buttons disabled
        }
    }

    method Redraw {} {

        # Block the UI during the redraw.
        tixBusy $itk_component(hull) on

        graph $itk_component(canvas) destroy
        $itk_component(canvas) delete all

        upvar #0 $itk_component(canvas)-SupNum SupNum
        upvar #0 $itk_component(canvas)-BasNum BasNum

        catch {unset class_drawn}
        catch {unset SupNum}
        catch {unset BasNum}

        # Draw all classes.
        if {${base_root} == ""} {
            foreach entry [all_sorted_cls] {
                Draw_Classes ${entry} 1
            }
            # Draw related classes.
        } else {
            if {[info exists CanDraw]} {
                Draw_Classes ${RelativClass} 1
            }
            foreach entry ${base_root} {
                Draw_Classes ${entry}
            }
        }

        Draw_Links
        set_sorted_order
        graph_new_layout

        # To refill classes into combo box.
        set displayed 0

        # Return UI control to the user.
        tixBusy $itk_component(hull) off
    }

    method values {} {
        return ${base_root}
    }

    method control_buttons {state} {

        if {${base_root} == ""} {
            set allstate disabled
            set allrel sunken
        } else {
            set allstate normal
            set allrel raised
        }

        if {$itk_option(-menu) != "" && [winfo exists $itk_option(-menu)]} {
            $itk_option(-menu) entryconfig [get_indep String MultiHierDef]\
		    -state ${state}
            $itk_option(-menu) entryconfig [get_indep String MultiHierRel]\
		    -state ${state}
            $itk_option(-menu) entryconfig [get_indep String MultiHierAll]\
		    -state ${allstate}
        }

        if {$itk_option(-toolbar) != ""} {
    #        set exp ${toolbar}.ctreefr

            $itk_component(showderived) configure -state ${state}

            if {${state} == "disabled"} {
                set relief "-relief raised"
            } else {
                set relief ""
            }

            $itk_component(showrelated) configure -state ${state}
            $itk_component(showall)  configure -state ${allstate}\
		    -relief ${allrel}
        }
    }

    method set_sorted_order {} {
        set items [$itk_component(canvas) find withtag all]
        if {${items} != ""} {
            eval graph $itk_component(canvas) add ${items}
            return ""
        } else {
            return "error"
        }
    }

    method graph_new_layout {{redraw 1}} {
        global sn_options
        upvar #0 ${this}-order order
        upvar #0 ${this}-layoutstyle layout

        if {${redraw}} {
            graph $itk_component(canvas) clear
            if {[set_sorted_order] != ""} {
                return
            }
        }

        if {${order} == ""} {
            set order 0
        }

        set cmd [list graph $itk_component(canvas) config\
		    -gridlock 1 -order ${order}]
        if {${order} == 0} {
            lappend cmd -nodespacev $sn_options(def,ctree-horizspace)\
              -nodespaceh $sn_options(def,ctree-vertspace)
        } else {
            lappend cmd -nodespacev $sn_options(def,ctree-vertspace)\
              -nodespaceh $sn_options(def,ctree-horizspace)
        }

        #execute graph command
        eval ${cmd}

        graph $itk_component(canvas) layout ${layout}

        set reg [$itk_component(canvas) bbox all]

        set wd [lindex ${reg} 2]
        set he [lindex ${reg} 3]
        $itk_component(canvas) configure -scrollregion [list 0 0 ${wd} ${he}]

        return [list ${wd} ${he}]
    }

    method Title {{full 1}} {
        global sn_options
        set t "[get_indep String ClassHierarchy]"
        if {${base_root} != ""} {
            set t "${t}: ${base_root}"
        }
        if {${full}} {
            return [sn_title ${t}]
        } else {
            return ${t}
        }
    }
    method Icon {} {
        return [sn_view_icon [get_indep String ClassHierarchy] ${base_root}]
    }
    method SetTitle {} {
        ${topw} configure -title [Title] -iconname [Icon]
    }

    # Fill combobox with the correct entries
    # Filter for symbols.
    method filter {{all 0}} {
        upvar #0 $itk_option(-symbols_filter)-related related
        if {$itk_option(-symbols) != ""} {
            if {${related} && ${base_root} != ""} {
                $itk_option(-symbols) configure -contents [array names class_drawn]
            } else {
                $itk_option(-symbols) configure -contents [all_sorted_cls]
            }
        }
    }

    # This function is called, when the symbols combobox
    # is called.
    method postcommand {m} {
        if {! ${displayed}} {
            filter
            set displayed 1
        }
    }

    method activate {} {
        #display the contents always by activating
        set displayed 0

        if {$itk_option(-menu) != "" && [winfo exists $itk_option(-menu)]} {
            set mn [string range $itk_option(-menu)\
		    0 [expr [string last "." $itk_option(-menu)] - 1]]
            ${mn} entryconfig [get_indep String MultiClassHierarchy]\
              -state normal
        }
        if {$itk_option(-toolbar) != ""} {
            pack $itk_component(toolbar) -side left
        }
        if {$itk_option(-symbols) != ""} {
            $itk_option(-symbols) configure -entrytext ${base_root}
        }
    }

    method deactivate {} {
        if {$itk_option(-menu) != "" && [winfo exists $itk_option(-menu)]} {
            set mn [string range $itk_option(-menu) 0\
		    [expr [string last "." $itk_option(-menu)] - 1]]
            ${mn} entryconfig [get_indep String MultiClassHierarchy]\
              -state disabled
        }

        if {$itk_option(-toolbar) != ""} {
            pack forget $itk_component(toolbar)
        }
    }

    method gotosymbol {{scope ""} {sym ""} {cls ""} {file ""} {from ""} {type\
      ""} {prm ""} {to ""} {always 1}} {
        if {${cls} != ""} {
            return [goto "" ${cls}]
        }
        if {${sym} != ""} {
            return [goto "" ${sym}]
        }
        return 0
    }

    method goto {combo class} {
        global sn_options sn_sep
        if {${class} == ""} {
            return
        }

        #is the class availiable
        if {[::info commands paf_db_cl] != ""} {
            set avail [paf_db_cl seq -data -first -col 0 "${class}${sn_sep}"]
            if {${avail} == ""} {
                return
            }
        } else {
            return
        }

        #dump the current view into the history stack
        ${topw} history_stack_add_point ${this}

        #class already displayed
        if {[see_item ${class}] == ""} {
            return
        }
        #draw the new class
        Focus_On ${class}

        if {$itk_option(-symbols) != ""} {
            $itk_option(-symbols) selecttext ${class}
        }
    }

    method currentclass {{id ""}} {
        if {${id} == ""} {
            set id current
        }
        return [lindex [split [$itk_component(canvas) itemcget ${id} -text] \n] 0]
    }

    method Selection {} {
        set sel [$itk_component(canvas) itemcget \
		[$itk_component(canvas) select item] -text]
        set sel [lindex [split ${sel} \n] 0]
        if {${sel} != ""} {
            return [list cl ${sel} "" "" "" "" "" ""]
        }
        return ""
    }

    method clearselection {} {
        $itk_component(canvas) select clear
    }

    method Focus {} {
        focus $itk_component(canvas)
    }

    #make a title for the Dump position, this is usefull
    #for the views stack (prev/next)
    method DumpTitle {{dump ""}} {
        if {${dump} == ""} {
            set dump [Dump]
        }
        return "CTree [lindex ${dump} 1]"
    }

    method AddHistoryFromDump {dump title} {
        set class [lindex ${dump} 1]
        sn_add_history ctree ${class} [sn_make_history_title tree "" ${class}]\
          sn_classtree
    }

    #return the important data to restore this widget
    #in later time again (used by saving the project)
    method Dump {} {
        return [Selection]
    }

    #gets the result from the function "Dump" to
    #restore the older state (used by restoring the project)
    method Restore {str} {
        eval ${this} gotosymbol ${str}
        #Focus
    }

    method Close {{mode 0}} {
        return 1
    }

    method Refresh_Display {} {
        global sn_options
        reset
        catch {unset sorted_classes}
    }

    method Update_Layout {} {
        global sn_options

        global ${this}-layoutstyle
        global ${this}-order

        #apply new colors to the canvas
        $itk_component(canvas) configure\
		-selectforeground $sn_options(def,select-fg)\
		-selectbackground $sn_options(def,select-bg)

        set ${this}-layoutstyle $sn_options(def,ctree-layout)
        set ${this}-order $sn_options(def,ctree-view-order)

        graph_new_layout 1
    }

    method whoami {} {
        return ctree
    }

    proc reset {} {
        catch {unset abstract_classes}
        catch {unset sorted_classes}
        catch {unset parent_classes}
    }

    proc is_abstract {cls} {
        global sn_sep
        #use cache to determine if the class has abstracts
        if {[info exists abstract_classes(${cls})]} {
            return $abstract_classes(${cls})
        }
        if {[::info commands paf_db_md] != ""} {
            set virtuals [paf_db_md seq -first -col [list 0 1\
              "5 :${virt_bit}:${virt_bit}"] "${cls}${sn_sep}"]
            if {${virtuals} == ""} {
                set virtuals [paf_db_md seq -first -col [list 0 1\
                  "5 :${purevirt_bit}:${purevirt_bit}"] "${cls}${sn_sep}"]
            }
        } else {
            set virtuals ""
        }
        if {${virtuals} != ""} {
            set abstract_classes(${cls}) 1
        } else {
            set abstract_classes(${cls}) 0
        }
        return $abstract_classes(${cls})
    }

    proc all_sorted_cls {} {
        if {[info exists sorted_classes]} {
            return ${sorted_classes}
        }

        if {[::info commands paf_db_cl] != ""} {
            set sorted_classes [::lunique [::lsort -dictionary [paf_db_cl seq\
              -uniq -data -col 0]]]
        } else {
            set sorted_classes ""
        }
        return ${sorted_classes}
    }

    #return a list of all inheritences of a class
    proc all_inheritences {cls} {
        if {[info exists inh_classes(${cls})]} {
            return $inh_classes(${cls})
        }
        if {[::info commands paf_db_in] != ""} {
            set inh_classes(${cls}) [::lunique [::lsort -dictionary\
              [paf_db_in seq -data -col [list 0 "1 +${cls}"]]]]
        } else {
            set inh_classes(${cls}) ""
        }
        return $inh_classes(${cls})
    }

    proc FillValues {} {
        global sn_options
        if {![info exists virt_bit]} {
            set virt_bit [Class&::convert_sym_to_bit {virtual}]
            set purevirt_bit [Class&::convert_sym_to_bit {{pure virtual}}]
        }
    }

    #view the hierarchy of a specified class.
    #probably create a new window
    proc ClassHierarchy {{class ""}} {
        global tkeWinNumber
        set s [MultiWindow&::find_Reusable]
        if {${s} == ""} {
            set new 1
            incr tkeWinNumber
            set s ".multiwindow-${tkeWinNumber}"
            MultiWindow& ${s} -raise ctree
        } else {
            ${s} view ctree
        }
        [${s} ActiveWidget] gotosymbol cl ${class}
    }

    protected variable print_dialog ""
    protected variable topw
    protected variable displayed 0
    protected variable CanDraw
    protected variable RelativClass ""
    protected variable class_drawn
    protected variable can ""
    protected variable list
    protected variable display_filenames 0

    common inh_classes
    common tag_level
    common abstract_classes
    common virt_bit
    common purevirt_bit
    common classes
    common sorted_classes
    common parent_classes

    public variable base_root ""

    # Goto this class.
    itk_option define -classname classname Classname "" {
        if {[winfo exists $itk_component(canvas)]} {
            goto "" $itk_option(-classname)
        }
    }

    # FIXME: These should be set using
    # option add *WhatEverClass.width 640 widgetDefault
    public variable width 640
    public variable height 480

    public variable goto ""
}

proc sn_classtree {{class ""}} {
    global Switch_Is_Enabled
    incr Switch_Is_Enabled -1

    ClassTree&::ClassHierarchy ${class}

    incr Switch_Is_Enabled
}


