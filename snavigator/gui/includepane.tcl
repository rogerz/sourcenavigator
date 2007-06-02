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
## Include Browser that can be integrated in any subwindow.
##
#####################################################
itcl::class Include& {

    inherit sourcenav::MultiChild

    constructor {args} {
        global sn_options

        # Init some values.
        set topw [winfo toplevel $itk_component(hull)]

	# All thulls should be replaced with itk_component(hull)
        set thull $itk_component(hull)
        set can ${thull}.can

        set order $sn_options(def,include-disporder)
        set layoutstyle $sn_options(def,include-layout)

        eval itk_initialize $args

        #draw Menu & Toolbar entries
        #Add menu entries for the class hierarchy
        if {$itk_option(-menu) != "" && [winfo exists $itk_option(-menu)]} {
            $itk_option(-menu) add command -label [get_indep String Includes]\
              -underline [get_indep Pos Includes] -command "${this} references to" -state disabled
            $itk_option(-menu) add command -label [get_indep String IncludedFrom]\
              -underline [get_indep Pos IncludedFrom] -command "${this} references by" -state disabled
            $itk_option(-menu) add command -label [get_indep String RemIncludes]\
              -underline [get_indep Pos RemIncludes] -command "${this} removeIncludes" -state disabled
            $itk_option(-menu) add command -label [get_indep String RemIncludedFrom]\
              -underline [get_indep Pos RemIncludedFrom] -command "${this} removeIncludedFrom" -state disabled
        }

        # View icons on the toolbar.
        if {$itk_option(-toolbar) != "" && [winfo exists $itk_option(-toolbar)]} {
            set exp $itk_option(-toolbar).incbr
            pack [frame ${exp}] -side left
            # DELETE Button
            button ${exp}.remove -takefocus 0 -image waste_image -command\
              " $itk_option(-menu) invoke {[get_indep String RemIncludes]} "\
              -state disabled
            balloon_bind_info ${exp}.remove [get_indep String RemIncludes]
            pack ${exp}.remove -side left

            #refered to Button
            button ${exp}.ref_to -takefocus 0 -image right_image -command\
              " $itk_option(-menu) invoke {[get_indep String Includes]} " -state disabled
            balloon_bind_info ${exp}.ref_to [get_indep String IncludesINFO]
            pack ${exp}.ref_to -side left

            button ${exp}.ref_by -takefocus 0 -image left_image -command\
              " $itk_option(-menu) invoke {[get_indep String IncludedFrom]} "\
              -state disabled
            balloon_bind_info ${exp}.ref_by [get_indep String IncludedFromINFO]
            pack ${exp}.ref_by -side left

            #draw the entry for the levels in the toolbar of the include browser
            set frame ${exp}
            frame ${frame}.space -width 10
            pack ${frame}.space -side left
            label ${frame}.lbl -anchor center -relief groove -text [get_indep String\
              IncLevelTit] -underline [get_indep Pos IncLevelTit]
            entry ${frame}.txt -relief groove -textvar ${this}.incMaxLevels\
              -width 3
            bind ${frame}.txt <Return> "catch \{${this} view_include \[${this}\
              baseroot\]\}"
            pack ${frame}.lbl -side left -fill y
            pack ${frame}.txt -side left -fill y
        }

        canvas ${can} -borderwidth 2 -xscrollcommand "${this}.scrollx set"\
          -yscrollcommand "${this}.scrolly set" -xscrollincrement 20\
          -yscrollincrement 20
        ${can} bind inc <1> "${this} mark_item"
        ${can} bind inc <Double-1> "${this} edit_it \[%W itemcget current -text\]"
        ${can} bind inc <ButtonPress-3> "${this} mark_item; tk_popup\
          ${can}.menu %X %Y"

        #option menu
        set m ${can}.menu
        menu ${can}.menu -tearoff 0 -postcommand\
          "${this} update_post_menu"
        wm overrideredirect ${can}.menu 1
        ${m} add command -label [get_indep String Includes] -command "${this} references to" -state ${ref_to_state}
        ${m} add command -label [get_indep String IncludedFrom] -command "${this} references by" -state ${ref_by_state}
        ${m} add separator
        ${m} add command -label [get_indep String RemIncludes] -command "${this} removeIncludes" -state ${rem_to_state}
        ${m} add command -label [get_indep String RemIncludedFrom] -command "${this} removeIncludedFrom" -state ${rem_by_state}

        scrollbar ${thull}.scrollx -orient horiz -command " ${can} xview "\
          -jump $sn_options(def,canvas-tree-jump)
        scrollbar ${thull}.scrolly -command " ${can} yview "\
          -jump $sn_options(def,canvas-tree-jump)

        grid ${can} -row 0 -column 0 -sticky news
        grid ${thull}.scrollx -row 1 -column 0 -sticky ew
        grid  ${thull}.scrolly -row 0 -column 1 -sticky ns

        grid rowconfigure $itk_component(hull) 0 -weight 1
        grid columnconfigure $itk_component(hull) 0 -weight 1

        #Draw tree of required include
        start ${goto}

        #Display the built tree
        Redraw

        Update_Layout

        #call user defined function
        catch {sn_rc_include $itk_component(hull) ${can}}
    }

    destructor {
        # If this is the last object, reset the common variables.
        if {[itcl::find objects -class [info class]] == ${this}} {
            reset
        }
    }

    #load all include files into a common variable.
    proc all_includes {} {
        if {[::info commands paf_db_iu] != ""} {
            if {![info exists all_includes]} {
                set all_includes [paf_db_iu seq -data -col 0]
                eval lappend all_includes [paf_db_iu seq -data -col 2]
                set all_includes [::lunique [::lsort\
                  -dictionary ${all_includes}]]
            }
        } else {
            set all_includes ""
        }
        return ${all_includes}
    }

    method Redisplay {} {
        view_include [baseroot] 1
        set displayed 0
    }

    method start {selected} {
        reset

        #begin draeing with the selected include
        set includes_to_view ${selected}
        set base_root ${selected}

        FillValues
    }

    proc ResetIncludes {} {
        catch {unset Includes}
        catch {unset Included_From}
        catch {unset all_includes}
    }

    method reset {} {
        catch {unset sons}
        catch {unset parents}
        catch {unset all_roots}
        catch {unset root}
        set includes_to_view ""
    }

    #deletes an entry from string array
    proc DelArrEntry {array n} {
        upvar ${array} arr

        if {! [info exist arr]} {
            set arr ""
        }

        set i [lsearch ${arr} ${n}]
        if {${i} != -1} {
            set arr [lreplace ${arr} ${i} ${i}]
        }
    }

# FIXME: this print stuff needs to go in a common base class !
    method print {} {
        global sn_options tcl_platform
        if {$tcl_platform(platform) == "windows"} {
            ide_print_canvas ${can}
        } else {
            if {${print_dialog} == "" || [itcl::find objects ${print_dialog}]\
              == ""} {
                set print_dialog [PrintDialog $itk_component(hull).printdialog \
                  -leader ${topw} \
                  -modality application \
                  -canvas ${can}\
                  -file [file join $sn_options(profile_dir) include.ps]]
	        $print_dialog transient ${topw}
	        $print_dialog activate
	        itcl::delete object $print_dialog
            } else {
                ${print_dialog} raise
            }
        }
    }

    method Draw_Includes {rot} {
        global sn_options
        if {[info exists inc_drawn(${rot})]} {
            return
        }

        set inc_drawn(${rot}) 1

        set tid [${can} create text 0 0 -text ${rot} -tags [list ${rot} incT\
          inc] -anchor nw -font $sn_options(def,include_font)]

        lappend tids ${tid}

        foreach entry $sons(${rot}) {
            if {${rot} != ${entry}} {
                Draw_Includes ${entry}
            }
        }
    }

    method Draw_Links {} {
        global sn_options
        foreach include [array names parents] {
            if {![info exists inc_drawn(${include})]} {
                continue
            }
            foreach child $parents(${include}) {
                if {[info exists inc_drawn(${child})]} {
                    if {${child} != ${include}} {
                        set tid [${can} create edge -1m -1m -1m -1m -tag\
                          [list child:${child} e:${child} e:${include}\
                          b:${child}:${include} edge] -from [${can} find\
                          withtag ${child}] -to [${can} find withtag\
                          ${include}] -arrow last\
                          -fill $sn_options(def,canv-line-fg)]
                        lappend tids ${tid}
                    }
                }
            }
        }
    }

    method edit_it {name} {
        if {${name} == ""} {
            return
        }
        if {$itk_option(-doubleclickcommand) != ""} {
            eval $itk_option(-doubleclickcommand) [Selection]
        } else {
            sn_display_object "f" ${name}
        }
    }

    method mark_item {{incname ""}} {
        global sn_options
        upvar #0 ${can}-marked_edges m_edges

        if {[string compare ${incname} ""] == 0} {
            #select current class
            set id current
            set incname [${can} itemcget ${id} -text]
        } elseif {[string match {[0-9]*} ${incname}]} {
            set id ${incname}
            set incname [${can} itemcget ${id} -text]
        } else {
            #select specified name
            set id [${can} find withtag ${incname}]
        }

        catch {${can} select from ${id} 0}
        catch {${can} select to ${id} end}

        #If the selection is not owned by us we do not
        #want the marking to disappear.
        selection own ${can} " "
        #selection own -command "$this lose_selection $id" $can

        #unmark old position
        catch {${can} itemconfig "e:${m_edges}"\
          -fill $sn_options(def,canv-line-fg)}
        set m_edges ${incname}

        ${can} itemconfig e:${incname} -fill $sn_options(def,tree-select-line)
        ${can} raise e:${incname}

        if {[winfo exists ${can}.w-${incname}]} {
            set relief sunken
        } else {
            set relief raised
        }

        #set the buttons to current configuration
        control_buttons

        if {$itk_option(-selectcommand) != "" && ${oldSelectedFile} != ${incname}} {
            eval $itk_option(-selectcommand) [Selection]
            set oldSelectedFile ${incname}
        }

        if {$itk_option(-symbols) != ""} {
            $itk_option(-symbols) selecttext ${incname}
        }
        return [list ${incname} ${id}]
    }

    # This function assures that the item will be on the screen.
    method see_item {{cname ""}} {
        if {[string match {[0-9]*} ${cname}]} {
            mark_item ${cname}
            set id ${cname}
        } else {
            set opts [mark_item ${cname}]
            set cname [lindex ${opts} 0]
            set id [lindex ${opts} 1]
        }

        set c ${can}

        set coords [${c} bbox ${id}]

        set x1 [${c} canvasx 0]
        set x2 [${c} canvasx [winfo width ${c}]]
        set y1 [${c} canvasy 0]
        set y2 [${c} canvasy [winfo height ${c}]]
        set enclosed [${c} find enclosed ${x1} ${y1} ${x2} ${y2}]
        # If the item is not fully on the screen, we scroll the canvas.
        if {[lsearch -exact ${enclosed} ${id}] == -1} {
            set scr_reg [lindex [${c} configure -scrollregion] 4]

	    catch {set wid [expr {[lindex ${scr_reg} 2] - [lindex ${scr_reg} 0]}]}
	    catch {set hei [expr {[lindex ${scr_reg} 3] - [lindex ${scr_reg} 1]}]}

            set xoff [expr {double([winfo width ${c}]) / 3}]
            if {[info exists wid]} {
                set pos [expr {double([lindex ${coords} 0] - ${xoff}) / ${wid}}]
                ${c} xview moveto ${pos}
            }

            if {[info exists hei]} {
                set yoff [expr {double([winfo height ${c}]) / 3}]
                set pos [expr {double([lindex ${coords} 1] - ${yoff}) / ${hei}}]
                ${c} yview moveto ${pos}
            }
        }
    }

    method draw_rectangles {can} {
        foreach b [${can} find withtag incT] {
            set geom [${can} bbox ${b}]
            set t [${can} itemcget ${b} -text]
            set id [eval ${can} create rect ${geom} -tags [list boxes ${t}]]
            lappend tids ${id}
        }
    }

    method Redraw {} {

        if {![array exists parents]} {
            return
        }
        ${can} xview moveto 0
        ${can} yview moveto 0

        #delete all drawn widgets in the canvas
        graph ${can} destroy
        ${can} delete all

        catch {unset inc_drawn}

        # unset tids 
        set tids ""

        # Draw the tree
        if {[string compare ${base_root} ""] == 0} {
            foreach entry [array names parents] {
                Draw_Includes ${entry}
            }
        } else {
            foreach entry ${base_root} {
                Draw_Includes ${entry}
            }
        }

        Draw_Links

        set_sorted_order

        set geo [graph_new_layout 0]

        #		draw_rectangles $can
        ${can} raise incT
        #		$can raise edge incT

        set wd [lindex ${geo} 0]
        set he [lindex ${geo} 1]

        #filter include files
        set displayed 0
    }

    method FillValues {} {
        set includes_to_view [lsort -command sn_compare ${includes_to_view}]

        foreach n ${includes_to_view} {
            set parents(${n}) ""
            set sons(${n}) ""
        }

        ReadMaxLevels

        foreach n ${includes_to_view} {
            #append to every include file the included files in this
            FillValuesRec ${n} parents root 1 ${incMaxLevels}
        }
    }

    method FillValuesRec {n parents root level max_levels} {
        upvar ${parents} arr
        upvar ${root} r

        if {! [info exists sons(${n})]} {
            set sons(${n}) ""
        }

        if {[includes ${n}] == ""} {
            set r(${n}) 0
        } else {
            set r(${n}) 0

            # view up to the maximum levels if given
            if {${max_levels} > 0 && ${level} >= ${max_levels}} {
                return
            }

            # Set Level Counter up to one
            set level [expr {${level} + 1}]

            foreach entry [includes ${n}] {
                if {${entry} == ${n}} {
                    continue
                }
                lappend arr(${entry}) ${n}
                lappend sons(${n}) ${entry}
                if {![info exists r(${entry})]} {
                    set r(${entry}) 0

                    FillValuesRec ${entry} arr r ${level} ${max_levels}
                }
            }

            # By leaving the Program decriment Level Counter to
            # origin value as given
            set max_levels [expr {${level} - 1}]
        }
    }

    method ReadMaxLevels {} {
# FIXME: remove this global variable
        upvar #0 ${this}.incMaxLevels incMaxLevels
        if {[catch {set incMaxLevels [expr {int(${incMaxLevels}) + 0}]}] ||\
          ${incMaxLevels} < 0} {
            set incMaxLevels 2
        }
        DispMaxLevels
    }

    method DispMaxLevels {} {
        if {$itk_option(-toolbar) != "" && [winfo exists $itk_option(-toolbar)]} {
            $itk_option(-toolbar).incbr.txt delete 0 end
            $itk_option(-toolbar).incbr.txt insert 0 ${incMaxLevels}
        }
    }

    method view_include {include {reset 0}} {
        #nothing selected
        if {${include} == ""} {
            return
        }

        #dump the current view into the history stack
        if {!${reset}} {
            ${topw} history_stack_add_point ${this}
        }

        #reset the variables
        reset

        set baseroot ${include}

        #set the new selected item
        set includes_to_view [list ${include}]
        set base_root ${includes_to_view}

        #calculate the coordinates
        FillValues

        #Redraw the Widget
        Redraw

        set tid [${can} find withtag ${include}]

        set level ${incMaxLevels}
        if {${incMaxLevels} > 1} {
            set level [expr ${incMaxLevels} - 1]
        }
        references by ${tid} ${level}

        if {$itk_option(-symbols) != ""} {
            $itk_option(-symbols) selecttext ${include}
        }
        set base_root ""

        mark_item ${include}
    }

    method values {} {
        return ${base_root}
    }

    # 0 <==> File includes
    # 2 <==> File included from
    #files that are included from this file
    proc includes {name} {
        if {! [info exists Includes(${name})]} {
            if {[::info commands paf_db_iu] != ""} {
                set Includes(${name}) [lunique [lsort\
                  -dictionary [paf_db_iu seq -col 0 -end ${name}]]]
            } else {
                set Includes(${name}) ""
            }
        }
        return $Includes(${name})
    }

    # 0 <==> File includes
    # 2 <==> File included from
    #files that include this file
    proc included_from {name} {
        global sn_sep
        if {! [info exists Included_From(${name})]} {
            if {[::info commands paf_db_iu] != ""} {
                set Included_From(${name}) [lunique [lsort\
                  -dictionary [paf_db_iu seq -col 2 "${name}${sn_sep}"]]]
            } else {
                set Included_From(${name}) ""
            }
        }
        return $Included_From(${name})
    }

    method control_buttons {{id ""}} {
        if {${id} == ""} {
            set id [${can} select item]
        }
        if {${id} == ""} {
            return
        }

        set includes_state disabled
        set included_from_state disabled
        set remove_state disabled
        set removeFrom_state disabled

        if {${id} != ""} {
            set name [${can} itemcget ${id} -text]

            #there are Items availiable, include this file
            #force the Includes list, to see if there are includes
            #availiable, that are not drawned on the screen
            foreach entry [includes ${name}] {
                if {[catch {set ret [lsearch -exact $sons(${name})\
                  ${entry}]}]} {
                    return
                }
                if {${ret} == -1} {
                    set includes_state normal
                    break
                }
            }
            if {![info exists sons(${name})]} {
                set sons(${name}) ""
            }
            if {$sons(${name}) != ""} {
                set remove_state normal
            }

            if {![info exists parents(${name})]} {
                set parents(${name}) ""
            }

            #force the Included_From list, to see if there are includes
            #availiable, that are not drawned on the screen
            foreach entry [included_from ${name}] {
                if {[lsearch -exact $parents(${name}) ${entry}] == -1} {
                    set included_from_state normal
                    break
                }
            }

            # Parents availiable.
            if {$parents(${name}) != ""} {
                set removeFrom_state normal
            }
        }

        if {$itk_option(-menu) != "" && [winfo exists $itk_option(-menu)]} {
            $itk_option(-menu) entryconfig [get_indep String Includes]\
              -state ${includes_state}
            $itk_option(-menu) entryconfig [get_indep String IncludedFrom]\
              -state ${included_from_state}
            $itk_option(-menu) entryconfig [get_indep String RemIncludes]\
              -state ${remove_state}
            $itk_option(-menu) entryconfig [get_indep String RemIncludedFrom]\
              -state ${removeFrom_state}
        }

        if {$itk_option(-toolbar) != "" && [winfo exists $itk_option(-toolbar)]} {
            $itk_option(-toolbar).incbr.ref_to configure -state ${includes_state}
            $itk_option(-toolbar).incbr.ref_by configure -state ${included_from_state}
            $itk_option(-toolbar).incbr.remove configure -state ${remove_state}
        }

        #option menu
        ${can}.menu entryconfig [get_indep String Includes]\
          -state ${includes_state}
        ${can}.menu entryconfig [get_indep String IncludedFrom]\
          -state ${included_from_state}
        ${can}.menu entryconfig [get_indep String RemIncludes]\
          -state ${remove_state}
        ${can}.menu entryconfig [get_indep String RemIncludedFrom]\
          -state ${removeFrom_state}

        return [list ${includes_state} ${included_from_state}]
    }

    method set_sorted_order {} {
        if {${tids} != ""} {
            eval graph ${can} add ${tids}
        }
    }

    method graph_new_layout {{redraw 1}} {
        global sn_options

        if {${tids} == ""} {
            return
        }

        if {${redraw}} {
            graph ${can} clear
            set_sorted_order
        }

        if {${order} == ""} {
            set order 0
        }
        if {${order} == 0} {
            set cmd "-nodespacev $sn_options(def,include-horizspace)\
              -nodespaceh $sn_options(def,include-vertspace)"
        } else {
            set cmd "-nodespacev $sn_options(def,include-vertspace) \
              -nodespaceh $sn_options(def,include-horizspace)"
        }
        eval graph ${can} configure -gridlock 1 -order ${order} ${cmd}

        if {[info exists tids] && [string trim ${tids}] != ""} {
            graph ${can} layout ${layoutstyle}

            set reg [${can} bbox all]

            set wd [lindex ${reg} 2]
            set he [lindex ${reg} 3]
            ${can} configure -scrollregion [list 0 0 ${wd} ${he}]
            set lst [list ${wd} ${he}]
        } else {
            set lst [list 10 10]
        }

        return ${lst}
    }

    method update_post_menu {} {
        set m ${can}.menu
        set view ${this}.menu.view

        set id [${can} select item]
        if {${id} == ""} {
            return
        }
        set sym [string trim [${can} itemcget ${id} -text]]
    }

    method references {type {sym_id ""} {level -1}} {
        if {${level} == -1} {
            ReadMaxLevels
            set maxlevels ${incMaxLevels}
        } else {
            set maxlevels ${level}
        }

        if {${type} == "both"} {
            set type [list to by]
        }

        set scroll_to_top 0
        switch -glob ${sym_id} {
            {[0-9]*} {
                    set sym [${can} itemcget ${sym_id} -text]
                    set sym_tp [lindex ${sym} 0]
                    set sym [lindex ${sym} end]
                }
            {} {
                    set sym_id [${can} select item]
                    if {${sym_id} == ""} {
                        return
                    }
                    set sym [${can} itemcget ${sym_id} -text]
                    set sym_tp [lindex ${sym} 0]
                    set sym [lindex ${sym} end]
                }
            default {
                    return
                }
        }

        if {[lsearch ${type} "by"] != -1} {
            show_included_from ${sym_id} 0 ${maxlevels}

            if {$itk_option(-menu) != "" && [winfo exists $itk_option(-menu)]} {
                $itk_option(-menu) entryconfig [get_indep String IncludedFrom]\
                  -state disabled
            }
            if {$itk_option(-toolbar) != "" && [winfo exists $itk_option(-toolbar)]} {
                $itk_option(-toolbar).incbr.ref_by configure -state disabled
            }
        }

        if {[lsearch ${type} "to"] != -1} {
            show_includes ${sym_id} 0 ${maxlevels}

            if {$itk_option(-menu) != "" && [winfo exists $itk_option(-menu)]} {
                $itk_option(-menu) entryconfig [get_indep String Includes] -state disabled
            }
            if {$itk_option(-toolbar) != "" && [winfo exists $itk_option(-toolbar)]} {
                $itk_option(-toolbar).incbr.ref_to configure -state disabled
            }
        }

        # This is a workaround for 'tree'.
        if {${layoutstyle} == "tree"} {
            graph_new_layout 1
        } else {
            graph ${can} layout ${layoutstyle}
            set reg [${can} bbox all]
            set wd [lindex ${reg} 2]
            set he [lindex ${reg} 3]
            ${can} configure -scrollregion [list 0 0 ${wd} ${he}]
        }

        if {${scroll_to_top}} {
            ${can} xview moveto 0.0
            ${can} yview moveto 0.0
        }

        control_buttons

        see_item ${sym_id}
    }

    method show_included_from {sym_id level maxlevels} {
        global sn_options
        set sym [${can} itemcget ${sym_id} -text]

        if {${maxlevels} > 0 && ${level} >= ${maxlevels}} {
            return
        }

        #no Included Files available
        if {[included_from ${sym}] == ""} {
            return
        }

        if {! [info exists sons(${sym})]} {
            set sons(${sym}) ""
        }
        if {! [info exists parents(${sym})]} {
            set parents(${sym}) ""
        }

        # Add the included files from this file
        foreach s [included_from ${sym}] {

            lappend sons(${s}) ${sym}
            lappend parents(${sym}) ${s}

            #text not drawned
            if {! [info exists inc_drawn(${s})] || $inc_drawn(${s}) != 1} {

                set tid [${can} create text 0 0 -text ${s} -tags [list ${s}\
                  incT inc] -anchor nw -font $sn_options(def,include_font)]

                set inc_drawn(${s}) 1
                set tids [linsert ${tids} 0 ${tid}]
            } else {
                set eid [${can} find withtag "b:${s}:${sym}"]
                if {${eid} != ""} {
                    continue
                }
                set tid ""
            }

            if {${s} == ${sym}} {
                continue
            } else {
                set eid [${can} create edge -1m -1m -1m -1m -tag\
                  [list child:${s} e:${s} e:${sym} b:${s}:${sym} edge]\
                  -to [${can} find withtag ${sym}] -from [${can} find withtag\
                  ${s}] -arrow last -fill $sn_options(def,canv-line-fg)]
                lappend tids ${eid}
            }

            if {${tid} != "" && ${eid} != ""} {
                eval graph ${can} add ${tid} ${eid}
                show_included_from ${tid} [expr ${level} + 1] ${maxlevels}
            }\
            elseif {${eid} != ""} {
                eval graph ${can} add ${eid}
            }
        }

        #mark_item $sym
    }

    method show_includes {sym_id level maxlevels} {
        global sn_options
        set sym [${can} itemcget ${sym_id} -text]

        if {${maxlevels} > 0 && ${level} >= ${maxlevels}} {
            return
        }

        #no Included Files available
        if {[includes ${sym}] == ""} {
            return
        }

        if {! [info exists sons(${sym})]} {
            set sons(${sym}) ""
        }
        if {! [info exists parents(${sym})]} {
            set parents(${sym}) ""
        }

        # Add the Includes that include this file
        foreach s [includes ${sym}] {

            lappend sons(${sym}) ${s}
            lappend parents(${s}) ${sym}

            #text not created, create now
            if {! [info exists inc_drawn(${s})] || $inc_drawn(${s}) != 1} {

                set tid [${can} create text 0 0 -text ${s} -tags [list ${s}\
                  incT inc] -anchor nw -font $sn_options(def,include_font)]

                set inc_drawn(${s}) 1
                lappend tids ${tid}

                #text already created
            } else {
                set eid [${can} find withtag "b:${sym}:${s}"]

                #edge already exists
                if {${eid} != ""} {
                    continue
                }
                set tid ""
            }

            if {${s} == ${sym}} {
                continue
            } else {
                set eid [${can} create edge -1m -1m -1m -1m -tag\
                  [list child:${sym} e:${s} e:${sym} b:${sym}:${s} edge]\
                  -from [${can} find withtag ${sym}] -to [${can} find withtag\
                  ${s}] -arrow last -fill $sn_options(def,canv-line-fg)]
                lappend tids ${eid}
            }

            if {${tid} != "" && ${eid} != ""} {
                graph ${can} add ${tid} ${eid}

                #recursive
                show_includes ${tid} [expr ${level} + 1] ${maxlevels}
            }\
            elseif {${eid} != ""} {
                graph ${can} add ${eid}
            }
        }

        #mark_item $sym
    }

    method all_children {can basename name result} {
        upvar ${result} res

        set ids ""

        if {! [info exist sons(${name})] || $sons(${name}) == "" ||\
          [info exist visited(${name})] && $visited(${name}) == 1} {
            return
        }

        set visited(${name}) 1

        foreach n $sons(${name}) {

            #do not delete the original name
            if {${n} == ${basename}} {
                continue
            }

            set ids [${can} find withtag ${n}]

            #delete this include from other parents
            catch {unset inc_drawn(${n})}

            #mark edges from and to the include
            eval lappend ids [${can} find withtag "e:${n}"]

            if {${ids} != ""} {
                eval lappend res ${ids}
            }

            all_children ${can} ${basename} ${n} res

            #delete item from his parents
            if {[info exist parents(${n})]} {
                foreach p $parents(${n}) {
                    DelArrEntry sons(${p}) ${n}
                }
            }
            catch {unset parents(${n})}

            #delete this include from other sons
            #			if { [info exist sons($n)] } {
            #				foreach s $sons($n) {
            #					DelArrEntry parents($s) $n
            #				}
            #			}
            catch {unset sons(${n})}
        }

        #remove the id's from tids
        foreach id ${ids} {
            DelArrEntry tids ${id}
        }
    }

    # This methods deletes the Includes Subtree of an Include
    method removeIncludes {{id ""}} {
        if {${id} == ""} {
            set id [${can} select item]
            if {${id} == ""} {
                return
            }
        }

        set name [${can} itemcget ${id} -text]
        set children_ids ""
        catch {unset visited}
        all_children ${can} ${name} ${name} children_ids
        catch {unset visited}

        if {${children_ids} == ""} {
            return
        }

        eval graph ${can} remove ${children_ids}
        eval ${can} delete ${children_ids}

        graph_new_layout 1

        control_buttons

        see_item ${id}
    }

    method all_parents {can basename name result} {
        upvar ${result} res

        set ids ""

        if {! [info exist parents(${name})] || $parents(${name}) == "" ||\
          [info exist visited(${name})] && $visited(${name}) == 1} {
            return
        }

        set visited(${name}) 1

        foreach n $parents(${name}) {

            #do not delete the original name
            if {${n} == ${basename}} {
                continue
            }

            set ids [${can} find withtag ${n}]

            catch {unset inc_drawn(${n})}

            #mark edges from and to the include
            eval lappend ids [${can} find withtag "e:${n}"]

            if {${ids} != ""} {
                eval lappend res ${ids}
            }
            all_parents ${can} ${basename} ${n} res

            #delete this include from other sons
            if {[info exist sons(${n})]} {
                foreach s $sons(${n}) {
                    DelArrEntry parents(${s}) ${n}
                }
            }
            catch {unset sons(${n})}
            catch {unset parents(${n})}
        }

        #remove the id's from tids
        foreach id ${ids} {
            DelArrEntry tids ${id}
        }
    }
    # This methods deletes the Included_From Subtree of an Include
    method removeIncludedFrom {{id ""}} {
        if {${id} == ""} {
            set id [${can} select item]
            if {${id} == ""} {
                control_buttons
                return
            }
        }
        set name [${can} itemcget ${id} -text]
        set children_ids ""

        catch {unset visited}
        all_parents ${can} ${name} ${name} children_ids
        catch {unset visited}

        if {${children_ids} == ""} {
            return
        }

        eval graph ${can} remove ${children_ids}
        eval ${can} delete ${children_ids}

        graph_new_layout 1

        control_buttons

        see_item ${id}
    }

    method baseroot {} {
        set sel [${can} itemcget [${can} select item] -text]
        if {${sel} == ""} {
            return ${baseroot}
        } else {
            return ${sel}
        }
    }
    method Title {{full 1}} {
        global sn_options
        set t [get_indep String IncludeTree]
        if {${baseroot} != ""} {
            set t "${t}: ${baseroot}"
        }
        if {${full}} {
            return [sn_title ${t}]
        } else {
            return ${t}
        }
    }
    method Icon {} {
        return [sn_view_icon [get_indep String IncludeTree] ${baseroot}]
    }
    method SetTitle {} {
        global sn_options

        set t [sn_title [get_indep String IncludeTree]]
        if {${baseroot} != ""} {
            set t "${t}: ${baseroot}"
            set icon "[get_indep String IncludeTree] ${baseroot}"
        } else {
            set icon [get_indep String IncludeTree]
        }

        ${topw} configure -title ${t} -iconname ${icon}
    }

    #Filter for symbols
    method filter {{all 0}} {
        if {$itk_option(-symbols) == ""} {
            return
        }
# FIMXE: we are accessing a global variable from somewhere, needs fixing.
        upvar #0 $itk_option(-symbols_filter)-related related
        if {${related}} {
            $itk_option(-symbols) configure -contents [::lsort -command sn_compare [array names\
              inc_drawn]]
        } else {
            $itk_option(-symbols) configure -contents [all_includes]
        }
    }

    #this function is called, when the symbols combobox
    #is called
    method postcommand {m} {
        if {! ${displayed}} {
            filter
            set displayed 1
        }
    }

    method activate {} {
        #enable menu
        if {$itk_option(-menu) != "" && [winfo exists $itk_option(-menu)]} {
            set mn [string range $itk_option(-menu) 0 [expr [string last "." $itk_option(-menu)] - 1]]
            ${mn} entryconfig [get_indep String MultiInclude] -state normal
        }
        if {$itk_option(-toolbar) != "" && [winfo exists $itk_option(-toolbar)]} {
            pack $itk_option(-toolbar).incbr -side left
        }

        if {$itk_option(-symbols) != ""} {
            $itk_option(-symbols) configure -entrytext ${base_root}
        }

        #always fill combobox by activating
        set displayed 0
    }
    method deactivate {} {
        if {$itk_option(-menu) != "" && [winfo exists $itk_option(-menu)]} {
            set mn [string range $itk_option(-menu) 0 [expr [string last "." $itk_option(-menu)] - 1]]
            ${mn} entryconfig [get_indep String MultiInclude] -state disabled
        }
        if {$itk_option(-toolbar) != "" && [winfo exists $itk_option(-toolbar)]} {
            pack forget $itk_option(-toolbar).incbr
        }
    }

    method correct_include {inc} {
        global sn_options sn_sep sn_root
        if {${inc} == ""} {
            return
        }
        #is include availiable ?
        if {[catch {set avail [paf_db_iu seq -first -col 0\
          "${inc}${sn_sep}"]}] || ${avail} == ""} {
            #perhaps it's a .c file
            if {[catch {set avail [paf_db_iu seq -first -col 0 -end ${inc}]}]\
              || ${avail} == ""} {
                #add prefix and try again
                set inc "${sn_root}${inc}"
                if {[catch {set avail [paf_db_iu seq -first -col 0\
                  -strstr "${inc}${sn_sep}"]}] || ${avail} == ""} {
                    return ""
                }
                set inc [lindex ${avail} 0]
            }
        }
        return ${inc}
    }

    #view include relations to a selected string or to
    #a given filename
    method gotosymbol {{scope ""} {sym ""} {cls ""} {file ""} {from ""} {type\
      ""} {prm ""} {to ""} {always 1}} {
        set inc ""
        #selection has more priority !!
        if {${sym} != ""} {
            set inc [correct_include ${sym}]
        }
        if {${inc} == "" && ${file} != ""} {
            set inc [correct_include ${file}]
        }
        if {${inc} == "" || ${inc} == ${baseroot}} {
            return 1
        }
        #disp tree
        view_include ${inc}
        return 1
    }

    method goto {combo inc} {
        #disp tree
        view_include ${inc}
    }

    method Selection {} {
        #set sel [$can itemcget [$can select item] -text]
        set id [${can} select item]
        if {${id} == ""} {
            set id [${can} find withtag current]
        }
        set sel [${can} itemcget ${id} -text]
        if {${sel} != ""} {
            return [list "" "" "" ${sel} "" "" "" ""]
        } else {
            return ""
        }
    }

    method clearselection {} {
        ${can} select clear
    }

    proc ViewInclude {selected} {
        global tkeWinNumber

        set win [MultiWindow&::find_Reusable]
        if {${win} == ""} {
            incr tkeWinNumber
            set win ".multiwindow-${tkeWinNumber}"

            # Create the Include Hierarchy
            MultiWindow& ${win} -raise incbr
        } else {
            ${win} view incbr
        }
        [${win} ActiveWidget] goto "" ${selected}
        #sn_add_history inc  [list $selected]  [sn_make_history_title inc\
          "" $selected]  sn_include
    }

    method Refresh_Display {} {
        global sn_options
        ResetIncludes
        Redisplay
    }

    method Update_Layout {} {
        global sn_options

        ${can} configure -selectforeground $sn_options(def,select-fg)\
          -selectbackground $sn_options(def,select-bg)

        set layoutstyle $sn_options(def,include-layout)
        set order $sn_options(def,include-disporder)

        graph_new_layout 1
    }

    method Focus {} {
        focus ${can}
    }

    #make a title for the Dump position, this is usefull
    #for the views stack (prev/next)
    method DumpTitle {{dump ""}} {
        if {${dump} == ""} {
            set dump [Dump]
        }
        return "Include [lindex ${dump} 3]"
    }

    method AddHistoryFromDump {dump title} {
        set file [lindex ${dump} 1]
        sn_add_history inc [list ${file}] [sn_make_history_title inc\
          "" ${file}] sn_include
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

    method whoami {} {
        return incbr
    }

    private variable layoutstyle
    private variable order

    protected variable topw
    protected variable can ""
    protected variable baseroot ""
    protected variable parents
    protected variable sons
    protected variable includes_to_view
    protected variable all_roots
    protected variable tids ""
    protected variable base_root ""
    protected variable root
    #so we can view the include list up to limited tree depth
    protected variable inc_drawn
    #with help this variable can cicyles be detected by searching the tree
    #to hide a node
    protected variable visited
    protected variable oldSelectedFile ""
    protected variable displayed 0

    protected variable ref_to_state "normal"
    protected variable ref_by_state "normal"
    protected variable rem_to_state "normal"
    protected variable rem_by_state "normal"

    common incMaxLevels 2
    common Includes
    common Included_From
    common selected_include ""

    #contains all include files
    common all_includes
    common tag_level ""

    public variable goto ""
    public variable width 640
    public variable height 480

    protected variable print_dialog ""
}

proc sn_include {{file ""}} {
    global Switch_Is_Enabled
    incr Switch_Is_Enabled -1

    Include&::ViewInclude ${file}

    incr Switch_Is_Enabled
}


