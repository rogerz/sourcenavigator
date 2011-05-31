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
# multixref.tcl - Cross-referencing view that can be opened in any subwindow.
# Copyright (C) 1998 Cygnus Solutions.

itcl::class XRef& {

    inherit sourcenav::MultiChild

    constructor {args} {
        global sn_options

        set class [${this} info class]
        set bg [$itk_component(hull) cget -background]

        set can $itk_component(hull).can


	eval itk_initialize $args
# FIXME: global variable hacks need to be removed!
        #order & layout
        global ${this}-order
        global ${this}-layoutstyle
        set ${this}-order $sn_options(def,xref-disp-order)
        set ${this}-layoutstyle $sn_options(def,xref-layout)

        #init cross reference scopes, as example 'mi' not 'md', 'fu' not 'fd'
        set cross_scopes "cl ec fd fu gv iv ma mi t ud"
        foreach sc {con cov su} {
            if {[::info commands paf_db_${sc}] != ""} {
                lappend cross_scopes con cov su
                break

            }
        }
        if {${cross_shown_scopes} == ""} {
            set cross_shown_scopes [paf_db_proj get -key cross_shown_scopes]
            set cross_ref_access [paf_db_proj get -key cross_ref_access]
        }
        if {${cross_shown_scopes} == ""} {
            set cross_shown_scopes "cl con cov ec e fu fd gv iv ma mi md lv su\
              t ud p r u w"
            set cross_ref_access "p r u w"
        }

        set topw [winfo toplevel $itk_component(hull)]

        #when the cross reference is called without toolbar, MaxLevels
        #will be not defined.
        uplevel #0 "set ${this}-MaxLevels 1"

        #Add menu entries for the class hierarchy
        if {$itk_option(-menu) != "" && [winfo exists $itk_option(-menu)]} {
            $itk_option(-menu) add command -label [get_indep String TakeRoot]\
              -state disabled -underline [get_indep Pos TakeRoot] -command "${this} make_selection_to_root"
            $itk_option(-menu) add command -label [get_indep String PafCrossToDetail]\
              -state disabled -underline [get_indep Pos PafCrossToDetail]\
              -command "${this} xbrowse to"
            $itk_option(-menu) add command -label [get_indep String PafCrossByDetail]\
              -state disabled -underline [get_indep Pos PafCrossByDetail]\
              -command "${this} xbrowse by"
            $itk_option(-menu) add command -label [get_indep String CrfRefTo]\
              -underline [get_indep Pos CrfRefTo] -state disabled -command\
              " ${this} references to "
            $itk_option(-menu) add command -label [get_indep String CrfRefBy]\
              -underline [get_indep Pos CrfRefBy] -command " ${this}\
              references by " -state disabled
            $itk_option(-menu) add command -label [get_indep String CrfRemSubN]\
              -underline [get_indep Pos CrfRemSubN] -command "${this} remove both" -state disabled
            $itk_option(-menu) add command -label [get_indep String CrfRemRefTo]\
              -underline [get_indep Pos CrfRemRefTo] -command "${this} remove to"
            $itk_option(-menu) add command -label [get_indep String CrfRemRefBy]\
              -underline [get_indep Pos CrfRemRefBy] -command "${this} remove by"
            $itk_option(-menu) add separator
            $itk_option(-menu) add command -label [get_indep String XRefFilter]\
              -underline [get_indep Pos XRefFilter] -command "[info class]::xref_filter"
            $itk_option(-menu) add separator
            $itk_option(-menu) add checkbutton -label [get_indep String CrossAcceptParam]\
              -underline [get_indep Pos CrossAcceptParam]\
              -variable ${this}-accept_param -command " ${this} toggle_param m "
            $itk_option(-menu) add checkbutton -label [get_indep String\
              CrossAcceptStatic] -underline [get_indep Pos CrossAcceptStatic]\
              -variable ${this}-accept_static -command " ${this} toggle_static\
              m "
            $itk_option(-menu) add checkbutton -label [get_indep String CrossDispParam]\
              -underline [get_indep Pos CrossDispParam]\
              -variable ${this}-disp_param -command " ${this}\
              toggle_disp_param m "
            $itk_option(-menu) add checkbutton -label [get_indep String CrossBoxes]\
              -underline [get_indep Pos CrossBoxes]\
              -variable ${this}-draw_rectangles -command " ${this}\
              toggle_boxes m "
        }

        if {$itk_option(-toolbar) != ""} {
            set exp $itk_option(-toolbar).xref
            frame ${exp}
	    
            button ${exp}.rem_ref_to -takefocus 0 -image del_right_image -command\
              " ${this} remove to "
            balloon_bind_info ${exp}.rem_ref_to [get_indep String CrfRemRefToINFO]
            pack ${exp}.rem_ref_to -side left

            button ${exp}.ref_to -takefocus 0 -image right_image -command\
              " ${this} references to " -state disabled
            balloon_bind_info ${exp}.ref_to [get_indep String CrfRefToINFO]
            pack ${exp}.ref_to -side left

            button ${exp}.remove -image waste_image -takefocus 0 -command "${this} remove both" -state disabled
            balloon_bind_info ${exp}.remove [get_indep String CrfRemSubN]
            pack ${exp}.remove -side left

            button ${exp}.ref_by -takefocus 0 -image left_image -command\
              " ${this} references by " -state disabled
            balloon_bind_info ${exp}.ref_by [get_indep String CrfRefByINFO]
            pack ${exp}.ref_by -side left

            button ${exp}.rem_ref_by -takefocus 0 -image del_left_image -command\
              " ${this} remove by "
            balloon_bind_info ${exp}.rem_ref_by [get_indep String CrfRemRefByINFO]
            pack ${exp}.rem_ref_by -side left

            if {0} {
                #DO WE NEED THIS BUTTONS IN THE TOOLBAR????????

                button ${exp}.xbrowse_to -takefocus 0\
                  -image cross_browse_to_image -command "  " -state disabled
                bind ${exp}.xbrowse_to <Any-ButtonPress> "if {%b == 1}\
                  {${this} xbrowse to}"
                balloon_bind_info ${exp}.xbrowse_to [get_indep String\
                  PafCrossToDetail]
                pack ${exp}.xbrowse_to -side left

                button ${exp}.xbrowse_by -takefocus 0\
                  -image cross_browse_by_image -command "  " -state disabled
                bind ${exp}.xbrowse_by <Any-ButtonPress> "if {%b == 1}\
                  {${this} xbrowse by}"
                balloon_bind_info ${exp}.xbrowse_by [get_indep String\
                  PafCrossByDetail]
                pack ${exp}.xbrowse_by -side left
            }

            #WE NEED A FILTER BUTTON IN THE TOOLBAR
	    button ${exp}.filter -text [get_indep String Mixer]\
	    	-takefocus 0\
	    	-underline [get_indep Pos Mixer]\
	    	-pady 0\
            	-command "[info class]::xref_filter "

	    balloon_bind_info ${exp}.filter [get_indep String XRefFilterINFO]
            pack ${exp}.filter -side left -fill y -expand n

            #we want to support levels by viewing references
            frame ${exp}.space -width 10
            pack ${exp}.space -side left
            label ${exp}.lbl -anchor center -relief groove -text [get_indep String\
              IncLevelTit] -underline [get_indep Pos IncLevelTit]
            entry ${exp}.txt -relief groove -textvar ${this}-MaxLevels -width 3
            bind ${exp}.txt <Return> "${this} explore_calls"
            pack ${exp}.lbl -side left -fill y
            pack ${exp}.txt -side left -fill y

            #cancel button to cancel fetching, especialy when
            #more than one level is given
            button ${exp}.cancel -takefocus 0 -text [get_indep String Cancel]\
              -command " ${this} cancel_fetching " -pady 0 -state disabled
            balloon_bind_info ${exp}.cancel [get_indep String XRefCancelINFO]
            pack ${exp}.cancel -side left -fill y -expand n

            if {0} {
                #DO WE NEED THIS BUTTONS IN THE TOOLBAR????????

                #button to enable/disable parameter differences
                button ${exp}.param -takefocus 0 -text [get_indep String\
                  CrossAcceptParam] -relief sunken -image cross_param_image\
                  -command "  "
                bind ${exp}.param <Any-ButtonPress> "if {%b == 1} {${this} toggle_param ${exp}.param}"
                balloon_bind_info ${exp}.param [get_indep String\
                  CrossAcceptParamINFO]
                pack ${exp}.param -side left

                #button to enable/disable static references
                button ${exp}.static -takefocus 0 -text [get_indep String\
                  CrossAcceptStatic] -relief raised -image cross_static_image\
                  -command "  "
                bind ${exp}.static <Any-ButtonPress> "if {%b == 1} {${this} toggle_static ${exp}.static}"
                balloon_bind_info ${exp}.static [get_indep String\
                  CrossAcceptStaticINFO]
                pack ${exp}.static -side left

                #button to enable/disable displaying parameters on the canvas
                button ${exp}.disp_param -takefocus 0 -text [get_indep String\
                  CrossDispParam] -relief raised -image cross_disp_param_image\
                  -command "  "
                bind ${exp}.disp_param <Any-ButtonPress> "if {%b == 1}\
		 {${this} toggle_disp_param ${exp}.disp_param}"
                balloon_bind_info ${exp}.disp_param [get_indep String\
                  CrossDispParamINFO]
                pack ${exp}.disp_param -side left
                pack ${exp} -side left -fill x

                #button to enable boxes or to disable it
                button ${exp}.boxes -takefocus 0 -text [get_indep String\
                  CrossBoxes] -relief raised -image cross_boxes_image\
                  -command "  "
                bind ${exp}.boxes <Any-ButtonPress> "if {%b == 1} {${this} toggle_boxes ${exp}.boxes}"
                balloon_bind_info ${exp}.boxes [get_indep String CrossBoxesINFO]
                pack ${exp}.boxes -side left
            }
        }

        canvas ${can} -xscrollcommand "$itk_component(hull).scrollx set"\
          -yscrollcommand "$itk_component(hull).scrolly set" -xscrollincrement 20\
          -yscrollincrement 20 -closeenough 0

        ${can} bind sym <1> "${this} mark_item"
        ${can} bind sym <Double-1> "${this} handle_doubleclick ${can}"
        ${can} bind sym <ButtonPress-3> "${this} mark_item current 0; tk_popup\
          ${can}.menu %X %Y"

        scrollbar $itk_component(hull).scrollx -orient horiz -command " ${can} xview "\
          -jump $sn_options(def,canvas-tree-jump)
        scrollbar $itk_component(hull).scrolly -command " ${can} yview "\
          -jump $sn_options(def,canvas-tree-jump)

        #Option menu for XRef items
        menu ${can}.menu -tearoff 0 -background ${bg} -postcommand\
          "${this} update_post_menu"
        wm overrideredirect ${can}.menu 1
        #References To/By
        ${can}.menu add command -label [get_indep String CrfRefTo]\
          -state disabled -underline [get_indep Pos CrfRefTo] -command\
          " ${this} references to "
        ${can}.menu add command -label [get_indep String CrfRefBy]\
          -state disabled -underline [get_indep Pos CrfRefBy] -command\
          " ${this} references by "
        #Details To/By
        ${can}.menu add command -label [get_indep String PafCrossToDetail]\
          -state disabled -underline [get_indep Pos PafCrossToDetail] -command "${this} xbrowse to"
        ${can}.menu add command -label [get_indep String PafCrossByDetail]\
          -state disabled -underline [get_indep Pos PafCrossByDetail] -command "${this} xbrowse by"
        ${can}.menu add command -label [get_indep String CrfRemSubN]\
          -state disabled -underline [get_indep Pos CrfRemSubN] -command "${this} remove both"
	${can}.menu add command -label [get_indep String CrfRemRefTo]\
	  -underline [get_indep Pos CrfRemRefTo] -command "${this} remove to"
	${can}.menu add command -label [get_indep String CrfRemRefBy]\
	  -underline [get_indep Pos CrfRemRefBy] -command "${this} remove by"
        ${can}.menu add separator
        ${can}.menu add command -label [get_indep String TakeRoot]\
          -state disabled -underline [get_indep Pos TakeRoot] -command "${this} make_selection_to_root"
        ${can}.menu add separator

        ${can}.menu add checkbutton -label [get_indep String CrossAcceptParam]\
          -underline [get_indep Pos CrossAcceptParam]\
          -variable ${this}-accept_param -command " ${this} toggle_param m "
        ${can}.menu add checkbutton -label [get_indep String\
          CrossAcceptStatic] -underline [get_indep Pos CrossAcceptStatic]\
          -variable ${this}-accept_static -command " ${this} toggle_static m "

        grid ${can} -row 0 -column 0 -sticky news
        grid $itk_component(hull).scrollx -row 1 -column 0 -sticky ew
        grid $itk_component(hull).scrolly -row 0 -column 1 -sticky ns

        grid rowconfigure $itk_component(hull) 0 -weight 1
        grid columnconfigure $itk_component(hull) 0 -weight 1

        #Set colors and fonts
        Update_Layout

        #something is selected, goto the selection
        if {${goto} != ""} {
            goto "" ${goto}
        }

        #call the user defined procedure
        catch {sn_rc_xref $itk_component(hull) ${can}}
    }
    destructor {
        foreach v [::info globals "${this}-*"] {
            catch {uplevel #0 unset ${v}}
        }
    }

    method build_text {id} {
        global sn_sep
        set class [lindex $xinfos(${id}) ${class1_pos}]
        if {${class} == "#"} {
            set class ""
        }
        set key [lindex $xinfos(${id}) ${item1_pos}]
        set what [lindex $xinfos(${id}) ${what1_pos}]
        set text [string trim "${key}(${what}) ${class}"]

        if {${disp_param} && [lsearch -exact ${scopes_with_parameters}\
          ${what}] != -1} {
            set text " [string trim "[lindex $xinfos(${id}) ${type1_pos}]\
              ${text}([lindex $xinfos(${id}) ${param1_pos}])"]"
        }

        return ${text}
    }

    method toggle_boxes {{w ""}} {
        global sn_options
        upvar #0 ${this}-draw_rectangles draw
        if {${w} != "m"} {
            if {${draw}} {
                if {${w} != ""} {
                    ${w} config -relief raised
                }
                set draw 0
            } else {
                if {${w} != ""} {
                    ${w} config -relief sunken
                }
                set draw 1
            }
        }
        set draw_rectangles ${draw}
        set sn_options(both,xref-draw-rect) ${draw}

        ${can} delete %boxes%

        draw_rectangles ${can} ${draw_rectangles}
    }

    method toggle_disp_param {{w ""}} {
        global sn_options
        upvar #0 ${this}-disp_param dsp_prm

        if {[sn_processes_running]} {
            bell
            return
        }
        if {${w} != "m"} {
            if {${disp_param}} {
                if {${w} != ""} {
                    ${w} config -relief raised
                }
                set dsp_prm 0
            } else {
                if {${w} != ""} {
                    ${w} config -relief sunken
                }
                set dsp_prm 1
            }
        }
        set disp_param ${dsp_prm}
        set sn_options(both,xref-disp-param) ${dsp_prm}

        foreach id [${can} find withtag sym] {
            ${can} itemconfig ${id} -text [build_text ${id}]
        }
        graph_new_layout 1

        ${can} delete %boxes%
        draw_rectangles ${can} ${draw_rectangles}

        #reselect the selected item
        set id [${can} select item]
        if {${id} != ""} {
            ${can} select from ${id} 0
            ${can} select to ${id} end
        }
    }

    method toggle_param {{w ""}} {
        global sn_options
        upvar #0 ${this}-accept_param param

        if {[sn_processes_running]} {
            bell
            return
        }

        if {${w} != "m"} {
            if {${param}} {
                if {${w} != ""} {
                    ${w} config -relief raised
                }
                set param 0
                set accept_param 0
            } else {
                if {${w} != ""} {
                    ${w} config -relief sunken
                }
                set param 1
            }
        }
        set accept_param ${param}
        #save the parameter for next usage
        set sn_options(both,xref-accept-param) ${param}

        redisplay_root
    }

    method toggle_static {{w ""}} {
        global sn_options
        upvar #0 ${this}-accept_static static

        if {[sn_processes_running]} {
            bell
            return
        }
        if {${w} != "m"} {
            if {${static}} {
                if {${w} != ""} {
                    ${w} config -relief raised
                }
                set static 0
            } else {
                if {${w} != ""} {
                    ${w} config -relief sunken
                }
                set static 1
            }
        }
        set accept_static ${static}
        #save the parameter for next usage
        set sn_options(both,xref-accept-static) ${static}

        redisplay_root
    }

    method redisplay_root {} {
        if {[sn_processes_running]} {
            return
        }
        set id [${can} find withtag %root%]
        if {${id} == ""} {
            return
        }
        set line [convert_data_to_line ${id}]
        remove ${id}
        #delete the root item
        eval graph ${can} remove ${id}
        eval ${can} delete ${id}
        references both x ${line}
    }

    #view cross ref. of item by viewing it at root
    #delete old entries!
    method make_selection_to_root {} {
        if {[sn_processes_running]} {
            bell
            return
        }
        set id [${can} select item]
        if {${id} == ""} {
            return
        }

        #item and root are the same, don't do any thing.
        if {${id} == ${root}} {
            return
        }
        #remove all id's excepting "id" and it's sub tree
        remove ${root} ${id}

        #delete the root item
        eval graph ${can} remove ${root}
        eval ${can} delete ${root}
        graph_new_layout 1
        control_buttons
        see_item ${id}

        #add root tag to it's tags
        set tags [${can} itemcget ${id} -tags]
        ${can} itemconfig ${id} -tags "%root% ${tags}"
        #save id as root id
        set root ${id}

        #display root in the root entry
        set key [lindex $xinfos(${id}) ${item1_pos}]
        set what [lindex $xinfos(${id}) ${what1_pos}]
        set cls [lindex $xinfos(${id}) ${class1_pos}]
        if {${cls} == "#"} {
            set cls ""
        }
        set lbl [string trim "${key}(${what}) ${cls}"]
        upvar #0 ${this}-root_symbol_entry root_symbol_entry
        set root_symbol_entry ${lbl}
    }

    method list_selection_to_root {line procedure} {
        global tkeWinNumber sn_sep

        if {[sn_processes_running]} {
            bell
            return
        }

        if {${line} == ""} {
            bell
            return
        }

        #Verify if the procedure called by the Text entry
        if {[string first "\t" ${line}] == -1 || [string first "\:\:" ${line}]\
          != -1} {
            set i [string first "\:\:" ${line}]
            if {${i} != -1} {
                set class [string range ${line} 0 [expr ${i} - 1]]
                set key [string range ${line} [expr ${i} + 2] end]
                set line [list ${key} ${class}]
            } else {
                set line [split ${line}]
            }
            #the procedure called from treetable
        } else {
            set line [split ${line} \t]
        }

        set key [lindex ${line} 0]
        set class [lindex ${line} 1]

        #split string to key and type, like fu, fd, ...
        get_key_and_scope ${key} key scope

        #get parameter list and delete the brackes "(...)" from the list
        set param [lindex ${line} 2]
        if {${param} != ""} {
            set plen [string length ${param}]
            set param [string range ${param} 1 [expr ${plen} - 2]]
        }

        #Use the separator ansted of the blank character
        set class [string trim ${class}]
        set key [string trim ${key}]
        if {${class} != ""} {
            set pattern "${class}${sn_sep}${key}"
        } else {
            set pattern ${key}
        }

        if {${scope} != "ud"} {
            #retrieve the symbol by accepting the parameter list
            incr tkeWinNumber
            set win "crossrootretriever-${tkeWinNumber}"
            Retriever& ${win} -pattern ${pattern} -what ${scope}\
              -param ${param} -merge {{md mi} {fd fu} {fr fu}}\
              -titlename [get_indep String CrossRetriever]\
              -client_func ${procedure} -client_data "" -terminate 1\
              -variable ${this}-Wait -bell 0
# FIXME: stuff like this should use itcl::local !
            set ret [${win} return_status]
            itcl::delete object ${win}
            if {${ret} != 0} {
                return
            }

            #if we still don't find anything, find any with all scopes
            #and without merging declaration and implementation
            Retriever& ${win} -pattern ${pattern} -what "" -merge {{md mi}\
              {fd fu} {fr fu}} -titlename [get_indep String CrossRetriever]\
              -client_func ${procedure} -client_data "" -terminate 1\
              -variable ${this}-Wait -bell 0
            set ret [${win} return_status]
            itcl::delete object ${win}
            if {${ret} != 0} {
                return
            }
            #if we still don't find anything, find any with all scopes
            #and without merging declaration and implementation
            Retriever& ${win} -pattern ${pattern} -what ""\
              -titlename [get_indep String CrossRetriever]\
              -client_func ${procedure} -client_data "" -terminate 1\
              -variable ${this}-Wait -bell 0
            set ret [${win} return_status]
            itcl::delete object ${win}
            if {${ret} != 0} {
                return
            }

            #the symbol does not exist any where, also list it
            #as is. "symbol(ud)..."
            #add to line the type field as empty field.
            if {${scope} == ""} {
                set line [lreplace ${line} 0 0 "${key}(ud)"]
            }
        }
        set line [linsert ${line} 2 ""]
        eval ${procedure} [list ""] [list ${line}] 1

        catch {mark_item ${root}}
    }

    method recall_make_selection_to_root {w {target ""} {client_data ""}} {
        if {${w} != ""} {
            if {[catch {set sel [${w} curselection]}] || ${sel} == ""} {
                return
            }
            set target [${w} get [lindex ${sel} 0]]
            set data [${w} itemcget [lindex ${sel} 0] -data]
        } else {
            set data ""
        }
        if {[string first \t ${target}] == -1} {
            set target [join ${target} \t]
        }

        #on this position we have all the information what we need
        #about the object to view it's references
        set target [string trim ${target}]

        references both "x" ${target} ${data}
    }

    #display all the known informations from the cross reference
    #accepting the seted filter
    #all == -1: disable all filter flags
    #all == 1: enable all filter flags
    #all == 0: current state of filter flags
    private method fill {{all 0}} {
        global combobox_editor_scopes

        if {$itk_option(-symbols) == "" || ${all} == -2 ||
                [::info commands paf_db_to] == ""} {
            return
        }

        if {$itk_option(-symbols_filter) != ""} {
            upvar #0 $itk_option(-symbols_filter)-related related
            set qry ""
            foreach s [array names combobox_editor_scopes] {
                upvar #0 $itk_option(-symbols_filter)-${s} value
                if {${all} == -1} {
                    uplevel #0 "set $itk_option(-symbols_filter)-${s} off"
                    continue
                }
                if {${all} ||([info exists value] && ${value} != "off")} {
                    foreach db $combobox_editor_scopes(${s}) {
                        if {[::info commands paf_db_${db}] != ""} {
                            uplevel #0 "set $itk_option(-symbols_filter)-${s} ${s}"
                            lappend qry ${db}
                        }
                    }
                }
            }
            #undefined scopes
            upvar #0 $itk_option(-symbols_filter)-ud value
            if {${all} == -1} {
                uplevel #0 "set $itk_option(-symbols_filter)-ud off"
            }\
            elseif {${all}} {
                uplevel #0 "set $itk_option(-symbols_filter)-ud ud"
                lappend qry ud
            }\
            elseif {[info exists value] && ${value} != "off"} {
                lappend qry ud
            }
        } else {
            set qry ""
            set related 1
        }

        if {${qry} == ""} {
            $itk_option(-symbols) configure -contents ""
        } else {
            ${topw} configure -cursor watch
            update idletasks
            # We need to do some simple caching or we're not
            # going very responsive if we got to do a big lookup.
            if {$qry != $lastupdate} {
                set xref_sym_cache [lunique [lsort\
                                      [paf_db_to seq -strstr $qry -cross]]]
                set lastupdate $qry
            }
            $itk_option(-symbols) configure -contents $xref_sym_cache
            ${topw} configure -cursor ""
        }
    }

    method update_post_menu {} {
    }

    proc all_children {can pids result {skip ""}} {
        upvar ${result} res

        #select all children of a symbol, there are:
        #references to
        #references by
        #Tree to
        #Tree by
        foreach i ${pids} {

            set ids [${can} find withtag "to%${i}"]
            eval lappend ids [${can} find withtag "by%${i}"]
            eval lappend ids [${can} find withtag "browse_to%${i}"]
            eval lappend ids [${can} find withtag "browse_by%${i}"]

            if {${skip} != ""} {
                set j [lsearch -exact ${ids} ${skip}]
                if {${j} != -1} {
                    set ids [lreplace ${ids} ${j} ${j}]
                }
            }

            if {${ids} != ""} {
                eval lappend res ${ids}
                all_children ${can} ${ids} res ${skip}
            }
        }
    }

    proc resize {frm can x y id} {
        set coords [${can} coords ${id}]
        set fx [winfo rootx ${frm}]
        set fy [winfo rooty ${frm}]

        set w [expr ${x} - ${fx}]
        set h [expr ${y} - ${fy}]
        ${can} itemconfig ${id} -width ${w} -height ${h}
    }

    # This methods deletes the subnodes of a node, based on their type
    method remove { type {id ""} {skip ""}} {
        if {${id} == ""} {
            set id [${can} select item]
            if {${id} == ""} {
                control_buttons
                return
            }
        }

	set children_ids ""
        set children_children_ids ""
	
	# only search for type'd ids
	if { ${type} == "to" } {
	        set children_ids [${can} find withtag "to%${id}"]
        	eval lappend children_ids [${can} find withtag "browse_to%${id}"]
	} elseif { ${type} == "by" } {
	        set children_ids [${can} find withtag "by%${id}"]
        	eval lappend children_ids [${can} find withtag "browse_by%${id}"]
	} else {
    		# type is "both" or something else
	        all_children ${can} ${id} children_ids ${skip}
        }
        
	if {${children_ids} == ""} {
	    return
        }

        foreach c ${children_ids} {

            #delete depeneded variables or array entries
            catch {unset xinfos(${c})}
	    
	    if {[${can} type ${c}] == "window"} {
                set frm [${can} itemcget ${c} -window]
                ${frm}.sel delete

                destroy ${frm}
	    }
	    
	    # and also remove all children of all found starter nodes
	    all_children ${can} ${c} children_children_ids ${skip}
	    eval graph ${can} remove ${children_children_ids}
            eval ${can} delete ${children_children_ids}
        }
        
	eval graph ${can} remove ${children_ids}
        eval ${can} delete ${children_ids}

        graph_new_layout 1
        control_buttons
        see_item ${id}
    }

    method print {} {
        global sn_options tcl_platform

        if {$tcl_platform(platform)=="windows"} {

            ide_print_canvas ${can}
        } else {

            if {${print_dialog} == "" || [itcl_info objects ${print_dialog}]\
              == ""} {
                set print_dialog [PrintDialog $itk_component(hull).printdialog \
                  -leader ${topw} \
                  -modality application \
                  -canvas ${can} \
                  -file [file join $sn_options(profile_dir) xref.ps]]
	        $print_dialog transient ${topw}
	        $print_dialog activate
	        itcl::delete object $print_dialog
            } else {
                ${print_dialog} raise
            }
        }
    }

    # This proc takes the name of the scope from the input name.
    # There are two possibilities: 'symbol(scope)' or 'scope symbol'.
    # If the input is a canvas item, the scope is the first element in the name.
    method XRefEdit {} {
        upvar #0 ${this}-MaxLevels MaxLevels

        set id [${can} select item]
        if {${id} == "" || ![info exists xinfos(${id})]} {
            return
        }

        #if we have information about the localization of the
        #item to edit it, goto it's localization, otherwise
        #search for function with the KNOWN type and parameters
        set line $xinfos(${id})

        #get range and file name of the item name
        if {[lindex ${line} ${range1_from}] == ""} {
            set line [get_item_range "" ${id} ${MaxLevels}]
            if {${line} == ""} {
                set line $xinfos(${id})
            }
            bind_text_for_balloon ${id} rebind
        }

        set key [lindex ${line} ${item1_pos}]
        set class [lindex ${line} ${class1_pos}]
        set scope [lindex ${line} ${what1_pos}]
        set file [lindex ${line} ${file1_pos}]
        set from [lindex ${line} ${range1_from}]
        if {${class} == "#"} {
            set class ""
        }
        set name [string trim "${class} ${key}"]

        #goto the file directly, without searching
        if {${file} != ""} {
            sn_edit_file [list 0 ${name} ${scope}] ${file} ${from} 0
            #sn_add_history $scope  [list $scope $name 0 $file] \
              [sn_make_history_title edit $scope "$class $key"]
        } else {
            bell
        }
    }

    method handle_doubleclick {can} {
        if {[sn_processes_running]} {
            bell
            return
        }
        if {$itk_option(-doubleclickcommand) != ""} {
            eval $itk_option(-doubleclickcommand) [Selection]
        } else {
            XRefEdit
        }
    }

    method convert_data_to_line {id} {
        set data $xinfos(${id})
        set class [lindex ${data} ${class1_pos}]
        if {${class} == "#"} {
            set class ""
        }
        set line "[lindex ${data} ${item1_pos}]([lindex ${data}\
          ${what1_pos}])\t${class}\t[lindex ${data} ${type1_pos}]\t([lindex\
          ${data} ${param1_pos}])\t[lindex ${data} ${file1_pos}]\t[lindex\
          ${data} ${range1_from}]\t[lindex ${data} ${range1_to}]"
        return ${line}
    }

    method make_line_to_info {id} {
        set to_num [expr [llength [${can} find withtag to%${id}]] / 2]
        set by_num [expr [llength [${can} find withtag by%${id}]] / 2]

        set ent $xinfos(${id})
        set scope [lindex ${ent} ${what1_pos}]
        set class [lindex ${ent} ${class1_pos}]
        set item [lindex ${ent} ${item1_pos}]

        if {${class} != "#"} {
            set txt " [string trim "${class}\:\:${item}(${scope})"]"
        } else {
            set txt " [string trim ${item}(${scope})]"
        }
        if {${scope} == "ud"} {
            set prm "[lindex ${ent} ${param1_pos}]"
        } else {
            set prm ""
        }
        if {[lsearch -exact ${scopes_with_parameters} ${scope}] != -1} {
            set prm "([lindex ${ent} ${param1_pos}])"
        }\
        elseif {${scope} == "ud" && ${prm} != ""} {
            set prm "(${prm})"
        }
        set txt [string trim "[lindex ${ent} ${type1_pos}] ${txt} ${prm}"]
        return "[get_indep String XReferences]: \[${to_num}, ${by_num}\],\
          ${txt}"
    }

    method convert_line_to_data {line data} {
        set ll [split ${line} \t]
        set item [lindex ${ll} 0]
        set class [lindex ${ll} 1]
        set type [lindex ${ll} 2]
        set param [lindex ${ll} 3]
        set file [lindex ${ll} 4]
        set from [lindex ${ll} 5]
        set to [lindex ${ll} 6]

        if {${class} == ""} {
            set class "#"
        }

        get_key_and_scope ${item} item scope

        set param [string range ${param} 1 end]
        if {${param} != ""} {
            set len [string length ${param}]
            set param [string range ${param} 0 [expr ${len} - 2]]
        }

        if {${data} != ""} {
            set data [split ${data} \t]
            set file [lindex ${data} 0]
            set from [lindex ${data} 1]
            set to [lindex ${data} 2]
        }

        #init linfo with empty strings
        for {set i 0} {${i} < ${line_arg_count}} {incr i} {
            lappend linfo ""
        }
        set linfo [lreplace ${linfo} ${class1_pos} ${class1_pos} ${class}]
        set linfo [lreplace ${linfo} ${item1_pos} ${item1_pos} ${item}]
        set linfo [lreplace ${linfo} ${what1_pos} ${what1_pos} ${scope}]
        #root haven't ref. file
        set linfo [lreplace ${linfo} ${file_pos} ${file_pos} ""]
        set linfo [lreplace ${linfo} ${param1_pos} ${param1_pos} ${param}]
        set linfo [lreplace ${linfo} ${range1_from} ${range1_from} ${from}]
        set linfo [lreplace ${linfo} ${range1_to} ${range1_to} ${to}]
        set linfo [lreplace ${linfo} ${file1_pos} ${file1_pos} ${file}]
        set linfo [lreplace ${linfo} ${type1_pos} ${type1_pos} ${type}]
        return ${linfo}
    }

    protected variable fetching_canceled 0
    common fetching_active 0
    protected variable this_fetching 0
    #this pocedure is called from the db-process to
    #verify if the user has canceled the process.
    #is db-fetching canceled??
    method update_db_fetching {} {
        #dispatch events
        update
        update idletasks
        update
        if {${fetching_canceled}} {
            return 0
        } else {
            return 1
        }
    }

    protected variable Cancel_Dialog_Id 0
    method Delete_Cancel_Dialog {} {
        catch {after cancel ${Cancel_Dialog_Id}}
        catch {destroy ${this}.wait_dlg}
        update idletasks
    }

    method Display_Cancel_Dialog {} {
        catch {sn_wait_dialog ${this} [get_indep String WaitOrCancelXRef]\
          [Title 0] "${this} cancel_fetching"}
        update idletasks
    }

    method enable_cancel {state} {
        catch {$itk_option(-toolbar).xref.cancel config -state ${state}}
    }

    #two functions:
    #A. cancel displaying the Xref information, also db-fetching
    #B. cancel xref-generation
    method cancel_fetching {} {
        #cancel xref-generation
        if {[sn_processes_running]} {
            xref_cancel
            return
        }
        set fetching_canceled 1
        update
        update idletasks
        update
    }

    method cancel_Button_State {state} {

        if {${state} == "normal"} {

            if {${fetching_active}} {
                # Fetch-process is active (project wide)!!
                sn_error_dialog [get_indep String CannotProceed] [Title 0]
                return 0
            }

            # Disable fetching for the hole project.
            incr fetching_active

            # We will block the GUI until processing is over.
            tixBusy $itk_component(hull) on

            # Current window busy.
            incr this_fetching

            # Display a wait-cancel dialog box.
            set Cancel_Dialog_Id [after 500 "${this} Display_Cancel_Dialog"]
        } else {
            # Reenable fetching for cross-reference.
            incr fetching_active -1

            # Where finished, release the GUI back to the user.
            tixBusy $itk_component(hull) off

            # Current window is idle.
            incr this_fetching -1

            # Delete dialog window, if active.
            Delete_Cancel_Dialog
        }

        set fetching_canceled 0

        if {[winfo exists $itk_option(-toolbar).xref.cancel]} {
            catch {${toolbar}.xref.cancel config -state ${state}}
            update idletasks
        }

        # We can continue with fetching.
        return 1
    }

    method references {type {id ""} {line ""} {data ""}} {
        global sn_options
        upvar #0 ${this}-layoutstyle layoutstyle
        upvar #0 ${this}-MaxLevels MaxLevels

        # If dbimp active, cancel
        if {[sn_processes_running]} {
            bell
            return
        }

        # No cross reference availiable.
        if {[::info commands paf_db_to] == ""} {
            return
        }

        # 'both' <==> 'to' and 'by'
        if {${type} == "both"} {
            set type [list to by]
        }

        switch -glob ${id} {
            {[0-9]*} {
                    set sym [${can} itemcget ${id} -text]
                    set sym [lindex ${sym} end]
                }
            {} {
                    set id [${can} select item]
                    if {${id} == ""} {
                        return
                    }
                }
            default {

                    # Delete OLD ENTRIES.
                    ${this} clear

                    # It doesn't exist we create it.

                    set info [convert_line_to_data ${line} ${data}]

                    set class [lindex ${info} ${class1_pos}]
                    if {${class} == "#"} {
                        set class ""
                    }
                    set key [lindex ${info} ${item1_pos}]
                    set what [lindex ${info} ${what1_pos}]
                    set rart [lindex ${info} ${refart_pos}]
                    set text [string trim "${rart} ${key}(${what}) ${class}"]
                    set lbl ${text}

                    if {${what} == ""} {
                        set what "?"
                    }

                    if {${disp_param} && [lsearch\
                      -exact ${scopes_with_parameters} ${what}] != -1} {
                        set text " [string trim "[lindex ${info} ${type1_pos}]\
                          ${text}([lindex ${info} ${param1_pos}])"]"
                    }

                    set fill ""
                    set font $sn_options(def,xref-font)

                    set id [eval [list ${can} create text -1000 -1000 -text\
                      " ${text}" -anchor nw -font ${font}] ${fill}]

                    # Set tags for the ID and NOT for symbol name.
                    ${can} itemconfig ${id} -tags "sym %root% root%${id} to%"

                    # Store information about the item from the given line
                    # information.
                    set xinfos(${id}) ${info}
                    bind_text_for_balloon ${id}

                    ${can} delete %boxes%
                    graph ${can} add ${id}

                    set root ${id}

                    # Add item to history
                    #set ref_root_sym $text
                    #regsub -all {[ ]+} $ref_root_sym {} ref_root_sym
                    if {${class} != ""} {
                        set member "${class}\:\:${key}"
                    } else {
                        set member ${key}
                    }
                    #set data [list $type $member $what [lindex $info\
                      $file1_pos] [lindex $info $range1_from] [lindex $info\
                      $range1_to]]
                    #sn_add_history xref $data [sn_make_history_title xref\
                      $what "$class $key"]

                    upvar #0 ${this}-root_symbol_entry root_symbol_entry
                    set root_symbol_entry ${lbl}
                }
        }

        # Enable cancel button, be sure that no other
        # process is already running.
        if {![cancel_Button_State normal]} {
            bell
            return
        }

        set ret ""
        # Show referencies by.
        if {[lsearch ${type} "by"] != -1} {
            set ret [show_calls by ${id} ${MaxLevels}]

            if {$itk_option(-menu) != "" && [winfo exists $itk_option(-menu)]} {
                $itk_option(-menu) entryconfig [get_indep String CrfRefBy] -state disabled
            }
            if {$itk_option(-toolbar) != ""} {
                ${exp}.ref_by config -state disabled
            }
        }

        # Show referencies to.
        if {${ret} != "-1" && [lsearch ${type} "to"] != -1} {
            show_calls to ${id} ${MaxLevels}
            if {$itk_option(-menu) != "" && [winfo exists $itk_option(-menu)]} {
                $itk_option(-menu) entryconfig [get_indep String CrfRefTo] -state disabled
            }
            if {$itk_option(-toolbar) != ""} {
                ${exp}.ref_to config -state disabled
            }
        }

        # This is a workaround for 'tree'.
        ${can} delete %boxes%

        # Reset cancel variable
        cancel_Button_State disabled

        graph_new_layout 1

        control_buttons

        see_item ${id}
    }

    # This function assures that the item will be on the screen.
    method see_item {{id ""}} {
        if {${id} == ""} {
            set id [${can} select item]
            if {${id} == ""} {
                return
            }
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

            set wid [expr [lindex ${scr_reg} 2] - [lindex ${scr_reg} 0]]
            set hei [expr [lindex ${scr_reg} 3] - [lindex ${scr_reg} 1]]

            set xoff [expr double([winfo width ${c}]) / 3]
            set pos [expr double([lindex ${coords} 0] - ${xoff}) / ${wid}]
            ${c} xview moveto ${pos}

            set yoff [expr double([winfo height ${c}]) / 3]
            set pos [expr double([lindex ${coords} 1] - ${yoff}) / ${hei}]
            ${c} yview moveto ${pos}
        }
    }

    proc draw_rectangles {can {draw 1}} {
        if {!${draw}} {
            return
        }
        foreach b [${can} find withtag sym] {
            set geom [${can} bbox ${b}]
            set y [expr [lindex ${geom} 1] - 2]
            set geom [lreplace ${geom} 1 1 ${y}]
            eval ${can} create rect ${geom} -tags %boxes%
        }
    }

    method bind_text_for_balloon {id {rebind ""}} {
        set class [lindex $xinfos(${id}) ${class1_pos}]
        set what [lindex $xinfos(${id}) ${what1_pos}]
        set file [lindex $xinfos(${id}) ${file1_pos}]
        set refart [lindex $xinfos(${id}) ${refart_pos}]
        set item " [lindex $xinfos(${id}) ${item1_pos}]"
        set param "([lindex $xinfos(${id}) ${param1_pos}])"
        set type [join [lindex $xinfos(${id}) ${type1_pos}]]

        if {${class} == "#"} {
            set class ""
        }
        if {${class} != ""} {
            set class " ${class}"
        }
        if {${type} != ""} {
            set type " ${type}"
        }
        #we accept for unknown symbols none empty parameter list
        #when an unknown symbol have empty parameter list, delete the
        #"()" from the parameter list.
        if {${what} != "ud" && [lsearch -exact ${scopes_with_parameters}\
          ${what}] == -1} {
            set param ""
        }\
        elseif {${what} == "ud" && ${param} == "()"} {
            set param ""
        }
        if {${param} != ""} {
            set param " ${param}"
        }
        if {${file} != ""} {
            set file " ${file}"
        }
        if {${refart} != ""} {
            set refart " ${refart}"
        }
        if {${param} != ""} {
            set param " ${param}"
        }

        set txt "${type}${class}${item}(${what})${param}${refart}${file}"

        if {${rebind} == ""} {
            canvas_bind_info ${can} ${id} ${txt}
        } else {
            canvas_rebind_info ${can} ${id} ${txt}
        }
    }

    #if level not equal ziro, don't view choose window
    method get_item_range {ref id {level 0}} {
        global sn_options sn_sep tkeWinNumber
        global ${this}-Wait
        upvar #0 ${this}-MaxLevels MaxLevels

        set line $xinfos(${id})
        set scope [lindex ${line} ${what1_pos}]
        set file [lindex ${line} ${file_pos}]
        set param [lindex ${line} ${param1_pos}]
        set cls [lindex ${line} ${class1_pos}]
        set sym [lindex ${line} ${item1_pos}]

        #localize the item to reference/edit
        while {1} {
            if {[lsearch -exact ${types_with_classes} ${scope}] != -1} {
                set key [string trim "${cls}${sn_sep}${sym}" " ${sn_sep}"]
                if {[::info commands paf_db_${scope}] != ""} {
                    set total [paf_db_${scope} seq -uniq -col {2 3 4 6 7}\
                      "${key}${sn_sep}"]
                } else {
                    set total ""
                }
            } else {
                set key [string trim ${sym}]
                if {[::info commands paf_db_${scope}] != ""} {
                    set total [paf_db_${scope} seq -uniq -col {1 2 3 5 6}\
                      "${key}${sn_sep}"]
                } else {
                    set total ""
                }
            }

            #if result is empty, it can be a function or method with
            #declaration only
            if {${total} == "" && [lsearch -exact {mi fu} ${scope}] != -1} {
                switch ${scope} {
                    "mi" {
                            set scope "md"
                        }
                    "fu" {
                            set scope "fd"
                        }
                    default {
                            break
                        }
                }
            } else {
                break
            }
        }

        #pick up a function in the same file as reference
        #this has the biggest priority
        foreach l ${total} {
            set prm [lindex ${l} 4]
            if {${file} == [lindex ${l} 1] && ${prm} == ${param}} {
                set line [lreplace ${line} ${range1_from} ${range1_from}\
                  [lindex ${l} 0]]
                set line [lreplace ${line} ${range1_to} ${range1_to}\
                  [lindex ${l} 2]]
                set line [lreplace ${line} ${file1_pos} ${file1_pos}\
                  [lindex ${l} 1]]
                set line [lreplace ${line} ${type1_pos} ${type1_pos}\
                  [lindex ${l} 3]]
                set xinfos(${id}) ${line}
                return ${line}
            }
        }

        #it can be more than one item availiable
        set cnt 0
        foreach l ${total} {
            set type [lindex ${l} 3]
            set prm [lindex ${l} 4]
            if {${prm} == ${param}} {
                incr cnt
                if {${cnt} == 1} {
                    set ll ${l}
                }\
                elseif {${cnt} > 1} {
                    break
                }
            }
        }

        #we have only one match, return it
        if {${cnt} == 1 || ${cnt} > 1 && ${level} != ${MaxLevels}} {
            set line [lreplace ${line} ${range1_from} ${range1_from}\
              [lindex ${ll} 0]]
            set line [lreplace ${line} ${range1_to} ${range1_to} [lindex ${ll}\
              2]]
            set line [lreplace ${line} ${file1_pos} ${file1_pos} [lindex ${ll}\
              1]]
            set line [lreplace ${line} ${type1_pos} ${type1_pos} [lindex ${ll}\
              3]]
            set xinfos(${id}) ${line}
            return ${line}

            #we have more than one match, retrieve one match
        }\
        elseif {${cnt} > 1} {

            incr tkeWinNumber
            set win "crossidentifyretriever-${tkeWinNumber}"

            #we use now a class
            Retriever& ${win} -pattern ${key} -what ${scope} -param ${param}\
              -titlename [get_indep String CrossRetriever]\
              -client_func "${this} recall_show_calls" -client_data\
              [list ${ref} ${id}] -terminate 1 -merge {{md mi} {fd fu}\
              {fr fu}} -variable ${this}-Wait -bell 0
            itcl::delete object ${win}
            return $xinfos(${id})
        }

        #there is no symbol matched the availiable parameters
        #pick up the first item, if there is only one item
        set len [llength ${total}]
        if {${total} != "" && ${len} == 1 || ${len} > 1 && ${level} !=\
          ${MaxLevels}} {
            set l [lindex ${total} 0]
            set line [lreplace ${line} ${range1_from} ${range1_from}\
              [lindex ${l} 0]]
            set line [lreplace ${line} ${range1_to} ${range1_to} [lindex ${l}\
              2]]
            set line [lreplace ${line} ${file1_pos} ${file1_pos} [lindex ${l}\
              1]]
            set line [lreplace ${line} ${type1_pos} ${type1_pos} [lindex ${l}\
              3]]
            set line [lreplace ${line} ${param1_pos} ${param1_pos}\
              [lindex ${l} 4]]
            set xinfos(${id}) ${line}
        }\
        elseif {${total} != "" && ${level} == ${MaxLevels}} {
            incr tkeWinNumber
            set win "crossidentifyretriever-${tkeWinNumber}"
            Retriever& ${win} -pattern ${key} -what ${scope} -param ""\
              -merge {{md mi} {fd fu} {fr fu}} -titlename [get_indep String\
              CrossRetriever] -client_func "${this} recall_show_calls"\
              -client_data [list ${ref} ${id}] -terminate 1\
              -variable ${this}-Wait -bell 0
            set ret [${win} return_status]
            itcl::delete object ${win}
            if {${ret} != 0} {
                return ""
            }
            return $xinfos(${id})

            #we don't find any thing about the item
            #search any item in the database
        }\
        elseif {${level} == ${MaxLevels}} {
            incr tkeWinNumber
            set win "crossidentifyretriever-${tkeWinNumber}"
            Retriever& ${win} -pattern ${key} -what all -merge {{md mi}\
              {fd fu} {fr fu}} -titlename [get_indep String CrossRetriever]\
              -client_func "${this} recall_show_calls" -client_data\
              [list ${ref} ${id}] -terminate 1 -variable ${this}-Wait -bell 0
            set ret [${win} return_status]
            itcl::delete object ${win}
            if {${ret} == 0} {
                #find pattern without merging fu=fd and mi=md
                Retriever& ${win} -pattern ${key} -what all\
                  -titlename [get_indep String CrossRetriever]\
                  -client_func "${this} recall_show_calls" -client_data\
                  [list ${ref} ${id}] -terminate 1 -variable ${this}-Wait\
                  -bell 0
                set ret [${win} return_status]
                itcl::delete object ${win}
                if {${ret} != 0} {
                    return ""
                }
            }
            return $xinfos(${id})
        }
        return ${line}
    }

    #given is a list of tags, show if 'tags' already contains the
    #tag text saved by 'tag', if yes, return the fully tag name
    #empty else.
    proc store_tags {tags tag} {
        set i [string first ${tag} ${tags}]
        if {${i} != -1} {
            incr i
            set rest [string range ${tags} ${i} end]
            set j [string first ${rest} " "]
            if {${j} == -1} {
                set j [string length ${rest}]
            }
            set in [string range ${tags} ${i} ${j}]
        } else {
            set in ""
        }
        return ${in}
    }

    #if we have an unknown symbol and we changed it with an known
    #we must rebin the tags to this symbol to fit to the state
    # of the new symbol
    method rebind_tags {id} {
        set have_to [read_references to $xinfos(${id}) 0 1]
        if {${have_to} != ""} {
            set have_to "have_to%${id}"
        } else {
            set have_to ""
        }
        set have_by [read_references by $xinfos(${id}) 0 1]
        if {${have_by} != ""} {
            set have_by "have_by%${id}"
        } else {
            set have_by ""
        }
        set tags [${can} itemcget ${id} -tags]
        set in_to [store_tags ${tags} " to%"]
        set in_by [store_tags ${tags} " by%"]

        if {${id} == ${root}} {
            set rt "%root%"
        } else {
            set rt ""
        }

        ${can} itemconfig ${id} -tags [string trim "${rt} sym ${in_to}\
          ${in_by}"]
    }

    #this function is called by retriever to identify current symbol
    #status. It's set the new informations to the symbol and display it
    #in the canvas.
    method recall_show_calls {w {target ""} {client_data ""}} {
        set ref [lindex ${client_data} 0]
        set id [lindex ${client_data} 1]

        #get selected item
        if {${w} != ""} {
            if {[catch {set sel [${w} curselection]}] || ${sel} == ""} {
                return
            }
            set target [${w} get [lindex ${sel} 0]]
            set data [${w} itemcget [lindex ${sel} 0] -data]

            set target [split ${target} \t]

            set data [split ${data} \t]
            set file [lindex ${data} 0]
            set from [lindex ${data} 1]
            set to [lindex ${data} 2]
            set scope [lindex ${data} 3]

        } else {
            set target [split ${target} \t]

            set file [lindex ${target} 4]
            set from [lindex ${target} 5]
            set to [lindex ${target} 6]
            get_key_and_scope [lindex ${target} 0] x scope
        }

        set class [lindex ${target} 1]
        set type [lindex ${target} 2]
        set param [lindex ${target} 3]
        set param [string range ${param} 1 end]
        if {${param} != ""} {
            set len [string length ${param}]
            set param [string range ${param} 0 [expr ${len} - 2]]
        }
        if {${class} == ""} {
            set class "#"
        }

        set line $xinfos(${id})
        set line [lreplace ${line} ${class1_pos} ${class1_pos} ${class}]
        set line [lreplace ${line} ${what1_pos} ${what1_pos} ${scope}]
        set line [lreplace ${line} ${file1_pos} ${file1_pos} ${file}]
        set line [lreplace ${line} ${range1_from} ${range1_from} ${from}]
        set line [lreplace ${line} ${range1_to} ${range1_to} ${to}]
        set line [lreplace ${line} ${type1_pos} ${type1_pos} ${type}]
        set line [lreplace ${line} ${param1_pos} ${param1_pos} ${param}]


        set item [lindex ${line} ${item1_pos}]
        if {${class} != "#"} {
            set tx [string trim "${item}(${scope}) ${class}"]
        } else {
            set tx [string trim "${item}(${scope})"]
        }

        #change the old displayed text to the correct new text
        ${can} itemconfig ${id} -text " ${tx}"

        #reset xinfos for the new changes
        set xinfos(${id}) ${line}

        #rebind tags accepting the new futures
        rebind_tags ${id}

        #rebind info help with the new text
        bind_text_for_balloon ${id} rebind

        graph_new_layout 1

        #reselect the item
        catch {
            ${can} select from ${id} 0
            ${can} select to ${id} end
        }
    }

    method read_references {ref line {uniq 1} {have 0}} {
        global sn_options sn_sep
        set scope [lindex ${line} ${what1_pos}]
        set name [lindex ${line} ${item1_pos}]

        #not all scopes can have references to, we don't
        #need to continue
        if {${name} == "" || ${ref} == "to" && [lsearch\
          -exact ${scopes_with_ref_to} ${scope}] == -1} {
            return ""
        }
        if {${accept_param}} {
            #get parameter list from line
            if {[string first ${scope} ${scopes_with_parameters}] != -1} {
                set param [lindex ${line} ${param1_pos}]
                set prm [list "=${param}"]
            } else {
                set prm [list "|"]
            }
            if {${ref} == "to"} {
                #verify if we have data informations, data informations are\
                  usefull
                #to localize the item position
                set file [lindex ${line} ${file1_pos}]
                if {${file} != ""} {
                    set fl [list "=${file}"]
                    #get the line number from format (ll.cc ==> ll)
                    set from [lindex ${line} ${range1_from}]
                    if {${from} != -1} {
                        set from [string trimleft [lindex [split ${from} .] 0]\
                          0]
                    }
                    set to [lindex ${line} ${range1_to}]
                    if {${to} != -1} {
                        set to [string trimleft [lindex [split ${to} .] 0] 0]
                    }
                    if {${from} == "" || ${from} < 0} {
                        set from 0
                    }
                    if {${to} == "" || ${to} < 0} {
                        set to 100000000
                    }
                    set FromTo [list "-${from}:${to}"]
                } else {
                    set fl [list "|"]
                    set FromTo "|"
                }
            } else {
                set fl [list "|"]
                set FromTo "|"
            }
        } else {
            set fl [list "|"]
            set FromTo "|"
            set prm [list "|"]
        }

        set symbol "[lindex ${line}\
          ${class1_pos}]${sn_sep}${name}${sn_sep}${scope}${sn_sep}"

        #fetch the references from the database.
        #format:
        #0: class or "#" by no class members
        #1: symbol
        #2: what
        #3: Parameter
        #4: reference art (r, w, p, u)
        #5: file
        #6: File position (line number)

        if {[info commands paf_db_${ref}] == ""} {
            return ""
        }
        set total [paf_db_${ref} seq -uniq -updatecommand\
          "${this} update_db_fetching" -col [list 4 3 5 10 6 "8 ${fl}"\
          "7 ${FromTo}" "9 ${prm}"] ${symbol}]

        #by 'have', we don't need to sort the readed data
        if {${have}} {
            return [cross_services "" filter ${total} ${ref} ${line} ${uniq}\
              ${have} ${accept_param} ${accept_static} ${cross_shown_scopes}\
              ${cross_ref_access}]
        } else {
            return [cross_services "" filter [::lsort -command sn_compare\
              ${total}] ${ref} ${line} ${uniq} ${have} ${accept_param}\
              ${accept_static} ${cross_shown_scopes} ${cross_ref_access}]
        }
    }

    method explore_calls {{id ""}} {
        upvar #0 ${this}-MaxLevels MaxLevels

	# Check that MaxLevels is a valid
	# numeric expression. (It still might
	# not be a valid level value.)

	if {[catch {expr $MaxLevels - 1}] == 1} {
	    set MaxLevels 1
	}

        if {[sn_processes_running]} {
            bell
            return
        }

        if {${id} == ""} {
            set id [${can} select item]
            if {${id} == ""} {
                return
            }
        }

        # Process is already running.
        if {![cancel_Button_State normal]} {
            bell
            return
        }

        #remove old entries
        remove ${id}

        #add references to
        set to [${can} find withtag to%${id}]
        set have [read_references to $xinfos(${id}) 1 1]
        if {${have} != "" && ${to} == ""} {
            show_calls to ${id} ${MaxLevels}
        }
        #add references by
        set by [${can} find withtag by%${id}]
        if {${by} == ""} {
            set have [read_references by $xinfos(${id}) 1 1]
            if {${have} != ""} {
                show_calls by ${id} ${MaxLevels}
            }
        }

        graph_new_layout 1

        cancel_Button_State disabled
    }

    method show_calls {ref id {level 0}} {
        global sn_options
        upvar #0 ${this}-MaxLevels MaxLevels

        if {[sn_processes_running]} {
            bell
            return
        }

        #displaying Xref is canceled by user
        update
        if {${fetching_canceled}} {
            return
        }

        set line $xinfos(${id})

        #get range and file name of the item name
        if {${ref} != "by" && [lindex ${line} ${what1_pos}] != "ud" &&\
          [lindex ${line} ${range1_from}] == ""} {
            set line [get_item_range ${ref} ${id} ${level}]
            if {${line} == ""} {
                return "-1"
            }
            bind_text_for_balloon ${id} rebind
            if {${line} == ""} {
                set line $xinfos(${id})
            }
        }

        set total [read_references ${ref} ${line}]
        set edges ""
        foreach ent ${total} {
            #merge equivalent lines
            set scope [lindex ${ent} ${what1_pos}]
            set class [lindex ${ent} ${class1_pos}]
            set item [lindex ${ent} ${item1_pos}]
            set rart [lindex ${ent} ${refart_pos}]

            if {${class} != "#"} {
                set tx " [string trim "${rart} ${item}(${scope}) ${class}"]"
            } else {
                set tx " [string trim "${rart} ${item}(${scope})"]"
            }

            if {${disp_param}} {
                if {[lsearch -exact ${scopes_with_parameters} ${scope}] != -1} {
                    set prm "([lindex ${ent} ${param1_pos}])"
                }\
                elseif {${scope} == "ud"} {
                    set prm "([lindex ${ent} ${param1_pos}])"
                    if {${prm} == "()"} {
                        set prm ""
                    }
                } else {
                    set prm ""
                }
                set tx " [string trim "[lindex ${ent}\
                  ${type1_pos}]${tx}${prm}"]"
            }

            #verify if the function has references
            #set font $sn_options(def,xref-branch-font)
            #set fill "-fill $sn_options(def,xref-branch-fg)"
            set font $sn_options(def,xref-font)

            #create the texts so that the text is not shown
            #in the canvas, because the texts are then shown
            #over each other
            set tid [${can} create text -50 -50 -text ${tx} -anchor nw\
              -font ${font}]

            ${can} itemconfig ${tid} -tags "sym ${ref}% ${ref}%${id}"

            #draw text and edges
            if {${ref} == "to"} {
                set eid [${can} create edge -1m -1m -1m -1m -tag\
                  "edge_to to%${id} edge%${tid}" -from ${id} -to ${tid}\
                  -arrow last -fill $sn_options(def,canv-line-fg)]
            } else {
                set eid [${can} create edge -1m -1m -1m -1m -tag\
                  "edge_by by%${id} edge%${tid}" -from ${id} -to ${tid}\
                  -arrow first -fill $sn_options(def,xref-used-by-fg)]
            }

            set xinfos(${tid}) ${ent}
            bind_text_for_balloon ${tid}

            lappend edges ${tid} ${eid}

            #if the symbol have references to "ref" then view it's references
            #when the levels are more than one.
            if {${level} > 1} {
                if {(${ref} == "to" && [read_references to ${ent} 0 1] !=\
                  "" || ${ref} == "by" && [read_references by ${ent} 0 1] !=\
                  "")} {
                    show_calls ${ref} ${tid} [expr ${level} - 1]
                }
            }
        }
        if {${edges} != ""} {
            graph ${can} add ${edges}
        }
        return ${edges}
    }

    method edit_location {w y} {
        set line [${w} get [${w} nearest ${y}]]
        set len [llength ${line}]
        incr len -1
        set file [lindex ${line} ${len}]
        incr len -1
        set lnum [string trimleft [lindex ${line} ${len}] "0"]
        if {$itk_option(-selectcommand) != ""} {
            eval $itk_option(-selectcommand) [list "" "" "" ${file} ${lnum}]
        } else {
            sn_edit_file dummy ${file} ${lnum} 0
        }
    }

    method values {} {
        return ${base_root}
    }

    method mark_item {{item ""} {exec 1}} {
        if {[sn_processes_running]} {
            bell
            return
        }

        if {[string compare ${item} ""] == 0} {
            set id [${can} find withtag current]
            set item [${can} itemcget ${id} -text]
        } else {
            set id [${can} find withtag ${item}]
        }

        if {${id} == ""} {
            return
        }

        catch {${can} select from ${id} 0}
        catch {${can} select to ${id} end}

        # If the selection is not owned by us we do not want the marking to\
          disappear.
        selection own ${can} " "

        control_buttons

        #handle select command
        if {${exec} && $itk_option(-selectcommand) != ""} {
            eval $itk_option(-selectcommand) [Selection]
        }

        if {$itk_option(-symbols) != ""} {
            if {[string first " " ${item}] == 0} {
                $itk_option(-symbols) selecttext [string trim ${item}]
            } else {
                $itk_option(-symbols) selecttext [lrange ${item} 1 end]
            }
        }

        return [list ${item} ${id}]
    }

    method control_buttons {{id ""}} {
        if {[::info commands paf_db_by] == "" || [::info commands paf_db_to]\
          == ""} {
            return
        }
        if {${id} == ""} {
            set selid [${can} select item]
            set id ${selid}
        } else {
            set selid ""
        }
        set ref_to_state disabled
        set ref_by_state disabled

        set remove_state disabled

        set browse_to_state disabled
        set browse_to_relief raised
        set browse_by_state disabled
        set browse_by_relief raised
        set take_root_state "disabled"

        if {${id} != ""} {
            set tags [${can} itemcget ${id} -tags]
            # Verify if the symbol has references by.
            #set have_by [$can find withtag "have_by%$id"]
            if {[read_references by $xinfos(${id}) 1 1] != ""} {
                set have_by "have_by%"
            } else {
                set have_by ""
            }

            # Verify if we can view the browse by.
            if {${have_by} != ""} {
                set browse_by_state normal
            }
            # References by are displayed ?
            if {[${can} find withtag "by%${id}"] == ""} {
                # Verify if it have references by.
                if {${have_by} != ""} {
                    set ref_by_state normal
                }
            } else {
                set remove_state normal
            }

            # Verify if we can view the browse to.
            set have_to [read_references to $xinfos(${id}) 1 1]
            if {${have_to} != ""} {
                set browse_to_state normal
            }
            # Does it have reference to ?
            if {[${can} find withtag "to%${id}"] == ""} {
                # Verify if it have references to.
                if {${have_to} != ""} {
                    set ref_to_state normal
                }
            } else {
                set remove_state normal
            }

            # Is browse to frame viewed.
            if {[${can} find withtag "browse_to%${id}"] != ""} {
                set remove_state normal
                set browse_to_relief sunken
            }

            # Is browse by frame viewed
            if {[${can} find withtag "browse_by%${id}"] != ""} {
                set remove_state normal
                set browse_by_relief sunken
            }

            # View number of references.
            set $itk_option(-message_var) "[make_line_to_info ${id}]"
        } else {
            set $itk_option(-message_var) ""
        }

        # Handle menu entries, if availiable.
        if {$itk_option(-menu) != "" && [winfo exists $itk_option(-menu)]} {
            $itk_option(-menu) entryconfig [get_indep String CrfRefTo]\
              -state ${ref_to_state}
            $itk_option(-menu) entryconfig [get_indep String CrfRefBy]\
              -state ${ref_by_state}
            $itk_option(-menu) entryconfig [get_indep String CrfRemSubN]\
              -state ${remove_state}
            $itk_option(-menu) entryconfig [get_indep String PafCrossToDetail]\
              -state ${browse_to_state}
            $itk_option(-menu) entryconfig [get_indep String PafCrossByDetail]\
              -state ${browse_by_state}
            if {${id} != ${root}} {
                set take_root_state "normal"
            }
            $itk_option(-menu) entryconfig [get_indep String TakeRoot]\
              -state ${take_root_state}
        }

        # Handle toolbar buttons, if availiable.
        if {$itk_option(-toolbar) != ""} {
            ${exp}.ref_to config -state ${ref_to_state}
            ${exp}.ref_by config -state ${ref_by_state}
            ${exp}.remove config -state ${remove_state}
            if {[winfo exists ${exp}.xbrowse_to]} {
                ${exp}.xbrowse_to config -state ${browse_to_state}\
                  -relief ${browse_to_relief}
                ${exp}.xbrowse_by config -state ${browse_by_state}\
                  -relief ${browse_by_relief}
            }
        }

        # Handle rightmouse entries.
        ${can}.menu entryconfig [get_indep String CrfRefTo]\
          -state ${ref_to_state}
        ${can}.menu entryconfig [get_indep String CrfRefBy]\
          -state ${ref_by_state}
        ${can}.menu entryconfig [get_indep String PafCrossToDetail]\
          -state ${browse_to_state}
        ${can}.menu entryconfig [get_indep String PafCrossByDetail]\
          -state ${browse_by_state}
        ${can}.menu entryconfig [get_indep String CrfRemSubN]\
          -state ${remove_state}
        ${can}.menu entryconfig [get_indep String TakeRoot]\
          -state ${take_root_state}

        return [list ${ref_to_state} ${ref_by_state} ${browse_to_state}\
          ${browse_by_state}]
    }

    method graph_new_layout {{redraw 1}} {
        global sn_options
        upvar #0 ${this}-layoutstyle layout
        upvar #0 ${this}-order order

        if {[sn_processes_running]} {
            return
        }

        ${can} delete %boxes%

        if {${redraw}} {
            graph ${can} clear
            set ids [${can} find withtag all]
            if {${ids} == ""} {
                return
            }
            catch {eval graph ${can} add ${ids}}
        }

        if {${order} == ""} {
            set order 0
        }

        # New graph layout.
        if {${order} == 0} {
            set cmd [list -nodespacev $sn_options(def,xref-horizspace)\
              -nodespaceh $sn_options(def,xref-vertspace)]
        } else {
            set cmd [list -nodespacev $sn_options(def,xref-vertspace)\
              -nodespaceh $sn_options(def,xref-horizspace)]
        }

        eval graph ${can} config -order ${order} -gridlock 1 ${cmd}
        graph ${can} layout ${layout}

        draw_rectangles ${can} ${draw_rectangles}

        set reg [${can} bbox all]
        set wd [lindex ${reg} 2]
        set he [lindex ${reg} 3]
        ${can} configure -scrollregion [list 0 0 ${wd} ${he}]

        control_buttons
    }

    method clear {} {
        graph ${can} destroy
        ${can} delete all
    }

    proc xref_filter {} {
        global sn_options
        global xref_filter_status
        global sn_options

        if {[sn_processes_running]} {
            bell
            return
        }

        set t ${prefwin}

        if {[winfo exists ${t}]} {
            ${t} raise
            return
        }
        sourcenav::Window ${t}

        ${t} on_close "${t}.button_1 invoke"
        set title [string trimright [get_indep String XRefFilter] "."]
        ${t} configure -title [sn_title ${title}]
        ${t} configure -iconname ${title}

        sn_motif_buttons ${t} bottom 0 [get_indep String ok] [get_indep String\
          cancel]

        ${t}.button_0 config -command { set xref_filter_status "ok" }

        ${t}.button_1 config -command { set xref_filter_status "cancel" }

        set len1 [string length [get_indep String All]]
        set len2 [string length [get_indep String None]]
        if {${len1} < ${len2}} {
            set len1 ${len2}
        }
        incr len1 2
        if {${len1} < 6} {
            set len1 6
        }

        set button_all [button ${t}.all2 -text [get_indep String All] -width ${len1} -command "XRef&::reset_xref_filter ${t} 1"]

        pack $button_all -pady 2

        set button_none [button ${t}.none -text [get_indep String None] -width ${len1} -command "XRef&::reset_xref_filter ${t} 0"]
        pack $button_none -pady 2

        set scopes ${cross_scopes}
        if {[lsearch -exact $sn_options(sys,parser_switches) "-l"] != -1} {
            lappend scopes lv
        }

        foreach s [lsort ${scopes}] {
            upvar #0 ${t}-value-${s} val
            if {[lsearch -exact ${cross_shown_scopes} ${s}] == -1} {
                set val ""
            } else {
                set val ${s}
            }

            pack [checkbutton ${t}.${s} -text "[convert_scope_to_str ${s}]\
              \(${s}\)" -variable ${t}-value-${s} -onvalue ${s} -offvalue ""]\
              -anchor w -padx 10
        }

        frame ${t}.gap -height 2 -relief sunken -borderwidth 2 -width 300
        pack ${t}.gap -fill x -pady 10 -padx 5

        pack [label ${t}.access -text [get_indep String Access]]

        foreach s "r w p u" {
            upvar #0 ${t}-access-${s} val
            if {[lsearch -exact ${cross_ref_access} ${s}] == -1} {
                set val ""
            } else {
                set val ${s}
            }
            switch ${s} {
                "r" {
                        set tx [get_indep String Read]
                    }
                "w" {
                        set tx [get_indep String Written]
                    }
                "p" {
                        set tx [get_indep String Passed]
                    }
                "u" {
                        set tx [get_indep String Unused]
                    }
            }
            append tx " \(${s}\)"
            pack [checkbutton ${t}.${s} -text ${tx} -variable ${t}-access-${s}\
              -onvalue ${s} -offvalue ""] -anchor w -padx 10
        }

        ${t} move_to_mouse
        catch {${t} resizable no no}
        focus ${t}
        tkwait variable xref_filter_status

        itcl::delete object ${t}

        if {${xref_filter_status} == "cancel"} {
            return
        }

        set lst [::info globals "${t}-value-*"]
        set cross_shown_scopes ""
        foreach v ${lst} {
            upvar #0 ${v} value
            lappend cross_shown_scopes ${value}
        }

        set lst [::info globals "${t}-access-*"]
        set cross_ref_access ""
        foreach v ${lst} {
            upvar #0 ${v} value
            lappend cross_ref_access ${value}
        }

        paf_db_proj put cross_shown_scopes ${cross_shown_scopes}
        paf_db_proj put cross_ref_access ${cross_ref_access}
        paf_db_proj sync

        foreach obj [itcl_info objects "*" -class XRef&] {
            #redisplay windows
            ${obj} Refresh_Display
        }
    }

    proc reset_xref_filter {t set} {
        foreach v [::info globals "${t}-value-*"] {
            upvar #0 ${v} value
            if {${set}} {
                set scope [lindex [split ${v} "-"] end]
                set value ${scope}
            } else {
                set value ""
            }
        }
    }

    method xbrowse {ref {id ""}} {
        global sn_options
        upvar #0 ${this}-layoutstyle layout

        #dbimp active
        if {[sn_processes_running]} {
            bell
            return
        }

        if {${id} == ""} {
            set id [${can} select item]
            if {${id} == ""} {
                return
            }
        }

        set line $xinfos(${id})
        set name [lindex ${line} ${item1_pos}]
        if {${name} == ""} {
            return
        }
        set tagname "browse_${ref}%${id}"

        set frm ${can}.w-${ref}-${id}
        set info ${this}.status.msg

        if {[winfo exists ${frm}]} {
            graph ${can} remove ${tagname}
            ${can} delete ${tagname}
            ${frm}.sel delete
            destroy ${frm}
            graph_new_layout 1
            return
        }

        #read the references to view
        set result [read_references ${ref} ${line} 0]
        if {${result} == ""} {
            return
        }
        set sym "${name}([lindex ${line} ${what1_pos}])"
        set cls [lindex ${line} ${class1_pos}]
        if {${cls} != "#"} {
            set sym "${name} ${cls}"
        }
        if {$itk_option(-menu) != "" && [winfo exists $itk_option(-menu)]} {
            set bg [$itk_option(-menu) cget -background]
        } else {
            set bg [${topw}.menu cget -background]
        }
        set ebg [${can} cget -background]
        frame ${frm} -bd 2 -background ${bg} -relief raised
        set len [llength ${result}]
        if {${len} > 10} {
            set len 10
        }

        #view references in a list using tab stops
        set res ""
        foreach r ${result} {
            set var [lindex ${r} ${item1_pos}]
            set cls [lindex ${r} ${class1_pos}]
            set type [lindex ${r} ${what1_pos}]
            set refart [lindex ${r} ${refart_pos}]
            set line [lindex ${r} ${file_line_pos}]
            set file [lindex ${r} ${file_pos}]

            if {[string index ${cls} 0] == "#" || ${type} == "lv"} {
                set s "${var}\(${type}\)\t"
            } else {
                set s "${var}\(${type}\)\t${cls}"
            }

            lappend res "${s}\t${refart}\t${line}\t${file}"
        }
        set tabsize 4
        Tree ${frm}.sel -width 40 -height ${len} -selectmode browse\
          -font $sn_options(def,xref-font) -borderwidth 2 -takefocus 1\
          -relief sunken -bestfit 0 -truncate 1 -tabsize ${tabsize} -tabs\
          {160 120 30 70 200} -labels {Name Class R Line File} -contents ${res}
        ${frm}.sel treebind <ButtonRelease-1> "+${this} edit_location %W %y"

        frame ${frm}.resize -width 10 -height 10 -cursor crosshair\
          -relief raised -bd 2 -background ${bg}

        #the widgets drawn in the canvas haven't the default background
        #set the background manually
        set ifr ${frm}.sel
        foreach f [list ${ifr}.x ${ifr}.y ${ifr}.filter ${ifr}.filter.label\
          ${ifr}.size] {
            ${f} config -background ${bg}
        }
        #set background for the buttons in the columns
        for {set i 0} {${i} <= ${tabsize}} {incr i} {
            ${ifr}.size.btn${i} configure -background ${bg}
        }

        pack ${frm}.resize -side bottom -anchor se -pady 2
        pack ${frm}.sel -fill both -expand y

        set idw [${can} create window -1000 -1000 -window ${frm} -anchor nw\
          -tag "${tagname}"]

        bind ${frm}.resize <Button1-Motion> "[info class]::resize ${frm}\
          ${can} %X %Y ${idw}"

        bind ${frm}.resize <ButtonRelease-1> "${this} graph_new_layout"

        if {${ref} == "to"} {
            set arrow "last"
        } else {
            set arrow "first"
        }

        set ide [${can} create edge -1 -1 -1 -1 -from ${id} -to ${idw}\
          -tag ${tagname} -fill $sn_options(def,xref-list-branch-fg)\
          -arrow ${arrow}]

        update
        update idletasks
        graph_new_layout 1

        see_item ${idw}
    }

    method Title {{full 1}} {
        global sn_options
        set t [get_indep String PafCrossRef]
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
        return [sn_view_icon [get_indep String PafCrossRef] ${base_root}]
    }
    method SetTitle {} {
        ${topw} configure -title [Title] -iconname [Icon]
    }

    # The filter method is invoked by the multiview class when
    # one of the raido buttons in the symbol combo is clicked

    method filter {{all 0}} {
        fill ${all}
    }

    # Invoked when symbol combo drop down is activated

    method postcommand {m} {
        filter 1
    }

    method activate {} {
        if {$itk_option(-menu) != "" && [winfo exists $itk_option(-menu)]} {
            set mn [string range $itk_option(-menu) 0 [expr [string last "." $itk_option(-menu)] - 1]]
            ${mn} entryconfig [get_indep String MultiXRef] -state normal
        }
        if {$itk_option(-toolbar) != ""} {
            pack $itk_option(-toolbar).xref -side left
        }

        #save tab table for the combo box
        if {$itk_option(-symbols) != ""} {
            set old_tabs [[$itk_option(-symbols) component treew] cget -tabs]

            #Add tabulators for the readed values
            [$itk_option(-symbols) component treew] configure -tabs [list 250 150 300]

            $itk_option(-symbols) configure -entrytext ${base_root}
        }
    }

    # This method is invoked when the user reactivates
    # xref support in the project prefs.

    method Reenable {} {
    }

    method deactivate {} {
        if {$itk_option(-menu) != "" && [winfo exists $itk_option(-menu)]} {
            set mn [string range $itk_option(-menu) 0 [expr [string last "." $itk_option(-menu)] - 1]]
            ${mn} entryconfig [get_indep String MultiXRef] -state disabled
        }
        if {$itk_option(-toolbar) != ""} {
            pack forget $itk_option(-toolbar).xref
        }

        #restore old tab table for the combo box
        if {$itk_option(-symbols) != ""} {
            [$itk_option(-symbols) component treew] configure -tabs ${old_tabs}
        }
    }

    method goto {combo sym} {
        if {${combo} == "" || ${sym} == ""} {
            return
        }

        list_selection_to_root ${sym} "${this} recall_make_selection_to_root"
    }

    method Selection {{id ""}} {
        if {${id} == "" || ![info exists xinfos(${id})]} {
            set id [${can} select item]
            if {${id} == "" || ![info exists xinfos(${id})]} {
                return ""
            }
        }

        #get range and file name of the item name
        if {[lindex $xinfos(${id}) ${range1_from}] == ""} {
            set line [get_item_range "" ${id} -1]
            if {${line} != ""} {
                bind_text_for_balloon ${id} rebind
            }
        }

        set cls [lindex $xinfos(${id}) ${class1_pos}]
        if {${cls} == "#"} {
            set cls ""
        }

        #convert xref information to readable line
        return [list [lindex $xinfos(${id}) ${what1_pos}]\
          [lindex $xinfos(${id}) ${item1_pos}] ${cls} [lindex $xinfos(${id})\
          ${file1_pos}] [lindex $xinfos(${id}) ${range1_from}]\
          [lindex $xinfos(${id}) ${type1_pos}] [lindex $xinfos(${id})\
          ${param1_pos}] [lindex $xinfos(${id}) ${range1_to}]]
    }

    method gotosymbolCB {w {target ""} {client_data ""}} {
        set what [lindex ${client_data} 0]
        if {${what} != ""} {
            if {${w} != ""} {
                if {[catch {set sel [${w} curselection]}] || ${sel} == ""} {
                    return
                }
                set target [${w} get [lindex ${sel} 0]]
                set data [${w} itemcget [lindex ${sel} 0] -data]
            } else {
                set data ""
            }
            #on this position we have all the information what we need
            #about the object to view it's references
            set target [string trim ${target}]
            set pars [split ${target} "\t"]
            set file [lindex ${pars} 4]

            set trg [join [lrange ${pars} 0 1] "\t"]
            set sym [sn_get_symbol_and_scope ${trg}]
            set type [lindex ${sym} 1]
            set name [lindex ${sym} 0]

            if {${type} == "" || ${name} == ""} {
                return
            }

            #dump the current view into the history stack
            ${topw} history_stack_add_point ${this}

            references ${what} "x" ${target} ${data}
        } else {
            if {${target} != ""} {
                #dump the current view into the history stack
                ${topw} history_stack_add_point ${this}

                clear
                fill -2
                references both "x" ${target} ""
            }
        }
    }

    method gotosymbol {{scope ""} {sym ""} {cls ""} {file ""} {from ""} {type\
      ""} {prm ""} {to ""} {always 1} {what both}} {
        global tkeWinNumber

        if {${sym} == "" && ${cls} == ""} {
            return 0
        }

        #sometimes we get a sym as "{{}}"
        if {${sym} != ""} {
            if {[catch {set sym [eval join ${sym}]}]} {
                return 0
            }
        }
        regsub -all "\[ \t\n\]+" ${what} { } what
        regsub -all "\[ \t\n\]+" ${type} { } type
        set what [string trim ${what}]
        set sym [string trim ${sym}]
        set type [string trim ${type}]

        if {${cls} != ""} {
            set sym "${cls}\:\:${sym}"
        }

        #verify if there is a \t in the string, that's meen
        #the function is called by the symbol browser with file names
        #by cross reference we can't accept the file name to search for
        #a symbol.
        if {[string first "\t" ${sym}] != -1 || [string first " " ${sym}] !=\
          -1} {
            if {[string first "\t" ${sym}] != -1} {
                set sp "\t"
            } else {
                set sp " "
            }
            set lst [split [join [list ${sym}]] ${sp}]
            switch ${scope} {
                "md" -
                "mi" -
                "iv" {
                        #member with class name.
                        set sym "[lindex ${lst} 1]\:\:[lindex ${lst} 0]"
                    }
                default {
                        #only the first word
                        set sym [lindex ${lst} 0]
                    }
            }
        }
        regsub -all "\[ \t\n\]+" ${sym} { } sym
        if {${from} == ""} {
            set from -1
        }
        if {${to} == ""} {
            set to -1
        }

        #nothing is specified, call window only
        if {"${sym}${file}${prm}" == "" && ${from} == -1 && ${to} == -1} {
            gotosymbolCB ""
            return 0
        }
        #verify how many entries are matching the symbol
        set data [list ${what} ${sym} ${scope}]
        if {${scope} == "" || ${scope} == "f"} {
            set scope "all"
        }

        #verify if there is a matching string to view it's references
        incr tkeWinNumber
# These windows are being leaked, we need to be sure that they are kept track of! itk_component add ?
        set win retrcross-${tkeWinNumber}
        Retriever& ${win} -pattern ${sym} -what ${scope} -merge {{md mi}\
          {fd fu} {fr fu}} -file ${file} -from ${from} -to ${to} -param ${prm}\
          -window_prefix retrcross -titlename [get_indep String\
          CrossRetriever] -client_data ${data} -client_func\
          "${this} gotosymbolCB" -bell 0
        set ret [${win} return_status]

        if {${ret} == 0 && [lsearch -exact {fd fr md} ${scope}] != -1} {
            itcl::delete object ${win}
            incr tkeWinNumber
            set win retrcross-${tkeWinNumber}
            Retriever& ${win} -pattern ${sym} -what ${scope} -merge {{md mi}\
              {fd fu} {fr fu}} -param ${prm} -window_prefix retrcross\
              -titlename [get_indep String CrossRetriever]\
              -client_data ${data} -client_func "${this} gotosymbolCB" -bell 0\
              -warning 1
            set ret [${win} return_status]
        }

        #if no entries found by merging declaration and implemenation
        #try to find a declaration.
        if {${ret} == 0} {
            itcl::delete object ${win}
            incr tkeWinNumber
            set win retrcross-${tkeWinNumber}
            Retriever& ${win} -pattern ${sym} -what all -file ${file}\
              -from ${from} -to ${to} -window_prefix retrcross\
              -titlename [get_indep String CrossRetriever]\
              -client_data ${data} -client_func "${this} gotosymbolCB" -bell 0\
              -warning 1
            set ret [${win} return_status]
        }

        #if no entries found, try to find any declaration in all files
        if {${ret} == 0 &&(${scope} == "mi" || ${scope} == "md")} {
            itcl::delete object ${win}
            incr tkeWinNumber
            set win retrcross-${tkeWinNumber}
            Retriever& ${win} -pattern ${sym} -what "md"\
              -window_prefix retrcross -titlename [get_indep String\
              CrossRetriever] -client_data ${data} -client_func\
              "${this} gotosymbolCB" -bell 0 -warning 1
            set ret [${win} return_status]
        }

        if {${ret} == 0} {
            itcl::delete object ${win}
            if {${sym} != ""} {
                if {[string first "\:\:" ${sym}] != -1} {
                    set sym [split ${sym} "\:\:"]
                    set sym "[lindex ${sym} 1](ud)\t[lindex ${sym} 0]"
                } else {
                    set sym "${sym}(ud)"
                }
            }
            #open only the cross reference window and do nothing
            gotosymbolCB "" ${sym}
        }
        catch {mark_item ${root}}

        return 1
    }

    method clearselection {} {
        ${can} select clear
    }

    method Refresh_Display {} {
        redisplay_root
    }

    method Update_Layout {} {
        global sn_options

# FIXME : remove global variable hacks!
        global ${this}-layoutstyle
        global ${this}-order

        ${can} configure -selectforeground $sn_options(def,select-fg)\
          -selectbackground $sn_options(def,select-bg)

        ${can} itemconfigure edge_to -fill $sn_options(def,canv-line-fg)
        ${can} itemconfigure edge_by -fill $sn_options(def,xref-used-by-fg)

        set ${this}-layoutstyle $sn_options(def,xref-layout)
        set ${this}-order $sn_options(def,xref-disp-order)

        #more options from preferences
        global ${this}-accept_param
        set ${this}-accept_param $sn_options(both,xref-accept-param)
        set old_accept_param ${accept_param}
        set accept_param $sn_options(both,xref-accept-param)

        global ${this}-accept_static
        set ${this}-accept_static $sn_options(both,xref-accept-static)
        set old_accept_static ${accept_static}
        set accept_static $sn_options(both,xref-accept-static)

        global ${this}-disp_param
        set ${this}-disp_param $sn_options(both,xref-disp-param)
        set old_disp_param ${disp_param}
        set disp_param $sn_options(both,xref-disp-param)

        global ${this}-draw_rectangles
        set ${this}-draw_rectangles $sn_options(both,xref-draw-rect)
        set old_draw_rectangles ${draw_rectangles}
        set draw_rectangles $sn_options(both,xref-draw-rect)

        if {${old_accept_param} != ${accept_param} || ${old_accept_static} !=\
          ${accept_static}} {

            redisplay_root

        }\
        elseif {${old_draw_rectangles} != ${draw_rectangles} ||\
          ${old_disp_param} != ${disp_param}} {

            graph_new_layout 1

        }
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
        set scp [lindex ${dump} 0]
        set sym [lindex ${dump} 1]
        set cls [lindex ${dump} 2]
        set name [string trim "${sym}(${scp}) ${cls}"]

        return "XRef ${name}"
    }

    method AddHistoryFromDump {dump title} {
        set what [lindex ${dump} 0]
        set key [lindex ${dump} 1]
        set class [lindex ${dump} 2]
        sn_add_history xref [list ${dump}] [sn_make_history_title xref ${what}\
          "${class} ${key}"] sn_xref_gotosymbol
    }

    #return the important data to restore this widget
    #in later time again (used by saving the project)
    method Dump {} {
        set id [${can} find withtag %root%]
        if {${id} == ""} {
            return ""
        }
        return [Selection ${id}]
    }

    #gets the result from the function "Dump" to
    #restore the older state (used by restoring the project)
    method Restore {str} {
        eval ${this} gotosymbol ${str}
        #Focus
    }

    method Close {{mode 0}} {
        if {${this_fetching}} {
            sn_error_dialog [get_indep String CannotCloseWindow] [Title 0]
            #we can't close the view, during a db-fetch is active
            return 0
        } else {
            return 1
        }
    }

    method whoami {} {
        return xref
    }
    method next {} {
        return $itk_option(-next)
    }
    method can {} {
        return ${can}
    }

    # Used for cache symbol drop down results.
    private variable lastupdate "Out of date"
    private variable xref_sym_cache ""

    protected variable can ""
    protected variable topw
    protected variable base_root ""
    protected variable listprefix ""
    protected variable print_dialog ""
    #array to store informations about the drawn objects
    protected variable xinfos
    protected variable exp ""

    protected variable item1_pos 0
    protected variable class1_pos 1
    protected variable what1_pos 2
    protected variable param1_pos 3
    protected variable type1_pos 4

    protected variable file_pos 5
    protected variable file_line_pos 6

    protected variable file1_pos 7
    protected variable range1_from 8
    protected variable range1_to 9

    protected variable flags_pos 10
    protected variable refart_pos 11

    protected variable line_arg_count 12

    protected variable static_flag 0x8

    protected variable initial_line
    protected variable root ""
    protected variable old_tabs ""

    common prefwin ".set_cross_ref"
    common pre_sel_methods
    common scopes_with_ref_to {fu fd mi md fr su}
    common scopes_without_ref_to {cl con ec e iv ma t un gv lv}
    common scopes_with_parameters {fu mi fd md fr su}
    common types_with_classes {mi md fr iv}
    common class_scopes {cl}
    common cross_shown_scopes ""
    common cross_ref_access ""
    common cross_scopes
    common DEBUG 0

    public variable draw_rectangles 0
    public variable accept_param 0
    public variable accept_static 0
    public variable disp_param 0

    public variable goto ""

    public variable width 640
    public variable height 480
}

proc have_xref {} {
    #no cross reference availiable?
    if {[info commands paf_db_to] == "" || [info commands paf_db_by] == ""} {
        return 0
    }
    return 1
}


#convert names, like 'strcpy(fu)' to 'strcpy' and 'fu'
proc get_key_and_scope {item key scp} {
    upvar ${key} k
    upvar ${scp} s
    set lk [split ${item} "("]
    set i [string first "()" ${item}]
    if {${i} != -1} {
        set k [string range ${item} 0 [expr ${i} + 1]]
    } else {
        set k [lindex ${lk} 0]
    }
    set j [string first "(" ${item}]
    if {${j} != -1} {
        set s [lindex ${lk} end]
        set s [lindex [split ${s} ")"] 0]
    } else {
        set s ""
    }
}

proc xref_termometer_files {op files {factor 1}} {
    global xref_termometer
    if {![info exists xref_termometer(numfiles)]} {
        xref_termometer_init
    }
    if {${op} == "set"} {
        set xref_termometer(numfiles) 0
        set xref_termometer(files) ""
    }
    eval lappend xref_termometer(files) ${files}
    incr xref_termometer(numfiles) [expr {[llength ${files}] * ${factor}}]
}

proc xref_termometer_init {} {
    global sn_options
    global xref_termometer
    global xref_cancelled
    set xref_termometer(del_index) 0
    set xref_termometer(scann_index) 0
    set xref_termometer(numfiles) 1
    set xref_termometer(files) ""
    set xref_termometer(lastfile) ""
    set xref_cancelled 0
}

proc xref_cancel {} {
    set answer [tk_dialog auto [get_indep String XrefGeneration]\
      [get_indep String CancelXref] question_image 0 [get_indep String Ok]\
      [get_indep String Cancel]]

    #be sure that xref is still running
    if {${answer} == 0 && [sn_processes_running]} {
        global xref_cancelled
        incr xref_cancelled
    }
}

proc xref_disp_tmeter {txt value} {
    global xref_termometer
    global xref_termometer_progress_value

    #display xref status on the toolbar of the availiable windows
    set fnd 0
    foreach cls {Project& SymBr& MultiWindow&} {
        foreach obj [itcl::find objects -class ${cls}] {
            #actually it's not possible, that obj.msg doesn't
            #exist, but could happen because of synchronizing
            if {![winfo exists ${obj}.msg]} {
                continue
            }
            set frm ${obj}.msg
            if {![winfo exists ${frm}.termometer]} {
                catch {set xref_termometer_progress_value 0}
                ProgressBar ${frm}.termometer -labeltext ${txt}\
                  -maxvalue $xref_termometer(numfiles)\
                  -variable xref_termometer_progress_value
                ${frm}.termometer balloon [get_indep String TermoGenerateXRef]
                bind ${frm}.termometer <1> "xref_cancel"
                pack ${frm}.termometer -side right
            } else {
                ${frm}.termometer configure -labeltext ${txt}
            }
            set fnd 1
        }

        #if a project window is availiable, don't display the
        #status in other windows. except Xref-generation,
        #this must be displayed in all windows
        if {${fnd} && ${txt} != "xref" && ${cls} == "Project&"} {
            break
        }
    }
    if {[info exists xref_termometer_progress_value]} {
        set xref_termometer_progress_value ${value}
    }

    #enable Xref-cancel button to cancel the xref-gen
    foreach obj [itcl::find objects -class XRef&] {
        ${obj} enable_cancel normal
    }
}

#display status of xref processing
proc xref_termometer_disp {file is_delete} {
    global sn_options
    global xref_termometer

    if {[sn_batch_mode]} {
        puts stdout ${line}
        return
    }

    if {![info exists xref_termometer(numfiles)]} {
        xref_termometer_init
    }

    #store the last accessed file for bug reporting
    set xref_termometer(lastfile) $file

    if {$is_delete} {
        incr xref_termometer(del_index)
        upvar #0 xref_termometer(del_index) counter
        set txt "Deleting"
    } else {
        incr xref_termometer(scann_index)
        upvar #0 xref_termometer(scann_index) counter
        set txt "xref"
    }

    if {$xref_termometer(numfiles) <= 0} {
        set xref_termometer(numfiles) 1
    }

    #look at the index of the current file position, this is more
    #exactly because some files (especially header files) don't
    #have xred and will be not scanned.
    set i [lsearch -exact $xref_termometer(files) $xref_termometer(lastfile)]
    if {${i} != -1 && ${txt} != "Deleting"} {
        xref_disp_tmeter ${txt} ${i}
    } else {
        xref_disp_tmeter ${txt} ${counter}
    }
}

#delete all status termometers of xref processing
proc xref_delete_termometers {} {
    global xref_termometer
    global xref_cancelled
    global xref_termometer_progress_value

    #make sure that all progress bars are deleted. This
    #insures that the trace script bound on xref_termometer_progress_value
    #is deleted.
    foreach obj [itcl::find objects -class ProgressBar] {
        if {[string match "*.msg.termometer" ${obj}]} {
            itcl::delete object $obj
# FIXME: delete and then destroy? This seems broken
            catch {destroy ${obj}}
        }
    }

    #enable Xref-cancel button to cancel the xref-gen
    foreach obj [itcl::find objects -class XRef&] {
# FIXME: this catch is very bad, what if the method is renamed?
        catch {${obj} enable_cancel disabled}
    }

    #unset variables
    catch {unset xref_termometer}
    catch {set xref_termometer_progress_value 0}
    set xref_cancelled 0
}

#gets a string formatted to be compatible to gotosymbol command
proc sn_xref_gotosymbol {str} {
    global tkeWinNumber
    global Switch_Is_Enabled
    incr Switch_Is_Enabled -1

    set s [MultiWindow&::find_Reusable]
    if {${s} == ""} {
        set new 1
        incr tkeWinNumber
        set s ".multiwindow-${tkeWinNumber}"
        MultiWindow& ${s} -raise xref
    } else {
        ${s} view xref
    }

    eval [${s} ActiveWidget] gotosymbol ${str}

    incr Switch_Is_Enabled
}

#sym must have the format 'symbol(scope) class'
proc sn_xref {{what "both"} {sym ""} {type ""} {file ""} {from -1} {to\
  -1} {param ""}} {
    if {! [have_xref]} {
        bell
        return
    }
    get_key_and_scope ${sym} sym scope
    sn_xref_gotosymbol [list ${scope} ${sym} "" ${file} ${from} "" ${param}\
      ${to}]
}

