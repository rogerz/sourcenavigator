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
################################################
##
## Window for project symbols
##
################################################
itcl_class SymBr& {
    inherit sourcenav::Window

    constructor {{config ""}} {
        global sn_options

        set topw $itk_component(hull)
        set symbr $itk_component(hull).symbr

        on_close "$itk_component(hull) windows_close dummy"
        #don't close window by hitting Escape
      #  ${this} bind_tk <Escape> {;}

        withdraw

        if {${x} != ""} {
            $this configure -geometry [expr {int(${width})}]x[expr\
              {int(${height})}]+${x}+${y}
        } else {
            set height [expr {int([winfo screenheight .] *
                ($sn_options(def,window-size)*0.01))}]
            set width [expr {int([winfo screenwidth .] / 3)}]
            if {${width} < 300} {
                set width 300
            } elseif {${width} > 450} {
                set width 450
            }
            $this configure -geometry ${width}x${height}
        }

        set Toolbar $itk_component(hull).exp
        set Statusbar $itk_component(hull).msg

        AddMenu
        AddToolbar
        AddStatusbar

        $itk_component(hull).menu configure -font $sn_options(def,layout-font)

        Retr& ${symbr} \
            -symbols "" \
            -symbols_filter "" \
            -menu "$itk_component(hull).menu" \
            -toolbar "${Toolbar}" \
            -mesg_area "" \
            -height ${height} \
            -width ${width} \
            -mode symbr \
            -restore ${restore} \
            -selectcommand "${this} handle_select" \
	    -parent ${this}
        pack ${symbr} -expand y -fill both -side left

        ${symbr} SetTitle

        after idle "window_configure $itk_component(hull) deiconify [${symbr}.list tree]"

        #call user function
        catch {sn_rc_symbolbrowser $itk_component(hull)  $itk_component(hull).menu}
    }
    destructor {
        foreach v [::info globals "${this}-*"] {
            catch {uplevel #0 unset ${v}}
        }
    }
    method config {config} {
    }
    method AddToolbar {} {
        set exp ${Toolbar}
        frame ${Toolbar} -relief groove -border 2
        pack ${Toolbar} -side top -fill x

        #Hierarchy
        button ${exp}.tree -takefocus 0 -image tree_image -command "${this} run_ctree" -state disabled
        bind_history ${exp}.tree tree
        balloon_bind_info ${exp}.tree [get_indep String INFOHierarchy]
        pack ${exp}.tree -side left

        #Class
        button ${exp}.class -takefocus 0 -image watch_image -command "${this} run_class" -state disabled
        bind_history ${exp}.class browse
        balloon_bind_info ${exp}.class [get_indep String INFOStartClassBrw]
        pack ${exp}.class -side left

        #Xref
        button ${exp}.xref -takefocus 0 -image crossref_image -command\
          " ${this} run_xref " -state disabled
        bind_history ${exp}.xref xref
        balloon_bind_info ${exp}.xref [get_indep String StartXRefINFO]
        pack ${exp}.xref -side left

        #Include
        button ${exp}.inc -takefocus 0 -image include_image -command "${this} run_include" -state disabled
        balloon_bind_info ${exp}.inc [get_indep String IncludeLaunch]
        pack ${exp}.inc -side left

        pack [frame ${exp}.space -width 7] -side left
    }
    method AddStatusbar {} {
        global sn_options
        frame ${Statusbar}
        pack [label ${Statusbar}.msg -font $sn_options(def,layout-font)\
          -relief groove -bd 2 -anchor w -textvar ${this}.msg] -expand y\
          -fill both -side left
        pack ${Statusbar} -side bottom -fill x
    }

    method handle_select {{scope ""} {sym ""} {cls ""} {file ""} {from ""}\
      {type ""} {prm ""} {to ""} {always 1}} {
        if {${scope} == "cl"} {
            set cls ${sym}
        }
        if {${cls} != "" && ${scope} != "cov"} {
            set class_state normal
        } else {
            set class_state disabled
        }
        if {${file} != ""} {
            set inc_state [tool_Exists incbr]
        } else {
            set inc_state disabled
        }
        set xref_state [tool_Exists xref]
        if {${sym} == "" && ${xref_state} == "normal"} {
            set xref_state disabled
        }
        ${Toolbar}.tree config -state ${class_state}
        ${Toolbar}.class config -state ${class_state}
        ${Toolbar}.inc config -state ${inc_state}
        ${Toolbar}.xref config -state ${xref_state}
    }

    method run_tool {tool {scope ""} {sym ""} {cls ""} {file ""} {from ""}\
      {type ""} {prm ""} {to ""} {always 1}} {
        switch -- ${tool} {
            "inc" {
                    sn_include ${file}
                }
            "xref" {
                    if {${from} == ""} {
                        set from -1
                    }
                    if {${to} == ""} {
                        set to -1
                    }
                    if {${cls} != ""} {
                        set sym "${cls}\:\:${sym}(${scope})"
                    } else {
                        set sym ${sym}(${scope})
                    }
                    sn_xref both [string trim ${sym}] ${type} ${file} ${from}\
                      ${to} ${prm}
                }
            "class" {
                    #go direct to the member if something given
                    sn_classbrowser "" [list ${scope} ${sym} ${cls} ${file}\
                      ${from} ${type} ${prm} ${to}]
                }
            "ctree" {
                    if {${scope} == "cl"} {
                        set cls ${sym}
                    }
                    sn_classtree ${cls}
                }
            default {
                    bell
                }
        }
    }

    method run_ctree {} {
        eval run_tool ctree [${symbr} Selection]
    }
    method run_class {} {
        eval run_tool class [${symbr} Selection]
    }

    method run_include {} {
        eval run_tool inc [${symbr} Selection]
    }

    method run_xref {} {
        eval run_tool xref [${symbr} Selection]
    }

    #close window, if it is not the last window
    #exit project, if it is the last window
    method windows_close {m} {
        if {[MultiWindow&::num_Existing_windows] > 1} {
            if {[Close]} {
                #close window
                itcl::delete object ${this}
            }
        } else {
            #exit the project
            MultiWindow&::file_close_project
        }
    }
    method edit_post {m} {
    }
    method tools_post {m} {
    }
    method AddMenu {} {
        global sn_options sn_scopes
        set m  $itk_component(hull).menu

        set state normal

        menu ${m} -tearoff 0
        ## File menu
        ####################
        menu ${m}.file -tearoff 0 -postcommand\
          "ProjectMenuEntries_post ${m}.file"
        ${m} add cascade -label [get_indep String EditFile] -menu ${m}.file\
          -underline [get_indep Pos EditFile]

        ${m}.file configure -font $sn_options(def,layout-font)

        #project menu entries
        AddProjectMenuEntries ${m}.file  $itk_component(hull)
        ${m}.file add separator

        #close window
        ${m}.file add command -command " ${this} windows_close ${m}.file "\
          -label [get_indep String WindowsClose] -underline [get_indep Pos\
          WindowsClose] -accelerator "Ctrl+W"

        ${m}.file add command -label [get_indep String Exit]\
          -underline [get_indep Pos Exit] -command " sn_exit "\
          -accelerator "Ctrl+Q"

        ## View menu
        ############
        menu ${m}.edit -tearoff 0 -postcommand "${this} edit_post ${m}.edit"
        ${m}.edit config -font $sn_options(def,layout-font)

        ${m} add cascade -label [get_indep String View] -menu ${m}.edit\
          -underline [get_indep Pos View]

        #add the availiable scopes to the view menu
        Add_View_Entries

        ## Tools menu column
        menu ${m}.tools -tearoff 0 -postcommand "${this} tools_post ${m}.tools"

        ${m}.tools config -font $sn_options(def,layout-font)

        ${m} add cascade -label [get_indep String Tools] -menu ${m}.tools\
          -underline [get_indep Pos Tools]

        ## misc. services
        AddMiscSubMenu ${m}.tools

        ## History
        AddHistMenu ${m}

        ##Windows
        AddWindowsMenu ${m} $itk_component(hull)

        ##Help
        AddHelpMenu ${m} $itk_component(hull)

        $itk_component(hull) configure -menu ${m}

        #tkwait visibility $this
        raise
    }

    #######################
    ## View menu Entries ##
    #######################
    method Add_View_Entries {} {
        global sn_scopes

        set m ${this}.menu

        #delete old entries
        ${m}.edit delete 0 end

        #exclusive mode
        ${m}.edit add checkbutton -label [get_indep String ExclusiveMode]\
          -underline [get_indep Pos ExclusiveMode]\
          -variable ${symbr}-exclusive -onvalue 1 -offvalue 0 -command\
          " ${symbr} Exclusive_Mode {} {} " -state normal
        ${m}.edit add separator

        #Add filter options into edit menu
        foreach sc ${sn_scopes} {
            set s [convert_scope_to_str ${sc}]
            set i [convert_scope_to_num ${sc}]
            if {[::info commands paf_db_${sc}] != ""} {
                set state normal
            } else {
                set state disabled
            }
            ${m}.edit add checkbutton -label ${s} -underline ${i}\
              -variable ${symbr}-sc-${sc} -onvalue ${sc} -offvalue ""\
              -command " ${symbr} ExecFilter ${sc} exec " -state ${state}
        }
        ${m}.edit add checkbutton -label [get_indep String Files]\
          -underline [get_indep Pos Files] -variable ${symbr}-sc-files\
          -onvalue files -offvalue "" -command " ${symbr} ExecFilter files\
          exec " -state normal
    }

    method Update_View_Entries {} {
        global sn_scopes
        set m ${this}.menu.edit

        foreach sc ${sn_scopes} {
            set s [convert_scope_to_str ${sc}]
            set i [convert_scope_to_num ${sc}]
            if {[::info commands paf_db_${sc}] != ""} {
                set state normal
            } else {
                set state disabled
            }
            if {[catch {${m} entryconfig ${s} -state ${state}}]} {
                #failed scope, recreate the hole menu entries
                Add_View_Entries
                return
            }
        }

        if {[::info commands paf_db_f] != ""} {
            set state normal
        } else {
            set state disabled
        }
        ${m} entryconfig [get_indep String Files] -state ${state}
    }

    method Refresh_Display {} {
        #update the menu entries
        Update_View_Entries

        #update the toolbar icons
        eval handle_select [${symbr} Selection]
    }

    method Dump {} {
        return [${symbr} Dump]
    }

    method Restore {str} {
        if {${str} == ""} {
            return
        }
        ${symbr} Restore ${str}
    }

    #verify if all buffers can be refereted.
    proc CloseAll {} {
        foreach win [itcl_info objects "*" -class SymBr&] {
            if {[${win} Close] == 0} {
                return 0
            }
        }
        return 1
    }
    method Close {} {
        #ask Retriever if window can be closed
        return [${symbr} Close]
    }

    method symbr {} {
        return ${symbr}
    }

    #This function saves the status of all opened sym. br.
    #in the project file.
    proc SaveYourSelf {} {
        sn_log "SymBr& SaveYourSelf..."

        set val ""
        foreach win [itcl_info objects "*" -class SymBr&] {
            #window id
            set wdump "symbr"

            #window geom
            lappend wdump [winfo x [${win} component hull]]
            lappend wdump [winfo y [${win} component hull]]
            lappend wdump [winfo width [${win} component hull]]
            lappend wdump [winfo height [${win} component hull]]
            lappend wdump [wm geometry [${win} component hull]]
            lappend wdump [${win} Dump]
            lappend val ${wdump}
        }
        paf_db_proj put SymBr ${val}

        sn_log "SymBr& SaveYourSelf...end"
    }

    #This function restores all the windows saved in the
    #project file
    proc RestoreYourSelf {} {
        global tkeWinNumber

        set val [paf_db_proj get -key SymBr]
        if {${val} != ""} {
            foreach win ${val} {
                #create window
                incr tkeWinNumber
                set nwin ".multisymbr-${tkeWinNumber}"
                #don't exec filter
                SymBr& ${nwin} -x [lindex ${win} 1] -y [lindex ${win} 2]\
                  -width [lindex ${win} 3] -height [lindex ${win} 4] -restore 1
                ${nwin} configure -geometry [lindex ${win} 5]
                #restore old retriever status
                ${nwin} Restore [lindex ${win} 6]
            }
        }
    }

    protected symbr ""
    protected topw "."
    protected Toolbar ""
    protected SymbolsFilter ""
    protected Statusbar ""

    public restore 0
    public x ""
    public y ""
    public width 45
    public height 30
}


