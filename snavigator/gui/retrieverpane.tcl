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
# multiretr.tcl - Retriever widget that can be opened in a subwindow.
# Copyright (C) 1998 Cygnus Solutions.

itcl::class Retr& {
    inherit sourcenav::MultiChild

    constructor args {
        global sn_options
        global sn_scopes

        #FIXME: This next line probably isn't needed
        set class [${this} info class]

        set topw [winfo toplevel $itk_component(hull)]
        set searchbtn $itk_component(hull).pattern.search
        set filterbtn $itk_component(hull).pattern.filter

        global ${this}-pattern

	eval itk_initialize $args
 
        #add menu entries
        if {$itk_option(-menu) != ""} {
        }

        #add toolbar icons
        if {$itk_option(-toolbar) != ""} {
            set retrfr $itk_option(-toolbar).retrfr
            pack [frame ${retrfr}] -side left

            set filterbtn ${retrfr}.filter

            #no prev/next buttons for symbol browser
            if {${mode} != "symbr"} {
                #prev
                button ${retrfr}.prev -takefocus 0 -text [get_indep String\
                  Prev] -image del_image -command " ${this} next_entry -1 "
                balloon_bind_info ${retrfr}.prev [get_indep String PrevINFO]
                pack ${retrfr}.prev -side left
                #next
                button ${retrfr}.next -takefocus 0 -text [get_indep String\
                  Next] -image add_image -command " ${this} next_entry 1 "
                balloon_bind_info ${retrfr}.next [get_indep String NextINFO]
                pack ${retrfr}.next -side left
            }

            #add predefined icons for files/classes/functions
            if {${mode} == "symbr"} {
                button ${retrfr}.class -takefocus 0 -image classes_image\
                  -command "
                        set ${this}-exclusive 1
                        ${this} ExecFilter cl
                    " -state ${cl_state}
                balloon_bind_info ${retrfr}.class [get_indep String INFOClasses]
                pack ${retrfr}.class -side left

                button ${retrfr}.meth -takefocus 0 -image method_image\
                  -command "
                        set ${this}-exclusive 1
                        ${this} ExecFilter md
                    " -state ${md_state}
                balloon_bind_info ${retrfr}.meth [get_indep String INFOMethods]
                pack ${retrfr}.meth -side left

                button ${retrfr}.func -takefocus 0 -image function_image\
                  -command "
                        set ${this}-exclusive 1
                        ${this} ExecFilter fu
                    " -state ${fu_state}
                balloon_bind_info ${retrfr}.func [get_indep String\
                  INFOFunctions]
                pack ${retrfr}.func -side left

                button ${retrfr}.file -takefocus 0 -image files_image -command "
                        set ${this}-exclusive 1
                        ${this} ExecFilter files
                    " -state ${f_state}
                balloon_bind_info ${retrfr}.file [get_indep String INFOFiles]
                pack ${retrfr}.file -side left
            } else {
                #Filter button
                #No filter for symbol browser
                button ${filterbtn} -takefocus 0 -text [get_indep String\
                  RetrieverFilter] -underline [get_indep Pos RetrieverFilter]\
                  -pady 0 -command " ${this} start_filter "
                bind ${filterbtn} <Return> "${searchbtn} invoke; break"
                balloon_bind_info ${filterbtn} [get_indep String\
                  RetrieverFilterINFO]
                pack ${filterbtn} -side left -fill y
            }
        }

        frame $itk_component(hull).pattern
        label $itk_component(hull).pattern.l -text [get_indep String MultiRetrieverPattern]\
          -underline [get_indep Pos MultiRetrieverPattern]
        pack $itk_component(hull).pattern.l -side left -padx 5

        #Entry for pattern
        entry $itk_component(hull).pattern.e -textvariable ${this}-pattern -width 5\
          -exportselection n
        bind $itk_component(hull).pattern.e <Return> "${searchbtn} invoke; break"
        pack $itk_component(hull).pattern.e -side left -fill both -expand y

        #Filter button
        #button $filterbtn  #		-text [get_indep String RetrieverFilter]\
          #		-command "$this start_filter"
        #bind $filterbtn <Return> "$searchbtn invoke; break"
        #balloon_bind_info $filterbtn [get_indep String RetrieverFilterINFO]
        #pack $filterbtn -side left

        #search button
        button ${searchbtn} -text [get_indep String UtilSearch]\
          -underline [get_indep Pos UtilSearch] -command " ${this}\
          start_search "
        balloon_bind_info ${searchbtn} [get_indep String RetrieverSearchINFO]
        pack ${searchbtn} -side left -padx 5

        pack $itk_component(hull).pattern -fill x -pady 4

        #color darkgray = #aaaaaa
        Tree $itk_component(hull).list -fillselection 1 -selectmode browse\
          -exportselection n -font $sn_options(def,default-font) -tabs\
          {200 150 100 250 200} -tabsize 4 -labels [list "[get_indep String\
          Name]" "[get_indep String Class]" "[get_indep String Type]"\
          "[get_indep String Parameters]" "[get_indep String File]"]\
          -bestfit 0 -truncate 1 -indentwidth 20 -filter ""\
          -filterextension "\t*\t*" -fillcommand "display_contents"\
          -height ${height} -width ${width}
        #	-hiddenimage type_cl+_image
        pack $itk_component(hull).list -side bottom -fill both -expand y
        set tree [$itk_component(hull).list tree]

        $itk_component(hull).list treebind <Double-1> "edit_symbol %W"

# FIXME: all these bindings on the tree class are ugly, we need a new tree class.
        #[namespace tail $this].list treebind <ButtonRelease-1> "Retriever& :: expand_classes $this\
          %x %y display_contents"
        $itk_component(hull).list treebind <ButtonRelease-1> "${this} handle_click %x %y"

        $itk_component(hull).list treebind <Return> "
			%W selection clear 0 end
			%W selection set active
			update idletasks
			edit_symbol %W
			break"

        #it has two bindings depended on selectcommand
        $itk_component(hull).list treebind <space> [${this}.list treebind <Return>]


        if {${mode} != "retr" || [catch {set str [string trim\
          [selection get]]}]} {
            set str "*"
        }
        set ${this}-pattern ${str}

        #enable all filer-scopes except files
        InitScopes

        if {${mode} == "symbr"} {
            set Retr_Title [get_indep String MultiBrowser]
            if {!${restore}} {
                after idle "catch {${this} ExecFilter files}"
            }
        }

        control_buttons

        focus $itk_component(hull).pattern.e

        #call user defined function
        catch {sn_rc_retriever $itk_component(hull) $itk_component(hull).list}
    }

    destructor {
        foreach v [::info globals "${this}-*"] {
            catch {uplevel #0 unset ${v}}
        }
    }
    method cget {config} {
    }
    method config {config} {
    }

    #by default, all scopes are enabled except files
    method InitScopes {} {
        global sn_scopes

        foreach sc ${sn_scopes} {
            global ${this}-sc-${sc}
            if {[::info commands paf_db_${sc}] != "" && ${mode} == "retr"} {
                set ${this}-sc-${sc} ${sc}
                set ScopesFilter(${sc}) ${sc}
            } else {
                set ${this}-sc-${sc} ""
                set ScopesFilter(${sc}) ""
            }
        }
        #symbol browser initialized with files and has
        #exclusive mode as default
        global ${this}-sc-files
        if {${mode} == "symbr"} {
            upvar #0 ${this}-exclusive excl
            set excl 1
            set ${this}-sc-files "files"
            set ScopesFilter(files) "files"
        } else {
            set ${this}-sc-files ""
            set ScopesFilter(files) ""
        }
    }

    method RestoreScopes {scopes} {
        global sn_scopes

        foreach sc ${sn_scopes} {
            global ${this}-sc-${sc}
            if {[lsearch -exact ${scopes} ${sc}] != -1 && [::info commands\
              paf_db_${sc}] != ""} {
                set ${this}-sc-${sc} ${sc}
                set ScopesFilter(${sc}) ${sc}
            } else {
                set ${this}-sc-${sc} ""
                set ScopesFilter(${sc}) ""
            }
        }
        #symbol browser initialized with files and has
        #exclusive mode as default
        global ${this}-sc-files
        if {[lsearch -exact ${scopes} files] != -1 && [::info commands\
          paf_db_f] != ""} {
            set ${this}-sc-files "files"
            set ScopesFilter(files) "files"
        } else {
            set ${this}-sc-files ""
            set ScopesFilter(files) ""
        }
    }

    method Exclusive_Mode {chkfr srchbtn} {
        global sn_scopes
        upvar #0 ${this}-exclusive excl
        if {[info exists excl] && ${excl}} {
            if {[winfo exists ${srchbtn}]} {
                ${srchbtn} config -state disabled
                #$chkfr.all config -state disabled
                #$chkfr.none config -state disabled
            }

            foreach sc ${sn_scopes} {
                global ${this}-sc-${sc}
                set ${this}-sc-${sc} ""
            }
            global ${this}-sc-files
            set ${this}-sc-files ""
        } else {
            if {[winfo exists ${srchbtn}]} {
                ${srchbtn} config -state normal
                #$chkfr.all config -state normal
                #$chkfr.none config -state normal
            }
        }
    }

    method ExecFilter {scope {exec ""}} {
        global sn_scopes
        upvar #0 ${this}-exclusive excl

        if {! ${excl}} {
            if {${exec} == "exec"} {
                start_search
            }
            return
        }

        foreach sc ${sn_scopes} {
            global ${this}-sc-${sc}
            if {${sc} != ${scope}} {
                set ${this}-sc-${sc} ""
            } else {
                set ${this}-sc-${sc} ${sc}
            }
        }
        global ${this}-sc-files
        if {${scope} != "files"} {
            set ${this}-sc-files ""
        } else {
            set ${this}-sc-files files
        }

        #execute search every time the filter is changed
        start_search
    }

    method DeleteFilter {t} {
        global sn_scopes
        foreach sc ${sn_scopes} {
            upvar #0 ${this}-sc-${sc} scope
            if {[info exists scope]} {
                set ScopesFilter(${sc}) ${scope}
            }
        }
        upvar #0 ${this}-sc-files files
        if {[info exists files]} {
            set ScopesFilter(files) ${files}
        }
        itcl::delete object ${t}
    }
    method start_filter {} {
        global sn_options
        global sn_scopes

        set t [namespace tail ${this}-mixer]
        if {[winfo exists ${t}]} {
            ${t} raise
            return
        }

        upvar #0 ${this}-exclusive excl
        if {[info exists excl] && ${excl}} {
            set btnstate disabled
        } else {
            set btnstate normal
        }

        sourcenav::Window ${t}
        ${t} on_close "${t}.button_0 invoke"
        ${t} withdraw
        set title [string trimright [get_indep String Mixer] "."]
        ${t} configure -title ${title}
        ${t} configure -iconname ${title}

        sn_motif_buttons ${t} bottom 0 [get_indep String UtilSearch]\
          [get_indep String Close]
        ${t}.button_0 config -state ${btnstate} -command " ${this}\
          start_search "
        ${t}.button_1 config -command " ${this} DeleteFilter ${t} "

        frame ${t}.chk

        #Buttons ALL,NONE,AND
        set chkfr ${t}.chk.btns
        frame ${chkfr}
        pack ${chkfr} -fill x -expand y -pady 10

        button ${chkfr}.all -text [get_indep String All] -command " ${this}\
          handle_all_none ${this}-sc 1 "
        balloon_bind_info ${chkfr}.all [get_indep String ClassAllINFO]
        button ${chkfr}.none -text [get_indep String None] -command " ${this}\
          handle_all_none ${this}-sc 0 "
        balloon_bind_info ${chkfr}.none [get_indep String ClassNoneINFO]

        #exclusive mode (only one categorie)
        checkbutton ${chkfr}.excl -text [get_indep String ExclusiveMode]\
          -underline [get_indep Pos ExclusiveMode] -variable ${this}-exclusive\
          -onvalue 1 -offvalue 0 -command " ${this} Exclusive_Mode ${chkfr}\
          ${t}.button_0 " -state normal
        balloon_bind_info ${chkfr}.excl [get_indep String ExclusiveModeINFO]

        ::grid ${chkfr}.all ${chkfr}.none ${chkfr}.excl -padx 10 -pady 10\
          -sticky w

        set widgets ""
        foreach sc ${sn_scopes} {
            set s [convert_scope_to_str ${sc}]
            set i [convert_scope_to_num ${sc}]
            global ${this}-sc-${sc}
            set ${this}-sc-${sc} ${sc}
            if {[::info commands paf_db_${sc}] != ""} {
                if {![info exists ScopesFilter(${sc})]} {
                    set ${this}-sc-${sc} ${sc}
                } else {
                    set ${this}-sc-${sc} $ScopesFilter(${sc})
                }
                set state normal
            } else {
                set ${this}-sc-${sc} ""
                set state disabled
            }
            checkbutton ${chkfr}.${sc} -text ${s} -underline ${i}\
              -variable ${this}-sc-${sc} -onvalue ${sc} -offvalue ""\
              -command " ${this} ExecFilter ${sc} " -state ${state}
            lappend widgets ${chkfr}.${sc}
        }
        global ${this}-sc-files
        if {![info exists ScopesFilter(files)]} {
            set ${this}-sc-files ""
        } else {
            set ${this}-sc-files $ScopesFilter(files)
        }
        checkbutton ${chkfr}.files -text [get_indep String Files]\
          -underline [get_indep Pos Files] -variable ${this}-sc-files\
          -onvalue files -offvalue "" -command " ${this} ExecFilter files "\
          -state normal
        lappend widgets ${chkfr}.files

        set cnt [llength ${widgets}]
        for {set i 0} {${i} < ${cnt}} {incr i 3} {
            ::eval grid [lrange ${widgets} ${i} [expr ${i} + 2]] -padx 10\
              -sticky w
        }
        pack ${t}.chk -anchor w -padx 10 -pady 10

        ${t} move_to_mouse
        catch {${t} resizable no no}
        wm deiconify ${t}
        focus ${t}
    }

    protected variable with_class "md mi iv fr cov"
    protected variable with_type "md mi iv fr fd fu con ec su gv t cov files"
    protected variable with_param "fr md mi fr fd fu"
    protected variable SearchActive 0

    method cancel_fetching {} {
        ${searchbtn} config -command "  " -state disabled
        if {[winfo exists ${this}-mixer.button_0]} {
            ${this}-mixer.button_0 config -command "  " -state disabled
        }
        retr_cancel_fetching
    }

    protected variable Cancel_Dialog_Id 0
    method Delete_Cancel_Dialog {} {
        after cancel ${Cancel_Dialog_Id}
        destroy $itk_component(hull).wait_dlg
        update idletasks
    }

    method Display_Cancel_Dialog {} {
        sn_wait_dialog $itk_component(hull) [get_indep String WaitOrCancelRetriever]\
          [Title 0] "${this} cancel_fetching"
        update idletasks
    }

    method start_search {} {
        upvar #0 ${this}-pattern ptrn

        #verify if a fetch is already active
        if {${SearchActive}} {
            bell
            return
        }

        if {${ptrn} == ""} {
            set ptrn "*"
        }
        set pat ${ptrn}

        ${this} SetTitle

        set scopes [retriever_what_to_qry [namespace tail ${this}]]
        if {${scopes} == ""} {
            #nothing is selected
            bell
            return
        }

        #display dialog box
        set Cancel_Dialog_Id [after 500 "${this} Display_Cancel_Dialog"]

        incr SearchActive

        # Block the UI while we search.
        tixBusy $itk_component(hull) on

        set oldcmd [${searchbtn} cget -command]
        set oldstate [${searchbtn} cget -state]
        set oldtext [${searchbtn} cget -text]
        set oldund [${searchbtn} cget -underline]

        ${searchbtn} config -text [get_indep String Cancel]\
          -underline [get_indep Pos Cancel] -command " ${this} cancel_fetching "
        if {[winfo exists ${this}-mixer.button_0]} {
            ${this}-mixer.button_0 config -text [get_indep String Cancel]\
              -command " ${this} cancel_fetching "
        }
        update
        update idletasks

        #display correct column labels
        if {${scopes} == "files"} {
            ${this}.list change_label 0 [get_indep String File]
            ${this}.list change_label 1 [get_indep String Directory]
            ${this}.list change_label 2 [get_indep String Type]
            ${this}.list change_label 3 ""
            ${this}.list change_label 4 ""

            ${this}.list toggle_column 0 "" 1
            ${this}.list toggle_column 1 "" 1
            ${this}.list toggle_column 2 "" 1
            ${this}.list toggle_column 3 "" 0
            ${this}.list toggle_column 4 "" 0
        } else {
            set type ""
            set prm ""
            set class ""
            foreach s ${scopes} {
                if {[lsearch -exact ${with_type} ${s}] != -1} {
                    set type [get_indep String Type]
                }
                if {[lsearch -exact ${with_param} ${s}] != -1} {
                    set prm [get_indep String Parameters]
                }
                if {[lsearch -exact ${with_class} ${s}] != -1} {
                    set class [get_indep String Class]
                }
            }
            if {[string first "files" ${scopes}] != -1} {
                set dir Directory
            } else {
                set dir ""
            }
            if {${dir} != "" && ${class} != ""} {
                set txt [file join ${class} ${dir}]
            } else {
                set txt "${class}${dir}"
            }
            ${this}.list change_label 0 [get_indep String Name]
            ${this}.list change_label 1 ${txt}
            ${this}.list change_label 2 ${type}
            ${this}.list change_label 3 ${prm}
            ${this}.list change_label 4 [get_indep String File]

            if {${txt} == ""} {
                ${this}.list toggle_column 1 "" 0
            } else {
                ${this}.list toggle_column 1 "" 1
            }
            if {${type} == ""} {
                ${this}.list toggle_column 2 "" 0
            } else {
                ${this}.list toggle_column 2 "" 1
            }
            if {${prm} == ""} {
                ${this}.list toggle_column 3 "" 0
            } else {
                ${this}.list toggle_column 3 "" 1
            }
            ${this}.list toggle_column 4 "" 1
        }

        display_contents ${this}.list [read_matched_from_db ${this}.list\
          ${scopes} -glob ${pat}]

        ${this}.list sort_refresh

        #delete cancel button, if availiable
        Delete_Cancel_Dialog

        #to discard the events bound on the buttons
        update

        #$searchbtn config -state $oldstate
        ${searchbtn} config -text ${oldtext} -underline ${oldund}\
          -command ${oldcmd} -state normal

        if {[winfo exists ${this}-mixer.button_0]} {
            ${this}-mixer.button_0 config -text ${oldtext} -command ${oldcmd}\
              -state normal
        }

        incr SearchActive -1

        # Release the UI back to the user.
        tixBusy $itk_component(hull) off
    }

    method next_entry {amount} {
        if {[${tree} size] <= 0} {
            bell
            return
        }
        ${tree} activate [expr [${tree} index active] + ${amount}]
        ${tree} selection clear 0 end
        ${tree} see active
        ${tree} selection set active
        if {$itk_option(-selectcommand) != ""} {
            ::eval $itk_option(-selectcommand) [Selection]
        } else {
            #emulate double click
            edit_symbol ${tree}
        }
    }

    method handle_all_none {prefix all} {
        global sn_scopes
        upvar #0 ${this}-exclusive excl
        set excl 0
        ${this}-mixer.button_0 config -state normal
        foreach sc ${sn_scopes} {
            if {${all}} {
                if {[info commands paf_db_${sc}] == ""} {
                    set val ""
                } else {
                    set val ${sc}
                }
            } else {
                set val ""
            }
            set ::${prefix}-${sc} ${val}
        }

        # Note: The "files" option is not selected by all but
        # it is deselected by none.

        if {!$all} {
            set ::${prefix}-files ""
        }
    }

    method handle_space {} {
        ${tree} selection clear 0 end
        ${tree} selection set active
        if {$itk_option(-selectcommand) != ""} {
            ::eval $itk_option(-selectcommand) [Selection]
        }
    }

    #Click can toggle a subtree or goto a symbol
    method handle_click {x y} {
        set res [Retriever&::expand_classes ${this} ${x} ${y}\
          display_contents]
        if {${res} != "" && $itk_option(-selectcommand) != ""} {
            ::eval $itk_option(-selectcommand) [Selection]
        }
    }

    method Title {{full 1}} {
        global sn_options
        upvar #0 ${this}-pattern ptrn
        if {${ptrn} == ""} {
            set ptrn "*"
        }
        set t [string trimright ${Retr_Title} "."]
        if {${mode} != "symbr" && ${ptrn} != ""} {
            set t "${t}: ${ptrn}"
        }
        if {${full}} {
            return [sn_title ${t}]
        } else {
            return ${t}
        }
    }
    method Icon {} {
        return [sn_view_icon ${Retr_Title}]
    }
    method SetTitle {} {
        ${topw} configure -title [Title] -iconname [Icon]
    }

    method print {} {
        if {$itk_option(-next) != ""} {
            $itk_option(-next) print
        } else {
            ${this}.list print_dialog_box
        }
    }

    method filter {{all 0}} {
    }

    #this function is called, when the symbols combobox
    #is called
    method postcommand {m} {
        set ed [MultiWindow&::list_find $itk_option(-next) edit]
        if {$itk_option(-symbols) == "" || ${ed} == ""} {
            return
        }
        ${ed} postcommand ${m}
    }

    method activate {} {
        if {$itk_option(-symbols) != ""} {
            set ed [MultiWindow&::list_find $itk_option(-next) edit]
            if {${ed} == ""} {
                $itk_option(-symbols) configure -state disabled
            }
        }
        if {$itk_option(-menu) != ""} {
            #set mn [string range $menu 0 [expr [string last "." $menu] - 1]]
            #$mn entryconfig [get_indep String MultiRetriever]	-state normal
        }
        if {$itk_option(-toolbar) != ""} {
            pack $itk_option(-toolbar).retrfr -side left
        }
    }

    method deactivate {} {
        if {$itk_option(-symbols) != ""} {
            $itk_option(-symbols) configure -state normal
        }
        if {$itk_option(-menu) != ""} {
            #set mn [string range $menu 0 [expr [string last "." $menu] - 1]]
            #$mn entryconfig [get_indep String MultiRetriever]	-state disabled
        }
        if {$itk_option(-toolbar) != ""} {
            pack forget $itk_option(-toolbar).retrfr
        }
        #hide filter window, if availiable
        if {[winfo exists ${this}-mixer]} {
            itcl::delete object ${this}-mixer
        }
    }

    method Selection {} {
        global sn_sep
        set sel [lindex [${tree} curselection] 0]
        if {${sel} == ""} {
            return ""
        }
        set txt [split [${tree} get ${sel}] \t]
        set data [split [${tree} itemcget ${sel} -data] \t]
        set file [lindex ${data} 0]
        set from [lindex ${data} 1]
        set to [lindex ${data} 2]

        if {${data} != "1"} {
            get_key_and_scope [lindex ${txt} 0] sym scope
            return [list ${scope} ${sym} [lindex ${txt} 1] ${file} ${from}\
              [lindex ${txt} 2] [lindex ${txt} 3] ${to}]
        } else {
            set file [lindex ${txt} 0]
            set dir [lindex ${txt} 1]
            if {${dir} != ""} {
                set file [file join ${dir} ${file}]
            }
            #it's a file entry, return only file name
            return [list "" "" "" ${file} "" "" "" ""]
        }
    }
    method gotosymbol {{scope ""} {sym ""} {cls ""} {file ""} {from ""} {type\
      ""} {prm ""} {to ""} {always 1}} {
        return 1
    }
    method clearselection {} {
        ${tree} selection clear 0 end
    }

    method control_buttons {} {
        foreach sc [list cl md fu f] {
            if {[::info commands paf_db_${sc}] != ""} {
                set ${sc}_state normal
            } else {
                set ${sc}_state disabled
            }
        }
        if {$itk_option(-toolbar) != "" && [winfo exists $itk_option(-toolbar).retrfr.class]} {
            $itk_option(-toolbar).retrfr.class config -state ${cl_state}
            $itk_option(-toolbar).retrfr.meth config -state ${md_state}
            $itk_option(-toolbar).retrfr.func config -state ${fu_state}
            $itk_option(-toolbar).retrfr.file config -state ${f_state}
        }
    }

    method Update_Layout {} {
        global sn_options
        ${tree} config -font $sn_options(def,default-font)\
          -selectforeground $sn_options(def,select-fg)\
          -selectbackground $sn_options(def,select-bg)
    }

    method Focus {} {
        focus ${tree}
    }

    #refresh retriever, only by exclusive mode
    method Refresh_Display {} {
        upvar #0 ${this}-exclusive excl
        if {[info exists excl] && ${excl}} {
            #mark position
            set sel [${tree} curselection]
            #first viewed item in the tree
            set seen [${tree} index @0,0]
            set active [${tree} index active]

            start_search

            #restore older position
            if {${sel} != ""} {
                ${tree} selection clear 0 end
                ${tree} selection set ${sel}
            }
            if {${active} != ""} {
                ${tree} activate ${active}
            }
            if {${seen} != ""} {
                #restore back the older display
                ${tree} see -top ${seen}
            }
        }
        control_buttons
    }

    #make a title for the Dump position, this is usefull
    #for the views stack (prev/next)
    method DumpTitle {{dump ""}} {
        if {${dump} == ""} {
            set dump [Dump]
        }
        set pattern [lindex ${dump} 0]
        set scopes [lindex ${dump} 1]

        if {${pattern} == ""} {
            set pattern "*"
        }
        if {${scopes} == ""} {
            set scopes "files"
        }
        return "Retrieve [string trim "${scopes}(${pattern})"]"
    }

    method AddHistoryFromDump {dump title} {
    }

    #return the important data to restore this widget
    #in later time again (used by saving the project)
    method Dump {} {
        upvar #0 ${this}-pattern pattern
        upvar #0 ${this}-exclusive excl
        if {![info exists excl]} {
            set excl 0
        }
        set scopes [retriever_what_to_qry ${this}]

        return [list ${pattern} ${scopes} ${excl}]
    }

    #gets the result from the function "Dump" to
    #restore the older state (used by restoring the project)
    method Restore {str} {
        upvar #0 ${this}-pattern pattern
        upvar #0 ${this}-exclusive excl

        set pattern [lindex ${str} 0]
        set excl [lindex ${str} 2]

        set scopes [lindex ${str} 1]
        RestoreScopes ${scopes}

        start_search
    }

    #window can't be closed when a fetch is active.
    method Close {{mode 0}} {
        if {${SearchActive}} {
            bell
            sn_error_dialog [get_indep String CannotCloseWindow] ${Retr_Title}
            return 0
        }
        return 1
    }

    method whoami {} {
        return retr
    }

    #if an editor is added to the retriever, call the
    #goto function of the editor
    method goto {combo txt} {
        set ed [MultiWindow&::list_find $itk_option(-next) edit]
        if {$itk_option(-symbols) != "" && ${ed} != ""} {
            #dump the current view into the history stack
            ${topw} history_stack_add_point ${ed}

            ${ed} goto ${combo} ${txt}
        }
    }

    protected variable searchbtn ""
    protected variable topw
    protected variable tree ""
    protected variable treew ""
    protected variable ScopesFilter

    protected variable cl_state disabled
    protected variable md_state disabled
    protected variable fu_state disabled
    protected variable f_state disabled

    public variable Retr_Title [get_indep String Retriever]

    itk_option define -selectcommand selectcommand Selectcommand "" {
        if {[winfo exists ${this}.list]} {
            ${this}.list treebind <space> "${this} handle_space"
        }
    }

    public variable pattern "" {
        if {[winfo exists ${this}.pattern.e]} {
            global ${this}-pattern
            set ${this}-pattern ${pattern}
        }
    }

    public variable width 60
    public variable height 30

    #can be
    #"retr " to interact as a retriever
    #"symbr" to interact as symbol browser
    public variable mode "retr"

    #true, when a symbol browser is created during a restore session
    public variable restore 0
}




