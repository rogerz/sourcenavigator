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
# rcs.tcl - The version control browser.
# Copyright (C) 1998 Cygnus Solutions.

itcl::class RevisionCtrl& {
    inherit sourcenav::Window

    # Builds the toplevel window for the Project Editor, called '$this'
    constructor {args} {
        global sn_options sn_path

        set font $sn_options(def,default-font)

	eval itk_initialize $args
        # loaded project source code files relative to the main 
        # project directory which are not hidden:
        set proj_files [sn_project_file_list 0]

        sn_create_window $itk_component(hull)
        #only one window
        $itk_component(hull).status.reuse config -state disabled

        #------------------------------------------------------------------
        # Split the main Window into three parts:
        # Top:	Selected Files, Actions, Unloaded Files
        # Bottom: Symbols and history
        #------------------------------------------------------------------
        PanedWindow $itk_component(hull).p -width 900 -height 600 -fraction "0.55"\
          -orientation y -min 0

        set tp [lindex [$itk_component(hull).p panes] 0]
        set bp [lindex [$itk_component(hull).p panes] 1]

        #--------------------------------------------------------------
        # Split the top part into selected and (actions+unloaded)
        #--------------------------------------------------------------
        set selected ${tp}
        set selected_list ${tp}.selected

        #------------------------------------------------------------------
        # Build Menu-Frame for $this
        #------------------------------------------------------------------
        set m $itk_component(hull).menu

        ${m}.edit insert 0 separator

        ${m}.edit insert 0 command -label [get_indep String Statistics]\
          -underline [get_indep Pos Statistics] -command " sn_statistic "

        set m ${m}.edit
        ${m} configure -postcommand "[info class]::set_menu_state ${m}\
          ${selected_list} ${this}"
        ${m} insert 0 command -label [get_indep String ChooseCheckOut]\
          -underline [get_indep Pos ChooseCheckOut] -command " ${this}\
          check_in_out co "

        ${m} insert 1 command -label [get_indep String ChooseDiscard]\
          -underline [get_indep Pos ChooseDiscard] -command " ${this}\
          check_in_out unco "

        ${m} insert 2 command -label [get_indep String ChooseCheckIn]\
          -underline [get_indep Pos ChooseCheckIn] -command " ${this}\
          check_in_out ci "

        ${m} insert 3 command -label [get_indep String ChooseDel]\
          -underline [get_indep Pos ChooseDel] -command " ${this} check_in_out\
          del "

        ${m} insert 4 command -label [get_indep String ChooseLock]\
          -underline [get_indep Pos ChooseLock] -command " ${this}\
          check_in_out lock "

        ${m} insert 5 command -label [get_indep String ChooseUnlock]\
          -underline [get_indep Pos ChooseUnlock] -command " ${this}\
          check_in_out unlock "

        ${m} insert 6 separator
        ${m} insert 7 command -label [get_indep String ChooseDiff]\
          -underline [get_indep Pos ChooseDiff] -command " ${this} diff "

        ${m} insert 8 separator

        #------------------------------------------------------------------
        # Split the bottom part into Symbols and History
        #------------------------------------------------------------------
        PanedWindow ${bp}.rcslog -fraction 0.2 -orientation x

        pack ${bp}.rcslog -fill both -expand y

        set symbols [lindex [${bp}.rcslog panes] 0]
        set history [lindex [${bp}.rcslog panes] 1]

        #------------------------------------------------------------------
        # Build views-chooser in express line
        #------------------------------------------------------------------

        set views [paf_db_proj get -key views]
        set cur [paf_db_proj get -key db_exclude]
        if {${views} == ""} {
            set views "default"
        }

        set val [list]
        foreach v ${views} {
            eval lappend val [lindex ${v} 0]
        }

        Combo& $itk_component(hull).exp.view -width 20 -label\
          [get_indep String View] -contents ${val}\
          -selectcommand "$this open_view"

        set active [$itk_component(hull).exp.view selecttext ${cur}]

        pack $itk_component(hull).exp.view -side left

        #------------------------------------------------------------------
        # Build button line at bottom
        #------------------------------------------------------------------

        pack $itk_component(hull).p -fill both -expand y

        label ${selected}.label -anchor w -text [get_indep String ChooseFiles]

        set itk_option(-input) ${proj_files}

        set font $sn_options(def,default-font)

        #------------------------------------------------------------------
        # Build top paned window
        #------------------------------------------------------------------

        Selector& ${selected_list} -exportselection n\
          -selectmode extended -font ${font}\
          -bitmap @$sn_path(bitmapdir)/file.xbm -contents $itk_option(-input)

        set rcs_hist_top ${history}.top
        set rcs_hist_show ${history}.show
        ${selected_list} treebind <Double-1> "${this} edit_file"
        ${selected_list} treebind <Double-3> "${this} edit_file"
        ${selected_list} treebind <Return> "${this} edit_file; break"
        ${selected_list} treebind <space> "${this} edit_file; break"

        pack ${selected}.label -fill x
        pack ${selected_list} -fill both -expand y

        ${this} configure -title [sn_title [get_indep String RevisionControlEditorNoKey]]
        ${this} configure -iconname Editor

        #------------------------------------------------------------------
        # Build bottom paned window
        #------------------------------------------------------------------

        frame ${rcs_hist_top}
        label ${rcs_hist_top}.l -text [get_indep String ChooseHistory]\
          -textvariable ${this}-history
        pack ${rcs_hist_top}.l -fill x -expand y -side left

        # Add a pull down menu at the history window to choose on of the
        # (or all) revision-logs to display:
        Combo& ${rcs_hist_top}.r -width 15 \
          -selectcommand "${this} log_revision"
        pack ${rcs_hist_top}.r -side left

        set rcs_symb_top ${symbols}.top
        set rcs_symb_show ${symbols}.show

        label ${rcs_symb_top} -text [get_indep String SymbolicTags]

        listbox ${rcs_hist_show} -font ${font}

        pack ${rcs_hist_top} -fill x
        pack ${rcs_hist_show} -fill both -expand y

        listbox ${rcs_symb_show} -font ${font}

        pack ${rcs_symb_top} -fill x -pady 5
        pack ${rcs_symb_show} -fill both -expand y

        #---------------------------------------------------------------------
        # What to do if there is a mouse click on a file:
        # a) No version control:
        #	Clear the selection of the 'unloaded' list
        # b) A new file has been selected:
        #	Show rcs data
        #---------------------------------------------------------------------
        set l ${selected_list}.list_text.list

        ${selected_list} treebind <1> "
			${this} show_log \[%W get \[%W nearest %y\]\] ${rcs_hist_show}\
          ${rcs_symb_show} 1
			%W activate \[%W nearest %y\]
		"

        ${selected_list} treebind <3> "${this} show_log \[%W get \[%W nearest\
          %y\]\] ${rcs_hist_show} ${rcs_symb_show} 1"


        ${this} move_to_mouse
        ${this} take_focus

        ${selected_list} activate end
        toggle_show

        #call user defined procedure
        catch {sn_rc_project_editor $itk_component(hull) ${this}.menu ${selected_list}}
    }

    destructor {
        foreach v [info globals ${this}] {
            uplevel #0 unset ${v}
        }
    }

    proc set_menu_state {menu sel_list cls} {
        global sn_options
        global sn_verctl_options

        set rcstype $sn_options(both,rcs-type)

        if {![info exists sn_verctl_options(${rcstype},checkout)] || ![info\
          exists sn_verctl_options(${rcstype},checkout-exclusive)] || ![info\
          exists sn_verctl_options(${rcstype},checkout-individual)]} {
            ${menu} entryconfig 0 -state disabled
        }

        if {![info exists sn_verctl_options(${rcstype},discard)]} {
            ${menu} entryconfig 1 -state disabled
        }

        if {!([info exists sn_verctl_options(${rcstype},checkin)] && ![info\
          exists sn_verctl_options(${rcstype},checkin-exclusive)]) || ![info\
          exists sn_verctl_options(${rcstype},checkin-comment-via)]} {
            ${menu} entryconfig 2 -state disabled
        }

        if {![info exists sn_verctl_options(${rcstype},delete-revision)]} {
            ${menu} entryconfig 3 -state disabled
        }

        if {![info exists sn_verctl_options(${rcstype},lock)] || ![info exists\
          sn_verctl_options(${rcstype},lock-individual)]} {
            ${menu} entryconfig 4 -state disabled
        }

        if {![info exists sn_verctl_options(${rcstype},unlock)] || ![info\
          exists sn_verctl_options(${rcstype},unlock-individual)]} {
            ${menu} entryconfig 5 -state disabled
        }

        if {![info exists\
          sn_verctl_options(${rcstype},checkout-individual-to-stdout)]} {
            ${menu} entryconfig 7 -state disabled
        }
    }

    method open_view {view} {
        set active 1
        sn_change_view ${view}
        Refresh
    }

    method log_revision {{rev ""}} {
        # called from a combo box at the top right of the history window
        # to display revision log information
        set rev [string trim ${rev}]
        if {[string compare ${rev} [get_indep String All]] == 0} {
            # ok, display something about all revisions
            set rev ""
        }

        set files ""
        foreach f [${selected_list} marked] {
            lappend files [lindex ${f} 0]
        }
        if {[string compare ${files} ""] != 0} {
            # there are selected files, so go on and show the log information
            ${this} show_log ${files} ${rcs_hist_show} ${rcs_symb_show} 1 ${rev}
        }
    }

    method check_in_out {cmd} {
        set files [source_file_names [${selected_list} marked]]
        if {${files} == ""} {
            return
        }

        # Block the UI while running the command.
        tixBusy $itk_component(hull) on

        switch ${cmd} {
            "ci" {
                    sn_rcs_checkin ${files}
                }
            "co" {
                    sn_rcs_checkout ${files}
                }
            "unco" {
                    sn_rcs_discard ${files}
                }
            "lock" {
                    sn_rcs_lockunlockdel lock ${files}
                }
            "unlock" {
                    sn_rcs_lockunlockdel unlock ${files}
                }
            "del" {
                    sn_rcs_lockunlockdel del ${files}
                }
        }

        # Release the UI back to the user.
        tixBusy $itk_component(hull) off
    }

    method hist_show_name {} {
        return ${rcs_hist_show}
    }

    method symb_show_name {} {
        return ${rcs_symb_show}
    }

    proc refresh {files} {
        load_again ${files}

        set objects [itcl_info objects "*" -class [info class]]
        foreach obj ${objects} {
            # $obj append_lock_info $files
            set show [${obj} hist_show_name]
            set symb [${obj} symb_show_name]
            ${obj} show_log [lindex ${files} 0] ${show} ${symb}
        }
    }

    proc load_again {files} {
        sn_parse_uptodate ${files}
    }

    method edit_file {} {
        set name [lindex [${selected_list} get active] 0]
        sn_edit_file dummy ${name}
    }

    method diff {} {
        set o_files [source_file_names [${selected_list} marked]]
        sn_rcs_diff ${this} ${o_files}
    }

    method toggle_show {} {
 
        set file_list $itk_option(-input)
        set state normal
        set load_state disabled
        ${selected_list} selection clear 0 end

        ${rcs_hist_top}.r configure -contents {}
        ${selected_list} activate end
    }

    #---------------------------------------------------------------------
    # modifies the display of the file list by appending
    # information about locked revisions and who locked them
    # Parameters: files	   = a list of files to get information for
    #---------------------------------------------------------------------
    method append_lock_info {files} {
        global sn_options
    }

    protected variable file_list

    proc source_file_names {files} {
        set fls ""
        foreach f ${files} {
            lappend fls [lindex ${f} 0]
        }
        return ${fls}
    }

    method show_log {file log sym {bsy 0} {rev ""}} {
        # display information in the history window
        global ${this}-history

        set file [lindex ${file} 0]

        # Block UI while we get the log info.
        tixBusy $itk_component(hull) on

        # set the title string of the history window
        set hist_lb [get_indep String ChooseHistory]

        # get the information of the file
        set error [catch {sn_rcs_get_revisions ${file} ${rev} sy rv lg 0} ret]

        # clean the tags and history windows
        ${log} delete 0 end
        ${sym} delete 0 end

        if {!${error} && ${ret}} {
            # lg   = log text
            # sy   = symbol text
            # rv   = list of existing revisions for $file
            # 0	= no busy flag
            set rv [linsert ${rv} 0 [get_indep String All]]
            # insert the log information into the history window
            eval ${log} insert end ${lg}
            eval ${sym} insert end ${sy}
            if {${rev} != ""} {
                set curs end
            } else {
                set curs 0
            }
            ${log} see ${curs}
            ${sym} see ${curs}

            append hist_lb " " ${file}
        } else {
            # there were no rcs information, make check in possible
            set rv ""
        }
        # finally display the results:
        set ${this}-history ${hist_lb}
        if {${rev} == ""} {
            ${rcs_hist_top}.r configure -contents ${rv}
        }
        # Release UI back to user.
        tixBusy $itk_component(hull) off
    }

    method locking {} {
        return ${locking}
    }

    # Here you should read the value of 'locking' from a global
    # variable of the project. Having it set to 0 is not desireable.
    common locking 1

    itk_option define -input input Input {} {
        if {[winfo exists ${selected_list}]} {
            ${selected_list} setSelectorContents $itk_option(-input)
        }
    }

    method Take {{bsy 1} {nodisable ""}} {
        global ${this}-value

        if {${bsy}} {
            ${this} grab set
        }

        tkwait variable ${this}-value

        if {[catch {set ret [set ${this}-value]}]} {
            set ret "cancel"
        }

        if {${bsy}} {
            catch {${this} grab release}
        }

        itcl::delete object ${this}

        return ${ret}
    }

    method active {{val -1}} {
        if {${val} != -1} {
            set active ${val}
        }
        return ${active}
    }

    method Refresh {} {

        ${this} configure -input [sn_project_file_list 0]
        set file_list $itk_option(-input) 
        set active 0

        ${selected_list} setSelectorContents [sn_project_file_list 0]
        toggle_show
    }

    proc Refresh_YourSelf {} {
        foreach prj [itcl_info objects "*" -class RevisionCtrl&] {
            if {! [${prj} active]} {
                ${prj} open_view default
            }
        }
    }

    public variable ovr ""
    protected variable ok
    protected variable files_to_unload ""
    protected variable selected_list ""
    protected variable rcs_hist_top
    protected variable rcs_hist_show
    protected variable rcs_symb_top
    protected variable rcs_symb_show
    protected variable active 0
}

