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
itcl::class Project& {

    inherit sourcenav::Window

    constructor {args} {
        global sn_options
        global sn_newargs

        set width 780
        set height 580

        withdraw

        eval itk_initialize $args

        ${this} on_close "${this} windows_close"

        set Toolbar $itk_component(hull).exp
        set Statusbar $itk_component(hull).msg
        set Reuse $itk_component(hull).msg.reuse

        #state for new project dependent widgets
        if {$itk_option(-new_project)} {
            set new_state normal
            set exist_state disabled
        } else {
            set new_state disabled
            set exist_state normal
        }

        AddToolbar
        #AddMenu
        AddStatusbar

        #Field for project name
        set sn_newargs(path) $sn_options(sys,project-file)
        if {! [file exists $sn_newargs(path)] && [file exists\
          $sn_newargs(path).proj]} {
            set sn_newargs(path) $sn_newargs(path).proj
        }
        set prjname_Entry $itk_component(hull).prjname

        LabelEntryButton& ${prjname_Entry} -text [get_indep String\
          ProjectName] -underline [get_indep Pos ProjectName] -anchor nw\
          -variable sn_newargs(path) -native 1 -buttonballoon\
          [get_indep String ChooseINFO] -save_open "save"\
          -defaultextension ".proj" -extensions $sn_options(project_extensions)


        pack ${prjname_Entry} -side top -anchor nw -fill x -padx 5 -pady 5
        ${prjname_Entry} configure -state ${new_state}

        #frame for tree & Buttons
        set treebtnsfr $itk_component(hull).treebtns
        pack [frame ${treebtnsfr}] -side top -fill both -expand 1

        #Tree frame
        set treefr ${treebtnsfr}.tree
        pack [frame ${treefr}] -side left -fill both -expand 1

        #project views
        set Views_Combo ${treefr}.views
        Combo& ${Views_Combo} \
          -label [get_indep String View] -entryvariable ${this}-view\
          -selectcommand "${this} change_view ${Views_Combo}"\
          -postcommand "${this} post_views ${Views_Combo}" -state ${exist_state}
        pack ${Views_Combo} -side top -fill x

        #Project tree
        set tree ${treefr}.tree
        set treew ${tree}.tree
        Tree ${tree} -tabs {250 250} -tabsize 1 -labels {Name Directory}\
          -indentwidth 20 -selectborderwidth 2 -fillcommand\
          "${this} Display_ProjectFiles" -selectmode extended\
          -font $sn_options(def,default-font)\
          -selectforeground $sn_options(def,select-fg)\
          -selectbackground $sn_options(def,select-bg) -plusimage plus_image\
          -minusimage minus_image
        #		-hiddenimage dir+_image
        pack ${tree} -side top -fill both -expand 1
        set treew [${tree} tree]

        ${tree} treebind <1> "${this} tree_toggle %W %x %y"
        ${tree} treebind <ButtonRelease-1> "${this} control_file_buttons"
        ${tree} treebind <3> "focus %W; ${this} select_post_tree_menu %W %X %Y %y;\
          break"
        ${tree} treebind <Double-1> "${this} tree_edit_file %W"
        ${tree} treebind <Return> "${this} tree_edit_file %W; break"
        ${tree} treebind <space> "${this} tree_edit_file %W; break"

        #Buttons frame
        set btnsfr ${treebtnsfr}.btns
        pack [frame ${btnsfr}] -side right -fill y -padx 5 -pady 5

        #file commands frame
        set filefr ${treebtnsfr}.btns.file
        pack [frame ${filefr}] -side left -fill y -padx 5 -pady 5

        #View button
        button ${filefr}.view -text [get_indep String View]\
          -underline [get_indep Pos View] -state disabled -command "${this} hide_view_unload_files View \[${treew} curselection\]"
        balloon_bind_info ${filefr}.view [get_indep String ViewINFO]

        #hide button
        button ${filefr}.hide -text [get_indep String Hide]\
          -underline [get_indep Pos Hide] -state disabled -command "${this} hide_view_unload_files Hide \[${treew} curselection\]"
        balloon_bind_info ${filefr}.hide [get_indep String HideINFO]

        #unload button
        button ${filefr}.unload -text [get_indep String Unload]\
          -underline [get_indep Pos Unload] -state disabled -command "${this} hide_view_unload_files Unload \[${treew} curselection\]"
        balloon_bind_info ${filefr}.unload [get_indep String UnloadINFO]

        #statistic button
        button ${filefr}.stat -text [get_indep String Statistics]\
          -underline [get_indep Pos Statistics] -state disabled -command "
                ${this} disp_file_statistic \[${treew} curselection\]"
        balloon_bind_info ${filefr}.stat [get_indep String StatisticsINFO]

        foreach button {view hide unload stat} {
            pack ${filefr}.${button} -side top -pady 3 -anchor nw -fill x
        }

        #project commands frame
        set btnsfr ${treebtnsfr}.btns.prj
        pack [frame ${btnsfr}] -side right -fill y -padx 5 -pady 5 -anchor ne

        #Ok button
        button ${btnsfr}.ok -text [get_indep String Ok] -underline\
          [get_indep Pos Ok] -command " ${this} apply exit " -state ${new_state}
        balloon_bind_info ${btnsfr}.ok [get_indep String ProjectOkINFO]

        #Apply button
        button ${btnsfr}.apply -text [get_indep String Apply]\
          -underline [get_indep Pos Apply] -command " ${this} apply "\
          -state ${exist_state}
        balloon_bind_info ${btnsfr}.apply [get_indep String ApplyINFO]

        #Undo button
        button ${btnsfr}.undo -text [get_indep String EditUndo]\
          -underline [get_indep Pos EditUndo] -command " ${this} undo "\
          -state ${exist_state}
        balloon_bind_info ${btnsfr}.undo [get_indep String UndoINFO]

        #cancel button
        button ${btnsfr}.cancel -text [get_indep String Cancel]\
          -underline [get_indep Pos Cancel] -command " ${this} windows_close "\
          -state normal
        balloon_bind_info ${btnsfr}.cancel [get_indep String ProjectCancelINFO]
        foreach button {ok apply undo cancel} {
            pack ${btnsfr}.${button} -side top -pady 3 -anchor nw -fill x
        }

        pack [frame ${btnsfr}.space1 -height 15] -side top -fill x

        #allow to add files any way.
        set state normal

        #Add ... button
        button ${btnsfr}.add -text [get_indep String LoadFiles]\
          -underline [get_indep Pos LoadFiles] -command " ${this} add_files "\
          -state ${state}
        balloon_bind_info ${btnsfr}.add [get_indep String LoadNewFilesINFO]

        #Add a directory
        button ${btnsfr}.adddir -text [get_indep String LoadDirectory]\
          -underline [get_indep Pos LoadDirectory] -command " ${this}\
          add_directory " -state ${state}
        balloon_bind_info ${btnsfr}.adddir [get_indep String LoadDirectoryINFO]

        #Add from a project
        button ${btnsfr}.addprj -text [get_indep String LoadFromProject]\
          -underline [get_indep Pos LoadFromProject] -command " ${this}\
          add_from_project " -state ${state}
        balloon_bind_info ${btnsfr}.addprj [get_indep String\
          LoadFromProjectINFO]

        foreach button {add adddir addprj} {
            pack ${btnsfr}.${button} -side top -pady 3 -anchor nw -fill x
        }

        pack [frame ${btnsfr}.space2 -height 15] -side top -fill x

        #Project Preferences
        #No project preferences if the project already exists
        if {$itk_option(-new_project)} {
            button ${btnsfr}.pref -text [get_indep String preference]\
              -underline [get_indep Pos preference] -command\
              " sn_project_preferences $itk_option(-new_project) " -state normal
            balloon_bind_info ${btnsfr}.pref [get_indep String preference]
            pack ${btnsfr}.pref -side bottom -side top -pady 20 -anchor nw\
              -fill x
        }

        if {$itk_option(-new_project)} {
            ${this} configure -title [sn_title [get_indep String NewProj]\
	$itk_option(-new_project)]
        } else {
            ${this} configure -title [sn_title [get_indep String ProjectEditor]]
        }
        ${this} configure -iconname [get_indep String ProjectEditor]

        #correct tabs
        set Tree_Mode ""
        toggle_disp_mode

        #Read project file list
        ReadProjectFiles 1

        #fill the tree widget with the file names of
        #the project
        Refresh

        ${this} configure -geometry ${width}x${height}
        ${this} centerOnScreen
        ${this} deiconify
# FIXME: Do we really need this call to take_focus here?
        ${this} take_focus
	
        #call user function
        if {[catch {sn_rc_projecteditor $itk_component(hull)\
                      $itk_component(hull).menu} err]} {
            sn_log "error while exec sn_rc_projecteditor: ${err}"
        }

        if {$itk_option(-variable) != ""} {
            vwait $itk_option(-variable)
        }

    }

    destructor {
        foreach v [::info globals "${this}-*"] {
            catch {uplevel #0 unset ${v}}
        }
    }

    method AddToolbar {} {
        frame ${Toolbar} -relief groove -border 2
        pack ${Toolbar} -side top -fill x

        #View mode (tree, categorie, list)
        global ${this}-Tree_Mode
        set ${this}-Tree_Mode ${Tree_Mode}
        set lbls [list [get_indep String Tree] [get_indep String Categorie]\
          [get_indep String List]]
        Radio& ${Toolbar}.treemode -variable ${this}-Tree_Mode -labels ${lbls}\
          -contents {tree cat list} -label [get_indep String ViewMode]\
          -labelwidth -1 -command " ${this} toggle_disp_mode "
        pack ${Toolbar}.treemode -side left

        pack [frame ${Toolbar}.space -width 5] -side left

        #display dirname
        #button $Toolbar.dirs  #		-text "directories"  #		-image sign_image \
          #		-command "$this toggle_directories $Toolbar.dirs"  #		-relief\
          sunken  #		-state disabled
        #balloon_bind_info $Toolbar.dirs [get_indep String Disp_Directories]
        #pack $Toolbar.dirs -side left
        global ${this}-disp_Directories
        set ${this}-disp_Directories ${disp_Directories}
        checkbutton ${Toolbar}.dirs -text [get_indep String Fullpath]\
          -variable ${this}-disp_Directories -command " ${this}\
          toggle_directories ${Toolbar}.dirs " -onvalue 1 -offvalue 0\
          -state disabled
        balloon_bind_info ${Toolbar}.dirs [get_indep String Disp_Directories]
        pack ${Toolbar}.dirs -side left
    }

    protected variable disp_Directories 1
    method toggle_directories {btn} {
        upvar #0 ${this}-disp_Directories disp_Dir
        set disp_Directories ${disp_Dir}
        #toggle displaying tabs
        toggle_disp_mode
        #redisplay contents
        Display_ProjectFiles
    }

    method AddStatusbar {} {
        global sn_options

        frame ${Statusbar}

        pack [label ${Statusbar}.msg -font $sn_options(def,layout-font)\
          -relief groove -bd 2 -anchor w -textvar ${this}.msg] -expand y\
          -fill x -side left
        pack ${Statusbar} -side bottom -fill x
    }

    method handle_select {} {
    }
    method file_post {m} {
    }
    method edit_post {m} {
    }
    method tools_post {m} {
    }
    method AddMenu {} {
        global sn_options
        set m $itk_component.menu

        set state normal

        menu ${m} -tearoff 0

        ## File menu
        ####################
        menu ${m}.file -tearoff 0 -postcommand "${this} file_post ${m}.file"
        ${m} add cascade -label [get_indep String EditFile] -menu ${m}.file\
          -underline [get_indep Pos EditFile]

        #Statistiks
        if {$itk_option(-new_project)} {
            set state disabled
        } else {
            set state normal
        }
        ${m}.file add command -label [get_indep String Statistics]\
          -underline [get_indep Pos Statistics] -command " sn_statistic "\
          -state ${state}

        ${m}.file add separator

        #close window
        ${m}.file add command -command " ${this} windows_close "\
          -label [get_indep String WindowsClose] -underline [get_indep Pos\
          WindowsClose]
        #exit program
        ${m}.file add command -label [get_indep String Exit]\
          -underline [get_indep Pos Exit] -command " sn_exit "

        ##Windows
        AddWindowsMenu ${m} $itk_component(hull)

        ##Help
        AddHelpMenu ${m} $itk_component(hull)

        ${this} configure -menu ${m}
    }

    method control_file_buttons {} {
        set sels [${treew} curselection]
        set view_state normal
        set unload_state normal
        set hide_state normal
        set rename_state normal
        set statistic_state disabled
        if {${sels} != ""} {
            set rootname [${treew} rootname [lindex ${sels} 0]]
            set rootname2 [${treew} rootname [lindex ${sels} end]]
        } else {
            set rootname ""
            set rootname2 ""
        }
        if {${rootname} != "" && ${rootname} == ${rootname2}} {
            if {${rootname} == [get_indep String ProjectFiles]} {
                set view_state disabled
                set statistic_state normal
            }\
            elseif {${rootname} == [get_indep String UnloadedFiles]} {
                set unload_state disabled
            } else {
                set hide_state disabled
            }
        } else {
            set view_state disabled
            set unload_state disabled
            set hide_state disabled
            set rename_state disabled
        }
        if {$itk_option(-new_project)} {
            #no hide by new projects
            set hide_state disabled
            set statistic_state disabled
        }
        ${filefr}.view configure -state ${view_state}
        ${filefr}.hide configure -state ${hide_state}
        ${filefr}.unload configure -state ${unload_state}
        ${filefr}.stat configure -state ${statistic_state}
    }

    method control_buttons {} {
        #we can't apply the changes if we don't have files active,
        #at least one file must be availiable
        #if {$ProjectFiles == ""} {
        #	set can_apply 0
        #}
        if {${can_apply}} {
            set apply_state normal
            set undo_state normal
        } else {
            set apply_state disabled
            set undo_state disabled
        }
        if {$itk_option(-new_project)} {
            set ok_state normal
        } else {
            set ok_state ${apply_state}
        }

        if {$itk_option(-new_project)} {
            set apply_state disabled
        }
        #user can has have already closed the window
        if {[winfo exists ${btnsfr}]} {
            ${btnsfr}.ok configure -state ${ok_state}
            ${btnsfr}.apply configure -state ${apply_state}
            ${btnsfr}.undo configure -state ${undo_state}

            control_file_buttons
        }
    }

    #edit file
    method tree_edit_file {w} {
        #no edit, if something is changed, because a update
        #of the files occurs refreshing the project editor
        #and the changes will be lost.
        if {${can_apply}} {
            bell
            return
        }

        ${w} selection clear 0 end
        ${w} selection set active
        set idx [${w} curselection]
        if {[${w} children ${idx}] != "" || [${w} levels ${idx}] == 0} {
            bell
            return
        }
        set file [build_filepath ${treew} ${Tree_Mode} ${idx}]
        sn_edit_file dummy [sn_convert_FileName ${file}]
    }

    #post menu for tree
    method select_post_tree_menu {w X Y y} {
        set m .prj_post_tree_menu
        catch {destroy ${m}}
        menu ${m} -tearoff 0
        wm overrideredirect ${m} 1
        set y [${w} nearest ${y}]

        #we are on the base item
        if {${Tree_Mode} != "list" && [${w} itemcget ${y} -parent] == -1} {
            set rootitem 1
        } else {
            set rootitem 0
        }

        set sels [${w} curselection]

        #verify it the pointer points over a selected entry
        #if not, delete selection and select the current entry
        if {[lsearch -exact ${sels} ${y}] == -1} {
            ${w} selection clear 0 end
            ${w} selection set ${y}
            set sels ${y}
        }

        #for multi selection, only one categorie can be selected
        set rootname [${w} rootname [lindex ${sels} 0]]
        set rootname2 [${w} rootname [lindex ${sels} end]]
        if {${rootname} != ${rootname2}} {
            ${w} selection clear 0 end
            ${w} selection set ${y}
            set sels ${y}
        }

        set selcnt [llength ${sels}]
        if {${selcnt} == 1} {
            set file [${w} get ${y}]
        }\
        elseif {${selcnt} > 1} {
            set file "${selcnt} Files"
        } else {
            set file ""
        }

        set view_state normal
        set unload_state normal
        set hide_state normal
        set rename_state normal

        #file is marked as hidden
        if {${rootname} == [get_indep String HiddenFiles]} {
            set hide_state disabled
            #file is marked as unloaded
        }\
        elseif {${rootname} == [get_indep String UnloadedFiles]} {
            set unload_state disabled
            #file is viewed
        } else {
            set view_state disabled
        }
        if {${file} == ""} {
            set lbl ""
            set view_state disabled
            set unload_state disabled
            set hide_state disabled
            set rename_state disabled
        } else {
            set lbl " '${file}'"
        }

        #View
        ${m} add command -label "[get_indep String View]${lbl}"\
          -underline [get_indep Pos View] -command "${this} hide_view_unload_files View [list ${sels}]" -state ${view_state}

        #Hide
        ${m} add command -label "[get_indep String Hide]${lbl}"\
          -underline [get_indep Pos Hide] -command "${this} hide_view_unload_files Hide [list ${sels}]" -state ${hide_state}

        ${m} add separator

        #Unload
        ${m} add command -label "[get_indep String Unload]${lbl}"\
          -underline [get_indep Pos Unload] -command "${this} hide_view_unload_files Unload [list ${sels}]" -state ${unload_state}

        control_file_buttons

        tk_popup ${m} ${X} ${Y}
    }

    method disp_file_statistic {sel} {
        #verify if the first item is selected (hole project)
        if {[lindex ${sel} 0] == "0"} {
            set sel ""
        }
        if {${sel} != ""} {
            sn_statistic [build_file_list ${treew} ${Tree_Mode} ${sel}]
        } else {
            #call it for all project
            sn_statistic
        }
    }

    method hide_view_unload_files {cmd sels} {
        set files [build_file_list ${treew} ${Tree_Mode} ${sels}]
        set y [lindex ${sels} 0]

        if {${files} == ""} {
            bell
            return
        }

        #store the current state in the undo-variables to be
        #able to undo the last action
        set Undo_ProjectFiles ${ProjectFiles}
        set Undo_HiddenFiles ${HiddenFiles}
        set Undo_files_to_unload ${files_to_unload}

        if {${cmd} == "Hide"} {
            eval lappend HiddenFiles ${files}

            #delete to hide files from availiable list
            foreach f ${files} {
                #verify where the files have to be removed from
                if {${view_state} == "disabled"} {
                    #delete file from the project list
                    set i [lsearch -exact ${ProjectFiles} ${f}]
                    if {${i} >= 0} {
                        set ProjectFiles [lreplace ${ProjectFiles} ${i} ${i}]
                    }
                } else {
                    #delete files from the unloaded list
                    set i [lsearch -exact ${files_to_unload} ${f}]
                    if {${i} >= 0} {
                        set files_to_unload [lreplace ${files_to_unload} ${i}\
                          ${i}]
                    }
                }
            }
            Display_ProjectFiles
            ${tree} see ${y}
        }\
        elseif {${cmd} == "Unload"} {
            if {!$itk_option(-new_project)} {
                eval lappend files_to_unload ${files}
            }

            #delete to unload files from availiable list
            foreach f ${files} {
                #delete files from viewed list
                if {${view_state} == "disabled"} {
                    set i [lsearch -exact ${ProjectFiles} ${f}]
                    if {${i} >= 0} {
                        set ProjectFiles [lreplace ${ProjectFiles} ${i} ${i}]
                    }
                } else {
                    #delete files from the hidden list
                    set i [lsearch -exact ${HiddenFiles} ${f}]
                    if {${i} >= 0} {
                        set HiddenFiles [lreplace ${HiddenFiles} ${i} ${i}]
                    }
                }
            }
            Display_ProjectFiles
            ${tree} see ${y}
        }\
        elseif {${cmd} == "View"} {
            eval lappend ProjectFiles ${files}

            #view files
            foreach f ${files} {
                #delete files from hidden list
                if {${hide_state} == "disabled"} {
                    set i [lsearch -exact ${HiddenFiles} ${f}]
                    if {${i} >= 0} {
                        set HiddenFiles [lreplace ${HiddenFiles} ${i} ${i}]
                    }
                } else {
                    #delete files from the unloaded list
                    set i [lsearch -exact ${files_to_unload} ${f}]
                    if {${i} >= 0} {
                        set files_to_unload [lreplace ${files_to_unload} ${i}\
                          ${i}]
                    }
                }
            }
            Display_ProjectFiles
            ${tree} see ${y}
        } else {
            bell
            return
        }
        set can_apply 1
        control_buttons
    }

    method rename_file {file y} {
        global sn_root
        set children [${treew} children ${y}]
        if {${children} != ""} {
            set files [build_file_list ${treew} ${Tree_Mode} ${y}]
        } else {
            set files ${file}
        }

        set nname [sn_prompt_for_files [get_indep String Rename] [list\
          [list ${file} ${file}]]]
        if {${nname} == ""} {
            return ""
        }
        set nname [lindex ${nname} 0]
        if {[string first ${sn_root} ${nname}] != -1} {
            bell
            return
        }
    }

    method tree_toggle {tr x y} {
        global projtree
        global ${this}.msg
        set ret [${tr} identify ${x} ${y}]
        set idx [${tr} nearest ${y}]
        set $itk_component(hull).msg [build_filepath ${treew} ${Tree_Mode} ${idx}]

        #no items or no tree mode
        if {[${tr} size] < 0} {
            return
        }

        #verify if the icon of a sub tree is clicked
        if {${ret} != "text"} {
            #save info that this sub directory is closed
            set path [build_filepath ${treew} ${Tree_Mode} ${idx} -1]
            if {${ret} == "view"} {
                set projtree(${Tree_Mode}/${path}) 1
            } else {
                ::catch {unset projtree($Tree_Mode/$path)}
            }

            set img [${tr} itemcget ${idx} -image]
            #it's a directory
            if {[string first "dir" ${img}] == 0} {
                ${tr} toggle @${x},${y}
                return
            }
            #it's a file (view/hide)
            return
        }
    }

    method post_views {cmb} {
    }

    method change_view {cmb view} {
        upvar #0 ${this}-view vw
        if {${view} == ""} {
            set vw "default"
            set view ${vw}
        }
        if {${CurrentView} == ${view}} {
            return
        }
        #if the current view is changed, ask if the
        #user wants to apply the changes to the current view
        if {${can_apply}} {
            set answer [tk_dialog auto [get_indep String View]\
              "${CurrentView}: [get_indep String HasBeenModified]"\
              question_image 0 [get_indep String Apply] [get_indep String\
              Cancel]]
            if {${answer} != 0} {
                set vw ${CurrentView}
                return
            }

            set cur_view ${view}
            set vw ${CurrentView}

            #apply changes
            apply

            set vw ${cur_view}
        }

        #verify if the view is already availiable
        set found 0
        foreach v ${ProjectViews} {
            if {[lindex ${v} 0] == ${view}} {
                set found 1
                break
            }
        }

        if {${found} == 0} {
            set answer [tk_dialog auto [get_indep String View] "${view}:\
              [get_indep String AskForNewView]" question_image 0\
              [get_indep String Yes] [get_indep String Cancel]]
            if {${answer} != 0} {
                set vw ${CurrentView}
                return
            }
        }

        #change to an existing view
        set CurrentView ${view}
        sn_change_view ${view}
        ReadProjectFiles
    }

    method ReadProjectFiles {{reset 1}} {
        if {${reset}} {
            catch {unset ProjectFiles}
        }

        if {$itk_option(-new_project)} {
            set ProjectFiles $itk_option(-new_ProjectFiles)
            set ProjectViews "default"
            set CurrentView "default"
            set HiddenFiles ${new_HiddenFiles}
            set files_to_unload ""
        }\
        elseif {![info exists ProjectFiles]} {
            set ProjectFiles [sn_project_file_list 0]

            #save this sort of files to be ignored by applying the
            #changes to the project
            set orig_ProjectFiles ${ProjectFiles}

            if {[::info commands paf_db_proj] != ""} {
                set ProjectViews [paf_db_proj get -key views]
                set CurrentView [paf_db_proj get -key db_exclude]
            } else {
                set ProjectViews ""
                set CurrentView ""
            }
            if {${CurrentView} == ""} {
                set CurrentView "default"
            }
            set HiddenFiles [project_excluded_file_list]
            set files_to_unload ""
        }

        if {${reset}} {
            # Set last_dir to first directory on file list.
            set default [file dirname [lindex $ProjectFiles 0]]
            if {[file exists $default] == 1} {
                set last_dir $default
            }
        }

        #Project views
        if {${ProjectViews} == ""} {
            set ProjectViews [list default ""]
        }
        #Only view names
        set Views ""
        foreach view ${ProjectViews} {
            lappend Views [lindex ${view} 0]
        }

        set can_apply 0
        control_buttons
    }

    method Refresh {} {
        #refresh views
        if {[winfo exists ${Views_Combo}]} {
            ${Views_Combo} configure -contents ${Views}
            ${Views_Combo} selecttext ${CurrentView}
            #$Views_Combo see 0
        }

        #Add project files into tree
        Display_ProjectFiles
    }

    method Display_ProjectFiles {{cls ""} {dummy ""}} {
        global sn_options
        global sn_path
        global Avail_Parsers Parser_Info

        set filter [${tree} getfilter]

        if {${filter} == "*"} {
            set pfiles [lsort ${ProjectFiles}]
            set hfiles [lsort ${HiddenFiles}]
            set ufiles [lsort ${files_to_unload}]
        } else {
            set pfiles [lsort [lmatch -glob 0 ${ProjectFiles} *${filter}]]
            set hfiles [lsort [lmatch -glob 0 ${HiddenFiles} *${filter}]]
            set ufiles [lsort [lmatch -glob 0 ${files_to_unload} *${filter}]]
        }

        switch -- ${Tree_Mode} {
            "tree" {
                    set dir_bitmap dir_image
                    set file_bitmap file_image

                    #delete old entries
                    ${treew} delete 0 end

                    #add project name as first icon
                    ${treew} insert 0 -text [get_indep String ProjectFiles]\
                      -image dir_image

                    fill_file_tree -widget ${treew} -fileimage file_s_image\
                      -dirimage dir_image -contents ${pfiles} -parent 0\
                      -basedir ${disp_Directories}

                    #Hidden files, if availiable
                    if {${HiddenFiles} != ""} {
                        set img dir_image
                        set hi [${treew} insert end -text [get_indep String\
                          HiddenFiles] -image ${img}]
                        fill_file_tree -widget ${treew}\
                          -fileimage file_h_image -dirimage dir_image\
                          -contents ${hfiles} -parent ${hi}\
                          -basedir ${disp_Directories}
                    }

                    if {${files_to_unload} != ""} {
                        set img dir_image
                        set ui [${treew} insert end -text [get_indep String\
                          UnloadedFiles] -image ${img}]
                        fill_file_tree -widget ${treew}\
                          -fileimage file_d_image -dirimage dir_image\
                          -contents ${ufiles} -parent ${ui}\
                          -basedir ${disp_Directories}

                    }
                }
            "list" {
                    #display an easy file list
                    #delete old entries
                    ${treew} delete 0 end

                    #add project name as first icon
                    ${treew} insert end -text [get_indep String ProjectFiles]\
                      -image dir_image
                    #$treew insert end list $pfiles -parent 0\
                      -image file_s_image
                    add_list_to_project ${treew} 0 file_s_image\
                      ${disp_Directories} ${pfiles}

                    #Hidden files, if availiable
                    if {${HiddenFiles} != ""} {
                        set hi [${treew} insert end -text [get_indep String\
                          HiddenFiles] -image dir_image]
                        #$treew insert end list $hfiles -parent $hi\
                          -image file_h_image
                        add_list_to_project ${treew} ${hi} file_h_image\
                          ${disp_Directories} ${hfiles}
                    }

                    #unloaded files, if availiable
                    if {${files_to_unload} != ""} {
                        set hi [${treew} insert end -text [get_indep String\
                          UnloadedFiles] -image dir_image]
                        #$treew insert end list $ufiles -parent $hi\
                          -image file_d_image
                        add_list_to_project ${treew} ${hi} file_h_image\
                          ${disp_Directories} ${ufiles}
                    }
                }
            "cat" {
                    #display files in groups

                    #get known types and extensions to sort
                    set types ""
                    foreach type ${Avail_Parsers} {
                        lappend types [list $Parser_Info(${type},TYPE)\
                          $Parser_Info(${type},SUF)]
                    }
                    set types [::lsort -dictionary ${types}]

                    #delete old entries
                    ${treew} delete 0 end

                    #add project name as first icon
                    ${treew} insert end -text [get_indep String ProjectFiles]\
                      -image dir_image
                    AddFilesAsCategorie ${treew} ${types} 0 ${pfiles}\
                      file_s_image ${disp_Directories}

                    #Hidden files, if availiable
                    if {${HiddenFiles} != ""} {
                        set hi [${treew} insert end -text [get_indep String\
                          HiddenFiles] -image dir_image]
                        AddFilesAsCategorie ${treew} ${types} ${hi} ${hfiles}\
                          file_h_image ${disp_Directories}
                    }

                    #unloaded files, if availiable
                    if {${files_to_unload} != ""} {
                        set hi [${treew} insert end -text [get_indep String\
                          UnloadedFiles] -image dir_image]
                        AddFilesAsCategorie ${treew} ${types} ${hi} ${ufiles}\
                          file_d_image ${disp_Directories}
                    }
                }
        }

        project_CloseAlreayClosed ${treew} ${Tree_Mode}
    }

    method toggle_disp_mode {} {
        upvar #0 ${this}-Tree_Mode tm

        if {${tm} == "tree"} {
            ${Toolbar}.dirs config -state disabled
        } else {
            ${Toolbar}.dirs config -state normal
        }
        #change tabs to delete unneeded tabs
        if {${tm} == "tree" || ${disp_Directories} == 0} {
            ${tree} change_tabs 0 250 Name
        } else {
            ${tree} change_tabs 1 {250 250} {Name Directory}
        }
        if {${Tree_Mode} != ${tm}} {
            set Tree_Mode ${tm}
            Display_ProjectFiles
        }
    }

    method undo {} {
        #ReadProjectFiles
        set x_Undo_HiddenFiles ${HiddenFiles}
        set x_Undo_files_to_unload ${files_to_unload}
        set x_Undo_ProjectFiles ${ProjectFiles}

        set ProjectFiles ${Undo_ProjectFiles}
        set HiddenFiles ${Undo_HiddenFiles}
        set files_to_unload ${Undo_files_to_unload}

        set Undo_HiddenFiles ${x_Undo_HiddenFiles}
        set Undo_files_to_unload ${x_Undo_files_to_unload}
        set Undo_ProjectFiles ${x_Undo_ProjectFiles}

        Display_ProjectFiles
    }

    proc can_create_project {prjdir prjname} {
        set name [sn_build_project_filename ${prjdir} ${prjname}]

        set ret [sn_is_project_busy ${name} in remuser remhost port pid]
        switch -- ${ret} {
            "othersystem" {
                    sn_error_dialog [format \
                        [get_indep String ProjAlreadyOpenedOtherSystem] \
                        ${remuser} ${name} ${remhost}]
                    return 0
            }
            "thisprocess" {
                    sn_error_dialog [format \
                        [get_indep String ProjAlreadyOpenedThisProcess] \
                        ${name}]
                    return 0
            }
            "thisuser" {
                    sn_error_dialog [format \
                        [get_indep String ProjAlreadyOpenedThisUser] \
                        ${name} ${pid}] 
                    return 0
            }
            "thissystem" {
	            sn_error_dialog [format \
                        [get_indep String ProjAlreadyOpenedThisSystem] \
                        ${remuser} ${name} ${pid}]
                    return 0
            }
            "error" {
                    return 0
            }
        }

        if {[file exists ${name}]} {
            set answer [tk_dialog auto [get_indep String FileExists] ${name}\
              question_image 0 [get_indep String Overwrite] [get_indep String\
              cancel]]
            if {${answer} == 1} {
                return 0
            } else {
                #delete existing project
                set ret [sn_delete_project ${name}]
                #unable to delete the project
                if {${ret} == 0} {
                    sn_error_dialog [get_indep String CannotDeleteProject]
                    return 0
                }
            }
        }
        return 1
    }

    method apply {{exit ""}} {
        global sn_options
        global sn_newargs

        upvar #0 ${this}-view view
        if {${view} == ""} {
            set view "default"
        }
        set CurrentView ${view}

        if {$itk_option(-new_project)} {
            set prjdir [file dirname [sn_filecmd format -sn $sn_newargs(path)]]
            set prjname [file tail [sn_filecmd format -sn $sn_newargs(path)]]

            #valid project name ??
            if {${prjname} == ""} {
                sn_error_dialog [get_indep String InvalidProjectName]
                focus [${prjname_Entry} entry]
                return
            }

            #valid project name ??
            if {[catch {set realprjdir [realpath -pwd [pwd] ${prjdir}]} err]} {
                sn_error_dialog "${prjdir}: ${err}"
                focus [${prjname_Entry} entry]
                return
            }

            #Does the directory exist?
            if {![file isdirectory ${prjdir}]} {
                sn_error_dialog [get_indep String InvalidProjectDir]
                focus [${prjdir_Entry} entry]
                return
            }

            #verify if the file exists or is being used
            if {! [can_create_project ${realprjdir} ${prjname}]} {
                return
            }
            #now we can set project file
            set sn_options(sys,project-file) [sn_filecmd format\
              -sn $sn_newargs(path)]

            #project directory can be only changed by new projects
            #accept only full pathes
            set sn_options(sys,project-dir) ${realprjdir}

            set itk_option(-new_ProjectFiles) ${ProjectFiles}
            set new_HiddenFiles ${HiddenFiles}
        } else {

            #add all directory prefixes to the path list
            set dirs [generate_pathes ${ProjectFiles}]
            foreach dir ${dirs} {
                if {[lsearch -exact $sn_options(include-source-directories)\
                  ${dir}] == -1} {
                    lappend sn_options(include-source-directories) ${dir}
                }
            }

            #we should here only pass the new files that have been added
            #to the project, not all the files that are alreay in the
            #project to reduce the loading time (over SAMBA it reduces
            #some minutes for hug projects)
            if {${orig_ProjectFiles} != ""} {
                set new_files ""
                foreach f ${ProjectFiles} {
                    if {[lsearch -exact ${orig_ProjectFiles} ${f}] == -1} {
                        lappend new_files ${f}
                    }
                }
            } else {
                set new_files ${ProjectFiles}
            }

            set ret [sn_load_hide_unload_files $sn_options(sys,project-dir)\
              ${new_files} ${HiddenFiles} ${files_to_unload} ${CurrentView}]
            if {!${ret}} {
                #operation not applied
                return
            }
        }

        set can_apply 0
        control_buttons

        if {$itk_option(-variable) != ""} {
            global $itk_option(-variable)
            set $itk_option(-variable) "ok"
        } elseif {${exit} == "exit"} {
            itcl::delete object $this
        }
        hide_loading_message
    }

    protected variable last_dir [pwd]

    method add_files_cb {files} {
        global sn_options tcl_platform

        #undo history
        if {[llength $files] > 0} {
            set Undo_ProjectFiles ${ProjectFiles}
            set Undo_HiddenFiles ${HiddenFiles}
            set Undo_files_to_unload ${files_to_unload}
        }

        #don't differ between upper/lower case on windows
        if {$tcl_platform(platform) == "windows"} {
            set nocase 1
        } else {
            set nocase 0
        }

        # Create map for ProjectFiles, HiddenFiles, and files_to_unload

        foreach lname {ProjectFiles HiddenFiles files_to_unload} {
            if {$nocase} {
                foreach file [set $lname] {
                    set ${lname}_MAP([string tolower $file]) {}
                }
            } else {
                foreach file [set $lname] {
                    set ${lname}_MAP($file) {}
                }
            }
        }

        foreach f ${files} {
            if {! $itk_option(-new_project)} {
                set f [sn_truncate_path $sn_options(sys,project-dir) ${f}]
            }
            if {$nocase} {
                set nc [string tolower $f]
            } else {
                set nc $f
            }
            if {[info exists ProjectFiles_MAP($nc)] ||
                [info exists HiddenFiles_MAP($nc)] ||
                [info exists files_to_unload_MAP($nc)]} {
                continue
            }
            lappend ProjectFiles ${f}
            set ProjectFiles_MAP($nc) {}
            set can_apply 1
        }

        #store last directory
        set last_dir [file dirname [lindex ${files} 0]]

        control_buttons
        Display_ProjectFiles
    }
    method add_files {} {
        Editor&::FileDialog $itk_component(hull) -title [get_indep String Open]\
          -script "${this} add_files_cb" -prefix add_files\
          -save_open open -initialdir ${last_dir}
    }

    method add_dir_cb {dir} {
        global sn_options
        global Avail_Parsers Parser_Info

        #scanning window
        sn_loading_message

        # Concatenate the known file extensions!
        set glob_expr ""
        foreach type ${Avail_Parsers} {
            set ex $Parser_Info(${type},SUF)
            foreach e ${ex} {
                if {[string index ${e} 0] != "*"} {
                    set e "*${e}"
                }
                if {${e} != ""} {
                    lappend glob_expr [list ${e}]
                }
            }
        }
        set glob_expr [lunique [lsort [join ${glob_expr}]]]
        set glob_expr [join ${glob_expr}]

        #read the files from the directory
        set fil_list [sn_glob -dirvar dirnames -match ${glob_expr} -dirlevel\
          -1 -ignore $sn_options(def,ignored-directories)\
          -updatecommand "sn_glob_updatecommand" ${dir}]

        add_files_cb ${fil_list}

        #hide scanning window
        hide_loading_message
    }

    method add_directory {} {
        Editor&::DirDialog ${this} -script "${this} add_dir_cb"\
          -initialdir ${last_dir}
    }

    #load files from other existing projects
    method add_from_project_cb {f} {
        global sn_options
        if {[catch {set fd [dbopen db_proj ${f} RDONLY [sn_db_perms] hash]}\
          err]} {
            sn_error_dialog ${err}
            return 0
        }
        set ProjectDir [file dirname ${f}]
        catch {cd ${ProjectDir}}
        set files_prefix [sn_project_database_prefix db_proj]

        if {[catch {set f_fd [dbopen db_f ${files_prefix}.f RDONLY\
          [sn_db_perms] btree]} err]} {
            ${fd} close
            sn_error_dialog ${err}
            return 0
        }
        set files [db_f seq -col 0]
        ${fd} close
        ${f_fd} close

        #add directory name to files if they haven't any prefix
        set nfiles ""
        foreach f ${files} {
            if {[file pathtype ${f}] != "absolute"} {
                set f [file join ${ProjectDir} ${f}]
            }
            lappend nfiles ${f}
        }

        add_files_cb ${nfiles}
    }

    method add_from_project {} {
        global sn_options

        Editor&::FileDialog ${this} -title [get_indep String Open]\
          -script "${this} add_from_project_cb" -initialdir ${last_dir}\
          -extensions $sn_options(project_extensions) -defaultextension ".proj"
    }

    method contents {} {
        return $itk_option(-new_ProjectFiles)
    }
    method hidden {} {
        return ${new_HiddenFiles}
    }

    method windows_close {} {
        if {[Close]} {
            if {$itk_option(-variable) != ""} {
                global $itk_option(-variable)
                set $itk_option(-variable) "cancel"
            } else {
                itcl::delete object ${this}
            }
        }
    }

    method Update_Layout {} {
        global sn_options
        ${tree} config -font $sn_options(def,default-font)\
          -selectforeground $sn_options(def,select-fg)\
          -selectbackground $sn_options(def,select-bg)
    }

    method Refresh_Display {} {
        ReadProjectFiles 1
        Refresh
    }

    #verify if all buffers can be reverted.
    proc CloseAll {} {
# FIXME: ugly itcl::find hack
        foreach win [itcl::find objects -class Project&] {
            if {[${win} Close] == 0} {
                return 0
            }
        }
        return 1
    }
# FIXME: This is a terrible name, what is this "Close" and "CloseAll" stuff for?
    method Close {} {
        #window can be closed
        return 1
    }

    #This function saves the status of all opened sym. br.
    #in the project file.
    proc SaveYourSelf {} {
    }

    #This function restores all the windows saved in the
    #project file
    proc RestoreYourSelf {} {
    }

    #refresh the project windows (max. one !!)
    proc Refresh_YourSelf {} {
        foreach prj [itcl::find objects -class Project&] {
            ${prj} Refresh_Display
        }
    }

    protected variable Toolbar ""
    protected variable Statusbar ""
    protected variable Reuse ""
    protected variable tree "."
    protected variable treew "."
    protected variable prjname_Entry "."
    protected variable prjdir_Entry "."

    protected variable Views_Combo "never exists"

    protected variable view_state normal
    protected variable unload_state normal
    protected variable hide_state normal

    protected variable btnsfr
    protected variable filefr
    protected variable can_apply 0

    protected variable Undo_HiddenFiles ""
    protected variable Undo_files_to_unload ""
    protected variable Undo_ProjectFiles ""

    common ProjectFiles ""
    common orig_ProjectFiles ""
    #with db filenames
    common ProjectViews ""
    #without db filenames
    common Views ""
    common CurrentView ""
    common HiddenFiles ""
    common files_to_unload ""

#    public variable new_ProjectFiles ""
    itk_option define -new_ProjectFiles new_ProjectFiles New_ProjectFiles "" 
    public variable new_HiddenFiles ""

    #can be
    #tree: display file names as tree
    #categorie: display file names as categories
    #list: display filenames as easy list
    public variable Tree_Mode "tree"
    public variable x ""
    public variable y ""
#    public variable new_project 0
    itk_option define -new_project new_Project New_Project "0"
    itk_option define -variable variable Variable ""
}

proc build_filepath {treew Tree_Mode y {upto 0}} {
    set file [${treew} get ${y}]
    switch -- ${Tree_Mode} {
        "tree" {
                set parent [${treew} itemcget ${y} -parent]
                for {set i ${parent}} {[${treew} level ${i}] > ${upto} && ${i}\
                  >= 0} {set i [${treew} itemcget ${i} -parent]} {
                    set file [file join [${treew} get ${i}] ${file}]
                }
                return ${file}
            }
        "cat" {
                #nothing to do
            }
        "list" {
                #nothing to do
            }
    }
    #/
    #  view-mode has the format "<file>\t<dir>"
    #/
    if {[string first "\t" ${file}] != -1} {
        set x [split ${file} \t]
        set d [lindex ${x} end]
        if {${d} != ""} {
            set file [file join ${d} [lindex ${x} 0]]
        } else {
            set file [lindex ${x} 0]
        }
        set file [sn_convert_FileName ${file}]
    }
    return ${file}
}

#build file names of the selected entries, including
#tree sub items
proc build_file_list {treew treemode sels} {
    set files ""
    foreach y ${sels} {
        set children [${treew} children ${y}]
        if {${children} != ""} {
            foreach ch ${children} {
                set ff [build_file_list ${treew} ${treemode} ${ch}]
                if {${ff} != ""} {
                    foreach f ${ff} {
                        if {[lsearch -exact ${files} ${f}] == -1} {
                            lappend files ${f}
                        }
                    }
                }
            }
        }\
        elseif {[${treew} levels ${y}] > 0} {
            set f [build_filepath ${treew} ${treemode} ${y}]
            if {[lsearch -exact ${files} ${f}] == -1} {
                lappend files ${f}
            }
        }
    }
    return ${files}
}

proc add_list_to_project {treew parent image basedir cnt} {
    global sn_options
    global sn_root
    if {! ${basedir}} {
        ${treew} insert end list ${cnt} -parent ${parent} -image ${image}
    } else {
        global tcl_platform
        if {$tcl_platform(platform) == "windows"} {
            set from 1
            set to 2
            set cmp ":${sn_root}"
        } else {
            set from 0
            set to 0
            set cmp ${sn_root}
        }
        set ff ""
        foreach f ${cnt} {
            set d [file dirname ${f}]
            if {${d} == "."} {
                set d $sn_options(sys,project-dir)
            }
            if {[string range ${d} ${from} ${to}] != ${cmp}} {
                set d "\t[file join $sn_options(sys,project-dir) ${d}]"
            } else {
                set d "\t${d}"
            }
            lappend ff "[file tail ${f}]${d}"
        }
        #we must resort files again, because "array names" does
        #unsort a sorted list!!
        ${treew} insert end list [lsort -dictionary ${ff}] -parent ${parent}\
          -image ${image}
    }
}

proc AddFilesAsCategorie {treew types parent cnt image basedir} {
    global sn_options
    global sn_root
    #add files as categories
    foreach f ${cnt} {
        set fnd 0
        #find categorie
        foreach l ${types} {
            set type [lindex ${l} 0]
            set ext [lindex ${l} end]
            foreach e ${ext} {
                if {[string match ${e} ${f}]} {
                    lappend files(${type}) ${f}
                    set fnd 1
                    break
                }
            }
            if {${fnd}} {
                break
            }
        }
        #no categorie
        if {! ${fnd}} {
            lappend files(uncategorized) ${f}
        }
    }

    #add found categories
    foreach nm [lsort -dictionary [array names files]] {
        set ni [${treew} insert end -text ${nm} -image dir_image\
          -parent ${parent}]
        if {${basedir}} {
            global tcl_platform
            if {$tcl_platform(platform) == "windows"} {
                set from 1
                set to 2
                set cmp ":${sn_root}"
            } else {
                set from 0
                set to 0
                set cmp ${sn_root}
            }
            set ff ""
            foreach f $files(${nm}) {
                set d [file dirname ${f}]
                if {${d} == "."} {
                    set d $sn_options(sys,project-dir)
                }
                if {[string range ${d} ${from} ${to}] != ${cmp}} {
                    set d "\t[file join $sn_options(sys,project-dir) ${d}]"
                } else {
                    set d "\t${d}"
                }
                lappend ff "[file tail ${f}]${d}"
            }
            #we must resort files again, because "array names" does
            #unsort a sorted list!!
            ${treew} insert end list [lsort -dictionary ${ff}] -parent ${ni}\
              -image ${image}
        } else {
            ${treew} insert end list $files(${nm}) -parent ${ni} -image ${image}
        }
    }
}

#look for the from user alreay hidden subtrees and hide thim before
#displaying the tree on the screen, this helps the user adding
#his files comfortable
proc project_CloseAlreayClosed {treew Tree_Mode} {
    global projtree sn_root
    set size [${treew} size]
    if {${size} == 0} {
        catch {unset projtree}
        return
    }
    set names [lsort -dictionary [array names projtree "${Tree_Mode}/*"]]
    if {${names} == ""} {
        return
    }
    foreach name ${names} {
        set start 0
        set basename [file tail ${name}]
        while {1} {
            set idx [${treew} search -realindex -exact -- ${basename} ${start}\
              end]
            if {${idx} == ""} {
                #rootnames with full path contain "/" on there name
                set f [file nativename [file join / ${basename}]]
                set idx [${treew} search -realindex -exact -- ${f} ${start} end]
            }
            if {${idx} == ""} {
                break
            }
            if {"${Tree_Mode}/[build_filepath ${treew} ${Tree_Mode} ${idx}\
              -1]" == ${name}} {
                #hide this sub tree
                ${treew} toggle ${idx} hide
                break
            }
            set start [expr ${idx} + 1]
            if {${start} >= ${size}} {
                break
            }
        }
    }
}

proc project_editor {} {
    set t .project_editor
    if {[winfo exists ${t}]} {
        ${t} raise
    } else {
        Project& ${t}
    }
}

