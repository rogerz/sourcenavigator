# Copyright (c) 2000, Red Hat, Inc.
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
# projmng.tcl - Implement several operations on projects.
# Copyright (C) 1998 Cygnus Solutions.

# FIXME: these procs ought to become methods of a Project class.

# This function reads the name of the used and still existing
# project file names.
proc sn_read_exist_projects {{truncate 0}} {
    global sn_history
    global sn_projects_list
    global sn_options
    global tcl_platform

    if {![info exists sn_history(project_size)]} {
        set sn_history(project_size) 10
    }

    set pf $sn_projects_list(filename)

    if {[info exists sn_projects_list(mtime)] && $sn_projects_list(mtime) ==\
      [file mtime ${pf}]} {
        if {${truncate}} {
            return [lrange $sn_projects_list(names) 0 $sn_history(project_size)]
        }
        return $sn_projects_list(names)
    }

    #read projects from the project file
    if {[catch {set prjfd [open ${pf}]}]} {
        return ""
    }
    fconfigure ${prjfd} -encoding $sn_options(def,system-encoding) -blocking 0
    set projs [split [read -nonewline ${prjfd}] "\n"]
    close ${prjfd}

    set sn_projects_list(mtime) [file mtime ${pf}]

    set exist_projs ""
    catch {unset scaned}
    foreach pr ${projs} {
        if {[file isfile ${pr}] && [file readable ${pr}]} {
            #make sure that the project file hasn't been added to the list,
            #this could happen, when files are stored with different roots
            #on windows  "C:/foo" and "C:\foo"
            set f [file nativename ${pr}]

            if {$tcl_platform(platform) == "windows"} {
                set scanf [string tolower ${f}]
            } else {
                set scanf ${f}
            }
            if {[info exists scaned(${scanf})]} {
                continue
            }
            #append original file
            lappend exist_projs ${f}
            set scaned(${scanf}) 1
        }
    }
    catch {unset scaned}

    set sn_projects_list(names) ${exist_projs}

    # Truncate if neceassary!
    if {${truncate}} {
        set exist_projs [lrange ${exist_projs} 0 $sn_history(project_size)]
    }

    return ${exist_projs}
}


#delete a SN project, given by project name
proc sn_delete_project {prjname {ask ""}} {
    global sn_options

    if {${ask} != ""} {
        set answer [tk_dialog auto [get_indep String ProjectDelete]\
          "[get_indep String DeleteProjectQuestion] \"${prjname}\" ?"\
          question_image 0 [get_indep String Yes] [get_indep String No]]
        if {${answer} != 0} {
            return -1
        }
    }

    sn_log "delete project <${prjname}>"

    #open the project file
    set prj_error [catch {set prj_descr [dbopen prj_db ${prjname} RDONLY\
      [sn_db_perms] hash]}]

    #init some error/debug routines
    set delerr 0
    set err ""

    #change to the project directory
    if {! ${prj_error}} {
        set prjDir [file dirname ${prjname}]
        catch {cd ${prjDir}}

        #read database filename prefix
        set db_prefix [sn_project_database_prefix prj_db]

        sn_log "database prefix <${db_prefix}>"

        #read symbol database directory
        set wd [sn_project_database_dir prj_db]

        if {${wd} == "" || ${db_prefix} == "" || [file pathtype ${wd}] !=\
          "absolute"} {
            set wd ""
        }

        #check wether the database is locked
        set ret [sn_is_project_busy ${prjname} in user host port pid]
# FIXME: This would allow the delete of locked project files!
        set ret 0

        switch -- ${ret} {
            "othersystem" {
                    sn_error_dialog [format \
                        [get_indep String ProjAlreadyOpenedOtherSystem] \
                        ${user} ${prjname} ${host}]
                    set ret 1
            }
            "thisprocess" {
                    sn_error_dialog [format \
                        [get_indep String ProjAlreadyOpenedThisProcess] \
                        ${prjname}]
                    set ret 1
            }
            "thisuser" {
                    sn_error_dialog [format \
                        [get_indep String ProjAlreadyOpenedThisUser] \
                        ${prjname} ${pid}] 
                    set ret 1
            }
            "thissystem" {
	            sn_error_dialog [format \
                        [get_indep String ProjAlreadyOpenedThisSystem] \
                        ${user} ${prjname} ${pid}]
                    set ret 1
            }
            "error" {
                    # ignore error
            }
        }

        #project can't be deleted
        if {${ret}} {
            sn_log "project can't be deleted"
            return 0
        }

        #open db files
        set prj_db_error [catch {set prj_f_descr [dbopen prj_db_f\
          ${db_prefix}.f RDONLY [sn_db_perms] btree]}]
        if {!${prj_db_error}} {
            #delete highlighting files
            if {![catch {set fls [prj_db_f seq -col 3\
              -result {*[a-zA-Z0-9_]*}]}]} {
                sn_log "delete related files <${fls}>"
                catch {eval file delete -- ${fls}}
            } else {
                sn_log "no database files found"
            }

            #close filenames database
            ${prj_f_descr} close
        }

        #delete temporary files
        if {${wd} != ""} {
            set fls [glob -nocomplain [file join ${wd} *.html] [file join\
              ${wd} dbimp_*] [file join ${wd} tmp_*]]
            if {${fls} != ""} {
                sn_log "delete temporary files <${fls}>"
                catch {eval file delete -- ${fls}}
            }
        }

        #delete all project related files (database files)
        set del_fls [glob -nocomplain ${db_prefix}.*]
        if {${del_fls} != ""} {
            sn_log "delete project database files <${del_fls}>"
            set delerr [catch {eval file delete -- ${del_fls}} err]
        } else {
            sn_log "no project database files found!"
            set delerr 0
        }

        if {${wd} != ""} {
            sn_log "try to delete database directory (if empty) <${wd}>"
            catch {file delete -- ${wd}}
            # It might not be empty.
        }

        #close the project file
        ${prj_descr} close

        #change to the current project directory
        catch {cd $sn_options(sys,project-dir)}
    }

    sn_log "delete project file <${prjname}>"

    #delete the project file itself.
    set prjdelerr [catch {file delete -- ${prjname}} prjerr]

    if {${delerr}} {
        sn_error_dialog ${err}
    }\
    elseif {${prjdelerr}} {
        sn_error_dialog ${prjerr}
    }

    sn_log "project <${prjname}> deleted"

    return 1
}

proc sn_delete_project_cb {btns t} {
    set idx [${t} curselection]
    if {${idx} == ""} {
        return
    }
    set prj [${t} get ${idx}]
    if {${prj} == ""} {
        bell
        return
    }
    set ret [sn_delete_project ${prj} ask_to_delete]
    if {${ret} == -1} {
        return
    }
    if {${ret} == 0} {
        sn_error_dialog [get_indep String CannotDeleteProject]
        return
    }

    #delete entry from the list
    ${t} delete ${idx}

    ${btns}.open config -state disabled
    ${btns}.delete config -state disabled
}

proc sn_new_project {} {
    create_interp {
        wm withdraw .
	sn_start_new_session --create
    }
}

#starts the project selector in a new interpreter, the
#interpreter is deleted, when the user cancels selection
#the selector is only called, when no other interp. are
#started.
proc sn_projectmanager {} {
    global tkeWinNumber
    global proj_selector_status
    if {[number_interp] == 1} {
        #create an interpreter and return immediatly to proceed
        #with the existing interpreter
        create_interp {
				wm withdraw .
				if {[sn_select_project nowait] == 0} {
				        sn_log "deleting interp because sn_select_project \
					    returned 0, in sn_projectmanager"
					delete_interp
				}
	}
    }
}

#waits for xref to be built
proc sn_is_waiting_for_xref {} {
    global sn_wait_xref_flag

    if {! [sn_processes_running]} {
        set sn_wait_xref_flag "finished"
        return
    }
    update idletasks

    after 1000 sn_is_waiting_for_xref
}

#create a new project
proc sn_new_project_cb {{t ""}} {
    global sn_options
    global sn_arguments
    global ProcessingCancelled

    sn_log "try to create project..."

    if {${t} != ""} {
        ${t} withdraw
    }

    if {![info exists sn_arguments(import-file)]} {
        set sn_arguments(import-file) ""
    }

    #create the project
    set ret [sn_create_new_project $sn_arguments(import-file)]
    if {${ret} == 0 || ${ProcessingCancelled}} {
        if {${ret} == 0 && ![sn_batch_mode]} {
            # If a half-created project is sitting around, delete it!
            if {[file exists $sn_options(sys,project-file)]} {
                sn_delete_project $sn_options(sys,project-file)
            }
        }
    
        if {${t} != ""} {
            window_configure ${t} deiconify
        }
        update idletasks
        set ProcessingCancelled 0
        return 0
    }

    if {${t} != ""} {
        global proj_selector_status
        destroy ${t}
        set proj_selector_status "open"
    }

    sn_log "new project has been created <1>"

    return 1
}

#opens a project, given by project name
proc sn_open_project_cb {pfile} {
    global ProcessingCancelled

    sn_log "open project <${pfile}>"

    sn_loading_message

    set ret [sn_read_project ${pfile}]
    if {${ret} != 1} {
        #error, close all db files
        db_close_files
        set ProcessingCancelled 0
        set ret 0
    } else {
        set ret 1
    }

    hide_loading_message

    return ${ret}
}

proc Open_Project {t} {
    global tcl_platform proj_selector_status

    set listbox $t.scrolllistbox.projs

    set p [$listbox curselection]
    if {${p} == ""} {
        return 0
    }
    set prj [$listbox get ${p}]
    if {[string compare ${prj} ""] == 0} {
        set prj [$listbox get active]
    }
    if {${prj} == ""} {
        return 0
    }

    ${t} withdraw

    if {$tcl_platform(platform) == "windows"} {
        regsub -all {\\} ${prj} {/} prj
    }

    sn_log "open project <${prj}>..."

    if {[sn_open_project_cb ${prj}]} {
        set proj_selector_status "open"
        destroy ${t}
        return 1
    } else {
        ${t} deiconify
    }
    return 0
}

proc Browse_For_Project {t} {
    global proj_selector_status

    ${t} withdraw
    set prjname [sn_choose_project ${t}]
    if {${prjname} == ""} {
        ${t} deiconify
        return 0
    }
    if {[sn_open_project_cb ${prjname}]} {
        set proj_selector_status "open"
        itcl::delete object ${t}
    } else {
        ${t} deiconify
        set ProcessingCancelled 1
        return 0
    }
    return 1
}

proc Close_Project_List_cb {{waiting "wait"}} {
    global ProcessingCancelled proj_selector_status
    if {${waiting} == "nowait"} {
        delete_interp
    } else {
        set ProcessingCancelled 1
        set proj_selector_status "cancel"
    }
}

# listbox scroll handling for sn_select_project.
proc scrollListbox {wid first last} {
    $wid set $first $last
    # TODO: When Tk supports enabled/disabled
    # scrollbars, we should enable and disable
    # our scrollbars when they ain't needed.
}

#Displays the projects to select one of them
proc sn_select_project {{waiting "wait"}} {
    global proj_selector_status
    global ProcessingCancelled
    global sn_history
    global tcl_platform

    #read project list
    set proj_list [lsort -dictionary [sn_read_exist_projects]]

    set t .sn_open_proj
    if {[winfo exists ${t}]} {
        sn_log "Raising existing project selection widget"
        ${t} raise
        return ${ProcessingCancelled}
    }

    # Hide the toplevel window until it is ready to be displayed
    sourcenav::Window ${t}
    ${t} withdraw
 
    ${t} on_close [list Close_Project_List_cb ${waiting}]
    ${t} configure -title [sn_title [get_indep String PafOpenProj] 1]
    ${t} configure -iconname [get_indep String ProductName]

    set open_str [get_indep String Open]
    set off [string first "." ${open_str}]
    if {${off} != -1} {
        set open_str [string range ${open_str} 0 [expr ${off} -1]]
    }
    pack [frame ${t}.btns] -side right -fill y -padx 5 -pady 5
    #new project
    button ${t}.btns.new -text [get_indep String NewProj]\
      -underline [get_indep Pos NewProj] -command " sn_new_project_cb ${t} "
    # Open existing
    button ${t}.btns.browse -text [get_indep String BrowseProj]\
      -underline [get_indep Pos BrowseProj] -command " Browse_For_Project\
      ${t} "
    # Open project
    button ${t}.btns.open -text [get_indep String Open] -underline\
      [get_indep Pos Open] -command " Open_Project ${t} "
    #Delete project
    button ${t}.btns.delete -text [get_indep String Delete]\
      -underline [get_indep Pos Delete] -command\
      " sn_delete_project_cb ${t}.btns $t.scrolllistbox.projs "
    # Cancel
    button ${t}.btns.exit -text [get_indep String Quit]\
      -underline [get_indep Pos Cancel] -command [list Close_Project_List_cb\
      ${waiting}]
# FIXME: this should all be gridded
    pack ${t}.btns.new ${t}.btns.browse ${t}.btns.open ${t}.btns.delete\
      ${t}.btns.exit -side top -fill x -pady 5

    set len [llength ${proj_list}]
    if {[catch {set max_len $sn_history(project_size)}]} {
        set max_len 50
    }
    if {${max_len} < ${len}} {
        set height $sn_history(project_size)
    } else {
        set height ${len}
    }
    if {${height} <= 0} {
        set height 6
    }

    sn_log "project list <${proj_list}>"

    if {$tcl_platform(platform) == "windows"} {
        set prjlst ${proj_list}
        set proj_list ""
        foreach prj ${prjlst} {
            sn_log "add project <${prj}, [file nativename ${prj}]>"
            lappend proj_list [file nativename ${prj}]
        }

        sn_log "windows project list <${proj_list}>"
    }

    # Use this frame to hold listbox and scrollbars.

    set f [frame $t.scrolllistbox -relief sunken\
	    -borderwidth 2]

    scrollbar $f.vertsb -orient vertical \
	    -relief flat -borderwidth 1 -highlightthickness 0
    scrollbar $f.horizsb -orient horizontal \
	    -relief flat -borderwidth 1 -highlightthickness 0

    listbox $f.projs -width 55 -relief flat -height $height \
	    -xscrollcommand "scrollListbox $f.horizsb" \
	    -yscrollcommand "scrollListbox $f.vertsb"

    foreach prj ${proj_list} {
        ${f}.projs insert end ${prj}
    }

    $f.vertsb configure -command "$f.projs yview"
    $f.horizsb configure -command "$f.projs xview"

    bind ${f}.projs <ButtonRelease-1> "+
						${t}.btns.open config -state normal
						${t}.btns.delete config -state normal"

    set return_binding "
				${f}.projs selection clear 0 end
				${f}.projs selection set active
				${t}.btns.open config -state normal
				update idletasks
				${t}.btns.open invoke
				break"

    bind ${f}.projs <Return> $return_binding
    bind ${f}.projs <space> $return_binding

    # Wait for ButtonRelease before invoking the open button.
    # Otherwise the ButtonRelease event will be lost and it will appear
    # as if Button-1 is being held down once the project is opened.
 
    bind ${f}.projs <Double-1> "bind $f.projs <ButtonRelease-1>\
                                \"${t}.btns.open invoke; break\";update; break"
    bind ${f}.projs <Escape> "${t}.btns.exit invoke; break"

    grid $f.projs -row 1 -column 1 -sticky news
    grid $f.vertsb -row 1 -column 2 -sticky ns
    grid $f.horizsb -row 2 -column 1 -sticky ew
    grid [frame $f.dummyframe -relief groove -borderwidth 2] -row 2 -column 2 -sticky news
    grid columnconfigure $f 1 -weight 1
    grid rowconfigure $f 1 -weight 1

    focus ${f}.projs

    pack $f -fill both -expand 1 -side left -padx 5 -pady 5

    # Make sure the first item (if any) is selected.
    $f.projs selection set 0

    # Bring up the window to the top and make it visible
    update idletasks
    ${t} centerOnScreen
    ${t} deiconify
    ${t} take_focus ${f}.projs

    # It must be here, otherwise we might
    # get problems on W-95.
    hide_loading_message

    catch {sn_rc_project_list ${t} ${f}.projs}

    set proj_selector_status ""
    #when a new interpreter is created, don't wait to
    #continue proceeding with the CALLING interpreter
    #VERY IMPORTANT
    if {${waiting} != "nowait"} {
        vwait proj_selector_status
        if {${proj_selector_status} == "cancel"} {
            set ProcessingCancelled 1
        }
        if {${ProcessingCancelled}} {
            return 0
        } else {
            return 1
        }
    } else {
        return 1
    }
}



