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
# project.tcl - Procedures for Project Managment
# Copyright (C) 1998 Cygnus Solutions.


#we need to find the correct directory of the
#symbols databases accepting backward compatibility
#to be able to upgrade older projects to new releases
proc sn_project_database_dir {{db_cmd "paf_db_proj"}} {
    set wd [${db_cmd} get -key both,db-directory]
    #backward compatib.
    if {${wd} == ""} {
        set wd [${db_cmd} get -key sn_sym_dir]
    }
    #backward compatib.
    if {${wd} == ""} {
        set wd [${db_cmd} get -key paf_sym_dir]
    }
    return ${wd}
}

#get database prefix
#(including db-dir and database name without extension)
proc sn_project_database_prefix {{db_cmd "paf_db_proj"}} {
    set db_prefix [${db_cmd} get -key db_files_prefix]
    #backward compatib.
    if {${db_prefix} == ""} {
        set db_prefix [${db_cmd} get -key db_files_prefix]
    }
    return ${db_prefix}
}

#this procedure is called after a project has been
#opened/created to finalize some steps to allow starting
#the project in correct state
proc sn_finalize_project {{new "open"}} {
    global sn_options

    #update the scope list
    sn_update_scope_list

    #read the project list and resort it, so that a list
    #of existing projects is built
    sn_update_project_hotlist

    #see if the customer wan't to reparse the project
    if {${new} == "open" && $sn_options(sys,reparse)} {
        Preferences&::Reparse dontask
    }

    if {[sn_batch_mode]} {
        return 0
    }

    if {${new} == "open"} {
        sn_loading_message [get_indep String RestoreWindows]
    } else {
        sn_loading_message
    }

    #restore saved windows, if nothing is saved, create
    #default window with the retriever and files are selected
    MultiWindow&::RestoreYourSelf

    #open a default window
    MultiWindow&::Default_Window

    after idle "hide_loading_message"

    return 0
}

# returns all files in the project of the current view
proc sn_project_file_list {{full 1}} {
    if {[info commands paf_db_f] == ""} {
        if {[info commands paf_db_proj] != ""} {
            set files [paf_db_proj get -key source_files]
        } else {
            set files ""
        }
    }\
    elseif {${full}} {
        #set files [paf_db_f seq -regexp {.*[^/]$} -col [list 0 1 2]]
        set files [paf_db_f seq -col {0 1 2}]
    } else {
        #set files [paf_db_f seq -regexp {.*[^/]$} -col [list 0]]
        set files [paf_db_f seq -col 0]
    }
    return ${files}
}



