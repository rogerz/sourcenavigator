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
# history.tcl - Routines to manipulate the action history.
# Copyright (C) 1998 Cygnus Solutions.

proc sn_make_history_title {cmd scope string} {
    set string [string trim ${string}]
    switch ${scope} {
        "md" -
        "mi" -
        "iv" {
                set key [string trim [lindex ${string} 1]]
                set cls [string trim [lindex ${string} 0]]
                return "${cmd} ${key}(${scope}) ${cls}"
            }
        "f" {
                set dir [file dirname ${string}]
                if {${dir} == "."} {
                    set dir ""
                }
                set f [file tail ${string}]
                return "${cmd} [string trim "${f}(${scope}) ${dir}"]"
            }
        default {
                if {${scope} != ""} {
                    return "${cmd} [string trim ${string}(${scope})]"
                } else {
                    return "${cmd} [string trim ${string}]"
                }
            }
    }
}

proc hist_edit_object {scope name {off 0} {file ""} {lbl ""}} {
    global sn_sep
    if {[info commands paf_db_${scope}] == ""} {
        bell
        return
    }

    #make sure that we use the defined separator to look in the
    #database
    if {[llength ${name}] > 1} {
        set name "[lindex ${name} 0]${sn_sep}[lindex ${name} 1]"
    } else {
        set name [join [join ${name}]]
    }

    if {${file} != ""} {
        set lst [paf_db_${scope} seq -data -end ${file} ${name}]
    } else {
        set lst [paf_db_${scope} seq -data ${name}]
    }

    set lst [lindex ${lst} ${off}]
    set lineoff [expr [llength ${lst}] - 2]
    set edit_file [lindex ${lst} end]
    set pos [lindex ${lst} ${lineoff}]

    sn_edit_file "${off} ${name} ${scope}" ${edit_file} ${pos}

    return 1
}

proc hist_build_label {where data} {
    switch ${where} {
        "class" {
                set lbl "[join [lindex ${data} 1]]\(cl\)"
                set prefix class
            }
        "xref" {
                set lbl [join [lindex ${data} 0]]
                set prefix xref
            }
        "cov" -
        "iv" -
        "md" -
        "fr" -
        "mi" {
                set lbl [split [lindex ${data} 0]]
                set lbl "[lindex ${lbl} 1]\(${where}\) [lindex ${lbl} 0]"
                set prefix [get_indep String UtilEdit]
            }
        default {
                set lbl [join [lindex ${data} 0]]
                set lbl "${lbl}\(${where}\)"
                set prefix [get_indep String UtilEdit]
            }
    }
    return "${prefix} [string trim ${lbl}]"
}

proc hist_GetCommand {where} {
    switch ${where} {
        "f" {
                set command sn_edit_file
            }
        "xref" {
                set command sn_xref
            }
        "class" {
                set command sn_classbrowser
            }
        default {
                set command hist_edit_object
            }
    }

    return ${command}
}

proc hist_ReplaceHistory {where data lbl command} {
    global sn_history history_List

    set line [list ${where} ${data} ${lbl} ${command}]
    set clen [string first " " ${lbl}]

    if {![info exists history_List(${where})]} {
        set history_List(${where}) ""
    }

    #look, if the entry already availiable in the history
    #by grep history, look only for patterns
    if {${where} == "grep" || ${where} == "retr"} {
        set off -1
        set i 0
        foreach x $history_List(${where}) {
            if {[lindex ${x} 2] == ${lbl}} {
                set off ${i}
                break
            }
            incr i
        }
    } else {
        set off [lsearch -exact $history_List(${where}) ${line}]
    }
    if {${off} != -1} {
        #don't change the location of the history item
        return
    }
    set history_List(${where}) [linsert $history_List(${where}) 0 ${line}]
    #limit history size
    if {[llength $history_List(${where})] > $sn_history(size)} {
        set history_List(${where}) [lrange $history_List(${where}) 0\
          $sn_history(size)]
    }
}

#New history strategy
#split the history to three parts:
#1. command name, like sn_xref for cross reference
#2. label for the menu entry
#4. client data to use by calling the history command, this data
#   field is free by caller
proc sn_add_history {where data {lbl ""} {command ""}} {
    global history_List
    global sn_history

    if {! [info exists history_List(${where})]} {
        set history_List(${where}) ""
    }
    if {${data} == ""} {
        return
    }

    if {${lbl} == ""} {
        set lbl [hist_build_label ${where} ${data}]
    }

    set lbl [string trim ${lbl}]

    if {${command} == ""} {
        set command [hist_GetCommand ${where}]
    }

    switch ${where} {
        "f" {
                if {[lindex ${data} 0] != "f"} {
                    set data [linsert ${data} 0 "f"]
                }
            }
    }
    #something wrong, break
    if {${lbl} == "" || ${lbl} == ${where} || ${command} == ""} {
        return
    }

    #edit scope
    if {[lsearch -exact $sn_history(scopes) ${where}] == -1} {
        set where "edit"
    }

    hist_ReplaceHistory ${where} ${data} ${lbl} ${command}
}

#exec the command for history
proc hist_HistoryExec {line} {
    set where [lindex ${line} 0]
    set data [lindex ${line} 1]
    set lbl [lindex ${line} 2]
    set cmd [lindex ${line} 3]

    if {[catch {eval ${cmd} ${data}} err]} {
        sn_log "History call: ${err}"
    }
}

#Construct the history for all tools
proc sn_post_history_menu {m} {
    global history_List
    global sn_history

    if {![info exists history_List]} {
        return
    }
    ${m} delete 0 last

    set strings [list MultiEditor MultiClassHierarchy MultiClass MultiXRef\
      MultiInclude Retriever Grep]

    set i 0
    foreach cat $sn_history(scopes) {
        catch {destroy ${m}.${cat}}
        #no tearoff
        menu ${m}.${cat} -tearoff 0 -postcommand "post_category ${m}.${cat}\
          ${cat}"

        ${m} add cascade -label [get_indep String [lindex ${strings} ${i}]]\
          -menu ${m}.${cat} -underline [get_indep Pos [lindex ${strings} ${i}]]

        if {[info exists history_List(${cat})] && $history_List(${cat}) != ""} {
            set avail 1
        } else {
            set avail 0
        }
        #if there is no entries, disable the sub menu
        if {!${avail}} {
            ${m} entryconfig ${i} -state disabled
        }
        incr i
    }
}

proc post_category {m cat} {
    global history_List
    ${m} delete 0 last
    if {![info exists history_List(${cat})]} {
        return
    }
    foreach l $history_List(${cat}) {
        set hist [lindex ${l} 2]
        set clen [string first " " ${hist}]

        #we don't need to add the prefix to the history
        #entry because we have already sub menus for 
        #every categorie
        set hist [string range ${hist} [expr ${clen} + 1] end]

        ${m} add command -command "hist_HistoryExec \[list ${l}\]" -label ${hist}
    }
}

#####################################
#Multi types history manipulation
#####################################
proc bind_history {btn cat} {
    bind ${btn} <3> "post_history ${btn} ${cat}"
}

proc post_history {btn cat} {
    set m .history_pop_menu
    catch {destroy ${m}}

    #set (x,y) for option-menu
    set y [expr {[winfo rooty ${btn}] + [winfo height ${btn}]}]
    set x [winfo rootx ${btn}]

    menu ${m} -title "History" -tearoff 0 -postcommand "history_post_menu ${m} ${cat}"
    wm overrideredirect ${m} 1
    tk_popup ${m} ${x} ${y}
}
proc history_post_menu {m cat} {
    global history_List
    if {![info exists history_List(${cat})]} {
        return
    }
    foreach l $history_List(${cat}) {
        set hist [lindex ${l} 2]
        set clen [string first " " ${hist}]
        set hist [string range ${hist} [expr ${clen} + 1] end]
        ${m} add command -command "hist_HistoryExec \[list ${l}\]" -label ${hist}
    }
}

proc sn_add_to_histroy_stack {variable value} {
    global sn_options
    global sn_history
    upvar #0 ${variable} var

    if {![info exists var]} {
        set var ""
    }
    set i [lsearch -exact ${var} ${value}]
    if {${i} != -1} {
        set var [lreplace ${var} ${i} ${i}]
    }
    set var [linsert ${var} 0 ${value}]
    #limit history size
    if {[llength ${var}] > $sn_history(size)} {
        set var [lrange ${var} 0 $sn_history(size)]
    }
    return ${var}
}

