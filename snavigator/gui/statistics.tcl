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
# statistics.tcl - Routines for generating project statistics.
# Copyright (C) 1998 Cygnus Solutions.

proc sn_update_statistic {} {
    set lst [sn_create_statistic]

    foreach w [lmatch [winfo children .] ".sn-statis-*"] {
        set t ${w}.statistic.t

        ${t} config -state normal
        ${t} delete 0.0 end
        ${t} insert end ${lst}
        ${t} config -state disabled
    }
}

proc sn_create_statistic {{files ""} {win ""}} {
    global prj_lines_num

    set lst "\n"

    if {${files} != ""} {
        set count [llength ${files}]
    } else {
        if {[catch {set count [llength [sn_project_file_list]]}]} {
            set count 0
        }
    }
    if {${count} == 1 && ${files} != ""} {
        append lst "[get_indep String File]: ${files}\n"
    } else {
        append lst "[get_indep String Files]:\t[format "%6d" ${count}]\n"
    }
    #display number of views
    if {${files} == ""} {
        if {[catch {set count [llength [paf_db_proj get -key views]]}]} {
            set count 0
        }
        append lst "[get_indep String View]:\t[format "%6d" ${count}]\n"
    }
    append lst "\n"
    foreach sc [list cl mi md fu fd t e ec gv iv ma su com con cov un] {
        lappend str [convert_scope_to_str ${sc}]
    }

    upvar #0 ${win}-cancel cancel
    foreach s [lsort ${str}] {
        set sc [convert_scope_to_sym ${s}]
        if {${files} != ""} {
            set count 0
            foreach file ${files} {

                #see if user canceld the operation
                update idletasks
                update
                if {[info exists cancel] && ${cancel}} {
                    return ""
                }

                if {[catch {set c [llength [paf_db_${sc} seq -end ${file}]]}]} {
                    break
                }
                incr count ${c}
            }
        } else {
            if {[catch {set count [llength [paf_db_${sc} seq -key]]}]} {
                set count 0
            }
        }
        append lst "${s}(${sc}):\t[format "%6d" ${count}]\n"
    }

    return ${lst}
}

proc sn_statistic_destroy {w} {
    global sn_statistic_run

    itcl::delete object ${w}

    set cou [llength [lmatch [winfo children .] ".sn-statis-*"]]
    set sn_statistic_run ${cou}
}

proc sn_statistic_cancel {win} {
    global ${win}-cancel
    set ${win}-cancel yes
}
proc sn_statistic {{files ""}} {
    global sn_options
    global tkeWinNumber sn_statistic_run

    set sn_statistic_run 1

    incr tkeWinNumber
    set s ".sn-statis-${tkeWinNumber}"

    upvar #0 ${s}-cancel cancel
    set cancel no
    sn_wait_dialog "" [get_indep String WaitForStatistics] [get_indep String\
      Statistics] "sn_statistic_cancel ${s}"

    update idletasks

    set lst [sn_create_statistic ${files} ${s}]

    catch {destroy .wait_dlg}

    #return if user canceled the fetching
    if {${cancel}} {
        return
    }

    sourcenav::Window ${s}
    ${s} on_close "sn_statistic_destroy ${s}"
    ${s} withdraw
    ${s} configure -geometry 550x450

    if {${files} != ""} {
        set len [llength ${files}]
        if {${len} > 1} {
            set t " of ${len} files"
        } else {
            set t " of ${files}"
        }
    } else {
        set t ""
    }
    ${s} configure -title [sn_title "[get_indep String Statistics]${t}"]

    sn_motif_buttons ${s} bottom 0 [get_indep String Close]
    ${s}.button_0 config -command " sn_statistic_destroy ${s} "

    set sta ${s}.statistic
    set t ${sta}.t
    frame ${sta}

    scrollbar ${sta}.x -command " ${t} xview " -orient horiz
    scrollbar ${sta}.y -command " ${t} yview "
    text ${t} -wrap none -width 35 -xscrollcommand "${sta}.x set"\
      -yscrollcommand "${sta}.y set"

    set font [${t} cget -font]
    set text_avg_width [font measure ${font} "M"]
    ${t} config -tabs [expr 25 * ${text_avg_width}] -height 22

    pack ${sta}.x -side bottom -fill x
    pack ${sta}.y -side right -fill y
    pack ${t} -fill both -expand y

    pack ${sta} -fill both -expand y -padx 5 -pady 5

    ${t} config -state normal

    ${t} delete 0.0 end
    ${t} insert end ${lst}

    ${t} config -state disabled

    ${s} move_to_mouse

}

