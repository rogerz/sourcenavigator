# Copyright (C) 2010 Enchanted Systems Limited
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
# optimizedb.tcl - Routines for optimizing project database.
#

#proc sn_update_optimize {} {
#    set lst [sn_create_optimize]
#
#    foreach w [lmatch [winfo children .] ".sn-optim-*"] {
#        set t ${w}.optimize.t
#
#        ${t} config -state normal
#        ${t} delete 0.0 end
#        ${t} insert end ${lst}
#        ${t} config -state disabled
#    }
#}

proc sn_create_optimize {{files ""} {win ""}} {
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

proc sn_optimize_destroy {w} {
    global sn_optimizedb_run

    itcl::delete object ${w}

    set cou [llength [lmatch [winfo children .] ".sn-optim-*"]]
    set sn_optimizedb_run ${cou}
}

proc sn_optimize_cancel {win} {
    global ${win}-cancel
    set ${win}-cancel yes
}

proc sn_optimize {} {
    global sn_options
    global tkeWinNumber sn_optimizedb_run

    set sn_optimizedb_run 1

    incr tkeWinNumber
    set s ".sn-optim-${tkeWinNumber}"

    upvar #0 ${s}-cancel cancel
    set cancel no
    sn_wait_dialog "" [get_indep String WaitForOptimizer] [get_indep String\
      Statistics] "sn_optimize_cancel ${s}"

    update idletasks

    set lst [sn_create_optimize ${files} ${s}]

    catch {destroy .wait_dlg}

    #return if user canceled the fetching
    if {${cancel}} {
        return
    }

    sourcenav::Window ${s}
    ${s} on_close "sn_optimize_destroy ${s}"
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
    ${s} configure -title [sn_title "[get_indep String Optimizer]${t}"]

    sn_motif_buttons ${s} bottom 0 [get_indep String Close]
    ${s}.button_0 config -command " sn_optimize_destroy ${s} "

    set sta ${s}.optimize
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

proc display_optim_window {op line} {
    global tcl_platform
    if {[winfo exists ${op}]} {
        global sn_progress_value
        ${op}.i.filename config -text ${line}
        incr sn_progress_value

        if {$tcl_platform(platform) == "windows"} {
            update idletasks
            update idletasks
        }
        update idletasks
    }\
    elseif {[sn_batch_mode]} {
        puts stdout [format [get_indep String ScanningFile] ${line}]
        flush stdout
    }
}

proc delete_optim_window {wdg} {
    global sn_optim_oldfocus
    if {[itcl::find object ${wdg}] == ${wdg}} {
        itcl::delete object ${wdg}
    }

    #restore old focus
    if {[info exists sn_optim_oldfocus] && [winfo exists\
      ${sn_optim_oldfocus}]} {
        take_focus [winfo toplevel ${sn_optim_oldfocus}] ${sn_optim_oldfocus}
    }
    
    # Update so that any windows the optim window was over
    # get to redraw before a possibly long wait.
    update
}

proc make_optim_window {filenum {enable_cancel 0}} {
    global sn_options
    global sn_progress_value
    global sn_optim_oldfocus

    if {[sn_batch_mode]} {
        return
    }

    set curs "watch"

    set sn_optim_oldfocus [focus]

    set t [sourcenav::Window [sourcenav::Window::next_window_name] -cursor ${curs}]
    set thull [$t component hull]
    ${t} withdraw
    ${t} on_close "${thull}.cancel invoke"
    ${t} configure -title [get_indep String Scanning]

    frame ${thull}.i
    label ${thull}.i.run -image company_image -bg black

    label ${thull}.i.filename -width 50 -anchor w

    #use a normal termometer (not bad unix scaler)
    set sn_progress_value 0
    ProgressBar ${thull}.scale -maxvalue ${filenum} -orientation horizontal\
      -showvalue 1 -variable sn_progress_value

    button ${thull}.cancel -text [get_indep String Cancel] -command "sn_processing_canceled 2"

    pack ${thull}.i.run -side left -padx 2 -pady 1 -anchor w
    pack ${thull}.i.filename -side left -fill x -anchor w -padx 10 -pady 20
    pack ${thull}.i -fill x -anchor w

    pack ${thull}.scale -fill x -padx 20 -pady 20
    if {${enable_cancel}} {
        pack ${thull}.cancel -side top -pady 2
    }

    catch {${thull} resizable yes no}

    ${thull} move_to_mouse

    update idletasks
    set geom [split [lindex [split [${t} geometry] "+"] 0] "x"]
    ${t} minsize [expr int([lindex ${geom} 0] / 3)] [lindex ${geom} 1]

    pack propagate ${thull}.i 0
    ${thull}.i.filename config -width 0

    catch {${thull} grab set}

    window_configure ${thull} deiconify ${thull}.scale

    # Ugh! Why does the window get mapped without
    # having the contents drawn under Windows?
    if {$::tcl_platform(platform) == "windows"} {
        sn_wait 300
    }

    return ${thull}
}

# This method is called when a fileevent on a subprocess pipe is ready. For
# instance the db compactor might have finished processing a file.

proc event_CompactPipeInput {eventfd sc} {
    global ProcessingCancelled
    global PafCompactPipeEnd
    global event_CompactPipeInput_last_accessed_file
    global sn_debug

    #the process has been canceled by the user
    if {${ProcessingCancelled} == 2} {
        sn_log "event_CompactPipeInput : processing was canceled by the user"
        fileevent ${eventfd} readable {}
        set PafCompactPipeEnd 1
	return
    }

    if {[catch {set eof [eof ${eventfd}]} err]} {
        set error 1
    } else {
        set error 0
    }

    #the parser crashed
    if {${error} || ${ProcessingCancelled} || ${eof}} {

# FIXME : Tcl fails to raise an error
# during a call to the close command
# when the program on the other end
# of a non-blocking pipe crashes.
# We work around this by putting the
# pipe back in blocking mode.
        fconfigure ${eventfd} -blocking 1
        set ret [catch {close ${eventfd}} err]
        if {${ret} && !${ProcessingCancelled}} {
            sn_log "event_CompactPipeInput close : error ${err}"

            # If the parser crashed, then we should show
            # an error message to the user instead of
            # just continuing.

            if {[string match "*child killed*" $err] ||
                    [string match "*abnormal program termination*" $err] ||
                    [string match "*child process exited abnormally*" $err]} {
                set crashed 1
            } else {
                set crashed 0
            }

            if {$sn_debug || $crashed} {
                sn_error_dialog ${err}
            }

            if {!$crashed} {
                # Setting ProcessingCancelled to 3 indicates that
                # the parsing process should continue. The next
                # block is skipped and the user is not asked if
                # they want to continue parsing.
                set ProcessingCancelled 3
            }
        }

        #ask the user to continue
        if {(${ret} || ${error}) && !${ProcessingCancelled}} {
            sn_log "event_CompactPipeInput : parser crashed or wrote \
                    to stderr, ProcessingCancelled is $ProcessingCancelled"
            set ProcessingCancelled 1
            sn_handle_parse_error
        }
        set PafCompactPipeEnd 1
        return
    }

    if {[catch {set line [gets ${eventfd}]}]} {
        return
    }
    
    if {[string equal $line ""]} {
        return
    }

    sn_log -l 2 "Info from pipe: ${line}"

    #status lines, ignore them
    set scanning "Status: Scanning: "
    set deleting "Status: Deleting: "
    if {[string first $deleting ${line}] == 0 ||
            [string first $scanning ${line}] == 0} {
        return
    }

    if {[string first "Error:" ${line}] != -1} {
        sn_log "Error from pipe: ${line}"

        set ev_fnc [fileevent ${eventfd} readable]
        fileevent ${eventfd} readable {}

        sn_error_dialog ${line}

        catch {fileevent ${eventfd} readable ${ev_fnc}}
    } else {
        # Check for parse status message from pipe,
	# format is "Status: Parsing: filename"
	set header "Status: Parsing: "
	if {[string first $header $line] == 0} {
            set fname [string range $line [string length $header] end]
            set event_CompactPipeInput_last_accessed_file ${fname}
            display_scale_window ${sc} ${fname}
	    update idletasks
	} else {
	    set ev_fnc [fileevent ${eventfd} readable]
	    fileevent ${eventfd} readable {}
	    sn_error_dialog "unrecognized pipe input \"${line}\""
	    catch {fileevent ${eventfd} readable ${ev_fnc}}
	}
    }
}

