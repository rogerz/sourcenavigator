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
# version.tcl - Procedures for version control system integration.
# Copyright (C) 1998 Cygnus Solutions.

proc sn_add_version_control_system {ident args} {
    global sn_options
    global sn_verctl_options

    foreach variable {checkout-with-lock use-relative-path}  {
        set sn_verctl_options(${ident},${variable}) 0
    }

    # some defaults
    set sn_verctl_options(${ident},diff-command) "diff"
    set sn_verctl_options(${ident},diff-ignore-whitespace) "-w"
    set sn_verctl_options(${ident},diff-ignore-case) "-i"

    foreach variable {history-replacements symbolic-tag-replacements} {
        set sn_verctl_options(${ident},${variable}) ""
    }

    set title [string toupper ${ident}]

    for {set i 0} {${i} < [llength ${args}]} {incr i} {
        set arg [lindex ${args} ${i}]
        incr i
        set val [lindex ${args} ${i}]

        switch -- ${arg} {
            "-checkin" {
                    set sn_verctl_options(${ident},checkin) ${val}
                }
            "-checkin-comment-via" {
                    set sn_verctl_options(${ident},checkin-comment-via) ${val}
                }
            "-checkin-exclusive" {
                    set sn_verctl_options(${ident},checkin-exclusive) ${val}
                }
            "-checkout" {
                    set sn_verctl_options(${ident},checkout) ${val}
                }
            "-checkout-exclusive" {
                    set sn_verctl_options(${ident},checkout-exclusive) ${val}
                }
            "-checkout-individual" {
                    set sn_verctl_options(${ident},checkout-individual) ${val}
                }
            "-checkout-individual-to-stdout" {
                    set\
                      sn_verctl_options(${ident},checkout-individual-to-stdout) ${val}
                }
            "-checkout-with-lock" {
                    if {${val} == "yes"} {
                        set sn_verctl_options(${ident},checkout-with-lock) 1
                    }
                }
            "-default" {
                    if {${val} == "yes"} {
                        set sn_options(both,rcs-type) ${ident}
                    }
                }
            "-delete-revision" {
                    set sn_verctl_options(${ident},delete-revision) ${val}
                }
            "-diff-command" {
                    set sn_verctl_options(${ident},diff-command) ${val}
                }
            "-diff-ignore-case" {
                    set sn_verctl_options(${ident},diff-ignore-case) ${val}
                }
            "-diff-ignore-whitespace" {
                    set sn_verctl_options(${ident},diff-ignore-whitespace)\
                      ${val}
                }
            "-discard" {
                    set sn_verctl_options(${ident},discard) ${val}
                }
            "-history" {
                    set sn_verctl_options(${ident},history) ${val}
                }
            "-history-pattern" {
                    set sn_verctl_options(${ident},history-pattern) ${val}
                }
            "-history-individual" {
                    set sn_verctl_options(${ident},history-individual) ${val}
                }
            "-history-individual-pattern" {
                    set sn_verctl_options(${ident},history-individual-pattern)\
                      ${val}
                }
            "-history-replacements" {
                    set sn_verctl_options(${ident},history-replacements) ${val}
                }
            "-ignore-dirs" {
                    foreach directory ${val} {
                        lappend sn_options(def,ignored-directories) ${directory}
                    }
                }
            "-lock" {
                    set sn_verctl_options(${ident},lock) ${val}
                }
            "-lock-individual" {
                    set sn_verctl_options(${ident},lock-individual) ${val}
                }
            "-revision-number-pattern" {
                    set sn_verctl_options(${ident},revision-number-pattern)\
                      ${val}
                }
            "-symbolic-tags-pattern" {
                    set sn_verctl_options(${ident},symbolic-tags-pattern) ${val}
                }
            "-symbolic-tags-replacements" {
                    set sn_verctl_options(${ident},symbolic-tags-replacements)\
                      ${val}
                }
            "-title" {
                    set title ${val}
                }
            "-unlock" {
                    set sn_verctl_options(${ident},unlock) ${val}
                }
            "-unlock-individual" {
                    set sn_verctl_options(${ident},unlock-individual) ${val}
                }
	    "-use-relative-path" {
             	    set sn_verctl_options(${ident},use-relative-path) $val
	        }
            default {
                    puts stderr "sn_add_version_control_system: unknown\
                      argument <${arg}>"
                }
        }
    }

    lappend sn_options(sys,supported-vcsystems) [list ${title} ${ident}]
}

proc sn_rcs_extract_text {patterns text} {
    set result ""
    foreach pair ${patterns} {
        if {[llength ${pair}] > 1} {
            set start 0
            if {[lindex ${pair} 0] != "start"} {
                foreach line ${text} {
                    if {[regexp -- [lindex ${pair} 0] ${line} ignore] > 0} {
                        break
                    }
                    incr start
                }
            }

            if {[lindex ${pair} 1] == "end"} {
                set end [expr [llength ${text}]]
            } else {
                set end [expr ${start}]
                foreach line [lrange ${text} ${start} end] {
                    if {[regexp -- [lindex ${pair} 1] ${line} ignore] > 0} {
                        break
                    }
                    incr end
                }
            }
            foreach line [lrange ${text} [expr ${start} + 1] [expr ${end}\
              - 1]] {
                lappend result ${line}
            }
        } else {
            set pattern ${pair}
            foreach line ${text} {
                if {[regexp -- ${pattern} ${line} ignore match] > 0} {
                    lappend result ${match}
                    break
                }
            }
        }
    }
    return ${result}
}

proc sn_revision_ctrl {} {
    global tkeWinNumber
    set new 1

    #only one project window can be opened
    set obj ".sn-rcs"
    if {[winfo exists ${obj}]} {
        ${obj} raise
    } else {
        RevisionCtrl& ${obj}
    }
}


proc sn_rcs_history {file revision syms history} {
    global sn_options
    global sn_verctl_options


    upvar ${syms} symbols
    upvar ${history} output

    set rcstype $sn_options(both,rcs-type)
    set file [RevisionCtrl&::source_file_names ${file}]


    if {![info exists sn_verctl_options(${rcstype},history-individual)] ||\
      ![info exists sn_verctl_options(${rcstype},history)]} {
        set output ""
        set symbols ""
        return {}
    }

    set relative_file [sn_rcs_process_filenames $sn_verctl_options($rcstype,use-relative-path) $file]

    if {${revision} != ""} {
        # seek history for a particular revision
        set cmd\
          "$sn_verctl_options(${rcstype},history-individual)${revision} ${relative_file}"
    } else {
        # seek history for all revisions
        set cmd "$sn_verctl_options(${rcstype},history) $relative_file"
    }

    # Change directory to file's directory. 
    if {$sn_verctl_options($rcstype,use-relative-path)} {
	set currentworkdir [pwd]
	cd [sn_rcs_get_common_path $file]
    }

    set output [sn_rcs_exec ${cmd} 0]

    # Move back to working directory.
    if {$sn_verctl_options($rcstype,use-relative-path)} {
	cd $currentworkdir
    }

    if {[info exists sn_verctl_options(${rcstype},symbolic-tags-pattern)]} {
        set patterns $sn_verctl_options(${rcstype},symbolic-tags-pattern)
        set symbols [sn_rcs_extract_text ${patterns} ${output}]
    } else {
        set symbols ""
    }

    if {[info exists\
      sn_verctl_options(${rcstype},symbolic-tags-replacements)]} {
        set replacements\
          $sn_verctl_options(${rcstype},symbolic-tags-replacements)
        sn_search_replace ${replacements} ${symbols} symbols
    }

    catch {unset patterns}

    if {${revision} != ""} {
        if {[info exists\
          sn_verctl_options(${rcstype},history-individual-pattern)]} {
            set patterns\
              $sn_verctl_options(${rcstype},history-individual-pattern)
        }
    } else {
        if {[info exists sn_verctl_options(${rcstype},history-pattern)]} {
            set patterns $sn_verctl_options(${rcstype},history-pattern)
        }
    }

    if {[info exists patterns]} {
        set output [sn_rcs_extract_text ${patterns} ${output}]
    }
    # else no modification to output.

    if {[info exists sn_verctl_options(${rcstype},history-replacements)]} {
        set replacements $sn_verctl_options(${rcstype},history-replacements)
        sn_search_replace ${replacements} ${output} output
    }

    # retrieve the list of available revision numbers

    if {[info exists sn_verctl_options(${rcstype},revision-number-pattern)]} {
        set pattern $sn_verctl_options(${rcstype},revision-number-pattern)
        return [sn_lgrep ${pattern} ${output}]
    } else {
        return {}
    }
}

# Obtains revision information about $file.

proc sn_rcs_get_revisions {file log_rev symbols vers {log ""} {bsy 1}} {
    global sn_options
    upvar ${symbols} sy
    upvar ${vers} versions


    if {${log} != ""} {
        upvar ${log} lg
    }

    # get log information

    set versions [sn_rcs_history ${file} ${log_rev} sy lg]

    set versions [string trim ${versions}]

    if {[string compare ${versions} ""] == 0} {
        return 0
    }
    return 1
}

# returns revisions of $file 
proc sn_rcs_get_version_nums {file revisions} {
    global sn_options
    global sn_verctl_options
    upvar ${revisions} revs

    set file [lindex ${file} 0]
    set rcstype $sn_options(both,rcs-type)
    set relative_file [sn_rcs_process_filenames $sn_verctl_options($rcstype,use-relative-path) $file]

    if {![info exists sn_verctl_options(${rcstype},history)]} {
        sn_log "Can't get version numbers, no $rcstype history command."
        return {}
    }

    # Change directory to file's directory.
    if {$sn_verctl_options($rcstype,use-relative-path)} {
	set currentworkdir [pwd]
	cd [sn_rcs_get_common_path $file]
    }

    if {[catch {sn_rcs_exec "$sn_verctl_options(${rcstype},history) ${relative_file}"\
      0} output]} {
        return 0
    }

    # Move back to working directory.
    if {$sn_verctl_options($rcstype,use-relative-path)} {
	cd $currentworkdir
    }

    # retrieve the list of available revision numbers
    if {[info exists sn_verctl_options(${rcstype},revision-number-pattern)]} {
        set pattern $sn_verctl_options(${rcstype},revision-number-pattern)
        set revs [sn_lgrep ${pattern} ${output}]
    } else {
        set revs {}
    }

    if {${revs} == ""} {
        return 0
    }

    return 1
}

proc sn_rcs_exec {cmd {bsy 0}} {
    global errorCode

    set retval ""
    set cmd [join ${cmd}]

    sn_log "sn_rcs_exec: About to exec $cmd."
    if {[catch {sn_exec_x_events ${cmd} ${bsy}} retval]} {
        sn_error_dialog ${retval}
        error ${retval}
    }

    return ${retval}
}

# Check in a list of files.

proc sn_rcs_checkin {marked_files} {
    global sn_options
    global sn_verctl_options

    set files [RevisionCtrl&::source_file_names ${marked_files}]
    set rcstype $sn_options(both,rcs-type)

    # create toplevel, but don't display it yet
    set w [sourcenav::Window::next_window_name]
    sourcenav::Window ${w}
    ${w} withdraw

    # remove the dots from the 'check in' string and set the title with it
    ${w} title "[string trimright [get_indep String ChooseCheckIn]\
      "."] ${files}"

    global ${w}.withlock
    if {![info exists ${w}.withlock]} {
        set ${w}.withlock $sn_verctl_options(${rcstype},checkout-with-lock)
    }

    set checkin ${w}.ctrl.button_0
    set cancel ${w}.ctrl.button_1

    frame ${w}.ctrl
    frame ${w}.ctrl.lock

    checkbutton ${w}.ctrl.lock.lock -text [get_indep String ChooseWithLock]\
      -variable ${w}.withlock

    pack ${w}.ctrl.lock.lock -pady 10 -fill x

    sn_motif_buttons ${w}.ctrl bottom -1 [get_indep String ok]\
      [get_indep String cancel]

    ${checkin} config -command " set ${w}-select_status \[set ${w}.withlock\] "
    ${cancel} config -command " set ${w}-select_status cancel "

    frame ${w}.ctrl.heading
    label ${w}.ctrl.heading.lb -text [get_indep String Comment]
    pack ${w}.ctrl.heading.lb -side left -padx 5

    frame ${w}.ctrl.rem
    set t ${w}.ctrl.rem.t
    text ${t} -height 4 -width 65 -wrap none

    pack ${t} -padx 5 -fill both -expand y
    pack ${w}.ctrl.heading -fill both -expand y
    pack ${w}.ctrl.rem -fill both -expand y
    pack ${w}.ctrl.lock -fill x -expand y
    pack ${w}.ctrl -fill both -expand y

    ${w} move_to_mouse

    ${w} on_close "${cancel} invoke"

    upvar #0 ${w}-select_status status
    set status ""

    tkwait variable ${w}-select_status

    set txt [string trim [${t} get 0.0 "end -1c"]]

    itcl::delete object ${w}

    if {${status} != "cancel"} {
        if {${status}} {
            if {[info exists sn_verctl_options(${rcstype},checkin-exclusive)]} {
                set cmd $sn_verctl_options(${rcstype},checkin-exclusive)
            } else {
                set cmd $sn_verctl_options(${rcstype},checkin)
            }
        } else {
            set cmd $sn_verctl_options(${rcstype},checkin)
        }

        # Separate out the filename arguments.
        append cmd " "

	set relative_files [sn_rcs_process_filenames $sn_verctl_options($rcstype,use-relative-path) $files]

        set via $sn_verctl_options(${rcstype},checkin-comment-via)
        switch ${via} {
            "cmdline" {
                    # Include quotes to keep the text as a single argument.
                    append cmd "\"${txt}\" ${relative_files}"
                }
            "file" {
                    set commentf [sn_tmpFileName]
                    set fd [open ${commentf} "w+"]
                    fconfigure ${fd} -encoding $sn_options(def,system-encoding) -blocking 0
                    puts ${fd} ${txt}
                    close ${fd}
                    append cmd "${commentf} ${relative_files}"
                }
            "stdin" {
                    set commentf [sn_tmpFileName]
                    set fd [open ${commentf} "w+"]
                    fconfigure ${fd} -encoding $sn_options(def,system-encoding) -blocking 0
                    puts ${fd} ${txt}
                    close ${fd}
                    append cmd "${relative_files} < ${commentf}"
                }
        }
	# Change directory to file's directory. 
	if {$sn_verctl_options($rcstype,use-relative-path)} {
	    set currentworkdir [pwd]
	    cd [sn_rcs_get_common_path $files]
	}

        set status [expr ![catch {sn_rcs_exec ${cmd}}]]

	# Move back to working directory.
	if {$sn_verctl_options($rcstype,use-relative-path)} {
	    cd $currentworkdir
	}

        if {${via} == "stdin"} {
            file delete -- ${commentf}
        }

        if {!${status}} {
            RevisionCtrl&::refresh ${marked_files}
        }
    } else {
        set status 0
    }
    set st ${status}
    unset status

    return ${st}
}

proc sn_rcs_checkout {marked_files {bsy 1}} {
    global sn_options
    global sn_verctl_options

    set rcstype $sn_options(both,rcs-type)

    set files [RevisionCtrl&::source_file_names ${marked_files}]
    set relative_files [sn_rcs_process_filenames $sn_verctl_options($rcstype,use-relative-path) $files]
    set versions ""
    if {[catch {sn_rcs_get_version_nums ${files} versions} msg]} {
        return
    }

    set w [sourcenav::Window::next_window_name]
    sourcenav::Window ${w}
    ${w} withdraw

    set t [string trimright [get_indep String ChooseCheckOut] "."]
    set tf [lindex [lindex ${marked_files} 0] 0]
    ${w} title "${t} ${tf}"

    global ${w}.withlock
    if {![info exists ${w}.withlock]} {
        set ${w}.withlock $sn_verctl_options(${rcstype},checkout-with-lock)
    }

    set checkout ${w}.ctrl.button_0
    set cancel ${w}.ctrl.button_1

    frame ${w}.ctrl
    frame ${w}.ctrl.lock

    checkbutton ${w}.ctrl.lock.lock -text [get_indep String ChooseWithLock]\
      -variable ${w}.withlock

    pack ${w}.ctrl.lock.lock -side left -padx 10 -fill x

    sn_motif_buttons ${w}.ctrl bottom -1 [get_indep String ok]\
      [get_indep String cancel]

    ${checkout} config -command " set ${w}-select_status \[set ${w}.withlock\] "
    ${cancel} config -command " set ${w}-select_status cancel "

    frame ${w}.ctrl.vers
    global ${w}.ctrl.vers
    entry ${w}.ctrl.vers.e -textvariable ${w}.ctrl.vers
    label ${w}.ctrl.vers.l -text [get_indep String ChooseRevision]
    pack ${w}.ctrl.vers.l -side left
    pack ${w}.ctrl.vers.e -side left -fill x -expand y -padx 5
    pack ${w}.ctrl.vers -pady 2

    pack ${w}.ctrl.lock -fill x -expand y
    pack ${w}.ctrl -side left -fill both

    set ${w}.ctrl.vers [lindex ${versions} 0]

    Selector& ${w}.syms -contents ${versions} -height 5 -width 10 -sort ""

    ${w}.syms treebind <ButtonRelease-1> "set ${w}.ctrl.vers \[${w}.syms marked\]"
    ${w}.syms treebind <Double-1> "${checkout} invoke"

    pack ${w}.syms -side left -fill both -expand y

    ${w} move_to_mouse

    ${w} on_close "${cancel} invoke"

    upvar #0 ${w}-select_status status
    set status ""

    vwait ${w}-select_status
    itcl::delete object ${w}

    if {${status} != "cancel"} {
        upvar #0 ${w}.ctrl.vers rev

        if {${status}} {
            set cmd $sn_verctl_options(${rcstype},checkout-exclusive)${rev}
        } else {
            set cmd $sn_verctl_options(${rcstype},checkout)${rev}
        }

	# Change directory to file's directory. 
	if {$sn_verctl_options($rcstype,use-relative-path)} {
	    set currentworkdir [pwd]
	    cd [sn_rcs_get_common_path $files]
	}

        set status [expr ![catch {sn_rcs_exec "${cmd} ${relative_files}"}]]

	# Move back to working directory.
	if {$sn_verctl_options($rcstype,use-relative-path)} {
	    cd $currentworkdir
	}

        if {${status}} {
            RevisionCtrl&::refresh ${marked_files}
        }

    } else {
        set status 0
    }
    
    set sta ${status}
    unset status

    return ${sta}
}

proc sn_rcs_discard {marked_files {bsy 1}} {
    global sn_options
    global sn_verctl_options

    set files [RevisionCtrl&::source_file_names ${marked_files}]
    set rcstype $sn_options(both,rcs-type)
    set relative_files [sn_rcs_process_filenames $sn_verctl_options($rcstype,use-relative-path) $files]
    set cmd $sn_verctl_options(${rcstype},discard)
    if {${cmd} == {}} {
        sn_error_dialog [get_indep String DiscardNotSupported]
        return 0
    }

    # Change directory to file's directory. 
    if {$sn_verctl_options($rcstype,use-relative-path)} {
	set currentworkdir [pwd]
	cd [sn_rcs_get_common_path $files]
    }

    set result [sn_rcs_exec "${cmd} ${relative_files}"]

    # Move back to working directory.
    if {$sn_verctl_options($rcstype,use-relative-path)} {
	cd $currentworkdir
    }

    RevisionCtrl&::refresh ${marked_files}

    if {${result} == ""} {
        return 0
    }
    return 1
}

proc sn_rcs_lockunlockdel {cmd marked_files {bsy 1}} {
    global sn_options
    global sn_verctl_options

    set rcstype $sn_options(both,rcs-type)
    set files [RevisionCtrl&::source_file_names ${marked_files}]
    set relative_files [sn_rcs_process_filenames $sn_verctl_options($rcstype,use-relative-path) $files]

    switch ${cmd} {
        "lock" {
                set ti [string trimright [get_indep String ChooseLock] "."]
            }
        "unlock" {
                set ti [string trimright [get_indep String ChooseUnlock] "."]
            }
        "del" {
                set ti [string trimright [get_indep String ChooseDel] "."]
            }
    }

    set w [sourcenav::Window::next_window_name]
    sourcenav::Window ${w}
    ${w} withdraw

    set tf [lindex [lindex ${marked_files} 0] 0]
    ${w} title "${ti} ${tf}"

    set lock ${w}.ctrl.button_0
    set cancel ${w}.ctrl.button_1

    frame ${w}.ctrl
    sn_motif_buttons ${w}.ctrl bottom 0 [get_indep String ok]\
      [get_indep String cancel]

    ${lock} config -command " set ${w}-select_status [list ${cmd}] "
    ${cancel} config -command " set ${w}-select_status cancel "

    frame ${w}.ctrl.vers
    global ${w}.ctrl.vers
    entry ${w}.ctrl.vers.e -textvariable ${w}.ctrl.vers
    label ${w}.ctrl.vers.l -text [get_indep String ChooseRevision]

    pack ${w}.ctrl.vers.l -side left
    pack ${w}.ctrl.vers.e -side left -fill x -expand y -padx 5
    pack ${w}.ctrl.vers -pady 2
    pack ${w}.ctrl -side left -fill both

    set revisions ""
    catch {sn_rcs_get_version_nums ${files} revisions}
    set ${w}.ctrl.vers [lindex ${revisions} 0]

    Selector& ${w}.syms -contents ${revisions} -height 5 -width 10 -sort ""

    ${w}.syms treebind <ButtonRelease-1> "set ${w}.ctrl.vers \[${w}.syms marked\]"
    ${w}.syms treebind <Double-1> "${lock} invoke"
    pack ${w}.syms -side left -fill both -expand y

    ${w} move_to_mouse

    ${w} on_close "${cancel} invoke"

    upvar #0 ${w}-select_status status
    set status ""

    tkwait variable ${w}-select_status
    itcl::delete object ${w}

    if {${status} != "cancel"} {
        upvar #0 ${w}.ctrl.vers rev

        switch ${cmd} {
            "del" {
                    set cmdline $sn_verctl_options(${rcstype},delete-revision)

		    # Change directory to file's directory. 
	            if {$sn_verctl_options($rcstype,use-relative-path)} {
			set currentworkdir [pwd]
			cd [sn_rcs_get_common_path $files]
		    }

                    set result [expr ![catch {sn_rcs_exec\
                      "${cmdline}${rev} ${relative_files}"}]]

		    # Move back to working directory.
		    if {$sn_verctl_options($rcstype,use-relative-path)} {
		        cd $currentworkdir
		    }
                }
            "lock" {
                    if {[info exists\
                      sn_verctl_options(${rcstype},lock-individual)]} {
                        set cmdline\
                          $sn_verctl_options(${rcstype},lock-individual)

			# Change directory to file's directory. 
			if {$sn_verctl_options($rcstype,use-relative-path)} {
			    set currentworkdir [pwd]
			    cd [sn_rcs_get_common_path $files]
			}

                        set result [expr ![catch {sn_rcs_exec\
                          "${cmdline}${rev} ${relative_files}"}]]

			# Move back to working directory.
			if {$sn_verctl_options($rcstype,use-relative-path)} {
			    cd $currentworkdir
			}
                    } else {
                        set cmdline $sn_verctl_options(${rcstype},lock)

			# Change directory to file's directory. 
			if {$sn_verctl_options($rcstype,use-relative-path)} {
			    set currentworkdir [pwd]
			    cd [sn_rcs_get_common_path $files]
			}

                        set result [expr ![catch {sn_rcs_exec\
                          "${cmdline} ${relative_files}"}]]

			# Move back to working directory.
			if {$sn_verctl_options($rcstype,use-relative-path)} {
			    cd $currentworkdir
			}
                    }
                }
            "unlock" {
                    if {[info exists\
                      sn_verctl_options(${rcstype},unlock-individual)]} {
                        set cmdline\
                          $sn_verctl_options(${rcstype},unlock-individual)

			# Change directory to file's directory. 
			if {$sn_verctl_options($rcstype,use-relative-path)} {
			    set currentworkdir [pwd]
			    cd [sn_rcs_get_common_path $files]
			}

                        set result [expr ![catch {sn_rcs_exec\
                          "${cmdline}${rev} ${relative_files}"}]]

			# Move back to working directory.
			if {$sn_verctl_options($rcstype,use-relative-path)} {
			    cd $currentworkdir
			}

                    } else {
                        set cmdline $sn_verctl_options(${rcstype},unlock)

			# Change directory to file's directory. 
			if {$sn_verctl_options($rcstype,use-relative-path)} {
			    set currentworkdir [pwd]
			    cd [sn_rcs_get_common_path $relative_files]
			}

                        set result [expr ![catch {sn_rcs_exec\
                          "${cmdline} ${files}"}]]

			# Move back to working directory.
			if {$sn_verctl_options($rcstype,use-relative-path)} {
			    cd $currentworkdir
			}
                    }
                }
        }

        RevisionCtrl&::refresh ${marked_files}
    } else {
        set status 0
    }

    set sta ${status}
    unset status

    return ${sta}
}

proc sn_rcs_diff {basewindow files} {
    global sn_options
    global sn_verctl_options
    global env

    set rcstype $sn_options(both,rcs-type)

    # Clean any namespace off of basewindow.
    set basewindow [namespace tail $basewindow]

    # Block UI while we get the diff info.
    tixBusy $basewindow on

    # Create a window, dependend on the main window.
    set w ${basewindow}-rcs_diff_dialog
    if {[winfo exists ${w}]} {
        raise ${w}
        return
    }

# FIXME: This needs to be refactored into a widget
# that extends the Dialog class!
    sourcenav::Window ${w}
    ${w} withdraw
    ${w} transient ${basewindow}

    set ti [string trimright [get_indep String ChooseDiff] "."]
    ${w} title [list ${ti} ${files}]

    set diff ${w}.ctrl.button_0
    set cancel ${w}.ctrl.button_1

    frame ${w}.ctrl
    sn_motif_buttons ${w}.ctrl bottom 0 [get_indep String ok]\
      [get_indep String cancel]

    set cmd $sn_verctl_options(${rcstype},checkout-individual-to-stdout)

    ${diff} config -command " set ${w}-select_status [list ${cmd}] "
    ${cancel} config -command " set ${w}-select_status cancel "

    frame ${w}.ctrl.vers
    global ${w}.ignoreWhitespace ${w}.ignoreCase
    # ignore whitespace by default
    set ${w}.ignoreWhitespace 1
    set ${w}.ignoreCase 0

    global ${w}.ctrl.vers
    entry ${w}.ctrl.vers.e -textvariable ${w}.ctrl.vers
    label ${w}.ctrl.vers.l -text [get_indep String ChooseRevision]

    pack ${w}.ctrl.vers.l -side left
    pack ${w}.ctrl.vers.e -side left -fill x -expand y -padx 5
    pack ${w}.ctrl.vers -pady 2
    frame ${w}.ctrl.opt
    checkbutton ${w}.ctrl.opt.ws -text [get_indep String IgnoreWS]\
      -variable ${w}.ignoreWhitespace -anchor w
    pack ${w}.ctrl.opt.ws -fill x -expand y
    checkbutton ${w}.ctrl.opt.ca -text [get_indep String IgnoreCase]\
      -variable ${w}.ignoreCase -anchor w
    pack ${w}.ctrl.opt.ca -fill x -expand y
    pack ${w}.ctrl.opt -fill x -expand y
    pack ${w}.ctrl -side left -fill both

    set versions ""

    sn_rcs_get_version_nums ${files} versions

    set ${w}.ctrl.vers [lindex ${versions} 0]
    Selector& ${w}.syms -contents ${versions} -height 5 -width 10 -sort ""

    ${w}.syms treebind <ButtonRelease-1> "set ${w}.ctrl.vers \[${w}.syms marked\]"
    ${w}.syms treebind <Double-1> "${diff} invoke"
    pack ${w}.syms -side left -fill both -expand y

    ${w} on_close "${cancel} invoke"

    # Release the UI block
    tixBusy $basewindow off

    ${w} centerOnScreen
    ${w} deiconify
    # Do we need tk do a ${w} take_focus ??
    ${w} grab set

    upvar #0 ${w}-select_status status
    set status ""

    vwait ${w}-select_status

    itcl::delete object ${w}

    if {${status} == "cancel"} {
        return
    }

    set home $env(HOME)
    if {[string compare ${home} "/"] != 0} {
        append home "/"
    }

    foreach f ${files} {
        set t_m_p [sn_tmpFileName]
        set err [sn_tmpFileName]
	set relative_f [sn_rcs_process_filenames $sn_verctl_options($rcstype,use-relative-path) $f]

        upvar #0 ${w}.ctrl.vers rev

	# Change directory to file's directory. 
	if {$sn_verctl_options($rcstype,use-relative-path)} {
	    set currentworkdir [pwd]
	    cd [sn_rcs_get_common_path $files]
	}

        set cmd "${status}${rev} ${relative_f} > ${t_m_p} 2> ${err}"

        if {[catch [eval exec -- ${cmd}] msg]} {
	    # Move back to working directory.
	    if {$sn_verctl_options($rcstype,use-relative-path)} {
		cd $currentworkdir
	    }
	    if {![catch {set fd [open ${err}]}]} {
                fconfigure ${fd} -encoding $sn_options(def,system-encoding) -blocking 0
                set ermsg [read -nonewline ${fd}]
                close ${fd}
                append msg "\n" ${ermsg}
            }
            sn_error_dialog ${msg}

            file delete -- ${t_m_p} ${err}
            continue
        }

	# Move back to working directory.
	if {$sn_verctl_options($rcstype,use-relative-path)} {
	    cd $currentworkdir
	}

        set fd [open ${err} "r"]
        fconfigure ${fd} -encoding $sn_options(def,system-encoding) -blocking 0
        set stderr_msg [split [read -nonewline ${fd}] "\n"]
        close ${fd}

        set stderr_msg [lindex ${stderr_msg} 0]

        if {![file exists ${t_m_p}] || [file size ${t_m_p}] == 0} {
            file delete -- ${t_m_p}
            sn_error_dialog [get_indep String ChooseNothingCheckedOut]
            continue
        }

        set diffopt ""
        if [set ${w}.ignoreWhitespace] {
            set diffopt $sn_verctl_options(${rcstype},diff-ignore-whitespace)
        }
        if [set ${w}.ignoreCase] {
            set diffopt\
              "${diffopt} $sn_verctl_options(${rcstype},diff-ignore-case)"
        }

        #make sure we stay in project directory (different interpeters).
        catch {cd $sn_options(sys,project-dir)}

        RCSTopdiff& [sourcenav::Window::next_window_name] ${f} ${t_m_p}\
          ${stderr_msg} [sn_title DIFF] ${diffopt}
    }
}

proc sn_rcs_get_common_path {files} {
    if {$files == ""} {
	return
    }

    set dirname [file dirname [lindex $files 0]]

    foreach file $files {
	set filedir [file dirname $file]

	while {[string first $dirname $filedir] == -1} {
	    # TODO: check for error conditions.
	    set dirname [file dirname $dirname]
	    if {$dirname == "/"} {
		sn_error_dialog [get_indep String RCSRelativePathWarning]
	    }
	}
    }

    return $dirname
}

proc sn_rcs_process_filenames {userelativepath {files ""}} {
    if {$files == ""} {
	return
    }
    
    if {$userelativepath == 0} {
	return $files
    }

    set dirname [file dirname [lindex $files 0]]

    foreach file $files {
	set filedir [file dirname $file]
	while {[string first $dirname $filedir] == -1} {
	    # TODO: check for error conditions.
	    set dirname [file dirname $dirname]
 	    if {$dirname == "/"} {
		sn_error_dialog [get_indep String RCSRelativePathWarning]
	    }
	}
    }

    set filenames ""

    foreach $file $files {
	lappend filenames [file join [lrange [file split $file] [llength [file split $dirname]] end]]
    }

    return $filenames
}



