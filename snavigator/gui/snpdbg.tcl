# Copyright (c) 2004, Mo DeJong.
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
# snpdbg.tcl - Parser debug program support code


# Start SN Parser debug session.

proc snpdbg_session {} {
    global snpdbg_optvar
    global Avail_Parsers
    global Parser_Info
    global sn_options
    global sn_path

    set notimp 0

    if {$notimp} {
        tk_dialog auto "Not Implemented Yet!" "Not Implemented Yet!" \
          question_image 0 Ok
        exit
    }

    set snpdir [file join [pwd] SNPDBG]
    if {[file isdirectory $snpdir]} {
        foreach file [glob -nocomplain $snpdir/*] {
            file delete $file
        }
    } else {
        file mkdir $snpdir
    }
    sn_set_tmp_dir $snpdir


    wm withdraw .
    # Create a window that asks the use what they want to do
    set type_d [sourcenav::Dialog .snpdbg -modality application -background white]
    set hull [$type_d component hull]

    $type_d configure -title "Source-Navigator Parser Debugger"

    image create photo sn_logo \
        -file [file join $sn_path(bitmapdir) sn_logo.gif]

    set img [label $hull.img -image sn_logo -bg white]
    set msg [message $hull.msg -aspect 1000 -text "What would you like to do?" -bg white]

    set txt_one "Choose a directory containing files to parse"
    set txt_two "Choose a single file to parse"

    set snpdbg_optvar 1
    set opt1 [checkbutton $hull.opt1 -text $txt_one -variable snpdbg_optvar -onvalue 1 \
            -bg white]
    set opt2 [checkbutton $hull.opt2 -text $txt_two -variable snpdbg_optvar -onvalue 2 \
            -bg white]

    set button_frame [frame $hull.buttons]
    set continue_button [button $button_frame.continue -text Continue \
        -command [list $type_d deactivate continue] -bg white]
    set exit_button [button $button_frame.exit -text Exit \
        -command [list $type_d deactivate exit] -bg white]

    grid rowconfigure $hull 0 -minsize 15
    grid $img  -row 1 -column 1 -sticky w
    grid $msg  -row 2 -column 1 -sticky w
    grid $opt1 -row 3 -column 1 -sticky w
    grid $opt2 -row 4 -column 1 -sticky w
    grid rowconfigure $hull 5 -minsize 15

    pack $continue_button $exit_button -side left
    grid $button_frame -row 6 -column 1
    grid rowconfigure $hull 5 -minsize 15

    grid columnconfigure $hull 0 -minsize 15
    grid columnconfigure $hull 2 -minsize 15

    set result [$type_d activate]

    if {$result == "continue"} {
        #puts "continue: option chosen was $snpdbg_optvar"

        if {$snpdbg_optvar == 2} {
            # Get a single file and parse it
            set file [Editor&::FileDialog $hull \
                -title [get_indep String Open] \
                -save_open open \
                -initialdir [pwd]]
            #puts "file is \"$file\""
            if {$file == ""} {
                exit
            }
            if {[sn_get_file_type $file] == "others"} {
                # Show error dialog
                exit
            }

            destroy $type_d
            set files [list $file]
            snpdbg_parse_files $files
        } elseif {$snpdbg_optvar == 1} {
            # Get files in dir and its subdirs and parse them
            set dir [Editor&::DirDialog $hull \
                -title [get_indep String Open] \
                -dir [pwd]]
            #puts "dir is \"$dir\""

            # Concatenate the known file extensions!
            set glob_expr [list]
            foreach p ${Avail_Parsers} {
                foreach e $Parser_Info(${p},SUF) {
                    if {[string index ${e} 0] != "*"} {
                        set e "*${e}"
                    }
                    lappend glob_expr [list ${e}]
                }
            }
	    set glob_expr [join [lunique [lsort [join ${glob_expr}]]]]

            set unfiles [sn_glob -nocomplain \
                    -match ${glob_expr} \
                    -ignore $sn_options(def,ignored-directories) \
                    -- $dir]
            foreach file $unfiles {
                if {![file isfile $file]} { continue }
                #puts "file is \"$file\""
                set type [sn_get_file_type $file]
                if {$type != "others"} {
                    lappend files_map($type) $file
                }
	    }
            #puts "filtered based on extension"

            set files [list]
            foreach type [lsort -dictionary [array names files_map]] {
                foreach file $files_map($type) {
                    lappend files $file
                }
            }

            #foreach file $files {
            #    puts "browser file is \"$file\""
            #}

            destroy $type_d
            snpdbg_parse_files $files
        } else {
            error "unknown option \"$snpdbg_optvar\""
        }
    } else {
        exit
    }
}

proc snpdbg_parse_files { files } {
    global Parser_Info
    global sn_options
    global sn_path

    set snpdir [file join [pwd] SNPDBG]
    cd $snpdir


    # Create a toplevel that will display parser results
    set type_d [sourcenav::Window .snpdbg -geometry 700x400 \
        -background white -title "Source-Navigator Parser Debugger"]

    $type_d on_close {exit}

    set hull [$type_d component hull]

    set notebook [tixNoteBook $hull.nb]

    set files_frame [${notebook} add files \
        -label Files \
        -under 0]

    set exec_frame [${notebook} add exec \
        -label Exec \
        -under 0]

    pack $notebook -fill both -expand true

    set files_text [text $files_frame.t -bg white]
    set files_scroll [scrollbar $files_frame.s -orient vertical]

    $files_text configure -yscrollcommand [list $files_scroll set]
    $files_scroll configure -command [list $files_text yview]

    set exec_text [text $exec_frame.t -bg white]
    set exec_scroll [scrollbar $exec_frame.s -orient vertical]

    $exec_text configure -yscrollcommand [list $exec_scroll set]
    $exec_scroll configure -command [list $exec_text yview]

    pack $files_scroll $exec_scroll -side right -fill y
    pack $files_text $exec_text -fill both -expand true

    set crashing_files [list]

    foreach file $files {
        set crashed 0

        $files_text insert end "$file: "
        $exec_text insert end "$file:\n"
        update
        #puts "Processing file \"$file\""

        set rname [file tail $file]
        set type [sn_get_file_type $file]
        set brow_cmd $Parser_Info(${type},BROW)
        set brow_tail [file rootname [file tail $brow_cmd]]

        set xref ${rname}_xref
        set browser_output ${rname}_${brow_tail}
        set dbimp_output ${rname}_dbimp
        set dbimp_cmd [list [file join $sn_path(parserdir) dbimp]]

        # If parsing c/c++ file with cbrowser, pass
        # a flag to sn_load_xref so that it knows to
        # use cbrowser2 in the xref gen stage.
        if {[string first cbrowser $brow_cmd] != -1} {
            set cbrowser_xref 1
        } else {
            set cbrowser_xref 0
        }

        # FIXME: Don't think this works without current project
        #append macro files to the parser
        set macroflag $Parser_Info(${type},MACRO)
        if {${macroflag} == "-m"} {
            foreach mf $sn_options(macrofiles) {
                lappend brow_cmd -m ${mf}
            }
        }

        if {$Parser_Info(${type},BROW_SWITCH) != ""} {
            lappend brow_cmd $Parser_Info(${type},BROW_SWITCH)
        }
        #'-t' means drop /... files.
        lappend brow_cmd -t

        # FIXME: We don't pass include file dirs from a project

        if {$cbrowser_xref} {
            lappend brow_cmd -n snpdbg
        }

        set cmd $brow_cmd
        lappend cmd $file

        $files_text insert end "${brow_tail} "
        $exec_text insert end "$cmd > $browser_output\n"

        set result [snpdbg_run $cmd $browser_output]
        if {[string range $result 0 6] == "CRASHED"} {
            $files_text insert end "CRASHED "
            $exec_text insert end "$result\n"
            set crashed 1
        } elseif {$result != {}} {
            $exec_text insert end "$result\n"
        }
        update

        # Invoke dbimp to process symbols output by browser
        lappend dbimp_cmd -c 300 -C 3000

        set cmd $dbimp_cmd
        lappend cmd -f $browser_output snpdbg

        $files_text insert end "dbimp "
        $exec_text insert end "$cmd\n"
 
        set result [snpdbg_run $cmd]
        if {[string range $result 0 6] == "CRASHED"} {
            $files_text insert end "CRASHED "
            $exec_text insert end "$result\n"
            set crashed 1
        } elseif {$result != {}} {
            $exec_text insert end "$result\n"
        }
        update

        # Invoke browser again with "-x xref_filename" flag
        set cmd $brow_cmd
        lappend cmd -x $xref $file

        $files_text insert end "xref "
        $exec_text insert end "$cmd > /dev/null\n"

        set result [snpdbg_run $cmd /dev/null]
        if {[string range $result 0 6] == "CRASHED"} {
            $files_text insert end "CRASHED "
            $exec_text insert end "$result\n"
            set crashed 1
        } elseif {$result != {}} {
            $exec_text insert end "$result\n"
        }
        update

        # Invoke cbrowser2 if needed to process the xref output
        if {$cbrowser_xref} {
            # Convert .../cbrowser to  .../cbrowser2
            set cbr2_cmd $Parser_Info(${type},BROW)
            set dirname [file dirname $cbr2_cmd]
            set cbr2_cmd [file join $dirname cbrowser2]
            set cbr2_cmd [list $cbr2_cmd]

            set macroflag $Parser_Info(${type},MACRO)
            if {${macroflag} == "-m"} {
                foreach mf $sn_options(macrofiles) {
                    lappend cbr2_cmd -m ${mf}
                }
            }         

            if {$Parser_Info(${type},BROW_SWITCH) != ""} {
                lappend cbr2_cmd $Parser_Info(${type},BROW_SWITCH)
            }

            lappend cbr2_cmd -n snpdbg
            lappend cbr2_cmd -c 300 -C 3000
            lappend cbr2_cmd $xref

            # Output of cbrowser2 becomes input to dbimp
            append browser_output "2"
            set xref $browser_output

            set cmd $cbr2_cmd

            $files_text insert end "cbrowser2 "
            $exec_text insert end "$cmd > $browser_output\n"

            set result [snpdbg_run $cmd $browser_output]
            if {[string range $result 0 6] == "CRASHED"} {
                $files_text insert end "CRASHED "
                $exec_text insert end "$result\n"
                set crashed 1
            } elseif {$result != {}} {
                $exec_text insert end "$result\n"
            }
            update
        }

        # Invoke dbimp to procss xref symbols
        set cmd $dbimp_cmd
        lappend cmd -f $xref snpdbg

        $files_text insert end "dbimp "
        $exec_text insert end "$cmd\n"

        set result [snpdbg_run $cmd]
        if {[string range $result 0 6] == "CRASHED"} {
            $files_text insert end "CRASHED "
            $exec_text insert end "$result\n"
            set crashed 1
        } elseif {$result != {}} {
            $exec_text insert end "$result\n"
        }

        $files_text insert end "\n"
        
        $files_text see end
        $exec_text see end
        update

        if {$crashed} {
            lappend crashing_files $file
        }
    }

    set new_db_files [glob -nocomplain snpdbg.*]
    if {$new_db_files != {}} {
        $files_text insert end "Created Databases \"$new_db_files\"\n"
    }

    if {$crashing_files != {}} {
        $files_text insert end "Crash detected in $crashing_files\n"
    } else {
        $files_text insert end "No Parser Crashes detected\n"
    }
    $files_text see end
}

proc snpdbg_run { cmd {redir {}} } {
    global snpdbg_pipewait
    global snpdbg_piperead
    global snpdbg_pipecrash

    #puts "cmd is \"$cmd\""

    # Open pipe to subprocess
    if {[catch {set fd [open "| ${cmd}" r]} err]} {
        sn_error_dialog ${err}
        return
    }

    fconfigure $fd \
        -encoding utf-8 \
        -blocking 0

    set snpdbg_pipewait 0
    set snpdbg_piperead ""
    set snpdbg_pipecrash ""

    fileevent $fd readable [list snpdbg_readable $fd]

    vwait snpdbg_pipewait

    if {$redir != {} && $redir != "/dev/null"} {
        #puts "redirecting snpdbg_piperead of \"$snpdbg_piperead\" to $redir"

        set fd [open $redir w]
        fconfigure $fd -encoding utf-8
        puts -nonewline $fd $snpdbg_piperead
        close $fd
        set snpdbg_piperead ""
    } elseif {$redir == "/dev/null"} {
        set snpdbg_piperead ""
    }

    if {$snpdbg_pipecrash != ""} {
        return "CRASHED: $snpdbg_pipecrash"
    }

    return $snpdbg_piperead
}

proc snpdbg_readable { fd } {
    global snpdbg_pipewait
    global snpdbg_piperead
    global snpdbg_pipecrash

    if {[eof $fd]} {
        fconfigure $fd -blocking 1

        set err ""
        set status [catch {close $fd} err]
        #puts "pipe close-exit status: ${status}, ${err}"

        if {$status &&
                ([string match "*child killed*" $err] ||
                 [string match "*abnormal program termination*" $err] ||
                 [string match "*child process exited abnormally*" $err])} {
            set snpdbg_pipecrash $err
        }

        set snpdbg_pipewait 1
    } else {
        set data [read -nonewline $fd]
        #puts "read data \"$data\" from pipe"
        append snpdbg_piperead $data
    }
}




# Parser Exec Examples:

# Non xref build:
# exlbrowser -t -y /usr/tmp/parse_example/.snprj/tmp_jNv3E1 -I /usr/tmp/parse_example/.snprj/tmp_XGZA4m | dbimp -c 300 -H modrick.localdomain -P 25236 .snprj/parse_example


# XRef build:
# exlbrowser -t -y /usr/tmp/parse_example/.snprj/tmp_UrblBo -x /usr/tmp/parse_example/.snprj/tmp_8hea5u -I /usr/tmp/parse_example/.snprj/tmp_IwHTXh | /share/SN52A/install/bin/dbimp -c 300 -H modrick.localdomain -P 25245 .snprj/parse_example
# dbimp -H modrick.localdomain -P 25245 -c 300 -C 3000 -f /usr/tmp/parse_example/.snprj/tmp_8hea5u .snprj/parse_example


# cbrowser -t -n .snprj/tcl_n_c -y /usr/tmp/tcl_n_c/.snprj/tmp_UjCHNR -x /usr/tmp/tcl_n_c/.snprj/tmp_bh0Usd -I /usr/tmp/tcl_n_c/.snprj/tmp_F5U1b3 | dbimp -c 300 -H modrick.localdomain -P 1023 .snprj/tcl_n_c
