# Copyright (c) 2002, Mo DeJong
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

itcl::class sourcenav::ExecGrepDriver {
    inherit sourcenav::GrepDriver

    destructor {}

    public method start { pat files nocase max }

    public method finish {}

    public method cancel {}

    public method getScaleValueVariable {}

    public proc isAvailable {}

    public method setTextWidget { widget }

    public method isValidPattern { pat }

    private method ExecGrepEvent {cmd}

    private method grepReadableEvent {}

    private method close {}

    # Variables

    private common grep_exe
    private variable grep_running
    private variable grep_canceled
    private variable grep_fd ""
    private variable case_sensitive

    # Max number of matches to insert into text widget
    private variable maxmatches

    # Set to "1" if max seach limit is hit.
    private variable max_search_reached

    # Set to the pattern we are searching for only while
    # a grep is actually active, otherwise set to ""
    private variable activePattern ""

    # The text widget grep results will be added to
    private variable text ""

    # Map from file name to files list index
    private variable files_Matched

    # The number of files that are being searched through in a grep
    private variable files_Count 0

    # Variable linked to the progress bar display
    private variable scalevalue 0
}

itcl::body sourcenav::ExecGrepDriver::destructor { } {
    # Make sure any grep process is terminated
    if {${grep_fd} != "" && [catch {::close ${grep_fd}} err]} {
        sn_log "caught error in ExecGrepDriver dtor : \"$err\""
    }
}

itcl::body sourcenav::ExecGrepDriver::start { pat files nocase max } {
    global sn_path
    global errorInfo errorCode
 
    # We should have an empty buffer to start with
    if {$text == ""} {
        error "no text widget"
    }
    if {[$text index end] != "2.0"} {
        error "text widget $text is not empty"
    }

    if {$nocase} {
        set case_sensitive 0
    } else {
        set case_sensitive 1
    }

    set maxmatches $max

    # Count files
    set files_Count [llength ${files}]

    # The actual count is off by one because we update the % done meter
    # based on the match read back from grep
    incr files_Count -1

    # isAvailable has not been invoked, do it before using grep_exe
    if {![info exists grep_exe]} {
        if {![isAvailable]} {
            error "exec grep driver not available"
        }
    }

    # grep command
    set cmd [list $grep_exe "-n"]

    # Ignore case
    if {! ${case_sensitive}} {
        lappend cmd "-i"
    }

    lappend cmd "--"

    # The pattern to search for
    set activePattern ${pat}
    lappend cmd ${pat}

    sn_log "execing: ${cmd} \$FILES"

    # Add the files to search for to the
    # grep command. We also create an array
    # that mapps the file name to its
    # position in the search so that we
    # can update our % done meter. 

    # Clear out old array (it could be huge!)
    catch {unset files_Matched}

    set filecount 0
    set groupcount 0
    set groupsize 40
    set filegroups($groupcount) [list]

    # If there is only one file, add an empty dummy file
    # so that the first file's name will appear in
    # the grep output.

    if {[llength $files] == 1} {
        set dummy [sn_tmpFileName]
        ::close [open $dummy w]
        lappend files $dummy
    }

    set len [llength $files]
    for {set i 0} {$i < $len} {incr i} {
        set file [lindex $files $i]
        set files_Matched($file) $i
        if {$filecount > $groupsize} {
            set filecount 0
            incr groupcount
            set filegroups($groupcount) [list]
	}
        lappend filegroups($groupcount) $file
        incr filecount
    }

    set scalevalue 0
    set max_search_reached 0
    set errorInfo ""
    set counter 0
    set grep_canceled 0

    for {set groupindex 0} {$groupindex <= $groupcount} {incr groupindex} {
        sn_log "grouploop $groupindex : will run $cmd"
        set execCmd "$cmd $filegroups($groupindex)"

        if {$grep_canceled} {
            sn_log "breaking out of group loop because of grep_canceled"
            break
        }

        if {$max_search_reached} {
            sn_log "breaking out of group loop because of max_search_reached"
            break
        }

        set grep_running 1
        ExecGrepEvent $execCmd
        vwait [itcl::scope grep_running]
        set scalevalue [expr $groupindex * $groupsize + $groupsize]
    }

    if {[info exists dummy]} {
        file delete $dummy
    }
}

itcl::body sourcenav::ExecGrepDriver::finish { } {
#    sn_log "ExecGrepDriver::finish : [clock format [clock seconds]]"

    # Set progress bar/meter to 100%.
    set scalevalue ${files_Count}

    set t $text
    $t configure -state normal

    set pat $activePattern
    regsub -all "\<" ${pat} "\m" pat
    regsub -all "\>" ${pat} "\M" pat

    set pat_len 0
    set numlines [expr {int([$t index end]) - 1}]
    set index_list [list]

    if {! ${case_sensitive}} {
        set case -nocase
    } else {
        set case -exact
    }

    # Perform highlighing (colorization) of the results.

    set filename_len 0

    # {^(([A-Z].)(:?)([^:]*)|([^:]*))(:?)([0-9]+)(:?)}
    # matches things like:
    #
    #        /home/user/work/files/project.c:108:
    #        C:/home/user/work/files/project.c:108:
    #
    # So we don't color parts of the filename by mistake.

    set filename_pat {^(([A-Z].)(:?)([^:]*)|([^:]*))(:?)([0-9]+)(:?)}
    set line_index [$t search -count filename_len -regexp -- \
         $filename_pat 1.0 end]
  
    while {$line_index != ""} {
        set start_next_line [list $line_index + 1 lines]
        set line_index [list $line_index + $filename_len chars]
        set index [$t search $case -count pat_len -regexp -- \
	    $pat $line_index $start_next_line]

        if {$index == ""} {
            sn_log "GREP: Colorizing error at ($line_index,$start_next_line)\
                or attempting to colorize a Formatted pattern >$pat<."
            break
        }

        # Now that we've skipped over the filename and
        # line number we can tag every match until the
        # the line.

        while {$index != ""} {
            set end_index [list $index + $pat_len chars]
            lappend index_list $index $end_index
            set index [$t search $case -count pat_len -regexp -- \
	        $pat $end_index $start_next_line]
        }

        # Skip the filename and line number at the begining again.

	set line_index [$t search -count filename_len -regexp -- \
            $filename_pat $end_index end]
    }

    if {$index_list != {}} {
        eval {$t tag add grep} ${index_list}
    }

    if {$max_search_reached} {
        $t insert end \n
        $t insert end [get_indep String GrepTruncatedString]
        $t insert end \n
    } elseif {$grep_canceled} {
        # Check if the last character in the buffer is a
        # newline and add one if needed.

        for {set i 1} {1} {incr i} {
            set index [$t index [list end - $i char]]
            if {[$t get $index] != "\n" || $index == "1.0"} {
                break
            }
        }

        set lastline [expr {int($index)}]
        set lastline_text [$t get $lastline.0 {end - 1 char}]
        if {[string index $lastline_text end] != "\n"} {
            $t insert end \n
        }

        $t insert end [get_indep String GrepCanceledString]
        $t insert end \n
    }

    $t configure -state disabled
    sn_log "Done tagging : [clock seconds], $numlines lines"
}

itcl::body sourcenav::ExecGrepDriver::ExecGrepEvent {cmd} {
    global sn_options
    global errorInfo errorCode env
    global tcl_platform

    #make sure we stay in project directory (different interpeters).
    catch {cd $sn_options(sys,project-dir)}

    if {$tcl_platform(platform) != "windows"} {
        set home $env(HOME)
        if {[string compare ${home} "/"] != 0} {
            append home "/"
        }
        regsub -all "~/" ${cmd} ${home} cmd
    }

    sn_log "Start exec grep : [clock seconds]"

    if {[catch {set grep_fd [open "| ${cmd}" r]} errmsg]} {
        sn_error_dialog "-code error -errorinfo ${errorInfo}\
            -errorcode ${errorCode} ${errmsg}"
        return
    }

# FIXME: we should play around with the default buffer size to see
# if we can improve the grep read performance.
    fconfigure ${grep_fd} \
        -encoding $sn_options(def,system-encoding) \
        -blocking 0 \
        -buffering full

    if {[catch {fileevent ${grep_fd} readable \
            [itcl::code ${this} grepReadableEvent]} err]} {
        sn_log "caught error assigning fileevent : \"$err\""
        ${this} close
    }
}

itcl::body sourcenav::ExecGrepDriver::close {} {
    global sn_options

    sn_log "Close exec grep : [clock seconds]"

    # close older descriptor to be secure that no other grep
    # is running

    if {${grep_fd} != "" && [catch {::close ${grep_fd}} err]} {
        sn_log "caught error while closing grep fd : \"$err\""
    }
    sn_log "closed grep fd ${grep_fd}"
    set grep_fd ""
    set grep_running 0
}

itcl::body sourcenav::ExecGrepDriver::grepReadableEvent {} {
    set close_it 0

    if {[catch {read ${grep_fd}} buf] || [eof ${grep_fd}]} {
        sn_log "closing grep fd because of read error or EOF"
        set close_it 1
    } else {
        sn_log "grepReadableEvent read [string length $buf] bytes\
            ([llength [split $buf \n]] lines)"
        #sn_log "\"$buf\""

        set t $text
        $t config -state normal
        $t insert end $buf

        # Trim the grep to the maximum number of lines
        # Walk backwards from the end of the buffer a
        # character at a time until we find a non-newline.

        for {set i 1} {1} {incr i} {
            set index [$t index [list end - $i char]]
            if {[$t get $index] != "\n" || $index == "1.0"} {
                break
            }
        }
        set numlines [expr {int($index)}]
        
        if {$numlines > $maxmatches} {
            set close_it 1
            set max_search_reached 1
            $t delete [list $maxmatches.0 + 1 lines] end

            # Walk back again to get the trimmed numlines
            for {set i 1} {1} {incr i} {
                set index [$t index [list end - $i char]]
                if {[$t get $index] != "\n" || $index == "1.0"} {
                    break
                }
            }
            set numlines [expr {int($index)}]

            sn_log "trimmed results to $numlines lines"
        }

        $t see end

        # Update the % done meter, we grab the last full
        # line out of the match, get the file name, and
        # then map that to the file position in the search.

        set last [$t get $numlines.0 $numlines.end]
        sn_log "Last line from grep is \"$last\""

        if {$last != ""} {
	    set last_file [lindex [split $last :] 0]
            if {[info exists files_Matched($last_file)]} {
                sn_log "Last file in read buffer is \"$last_file\""
                set scalevalue $files_Matched($last_file)
                sn_log "New scale value is $scalevalue of ${files_Count}"
            }
        }

        $t config -state disabled
    }

    if {$close_it} {
        ${this} close
    }

    # Note: It is critically important that we never
    # call update inside this fileevent callback!
    # If we did, it would recursively process fileevent
    # callbacks. That could trash the stack if the
    # grep results were large enough.
}

itcl::body sourcenav::ExecGrepDriver::cancel {} {
    set grep_canceled 1
    ${this} close
}

itcl::body sourcenav::ExecGrepDriver::getScaleValueVariable {} {
    return [itcl::scope scalevalue]
}

itcl::body sourcenav::ExecGrepDriver::isAvailable {} {
    global tcl_platform sn_path

    if {[info exists grep_exe]} {
        return 1
    }

    # If there is a grep in the directory with the other binaries, use it.
    # Otherwise, check for grep on the path

    if {$tcl_platform(platform) == "windows"} {
        set exeext .exe
    } else {
        set exeext ""
    }

    set grep_cmd [file join $sn_path(bindir) "grep$exeext"]

    if {! [file exists $grep_cmd]} {
        set grep_cmd "egrep"
    }

    # Pass in an empty second file to make the first file's name
    # actually gets printed in the grep output.

    set f1 [sn_tmpFileName]
    set fd [open $f1 w]
    puts $fd "ONE"
    ::close $fd

    set f2 [sn_tmpFileName]
    ::close [open $f2 w]

    sn_log "Attempting to grep exec check : $grep_cmd -n -i -- one $f1 $f2"

    if {[catch {exec $grep_cmd -n -i -- one $f1 $f2} str]} {
        sn_log "grep exec check failed \"$str\""
    } else {
        set expected "$f1:1:ONE"

        if {[string equal $str $expected]} {
            set grep_exe $grep_cmd
            sn_log "Found usable grep: $grep_exe"
            file delete $f1 $f2
            return 1
        } else {
            sn_log "Expected \"$expected\" but got \"$str\""
        }
    }

    file delete $f1 $f2
    return 0
}

itcl::body sourcenav::ExecGrepDriver::setTextWidget { widget } {
    set text $widget
}

itcl::body sourcenav::ExecGrepDriver::isValidPattern { pat } {
    return 1
}
