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
# grep.tcl - Operations using the familiar grep(1) command.
# Copyright (C) 1998 Cygnus Solutions.

# Get a search string & execute a grep command.
# If a pattern is not provided get it from the selection.
#

# FIXME : this needs to switch to the grep window that
# is currently accessable, but how do we know which on that is?
# It gets even worse when dealing with multiple projects in
# a single interp (We need a redesign!)

proc sn_grep {{pat {}}} {
    if {$pat == {} && [catch {set pat [string trim [selection get]]}]} {
        bell
        sn_error_dialog \
	    [get_indep String NoSelection] \
	    [get_indep String MultiGrep]
        return
    }

    sn_error_dialog NOT_IMPLEMENTED_YET(sn_grep)

    return
}

# This method seems to be called when you double click
# in the results list of a grep. It would seem that
# it would open an editor on the given line.

proc sn_grep_edit_file {line} {
    global sn_options

    #make sure we stay in project directory (different interpeters).
    catch {cd $sn_options(sys,project-dir)}

    set pars [sn_goto_comp_error_or_grep ${line}]
    set file [lindex ${pars} 0]
    set pos [lindex ${pars} 1]

    #be sure that file is not empty, this cause an error
    if {${file} == ""} {
        return no
    }

    sn_log "sn_grep_edit_file file <${file}> line <${pos}>"

    set conv_file [sn_convert_FileName ${file}]
    
    sn_log "sn_convert_FileName \"${file}\" -> \"${conv_file}\""
    
    if {${pars} == "" || ![file exists ${conv_file}] || ${pos} == ""} {
        bell
        return no
    }

    sn_edit_file dummy ${conv_file} ${pos}
    return
}

# This method is like sn_grep_edit_file except that it
# does some extra work to try to find files in
# the project. This is needed because file names returned
# by tools (like gcc) may return path names that
# are relative to the build directory or a subdirectory
# of the build directory. These file might not live
# in a subdirectory of the project file, so we look
# for file names in the database using pattern matching.

proc sn_make_edit_file {line} {
    global sn_options tcl_platform

    #make sure we stay in project directory (different interpeters).
    catch {cd $sn_options(sys,project-dir)}

    set pars [sn_goto_comp_error_or_grep ${line}]
    set file [lindex ${pars} 0]
    set pos [lindex ${pars} 1]

    if {${file} == ""} {
        return no
    }

    sn_log "sn_make_edit_file file <${file}> line <${pos}>"

    if {$tcl_platform(platform) == "windows" &&
        [file pathtype ${file}] == "volumerelative"} {
        # Tcl reports a path like /usr/include/assert.h
        # as volumerelative under Windows. We treat
        # this as a Cygwin path.

        if {[catch {exec cygpath -w $file} err]} {
            sn_log "Cygwin conversion failed : $err"
        } else {
            sn_log "Cygwin conversion, <${file}> -> <${err}>"

            if {[file exists ${err}]} {
                set file $err
            } else {
                sn_log "Cygwin converted file <${err}> did not exist"
            }
        }

        set conv_file [sn_convert_FileName ${file}]
    } elseif {[file pathtype ${file}] == "absolute"} {
        set conv_file [sn_convert_FileName ${file}]
    } else {
        # We have a relative path, there is
	# no way to know what make subdirectory
	# it might be relative to, so we also try
	# to match based on the last file element.
	#
	# This could match with */bar.c for
	# a path like ../../foo/bar.c
	set conv_file [sn_convert_FileName ${file} 1]
    }

    sn_log "sn_convert_FileName \"${file}\" -> \"${conv_file}\""
    
    if {${conv_file} == "" || ![file exists ${conv_file}] || ${pos} == ""} {
        bell
        return no
    }

    sn_edit_file dummy ${conv_file} ${pos}
    return
}

# This function tries to figure out the name of a file
# and position from a line. It returns a pair consisting
# of a file name and a line number.
proc sn_goto_comp_error_or_grep {sym} {
    global grep_compiled_err_patterns
    global tcl_platform sn_options

    if {![info exists grep_compiled_err_patterns]} {
        set name [sn_search_file sn_cmp_g.pat]

        set fd [open ${name}]
        fconfigure ${fd} -encoding $sn_options(def,system-encoding) -blocking 0
        set lst [read -nonewline ${fd}]
        set lst [split ${lst} "\n"]
        close ${fd}

        set grep_compiled_err_patterns [lmatch -regexp ${lst} {^[^#]}]
    }

    set file ""
    set line 0
    foreach pat ${grep_compiled_err_patterns} {
        set sv1 ""
        set sv2 ""
        set mat [regexp -- ${pat} ${sym} mat_var sv1 sv2]
        if {${mat}} {
            if {[string match {[0-9]*} ${sv2}]} {
                set file ${sv1}
                set line ${sv2}
            } else {
                set file ${sv2}
                set line ${sv1}
            }
            # Convert e.g. 123,35 to 123.35!
            regsub {,} ${line} {.} line
            return [list ${file} ${line}]
        }
    }
    return ""
}

