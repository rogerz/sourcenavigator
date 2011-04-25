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
# utilities.tcl - Globals/parameters and useful Tcl procs.
# Copyright (C) 1998 Cygnus Solutions.

# get_userid
# Returns the real uid of the user running the application. 0 is
# returned if it cannot be obtained for some reason.  (This is
# safe--we cannot and do not use uid 0 for anything other than
# generating content for lock files).

proc get_userid {} {
    global tcl_platform

    if {$tcl_platform(platform) != "unix"} {
        return 0
    }

    # This assumes that id(1) returns a string roughly of the form:
    # uid=501(bje) gid=100(cygnus) groups=100(cygnus)

    catch {exec id} output
    if {[regexp {[0-9]+} ${output} uid] != 1} {
        return 0
    } else {
        return ${uid}
    }
}

# get_username
# Returns the login name of the user running the application.  If the
# USERNAME environment variable isn't set on Windows, we just make do.

proc get_username {} {
    global env tcl_platform

    switch -- $tcl_platform(platform) {
        "windows" {
                if {[info exists $env(USERNAME)]} {
                    return $env(USERNAME)
                } else {
                    return unknown
                }
            }
        "unix" {
                if {[catch {exec logname} username]} {
                    return unknown
                } else {
                    return ${username}
                }
            }
        default {
                return unknown
            }
    }
}

# lunique
# This procedure returns a list which contains only unique elements.
# The input list must be sorted.

proc lunique {sortedlist} {
    set first 1
    set prev {}
    set uniqlist {}
    foreach elt ${sortedlist} {
        if {[string compare ${elt} ${prev}]} {
            lappend uniqlist ${elt}
        }\
        elseif {${first}} {
            # Starting condition.
            lappend uniqlist ${elt}
        }
        #first must be set after the first step, regardless,
        #if we have empty string at the beginning
        set first 0
        set prev ${elt}
    }
    return ${uniqlist}
}

# sn_file_format
# Tests a file on its newline type and returns:
#  auto      file not found
#  lf        file contains NL (UNIX)
#  crlf      file contains NL and CR (Win32)
#  cr        file contains CR only

proc sn_file_format {filename} {
    if {![file isfile ${filename}] || [catch {set f [open ${filename}]}]} {
        return auto
    }

    fconfigure ${f} -translation binary -blocking 0
# FIXME: if the file being tested is really large, this could really be slow!
    set buffer [read ${f}]
    close ${f}

    # Look for carriage return/linefeed patterns in the buffer.

    if {[string first \x0a\x0d ${buffer}] != -1 || [string first \x0d\x0a\
      ${buffer}] != -1} {
        return crlf
    }
    if {[string first \x0a ${buffer}] != -1} {
        return lf
    }
    if {[string first \x0d ${buffer}] != -1} {
        return cr
    }
    return auto
}

# A `grep' equivalent for Tcl.  Returns a list of all list elements matching
# the given regular expression.

proc sn_lgrep {pattern text} {
    set result ""
    foreach line ${text} {
        if {[regexp ${pattern} ${line} ignore match] > 0} {
            lappend result ${match}
        }
    }
    return ${result}
}

# A search-and-replace mechanism for replacing text in a list of strings.
# The `replacements' argument should be a list of search/replace pairs, where
# a pair is defined to be a list of two elements (ie. {{a b} {c d}} will
# replace all a's with b's and c's with d's.  The result is placed in the
# variable `newtext' and the procedure always returns 0.

proc sn_search_replace {replacements text newtext} {
    upvar ${newtext} newText
    set t ${text}

    foreach pair ${replacements} {
        set search [lindex ${pair} 0]
        set replace [lindex ${pair} 1]

        regsub -all ${search} ${t} ${replace} t
    }
    set newText ${t}
    return 0
}

# Save the contents of a specified text widget into a specified file
# The previous contents of the file is saved to a ".bak" file before being 
# overwritten.
#
proc sn_save_file {w file} {
    global sn_options
    global tcl_platform

    #try to save the file in it's original format, if user
    #wants to
    if {$sn_options(def,edit-file-translation) == "keep"} {
        set file_format [sn_file_format ${file}]
    } else {
        set file_format $sn_options(def,edit-file-translation)
    }
    sn_log "file OS-format: <${file_format}>"

    if {[file exists ${file}]} {
        if {$tcl_platform(platform) != "windows"} {
            set perm [file attributes ${file} -permission]
        }
        if {$sn_options(def,edit-create-bak) == 1} {
            # Rename the file!
            catch {file rename -force ${file} ${file}.bak}
        }
        set ideevent file-changed
    } else {
        set ideevent file-created
        set perm ""
    }

    if {[catch {set savefd [eval open [list ${file}] "w+"]} err]} {
        sn_error_dialog ${err}

        return 0
    }

    #set translation flag and encoding
    fconfigure ${savefd} -translation "auto ${file_format}"\
      -encoding $sn_options(def,encoding) \
      -blocking 0

    puts -nonewline ${savefd} [${w} get 1.0 end]
    close ${savefd}

    if {$tcl_platform(platform) != "windows" && ${perm} != ""} {
        catch {file attributes ${file} -permission ${perm}}
    }

    maybe_ide_event post ${ideevent} ${file}

    return 1
}

proc sn_load_pixmaps {} {
    global sn_options
    global sn_path
    global errorInfo errorCode

    set pixmap "pixmap"
    set xpm "xpm"

    sn_log -l 2 "PIXMAP: ${pixmap}, xpm: ${xpm}"
    set bitd $sn_path(bitmapdir)

    if {[catch {
        # FIXME: This is a terrible name for an arrow xpm!
        image create ${pixmap} add_image -file ${bitd}/add.${xpm}
        image create ${pixmap} addall_image -file ${bitd}/addall.${xpm}
        image create ${pixmap} arrow_image -file ${bitd}/arrow.${xpm}
        image create ${pixmap} browse_image -file ${bitd}/browse.${xpm}
        image create ${pixmap} classes_image -file ${bitd}/classes.${xpm}
        image create ${pixmap} compile_image -file ${bitd}/compile.${xpm}
        image create ${pixmap} copy_image -file ${bitd}/copy.${xpm}
        image create ${pixmap} crossref_image -file ${bitd}/crossref.${xpm}
        image create ${pixmap} cut_image -file ${bitd}/cut.${xpm}
        image create ${pixmap} del_image -file ${bitd}/del.${xpm}
        image create ${pixmap} delall_image -file ${bitd}/delall.${xpm}
        image create ${pixmap} down_image -file ${bitd}/down.${xpm}
        image create ${pixmap} files_image -file ${bitd}/files.${xpm}
        image create ${pixmap} find_image -file ${bitd}/find.${xpm}
        image create ${pixmap} find_image -file ${bitd}/find.${xpm}
        image create ${pixmap} function_image -file ${bitd}/function.${xpm}
        image create ${pixmap} go_image -file ${bitd}/go.${xpm}
        image create ${pixmap} grep_image -file ${bitd}/grep.${xpm}
        image create ${pixmap} include_image -file ${bitd}/include.${xpm}
        image create ${pixmap} left_image -file ${bitd}/left.${xpm}
        image create ${pixmap} del_left_image -file ${bitd}/tilda_left.${xpm}
        image create ${pixmap} method_image -file ${bitd}/method.${xpm}
        image create ${pixmap} new_image -file ${bitd}/new.${xpm}
        image create ${pixmap} next_image -file ${bitd}/next.${xpm}
        image create ${pixmap} open_image -file ${bitd}/open.${xpm}
        image create ${pixmap} paste_image -file ${bitd}/paste.${xpm}
        image create ${pixmap} print_image -file ${bitd}/print.${xpm}
        image create ${pixmap} rebuild_image -file ${bitd}/build.${xpm}
        image create ${pixmap} right_image -file ${bitd}/right.${xpm}
        image create ${pixmap} del_right_image -file ${bitd}/tilda_right.${xpm}
        image create ${pixmap} save_image -file ${bitd}/save.${xpm}
        image create ${pixmap} search_image -file ${bitd}/search.${xpm}
        image create ${pixmap} sign_image -file ${bitd}/sign.${xpm}
        image create ${pixmap} stree_image -file ${bitd}/stree.${xpm}
        image create ${pixmap} tree_image -file ${bitd}/tree.${xpm}
        image create ${pixmap} undo_image -file ${bitd}/undo.${xpm}
        image create ${pixmap} waste_image -file ${bitd}/waste.${xpm}
        image create ${pixmap} watch_image -file ${bitd}/watch.${xpm}

        image create ${pixmap} enabled_image -file ${bitd}/enabled.${xpm}
        image create ${pixmap} unvisited_image -file ${bitd}/unvisited.${xpm}
        image create ${pixmap} undefined_image -file ${bitd}/undef.${xpm}

        #browser images
        image create ${pixmap} cls_br__image -file ${bitd}/clsbr.${xpm}
        image create ${pixmap} cls_br_v_image -file ${bitd}/clsbr_v.${xpm}
        image create ${pixmap} cls_br_v+_image -file ${bitd}/clsbr_v+.${xpm}
        image create ${pixmap} cls_br_v+-_image -file ${bitd}/clsbr_v+-.${xpm}
        image create ${pixmap} cls_br_v-_image -file ${bitd}/clsbr_v-.${xpm}
        image create ${pixmap} cls_br_+_image -file ${bitd}/clsbr_+.${xpm}
        image create ${pixmap} cls_br_+-_image -file ${bitd}/clsbr_+-.${xpm}
        image create ${pixmap} cls_br_-_image -file ${bitd}/clsbr_-.${xpm}
        image create ${pixmap} cls_br_s_image -file ${bitd}/clsbr_s.${xpm}
        image create ${pixmap} cls_br_s+_image -file ${bitd}/clsbr_s+.${xpm}
        image create ${pixmap} cls_br_s-_image -file ${bitd}/clsbr_s-.${xpm}
        image create ${pixmap} cls_br_s+-_image -file ${bitd}/clsbr_s+-.${xpm}
        image create ${pixmap} cls_br_ps_image -file ${bitd}/clsbr_s.${xpm}
        image create ${pixmap} cls_br_p_image -file ${bitd}/clsbr_p.${xpm}
        image create ${pixmap} cls_br_pv_image -file ${bitd}/clsbr_pv.${xpm}
        image create ${pixmap} cls_br_pv+_image -file ${bitd}/clsbr_pv+.${xpm}
        image create ${pixmap} cls_br_pv+-_image -file ${bitd}/clsbr_pv+-.${xpm}
        image create ${pixmap} cls_br_pv-_image -file ${bitd}/clsbr_pv-.${xpm}
        image create ${pixmap} cls_br_p+_image -file ${bitd}/clsbr_p+.${xpm}
        image create ${pixmap} cls_br_p+-_image -file ${bitd}/clsbr_p+-.${xpm}
        image create ${pixmap} cls_br_p-_image -file ${bitd}/clsbr_p-.${xpm}
        image create ${pixmap} cls_br_private_image -file ${bitd}/clsbr_private.${xpm}

        #retriever images
        image create ${pixmap} type_cl_image -file ${bitd}/type_cl.${xpm}
        image create ${pixmap} type_cl+_image -file ${bitd}/type_cl+.${xpm}
        image create ${pixmap} type_cl-_image -file ${bitd}/type_cl-.${xpm}
        image create ${pixmap} type_com_image -file ${bitd}/type_com.${xpm}
        image create ${pixmap} type_con_image -file ${bitd}/type_con.${xpm}
        image create ${pixmap} type_cov_image -file ${bitd}/type_cov.${xpm}
        image create ${pixmap} type_ec_image -file ${bitd}/type_ec.${xpm}
        image create ${pixmap} type_e_image -file ${bitd}/type_e.${xpm}
        image create ${pixmap} type_fr_image -file ${bitd}/type_fr.${xpm}
        image create ${pixmap} type_fd_image -file ${bitd}/type_fd.${xpm}
        image create ${pixmap} type_fu_image -file ${bitd}/type_fu.${xpm}
        image create ${pixmap} type_gv_image -file ${bitd}/type_gv.${xpm}
        image create ${pixmap} type_iv_image -file ${bitd}/type_iv.${xpm}
        image create ${pixmap} type_lv_image -file ${bitd}/type_lv.${xpm}
        image create ${pixmap} type_ma_image -file ${bitd}/type_ma.${xpm}
        image create ${pixmap} type_mi_image -file ${bitd}/type_mi.${xpm}
        image create ${pixmap} type_md_image -file ${bitd}/type_md.${xpm}
        image create ${pixmap} type_su_image -file ${bitd}/type_su.${xpm}
        image create ${pixmap} type_t_image -file ${bitd}/type_t.${xpm}
        image create ${pixmap} type_un_image -file ${bitd}/type_un.${xpm}
        image create ${pixmap} type_ud_image -file ${bitd}/type_ud.${xpm}

        #cross reference images
        image create ${pixmap} cross_browse_to_image\
          -file ${bitd}/cross_to.${xpm}
        image create ${pixmap} cross_browse_by_image\
          -file ${bitd}/cross_by.${xpm}
        image create ${pixmap} cross_boxes_image -file ${bitd}/boxes.${xpm}
        image create ${pixmap} cross_param_image\
          -file ${bitd}/cross_param.${xpm}
        image create ${pixmap} cross_static_image\
          -file ${bitd}/cross_static.${xpm}
        image create ${pixmap} cross_disp_param_image\
          -file ${bitd}/cross_dispparam.${xpm}

        #image for retriever to store or close retriever window after selection
        image create ${pixmap} hold_image -file ${bitd}/hold.${xpm}
        image create ${pixmap} hold_on_image -file ${bitd}/hold_on.${xpm}
        image create ${pixmap} hold_off_image -file ${bitd}/hold_off.${xpm}

        image create ${pixmap} dir_image -file ${bitd}/dir.xpm
        image create ${pixmap} dir+_image -file ${bitd}/dir_+.xpm
        image create ${pixmap} dir-_image -file ${bitd}/dir_-.xpm
        image create ${pixmap} file_image -file ${bitd}/file.xpm
        image create ${pixmap} file_s_image -file ${bitd}/file_s.xpm
        image create ${pixmap} file_b_image -file ${bitd}/file_b.xpm
        image create ${pixmap} file_h_image -file ${bitd}/file_h.xpm
        image create ${pixmap} file_d_image -file ${bitd}/file_d.xpm
        image create ${pixmap} file_+_image -file ${bitd}/file_+.xpm
        image create ${pixmap} file_-_image -file ${bitd}/file_-.xpm
        image create ${pixmap} parent_image -file ${bitd}/parent.${xpm}
        image create ${pixmap} filter_image -file ${bitd}/filter.${xpm}

        image create ${pixmap} next2_image -file ${bitd}/next2.${xpm}
        image create ${pixmap} prev_image -file ${bitd}/prev.${xpm}

        # Company logo
        #image create ${pixmap} company_image -file ${bitd}/shadowman.${xpm}
        image create photo company_image -file ${bitd}/sn_logo.gif -palette 0/0/8

        #images for the new treetable
        image create ${pixmap} plus_image -file ${bitd}/plus.${xpm}
        image create ${pixmap} minus_image -file ${bitd}/minus.${xpm}
        image create ${pixmap} unknown_image -file ${bitd}/unknown.${xpm}

        #images for textual class hierarchy display
        image create ${pixmap} play_image -file ${bitd}/play.${xpm}
        image create ${pixmap} rplay_image -file ${bitd}/rplay.${xpm}

        #create image for the Tk/file dialog box
        global tkPriv
        set tkPriv(updirImage) [image create pixmap tkPriv(updirImage)\
          -file [file join ${bitd} updir.xpm]]
        set tkPriv(folderImage) dir_image
        set tkPriv(fileImage) file_image
    } errmsg]} {
        set e_inf ${errorInfo}
        set e_code ${errorCode}

        sn_log "${errorCode},${errorInfo}"

        return -code error -errorinfo ${e_inf} -errorcode ${e_code} ${errmsg}
    }
}

proc sn_get_symbol_and_scope {symm} {
    global sn_all_scopes

    regsub -all "\[ \t\]+" ${symm} { } sym

    # Check .e.g operator() !
    if {[string match {*()*} ${sym}]} {
        regsub -all {\(\)} ${sym} {%%} sym
    }

    if {![regsub -all {\(|\)} ${sym} { } sym]} {
        return ""
    }
    regsub -all {%%} ${sym} {()} sym

    if {[llength ${sym}] == 3} {
        set name "[lindex ${sym} 2] [lindex ${sym} 0]"
        set scope [lindex ${sym} 1]
    } else {
        set name [lindex ${sym} 0]
        set scope [lindex ${sym} 1]
    }

    if {[lsearch -exact ${sn_all_scopes} ${scope}] == -1} {
        return ""
    }
    return [list ${name} ${scope}]
}

proc sn_external_editor {file} {
    global Parser_Info
    # Use always file extensions
    set type [sn_get_file_type ${file}]
    return $Parser_Info(${type},EDIT)
}

#return the highlight browser command
proc sn_highlight_browser {file {cmd "b"}} {
    global Parser_Info Avail_Parsers
    set type [paf_db_f get -key -col {0} ${file}]
    if {${type} == ""} {
        set type [sn_get_file_type ${file}]
    }
    set exe_cmd ""
    set cmd_swi ""
    if {${cmd} == "h"} {
        set exe_cmd $Parser_Info(${type},HIGH)
        set cmd_swi $Parser_Info(${type},HIGH_SWITCH)
    } else {
        set exe_cmd $Parser_Info(${type},BROW)
        set cmd_swi $Parser_Info(${type},BROW_SWITCH)
    }
    #no highlighter
    if {${exe_cmd} == ""} {
        return ""
    }
    #make sure that the hightlighter is given as a list item,
    #to avoid conflicts when the command contains blanks.
    set browcmd [list ${exe_cmd}]
    if {${cmd_swi} != ""} {
        eval lappend browcmd ${cmd_swi}
    }
    return ${browcmd}
}

# This function returns the file type given a file name.
# A file type is a descriptive string like "java", "asm",
# "m4" and so on. If a specific match can not be found
# then the catch all type "others" is returned.
proc sn_get_file_type {file} {
    global sn_options
    global Avail_Parsers Parser_Info

    set tail [file tail $file]

    #test "others" extension matching as last possibility
    #the user can add "*" to others to include all
    #file that don't match another languages.
    foreach p ${Avail_Parsers} {
        if {$Parser_Info(${p},TYPE) != "others"} {
            foreach pattern $Parser_Info(${p},SUF) {
                if {[string equal $pattern $tail] ||
                        [string match $pattern $tail]} {
                    sn_log "$tail file type is $p"
                    return ${p}
                }
            }
        }
    }
    return "others"
}

proc sn_check_browsers_path {} {
    global sn_path tcl_platform
    global Avail_Parsers Parser_Info

    foreach type ${Avail_Parsers} {
        # Parser command.
        set cmd $Parser_Info(${type},BROW)
	
        if {${cmd} != ""} {
            if {$tcl_platform(platform) == "windows" && ! [file exists ${cmd}]} {
                append cmd ".exe"
            }
            set Parser_Info(${type},BROW) ${cmd}

        }

        # Highlighting command.
        set cmd $Parser_Info(${type},HIGH)
        if {${cmd} != ""} {
            if {$tcl_platform(platform) == "windows" && ! [file exists ${cmd}]} {
                append cmd ".exe"
            }
            set Parser_Info(${type},HIGH) ${cmd}

        }
    }

    return 1
}

#add new parsers into SN
proc sn_add_parser {type args} {
    global sn_options tcl_platform
    global Avail_Parsers Parser_Info

    # Do not add a parser twice. 

    if {[info exists Avail_Parsers] && [lsearch -exact ${Avail_Parsers}\
      ${type}] != -1} {
        return
    }

    set suf "*"
    set brow_cmd ""
    set high_cmd ""
    set editor ""
    set brow_switch ""
    set high_switch ""
    set case 1
    #parser understand macros??
    set macros ""

    set len [llength ${args}]
    for {set i 0} {${i} < ${len}} {incr i} {
        set arg [lindex ${args} ${i}]
        incr i
        set val [lindex ${args} ${i}]
        switch -- ${arg} {
            "-suffix" {
                    set suf ${val}
                }
            "-brow_cmd" {
                    set brow_cmd ${val}
                }
            "-high_cmd" {
                    set high_cmd ${val}
                }
            "-editor" {
                    set editor ${val}
                }
            "-brow_switch" {
                    set brow_switch ${val}
                }
            "-high_switch" {
                    set high_switch ${val}
                }
            "-case" {
                    set case ${val}
                }
            "-macros" {
                    #parser understand macros
                    set macros ${val}
                }
            default {
                    puts stderr "sn_add_parser: unknow argument <${arg}>"
                }
        }
    }

    if {$tcl_platform(platform) == "windows"} {
        if {${brow_cmd} != "" && [string first "." ${brow_cmd}] == -1} {
            append brow_cmd ".exe"
        }
        if {${high_cmd} != "" && [string first "." ${high_cmd}] == -1} {
            append high_cmd ".exe"
        }
    }

    if {![info exists Avail_Parsers]} {
        set Avail_Parsers ""
    }

    #add type to the availiable list
    if {[lsearch -exact ${Avail_Parsers} ${type}] == -1} {
        lappend Avail_Parsers ${type}
    }

    #add related data to the parser array
    set Parser_Info(${type},TYPE) ${type}
    set Parser_Info(${type},SUF) ${suf}
    set Parser_Info(${type},BROW) ${brow_cmd}
    set Parser_Info(${type},BROW_SWITCH) ${brow_switch}
    set Parser_Info(${type},HIGH) ${high_cmd}
    set Parser_Info(${type},HIGH_SWITCH) ${high_switch}
    set Parser_Info(${type},CASE) ${case}
    set Parser_Info(${type},MACRO) ${macros}
    set Parser_Info(${type},EDIT) ""

    sn_log "Added parser for types \{ ${suf} \} parser at ${brow_cmd}"
}

#executed when user specifies parser extensions on the
#command line or in his profile
#format "parser-ext=<language>,<Extension list>"
#as example: parser-ext=tcl,"*.tk *.tcl"
proc sn_parser_extension_trigger {var value} {
    global sn_user_specified
    global Avail_Parsers Parser_Info

    set wrong "wrong format for parser extension, usage:\
      parser-ext=TYPE,<extension list>
	as example use
		-D \"parser-ext=c++,*.c *.cpp *.h *.hpp\"
	to change the default extension list for the c++ parser."

    set i [string first "," ${value}]
    if {${i} <= 0} {
        sn_log -stderr ${wrong}
        return 0
    }
    set lng [string range ${value} 0 [expr {${i} - 1}]]

    #is the type correct?
    if {[lsearch -exact ${Avail_Parsers} ${lng}] == -1} {
        sn_log -stderr "unknown parser type <${lng}>, availiable types are\
          <${Avail_Parsers}>"
    }

    #get the extension list
    set ext [string range ${value} [expr {${i} + 1}] end]
    if {${ext} == ""} {
        sn_log -stderr ${wrong}
        return 0
    }

    #correct.
    set Parser_Info(${lng},SUF) ${ext}

    #mark the option as overriden using the command line
    #don't load it from stored prefernces settings
    set sn_user_specified(sys,parser,${lng},SUF) yes

    sn_log "Parser extension \"${lng}=${ext}\" accepted."

    return 0
}

#this function is called to add macrofiles into the c/c++ parser
proc sn_macrofiles_trigger {var value} {
    global sn_options
    global sn_user_specified

    if {[lsearch -exact $sn_options(macrofiles) ${value}] == -1} {
        if {[file isfile ${value}]} {
            lappend sn_options(macrofiles) ${value}
            sn_log "macro file \"${value}\" added to the macro list."
        } else {
            sn_log -stderr "macro file \"${value}\" doesn't exist."
            return 0
        }
    } else {
        sn_log -stderr "file \"${value}\" is already added as macro file."
        return 0
    }
    set sn_user_specified(macrofiles) yes
    return 1
}
proc sn_rep_macrofiles_trigger {var value} {
    global sn_options
    global sn_user_specified

    if {[file isfile ${value}]} {
        set sn_options(macrofiles) ${value}
        sn_log "replaced macro file list with \"${value}\"."
    } else {
        sn_log -stderr "macro file \"${value}\" doesn't exist."
        return 0
    }
    set sn_user_specified(macrofiles) yes
    return 1
}

proc sn_tmpFileName {{prefix "tmp_"}} {
    global tcl_platform env

    #see if temp files have a temp directory
    #they should be in and put them there - [irox:12.12.97]
    if {[info exists env(TEMP)]} {
        set temp_dir $env(TEMP)
    } else {
        if {[info exists env(TMP)]} {
            set temp_dir $env(TMP)
        } else {
            set temp_dir ""
        }
    }
    #be sure that the temporary directory exists
    if {${temp_dir} == "" || ! [file isdirectory ${temp_dir}]} {
        if {$tcl_platform(platform) == "windows"} {
            if {[file isdirectory "C:\\TEMP"]} {
                set dir "C:\\TEMP"
            }\
            elseif {[file isdirectory "C:\\TMP"]} {
                set dir "C:\\TMP"
            } else {
                set dir "C:\\"
            }
        } else {
            set dir "/tmp"
        }
        if {[file isdirectory ${dir}]} {
            set temp_dir ${dir}
        } else {
            set temp_dir ""
        }
    }
    sn_log "temp_dir = ${temp_dir}"
    return [tempnam ${temp_dir} ${prefix}]
}

# FIXME: this method does not seem to get called any where (should be find makepane?)
proc cancel_make {w cancel_str fd} {
    global PafMakeCancelled

    set PafMakeCancelled 1

    ${w}.cancel config -command "itcl::delete object ${w}" -text ${cancel_str}\
      -width [expr [string length ${cancel_str}] + 1]
    ${w}.mk config -cursor {}

    ${w}.mk insert end "\n[get_indep String MakeKilled]"
    ${w}.mk see "end"
    catch {exec kill [pid ${fd}]}
}

# FIXME: this method does not seem to get called anywhere (should be in makepane?)
proc event_handle_make_input {makefd w} {
    global sn_options
    global PafMakeCancelled

    set end [gets ${makefd} line]

    if {${PafMakeCancelled} || ${end} < 0} {
        catch {close ${makefd}}
        if {![winfo exists ${w}]} {
            return
        }
        set cancel_str [get_indep String Cancel]
        ${w}.cancel config -command " ${w} delete " -text ${cancel_str}\
          -width [expr [string length ${cancel_str}] + 1]
        ${w}.mk config -cursor {}

        if {!${PafMakeCancelled}} {
            ${w}.mk config -state normal
            ${w}.mk insert end "\n[get_indep String MakeEnd]"
            ${w}.mk config -state disabled
            ${w}.mk see end

            if {[catch {set ra $sn_options(def,make-raise)}]} {
                set ra 1
            }
            if {${ra}} {
                ${w} raise
            }

            bell -displayof ${w}
            bind ${w} <Escape> "${w}.cancel invoke"
        }
    } else {
        ${w}.mk config -state normal
        if {[${w}.mk index insert] != 1.0} {
            set line "\n${line}"
        }
        ${w}.mk insert end ${line}
        ${w}.mk config -state disabled
        ${w}.mk see end
        update idletasks
    }
}

#this procedure gets a file name and look if this file name
#is availiable in the project list by cutting the prefix or
#looking for the filename only or building the realpath
#of the file name
proc sn_convert_FileName {orig_name {any ""}} {
    global sn_options
    global tcl_platform
    global sn_root

    #sn_log "sncf: called with orig_name: ${orig_name} and any: ${any}"
    
    if {[catch {set name [join [realpath -pwd $sn_options(sys,project-dir)\
      ${orig_name}]]}]} {
        set name ${orig_name}
    }\
    elseif {![file exists ${name}]} {
        set name ${orig_name}
    }

    #Project directory is a prefix of the file looked for. Truncate it!
    if {[sn_filecmd begins $sn_options(sys,project-dir)${sn_root} ${name}]} {
        set nn [string range ${name} [expr [string length\
          $sn_options(sys,project-dir)] + 1] end]

        #WINDOWS: We have to look in the database to see if the file exists
        #         Because of upper/lower case, so "Foo.c" is equal "foo.c"
        if {$tcl_platform(platform) == "windows" && [info commands paf_db_f]\
          != ""} {
            set f [paf_db_f get -col 0 -first ${nn}]
            if {${f} != ""} {
                return ${f}
            }
        }
        return ${nn}
    }

    #it can be the first file in the project
    if {[info commands paf_db_f] != ""} {
        set f [paf_db_f get -col 0 -first ${name}]
        if {${f} != ""} {
	    return ${f}
        }
    }

    set test [sn_truncate_path $sn_options(sys,project-dir) ${name}]
    if {[string compare ${name} ${test}] != 0} {
        #filename is included in the project directory
        set name ${test}
    } else {
        #look for the file with full path
        if {[info commands paf_db_f] != ""} {
            
	    # be more careful when globbing files on the first try
	    # this partly solves problems with the makepane and wrongly
	    # opened files on cc errors
	    set file [paf_db_f seq -col 0 -first -glob [file join * ${name}] ]
            if {${file} != ""} {
		return ${file}
	    }
	    
	    # now be quite fuzzy about filesearch
	    set file [paf_db_f seq -col 0 -first -glob "*${name}"]
            if {${file} != ""} {
		return ${file}
            }
        }

        # look for any file that match this file, but can be
	# with different pathes
        if {${any} != "" && ! [file exists ${name}]} {
            set f [file join * [file tail ${name}]]
            if {[info commands paf_db_f] != ""} {
                set file [paf_db_f seq -col 0 -first -glob ${f}]
                if {${file} != ""} {
		    return ${file}
                }
            }
        }
    }

    #something like "./foo.c" ==> "foo.c"
    if {[string index ${name} 0] == "." && [file dirname ${name}] == "."} {
        set name [string range ${name} 2 end]
    }

    return ${name}
}

# This proc starts a UNIX command and collects the contents of stdout.
# The only feature is that the x i/o events will be processed.
# If busy is set, then the input events are destroyed.
proc sn_exec_x_events {cmd {busy 1}} {
    global sn_options
    global errorInfo errorCode SN_x_exec_errMsg env sn_debug
    global tcl_platform

    if {$tcl_platform(platform) != "windows"} {
        set home $env(HOME)
        if {[string compare ${home} "/"] != 0} {
            append home "/"
        }
        set home [file nativename ${home}]
        regsub -all "~/" ${cmd} ${home} cmd
    }
    set cmd [string trim ${cmd}]
    sn_log "exec_x_events: ${cmd}"

    if {[catch {set execfd [open "| ${cmd}"]} errmsg]} {
        set e_inf ${errorInfo}
        set e_code ${errorCode}
        return -code error -errorinfo ${e_inf} -errorcode ${e_code} ${errmsg}
    }

    fconfigure ${execfd} -encoding $sn_options(def,system-encoding) -blocking 0
    set id [lindex [pid ${execfd}] 0]
    global Exec_Running_${id}
    global Exec_Results_${id}

    upvar #0 Exec_Results_${id} results
    upvar #0 Exec_Running_${id} running
    set results ""
    set running 1

    set errorInfo ""
    fileevent ${execfd} readable "event_exec_x_events_proc  ${execfd}\
      Exec_Running_${id}  Exec_Results_${id}"

    vwait Exec_Running_${id}

    set res ${results}

    set ran ${running}
    catch {unset Exec_Results_${id} Exec_Running_${id}}

    if {[string compare ${ran} "NONE"] != 0 && [string compare ${ran} ""] !=\
      0} {
        if {${sn_debug} > 1} {
            sn_error_dialog ${SN_x_exec_errMsg}
        }
        return -code error -errorcode ${ran} ${SN_x_exec_errMsg}
    }

    return ${res}
}

proc event_exec_x_events_proc {xeventfd run res} {
    global errorCode SN_x_exec_errMsg
    upvar #0 ${run} running
    upvar #0 ${res} results

    if {[eof ${xeventfd}]} {
        if {[catch {close ${xeventfd}} SN_x_exec_errMsg]} {
            set running ${errorCode}
        } else {
            set running NONE
        }
    } else {
        lappend results [gets ${xeventfd}]
    }
    update idletasks
}

# This function returns the number of lines can be shown on a page
# of a text widget.
proc text_page_num_lines w {
    set opix [winfo reqheight ${w}]
    set npix [winfo height ${w}]

    if {${npix} > ${opix}} {
        set epix ${npix}
    } else {
        set epix ${opix}
    }
    incr epix -3
    set top [${w} index "@0,0"]
    set bot [${w} index "@0,${epix}"]
    if {${top} == ${bot}} {
        return [${w} cget -height]
    }
    return [expr int(${bot}) - int(${top})]
}

proc sn_search_file {file} {
    global sn_options
    global sn_path HOME

    foreach d [list $sn_options(sys,project-dir) $sn_options(profile_dir)\
      $sn_path(etcdir) $sn_options(both,db-directory)] {
        if {[file exists [file join ${d} ${file}]]} {
            sn_log "search file found: [file join ${d} ${file}]"
            return [file join ${d} ${file}]
        }
    }
    return ""
}

proc sn_print_ascii {file} {
    global sn_options

    # The contents of the file are plain ASCII, not
    # PostScript.  Turn them into a PostScript program, and then print.

    set outfname ${file}.ps
    set fontsize 11
    set font Courier
    set inch 72
    set pageheight [expr {11 * ${inch}}]
    set pagewidth [expr {8.5 * ${inch}}]
    set margin [expr {0.5 * ${inch}}]

    set outF [open ${outfname} w]
    fconfigure ${outF} -encoding $sn_options(def,system-encoding) -blocking 0

    # This piece of code writes a little header to the ASCII file
    # which will print the following ASCII text onto the printer.
    puts ${outF} "%!PS-Adobe-1.0"
    puts ${outF} "%%Creator: SN ASCII-to-PS converter"
    puts ${outF} "%%DocumentFonts: Courier"
    puts ${outF} "%%Pages: (atend)"
    puts ${outF} "/Courier findfont ${fontsize} scalefont setfont"
    puts ${outF} "/M{moveto}def"
    puts ${outF} "/S{show}def"
    set pages 1
    set y [expr {${pageheight}-${margin}-${fontsize}}]

    set G [open ${file} r]
    fconfigure ${fd} -encoding $sn_options(def,encoding) -blocking 0
    while {[gets ${G} str] != -1} {
        if {${y} < ${margin}} {
            puts ${outF} showpage
            incr pages
            set y [expr {${pageheight}-${margin}-${fontsize}}]
        }
        regsub -all {[()\\]} ${str} {\\&} str
        puts ${outF} "${margin} ${y} M (${str}) S"
        set y [expr {${y}-(${fontsize}+1)}]
    }
    close ${G}
    puts ${outF} showpage
    puts ${outF} "%%Pages: ${pages}"
    close ${outF}

    return ${outfname}
}

#print a given file
proc sn_print_file {cmd file {postscript "-nopostscript"}} {
    global tcl_platform

    sn_log "print file:${file}, cmd:${cmd}"

    if {$tcl_platform(platform) != "windows"} {
        if {${cmd} != ""} {
            #it's a pipe
            if {[string first "|" ${cmd}] > 0} {
                if {[catch {eval exec -- cat [list ${file}] | ${cmd}} err]} {
                    sn_error_dialog ${err}
                    return
                }
            } else {
                #pass the file to the command as an argument
                if {[catch {eval exec -- ${cmd} [list ${file}]} err]} {
                    sn_error_dialog ${err}
                    return
                }
            }
        }
    } else {
        if {${postscript} == "-nopostscript"} {
            #print's ASCII-files only
            sn_winprint_dialog ${file}
        } else {
            sn_winprint_dialog ${file} null postscript
        }
    }
}

#
# This function is used by sn_prompt_for_files.
proc sn_prompt_fileselector {ew {type ""}} {
    set file [string trim [${ew} get]]
    set dir [file dirname ${file}]

    if {${type} == "d"} {
        set file [Editor&::DirDialog [winfo toplevel ${ew}]\
          -initialdir ${dir}]
    } else {
        set file [Editor&::FileDialog [winfo toplevel ${ew}]\
          -initialdir ${dir}]
    }

    if {${file} != ""} {
        ${ew} delete 0 end
        ${ew} insert end ${file}
    }
}

# Parameters contains list with sublist.
# The first element of a sublist is a label the second
# is a suggested value.
# This function returns the contents of the entry boxes in a list.
proc sn_prompt_for_files {title pars} {
    set w [sourcenav::Window [sourcenav::Window::next_window_name]]

    ${w} configure -title ${title}

    sn_motif_buttons ${w} bottom 0 [get_indep String ok] [get_indep String\
      cancel]

    ${w}.button_0 configure -command " set ${w}-select_status ok "

    ${w}.button_1 configure -command " set ${w}-select_status cancel "

    pack [frame ${w}.browsers] -side left

    set width 7
    foreach p ${pars} {
        set l [lindex ${p} 0]
        if {[string length ${l}] > ${width}} {
            set width [expr [string length ${l}] * 2]
        }
    }
    set cou 0
    foreach p ${pars} {
        set l [lindex ${p} 0]
        set e [lindex ${p} 1]
        set b [lindex ${p} 2]

        set fr ${w}.entrys-${cou}
        pack [frame ${fr}] -side left -side top -fill x -expand y
        pack [label ${fr}.l -text ${l} -width ${width}] -anchor w -side left

        set ew [entry ${fr}.e -width 60]
        pack ${ew} -fill x -expand y -padx 10 -side left
        ${ew} insert end ${e}

        bind ${ew} <Return> "${w}.button_0 invoke; break"
        bind ${ew} <Escape> "${w}.button_1 invoke; break"

        if {${b} != "n"} {
            pack [button ${fr}.b -text [get_indep String Choose] -command\
              " sn_prompt_fileselector ${ew} ${b} "] -side left
        }
        incr cou
    }

    ${w} move_to_mouse
    ${w} take_focus ${w}.entrys-0.e

    catch {${w} resizable yes no}
    catch {${w} grab set}

    update idletasks
    set geom [split [lindex [split [${w} geometry] "+"] 0] "x"]
    ${w} minsize [lindex ${geom} 0] [lindex ${geom} 1]

    tkwait variable ${w}-select_status

    upvar #0 ${w}-select_status sel

    if {${sel} == "cancel"} {
        itcl::delete object ${w}
        return ""
    }

    set res ""
    set cou 0
    foreach p ${pars} {
        set ew ${w}.entrys-${cou}.e
        lappend res [string trim [${ew} get]]
        incr cou
    }
    itcl::delete object ${w}

    return ${res}
}

proc sn_get_class_inheritance_chain_intern {cls_name} {
    global sn_options sn_sep

    upvar #0 sn_get_class_inheritance_chain cls_arr

    set cls ${cls_name}

    if {[info commands paf_db_in] == ""} {
        return ${cls}
    }

    foreach sc [paf_db_in seq -uniq -col [list 1] "${cls_name}${sn_sep}"] {
        set c [lindex ${sc} 0]
        if {![info exists cls_arr(${c})]} {
            set cls_arr(${c}) 1
            eval lappend cls [sn_get_class_inheritance_chain_intern ${c}]
        }
    }
    return ${cls}
}

proc sn_get_class_inheritance_chain {cls_name} {
    global sn_get_class_inheritance_chain

    catch {unset sn_get_class_inheritance_chain}
    set classes [sn_get_class_inheritance_chain_intern ${cls_name}]
    catch {unset sn_get_class_inheritance_chain}

    return ${classes}
}

#functions to choose a file/directory
proc sn_choose_cb {entry variable txt} {
    set txt [file nativename ${txt}]
    if {${entry} != ""} {
        ${entry} delete 0 end
        ${entry} insert 0 ${txt}
    }
    if {${variable} != ""} {
        upvar #0 ${variable} var
        set var ${txt}
    }
    return 0
}

proc sn_choose_dir {entry {variable ""}} {
    Editor&::DirDialog ${entry} -title [get_indep String Open] -script\
      [list sn_choose_cb ${entry} ${variable}] -prefix sn_choose_dir
    return 0
}

proc sn_choose_file {entry {variable ""} {defext ""}} {
    Editor&::FileDialog ${entry} -title [get_indep String Open]\
      -script [list sn_choose_cb ${entry} ${variable}] -prefix sn_choose_file\
      -defaultextension ${defext}
    return 0
}

