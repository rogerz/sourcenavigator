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
# retriever.tcl - The retriver with tab stop support.
# Copyright (C) 1998 Cygnus Solutions.

itcl_class Retriever& {
    constructor {config} {
        global sn_options

        #read matched contents from the db.
        if {${contents} == ""} {
            set contents [read_matched_from_db "" ${what} ${mtype} ${pattern}\
              ${type} ${param} ${file} ${from} ${to} ${inherit} ${offset}\
              ${merge}]
        } else {
            #contents are specified as an option (-contents)
        }

        if {${contents} == ""} {
            if {${bell}} {
                bell
            }

            set return_status 0
            change_variable
            return 0
        }

        #if it found only one item
        if {${edit_single} && [llength ${contents}] == 1} {
            goto_symbol "" ${contents} ${client_data} ${client_func}

            #change variable and exit, doesn't wait any way
            set return_status 1
            change_variable
            return ${return_status}
        } else {
            #by more than one element, view list to select one item
            display_retrieves
        }

        #if variable not empty wait to close the procedure.
        if {${variable} != ""} {
            global ${variable}
            update idletasks
            tkwait variable ${variable}
        } else {
            set return_status 1
        }

        return ${return_status}

    }

    proc goto_symbol {w cnt cldata {clproc "edit_symbol"}} {
        if {${clproc} != ""} {
            if {[catch {eval ${clproc} [list ${w}] [list [lindex ${cnt} 0]]\
              [list ${cldata}]}]} {
                bell
            }
        }
    }

    destructor {
        if {${window} != ""} {
	    ${this} window_deleted
	}

        foreach v [::info globals "${this}-*"] {
            catch {uplevel #0 unset ${v}}
        }
    }

    method change_variable {} {
        if {${variable} != ""} {
            upvar #0 ${variable} var
            set var "changed"
        }
    }

    public inherit 0 {
    }

    proc retr_cross_ref {w} {
        if {[catch {set sel [lindex [${w} curselection] 0]}] || ${sel} == ""} {
            bell
            return
        }
        set line [split [${w} get ${sel}] \t]
        set data [split [${w} itemcget ${sel} -data] \t]
        set key [lindex ${line} 0]
        set cls [lindex ${line} 1]
        set type [lindex ${line} 2]
        set param [lindex ${line} 3]
        set file [lindex ${data} 0]
        set from [lindex ${data} 1]
        set to [lindex ${data} 1]

        set type [lindex [split ${key} "("] end]
        set type [lindex [split ${type} ")"] 0]
        switch ${type} {
            "md" {
                    set type "mi"
                }
            "fd" {
                    set type "fu"
                }
        }

        set i [string last "(" ${key}]
        set key [string range ${key} 0 [expr ${i} - 1]]

        if {${cls} != ""} {
            set key "${cls}\:\:${key}"
        }

        sn_xref both ${key} ${type} ${file} ${from} ${to}
    }

    method toggle_hold {w} {
        upvar #0 ${this}-hold hold
        if {${hold}} {
            ${w} config -relief raised -image hold_off_image
            set hold 0
            set hold_window 0
        } else {
            ${w} config -relief sunken -image hold_on_image
            set hold 1
            set hold_window 1
        }
    }

    public hold_window 0 {
        if {[winfo exists ${window}.exp.hold]} {
            if {${hold_window}} {
                set hold_window 0
            } else {
                set hold_window 1
            }
            toggle_hold ${window}.exp.hold
        }
    }


# FIXME: this method is only called in the constructor, and it seems to create a new
# topleve. Beside the fact that it is completely broken, I can seem to figure out
# what sequence of steps would actually bring up this window! (is it another "secret window" ?)
    #view list to choose one item in the list
    method display_retrieves {} {
        global sn_options
        global tkeWinNumber

        #now we have found more than one match, if warning
        #is enabled, do it
        if {${warning} && $sn_options(donot_display) == 0} {
            warning_many_matches
        }

        #create new window
        incr tkeWinNumber
        set window ".sn-${window_prefix}-${tkeWinNumber}"

        #build title
        build_title

        sn_create_window ${window}
        ${window} withdraw
        ${window} on_close "${this} window_deleted"

        #hold/close window after choosing an item
        button ${window}.exp.hold -takefocus 0 -text [get_indep String\
          HoldWindow] -image hold_off_image -command "  "
        ::bind ${window}.exp.hold <Any-ButtonPress> "if {%b == 1}\
          {${this} toggle_hold ${window}.exp.hold}"
        balloon_bind_info ${window}.exp.hold [get_indep String HoldWindowINFO]
        upvar #0 ${this}-hold hold
        set hold ${hold_window}
        frame ${window}.exp.space -width 3
        pack ${window}.exp.hold -side left
        pack ${window}.exp.space -side left

        #print should be possible too
        sn_window_append_print ${window} "${window}.list print_dialog_box"

        #shows some icons
        sn_window_add_icons ${window} [list ctree xref class]
        ${window}.exp.tree configure -state normal \
		-command "Retriever&::goto_class ${window} ctree"
        ${window}.exp.class config -state normal \
		-command "Retriever&::goto_class ${window} class"
        ${window}.exp.cross configure -state normal \
		-command "Retriever&::retr_cross_ref ${window}.list"

        set height [llength ${contents}]
        if {${height} == "" || ${height} <= 0} {
            set height 1
        }
        if {${height} > 20} {
            set height 20
        }

        set cols 4

        #now we use the new widget "treetable"
        #darkgray = "#aaaaaa"
        Tree ${window}.list -fillselection 1 -selectmode browse\
          -exportselection 0 -font $sn_options(def,default-font) -bestfit 1\
          -truncate 1 -tabsize ${cols} -tabs {80 80 80 120 200} -labels [list\
          "[get_indep String Name]" "[get_indep String Class]"\
          "[get_indep String Type]" "[get_indep String Parameters]"\
          "[get_indep String File]"] -indentwidth 20 -filterextension "\t*\t*"\
          -width ${retrieve_width} -height ${height}\
          -fillcommand "display_contents" -propagate 1
        #	-hiddenimage type_cl+_image

# FIXME: all this binding code needs to be cleaned up, can't we provide some
# generic binding routines instead of cutting and pasting the code everywhere!
        ${window}.list treebind <ButtonRelease-1> "Retriever&::expand_classes\
          ${window} %x %y"
        ${window}.list treebind <space> [::bind ${window}.list <ButtonRelease-1>]
        ${window}.list treebind <Return> "Retriever&::handle_return ${this} %W;\
          break"
        ${window}.list treebind <space> "[${window}.list treebind <Return>]; break"
        ${window}.list treebind <Double-1> "Retriever&::handle_doubleclick\
          ${this} %W; break"

        pack ${window}.list -side top -fill both -expand y

        #if variable is set, we use a dialog style window
        if {${variable} != ""} {
            bind ${window}.list.tree <Destroy> "${this} change_variable"
            catch {${window} grab set}
        }

        #$window.list config  -width $width  -height $height

        #set found symbols to the tree class
        ${window}.list setcontents ${contents}

        #display the found contents width -data option
        display_contents_x ${window}.list ${contents} -1 "don't resize"

        #contents is then not more needed
        catch {unset contents}

        ${window} configure -title [list ${title}]
        ${window} configure -iconname "${icon_prefix} ${icon}"

        #after idle "focus [$window.list tree]; $window move_to_mouse"
        after idle "focus [${window}.list tree]; ${window} move_to_mouse"

        #call user client function
        catch {sn_rc_retrieve ${window} ${window}.menu ${window}.list}
    }

    proc goto_class {w tree_or_class} {
        set cls [${w}.list.tree curselection]
        if {${cls} != ""} {
            set cols [split [${w}.list.tree get [lindex ${cls} 0]] \t]
            set cls [lindex ${cols} 0]
            get_key_and_scope ${cls} cls scope
            if {${scope} != "cl" && ${scope} != "un"} {
                set cls [lindex ${cols} 1]
            }
        }
        if {${tree_or_class} == "class"} {
            sn_classbrowser ${cls}
        } else {
            sn_classtree ${cls}
        }
    }

    #Convert a tree entry into the following format:
    #symbol class type param file from to
    proc convert_to_line {w} {
        set sel [lindex [${w} curselection] 0]
        if {${sel} == ""} {
            return ""
        }
        set txt [split [${w} get ${sel}] \t]
        set data [split [${w} itemcget ${sel} -data] \t]
        set file [lindex ${data} 0]
        set from [lindex ${data} 1]
        set to [lindex ${data} 2]
        return "[lindex ${txt} 0]\t[lindex ${txt} 1]\t[lindex ${txt}\
          2]\t[lindex ${txt} 3]\t${file}\t${from}\t${to}"
    }

    proc expand_classes {cls x y {dspproc "display_contents_x"}} {
        global sn_options sn_sep
        set tree ${cls}.list.tree
        #no items
        if {[${tree} size] <= 0} {
            return ""
        }
        #verify if the icon of a sub tree is clicked
        set ret [${tree} identify ${x} ${y}]
        set idx [${tree} nearest ${y}]

        if {${ret} == "view" || ${ret} == "hide"} {
            ${tree} toggle @${x},${y}
            set img [${tree} itemcget ${idx} -image]
            if {[string first "file" ${img}] == 0} {
                if {${ret} == "view"} {
                    set img file_+_image
                } else {
                    set img file_-_image
                }
            } else {
                if {${ret} == "view"} {
                    set img type_cl+_image
                } else {
                    set img type_cl-_image
                }
            }
            ${tree} itemconfig ${idx} -image ${img}
            return ""
        }
        if {${ret} == "text"} {
            return "text"
        }
        set txt [lindex [${tree} get ${idx}] 0]
        get_key_and_scope ${txt} key scope

        #expand classes and files
        if {${scope} == "cl"} {
            set dat [split [${tree} itemcget ${idx} -data] \t]
            set file [lindex ${dat} 0]
            set from [string trimleft [lindex [split [lindex ${dat} 1] {.}] 0]\
              0]
            set to [string trimleft [lindex [split [lindex ${dat} 2] {.}] 0] 0]

            foreach w [list md mi iv fr] {
                if {[::info commands paf_db_${w}] == ""} {
                    continue
                }
                if {${w} == "md" || ${w} == "mi"} {
                    set Op "("
                    set Cl ")\\t"
                } else {
                    set Op ""
                    set Cl "\\t"
                }
                set col [list "1 \(${w}\)\\t" "0 \\t" "6 \\t${Op}"\
                  "7 \"${Cl}\"" "3 \\t" "2 \\t" "4 \"-${from}:${to}\""]
                set res [paf_db_${w} seq -end ${file} -col ${col}\
                  "${key}${sn_sep}"]
                if {${res} != ""} {
                    lappend cnt ${res}
                }
            }
            if {![info exists cnt]} {
                set cnt ""
            }
            ${dspproc} ${cls}.list [::lsort -command sn_compare\
              [::join ${cnt}]] ${idx}
            set num [${tree} itemcget ${idx} -children]
            if {${num} > 0} {
                ${tree} itemconfig ${idx} -image type_cl-_image
            }
        }\
        elseif {${scope} == ""} {
            set pars [split [${tree} get ${idx}] \t]
            set f [lindex ${pars} 0]
            set d [lindex ${pars} 1]
            if {${d} != ""} {
                set f [file join ${d} ${f}]
            }
            ${dspproc} ${cls}.list [read_matched_from_db "" f -exact "" ""\
              "" ${f}] ${idx}
            set num [${tree} itemcget ${idx} -children]
            if {${num} > 0} {
                ${tree} itemconfig ${idx} -image file_-_image
            } else {
                ${tree} itemconfig ${idx} -image file_image
            }
        }
        return ""
    }

    proc handle_doubleclick {cls w} {
        catch {${cls} select_item ${w}}
    }

    proc handle_return {cls w} {
        ${w} selection clear 0 end
        ${w} selection set [${w} index active]
        update idletasks
        catch {${cls} select_item ${w}}
    }

    method select_item {w} {
        #an item is choosen
        set return_status 1

        #call user function
        eval ${client_func} ${w} [list ""] [list ${client_data}]

        if {${terminate}} {
            if {${call_before_terminate} != ""} {
                set ret [eval ${call_before_terminate} ${window}\
                  [list ${client_data}]]
                if {${ret} != ""} {
                    return
                }
            }
            change_variable
        }\
        elseif {! ${hold_window}} {
            #delete window after selection
            itcl::delete object ${this}
        }
    }

    method window_deleted {} {
        itcl::delete object ${window}
        set window ""
        set return_status -1
    }

    method config {config} {
    }

    #pattern to search an item
    public pattern "" {
        set pattern [pattern ${pattern}]
    }

    #converts the pattern to a correct form
    proc pattern {prn} {
        global sn_sep
        #verify if the pattern has C++ format
        set i [string first "\:\:" ${prn}]
        if {${i} != -1} {
            set prn [split ${prn} "\:\:"]
            set prn "[lindex ${prn} 0]${sn_sep}[lindex ${prn} end]"
        }
        return [string trim ${prn} " ${sn_sep}"]
    }
    method build_title {} {
        global sn_options

        if {${titlename} == ""} {
            set titlename [string trim [get_indep String Retriever] "."]
        }

        if {${title} == ""} {
            if {${pattern} != ""} {
                set t " : ${pattern}"
            }\
            elseif {${file} != ""} {
                set t " : ${file}"
            } else {
                set t ""
            }
            set title [sn_title ${titlename}${t}]
        }
        if {${icon} == ""} {
            set icon ${titlename}
        }
    }

    proc add_donot_call_checkbutton {w} {
        global sn_options
        # FIXME : this delete object if already exists stuff is ugly, need more generic solution
        if {[itcl::find object ${w}.checkbtns] == "${w}.checkbtns"} {
            itcl::delete object ${w}.checkbtns
        }
        CheckButton& ${w}.checkbtns -labels [list [get_indep String\
          DonotDisplay]] -balloons [list [get_indep String DoNotDisplayINFO]]\
          -variables [list sn_options(donot_display)] -label ""
        pack ${w}.checkbtns -in ${w}.top -side bottom -fill x -padx 10m -pady 3m
    }
    proc warning_many_matches {} {
        tk_dialog_with_widgets .retr-info [get_indep String ManyMatchesTitle]\
          [get_indep String ManyMatchesINFO] info_image 0 "Retriever& ::\
          add_donot_call_checkbutton" [get_indep String Ok]
    }

    #search item with following prototype
    public type ""
    #search item with following parameters
    public param ""
    public what "" {
        global sn_scopes
        if {${what} == ""} {
            set what all
        }
        if {${what} == "all"} {
            set what ${sn_scopes}
        }
    }
    public merge "" {
    }

    #file name in witch the item is to find
    public file ""

    #define range in witch the item is to find
    public from -1 {
        if {[string first "." ${from}] != -1} {
            set from [lindex [split ${from} "."] 0]
        }
        set from [string trimleft ${from} "0"]
    }
    public to -1 {
        if {${to} != -1 && ${from} == -1} {
            set from 0
        }
        if {[string first "." ${to}] != -1} {
            set to [lindex [split ${to} "."] 0]
        }
        set to [string trimleft ${to} "0"]
    }

    public mtype "-exact"
    public offset -1
    public edit_single 1
    public bell 1
    public client_func "edit_symbol" {
        if {${client_func} == ""} {
            set client_func edit_symbol
        }
    }
    #this is called when more than one match is found
    public warning 1
    public client_data ""
    public contents ""
    public window ""
    public titlename ""
    public title "" {
        build_title
    }
    public terminate 0
    public call_before_terminate ""
    public variable ""
    public watch 1
    public return_status 0
    public icon "" {
        build_title
    }
    public icon_prefix "" {
        if {${icon_prefix} == ""} {
            set icon_prefix ${pattern}
        }
    }

    public window_prefix "retr"

    common retrieve_width 70

    method return_status {} {
        return ${return_status}
    }
}

#edit the marked symbol in the retriever or class browser
proc edit_member {w {what "md"} {go ""}} {
    global sn_options sn_sep
    set cols [get_cols_and_offset ${w} file pos]
    if {${cols} == ""} {
        bell
        return
    }

    #It can be a file name entry
    if {${file} != "1"} {
        set off 0
        set lst [split [lindex ${cols} 0] \(]
        set method [lindex ${lst} 0]
        if {${what} == "md"} {
            set what [lindex [split [lindex ${lst} 1] \)] 0]
        }
        set name [lindex ${lst} 0]
        set class [lindex ${cols} 1]
        # If we search for method implementation we have to check the input\
          parameter list.
        if {${go} == "mi"} {
            #search for implementation
            if {[::info commands paf_db_mi] != ""} {
                set md_list [paf_db_mi seq\
                  -list "${class}${sn_sep}${method}${sn_sep}"]
            }
            #no implementation found?
            if {${md_list} != ""} {
                set md_list [lindex ${md_list} ${off}]
                set pos [lindex ${md_list} 2]
                set file [lindex ${md_list} 3]
            } else {
                set file ""
                set pos ""
            }
        }
        set name [list ${class} ${method}]

        if {${file} == ""} {
            sn_log "Method not found: ${class} ${method}(${what})"
            bell
        } else {
            sn_log "Found method: ${class} ${method}(${what})"

            #find offset to be possible to retrieve the same symbol in a\
              modified
            #file
            set off [sn_find_symbol_offset ${class} ${method} ${what} ${file}\
              ${pos}]

            set edname [list ${off} [string trim "${class} ${method} ${what}"]]
            sn_edit_file ${edname} ${file} ${pos}
            #sn_add_history $what [list $what $name $off $file]\
              [sn_make_history_title edit $what "$class $method"]
        }
    } else {
        #File name is restored in 'Name' and directory in 'Class'
        set file [lindex ${cols} 1]
        if {${file} == ""} {
            set file [lindex ${cols} 0]
        } else {
            set file [file join ${file} [lindex ${cols} 0]]
        }

        sn_edit_file f ${file}
        #sn_add_history f [list $file ""] [sn_make_history_title edit f $file]
    }
}

proc get_cols_and_offset {w filename filepos} {
    upvar ${filename} file
    upvar ${filepos} pos

    set y [lindex [${w} curselection] 0]
    if {${y} == ""} {
        return ""
    }

    #set up variables
    set data [split [${w} itemcget ${y} -data] \t]
    set file [lindex ${data} 0]
    set pos [lindex ${data} 1]

    #return columns as a tcl/list
    set pat [${w} get ${y}]
    return [split ${pat} \t]
}

#look for the offset of the symbol in the file, this is important
#to go to the correct symbol when the edited file is modified
proc sn_find_symbol_offset {class sym scope file line} {
    set cnt [read_matched_from_db "" ${scope} -exact [string trim\
      "${class} ${sym}"] "" "" ${file}]
    set off [lsearch -glob ${cnt}\
      "${sym}(${scope})\t${class}\t*\t*\t${file}\t${line}\t*"]
    if {${off} < 0} {
        set off 0
    }
    return ${off}
}

proc merge {what merge} {
    #by cross reference retriever it's not correct to view declaration
    #and implementation on the same time.
    #What we do hier, is deleting the declaration scopes from
    #the scope list.
    #merge md==mi and fd=fu
    foreach m ${merge} {
        set decl [lindex ${m} 0]
        set impl [lindex ${m} 1]

        if {[lsearch -exact ${what} ${impl}] != -1 || [lsearch -exact ${what}\
          ${decl}] != -1} {
            set idx [lsearch -exact ${what} ${decl}]
            if {${idx} != -1} {
                set what [lreplace ${what} ${idx} ${idx}]
            }
            if {[lsearch -exact ${what} ${impl}] == -1} {
                lappend what ${impl}
            }
        }
    }
    return ${what}
}

proc retr_cancel_fetching {} {
    global Retr_DbFetch_Canceled
    set Retr_DbFetch_Canceled 1
}

proc retr_update {} {
    global Retr_DbFetch_Canceled

    update
    update idletasks
    update

    if {${Retr_DbFetch_Canceled}} {
        #fetching canceled
        return 0
    }
    return 1
}


#######################################################################
#
#
# General procedure to fetch for symbol matches in the DB.
#
#
#######################################################################
#global variables to synchronize DB-fetches
global Retr_DbFetch_Active
set Retr_DbFetch_Active 0
global Retr_DbFetch_Canceled
set Retr_DbFetch_Canceled 0
proc read_matched_from_db {cls what mtype {pattern ""} {type ""} {param ""}\
  {file ""} {from -1} {to -1} {inherit 0} {offset -1} {merge ""}} {
    global sn_options sn_sep sn_scopes

    #Don't proceed if a DB-fetch is active.
    global Retr_DbFetch_Active
    if {${Retr_DbFetch_Active}} {
        bell
        sn_error_dialog [get_indep String CannotProceed] [get_indep String\
          Retriever]
        return ""
    }
    incr Retr_DbFetch_Active

    #reset cancel global variable
    global Retr_DbFetch_Canceled
    set Retr_DbFetch_Canceled 0

    #puts stdout "read_matched_from_db (cls:$cls, what:$what, mtype:$mtype,\
      pattern:$pattern, type:$type, param:$param, file:$file, from:$from,\
      to:$to, inh:$inherit, offs:$offset, merge:$merge)"

    set result ""
    set comp_key ""
    # Useful for simple queries.
    set key ""

    if {${what} == ""} {
        set what ${sn_scopes}
    }\
    elseif {${what} == "all"} {
        set what ${sn_scopes}
    }
    if {${merge} != ""} {
        set what [merge ${what} ${merge}]
    }
    if {${mtype} == "-exact"} {
        set key ${pattern}
        if {${key} != ""} {
            if {[string first ${sn_sep} ${key}] != -1} {
                set comp_key [list "${key}${sn_sep}"]
            }
            set key [list "${key}${sn_sep}"]
        }
        set simple_res_flt ""
        set file_res_flt ""
        if {${comp_key} == "" && ${pattern} != ""} {
            set comp_res_flt [list -strstr "${sn_sep}${pattern}${sn_sep}"]
        } else {
            set comp_res_flt ""
        }
    } else {
        # It can be case sensitive.
        set key ""
        if {${pattern} != "*"} {
            set g_pat [Tree::nocase_glob_pattern ${pattern}]
            set comp_res_flt [list -result "${g_pat}\(*"]

	    # Note: the "*" added on the end of the pattern
	    # will enable patterns such as "*.h" to match
	    # foo.h, without it the pattern *.h doesn't match
	    # anything! (I don't know why it works like this.)
            set file_res_flt [list -result ${g_pat}*]
            set simple_res_flt ${comp_res_flt}
        } else {
            set mtype "-exact"
            set comp_res_flt ""
            set file_res_flt ""
            set simple_res_flt ""
        }
    }

    #file is given
    if {${file} != ""} {
        set file_flt "-end [list ${file}]"
    } else {
        set file_flt ""
    }

    foreach w ${what} {
        set res ""

        #verify if the retrieving process is broken
        if {${Retr_DbFetch_Canceled}} {
            break
        }

        #format of output:
        #0. Name
        #1. Class
        #2. Type
        #3. Parameters
        #4. File
        #5. File From Line
        #6. File To Line
        switch ${w} {
            "md" -
            "mi" -
            "fr" -
            "iv" {
                    #this group has method name
                    #view type and parameters by mi and md
                    if {${w} == "md" || ${w} == "mi"} {
                        set Op "("
                        set Cl ")\\t"
                    } else {
                        set Op ""
                        set Cl "\\t"
                    }
                    set col [list "1 \(${w}\)\\t" "0 \\t" "6 \\t${Op}"\
                      "7 \"${Cl}\"" "3 \\t" "2 \\t" "4"]
                    set patlen [llength [split ${pattern} ${sn_sep}]]
                    if {${patlen} > 1 && ${mtype} == "-exact"} {
                        if {!${inherit}} {
                            #if inherit not set, accept only the members of
                            #the actual class
                            catch {eval lappend res [paf_db_${w} seq\
                              -updatecommand retr_update -end ${file}\
                              -col ${col} "${pattern}${sn_sep}"]}
                        } else {

                            set sup_class [lindex ${pattern} 0]
                            set mbr [lindex ${pattern} 1]
                            #find out if the class has parent classes, if true
                            #accept there members and symbols
                            foreach c\
                              [sn_get_class_inheritance_chain ${sup_class}] {
                                catch {eval lappend res [paf_db_${w} seq\
                                  -updatecommand retr_update -end ${file}\
                                  -col ${col} "${c}${sn_sep}${mbr}${sn_sep}"]}
                            }
                        }
                    } else {
                        catch {set res [eval paf_db_${w} seq\
                          -updatecommand retr_update ${file_flt}\
                          ${comp_res_flt} -col [list ${col}] ${comp_key}]}
                    }
                }
            "lv" -
            "cov" {
                    #local variables and commen variales in fortran
                    catch {set res [eval paf_db_${w} seq\
                      -updatecommand retr_update ${file_flt} ${comp_res_flt}\
                      -col [list [list "1 \(${w}\)\\t" "0 \\t" "6 \\t\\t"\
                      "3 \\t" "2"]] ${comp_key}]}
                }
            "f" {
                    #read all symbols in a file
                    if {[info commands paf_db_fil] != ""} {
                        set res [paf_db_fil seq -updatecommand retr_update\
                          -col [list "3 \(" "4 \)\\t" "2 \\t\\t\(" "8 \)\\t"\
                          "0 \\t" "6 \\t" "7"] "${file}${sn_sep}"]
                    } else {
                        set res ""
                    }
                }
            "files" {
                    #read file names
                    if {[info commands paf_db_f] != ""} {
                        set res [eval paf_db_f seq -updatecommand retr_update\
                          ${file_flt} ${file_res_flt} -col [list [list\
                          "0 #\\t" "1 \\t\\t\\t\\t"]] ${key}]
                    } else {
                        set res ""
                    }
                }
            default {

                    #following types haven't class names
                    #View type and parameters by a class of those, that can\
                      have 
                    #parameters
                    if {[lsearch -exact {fd fu} ${w}] != -1} {
                        set Op "("
                        set Cl ")\\t"
                    } else {
                        set Op ""
                        set Cl "\\t"
                    }
                    catch {set res [eval paf_db_${w} seq\
                      -updatecommand retr_update ${file_flt} ${simple_res_flt}\
                      -col [list [list "0 \(${w}\)\\t\\t" "5 \\t${Op}"\
                      "6 \"${Cl}\"" "2 \\t" "1 \\t" "3"]] ${key}]}
                }
        }
        if {${res} != ""} {
            eval lappend cnt ${res}
        }
    }
    if {![info exists cnt]} {
        set cnt ""
    }

    if {${from} == ""} {
        set from -1
    }
    if {${to} == ""} {
        set to -1
    }
    #filter type and parameter if availiable
    if {${type} != "" || ${param} != "" || ${from} != -1 || ${to} != -1} {
        #use an interactive filter to speed up the filtering
        set cnt [retriever_services "" filter ${cnt} ${pattern} ${file}\
          ${type} ${param} ${from} ${to}]
    }

    #reenable DB-fetching
    incr Retr_DbFetch_Active -1

    #if offset is specified, return only one entry
    #DON'T sort the results before!
    if {${offset} > -1 && [llength $cnt] > 0} {
        set cnt [list [lindex ${cnt} ${offset}]]
    }

    if {${cls} != ""} {
        set cnt [lsort -command sn_compare ${cnt}]
        ${cls} setcontents ${cnt}
        return ${cnt}
    } else {
        return [lsort -command sn_compare ${cnt}]
    }
}

proc display_contents_x {cls cnt {parent -1} {resize "resize"}} {
    if {${cnt} == ""} {
        bell
        return 0
    }
    #delete old entries
    if {${parent} == -1} {
        ${cls}.tree delete 0 end
        set filter [${cls} getfilter]
    } else {
        set filter "*"
    }

    #add the items using C/Function
    if {${filter} == "" || ${filter} == "*"} {
        retriever_services ${cls}.tree insert ${cnt} ${parent}
    } else {
        retriever_services ${cls}.tree insert [Tree::filter ${cnt}\
          ${filter}] ${parent}
    }

    #change size of the window
    if {${resize} == "resize"} {
        set height [${cls} size]
        if {${height} > 20} {
            set height 20
        }
        if {${height} > 0} {
            ${cls} config -height ${height}
        }
    }

    return 1
}

proc display_contents {cls cnt {parent -1}} {
    #delete old entries
    if {${parent} == -1} {
        ${cls}.tree delete 0 end
        set filter [${cls} getfilter]
    } else {
        set filter "*"
    }

    #add the items using C/Function
    if {${filter} == "" || ${filter} == "*"} {
        retriever_services ${cls}.tree insert ${cnt} ${parent}
    } else {
        retriever_services ${cls}.tree insert [Tree::filter ${cnt}\
          ${filter}] ${parent}
    }

    $cls SyncTabs
    return ""
}

proc edit_symbol {w {target ""} {client_data ""}} {
    if {${w} != ""} {
        edit_member ${w}
        return ""
    }

    if {${target} == ""} {
        return
    }
    set target [string trim ${target}]
    set pars [split ${target} "\t"]

    set file [lindex ${pars} 4]
    set pos [lindex ${pars} 5]

    set trg [join [lrange ${pars} 0 1] "\t"]
    set sym [sn_get_symbol_and_scope ${trg}]
    set type [lindex ${sym} 1]
    set name [lindex ${sym} 0]

    if {${type} == "" || ${name} == ""} {
        return
    }

    #if we have already the file and position, goto the item directly
    if {${file} != "" && ${pos} != ""} {
        sn_edit_file "" ${file} ${pos}
        #sn_add_history $type [list $type $name 0 $file]\
          [sn_make_history_title edit $type $name]
    } else {
        #We have here a file entry
        #format: 'foo.c \t sources/project/x \t c++'
        set file [lindex ${pars} 1]
        if {${file} == ""} {
            set file [lindex ${pars} 0]
        } else {
            set file [file join ${file} [lindex ${pars} 1]]
        }
        sn_edit_file "" ${file}
        #sn_add_history f [list $file] [sn_make_history_title edit f $file]
    }
}

proc sn_retrieve_symbol {pat what {file ""} {mtype -glob} {edit_single 1}\
  {bell 1} {client_func edit_symbol} {client_data ""} {type ""} {param ""}\
  {filter "*"} {offset -1} {merge ""}} {
    global sn_options
    global tkeWinNumber

    #find reusable window
    set window [find_reusable_window "retr-" 0]
    if {${window} == ""} {
        incr tkeWinNumber
        set win ".sn-retr-${tkeWinNumber}"
        #we use now a class
        Retriever& ${win} -pattern ${pat} -what ${what} -file ${file}\
          -type ${type} -param ${param} -mtype ${mtype}\
          -edit_single ${edit_single} -bell ${bell} -title "" -icon ""\
          -icon_prefix "" -offset ${offset} -client_func ${client_func}\
          -client_data ${client_data} -merge ${merge}

        if {[catch {set ret [${win} return_status]}]} {
            return 0
        }
        
        if {${ret} == 0} {
            itcl::delete object ${win}
        }
    } else {
        set pat [Retriever&::pattern ${pat}]
        set cnt [read_matched_from_db "" ${what} ${mtype} ${pat} ${type}\
          ${param} ${file} -1 -1 0 ${offset} ${merge}]
        if {[llength ${cnt}] == 1} {
            Retriever&::goto_symbol "" ${cnt} ${client_data} ${client_func}
            set ret 1
        } else {
            ${window}.list setcontents ${cnt}
            set ret [display_contents_x ${window}.list ${cnt}]
            if {${ret}} {
                if {[wm state ${window}] != "normal"} {
                    wm deiconify ${window}
                } else {
                    raise ${window}
                }
            }
        }
    }

    #Add only the history for retrieving without
    #a client function, because when the function
    #depeneds on an existing object (itcl class)
    #it will fail in the next call when this object
    #doesn't more exist
    if {(${client_func} == "edit_symbol" || ${client_func} == "") &&\
      ${client_data} == ""} {
        sn_add_history retr [list ${pat} ${what} ${file} ${mtype}\
          ${edit_single} ${bell} ${client_func} ${client_data} ${type}\
          ${param} ${filter}] [sn_make_history_title retr "" ${pat}]\
          sn_retrieve_symbol
    }
    return ${ret}
}

proc retriever_what_to_qry {var_prefix} {

    # Remove the namespace, since a fully qualified
    # var name will problably not match any thing
    # in the for loop below.
    set var_prefix [namespace tail $var_prefix]

    set res ""
    foreach v [info globals "${var_prefix}-sc-*"] {
        upvar #0 ${v} val
        if {${val} != ""} {
            lappend res ${val}
        }
    }
    return ${res}
}


