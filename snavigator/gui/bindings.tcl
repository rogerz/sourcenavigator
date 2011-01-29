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
# bindings.tcl - Bindings?
# Copyright (C) 1998 Cygnus Solutions.

# tkTreeTableUpDown --
#
# Moves the location cursor (active element) up or down by one element,
# and changes the selection if we're in browse or extended selection
# mode.
#
# Arguments:
# w -           The listbox widget.
# amount -      +1 to move down one item, -1 to move back one item.

proc sn_bindings {} {
    global sn_windows_to_check

    bind Listbox <3> {sn_listbox_post_menu %W %X %Y}
    bind Listbox <Control-3> {sn_listbox_post_menu %W %X %Y}
    bind Listbox <Shift-3> {sn_listbox_post_menu %W %X %Y}
    bind Canvas <2> {%W scan mark %x %y}
    bind Canvas <B2-Motion> {%W scan dragto %x %y}

    #Failed Button bindings from Tk
    bind Button <Tab> {focus [tk_focusNext %W]}
    bind Button <Shift-Tab> {focus [tk_focusPrev %W]}

    #failed checkbutton bindings from Tk
    bind Checkbutton <Tab> {focus [tk_focusNext %W]}
    bind Checkbutton <Shift-Tab> {focus [tk_focusPrev %W]}

    #failed radio button bindings from Tk
    bind Radiobutton <Tab> {focus [tk_focusNext %W]}
    bind Radiobutton <Shift-Tab> {focus [tk_focusPrev %W]}

    #bind alt-key to  traversale to buttons, check/radio buttons,
    #Text+Entry
    #The binding must be before the default "MENU-BINDING",
    #when it doesn't proceed, it call the menu binding
    set obindings [bind all <Alt-Key>]
    bind all <Alt-Key> "if {\[sn_execute_alt_accelerator %W %K\] == 0} {
					break
				}
				${obindings}"

    # reset to list of windows where we do need checks
    set sn_windows_to_check ""

    auto_load tkTextSetup

    sn_entry_default_bindings Entry

    sn_text_default_bindings Text

    sn_canv_goto_bindings Canvas

    sn_treetable_bindings TreeTable

    sn_init_keybindings
}

proc sn_listbox_post_menu {w x y} {
    set m .sn_pop_menu_listbox
    # It has to be destroyed because we might have problems with "tk_popup"!
    catch {destroy ${m}}

    menu ${m} -tearoff 0 -postcommand "sn_listbox_post_menu_update ${m} ${w}"
    wm overrideredirect ${m} 1

    tk_popup ${m} ${x} ${y}
}

proc sn_listbox_post_menu_update {m w} {
    ${m} delete 0 end
    set sel_size [llength [${w} curselection]]

    ${m} add command -label [format [get_indep String ListSize] [${w} size]\
      ${sel_size}]
}

# This seems to be a duplicate of the ide_treetable in libgui tree.tcl

proc sn_treetable args {
    set frame [lindex ${args} 0]

    set tree ${frame}.tree

    scrollbar ${frame}.x -orient horiz -command " ${tree} xview "
    scrollbar ${frame}.y -command " ${tree} yview "

    set args [lreplace ${args} 0 0 -yscrollcommand "${frame}.y set"\
      -xscrollcommand "${frame}.x set"]

    eval treetable ${tree} ${args}

    focus $tree

    return ${tree}
}

##########################################################################
## bindings for the new treetable widget                                ##
##########################################################################
proc tkTreeTableUpDown {w amount} {
    global tkPriv
    ${w} activate [expr [${w} index active] + ${amount}] ${amount}
    ${w} see active
    switch [${w} cget -selectmode] {
        browse {
                ${w} selection clear 0 end
                #			$w selection set active
            }
        extended {
                ${w} selection clear 0 end
                #			$w selection set active
                if {[string compare [${w} index active] ""] != 0} {
                    ${w} selection anchor active
                    set tkPriv(listboxPrev) [${w} index active]
                }
                set tkPriv(listboxSelection) {}
            }
    }
}

proc sn_treetable_bindings {t} {
    #the keybindings to the treetable are compatible to
    #those of listbox.
    bind ${t} <1> {
		if [winfo exists %W] {
			focus %W
			tkListboxBeginSelect %W [%W index @%x,%y]
		}
	}

    bind TreeTable <Double-1> {
	}

    bind ${t} <B1-Motion> {
		set tkPriv(x) %x
		set tkPriv(y) %y
		catch {tkListboxMotion %W [%W index @%x,%y]}
	}
    bind ${t} <ButtonRelease-1> {
		tkCancelRepeat
		%W activate @%x,%y
	}
    bind ${t} <Shift-1> {
		tkListboxBeginExtend %W [%W index @%x,%y]
	}
    bind ${t} <Control-1> {
		tkListboxBeginToggle %W [%W index @%x,%y]
	}
    bind ${t} <B1-Leave> {
		set tkPriv(x) %x
		set tkPriv(y) %y
		tkListboxAutoScan %W
	}
    bind ${t} <B1-Enter> {
		tkCancelRepeat
	}

    bind ${t} <Up> {
		tkTreeTableUpDown %W -1
	}


    if {[string equal "unix" $::tcl_platform(platform)]} {
        bind ${t} <Button-4> {
            %W yview scroll -5 units
        }
        bind ${t} <Shift-Button-4> {
            %W yview scroll -1 units
        }
        bind ${t} <Control-Button-4> {
            %W xview scroll -10 units
        }
        bind ${t} <Button-5> {
            %W yview scroll 5 units
        }
        bind ${t} <Shift-Button-5> {
            %W yview scroll 1 units
        }
        bind ${t} <Control-Button-5> {
            %W xview scroll 10 units
        }
    } else {
        bind ${t} <MouseWheel> {
            %W yview scroll [expr {- (%D / 120) * 4}] units
        }
    }

  bind ${t} <Shift-Up> {
		tkListboxExtendUpDown %W -1
	}
    bind ${t} <Down> {
		tkTreeTableUpDown %W 1
	}
    bind ${t} <Shift-Down> {
		tkListboxExtendUpDown %W 1
	}
    bind ${t} <Left> {
		%W xview scroll -1 units
	}
    bind ${t} <Control-Left> {
		%W xview scroll -1 pages
	}
    bind ${t} <Right> {
		%W xview scroll 1 units
	}
    bind ${t} <Control-Right> {
		%W xview scroll 1 pages
	}
    bind ${t} <Prior> {
		%W yview scroll -1 pages
		%W activate @0,0
	}
    bind ${t} <Next> {
		%W yview scroll 1 pages
		%W activate @0,0
	}
    bind ${t} <Control-Prior> {
		%W xview scroll -1 pages
	}
    bind ${t} <Control-Next> {
		%W xview scroll 1 pages
	}
    bind ${t} <Home> {
		%W xview moveto 0
	}
    bind ${t} <End> {
		%W xview moveto 1
	}
    bind ${t} <Control-Home> {
		%W activate 0
		%W see 0
		%W selection clear 0 end
		%W selection set 0
	}
    bind ${t} <Shift-Control-Home> {
		tkListboxDataExtend %W 0
	}
    bind ${t} <Control-End> {
		%W activate end
		%W see end
		%W selection clear 0 end
		%W selection set end
	}
    bind ${t} <Shift-Control-End> {
		tkListboxDataExtend %W end
	}
    bind ${t} <F16> {
		if {[selection own -displayof %W] == "%W"} {
			clipboard clear -displayof %W
			clipboard append -displayof %W [selection get -displayof %W]
    		}
	}
    bind ${t} <space> {
		tkListboxBeginSelect %W [%W index active]
	}
    bind ${t} <Select> {
		tkListboxBeginSelect %W [%W index active]
	}
    bind ${t} <Control-Shift-space> {
		tkListboxBeginExtend %W [%W index active]
	}
    bind ${t} <Shift-Select> {
		tkListboxBeginExtend %W [%W index active]
	}
    bind ${t} <Escape> {
		tkListboxCancel %W
	}
    bind ${t} <Control-slash> {
		tkListboxSelectAll %W
	}
    bind ${t} <Control-backslash> {
		if {[%W cget -selectmode] != "browse"} {
			%W selection clear 0 end
		}
	}

    bind ${t} <2> {
		%W scan mark %x %y
	}
    bind ${t} <B2-Motion> {
		%W scan dragto %x %y
	}

    #other bindings added to default listbox bindings
    bind ${t} <Key> {sn_tree_table_search_region %W %A %s}
    bind ${t} <3> {sn_listbox_post_menu %W %X %Y}
    bind ${t} <Control-3> {sn_listbox_post_menu %W %X %Y}
    # Sun Home
    bind ${t} <Any-F27> [bind ${t} <Home>]
    # Sun End
    bind ${t} <Any-R13> [bind ${t} <End>]
    # Sun Next
    bind ${t} <Any-R15> [bind ${t} <Next>]
    # Sun Prior
    bind ${t} <Any-R9> [bind ${t} <Prior>]

    bind ${t} <Tab> {focus [tk_focusNext %W]}
    bind ${t} <Shift-Tab> {focus [tk_focusPrev %W]}
}
proc sn_tree_table_search_in_widget {w a beg {end end}} {
    if {[${w} size] > 20000} {
        ${w} config -cursor watch
        update idletasks
    }
    set res [${w} search -nocase -begins -- ${a} ${beg} ${end}]
    ${w} config -cursor {}
    if {${res} == ""} {
        return -1
    }
    ${w} activate ${res}
    return ${res}
}

proc sn_tree_table_search_region {w a state} {

    #accept only ascii-characters
    if {[string compare ${a} ""] == 0 || [string length ${a}] > 1} {
        return -1
    }
    #returns if alt-key is pressed (reserved for menu)
    if {[expr {${state} & 8}] == 8} {
        return -1
    }

    upvar #0 ${w}-pat pat

    append pat ${a}
    set srch ${pat}
    set off [${w} index active]
    if {[string compare ${off} ""] == 0} {
        set sel 0
    } else {
        set sel [expr ${off} + 1]
    }
    # Search from the selection!
    set off [sn_tree_table_search_in_widget ${w} ${srch} ${sel}]

    if {${off} == -1 && ${sel} != 0} {
        # 	Search until the selection!
        set off [sn_tree_table_search_in_widget ${w} ${srch} 0 ${sel}]
    }
    if {${off} == -1} {
        if {[string length ${pat}] > 1} {
            set pat ${a}
            set srch ${pat}

            # Search from the selection!
            set off [sn_tree_table_search_in_widget ${w} ${srch} ${sel}]
            if {${off} == -1} {
                # 	Search until the selection!
                set off [sn_tree_table_search_in_widget ${w} ${srch} 0 ${sel}]
            }
        }
        if {${off} == -1} {
            set pat ""
            bell -displayof ${w}
            ${w} selection clear 0 end
            return 1
        }
    }
    ${w} activate ${off}
    ${w} activate ${off}
    ${w} yview see ${off}

    return 0
}
###########################################################################
## END of definitions for new treetable widget                           ##
###########################################################################

proc sn_tree_table_remove {w} {
    foreach s [lsort -decreasing -integer [${w} curselection]] {
        ${w} remove ${s}
    }
}

proc sn_treetable_scroll {lb which} {
    set cur [${lb} nearest 0]
    set sel [lindex [${lb} curselection] 0]
    if {${sel} == ""} {
        set sel 0
    }
    set hg [winfo height ${lb}]
    if {${hg} == 1} {
        set hg [winfo reqheight ${lb}]
    }

    set last [${lb} nearest ${hg}]
    set sz [${lb} size]
    set disp [expr "(${last} - ${cur}) + 1"]

    switch -- ${which} {
        Up {
                incr sel -1
                set cur [expr "${cur} <= 0 ? ${cur} : ${cur} - 1"]
            }
        Down {
                incr sel 1
                incr cur
                set newend [expr "${cur} + ${disp}"]
                if {${newend} >= ${sz}} {
                    set cur [expr "${sz} - ${disp}"]
                }
            }
        PgUp {
                incr cur "-${disp}"
                incr sel "-${disp}"
                if {${cur} < 0} {
                    set cur 0
                }
            }
        PgDn {
                incr cur ${disp}
                incr sel ${disp}
                set newend [expr "${cur} + ${disp}"]
                if {${newend} > ${sz}} {
                    set cur [expr "${sz} - ${disp}"]
                }
            }
        Home {
                set cur 0
                set sel 0
            }
        End {
                set cur [expr "${sz} - ${disp}"]
                set sel end
            }
        default {
                error "Unknown scroll request '${lb} ${which}'."
            }
    }
    ${lb} yview ${cur}
    ${lb} select from ${sel}
    ${lb} select to ${sel}
}

proc sn_entry_default_bindings {entry} {
    global sn_options

    bind ${entry} <Control-a> {tkEntrySetCursor %W 0}
    bind ${entry} <Control-b> {tkEntrySetCursor %W [expr [%W index insert] - 1]}
    bind ${entry} <Control-d> [bind ${entry} <Delete>]
    bind ${entry} <Control-e> {tkEntrySetCursor %W end}
    bind ${entry} <Control-f> {tkEntrySetCursor %W [expr [%W index insert] + 1]}
    bind ${entry} <Control-h> {tkEntryBackspace %W}
    bind ${entry} <Control-k> {%W delete insert end}
    bind ${entry} <Control-t> {tkEntryTranspose %W}
    bind ${entry} <Control-u> {
	    %W delete 0 end
	    %W xview 0
	}
    bind ${entry} <Meta-b> {
		tkEntrySetCursor %W [string wordstart [%W get] [expr [%W index insert] - 1]]
	}
    bind ${entry} <Meta-d> {
		%W delete insert [string wordend [%W get] [%W index insert]]
	}
    bind ${entry} <Meta-f> {
		%W delete [string wordstart [%W get] [expr [%W index insert] - 1]] insert
	}

    #add a hook for the french keyboard. "~" and "," work
    #bugy on windows, they are bound on AltGr-e' and AltGr-e`
    bind ${entry} <Alt-Control-KeyPress> {+
				if {%k == 55} {
					tkEntryInsert %W "`"
					break
				}
				if {%k == 50} {
					tkEntryInsert %W "~"
					break
				}
			}
}

# returns 1 if it could insert the digit otherwise 0.

proc sn_entry_insert_digit {w a scale {check 0} {bell 1}} {

    set input_char "0123456789eE.-"
    if {${check} == 0} {
        append input_char "NULnul!=<>|"
    }
    if {${a} == ","} {
        set a "."
    }
    if {[string first ${a} ${input_char}] == -1} {
        if {${a} != "" && ${bell}} {
            bell
        }
        return 0
    }
    switch -- ${a} {
        "." {
                if {${scale} == "0" || [string first "." [${w} get]] != -1} {
                    if {${a} != "" && ${bell}} {
                        bell
                    }
                    return 0
                }
            }
        "-" {
                if {[${w} index insert] != 0} {
                    if {${a} != "" && ${bell}} {
                        bell
                    }
                    return 0
                }
            }
        "E" -
        "e" {
                set fld [string tolower [${w} get]]
                if {${scale} == "0" || [string first "." ${fld}] == -1 ||\
                  [string first "e" ${fld}] != -1} {

                    if {${a} != "" && ${bell}} {
                        bell
                    }
                    return 0
                }
            }
    }

    set curs [${w} index insert]
    if {[catch {set first [${w} index sel.first]}]} {
        set first -1
    }
    if {[catch {set last [${w} index sel.last]}]} {
        set last -1
    }
    if {${first} >= 0 && ${last} >= 0 && ${first} <= ${curs} && ${curs} <=\
      [expr ${last} + 1]} {

        ${w} delete ${first} ${last}
    }
    ${w} insert insert ${a}

    return 1
}

proc sn_entry_insert_column {w a scale {check 0} {bell 1}} {

    set input_char\
      "01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$#@"
    if {${check} == 0} {
        append input_char "NULnul!=<>|"
    }
    if {[string first ${a} ${input_char}] == -1} {
        if {${a} != "" && ${bell}} {
            bell
        }
        return
    }

    set curs [${w} index insert]
    if {[catch {set first [${w} index sel.first]}]} {
        set first -1
    }
    if {[catch {set last [${w} index sel.last]}]} {
        set last -1
    }
    if {${first} >= 0 && ${last} >= 0 && ${first} <= ${curs} && ${curs} <=\
      [expr ${last} + 1]} {

        ${w} delete ${first} ${last}
    }
    ${w} insert insert ${a}
}

proc sn_text_default_bindings {text} {
    bind ${text} <Control-d> [bind ${text} <Delete>]
    bind ${text} <Control-a> [bind ${text} <Home>]
    bind ${text} <Control-e> [bind ${text} <End>]

    if {[string equal "unix" $::tcl_platform(platform)]} {
        bind ${text} <Button-5> [list %W yview scroll 5 units]
        bind ${text} <Button-4> [list %W yview scroll -5 units]
        bind ${text} <Shift-Button-5> [list %W yview scroll 1 units]
        bind ${text} <Shift-Button-4> [list %W yview scroll -1 units]
        bind ${text} <Control-Button-5> [list %W xview scroll 10 units]
        bind ${text} <Control-Button-4> [list %W xview scroll -10 units]
    } else {
        bind ${text} <MouseWheel> { %W yview scroll [expr {- (%D / 120) * 4}] units }
    }

    catch {
        bind ${text} <apLineDel> "[bind ${text} <Delete>]; break"
    }

    #add a hook for the french keyboard. "~" and "," work
    #bugy on windows, they are bound on AltGr-e' and AltGr-e`
    bind Text <Alt-Control-KeyPress> {+
				if {%k == 55} {
					tkTextInsertChar %W "`"
					break
				}
				if {%k == 50} {
					tkTextInsertChar %W "~"
					break
				}
			}

}

# tkTextClipboardKeysyms --
# This procedure is invoked to identify the keys that correspond to
# the "copy", "cut", and "paste" functions for the clipboard.
#
# Arguments:
# copy -        Name of the key (keysym name plus modifiers, if any,
#               such as "Meta-y") used for the copy operation.
# cut -         Name of the key used for the cut operation.
# paste -       Name of the key used for the paste operation.

catch {rename tkTextClipboardKeysyms {}}
# Don't use the original one.			
proc tkTextClipboardKeysyms {copy cut paste} {
    bind Text <${copy}> {
		if {[selection own -displayof %W] == "%W"} {
			clipboard clear -displayof %W
			catch {
				clipboard append -displayof %W [selection get -displayof %W]
			}
		}
	}
    bind Text <${cut}> {
		if {[selection own -displayof %W] == "%W"} {
			clipboard clear -displayof %W
			catch {
				clipboard append -displayof %W [selection get -displayof %W]
				%W delete sel.first sel.last
			}
		}
	}
    bind Text <${paste}> {
		catch {
			if {[%W compare sel.first <= insert] && [%W compare sel.last >= insert]} {
				%W delete sel.first sel.last
			}
		}
		catch {%W insert insert [selection get -displayof %W -selection CLIPBOARD]}
	}
}

# We do need this dummy function for bindings
proc sn_dummy_proc {k} {
    #	puts stdout "Keysym: \"$k\""
}

proc sn_input_check_in_window {win check} {
    global sn_windows_to_check

    if {[catch {set topl [winfo toplevel ${win}]}]} {
        return
    }

    set off [lsearch ${sn_windows_to_check} ${topl}]

    if {${off} == -1} {
        if {${check}} {
            lappend sn_windows_to_check ${topl}
        }
    } else {
        #it is already in the list, if it is already checked, return
        if {${check}} {
            return
        }
        if {${off} > 0} {
            set new_list [lrange ${sn_windows_to_check} 0 [expr ${off} -1]]
        } else {
            set new_list ""
        }
        set new_list2 [lrange ${sn_windows_to_check} [expr ${off} +1] end]

        set sn_windows_to_check ${new_list}

        if {[llength ${new_list2}] > 0} {
            lappend sn_windows_to_check ${new_list2}
        }
    }
}

proc sn_canvas_input_bindings {c entry} {
    bind ${c} <Double-1> {sn_dummy_proc %K}
    bind ${c} <Triple-1> {sn_dummy_proc %K}
    #	bind $c <3> [bind $c <1>]

    sn_canvas_mark_bindings ${c} ${entry}

    ${c} bind ${entry} <1> {
		%W select clear
		%W focus current
		%W icursor current @[%W canvasx %x],[%W canvasy %y]
		%W select from current @[%W canvasx %x],[%W canvasy %y]
		break
	}

    #	$c bind $entry <3> [$c bind $entry <1>]

    ${c} bind ${entry} <KeyPress> {
		sn_canvas_entry_insert_input %W [%W focus] %A %K
		break
	}
    catch {${c} bind ${entry} <dead_tilde> {
		sn_canvas_entry_insert_input %W [%W focus] "~" %K
		break
	}}
    catch {${c} bind ${entry} <dead_circumflex> {
		sn_canvas_entry_insert_input %W [%W focus] "^" %K
		break
	}}
    catch {${c} bind ${entry} <dead_grave> {
		sn_canvas_entry_insert_input %W [%W focus] "`" %K
		break
	}}
    catch {${c} bind ${entry} <ssharp> {
		sn_canvas_entry_insert_input %W [%W focus] "?" %K
		break
	}}

    ${c} bind ${entry} <Return> {sn_dummy_proc %K}
    ${c} bind ${entry} <Any-Escape> {sn_dummy_proc %K}
    ${c} bind ${entry} <Any-Control-KeyPress> {sn_dummy_proc %K}

    ${c} bind ${entry} <BackSpace> {
		if {[%W index [%W focus] insert] > 0} {
			%W dchars [%W focus] [expr [%W index [%W focus] insert] - 1]
			set %W-changed 1
		}
		break
	}
    ${c} bind ${entry} <Any-BackSpace> [${c} bind ${entry} <BackSpace>]
    ${c} bind ${entry} <Home> {%W icursor [%W focus] 0}
    # Sun Home
    ${c} bind ${entry} <F27> [${c} bind ${entry} <Home>]
    ${c} bind ${entry} <Control-Left> [${c} bind ${entry} <Home>]
    ${c} bind ${entry} <End> {%W icursor [%W focus] end}
    ${c} bind ${entry} <Shift-Home> [${c} bind ${entry} <End>]
    # Sun End
    ${c} bind ${entry} <R13> [${c} bind ${entry} <End>]
    ${c} bind ${entry} <Control-Right> [${c} bind ${entry} <End>]
    ${c} bind ${entry} <Control-d> {
		if {[catch {%W dchars [%W focus] sel.first sel.last}]} {
			%W dchars [%W focus] insert
		}
		set %W-changed 1
		break
	}
    catch {
        ${c} bind ${entry} <apLineDel> [${c} bind ${entry} <Control-d>]
    }

    ${c} bind ${entry} <Delete> [${c} bind ${entry} <Control-d>]

    ${c} bind ${entry} <Control-h> [${c} bind ${entry} <Any-BackSpace>]
    ${c} bind ${entry} <Control-k> {
		%W dchars [%W focus] insert end
		set %W-changed 1
		break
	}
    ${c} bind ${entry} <Control-l> {
		%W dchars [%W focus] 0 end
		set %W-changed 1
		break
	}
    ${c} bind ${entry} <Control-u> {
		%W dchars [%W focus] 0 end
		set %W-changed 1
		break
	}
    ${c} bind ${entry} <Control-w> {
		sn_canvas_entry_mark_word %W [%W focus] [%W index [%W focus] insert]
		catch {%W dchars [%W focus] sel.first sel.last}
		break
	}
    ${c} bind ${entry} <Left> {
		%W select clear
		if {[%W index [%W focus] insert] > 0} {
			%W icursor [%W focus]  [expr [%W index [%W focus] insert] - 1]
		}
		break
	}
    ${c} bind ${entry} <Right> {
		%W select clear
		%W icursor [%W focus] [expr [%W index [%W focus] insert] + 1]
		break
	}
    ${c} bind ${entry} <Down> {
		sn_canvas_line_down %W [%W focus]
		break
	}

    ${c} bind ${entry} <Up> {
		sn_canvas_line_up %W [%W focus]
		break
	}

    ${c} bind ${entry} <Control-x> {
		if {[selection own] != "%W"} {
			return
		}
		if {[catch {set cut [sn_selection get]}]} {
			return
		}
		catch {%W dchars [%W focus] sel.first sel.last}
		if {[string compare $cut ""] != 0} {
			blt_cutbuffer set $cut 0
		}
		set %W-changed 1
		break
	}
    # Sun Cut
    ${c} bind ${entry} <Any-F20> [${c} bind ${entry} <Control-x>]

    ${c} bind ${entry} <3> [${c} bind ${entry} <1>]

    ${c} bind ${entry} <2> {
		%W focus current
		%W icursor current @[%W canvasx %x],[%W canvasy %y]
		if {[catch {set pst [string trim [sn_selection get]]}] == 0} {
			regsub -all "\t|\n" $pst "" pst
			sn_canvas_entry_insert_input %W current $pst
		}
		break
	}
    ${c} bind ${entry} <Control-v> {
		if {[catch {set pst [string trim [blt_cutbuffer get 0]]}] == 0} {
			regsub -all "\t|\n" $pst "" pst
			sn_canvas_entry_insert_input %W [%W focus] $pst
		}
		break
	}

    # Sun Paste
    ${c} bind ${entry} <Any-F18> [${c} bind ${entry} <Control-v>]
    ${c} bind ${entry} <Shift-2> [${c} bind ${entry} <Control-v>]
}

proc sn_canvas_mark_bindings {c entry} {
    ${c} bind ${entry} <B1-Motion> {
		if {[%W index current insert] != [%W index current  @[%W canvasx %x],[%W\
      canvasy %y]]} {
			%W select to current @[%W canvasx %x],[%W canvasy %y]
		}
		break
	}
    ${c} bind ${entry} <Shift-Left> {
		if {[%W index [%W focus] insert] > 0} {
			if {[catch {set sn_mrk [%W index [%W focus] sel.first]}]} {
				set sn_mrk [%W index [%W focus] insert]
			}
			%W select adjust [%W focus] [expr $sn_mrk - 1]
		}
		break
	}
    ${c} bind ${entry} <Shift-R10> [${c} bind ${entry} <Shift-Left>]

    ${c} bind ${entry} <Shift-Right> {
		if {[%W index [%W focus] insert] >= 0} {
			if {[catch {set sn_mrk [%W index [%W focus] sel.last]}]} {
				set sn_mrk [%W index [%W focus] insert]
				%W select from [%W focus] $sn_mrk
				incr sn_mrk -1
			}
			%W select adjust [%W focus] [expr $sn_mrk + 1]
		}
		break
	}
    ${c} bind ${entry} <Shift-R12> [${c} bind ${entry} <Shift-Right>]

    ${c} bind ${entry} <Double-1> {
		sn_canvas_entry_mark_word %W current  @[%W canvasx %x],[%W canvasy %y]
		break
	}
    ${c} bind ${entry} <Triple-1> {
		%W select from current 0
		%W select to current end
		break
	}
    ${c} bind ${entry} <Control-c> {
		if {[catch {set cut [sn_selection get]}]} {
			return
		}
		blt_cutbuffer set $cut 0
		break
	}
    #$c bind $entry <Any-F4> [$c bind $entry <Any-F4>]

    # Sun Copy
    ${c} bind ${entry} <Any-F16> [${c} bind ${entry} <Control-c>]
}

proc sn_canvas_entry_insert_input {w id a {k ""} {forceinp 0} {bell 1}} {
    global sn_options
    global sn_windows_to_check
    global ${w}-changed

    if {[string compare ${a} ""] == 0} {
        sn_dummy_proc ${k}
        return
    }

    if {[lsearch ${sn_windows_to_check} [winfo toplevel ${w}]] == -1} {
        set check 0
    } else {
        set check 1
    }

    # syntax: %type_width_scale_font-width_font-height_flags

    set pars [sn_canvas_item_pars ${w} ${id}]
    set tags [lindex [${w} itemconfigure ${id} -tags] 4]
    if {[lsearch ${tags} "big"] == -1} {
        set big 0
    } else {
        set big 1
    }

    if {[llength ${pars}] < 2} {
        set type 4
        # SQL_CHA
        set width 0
        set scale 0
        set font_height 0
        set font_width 0
        set flags 0
    } else {
        set type [lindex ${pars} 0]
        set width [lindex ${pars} 1]
        set scale [lindex ${pars} 2]
        set font_height [lindex ${pars} 3]
        set font_width [lindex ${pars} 4]
        set flags [lindex ${pars} 5]
        # Support for older form versions.
        if {${font_height} == ""} {
            set font_height [expr $sn_options(def,desktop-font-size) + 2]
        }
    }
    if {${check} == 0} {
        set width 0
    }\
    elseif {${type} == 21 && ${forceinp} == 0} {
        # 	only lookup field
        if {${bell}} {
            bell
        }
        return
    }

    set ${w}-changed 1

    # Workaround SQLischar-Error !!!
    if {[SQLischar ${type}] && ${type} != 20} {
        set curs [${w} index ${id} insert]
        if {[catch {set first [${w} index ${id} sel.first]}]} {
            set first -1
        }
        if {[catch {set last [${w} index ${id} sel.last]}]} {
            set last -1
        }
        if {${first} >= 0 && ${last} >= 0 && ${first} <= ${curs} && ${curs} <=\
          [expr ${last} + 1]} {

            ${w} dchars ${id} ${first} ${last}
        }
        ${w} insert ${id} insert ${a}
    } else {
        if {${type} == 20} {
            for {set len [string length ${a}]; set i 0} {${i} < ${len}}\
              {incr i} {
                sn_canvas_entry_insert_column ${w} ${id} [string index ${a}\
                  ${i}] ${scale} ${check} ${bell}
            }
        } else {
            for {set len [string length ${a}]; set i 0} {${i} < ${len}}\
              {incr i} {
                if {![sn_canvas_entry_insert_digit ${w} ${id} [string index\
                  ${a} ${i}] ${scale} ${check} ${bell}]} {

                    break
                }
            }
        }
        if {${width} > 0} {
            set fld [lindex [${w} itemconfigure ${id} -text] 4]
            if {[string index ${fld} 0] != "-"} {
                incr width -1
            }

            set len [string first "." ${fld}]
            if {${len} > [expr ${width} - ${scale} -1]} {
                set width [expr ${width} - ${scale} -1]
            }\
            elseif {${scale} > 0 && ${len} == -1} {
                set width [expr ${width} - ${scale} -1]
            }
        }
    }
    set rid [sn_find_assigned_rect ${w} ${id}]
    if {${rid} != ""} {
        set coords [${w} coords ${rid}]
        set x [lindex ${coords} 2]
        set y [expr [lindex ${coords} 3] - ${font_height}]
        set idx [${w} index ${id} @${x},${y}]
        if {${idx} > ${width} && ${width} != 0} {
            set idx ${width}
        }
        ${w} dchars ${id} ${idx} end
    }\
    elseif {${width} > 0} {
        ${w} dchars ${id} ${width} end
    }
}

proc sn_canvas_entry_mark_word {w id ind} {
    set string [lindex [${w} itemconfigure ${id} -text] 4]

    for {set x [${w} index ${id} ${ind}]} {${x} > 0} {incr x -1} {
        if {([string first [string index ${string} ${x}] " \t"] < 0)\
          &&([string first [string index ${string} [expr ${x}-1]] " \t"] >=\
          0)} {

            break
        }
    }
    ${w} select from ${id} ${x}

    set len [string length ${string}]
    for {set x [${w} index ${id} ${ind}]} {${x} < ${len}} {incr x} {
        if {([string first [string index ${string} ${x}] " \t"] < 0)\
          &&([string first [string index ${string} [expr ${x}+1]] " \t"] >=\
          0)} {

            break
        }
    }
    ${w} select to ${id} [expr ${x}+1]
}

proc sn_canvas_entry_insert_column {w id a scale {check 0} {bell 1}} {

    set input_char\
      "01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$#@"
    if {${check} == 0} {
        append input_char "NULnul!=<>|"
    }
    if {[string first ${a} ${input_char}] == -1} {
        if {${a} != "" && ${bell}} {
            bell
        }
        return
    }

    set curs [${w} index insert]
    if {[catch {set first [${w} index ${id} sel.first]}]} {
        set first -1
    }
    if {[catch {set last [${w} index ${id} sel.last]}]} {
        set last -1
    }
    if {${first} >= 0 && ${last} >= 0 && ${first} <= ${curs} && ${curs} <=\
      [expr ${last} + 1]} {

        ${w} dchars ${id} ${first} ${last}
    }
    ${w} insert ${id} insert ${a}
}

# returns 1 if it could insert the digit otherwise 0.

proc sn_canvas_entry_insert_digit {w id a scale {check 0} {bell 1}} {

    set input_char "0123456789eE.-"
    if {${check} == 0} {
        append input_char "NULnul!=<>|"
    }
    if {${a} == ","} {
        set a "."
    }

    if {[string first ${a} ${input_char}] == -1} {
        if {${a} != "" && ${bell}} {
            bell
        }
        return 0
    }
    switch -- ${a} {
        "." {
                if {${scale} == "0" || [string first "." [lindex\
                  [${w} itemconfig ${id} -text] 4]] != -1} {
                    if {${a} != "" && ${bell}} {
                        bell
                    }
                    return 0
                }
            }
        "-" {
                if {[${w} index ${id} insert] != 0} {
                    if {${a} != "" && ${bell}} {
                        bell
                    }
                    return 0
                }
            }
        "E" -
        "e" {
                set fld [string tolower [lindex [${w} itemconfig ${id}\
                  -text] 4]]
                if {${scale} == "0" || [string first "." ${fld}] == -1 ||\
                  [string first "e" ${fld}] != -1} {

                    if {${a} != "" && ${bell}} {
                        bell
                    }
                    return 0
                }
            }
    }

    set curs [${w} index ${id} insert]
    if {[catch {set first [${w} index ${id} sel.first]}]} {
        set first -1
    }
    if {[catch {set last [${w} index ${id} sel.last]}]} {
        set last -1
    }
    if {${first} >= 0 && ${last} >= 0 && ${first} <= ${curs} && ${curs} <=\
      [expr ${last} + 1]} {

        ${w} dchars ${id} ${first} ${last}
    }
    ${w} insert ${id} insert ${a}

    return 1
}

proc sn_canvas_line_up {w id} {
    set sn_tmp_string [string range [lindex [${w} itemconfigure ${id}\
      -text] 4] 0 [expr [${w} index ${id} insert] -1]]

    set sn_tmp_off [string last "\n" ${sn_tmp_string}]

    if {${sn_tmp_off} != -1} {
        ${w} icursor ${id} ${sn_tmp_off}

        return 1
    }
    return 0
}

proc sn_canvas_line_down {w id} {
    set sn_tmp_ins [${w} index ${id} insert]
    set sn_tmp_string [string range [lindex [${w} itemconfigure ${id}\
      -text] 4] ${sn_tmp_ins} end]

    set sn_tmp_off [string first "\n" ${sn_tmp_string}]

    if {${sn_tmp_off} != -1} {
        ${w} icursor ${id} [expr ${sn_tmp_ins} + 1 + ${sn_tmp_off}]

        return 1
    }
    return 0
}

# syntax: %type_width_scale_height_linewidth_flags

proc sn_canvas_item_pars {c tid} {
    set tags [lindex [${c} itemconfigure ${tid} -tags] 4]
    set len [string last "%" ${tags}]

    set pars [string range ${tags} [expr ${len} +1] end]
    set off [string first " " ${pars}]
    if {${off} == -1} {
        set pars [split ${pars} "_"]
    } else {
        set pars [split [string range ${pars} 0 [expr ${off} - 1]] "_"]
    }
    return ${pars}
}

# This proc returns the id of the rectangle that belongs to the text.

proc sn_find_assigned_rect {c id} {
    set tags [lindex [${c} itemconfigure ${id} -tags] 4]
    set off [lsearch -regexp ${tags} {rid_[0-9]+}]

    if {${off} == -1} {
        return ""
    }

    set rid [lindex [split [lindex ${tags} ${off}] "_"] 1]

    if {[${c} type ${rid}] != "rectangle"} {
        return ""
    }
    return ${rid}
}

proc sn_canv_goto_bindings {c} {

    #apply input to canvases
    bind ${c} <ButtonRelease-1> "focus %W"

    if {[string equal "unix" $::tcl_platform(platform)]} {
        bind ${c} <Button-4> {sn_canvas_scroll %W MultiUp}
        bind ${c} <Button-5> {sn_canvas_scroll %W MultiDown}
        bind ${c} <Shift-Button-4> {sn_canvas_scroll %W Up}
        bind ${c} <Shift-Button-5> {sn_canvas_scroll %W Down}
        bind ${c} <Control-Button-4> {sn_canvas_scroll %W MultiLeft}
        bind ${c} <Control-Button-5> {sn_canvas_scroll %W MultiRight}
    } else {
        # don't see a way to use sn_canvas_scroll with this single binding
        bind ${c} <MouseWheel> {
            %W yview scroll [expr {- (%D / 120) * 4}] units
        }
    }

    bind ${c} <Left> {sn_canvas_scroll %W Left}
    bind ${c} <Right> {sn_canvas_scroll %W Right}
    bind ${c} <Control-Left> {sn_canvas_scroll %W PgLeft}
    bind ${c} <Control-Right> {sn_canvas_scroll %W PgRight}
    bind ${c} <Up> {sn_canvas_scroll %W Up}
    bind ${c} <Down> {sn_canvas_scroll %W Down}
    bind ${c} <Home> {sn_canvas_scroll %W Home}
    bind ${c} <F27> [bind ${c} <Home>]
    # Sun Home
    bind ${c} <Control-Up> [bind ${c} <Home>]
    bind ${c} <Shift-Up> [bind ${c} <Home>]
    bind ${c} <End> {sn_canvas_scroll %W End}
    bind ${c} <Shift-Home> [bind ${c} <End>]
    bind ${c} <Control-Down> [bind ${c} <End>]
    bind ${c} <Shift-Down> [bind ${c} <End>]
    bind ${c} <R13> [bind ${c} <End>]
    # Sun End
    bind ${c} <Next> {sn_canvas_scroll %W PgDn}
    bind ${c} <R15> [bind ${c} <Next>]
    # Sun Next
    bind ${c} <Prior> {sn_canvas_scroll %W PgUp}
    bind ${c} <R9> [bind ${c} <Any-Prior>]
    # Sun Prior
}

proc sn_canvas_scroll {c which} {
    switch -- ${which} {
        Left {
                ${c} xview scroll -1 unit
            }
        Right {
                ${c} xview scroll 1 unit
            }
        PgLeft {
                ${c} xview scroll -1 page
            }
        PgRight {
                ${c} xview scroll 1 page
            }
        HomeLeft {
                ${c} xview scroll -1 unit
            }
        HomeRight {
                ${c} xview scroll 1 unit
            }
        Up {
                ${c} yview scroll -1 unit
            }
        Down {
                ${c} yview scroll 1 unit
            }
        PgUp {
                ${c} yview scroll -1 page
            }
        PgDn {
                ${c} yview scroll 1 page
            }
        Home {
                ${c} yview moveto 0
            }
        End {
                ${c} yview moveto 1
            }
        MultiUp {
                ${c} yview scroll -5 units
            }
        MultiDown {
                ${c} yview scroll 5 units
            }
        MultiLeft {
                ${c} xview scroll -5 units
            }
        MultiRight {
                ${c} xview scroll 5 units
            }
    }
}

proc sn_selection args {
    set sel ""
    catch {set sel [eval selection ${args}]}
    return ${sel}
}

proc sn_remove_tags {w tags} {
    set current [bindtags ${w}]
    foreach tag ${tags} {
        set idx [lsearch -exact ${current} ${tag}]
        if {${idx} != -1} {
            set current [lreplace ${current} ${idx} ${idx}]
        }
    }
    bindtags ${w} ${current}
    return ${current}
}

proc sn_add_tags {w tags {pos 1}} {
    set current [bindtags ${w}]
    set otags ${current}
    #first delete avail tags from the list
    foreach tag ${tags} {
        set idx [lsearch -exact ${current} ${tag}]
        if {${idx} != -1} {
            set current [lreplace ${current} ${idx} ${idx}]
        }
    }

    set current [eval linsert [list ${current}] ${pos} ${tags}]

    #bind older and newer tags to the widget.
    bindtags ${w} ${current}

    return ${current}
}

proc sn_execute_alt_accelerator {win key} {
    set key [string tolower ${key}]
    if {! [string match {[a-z0-9]} ${key}]} {
    }
    return [sn_recursive_alt_accelerator ${win} [winfo toplevel ${win}] ${key}]
}

proc sn_recursive_alt_accelerator {origw win key} {
    if {![winfo ismapped ${win}]} {
        return -1
    }

    set idx -1
    foreach w [winfo children ${win}] {

        set class [winfo class ${w}]

        if {${class} == "Menu"} {
            continue
        }
        if {[sn_recursive_alt_accelerator ${origw} ${w} ${key}] == 0} {
            return 0
        }

        #locate the underlined character
        set ret [catch {set idx [${w} cget -underline]}]
        if {${ret} || ${idx} == -1} {
            continue
        }
        #try -text or -label
        set ret [catch {set ch [${w} cget -text]}]
        if {${ret}} {
            set ret [catch {set ch [${w} cget -label]}]
        }
        if {${ret}} {
            continue
        }
        #underlined character
        set ch [string range ${ch} ${idx} ${idx}]

        if {[string tolower ${ch}] == ${key}} {
            #accelerator for text entries, focus the editor
            #after this widget or invoke a check/radiobutton
            if {${class} == "Label"} {
                set parents [winfo parent ${w}]
                #scans all sub trees of this scope
                while {1} {
                    set parent [lindex ${parents} 0]
                    if {${parent} == ""} {
                        break
                    }
                    set parents [lreplace ${parents} 0 0]
                    foreach ch [winfo children ${parent}] {
                        set chclass [winfo class ${ch}]
                        if {[lsearch -exact {Text Entry} ${chclass}] != -1} {
                            #go to the next entry, if availiable
                            if {[string compare ${ch} ${origw}] == 0} {
                                continue
                            }
                            if {[${ch} cget -state] == "normal"} {
                                focus ${ch}
                                return 0
                            }
                        }\
                        elseif {[lsearch -exact {Radiobutton Checkbutton}\
                          ${chclass}] != -1} {
                            if {[${ch} cget -state] == "normal"} {
                                focus ${ch}
                                ${ch} invoke
                                return 0
                            }
                        } else {
                            #add to stack to look for.
                            lappend parents ${ch}
                        }
                    }
                }
            }\
            elseif {${class} == "Button"} {
                if {[${w} cget -state] == "normal"} {
                    ${w} invoke
                    return 0
                }
            }\
            elseif {[lsearch -exact {Radiobutton Checkbutton} ${class}] != -1} {
                if {[${w} cget -state] == "normal"} {
                    focus ${w}
                    ${w} invoke
                    return 0
                }
            } else {
                if {[${w} cget -state] == "normal"} {
                    focus ${w}
                    return 0
                }
            }
        }
    }
    return 1
}

proc sn_init_keybindings {} {
    global sn_options

    #keybindings for new windows
    set all all

    #be sure that only F-key is pressed, not Shift/Contro/Alt-F-key
    bind ${all} <F4> {
							if {%s == 0} {
								MultiWindow&::windows_new_symbr
								break
							}
						}

    bind ${all} <F5> {
							if {%s == 0} {
								MultiWindow&::windows_new_window
								break
							}
						}

    bind ${all} <F6> {
							if {%s == 0} {
								MultiWindow&::windows_new_window {} ctree
								break
							}
						}

    bind ${all} <F7> {
							if {%s == 0} {
								MultiWindow&::windows_new_window {} classbr
								break
							}
						}

    bind ${all} <F8> {
							if {%s == 0} {
								MultiWindow&::windows_new_window {} xref
								break
							}
						}

    bind ${all} <F9> {
							if {%s == 0} {
								MultiWindow&::windows_new_window {} incbr
								break
							}
						}

    bind ${all} <F2> {
				if {%s == 0} {
					MultiWindow&::windows_new_window {} retr
					break
				}
			}

    #keybindings for adding views
    bind ${all} <Control-F4> "del_last_view; break"

    bind ${all} <Control-F5> "add_view edit; break"

    bind ${all} <Control-F6> "add_view ctree; break"

    bind ${all} <Control-F7> "add_view classbr; break"

    bind ${all} <Control-F8> "add_view xref; break"

    bind ${all} <Control-F9> "add_view incbr; break"

    bind ${all} <Control-F10> "add_view build; break"

    #save all binding
    bind ${all} <$sn_options(sys,alt-traverse)-a> "Editor&::SaveAll; break"
    bind ${all} <$sn_options(sys,alt-traverse)-A> [bind ${all}\
      <$sn_options(sys,alt-traverse)-a>]

    #print
    bind ${all} <Control-p> "main_print %W; break"

    #close window
    bind ${all} <Control-w> "close_main_window %W; break"

    #exit SN
    bind ${all} <Control-q> "sn_exit; break"

    #goto error
    bind ${all} <Shift-Control-E> "MultiWindow&::search_gotoerror; break"

    #toggle between views
    bind ${all} <Control-Tab> "switch_to_next_view %W; break"
    #bind ${all} <Shift-Control-Tab> "?"

    # Prior -> PgUp, Next -> PgDn
    bind ${all} <Control-Prior> "switch_tab %W prev; break"
    bind ${all} <Control-Next> "switch_tab %W next; break"

    bind ${all} <Control-F2> "dbg_start; break"
}


