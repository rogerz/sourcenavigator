# This command will allow you to pick one item out of
# a list of items. A listbox widget will be displayed
# a the user is expected to select an item out of the
# list. The list elements are queried by calling the
# proc argument, it should be a command that returns
# a list of things. There is one special case, if
# the proc command only returns one thing, that
# one things will be picked automatically. The value
# that the user selects will be saved in the variable
# named by the var argument (in the global scope).

proc pick { var proc } {
    global picked
    set picked {}

    # We can't recover from the case where there are no items
    # to pick from, so we need to just wait until there is
    # something to pick in the list

    for {set things [$proc]} {$things == {}} {set things [$proc]} {
        sn_log "waiting until we have something to pick from"
        after 5000 "set picked {}"
        vwait picked
    }
    set num_things [llength $things]

    if {$num_things == 0} {
        error "num_things can not be zero"
    } elseif {$num_things == 1} {
	set picked $things
    } else {
	# We can only pick one thing at a time, just ignore 2nd request
	if {[winfo exists .pick]} {
	    return
	}

	set ptop [toplevel .pick]
	wm title $ptop "Pick $var"

	set lb [listbox $ptop.listbox -exportselection n]
	set cmd "
             set sel \[$lb curselection\]
             if {\$sel != {}} {
                 set picked \[lindex \[$lb get \$sel\] 0\]
             }"

	bind $lb <Double-1> $cmd
	set ok [button $ptop.ok -text Ok -command $cmd]
	pack $lb -fill both -expand true
	pack $ok -side bottom
	raise $ptop

        set old_things {}

	while {$picked == {}} {
	    set new_things [$proc]
	    if {$new_things != $old_things} {
		sn_log "new things appeared while waiting for selection in pick"
                if {[llength $new_things] == 1} {
                    set picked $new_things
                    break
                }
		set old_things $new_things
		$lb delete 0 end
		foreach new_thing $new_things {
		    $lb insert end $new_thing
		}
	    }
	    pause 1000
	}
	destroy $ptop
    }

    sn_log "picked thing $picked"

    # Set the variable the user passed in (in the global context)
    global $var
    set $var $picked
}

# Test the pick impl
#proc ret { } {return [list 1 2 3]}
#pick foo ret





# Create the main toplevel for this applicaiton

proc create_scenario { } {
    global scenario ctext cwidget

    # If they press the button twice, just ignore it
    if {[winfo exists .scenario]} {
        return
    }

    set scenario [toplevel .scenario]
    wm title $scenario "Scenario Builder"

    update
    lower $scenario
    grid rowconfigure $scenario 0 -weight 1
    grid rowconfigure $scenario 1 -weight 0
    grid rowconfigure $scenario 2 -weight 2
    grid columnconfigure $scenario 0 -weight 1
    grid columnconfigure $scenario 1 -weight 1
    grid columnconfigure $scenario 2 -weight 1

    set console [frame $scenario.console]
    set clab  [label $console.label -text "Sandbox Console"]
    set ctext [text $console.text -width 20 -height 10]
    set csend [button $console.eval -text "Evaluate" -command console_eval]
    set cclear [button $console.clear -text Clear -command console_clear]
    set sscroll [scrollbar $console.scroll -command "$ctext yview"]
    $ctext configure -yscrollcommand [list $sscroll set]

    bind $ctext <Control-s> [list $csend invoke]

    grid $clab  -row 0 -column 0 -sticky w
    grid $ctext -row 1 -column 0 -columnspan 2 -sticky news
    grid $csend -row 2 -column 0
    grid $cclear -row 2 -column 1
    grid $sscroll -row 1 -column 3 -sticky ns

    grid rowconfig $console 1 -weight 1
    grid columnconfig $console 0 -weight 1
    grid columnconfig $console 1 -weight 1

    set cwidget [entry $scenario.widget]
    grid $cwidget -row 1 -column 0 -columnspan 3 -sticky ew

    grid $console -row 2 -column 0 -columnspan 3 -sticky news


    create_toplevel_monitor
    create_widget_monitor
    create_operation_monitor

    raise $scenario
}

proc console_clear { } {
    global ctext
    $ctext delete 0.0 end
}

proc console_eval { } {
    global ctext

    # Look for a line like </RESULT>
    # only use the text that appears after
    # the last result line

    set buffer [$ctext get 0.0 end]
    if {[string trim $buffer] == ""} {
      return
    }
    set lines [split $buffer \n]
    set num_lines [llength $lines]
    for {set i $num_lines} {$i >= 0} {incr i -1} {
        set line [lindex $lines $i]
        if {$line == "</RESULT>"} {
            set buffer [join [lrange $lines [expr {$i + 1}] end] \n]
            break
        }
    }

    $ctext insert end \n
    set result [sandbox_eval $buffer]
    $ctext insert end <RESULT>\n${result}\n</RESULT>\n
    $ctext see end
}






# Create the listing of toplevel windows in the scenario window

proc create_toplevel_monitor { } {
    global scenario tlist

    # Listing of toplevels

    set tmon [frame $scenario.tmon]
    set tlab [label $tmon.label -text toplevels]
    set tlist [listbox $tmon.list -exportselection n]
    pack $tlab -side top -anchor w
    pack $tlist -fill both -expand true

    grid $tmon -row 0 -column 0 -sticky news

    bind $tlist <Button-1> {select_toplevel [%W get @%x,%y]}
}

proc select_toplevel { toplevel } {
    global scenario

    list_operations Toplevel
    set_selected_widget $toplevel
    send_buffer "raise $toplevel"
    raise $::scenario
    update
    monitor_widgets [send_buffer "kids $toplevel"]
}

# refresh the GUI display of toplevel windows in the
# remote interp

# FIXME: When the list of toplevels changes, we need to
# clear out the selected widget entry!

proc refresh_gui_toplevel_list { toplevels } {
    global tlist

    sandbox_toplevel_list_changed_callback $toplevels

    $tlist delete 0 end
    foreach toplevel $toplevels {
	$tlist insert end $toplevel
    }

    clear_selected_widget
    clear_widgets
    clear_operations
}

# We maintain a non-GUI list of toplevels that are
# visible in the remote interp. A callback based
# system will inform the GUI when there is a change.
# The toplevel list is stored in the snscenario interp.

proc refresh_toplevel_list { } {
    global toplevel_list toplevel_callback toplevel_after

    if {! [info exists toplevel_list]} {
        set toplevel_list {}
    }

    set tmp [send_buffer toplevels]

    if {$tmp != $toplevel_list} {
        sn_log "change in toplevel list from \{$toplevel_list\} to \{$tmp\}"
        set toplevel_list $tmp

        if {[info exists toplevel_callback] && \
                $toplevel_callback != {}} {
            sn_log "invoking toplevel callback \"$toplevel_callback\""
	    eval {$toplevel_callback} [list $toplevel_list]
	}
    }

# FIXME: put this back to 5000 when done!
    set toplevel_after [after 20000 refresh_toplevel_list]
}

# Set the command to invoke when the list of toplevels
# changes in the remote interp

proc set_toplevel_callback { cmd } {
    global toplevel_list toplevel_callback toplevel_after

    set toplevel_callback $cmd

    # When the toplevel_callback is changed, we need to
    # fire off an update event right away.

    after cancel $toplevel_after
# FIXME: This will break if we try to keep track of changed tops!
    set toplevel_list {}
    refresh_toplevel_list
}






# Listing of contained widgets inside the given toplevel

proc create_widget_monitor { } {
    global scenario wlist

    set wmon [frame $scenario.wmon]
    set wlab [label $wmon.label -text widgets]
    set wlist [listbox $wmon.list -exportselection n]
    pack $wlab -side top -anchor w
    pack $wlist -fill both -expand true

    grid $wmon -row 0 -column 1 -sticky news

    bind $wlist <Button-1> {widget_selected [%W index @%x,%y]}
}

proc clear_widgets { } {
    global wlist

    $wlist delete 0 end
}

proc monitor_widgets { wlist } {
    global index_to_widget_name_map
    catch {unset index_to_widget_name_map}

    set contents [list]
    set listIndex 0

    foreach w $wlist {
        set cls [send_buffer "winfo class $w"]
        set tail [lindex [split $w .] end]

        switch $cls {
            Entry -
            Button -
            Listbox -
	    TreeTable {
                set index_to_widget_name_map($listIndex) $w
                incr listIndex
                lappend contents [list $cls $tail]
	    }
            default {
                sn_log "skipped unknown $w \"$cls\""
            }
	}
    }

    clear_widgets
    foreach l $contents {
	$::wlist insert end $l
    }
}

proc widget_selected { index } {
    global index_to_widget_name_map

    set widget $index_to_widget_name_map($index)
    foreach {class name} [$::wlist get $index] break

    list_operations $class

    set_selected_widget $widget

}


# This command will set the context of the widget
# entry. This is the widget that displays the
# full path name of a widget in the GUI

proc set_selected_widget { wname } {
    global cwidget

    $cwidget delete 0 end
    $cwidget insert 0 $wname
    $cwidget selection from 0
    $cwidget selection to end
    $cwidget xview end

    sn_log "set_selected_widget $wname"
}

proc clear_selected_widget { } {
    global cwidget

    $cwidget delete 0 end
}

proc get_selected_widget { } {
    global cwidget
    $cwidget get
}


# Operations available on a given widget


proc create_operation_monitor { } {
    global scenario olist

    set ops [frame $scenario.ops]
    set olab [label $ops.label -text operations]
    set olist [listbox $ops.list -exportselection n]
    pack $olab -side top -anchor w
    pack $olist -fill both -expand true

    grid $ops -row 0 -column 2 -sticky news

    bind $olist <Button-1> {do_operation [%W get @%x,%y]}
}

proc clear_operations { } {
    global olist
    $olist delete 0 end
}

proc list_operations { class } {
    global olist

    set ops [list]

    switch $class {
	Button  {
	    lappend ops "Button Press"
	    lappend ops "Async Button Press"
	}
	Entry {
	    lappend ops "Enter Text"
	}
	TreeTable -
	Listbox {
	    lappend ops "Click Index"
            lappend ops "Double Click Index"
	}
        Toplevel {
            lappend ops "Destroy"
            lappend ops "Raise"
        }
	default {
	    error "unknown class $class"
	}
    }

    $olist delete 0 end
    foreach op $ops {
	$olist insert end $op
    }
}

proc do_operation { opname } {
    global ctext

    if {$opname == {}} {
	return
    }

    set widget [get_selected_widget]

    sn_log "now to do \"$opname\" to $widget"

    switch $opname {
        "Button Press" {
            $ctext insert end "mouse_click $widget"
        }
        "Async Button Press" {
            $ctext insert end "async mouse_click $widget"
        }
        "Click Index" {
            set index [get_input "Enter index" "Index:" 1]

            # convert index into x y coords

            $ctext insert end "mouse_click $widget"
        }
        "Double Click Index" {
            set index [get_input "Enter index" "Index:" 1]

            # convert index into x y coords

            $ctext insert end "mouse_double_click $widget"
        }
	"Enter Text" {
            set input [get_input "Entry Text" "Input:"]

            $ctext insert end "enter_text $widget \"$input\""
	}
        "Destroy" {
            $ctext insert end "destroy $widget"
        }
        "Raise" {
            $ctext insert end "raise $widget"
        }
    }

    $ctext insert end \n

    clear_operations
}


proc get_input { title label {default {}} } {
    global input

    set t [toplevel .index]
    wm title $t $title
    label $t.lab -text $label
    entry $t.ent
    if {$default != {}} {
        $t.ent insert 0 $default
    }
    button $t.done -text Ok -command "set input \[$t.ent get\]"

    grid $t.lab -row 0 -column 0
    grid $t.ent -row 0 -column 1
    grid $t.done -row 1 -column 0 -columnspan 2

    sn_log "waiting for input"
    vwait input
    sn_log "got input \"$input\""
    destroy $t

    return $input
}





proc pick_file { } {
    global runner_file

    # FIXME: This pattern thing is broken!

    set types {
	{{Source-Navigator Scenario}       {.sns}}
    }

    set fname [tk_getOpenFile -title "Choose a Scenario file" \
		   -filetypes $types \
                   -initialdir [file dirname $runner_file]]

    if {$fname == ""} {
        return
    }

    set runner_file $fname
    sn_log "picked new runner file \"$runner_file\""
}


# Create the toplevel "scenario runner" window where a
# user can select a file to be sourced

proc create_scenario_runner { } {
    global runner rtext runner_file

    # If they press the button twice, just ignore it
    if {[winfo exists .runner]} {
        return
    }

    set runner [toplevel .runner]
    wm title $runner "Scenario Runner"

    set rfile [entry $runner.file -textvariable runner_file]
    set rfilebutton [button $runner.filebutton -text ... \
        -command pick_file]

    set extra [frame $runner.extra]
    set rrunbutton [button $extra.runbutton -text Run \
        -command run_scenario]
    bind $runner <Return> [list $rrunbutton invoke]

    set delay_label [label $extra.delaylab -text "RPC delay :"]
    set delay_entry [entry $extra.delayentry -textvariable sandbox_delay]

    grid $rrunbutton -row 0 -column 0
    grid $delay_label -row 0 -column 1 -pad 10
    grid $delay_entry -row 0 -column 2

    set rtext [text $runner.text -width 80 -height 40]
    set rscroll [scrollbar $runner.scroll -command "$rtext yview"]
    $rtext configure -yscrollcommand [list $rscroll set]

    grid $rfile -row 0 -column 0 -sticky ew -pad 5
    grid $rfilebutton -row 0 -column 1 -columnspan 2 -sticky ew

    grid $extra -row 1 -column 0 -sticky w

    grid $rtext -row 2 -column 0 -columnspan 2 -sticky news
    grid $rscroll -row 2 -column 2 -sticky ns

    grid rowconfigure $runner 2 -weight 1
    grid columnconfigure $runner 0 -weight 1
    #grid columnconfigure $runner 1 -weight 1
}

proc run_scenario { } {
    global runner_file rtext

    if {! [file exists $runner_file]} {
        $rtext insert end "$runner_file does not exists!"
        return
    }

    $rtext insert end "Runing $runner_file\n\n"
    update

    set fd [open $runner_file r]
    set buffer [read $fd]
    close $fd

    sandbox_eval [list cd [file dirname $runner_file]]
    if {[catch {sandbox_eval $buffer} result]} {
        $rtext insert end "ERROR: ${result}\n"
        $rtext insert end $::errorInfo        
    } else {
        $rtext insert end "${result}\n"
    }
    $rtext see end
}
