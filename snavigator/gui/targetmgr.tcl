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
################################################
##
## Target Manager (Build Manager) Dialog.
## Copyright (c) 1999 Cygnus Solutions, Inc.
## Written by Ian Roxborough <irox@cygnus.com>
##
################################################

itcl::class snIdeTargetMgr {
    inherit sourcenav::Window

    protected variable creating_new_target 0

    # new target flag.
    protected variable last_selected_item ""


    constructor {{args ""}} {
        CreateDialog
        $this title [get_indep String IDETargetMgrTitle]
        $this take_focus
    }

    public method CreateDialog {} {

        itk_component add btlabel {
            label $itk_component(hull).btlabel \
                -text [get_indep String IDETMBuildTargets] \
                -under [get_indep Pos IDETMBuildTargets]
        }

        itk_component add lbframe {
            label $itk_component(hull).lbframe \
                -relief sunken -borderwidth 2
        }

# FIXME: I can select the empty line after the last build rule.
# This needs to be fixed!
        itk_component add tmlist {
            listbox $itk_component(lbframe).listbox \
                -width 40 \
                -relief flat \
                -yscrollcommand [list $itk_component(lbframe).scrolly set]
        }

        itk_component add scrolly {
            scrollbar $itk_component(lbframe).scrolly \
                -orient vertical \
                -command [list $itk_component(tmlist) yview]
        }

	itk_component add create {
            button $itk_component(hull).create \
                -width 11 \
                -command "${this} create_cb" \
                -text [get_indep String IDETMCreateTarget] \
                -under [get_indep Pos IDETMCreateTarget]
        }

	itk_component add rename {
            button $itk_component(hull).rename \
                -width 11 \
                -command "${this} rename_target_cb" \
                -text  [get_indep String IDETMRenameTarget] \
                -under [get_indep Pos IDETMRenameTarget]
        }

	itk_component add edit {
            button $itk_component(hull).edit \
                -width 11 \
                -command "${this} edit_target" \
                -text  [get_indep String IDETMEditTarget] \
                -under [get_indep Pos IDETMEditTarget]
        }

# FIXME: If you select a rule and then press duplicate and
# then triple click in the entry, you will still be able to
# press the duplicate button (which will generate an error)

	itk_component add dup {
            button $itk_component(hull).dup \
                -width 11 \
                -command "${this} copy_target_cb" \
                -text  [get_indep String IDETMCopyTarget] \
                -under [get_indep Pos IDETMCopyTarget]
        }

	itk_component add del {
            button $itk_component(hull).del \
                -width 11 \
                -command "${this} delete_target_cb" \
                -text  [get_indep String IDETMDeleteTarget] \
                -under [get_indep Pos IDETMDeleteTarget]
        }

	itk_component add done {
            button $itk_component(hull).done \
                -width 11 \
                -command "${this} done_cb" \
                -text  [get_indep String IDETargetMgrDone] \
                -under [get_indep Pos IDETargetMgrDone]
        }

	itk_component add entry {
            entry $itk_component(hull).entry \
        }


        # Grid the frame that will contain the listbox and
        # the scrollbar

	grid $itk_component(tmlist) -row 0 -column 0 -sticky news
        grid $itk_component(scrolly) -row 0 -column 1 -sticky ns

        grid rowconfigure $itk_component(lbframe) 0 -weight 1
        grid columnconfigure $itk_component(lbframe) 0 -weight 1

        # Grid the rest of the widgets into the parent frame

        grid $itk_component(btlabel) -row 0 -column 0 -sticky w \
            -padx 10 -pady 5
        grid $itk_component(lbframe) -row 1 -column 0 -rowspan 6 \
            -sticky news -padx 10

        grid $itk_component(create) -row 1 -column 1
        grid $itk_component(rename) -row 2 -column 1
        grid $itk_component(edit)   -row 3 -column 1
        grid $itk_component(dup)    -row 4 -column 1
        grid $itk_component(del)    -row 5 -column 1


        grid $itk_component(entry) -row 7 -column 0 \
            -sticky ew -padx 10
        grid $itk_component(done)  -row 7 -column 1

        grid columnconfigure $itk_component(hull) 0 -weight 1
        grid rowconfigure $itk_component(hull) 6 -weight 1

        # Get a list of targets
        set targets [GetTargetsList]

        foreach target ${targets} {
            $itk_component(tmlist) insert end ${target}
            sn_log "IDE_DEBUG: target = <${target}>"
        }

        set listcmdsel "${this} target_selected \[$itk_component(tmlist) curselection\]"
        set listcmdaction "${this} edit_target"

        # This should bing to a listbox select not a button click!
        bind $itk_component(tmlist) <ButtonRelease-1> \
            "${this} target_selected \[$itk_component(tmlist) curselection\]"


        bind $itk_component(tmlist) <Double-1> "${this} edit_target"

# FIXME: Can we bind this to the class instead of the instance?

        bind $itk_component(hull) <Escape> "$itk_component(done) invoke"
        bind $itk_component(entry) <KeyRelease> "${this} EntryBoxChanged %K"


        $this protocol WM_DELETE_WINDOW "$itk_component(done) invoke"

        HandleState
    }

    public method UpdateTargetList {} {
        # Delete old targets
        $itk_component(tmlist) delete 0 end

        # Get a list of targets
        set targets [GetTargetsList]

        foreach target ${targets} {
            $itk_component(tmlist) insert end ${target}
            sn_log "IDE_DEBUG: target = <${target}>"
        }
    }

    #TODO: This method has been earmark to be removed.
    public method new_target {} {
        set creating_new_target 1

        # Now the user can only:
        # press Delete to cancel creation
        # or press Set to create target.

        $itk_component(dup) configure -state disabled
        $itk_component(create) configure -state disabled
        $itk_component(edit) configure -state disabled
        $itk_component(del) configure -state normal
        $itk_component(done) configure -state disabled

        $itk_component(tmlist) insert end "new_target"
        $itk_component(tmlist) selection clear 0 end
        $itk_component(tmlist) selection set end
        target_selected [$itk_component(tmlist) curselection]
    }

    public method target_selected {{itemnum ""}} {

        if {${itemnum} != ""} {
            set last_selected_item ${itemnum}
            set item [$itk_component(tmlist) get ${itemnum}]
            $itk_component(entry) delete 0 end
            $itk_component(entry) insert end ${item}
        }
        HandleState
    }

    public method EntryBoxChanged {{keypress ""}} {

        # If the user is hitting backspace instead of
        # typing, they probably don't want us to start
        # selecting targets for them.  Unless nothing is
        # selected then we should try to select something.

        set list_value [$itk_component(tmlist) curselection]

        if {${keypress}=="BackSpace" && ${list_value}!=""} {

            # Just update the button state
            HandleState

            return
        }

        set entry_value [$itk_component(entry) get]

        # Check to see if new Entry box value
        # matches any list box values.
        # Why? so it can be selected from the list

        set matched 0

        set itemnum -1

        foreach item [$itk_component(tmlist) get 0 end] {
            incr itemnum
            if {${item} == ${entry_value}} {
                set matched 1
                break

            }
        }

        if {${matched}} {
            $itk_component(tmlist) selection clear 0 end
            $itk_component(tmlist) selection set ${itemnum}
        }

        HandleState
    }

    #TODO: This method has been earmarked to be removed.
    public method set_cb {} {
        #set stuff.

        #get new name
        set new_name [$itk_component(entry) get]

        #TODO: check that the new name has
        #not already be given to a target.

        #set new name
        set item ${last_selected_item}
        $itk_component(tmlist) delete ${item}
        $itk_component(tmlist) insert ${item} ${new_name}
        $itk_component(tmlist) selection set ${item}

        if {${creating_new_target} == 1} {
            #do create a target...
            set new_name [$itk_component(entry) get]

        } else {
            #do rename target.
        }

        #clear entry
        $itk_component(entry) delete 0 end

        if {${creating_new_target} == 1} {
            # enable the other controls
            $itk_component(dup) configure -state normal
            $itk_component(create) configure -state normal
            $itk_component(edit) configure -state normal
            $itk_component(del) configure -state normal
            $itk_component(done) configure -state normal

            #TODO: call target editor.

            set creating_new_target 0
        }

    }

    common editingtarget 0
    public method edit_target {} {

	if {$editingtarget == 1} {
	    # Target editor in use.
	    return
	}

	# Mark editing in use.
	set editingtarget 1

        # Get the targets name
        set target_name [$itk_component(entry) get]

        if {${target_name}==""} {
	    # No target selected.
            bell -displayof $itk_component(hull)

	    # Mark editing not in use.
	    set editingtarget 0
            return
        }

	if {[itcl::find objects -class snEditTarget .snte] != ".snte"} {
	    # Invoke target editor
	    snEditTarget .snte ${target_name}
	    
	    .snte transient $itk_component(hull)

	    .snte activate

	    itcl::delete object .snte

	    # Mark editing not in use.
	    set editingtarget 0

	    UpdateBuildWindows
	} else {
	    # We shouldn't end up here.
	    # But just incase.
	    return
	}
    }


    public method HandleState {{selected ""} {entered ""}} {

        # Get Listbox selection
        set selection [$itk_component(tmlist) curselect]
        if {${selected} == "" && ${selection} != ""} {
            set selected [$itk_component(tmlist) get ${selection}]
        }

        # Get Text Widget contents.
        if {${entered} == ""} {
            set entered [$itk_component(entry) get]
        }

        # Nothing entered, nothing selected.
        if {${selected} == "" && ${entered} == ""} {
            $itk_component(dup) configure -state disabled
            $itk_component(create) configure -state disabled
            $itk_component(edit) configure -state disabled
            $itk_component(del) configure -state disabled
            $itk_component(rename) configure -state disabled

            $itk_component(done) configure -state normal
            $itk_component(done) configure -default active
            bind $itk_component(hull) <Return> "$itk_component(done) invoke"
            $itk_component(edit) configure -default normal
            $itk_component(create) configure -default normal
            $itk_component(rename) configure -default normal
            return
        }

        # Something entered, nothing selected.
        if {${selected} == "" && ${entered} != ""} {
            # Only "Done" and "Create*" active
            $itk_component(dup) configure -state disabled
            $itk_component(create) configure -state normal
            $itk_component(edit) configure -state disabled
            $itk_component(del) configure -state disabled
            $itk_component(rename) configure -state disabled
            $itk_component(done) configure -state normal
            $itk_component(create) configure -default active
            bind $itk_component(hull) <Return> "$itk_component(create) invoke"
            $itk_component(edit) configure -default normal
            $itk_component(done) configure -default normal
            $itk_component(rename) configure -default normal
            return
        }

        # Something entered, something selected (matching)
        if {${entered} != "" && ${selected} != "" && ${selected} == ${entered}} {
            #"Done", "Edit*", "Duplicate", "Delete" and "Done" active
            $itk_component(dup) configure -state normal
            $itk_component(create) configure -state disabled
            $itk_component(rename) configure -state disabled
            $itk_component(edit) configure -state normal
            $itk_component(del) configure -state normal
            $itk_component(done) configure -state normal
            $itk_component(edit) configure -default active
            bind $itk_component(hull) <Return> "$itk_component(edit) invoke"
            $itk_component(done) configure -default normal
            $itk_component(create) configure -default normal
            $itk_component(rename) configure -default normal
            return
        }

        # Something entered, something seleted (not matching)
        if {${entered} != "" && ${selected} != "" && ${selected} != ${entered}} {
            #"Done", "Create*" and "Rename" active.
            $itk_component(dup) configure -state disabled
            $itk_component(create) configure -state normal
            $itk_component(rename) configure -state normal
            $itk_component(edit) configure -state disabled
            $itk_component(del) configure -state disabled
            $itk_component(done) configure -state normal
            $itk_component(create) configure -default active
            bind $itk_component(hull) <Return> "$itk_component(create) invoke"
            $itk_component(done) configure -default normal
            $itk_component(edit) configure -default normal
            $itk_component(rename) configure -default normal
            return
        }
    }

    public method create_cb {} {
        set new_name [$itk_component(entry) get]

        CreateTarget ${new_name}

        $itk_component(tmlist) insert end "${new_name}"
        $itk_component(tmlist) selection clear 0 end
        $itk_component(tmlist) selection set end

        target_selected [$itk_component(tmlist) curselection]

        snEditTarget .new_target ${new_name}
        .new_target activate
        itcl::delete object .new_target
        EntryBoxChanged
        UpdateBuildWindows
    }

    public method done_cb {} {
        itcl::delete object ${this}
    }

    public method copy_target_cb {} {
        set selected [$itk_component(tmlist) curselection]
        set target_name [$itk_component(tmlist) get ${selected}]
        set new_name [CopyTarget ${target_name}]
        UpdateTargetList
        $itk_component(entry) delete 0 end
        $itk_component(entry) insert end ${new_name}
        EntryBoxChanged
    }

    public method rename_target_cb {} {
        set selected [$itk_component(tmlist) curselection]
        set old_name [$itk_component(tmlist) get ${selected}]

        set new_name [$itk_component(entry) get]

        RenameTarget ${old_name} ${new_name}
        UpdateTargetList
        EntryBoxChanged
    }

    public method delete_target_cb {} {
        set dead_name [$itk_component(entry) get]
        DeleteTarget ${dead_name}
        UpdateTargetList
        $itk_component(entry) delete 0 end
        EntryBoxChanged
    }

    public method UpdateBuildWindows {} {
        # FIXME : This is a really nasty hack!
        foreach buildwindow [itcl::find objects -class Make] {
            ${buildwindow} RefreshTargetInfo
        }
    }
}

# Get list for targets from the db.
proc GetTargetsList {} {
    set targets_list [paf_db_proj get -key snide_targets]

    return ${targets_list}
}


# Create a build target
proc CreateTarget {target_name} {

    set targets [GetTargetsList]
    sn_log "IDE_DEBUG(proc CreateTarget): targets = <${targets}>"

    lappend targets ${target_name}
    sn_log "IDE_DEBUG(proc CreateTarget): targets = <${targets}>"

    #TODO: check target_name is unique

    sn_log "IDE_DEBUG: Creating build <${target_name}>"

    paf_db_proj put snide_targets ${targets}

    paf_db_proj sync
}

proc RenameTarget {old_target_name target_name} {
    set t_obj [snBuildTarget .oldone]
    ${t_obj} LoadData ${old_target_name}
    CreateTarget ${target_name}
    ${t_obj} SaveData ${target_name}
    itcl::delete object ${t_obj}

    DeleteTarget ${old_target_name}
}

proc DeleteTarget {target_name} {
    set del_target [snBuildTarget .targ_obj]
    ${del_target} DeleteData ${target_name}
    itcl::delete object ${del_target}
    DeleteTargetName ${target_name}
}

proc DeleteTargetName {target_name} {

    set targets [GetTargetsList]

    # Find it.
    set kill [lsearch ${targets} ${target_name}]

    # Remove it.
    set targets [lreplace ${targets} ${kill} ${kill}]

    # Save the rest. 
    paf_db_proj put snide_targets ${targets}
    paf_db_proj sync
}

proc CopyTarget {target_name} {

    set targets [GetTargetsList]

    # Get a unquie name
    set copy "Copy"
    set num ""
    while {[lsearch ${targets} "${target_name} ${copy}${num}"] != -1} {
        if {${num} == ""} {
            set num 1
        } else {
            incr num
        }
    }

    # Copy it.
    set t_obj [snBuildTarget .tcopy]
    ${t_obj} LoadData ${target_name}
    CreateTarget "${target_name} ${copy}${num}"
    ${t_obj} SaveData "${target_name} ${copy}${num}"
    itcl::delete object ${t_obj}

    return "${target_name} ${copy}${num}"
}

