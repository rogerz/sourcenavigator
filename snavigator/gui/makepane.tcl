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
########################################
##
## Make (build programs from source files)
##
########################################

# FIXME: This whole class is a bad copy of the greppane.tcl class.
# We need to figure out how to make these two use a common parent
# or how we can share the implementaitons!

itcl::class Make {

    inherit itk::Widget

    constructor {args} {
        global sn_options

        eval itk_initialize $args

        set s ${this}

        #add toolbar icons
        if {$itk_option(-toolbar) != ""} {
            set toolbarf $itk_option(-toolbar).toolbarf
            pack [frame ${toolbarf}] -side left
            #prev
            button ${toolbarf}.prev -takefocus 0 -text [get_indep String Prev]\
              -image del_image -command [itcl::code ${this} incr_selected_line -1]
            balloon_bind_info ${toolbarf}.prev [get_indep String PrevINFO]
            pack ${toolbarf}.prev -side left -fill x -expand n
            #next
            button ${toolbarf}.next -takefocus 0 -text [get_indep String Next]\
              -image add_image -command [itcl::code ${this} incr_selected_line +1]
            balloon_bind_info ${toolbarf}.next [get_indep String NextINFO]
            pack ${toolbarf}.next -side left -fill x -expand n
        }

        #Frame for command input
        set input $itk_component(hull).input
        frame ${input}
        pack ${input} -side top -padx 5 -fill x

        set entries ${input}.entries
        frame ${entries}
        pack ${entries} -fill x -expand y -side left -anchor nw -fill x

# FIXME: This widget should not have a file selection button
# on the far right side. It serves no purpose. Unfortunatly
# this is documented in man screen shots so we can not fix
# it yet.
        #command string
        Combo& ${entries}.entry \
            -labelwidth 14 \
            -anchor ne \
            -label [get_indep String MakeEntry] \
            -entryvariable ${this}-makecommand \
            -selectcommand "${this} make_select_command ${entries}.entry"
        balloon_bind_info [${entries}.entry component entry] [get_indep String\
          MakeEntryINFO]

        bind [${entries}.entry component entry] <Return> "${this} ExecMake"
        pack ${entries}.entry -side top -expand y -fill x -anchor nw

        #add choose button for the make command to evtl. choose an executable
        button ${entries}.entry.choose -text [get_indep String Choose]\
          -command [list sn_choose_file [${entries}.entry component entry]\
          $sn_options(executable_defaultext)]
        balloon_bind_info ${entries}.entry.choose [get_indep String ChooseINFO]
        pack ${entries}.entry.choose -side right -before ${entries}.entry.arrow

        #starting directory
        Combo& ${entries}.dir -labelwidth 14 -anchor ne -label\
          [get_indep String MakeStartDir] -entryvariable [itcl::scope startdir]

        bind [${entries}.dir component entry] <Return> "${this} ExecMake"
        pack ${entries}.dir -side top -expand y -fill x -anchor nw

        button ${entries}.dir.choose -text [get_indep String Choose]\
          -command [list sn_choose_dir [${entries}.dir component entry]]
        balloon_bind_info ${entries}.dir.choose [get_indep String ChooseINFO]
        pack ${entries}.dir.choose -side right -before ${entries}.dir.arrow

# FIXME : all the hard coded strings in this class need to go into the string table.
        # targets list
        Combo& ${entries}.targets \
            -labelwidth 14 \
            -anchor ne \
            -label "Build Targets" \
            -entryvariable [itcl::scope target] \
            -readonly 1 \
            -selectcommand "${this} targets_cb"

        pack ${entries}.targets -side top -expand y -fill x -anchor nw

# FIXME: put in string table (This is a problem because we check the string everywhere)
# Perhaps we should just use <Makefile>, that way it would not have the word External in it.
        # Setting the target list.
	set target "<External Makefile>"
        set targets_list [list $target]
        eval lappend targets_list [GetTargetsList]
        ${entries}.targets configure -contents ${targets_list}

        # Selecting the last target used, if any.
        if {! [catch {paf_db_proj get -key IDEMakeCurrentTarget} targ]} {
            if {![string equal $targ ""]} {
                set target $targ
            }
        }

        ${entries}.targets selecttext ${target}

        #display history for make entries
        make_add_history ${entries}.entry ${entries}.dir

        #Buttons start
        set btns ${input}.buttons
        frame ${btns}
        pack ${btns} -side right -anchor nw -fill x
        button ${btns}.start -text [get_indep String StartMake] -command "${this} ExecMake"
        balloon_bind_info ${btns}.start [get_indep String StartMakeINFO]
        pack ${btns}.start -side top -padx 8 -pady 4 -fill x

# FIXME : put in string table!
        #Button cancel, if needed
        button ${btns}.cancel -takefocus 0 -text "Stop" -command " ${this}\
          close_make cancel " -state disabled
        balloon_bind_info ${btns}.cancel [get_indep String CancelMakeINFO]
        pack ${btns}.cancel -side top -padx 8 -pady 4 -fill x

        button ${btns}.launch -takefocus 0 -text "Debug" -width 8 -command\
          " ${this} cb_launch_build "
        pack ${btns}.launch -side top -padx 8 -pady 4 -fill x

        # Results
	set browser [text $itk_component(hull).text \
	    -exportselection n \
	    -wrap word \
	    -width ${width} \
	    -height ${height}]

        ${browser} tag configure sel -background $sn_options(def,select-bg)
        Editor&::set_tab ${browser} 2

        # Lock down bindings for this widget so we only use ours
        bindtags $browser $browser
	bind $browser <ButtonPress-1> [itcl::code $this text_b1_down %W %x %y]
	bind $browser <B1-Motion> [itcl::code $this text_b1_motion %W %x %y]
        bind $browser <Double-1> [itcl::code $this text_b1_double %W %x %y]


        bind ${browser} <Return> "${this} handle_return"
        bind ${browser} <space> "${this} handle_return"
        bind ${browser} <3> {make_post_menu %W %X %Y ; break}

        pack ${browser} -side bottom -fill both -expand y

        set scroll_y [scrollbar $itk_component(hull).y \
	    -orient vertical \
	    -command "${browser} yview"]
	${browser} configure -yscrollcommand "${scroll_y} set"
        pack ${scroll_y} -side right -fill y -before ${browser}

        Update_Layout

        #set project directory to the start directory

        if {$sn_options(make-lastdir) != "" && [file isdirectory\
          $sn_options(make-lastdir)]} {
            set startdir [file nativename $sn_options(make-lastdir)]
        } else {
            set startdir [file nativename $sn_options(sys,project-dir)]
        }

        global ${this}-makecommand
        set ${this}-makecommand [file nativename\
            $sn_options(both,make-command)]

        if {${target} != "<External Makefile>"} {
            set b_target [snBuildTarget .tmptarget]
            ${b_target} LoadData ${target}
            ${entries}.dir configure -entrytext [${b_target} GetBuildDirectory]
            itcl::delete object ${b_target}

            # Disable the directory combobox so the
            # user can't edit the directory of the build target.
            ${entries}.dir.choose configure -state disabled
            ${entries}.dir configure -state disabled
        }

        SetTitle

	#call user defined function
        catch {sn_rc_make $itk_component(hull) ${browser}}

        focus [${entries}.entry component entry]
    }
    destructor {
        #to be sure that the process is terminated
        catch {close ${make_fd}}
        paf_db_proj put IDEMakeCurrentTarget ${target}
        foreach v [::info globals "${this}-*"] {
            catch {uplevel #0 unset ${v}}
        }
    }

    method handle_cancel {} {
        if {${cancelcommand} != ""} {
            ::eval ${cancelcommand}
        }
    }

    method getstartdir {} {
        return ${startdir}
    }

    method setmakecommand {cmd} {
# This needs to be an instance variable!
        upvar #0 ${this}-makecommand makecommand
        set makecommand ${cmd}
        return ${makecommand}
    }

    # Call back if the debug or Run button is pressed.	
    method cb_launch_build {} {
        set exec_error ""

        if {${target} == "<External Makefile>"} {
            # The users is building with a external Makefile.
            # We should bring up the debug dialog.
            sn_debugger
        } else {
            # We need to find out if it's going to launch with
            # or without the debugger.
            set b_target [snBuildTarget .tmptarget]
            ${b_target} LoadData ${target}
            if {[${b_target} GetLaunchWithDebugger]==1} {
                # We are launching with debugger.
                set runDir [${b_target} GetBuildDirectory]
                set execCmd [file join ${runDir}\
                  [${b_target} GetLaunchCommandLine]]
                sn_debugger ${runDir} [file join ${runDir} ${execCmd}]\
                  [${b_target} GetDebugger]
            } else {
                global tcl_platform
                # Just run it stand alone.
                set runDir [${b_target} GetBuildDirectory]
                set execCmd [${b_target} GetLaunchCommandLine]
# FIXME: all of this stuff needs to be in a more generic exec
# interface. Also, why catch and then run, why not check to see
# if the directory exists before doing all this? Known Bug cause!	
                catch {cd ${runDir}}
                if {$tcl_platform(os)=="Windows NT"} {
                    catch {exec cmd /c start ${execCmd} &} exec_error
                }\
                elseif {$tcl_platform(os)=="Windows 95"} {
                    catch {exec command /c start ${execCmd} &} exec_error
                } else {
                    set execcmd "exec ${execCmd}"
                    catch {eval ${execcmd}} exec_error
                }
                catch {cd $sn_options(sys,project-dir)}
            }
            itcl::delete object ${b_target}
        }

        if {${exec_error}!=""} {
            sn_error_dialog ${exec_error}
        }
    }

    # Call back if the selected "Build Target" is changed.
    method targets_cb {w} {
        sn_log "IDE_DEBUG(Make target_cb): target = ${target} "

        if {${target} == "<External Makefile>"} {
            # Enable the directory combobox so the
            # user can edit it.
            ${entries}.dir.choose configure -state normal
            ${entries}.dir configure -state normal

# FIXME: Put this and other strings in string table!
            # We will be launching with debugger.
            ${btns}.launch configure -text "Debug"
        } else {
            # enable the directory combobox
            # so we can edit it.
            ${entries}.dir.choose configure -state normal
            ${entries}.dir configure -state normal

            # We need to find out if it's going to launch with
            # or without the debugger.
            set b_target [snBuildTarget .tmptarget]
            ${b_target} LoadData ${target}
            if {[${b_target} GetLaunchWithDebugger]!=1} {
                # We are not using the debugger.
                ${btns}.launch configure -text "Run"
            } else {
                # We are launching with debugger.
                ${btns}.launch configure -text "Debug"
            }

            # Set the new directory in the combobox
            ${entries}.dir configure -entrytext [${b_target} GetBuildDirectory]


            # Disable the directory combobox so the
            # user can't edit the directory of the build target.
            ${entries}.dir.choose configure -state disabled
            ${entries}.dir configure -state disabled

            itcl::delete object ${b_target}
        }

    }


    # Update the targets combobox, debug/lunch and build dir.

    method RefreshTargetInfo {} {
        # get and set the targets list in the combobox.
        set targets_list [list "<External Makefile>"]
        eval lappend targets_list [GetTargetsList]
        ${entries}.targets configure -contents ${targets_list}

        if {${target} != "<External Makefile>"} {
            # enable the directory combobox
            # so we can edit it.
            ${entries}.dir.choose configure -state normal
            ${entries}.dir configure -state normal

            # We need to find out if it's going to launch with
            # or without the debugger.
            set b_target [snBuildTarget .tmptarget]
            ${b_target} LoadData ${target}
            if {[${b_target} GetLaunchWithDebugger]!=1} {
                # We are not using the debugger.
                ${btns}.launch configure -text "Run"
            } else {
                # We are launching with debugger.
                ${btns}.launch configure -text "Debug"
            }

            # Set the new directory in the combobox
            ${entries}.dir configure -entrytext [${b_target} GetBuildDirectory]


            # Disable the directory combobox so the
            # user can't edit the directory of the build target.
            ${entries}.dir.choose configure -state disabled
            ${entries}.dir configure -state disabled

            # Be clean, delete the build target after ourselfs.
            itcl::delete object ${b_target}
        }
    }

    method ExportMakefile {} {
        sn_log "IDE_DEBUG(Make target_cb): target = ${target} "
        if {${target} == "<External Makefile>"} {
            # We cannot generate a Makefile for this.
            bell
            return
        } else {
            # Get the filename and path which will be
            # used to save the Makefile.
            set filename [tk_getSaveFile]

            if {${filename}!=""} {
                # Generate the Makefile
                set mkgen [MakefileGen .mkg_save_mkfile ${target}]
                set mkfile [${mkgen} GenerateMakefile ${filename}]
                itcl::delete object .mkg_save_mkfile
            }
        }
    }

    method ExecMake {{build_action ""}} {
        global sn_options
        upvar #0 ${this}-makecommand makecommand

        #make is running
        if {${Proceed} != 0} {
            close_make
            return
        }

        #verify if all files are saved
        set modified 0
        foreach editor [itcl_info objects "*" -class Editor&] {
            if {[${editor} cget -file_changed]} {
                set modified 1
            }
        }

        #ask to save changed files before running make
        if {${modified}} {
            set answer [tk_dialog auto [get_indep String MultiMake]\
              [get_indep String WantToFastSave] error_image 0\
              [get_indep String Yes] [get_indep String No] [get_indep String\
              Cancel]]
            if {${answer} == 2} {
                return
            }
            if {${answer} == 0} {
                Editor&::SaveAll
            }
        }

        #add command and directory to history
        make_add_history ${entries}.entry ${entries}.dir

        # if user requires a make -k or make clean
        # doesn't get it from the GUI widget.
        if {${build_action}==""} {
            #read make command
            get_make_cmd make_cmd
        } else {
            set make_cmd [file nativename $sn_options(both,make-command)]
        }

        # Check to see if we are executing a
        # users Makefile or an SN build target.
        if {${target}!="<External Makefile>"} {

            # We are building an SN build target.
            set mkgen [MakefileGen .mkg ${target}]

            set mkfile [${mkgen} GenerateMakefile]
            itcl::delete object ${mkgen}
            # Only pass the last component of the
            # Makefile path as the -f argument.
            set mkfile [file tail $mkfile]
            set make_cmd "${make_cmd} -f ${mkfile}"

            # If we are building an embedded target
            # we need to generate the .runcfg file.
            global sn_elix
            set b_target [snBuildTarget .tmptarget]
            ${b_target} LoadData ${target}
            set tool_chain [GetToolChainObject [${b_target} GetToolChain]]
            if {${sn_elix} && [${tool_chain} GetIsEmbedded]} {
                global sn_options

                set prefix [file join [pwd] [${b_target} GetBuildDirectory]]/
                set fd [open ${prefix}.runcfg w]
                fconfigure ${fd} \
                    -encoding $sn_options(def,system-encoding) \
                    -blocking 0
                set cmd [Elix&::make_config_command\
                  [${tool_chain} GetShortName] ${prefix}]
                puts ${fd} ${cmd}
                close ${fd}
            }
            itcl::delete object ${b_target}
        }

        # Do we have to force rebuild
        if {${build_action}=="forced"} {
            # Everything to be rebuilt.
            set make_cmd "${make_cmd} -k"
        }\
        elseif {${build_action}=="clean"} {
            set make_cmd "${make_cmd} clean"
        } elseif {${target}!="<External Makefile>"} {
            set b_target [snBuildTarget .tmptarget]
            ${b_target} LoadData ${target}
            if {[${b_target} GetNeedsRebuild]} {
                set make_cmd "${make_cmd} clean all"
                ${b_target} SetNeedsRebuild 0
                ${b_target} SaveData
            }
            itcl::delete object ${b_target}
        }

        #on window we must mask all "\"
        regsub -all {\\} ${make_cmd} {\\\\} make_cmd

        # Bah! Tcl provides no way to read both
        # stdout and stderr from a pipe so we
        # pipe them both to cat and read that.
        lappend make_cmd |& cat

        #1. store last used make directory
        if {[file isdirectory ${startdir}]} {
            set sn_options(make-lastdir) ${startdir}

            #change to make directory
            catch {cd ${startdir}}
        }

        #2. start command
        sn_log "Executing make: ${make_cmd} in directory [pwd]"
        set ret [catch {set make_fd [open "| ${make_cmd}" r]} msg]

        #3. cd back to the project directory
        cd $sn_options(sys,project-dir)

        if {${ret}} {
            sn_error_dialog "${make_cmd}:\n${msg}"
            return
        }
        sn_log "encoding is -encoding $sn_options(def,system-encoding)"
        fconfigure ${make_fd} \
            -encoding $sn_options(def,system-encoding) \
            -blocking 0 \
            -buffering line

        fileevent ${make_fd} readable [itcl::code $this dispatch_MakeEvent]

        handle_proceed disabled
    }

    method get_make_cmd {make_cmd} {
        global sn_options
        global env tcl_platform
        upvar #0 ${this}-makecommand makecommand
        upvar ${make_cmd} cmd

        #don't return it as a list element.
        set cmd ${makecommand}
        return ${cmd}
    }

    method close_make {{cancel ""}} {
        handle_proceed normal ${cancel}
        bell
    }

    #disable/enable input fields when the process is running/terminated
    method handle_proceed {state {cancel ""}} {
        set ButtonPressed 0

        if {${state} == "normal"} {
            if {[catch {close ${make_fd}} err]} {
	        sn_log "caught make close error \"$err\""
	    }

            ${browser} configure -state normal
            if {${cancel} == "cancel"} {
# FIXME: put in string table!
                set endstr "*** Canceled ***"
            } else {
                set endstr "+++ End +++"
            }
            ${browser} insert end ${endstr}
            ${browser} configure -state disabled

            if {${toolbarf} != ""} {
                ${toolbarf}.prev configure -state normal
                ${toolbarf}.next configure -state normal
            }

            ${btns}.start configure -state normal
            ${btns}.cancel configure -state disabled
            ${btns}.launch configure -state normal

            ${entries}.entry.choose configure -state normal
            ${entries}.entry configure -state normal

            if {${target} == "<External Makefile>"} {
                ${entries}.dir.choose configure -state normal
                ${entries}.dir configure -state normal
            }
            ${entries}.targets configure -state normal

            set Proceed 0
        } else {
            # The make is now running
            set Proceed 1

            ${browser} configure -state normal
            ${browser} delete 0.0 end
            ${browser} configure -state disabled

            if {${toolbarf} != ""} {
                ${toolbarf}.prev configure -state disabled
                ${toolbarf}.next configure -state disabled
            }

            ${btns}.start configure -state disabled
            ${btns}.cancel configure -state normal
            ${btns}.launch configure -state disabled

            ${entries}.entry.choose configure -state disabled
            ${entries}.entry configure -state disabled

            ${entries}.dir.choose configure -state disabled
            ${entries}.dir configure -state disabled

            ${entries}.targets configure -state disabled
        }
    }

    method Title {{full 1}} {
        global sn_options
        set t [string trimright [get_indep String MultiMake] "."]
        set txt [${entries}.entry cget -entrytext]
        if {${txt} != ""} {
            set t "${t}: ${txt}"
        }
        if {${full}} {
            return [sn_title ${t}]
        } else {
            return ${t}
        }
    }
    method Icon {} {
        return [sn_view_icon [get_indep String MultiMake]]
    }

    method SetTitle {} {
	set top [winfo toplevel $itk_component(hull)]
	${top} configure -title [Title] -iconname [Icon]
    }

    method handle_return {} {
	set ind [lindex [${browser} tag ranges sel] 0]
	sn_log "handle_return index is $ind"
        if {$ind != ""} {
            ${browser} tag remove sel 0.0 end
            ${browser} tag add sel [list $ind linestart] [list $ind lineend + 1c]
            set res [sn_make_edit_file [${browser} get [list $ind linestart]\
              [list $ind lineend]]]
            if {${res} == "" && !${hold}} {
                itcl::delete object $this
            }
        }
        set ButtonPressed 1
    }

    # A single click will select a given line, if we have
    # an split pane goto the given symbol in the other pane

    private method handle_single_click {} {
        sn_log handle_single_click

        if {${selectcommand} != ""} {
            eval ${selectcommand} [Selection]
        }
        set ButtonPressed 1
    }

    # These methods handle our custom interaction with the
    # text widget. We use a text widget like it is a listbox
    # so we have to do our own special bindings.

    private method text_b1_down { w x y } {
        # Get the line number at the given x y position
        set line [lindex [split [$w index @$x,$y] .] 0]
        text_select_line $line
        bind $w <ButtonRelease-1> [itcl::code $this text_b1_up %W %x %y]
    }

    private method text_b1_motion { w x y } {
        # Get the line number at the given x y position
        set line [lindex [split [$w index @$x,$y] .] 0]
        text_select_line $line
    }

    private method text_b1_up { w x y } {
        sn_log "text_b1_up"
        set text_b1_really_up_after [after 500 [itcl::code $this handle_single_click]]
    }

    private method text_b1_double { w x y } {
        after cancel $text_b1_really_up_after
        bind $w <ButtonRelease-1> {}
        sn_log "makepane Text -> Button double $text_b1_current_line_num"
        $this handle_return
    }

    # Highlight the given line in the grep results

    private method text_select_line { linenum } {
        sn_log "text_select_line $linenum"

        if {$linenum == $text_b1_current_line_num} {
            return
        }

        set w $browser

        $w tag remove sel 0.0 end
        $w tag add sel $linenum.0 [list $linenum.0 lineend]
        $w see $linenum.0
        set text_b1_current_line_num $linenum
        set text_current_line [$w get $linenum.0 [list $linenum.0 lineend]]
        sn_log "new selected line_num is $text_b1_current_line_num, line is \"$text_current_line\""
    }

    # Move the selected line up or down by one element

    private method incr_selected_line { amount } {
        if {$amount != "+1" && $amount != "-1"} {
            error "incr amount must be +1 or -1, you gave \"$amount\""            
        }
        # Get last index and take last newline into account
        set lastline [lindex [split [$browser index end] .] 0]
        incr lastline -1

        set nextline [expr {$text_b1_current_line_num + $amount}]

        if {$nextline == 0} {
            # Text widget treats line 0 as line 1
            text_select_line $lastline
        } elseif {$nextline > $lastline} {
            # If they are on the last line, shoot them back to line 1
            text_select_line 1
        } else {
            text_select_line $nextline
        }
    }

    method Split_Line {} {
        global sn_options

        set rng [${browser} tag ranges sel]
        if {${rng} == ""} {
            return ""
        }
        set line [eval ${browser} get ${rng}]
        if {${line} == ""} {
            return
        }
        set pars [sn_goto_comp_error_or_grep ${line}]
        if {${pars} == ""} {
            return ""
        }
        set file [lindex ${pars} 0]
        set pos [lindex ${pars} 1]
        set file [sn_convert_FileName ${file}]

        return [list ${file} ${pos}]
    }

    method filter {{all 0}} {
    }

    method activate {} {
        if {${symbols} != "" &&(${next} == "" || [${next} whoami] != "edit")} {
            #delete old contents
            ${symbols} configure -contents ""
            ${symbols} selecttext ""
            ${symbols} configure -state disabled
        }
        if {${toolbarf} != ""} {
            pack ${toolbarf} -side left
        }
    }

    method deactivate {} {
        if {${symbols} != "" &&(${next} == "" || [${next} whoami] != "edit")} {
            ${symbols} configure -state disabled
        }
        if {${menu} != ""} {
        }
        if {${toolbarf} != ""} {
            pack forget ${toolbarf}
        }
    }

    #this function is called, when the symbols combobox
    #is called
    method postcommand {m} {
    }

    method Selection {} {
        global tcl_platform

        set sel [Split_Line]
        if {${sel} != ""} {
            set file [lindex ${sel} 0]
            set line [lindex ${sel} 1]
            if {$tcl_platform(platform) != "windows"} {
                set last [string last "/" ${file}]
                set len [string length ${file}]
                #sometimes we get strang results from\
                  'sn_goto_comp_error_or_grep'
                if {${last} != -1 && [expr ${last} + 1] == ${len} ||\
                  [string first "**" ${file}] == 0} {
                    set file ""
                    set line ""
                }
            }
        } else {
            set file ""
            set line ""
        }
        return [list "" "" "" ${file} ${line} "" "" ""]
    }

    method gotosymbol {{scope ""} {sym ""} {cls ""} {file ""} {from ""} {type\
      ""} {prm ""} {to ""} {always 1}} {
        if {${sym} != ""} {
            #convert string to be used for make
            regsub -all {\^|\\|\.|\*|\$|\-|\[|\]} ${sym} {\\&} sym
            regsub -all {\<|\>} ${sym} {[&]} sym
        }
        return 1
    }

    method clearselection {} {
        ${browser} tag remove sel 0.0 end
    }

    method Update_Layout {} {
        global sn_options

        ${browser} configure -font $sn_options(def,default-font)\
          -selectforeground $sn_options(def,select-fg)\
          -selectbackground $sn_options(def,select-bg)
    }

    method Focus {} {
        focus ${browser}
    }

    #make a title for the Dump position, this is usefull
    #for the views stack (prev/next)
    method DumpTitle {{dump ""}} {
        if {${dump} == ""} {
            set dump [Dump]
        }
        set scp [lindex ${dump} 0]
        set sym [lindex ${dump} 1]
        set cls [lindex ${dump} 2]
        set name [string trim "${sym}(${scp}) ${cls}"]
        return "Make ${name}"
    }

    method AddHistoryFromDump {dump title} {
    }

    #return the important data to restore this widget
    #in later time again (used by saving the project)
    method Dump {} {
        return [Selection]
    }

    #gets the result from the function "Dump" to
    #restore the older state (used by restoring the project)
    method Restore {str} {
        eval ${this} gotosymbol ${str}
        #Focus
    }

    method Close {{mode 0}} {
        return 1
    }

    method whoami {} {
        return make
    }
    method next {} {
        return ${next}
    }

# FIXME: This is currently broken, need to figure out where it is used.
    #if an editor is added to the make, call the
    #goto function of the editor
    method goto {combo txt} {
        if {${next} != "" && [${next} whoami] == "edit"} {

            #dump the current view into the history stack
            ${topw} history_stack_add_point ${next}

            ${next} goto ${combo} ${txt}
        }
    }

    method make_select_command {combo txt} {
        global sn_options
        global ${this}-makecommand

        set i [[${combo} component treew] search -exact -- ${txt} 0]

        if {${i} != ""} {
            set txt [lindex $sn_options(make_history_cmd) ${i}]
            set ${this}-makecommand [lindex ${txt} 0]
            set startdir [lindex ${txt} 1]
        }
    }

    #command history with directory
    proc make_history_moveup {cmdw cmddir} {
        global sn_options

        #add pattern into the history stack
        sn_add_to_histroy_stack sn_options(make_history_cmd) ${cmddir}

        #add the new history to the combo box
        set elms ""
        foreach el $sn_options(make_history_cmd) {
            lappend elms "[lindex ${el} 0] <[lindex ${el} end]>"
        }

        ${cmdw} configure -contents ${elms}
    }

    proc make_add_history {cmdw dirw} {
        global sn_options

        set cmd [${cmdw} cget -entrytext]
        set dir [${dirw} cget -entrytext]

        #command history with directory
        if {${cmd} != ""} {
            set cmddir [list ${cmd} ${dir}]
            sn_add_to_histroy_stack sn_options(make_history_cmd) ${cmddir}
        }
        #add the new history to the combo box
        set elms ""
        foreach el $sn_options(make_history_cmd) {
            lappend elms "[lindex ${el} 0] <[lindex ${el} end]>"
        }
        ${cmdw} configure -contents ${elms}

        #directory list
        if {${dir} != "" && [file isdir ${dir}]} {
            sn_add_to_histroy_stack sn_options(make_history_dir) ${dir}
        }
        ${dirw} configure -contents $sn_options(make_history_dir)
    }

    private method dispatch_MakeEvent {} {
        if {[catch {set res [gets ${make_fd} line]} err]} {
            sn_log "caught make pipe error \"$err\""
            ${this} close_make
            return
        }
        sn_log "read $res bytes, eof is [eof ${make_fd}], line is \"$line\""
        if {${res} == -1 && [string length $line] == 0 && ![eof ${make_fd}]} {
            # This means a read returned no data but the channel is
            # not at EOF, this can happen when cmd prints to stderr
            # handle this case by simply doing nothing.
            return
        } elseif {${res} > 0} {
            ${browser} configure -state normal
            ${browser} insert end ${line}\n
            ${browser} configure -state disabled
            if {! ${ButtonPressed}} {
                if {[${browser} tag ranges sel] == {}} {
	            ${browser} see end
                }
            }
        } elseif {${res} < 0} {
            ${this} close_make
        }
        update idletasks
    }

    # Variables used in implementing custom listbox like
    # bindings on the text widet
    private variable text_b1_really_up_after ""
    private variable text_b1_current_line_num -1
    private variable text_current_line

    private variable ButtonPressed 0
    private variable target
    private variable startdir ""

    protected variable make_fd ""
    protected variable Proceed 0
    protected variable entries ""
    protected variable btns ""
    protected variable toolbarf ""
    
    # Text widget that stores the lines
    # of output from the make command.
    protected variable browser ""

    #don't close window after selecting an entry
    public variable hold 1
    public variable width 80
    public variable height 10
    public variable symbols ""
    public variable symbols_filter ""

    itk_option define -toolbar toolbar ToolBar ""
    itk_option define -mesg_area mesg_area MsgArea ""
    
    public variable selectcommand ""
    public variable doubleclickcommand ""
    public variable cancelcommand ""

    #contains the next widget, when the window is spliten
    #into more than one area
    #Simulate a single chain
    public variable next ""
}

#returns the object file of a source file
proc sn_object_file {file} {
    global tcl_platform

    set i [string last "." ${file}]
    if {$tcl_platform(platform) == "windows"} {
        set obj "obj"
    } else {
        set obj "o"
    }
    #file contains no extension
    if {${i} < 1} {
        return ${file}.${obj}
    }
    return [string range ${file} 0 ${i}]${obj}
}

proc sn_make {} {
    global sn_options
    global tkeWinNumber

    set s [find_reusable_window "make-"]
    if {${s} == ""} {
        incr tkeWinNumber
        set s .sn-make-${tkeWinNumber}
        sn_create_window ${s} 1

        Make ${s}.make -toolbar ${s}.exp -mesg_area ${s}.status.msg
        pack ${s}.make -expand y -fill both -side left
        set menu [${s} cget -menu]

        set toolmenu [menu .make_tool_menu${tkeWinNumber} -tearoff 0]
# FIXME: Add to string table
        ${menu} insert 3 cascade -menu ${toolmenu} -label "Tools"
        add_make_tools_menu ${toolmenu} ${s}.make

        # FIXME : This should be converted to a non-modal Dialog.
        ${s} withdraw
        ${s} centerOnScreen
        ${s} deiconify
        ${s} raise
    } else {
        ${s} raise
    }

    return ${s}
}

proc add_make_tools_menu {tools_menu w_make} {
    ${tools_menu} insert 10 command -label "Build" -command " ${w_make}\
      ExecMake "
    ${tools_menu} insert 11 command -label "Force Build" -command " ${w_make}\
      ExecMake forced "
    ${tools_menu} insert 12 command -label "Clean Build" -command " ${w_make}\
      ExecMake clean "
    ${tools_menu} add separator
    ${tools_menu} add command -label "Export Makefile..." -command " ${w_make}\
      ExportMakefile "
}

# This method will be invoked when the user right clicks
# in the output window. This code is copied from the
# browser class. This is a really sucky way to extend
# a widget but we need to ship the product!

proc make_post_menu {w x y} {
    set m .sn_pop_menu_listbox
    # It has to be destroyed because we might have problems with "tk_popup"!
    catch {destroy ${m}}
    menu ${m} -tearoff 0 -postcommand "make_post_menu_update ${m} ${w}"
    wm overrideredirect ${m} 1
    tk_popup ${m} ${x} ${y}
}

# This will be invoked when the menu is actually ready to display

proc make_post_menu_update {m w} {
    ${m} delete 0 end

    ${m} add command -label [get_indep String SaveBufferAs] -command\
      " make_respond_to_save_as ${m} ${w} "
}

# This will be called if they choose "save as" from right click menu

proc make_respond_to_save_as {m w} {
    global sn_options

    # Get a file name to save the buffer as!
    set file [tk_getSaveFile -initialfile build.out]

    # They clicked Cancel
    if {${file} == ""} {
        return
    }

    # write the contents of the widget into the file

    if {[catch {open ${file} w} fd]} {
        sn_error_dialog "${fd}"
        return
    }

    fconfigure ${fd} -encoding $sn_options(def,system-encoding) -blocking 0

    puts ${fd} [${w} get 0.0 end]

    close ${fd}
}
