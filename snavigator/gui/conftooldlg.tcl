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
## Configure Build Rule Dialog for SN-IDE.
## Copyright (c) 1999 Cygnus Solutions, Inc.
## Written by Ian Roxborough <irox@cygnus.com>
##
################################################

itcl::class snIdeToolsDlg {

    inherit sourcenav::Dialog

    method test {} {
        if {[info objects ${toolchain}]!=""} {
            # It's ok.
        } else {
            # it's not ok.
            error "Tool chain ${toolchain} does not exist"
        }
    }

    # The dialog window path.
    protected variable dlgwin ""

    protected variable focus ""
    #Last focus before create dialog.

    protected variable okbutt ""
    # The OK button.

    protected variable debugLookUp ;# Array to lookup string tags
    protected variable optimizeLookUp ;# Array ...
    protected variable warnLookUp ;# Array ...
    protected variable codeLookUp ;# Array ...

    # flags widgets:
    protected variable debugLBX ""
    protected variable optimizeLBX ""
    protected variable warnLBX ""
    protected variable codeLBX ""
    protected variable exeLEB ""
    protected variable uflagsENT ""
    protected variable cmdlTXT ""

    # includes widgets:
    protected variable AutoGenList ""
    protected variable UserGenList ""

    # define buttons/widgets
    protected variable DefNewButt ""
    protected variable DefDelButt ""
    protected variable DefSetButt ""
    protected variable UserDefsList ""
    protected variable EditDef ""

    itk_option define -toolchain toolChain ToolChain ""
    itk_option define -rule rule Rule ""
    itk_option define -buildtarget buildTarget BuildTarget ""

    protected variable editdefbox ""
    constructor args {

	eval itk_initialize $args

	$this configure -modality application

        #TODO: check tool_chain, rule and build_target are valid

	CreateDialog
    }

    method CreateDialog {} {
        global tkPriv
        #TODO: CheckToolChain
        #TODO: CheckRuleType

        set finfo [$itk_option(-toolchain) GetFileInfo $itk_option(-rule)]
        $this configure -title \
		"[get_indep String IDEBuildRuleTitle] \[${finfo}\]"

        #create notebook widget
        set notebook [tixNoteBook $itk_component(hull).nb]

        #Add a leaf for Settings/flags.
        set flagsleaf [${notebook} add flags -label [get_indep String\
          IDEBRSettingsTab] -under [get_indep Pos IDEBRSettingsTab]]
        FlagPage ${flagsleaf}

        #Add a leaf for Incudes Files.
        set includesleaf [${notebook} add includes -label [get_indep String\
          IDEBRIncludesTab] -under [get_indep Pos IDEBRIncludesTab]]
        IncludesPage ${includesleaf}

        #Add a leaf for macro defines.
        set definesleaf [${notebook} add defines -label [get_indep String\
          IDEBRDefinesTab] -under [get_indep Pos IDEBRDefinesTab]]
        DefinesPage ${definesleaf}

        pack ${notebook} -pady 6 -padx 6

        # Add OK and Cancel buttons.
        set okbutt [button $itk_component(hull).ok -text [get_indep String IDEBROK]\
          -width 11 -command " ${this} ok_cb "]
        set cancelbutt [button $itk_component(hull).canit -text [get_indep String\
          IDEBRCancel] -width 11 -command " ${this} cancel_cb "]

        # bind Escape and Close Window button to Cancel
        wm protocol $itk_component(hull) WM_DELETE_WINDOW "${cancelbutt} invoke"
        bind $itk_component(hull) <Escape> "${cancelbutt} invoke"

        pack ${cancelbutt} ${okbutt} -pady 6 -padx 6 -side right

        InitializeFlagData
        UpdateCommandLineArguements dumy dumy
    }

    method FlagPage {container} {

        global FlagTags

        # TODO: check optimizations, debug, warnings, code generation.

        set optIDTagList [$itk_option(-toolchain) GetRuleFlagsIDTagsList $itk_option(-rule)\
          Optimize]
        set debugIDTagList [$itk_option(-toolchain) GetRuleFlagsIDTagsList $itk_option(-rule)\
          Debug]
        set warnIDTagList [$itk_option(-toolchain) GetRuleFlagsIDTagsList $itk_option(-rule)\
          Warning]
        set codeGenIDTagList [$itk_option(-toolchain) GetRuleFlagsIDTagsList $itk_option(-rule)\
          CodeGen]

        #
        # Create debug flag list
        #

        set debugLBX [combobox::combobox ${container}.dbglbx -command\
          " ${this} cb_combobox_changed "]
        set debugLBL [label ${container}.dbglbl -text [get_indep String\
          IDEBRDebugCombo]]

        if {${debugIDTagList} == ""} {
            #disable debug listbox if no flags
            ${debugLBX} configure -state disabled
        } else {
            foreach debugIDTag ${debugIDTagList} {
                set debugLookUp($FlagTags(${debugIDTag})) ${debugIDTag}
                set tmpCmd "${debugLBX} listinsert end\
                  \"$FlagTags(${debugIDTag})\""
                eval ${tmpCmd}
            }
            ${debugLBX} configure -editable 0
        }
        grid ${debugLBL} -column 1 -row 2 -sticky e
        grid ${debugLBX} -column 2 -row 2

        #
        # Create optimizations list of flags
        #

        set optimizeLBL [label ${container}.optlbl -text [get_indep String\
          IDEBROptimizationCombo]]
        set optimizeLBX [combobox::combobox ${container}.optlbx -command\
          " ${this} cb_combobox_changed "]

        if {${optIDTagList} == ""} {
            #disable Optimizations listbox if not flags
            ${optimizeLBX} configure -state disabled
        } else {
            foreach optIDTag ${optIDTagList} {
                set optimizeLookUp($FlagTags(${optIDTag})) ${optIDTag}
                set tmpCmd "${optimizeLBX} listinsert end\
                  \"$FlagTags(${optIDTag})\""
                eval ${tmpCmd}
            }
            ${optimizeLBX} configure -editable 0
        }

        grid columnconfigure ${container} 0 -minsize 10
        grid columnconfigure ${container} 3 -minsize 10
        grid columnconfigure ${container} 6 -minsize 10

        grid ${optimizeLBL} -column 4 -row 2 -sticky e
        grid ${optimizeLBX} -column 5 -row 2 -sticky we

        grid rowconfigure ${container} 1 -minsize 10
        grid rowconfigure ${container} 3 -minsize 10

        #
        # Create warnings flag list
        #

        set warnLBX [combobox::combobox ${container}.warnlbx -command\
          " ${this} cb_combobox_changed "]
        set warnLBL [label ${container}.warnlbl -text [get_indep String\
          IDEBRWarningCombo]]

        if {${warnIDTagList} == ""} {
            #disable warnings listbox if no flags
            ${warnLBX} configure -state disabled
        } else {
            foreach warnIDTag ${warnIDTagList} {
                set warnLookUp($FlagTags(${warnIDTag})) ${warnIDTag}
                set tmpCmd "${warnLBX} listinsert end\
                  \"$FlagTags(${warnIDTag})\""
                eval ${tmpCmd}
            }
            ${warnLBX} configure -editable 0
        }
        grid ${warnLBL} -column 1 -row 4 -sticky e
        grid ${warnLBX} -column 2 -row 4 -sticky we

        grid rowconfigure ${container} 5 -minsize 10

        #
        # Create Code Generation flag list
        #

        set codeLBX [combobox::combobox ${container}.codelbx -command\
          " ${this} cb_combobox_changed "]
        set codeLBL [label ${container}.codelbl -text [get_indep String\
          IDEBRCodeGenCombo]]

        if {${codeGenIDTagList} == ""} {
            #disable debug listbox if no flags
            ${codeLBX} configure -state disabled
        } else {
            foreach codeGenIDTag ${codeGenIDTagList} {
                set codeLookUp($FlagTags(${codeGenIDTag})) ${codeGenIDTag}
                set tmpCmd "${codeLBX} listinsert end\
                  \"$FlagTags(${codeGenIDTag})\""
                eval ${tmpCmd}
            }
            ${codeLBX} configure -editable 0
        }
        grid ${codeLBL} -column 4 -row 4 -sticky e
        grid ${codeLBX} -column 5 -row 4 -sticky we

        #
        # create user flags entry
        #

        set uflagsLBL [label ${container}.uflbl -text [get_indep String\
          IDEBRUserFlags]]
        set uflagsENT [entry ${container}.ufent -textvar userflags]
        bind ${uflagsENT} <KeyRelease> "${this} cb_combobox_changed"

        grid ${uflagsLBL} -column 1 -row 6 -sticky e
        grid ${uflagsENT} -column 2 -row 6 -sticky we

        #
        # create executable location
        #

        set exeLBL [label ${container}.exelbl -text [get_indep String\
          IDEBRExecLocation]]
        set exeLEB [LabelEntryButton& ${container}.exeleb -text ""]
        bind [${exeLEB} component entry] <KeyRelease> "${this} cb_combobox_changed"

        grid ${exeLBL} -column 4 -row 6 -sticky e
        grid ${exeLEB} -column 5 -row 6 -sticky we

        grid rowconfigure ${container} 7 -minsize 10

        #
        # Create text widget to display compiler command line flags
        #

        set cmdlTXT [text ${container}.cmdltxt -height 3 -width 10]

        grid ${cmdlTXT} -column 1 -columnspan 5 -row 8 -sticky we

        grid rowconfigure ${container} 9 -minsize 10

        # Update the Compiler Command Line when the user does something.
        bind ${container} <ButtonRelease> "${this} UpdateCommandLineArguements"
        bind ${container} <KeyRelease> "${this} UpdateCommandLineArguements"

    }

    method cb_combobox_changed {{dumy1 ""} {dumy2 ""}} {

        # Update the cmd line display to reflex new settings.
        UpdateCommandLineArguements
    }

    method UpdateCommandLineArguements {{dummy1 ""} {dummy2 ""}} {

        set cmdline [GetCompilerCommandLine]

        # clear widget
        ${cmdlTXT} configure -state normal
        ${cmdlTXT} delete 0.0 end

        ${cmdlTXT} insert end ${cmdline}
        # Stop people editting it.
        ${cmdlTXT} configure -state disabled

    }

    method GetCompilerCommandLine {} {

        # Get the tool
        set tool [[${exeLEB} component entry] get]
        if {${tool}==""} {
            set tool [$itk_option(-toolchain) GetTool $itk_option(-rule)]
        }

        # Get the basic action
        set basicaction [$itk_option(-toolchain) GetBasicAction $itk_option(-rule)]

        # Get the flags
        set flags ""
        set flags "${flags} [$itk_option(-toolchain) GetRuleFlags $itk_option(-rule) Debug\
          $debugLookUp([${debugLBX} get])]"
        set flags "${flags} [$itk_option(-toolchain) GetRuleFlags $itk_option(-rule) Optimize\
          $optimizeLookUp([${optimizeLBX} get])]"
        set flags "${flags} [$itk_option(-toolchain) GetRuleFlags $itk_option(-rule) Warning\
          $warnLookUp([${warnLBX} get])]"
        set flags "${flags} [$itk_option(-toolchain) GetRuleFlags $itk_option(-rule) CodeGen\
          $codeLookUp([${codeLBX} get])]"

        # Get user flags
        set userflags [${uflagsENT} get]

        # Get the marco defs
        set defines_list [${UserDefsList} get 0 end]
        set defines_spec [$itk_option(-toolchain) FormatDefines $itk_option(-rule)\
          ${defines_list}]

        # Get the include paths
        set includes_list1 [${AutoGenList} get 0 end]
        set includes_list2 [${UserGenList} get 0 end]
        set includes_list [concat ${includes_list1} ${includes_list2}]
        set include_spec [$itk_option(-toolchain) FormatIncludes $itk_option(-rule)\
          ${includes_list}]

        return "${tool} ${basicaction} ${flags} ${userflags} ${defines_spec}\
          ${include_spec}"
    }

    method InitializeFlagData {} {

        SetDebugFlags
        SetWarningFlags
        SetOptimizationFlags
        SetCodeGenerationFlags

        SetUserFlags
        SetCompilerLocation

        # Includes tab
        SetGeneratedIncludes
        SetUserIncludes

        # Defines tab
        SetDefinesList
    }

    method SetDebugFlags {{flags ""}} {
        global FlagTags
        if {${flags}==""} {
            #Let check the build object
            set flags [$itk_option(-buildtarget) GetToolChainFlags $itk_option(-rule) Debug]
        } else {
            set flags [$itk_option(-buildtarget) SetToolChainFlags $itk_option(-rule) Debug ${flags}]
        }

        if {${flags}==""} {
            # Select the first one in the list as default
            combobox::select ${debugLBX} 0
        } else {
            #set it to editable first
            ${debugLBX} configure -editable 1
            #Delete current selection
            ${debugLBX} delete 0 end
            #Add stored selection
            ${debugLBX} insert 0 "$FlagTags(${flags})"
            #Disable edit again
            ${debugLBX} configure -editable 0
        }
    }

    method SetWarningFlags {{flags ""}} {
        global FlagTags
        if {${flags}==""} {
            #Let check the build object
            set flags [$itk_option(-buildtarget) GetToolChainFlags $itk_option(-rule) Warning]
        } else {
            set flags [$itk_option(-buildtarget) SetToolChainFlags $itk_option(-rule) Warning\
              ${flags}]
        }

        if {${flags}==""} {
            # Select the first one in the list as default
            combobox::select ${warnLBX} 0
        } else {
            #set it to editable first
            ${warnLBX} configure -editable 1
            #Delete current selection
            ${warnLBX} delete 0 end
            #Add stored selection
            ${warnLBX} insert 0 "$FlagTags(${flags})"
            #Disable edit again
            ${warnLBX} configure -editable 0
        }
    }

    method SetOptimizationFlags {{flags ""}} {
        global FlagTags
        if {${flags}==""} {
            #Let check the build object
            set flags [$itk_option(-buildtarget) GetToolChainFlags $itk_option(-rule) Optimize]
        } else {
            set flags [$itk_option(-buildtarget) SetToolChainFlags $itk_option(-rule) Optimize\
              ${flags}]
        }
        if {${flags}==""} {
            # Select the first one in the list as default
            combobox::select ${optimizeLBX} 0
        } else {
            #set it to editable first
            ${optimizeLBX} configure -editable 1
            #Delete current selection
            ${optimizeLBX} delete 0 end
            #Add stored selection
            ${optimizeLBX} insert 0 "$FlagTags(${flags})"
            #Disable edit again
            ${optimizeLBX} configure -editable 0
        }
    }

    method SetCodeGenerationFlags {{flags ""}} {
        global FlagTags
        if {${flags}==""} {
            #Let check the build object
            set flags [$itk_option(-buildtarget) GetToolChainFlags $itk_option(-rule) CodeGen]
        } else {
            set flags [$itk_option(-buildtarget) SetToolChainFlags $itk_option(-rule) CodeGen\
              ${flags}]
        }

        if {${flags}==""} {
            # Select the first one in the list as default
            combobox::select ${codeLBX} 0
        } else {
            #set it to editable first
            ${codeLBX} configure -editable 1
            #Delete current selection
            ${codeLBX} delete 0 end
            #Add stored selection
            ${codeLBX} insert 0 "$FlagTags(${flags})"
            #Disable edit again
            ${codeLBX} configure -editable 0
        }
    }

    method SetUserFlags {{flags ""}} {
        if {${flags}==""} {
            set flags [$itk_option(-buildtarget) GetToolChainFlags $itk_option(-rule) UserFlags]
            # Clean current entry
            ${uflagsENT} delete 0 end
            ${uflagsENT} insert 0 ${flags}
        } else {
            $itk_option(-buildtarget) SetToolChainFlags $itk_option(-rule) UserFlags ${flags}
        }
    }

    method SetCompilerLocation {{local ""}} {

        set tc_local [$itk_option(-toolchain) GetTool $itk_option(-rule)]
        set bt_local [$itk_option(-buildtarget) GetToolChainFlags $itk_option(-rule) ToolLocation]

        if {${local}==""} {
            set local ${bt_local}
            if {${local}==""} {
                set local ${tc_local}
            }
             [${exeLEB} component entry] delete 0 end
             [${exeLEB} component entry] insert 0 ${local}
        } else {
            # Don't save compiler location if
            # it's the same as the toolchain one.
            # This may course confusion later.
            if {${local}==${tc_local}} {
                # Clear the ToolLocation field.
                $itk_option(-buildtarget) SetToolChainFlags $itk_option(-rule) ToolLocation ""
            } else {
                $itk_option(-buildtarget) SetToolChainFlags $itk_option(-rule) ToolLocation ${local}
            }
        }
    }

    method IncludesPage {leaf} {

        set leaffr [frame ${leaf}.frame]

        # Auto generated includes
        set AutoGenLabel [label ${leaffr}.aglbl -text [get_indep String\
          IDEBRAutoGenPathsLabel]]

	# Use this frame to hold listbox and scrollbars.

	set AutoGen [frame $leaffr.scrolllistbox -relief sunken\
		-borderwidth 2]

	scrollbar $AutoGen.vertsb -orient vertical \
		-relief flat -borderwidth 1 -highlightthickness 0
	scrollbar $AutoGen.horizsb -orient horizontal \
		-relief flat -borderwidth 1 -highlightthickness 0

        set AutoGenList [listbox $AutoGen.aglbx -height 3 -width 55 \
		-relief flat -borderwidth 0 \
		-xscrollcommand "scrollListbox $AutoGen.horizsb" \
		-yscrollcommand "scrollListbox $AutoGen.vertsb"]

	$AutoGen.vertsb configure -command "$AutoGenList yview"
	$AutoGen.horizsb configure -command "$AutoGenList xview"

	grid $AutoGenList -row 1 -column 1 -sticky news
	grid $AutoGen.vertsb -row 1 -column 2 -sticky ns
	grid $AutoGen.horizsb -row 2 -column 1 -sticky ew
	grid [frame $AutoGen.dummyframe -relief groove -borderwidth 2] -row 2 -column 2 -sticky news

	grid columnconfigure $AutoGen 1 -weight 1
	grid rowconfigure $AutoGen 1 -weight 1

        # User includes
        set UserGenLabel [label ${leaffr}.uglbl -text [get_indep String\
          IDEBRUserGenPathsLabel]]

	# Use this frame to hold listbox and scrollbars.

	set UserGen [frame $leaffr.ug_scrolllistbox -relief sunken\
		-borderwidth 2]

	scrollbar $UserGen.vertsb -orient vertical \
		-relief flat -borderwidth 1 -highlightthickness 0
	scrollbar $UserGen.horizsb -orient horizontal \
		-relief flat -borderwidth 1 -highlightthickness 0

        set UserGenList [listbox $UserGen.uglbx -height 3 -width 55 \
		-relief flat -borderwidth 0 \
		-xscrollcommand "scrollListbox $UserGen.horizsb" \
		-yscrollcommand "scrollListbox $UserGen.vertsb"]

	$UserGen.vertsb configure -command "$UserGenList yview"
	$UserGen.horizsb configure -command "$UserGenList xview"

	grid $UserGenList -row 1 -column 1 -sticky news
	grid $UserGen.vertsb -row 1 -column 2 -sticky ns
	grid $UserGen.horizsb -row 2 -column 1 -sticky ew
	grid [frame $UserGen.dummyframe -relief groove -borderwidth 2] -row 2 -column 2 -sticky news

	grid columnconfigure $UserGen 1 -weight 1
	grid rowconfigure $UserGen 1 -weight 1

        #The Buttons
        set GenButt [button ${leaf}.gbut -width 11 -text [get_indep String\
          IDEBRGeneratePaths] -under [get_indep Pos IDEBRGeneratePaths]\
          -command " ${this} generate_cb "]
        set DelButt [button ${leaf}.dbut -width 11 -text [get_indep String\
          IDEBRDeletePath] -under [get_indep Pos IDEBRDeletePath] -command\
          " ${this} cb_deletepath "]
        set AddButt [button ${leaf}.abut -width 11 -text [get_indep String\
          IDEBRAddPath] -under [get_indep Pos IDEBRAddPath] -command " ${this}\
          cb_addincpath "]

	grid x $AutoGenLabel - x x        x -sticky w
	grid x $AutoGen      - x $GenButt x -sticky we
	grid x $UserGenLabel - x $DelButt x -sticky w
	grid x $UserGen      - x $AddButt x -sticky we

        grid rowconfigure ${leaffr} 1 -minsize 10
	grid columnconfigure $leaffr 0 -minsize 10
	grid columnconfigure $leaffr 1 -weight 10 -minsize 10
	grid columnconfigure $leaffr 2 -weight 20
        grid columnconfigure $leaffr 3 -minsize 10
	grid columnconfigure $leaffr 5 -minsize 10 -weight 0

	# Pull the includes leaf in the notebook.

        grid ${leaffr} -column 1 -row 1 -rowspan 3 -sticky we
        grid columnconfigure ${leaf} 2 -weight 1
        grid columnconfigure ${leaf} 3 -minsize 10
    }

    method generate_cb {} {

        set includeslist [GenerateIncludesList]
        SetGeneratedIncludes ${includeslist}

        $itk_option(-buildtarget) SetAutoIncludes $itk_option(-rule) ${includeslist}
    }

    method cb_deletepath {} {

        # Get the auto gen list selection
        set selected [${AutoGenList} curselect]
        if {${selected}==""} {
            # Nothing selected, check the User list
            set selected [${UserGenList} curselect]
            if {${selected}==""} {
                # Nothing selected
                return
            }
            ${UserGenList} delete ${selected}
        } else {
            ${AutoGenList} delete ${selected}
        }
    }

    method GenerateIncludesList {} {

        set srcfiles [$itk_option(-buildtarget) GetSourceFiles]

        set includepaths [GenerateIncludePaths ${srcfiles}]

        return ${includepaths}
    }

    method SetGeneratedIncludes {{includes ""}} {

        if {${includes}==""} {
            set includes [$itk_option(-buildtarget) GetAutoIncludes $itk_option(-rule)]
        } else {
            $itk_option(-buildtarget) SetAutoIncludes $itk_option(-rule) ${includes}
        }

        # Clear old entries.
        ${AutoGenList} delete 0 end

        # Set new entries.
        set tmpcmd "${AutoGenList} insert end ${includes}"
        eval ${tmpcmd}


    }

    method SetUserIncludes {{includes ""}} {
        if {${includes}==""} {
            set includes [$itk_option(-buildtarget) GetUserIncludes $itk_option(-rule)]
        } else {
            if {${includes}=="<%none%>"} {
                set includes ""
            }
            $itk_option(-buildtarget) SetUserIncludes $itk_option(-rule) ${includes}
        }

        if {${includes}!=""} {

            # Clear old entries.
            ${UserGenList} delete 0 end

            # Set new entries.
            set tmpcmd "${UserGenList} insert end ${includes}"
            eval ${tmpcmd}
        }
    }

    method DefinesPage {leaf} {

        # User macro definitions
        set UserMacroLabel [label ${leaf}.deflbl -text [get_indep String\
          IDEBRMacroDefinesLabel]]

	set UserDefsFrame [frame $leaf.userdeffr -relief sunken\
		-borderwidth 2]
	scrollbar $UserDefsFrame.vertsb -orient vertical \
		-relief flat -borderwidth 1 -highlightthickness 0

        set UserDefsList [listbox $UserDefsFrame.deflbx -height 6 \
		-yscrollcommand "scrollListbox $UserDefsFrame.vertsb"]
        bind ${UserDefsList} <ButtonRelease-1> "${this} cb_defselect"
	$UserDefsFrame.vertsb configure -command "$UserDefsList yview"

	grid $UserDefsList -row 1 -column 1 -sticky news
	grid $UserDefsFrame.vertsb -row 1 -column 2 -sticky ns
	grid columnconfigure $UserDefsFrame 1 -weight 1
	grid rowconfigure $UserDefsFrame 1 -weight 1

        #The Buttons
        set DefDelButt [button ${leaf}.dbut -text [get_indep String\
          IDEBRDeleteMacro] -under [get_indep Pos IDEBRDeleteMacro] -width 11\
          -command " ${this} cb_deldef "]
        set DefNewButt [button ${leaf}.nubut -text [get_indep String\
          IDEBRCreateMacro] -under [get_indep Pos IDEBRCreateMacro] -width 11\
          -command " ${this} cb_newdef "]
        set DefSetButt [button ${leaf}.abut -text [get_indep String\
          IDEBRChangeMacro] -under [get_indep Pos IDEBRChangeMacro] -width 11\
          -command " ${this} cb_setdef "]

        #The macro entry widget
        set EditDef [entry ${leaf}.editdef -textvariable \
		[itcl::scope editdefbox]]

	trace variable [itcl::scope editdefbox] w "[itcl::code $this EditDefBoxChanged]"

        grid rowconfigure ${leaf} 1 -minsize 10
        grid columnconfigure ${leaf} 0 -minsize 10

        grid ${UserMacroLabel} -row 2 -column 1 -sticky w
        grid ${UserDefsFrame} -row 3 -column 1 -rowspan 3 -sticky we

        grid rowconfigure ${leaf} 6 -minsize 10

        grid ${EditDef} -row 7 -column 1 -sticky we

        grid ${DefNewButt} -column 3 -row 3
        grid ${DefDelButt} -column 3 -row 4
        grid ${DefSetButt} -column 3 -row 5
        grid columnconfigure ${leaf} 2 -minsize 10


        grid rowconfigure ${leaf} 8 -weight 10
        grid columnconfigure ${leaf} 9 -minsize 10
        grid columnconfigure ${leaf} 1 -weight 10
        grid rowconfigure ${leaf} 3 -weight 10
        grid rowconfigure ${leaf} 4 -weight 10
        grid rowconfigure ${leaf} 5 -weight 10
        grid rowconfigure ${leaf} 9 -minsize 10

        DefHandleState
    }

    method SetDefinesList {{defines ""}} {

        if {${defines}==""} {
            # Lets checks the build object
            set defines [$itk_option(-buildtarget) GetUserDefines $itk_option(-rule)]
        } else {
            if {${defines}=="<%none%>"} {
                set defines ""
            }
            # Set define in build object
            $itk_option(-buildtarget) SetUserDefines $itk_option(-rule) ${defines}
        }

        if {${defines}!=""} {
            # Populate defines listbox with defines.
            foreach define ${defines} {
                ${UserDefsList} insert end ${define}
            }
        }
    }

    method EditDefBoxChanged {{dummyarg1 ""} {dummyarg2 ""} {dummyargs ""}} {
        set entry_value [${EditDef} get]

        # Check to see if new Entry box value
        # matches any list box values.
        # Why? so it can be selected from the list
        set matched 0

        set itemnum -1

        foreach item [${UserDefsList} get 0 end] {
            incr itemnum
            if {${item}==${entry_value}} {
                set matched 1
                break

            }
        }

        if {${matched}} {
            ${UserDefsList} selection clear 0 end
            ${UserDefsList} selection set ${itemnum}
        }
        DefHandleState
    }

    method DefHandleState {{selected ""} {entered ""}} {

        # Get Listbox selection
        set selection [${UserDefsList} curselect]
        if {${selected} == "" && ${selection} != ""} {
            set selected [${UserDefsList} get ${selection}]
        }

        # Get Text Widget contents.
        if {${entered}==""} {
            set entered [${EditDef} get]
        }

        #Nothing selected. nothing entered
        if {${selected}=="" && ${entered}==""} {
            ${DefNewButt} configure -state disabled -default normal
            ${DefDelButt} configure -state disabled -default normal
            ${DefSetButt} configure -state disabled -default normal
            bind $itk_component(hull) <Return> "${okbutt} invoke"
            return
        }

        #Nothing selected. something entered
        if {${selected}=="" && ${entered}!=""} {
            ${DefNewButt} configure -state normal -default active
            bind $itk_component(hull) <Return> "${DefNewButt} invoke"
            ${DefDelButt} configure -state disabled -default normal
            ${DefSetButt} configure -state disabled -default normal
            return
        }

        #something selected. something entered (matching)
        if {${selected}!="" && ${entered}==${selected}} {
            ${DefNewButt} configure -state disabled -default normal
            ${DefDelButt} configure -state normal -default active
            bind $itk_component(hull) <Return> "${DefDelButt} invoke"
            ${DefSetButt} configure -state disabled -default normal
            return
        }

        #Something selected. Something entered (not matching)
        if {${selected}!="" && ${entered}!="" && ${selected}!=${entered}} {
            ${DefNewButt} configure -state normal -default active
            bind $itk_component(hull) <Return> "${DefNewButt} invoke"
            ${DefDelButt} configure -state disabled -default normal
            ${DefSetButt} configure -state normal -default normal
            return
        }

        #Something selected. Something entered (not matching)
        if {${selected} != "" && ${entered} == ""} {
            ${DefNewButt} configure -state disabled -default active
            bind $itk_component(hull) <Return> "$okbutt invoke"
            ${DefDelButt} configure -state disabled -default normal
            ${DefSetButt} configure -state disabled -default normal
            return
        }

    }

    method cb_newdef {} {

        # Get the new macro from the edit box.
        set new_def [${EditDef} get]

        # Add it to the macros list.
        ${UserDefsList} insert end "${new_def}"
        ${UserDefsList} selection clear 0 end
        ${UserDefsList} selection set end

        # Clean the edit box.
        ${EditDef} delete 0 end

        # Update the state of the buttons.
        EditDefBoxChanged
    }

    method cb_setdef {} {
        set new_def [${EditDef} get]

        set selected [${UserDefsList} curselect]
        ${UserDefsList} delete ${selected}
        ${UserDefsList} insert ${selected} ${new_def}
        EditDefBoxChanged
    }

    method cb_deldef {} {
        set selected [${UserDefsList} curselect]

        if {${selected} != ""} {
            ${UserDefsList} delete ${selected}

            ${EditDef} delete 0 end
        }
        DefHandleState
    }

    method cb_defselect {} {
        set selection [${UserDefsList} curselect]

        if {${selection} != ""} {
            set editdefbox [${UserDefsList} get ${selection}]
        }
        DefHandleState
    }

    method cb_addincpath {} {
        set include_path [Editor&::DirDialog $itk_component(hull) -title "Add Include\
          Path:"]

        # Check that a path was selected
        if {${include_path}!=""} {
            #TODO: check it's unique

            ${UserGenList} insert end ${include_path}
        }
    }

    method ok_cb {} {

        SaveData

	deactivate 1
    }

    method SaveData {} {

        # debug settings
        SetDebugFlags $debugLookUp([${debugLBX} get])

        # optimization settings
        SetOptimizationFlags $optimizeLookUp([${optimizeLBX} get])

        # code generation settings
        SetCodeGenerationFlags $codeLookUp([${codeLBX} get])

        # Warning level settings
        SetWarningFlags $warnLookUp([${warnLBX} get])

        # User defined compiler flags
        $itk_option(-buildtarget) SetToolChainFlags $itk_option(-rule) UserFlags [${uflagsENT} get]

        # Compiler tool.
        SetCompilerLocation [[${exeLEB} component entry] get]

        # Save Auto Includes
        SetGeneratedIncludes [${AutoGenList} get 0 end]

        # Save User Includes
        #(Pass it a secret message in they are no includes)
        set includes [${UserGenList} get 0 end]
        if {${includes}==""} {
            set includes "<%none%>"
        }
        SetUserIncludes ${includes}

        # Save Defines
        #(Pass it a secret message in they are no defines)
        set defines [${UserDefsList} get 0 end]
        if {${defines}==""} {
            set defines "<%none%>"
        }
        SetDefinesList ${defines}
    }


    method cancel_cb {} {
	deactivate 0
    }

    method DestroyDialog {} {
        # Delete all the objects.
        itcl::delete object $itk_component(hull)
    }
}

proc GenerateIncludePaths {srcfiles} {

    # Check paf_db_iu exists
    if {[info command paf_db_iu]==""} {
        # The Includes database hasn't been created.
        return ""
    }

    set all_includefiles ""
    set all_includepaths ""

    # Get the included files for each source file
    foreach srcfile ${srcfiles} {
        set includefiles [paf_db_iu seq -col 0 -end ${srcfile}]
        set all_includefiles [concat ${all_includefiles} ${srcfile}]
    }

    # Strip them down to the paths
    foreach includefile ${all_includefiles} {
        set includepath [file dirname ${includefile}]
        set all_includepaths [concat ${all_includepaths} ${includepath}]
    }

    # Remove and duplicates
    set all_includepaths [lsort ${all_includepaths}]
    set all_includepaths [lunique ${all_includepaths}]

    return ${all_includepaths}
}

proc TestIt {tc} {
    snIdeToolsDlg tst
    tst configure -toolchain ${tc} -ruleSuffix CC
    tst CreateDialog
}


