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
################################################
##
## Target Editor Dialog for SN-IDE.
## Copyright (c) 1999 Cygnus Solutions, Inc.
## Written by Ian Roxborough <irox@cygnus.com>
##
################################################

itcl::class snEditTarget {
    inherit sourcenav::Dialog

    # Public variables
    public variable TargetName ""

    # Target Name as store in db.
    public variable NewTarget ""
    # Set to one if it's a new target.

    # FIXME: this does not seem to get used anywhere, should be removed!
    # list of source files
    protected variable src_file_list ""

    # Table of rules.
    protected variable rulesindex

    # Build target object
    public variable b_target

    # Variable used to track the debug toggle button state
    private variable rb_debug_state 0

    # Var used to store linker location/command.
    private variable linkerlocal  ""

    constructor {target_name args} {
        #TODO: check target_name is valid

        set TargetName ${target_name}

        CreateDialog

        InitalizeData

        eval itk_initialize $args

        # Make the dialog modal	
	$this configure -modality application
    }

    destructor {
	DestroyDialog
    }

    ##################################################
    # snEditTarget::InitalizeData
    #
    # This create a build target object and load the
    # target data from the db to the object which is
    # used to supply the settings for the dialog.
    #
    public method InitalizeData {} {

        set b_target [snBuildTarget .te_target_data]

        ${b_target} LoadData ${TargetName}

        SetTargetType
        SetToolChain
        SetBuildDirectory
        SetLibraryFiles

        SetSourceFiles

        # Linker rule	
        SetOutputFilename
        SetLinkerEntryPoint
        SetUserLinkFlags
        SetLinkerExecutable
        SetLaunchDebugerInfo
        SetLaunchCommandLine

        #SetLibraryFiles
        HandleLibsTabState

        SetBuildRules
        UpdateCmdLine

	# Add custom GUI parts (if any).
	AddCustomGUI
    }

    method AddCustomGUI {} {

        set tool_chain [$itk_component(tchaincbx) get]
        set tchain_obj [GetToolChainObject ${tool_chain}]

	# Check if any custom code is there.
	if {[$tchain_obj GetUserTargetEditGUI] != ""} {
	    # Add custom GUI tab.
	    itk_component add custompage {
		$itk_component(notebook) add custom \
			-label "Custom"
	    }
    
	    [$tchain_obj GetUserTargetEditGUI] $itk_component(custompage) "[itcl::code $b_target]"
	}
    }

    method RemoveCustomGUI {} {

        set tool_chain [$itk_component(tchaincbx) get]
        set tchain_obj [GetToolChainObject ${tool_chain}]

	# Check if any custom code is there.
	if {[info exists itk_component(custompage)] && [winfo exists $itk_component(custompage)]} {
	    if {[$tchain_obj GetRemoveUserTargetEditGUI] != ""} {
		[$tchain_obj GetRemoveUserTargetEditGUI] $itk_component(custompage) [itcl::code $b_target]
	    }

	    # Remove custom GUI tab.
	    $itk_component(notebook) delete custom
	}
    }

    ##################################################
    # snEditTarget::CreateDialog
    #
    # This deals with the drawing and layout of the
    # Edit Target dialog box.
    #

    public method CreateDialog {} {

        $this title "[get_indep String IDETargetEdTitle] ${TargetName}"

        # Create notebook widget
        itk_component add notebook {
            tixNoteBook $itk_component(hull).notebook \
                -width 200 -height 200
        } {}

        # Add the leafs
        AddSrcPage
        AddLibPage
        AddRulesPage
        AddLinkPage

        # Add Build Directory Widget
        itk_component add bdirlbl {
            label $itk_component(hull).bdirlbl \
                -text [get_indep String IDETEBuildDir] \
                -under [get_indep Pos IDETEBuildDir]
        }

        itk_component add bdirleb {
            LabelEntryButton& $itk_component(hull).bdirleb
        } {}

        $itk_component(bdirleb) configure \
            -command [list sn_choose_dir [$itk_component(bdirleb) component entry]]

        # Add Target Type combobox

        itk_component add ttypelbl {
            label $itk_component(hull).ttypelbl \
                -text [get_indep String IDETETargetType] \
                -under [get_indep Pos IDETETargetType]
        }

        itk_component add ttypecbx {
            combobox::combobox $itk_component(hull).ttypecbx
        } {}

        # FIXME : We should check the tool chain to see what types
        # of builds are supported, but for now lets just
        # assume it's Executable, Library, DLL (Shared Library).

        $itk_component(ttypecbx) listinsert end "Executable"
        $itk_component(ttypecbx) listinsert end "Library"
        $itk_component(ttypecbx) configure -editable 0

        # Add Tool Chain combobox

        itk_component add tchainlbl {
            label $itk_component(hull).tchainlbl \
                -text [get_indep String IDETEToolChain] \
                -under [get_indep Pos IDETEToolChain]
        }

        itk_component add tchaincbx {
            combobox::combobox $itk_component(hull).tchaincbx \
                -command "${this} ToolChainChanged ${this}"
        } {}

        # Get list of installed tool chains.

        set ToolChainList [GetActiveToolChains]

	foreach toolchain $ToolChainList { 
	    $itk_component(tchaincbx) listinsert end [$toolchain GetLongName] 
        }

        # Add OK/Cancel Buttons
        itk_component add ok {
            button $itk_component(hull).ok \
                -width 11 \
                -command "${this} ok_cb" \
                -text [get_indep String IDETEOK]
        }

        itk_component add cancel {
            button $itk_component(hull).cancel \
                -width 11 \
                -command "${this} cancel_cb" \
                -text [get_indep String IDETECancel]
        }

        # Bind Escape and close window button to Cancel
        $this protocol WM_DELETE_WINDOW "$itk_component(cancel) invoke"
        bind $itk_component(hull) <Escape> "$itk_component(cancel) invoke"

        # Bind Return to the OK button.
        bind $itk_component(hull) <Return> "$itk_component(ok) invoke"

        # Put a 10 pixel border around the frame
        $this configure -borderwidth 10

        grid columnconfigure $itk_component(hull) 0 -weight 1
        grid rowconfigure $itk_component(hull) 6 -weight 1

        grid $itk_component(notebook) -row 0 -column 0 -sticky news -rowspan 7 \
            -padx 5 -pady 5

        grid $itk_component(bdirlbl) -row 0 -column 1 -columnspan 2 -sticky w
        grid $itk_component(bdirleb) -row 1 -column 1 -columnspan 2 -sticky ew

        grid $itk_component(ttypelbl) -row 2 -column 1 -columnspan 2 -sticky w
        grid $itk_component(ttypecbx) -row 3 -column 1 -columnspan 2 -sticky ew

        grid $itk_component(tchainlbl) -row 4 -column 1 -columnspan 2 -sticky w
        grid $itk_component(tchaincbx) -row 5 -column 1 -columnspan 2 -sticky ew

        grid $itk_component(ok) -row 8 -column 1
        grid $itk_component(cancel) -row 8 -column 2


        # This seems about right, might need some adjusting though
        configure -geometry 800x500

        # FIXME : we can not use reqwidth and reqheight because of tabnotebook!
        minsize 650 300
    }

    public method ToolChainChanged {my_this {dummy1 ""} {tool_chain ""}} {
	
	RemoveCustomGUI
        
	# Set the new toolchain.
        ${b_target} SetToolChain ${tool_chain}

        # Set the linker to reflect the new toolchain.
        SetLinkerExecutable

	# Update Build rules list (list may vary from toolchain to
	# toolchain).
	$my_this SetBuildRules

        # Update the command lines in build rules and link rules.
        ${my_this} UpdateCmdLine

	AddCustomGUI
    }

    private method AddSrcPage {} {

        global sn_options

        # Add leafs for source files.

        itk_component add sourcepage {
            $itk_component(notebook) add src \
                -label [get_indep String IDETESourceFilesTab] \
                -under [get_indep Pos IDETESourceFilesTab]
        }

        itk_component add trfr1 {
            frame $itk_component(sourcepage).trfr1 \
                -relief sunken -borderwidth 2
        }

        itk_component add srctree {
            sn_treetable $itk_component(trfr1) \
                -selectmode extended \
                -height 10 -width 1 -indent 20 -bw 2 -relief sunken \
                -font $sn_options(def,default-font) \
                -selectforeground $sn_options(def,select-fg) \
                -selectbackground $sn_options(def,select-bg)
        } {}

        grid $itk_component(srctree) -row 0 -column 0 -sticky news
        grid $itk_component(trfr1).x -row 1 -column 0 -sticky ew
        grid $itk_component(trfr1).y -row 0 -column 1 -sticky ns

        grid columnconfigure $itk_component(trfr1) 0 -weight 1
        grid rowconfigure $itk_component(trfr1) 0 -weight 1


        itk_component add trfr2 {
            frame $itk_component(sourcepage).trfr2 \
                -relief sunken -borderwidth 2
        }

        itk_component add srctree2 {
            sn_treetable $itk_component(trfr2) \
                -selectmode extended \
                -height 10 -width 1 -indent 20 -bw 2 -relief sunken \
                -font $sn_options(def,default-font) \
                -selectforeground $sn_options(def,select-fg) \
                -selectbackground $sn_options(def,select-bg)
        } {}

        grid $itk_component(srctree2) -row 0 -column 0 -sticky news
        grid $itk_component(trfr2).x -row 1 -column 0 -sticky ew
        grid $itk_component(trfr2).y -row 0 -column 1 -sticky ns

        grid columnconfigure $itk_component(trfr2) 0 -weight 1
        grid rowconfigure $itk_component(trfr2) 0 -weight 1


        set AllProjectFiles [sn_project_file_list 0]

        $itk_component(srctree2) delete 0 end
        $itk_component(srctree2) insert 0 -text Files
        fill_file_tree -widget $itk_component(srctree2) \
            -contents ${AllProjectFiles} -parent 0

        itk_component add arf {
            button $itk_component(sourcepage).arf \
                -width 11 \
                -command "${this} AddSourceFiles" \
                -text [get_indep String IDETEAddFiles] \
                -under [get_indep Pos IDETEAddFiles]
        }

        itk_component add pfs {
            label $itk_component(sourcepage).pfs \
                -text [get_indep String IDETEProjectFiles]
        }

        itk_component add tfs {
            label $itk_component(sourcepage).tfs \
                -text [get_indep String IDETETargetFile]
        }

        itk_component add clbut {
            button $itk_component(sourcepage).clbut \
                -width 11 \
                -command "${this} RemoveSourceFiles" \
                -text [get_indep String IDETEClearFile] \
                -under [get_indep Pos IDETEClearFile]
        }

        itk_component add clallbut {
            button $itk_component(sourcepage).clallbut \
                -width 11 \
                -command "${this} RemoveAllSourceFiles" \
                -text [get_indep String IDETEClearAllFiles]
        }

        itk_component add impfbut {
            button $itk_component(sourcepage).impfbut \
                -width 11 \
                -command "${this} import_files" \
                -text [get_indep String IDETEImportFiles]
        }

        itk_component add impdbut {
            button $itk_component(sourcepage).impdbut \
                -width 11 \
                -command "${this} import_dir" \
                -text [get_indep String IDETEImportDir]
        }

        itk_component add implbl {
            label $itk_component(sourcepage).implbl \
                -text [get_indep String IDETEImportLabel]
        }

# FIXME : this could really stand to be redone from scratch

        grid columnconfigure $itk_component(sourcepage) 0 -minsize 10
        grid rowconfigure $itk_component(sourcepage) 11 -minsize 10
        grid rowconfigure $itk_component(sourcepage) 10 -weight 10
        grid $itk_component(pfs) -column 1 -row 2 -sticky w
        grid $itk_component(trfr2) -column 1 -row 3 -rowspan 8 -sticky news

        grid columnconfigure $itk_component(sourcepage) 2 -minsize 10

        grid $itk_component(arf) -column 3 -row 3 -pady 4
        grid $itk_component(clbut) -column 3 -row 4 -pady 4
        grid $itk_component(clallbut) -column 3 -row 5 -pady 4
        grid rowconfigure $itk_component(sourcepage) 6 -minsize 20
        grid $itk_component(implbl) -column 3 -row 7 -pady 4
        grid $itk_component(impfbut) -column 3 -row 8 -pady 4
        grid $itk_component(impdbut) -column 3 -row 9 -pady 4

        grid columnconfigure $itk_component(sourcepage) 4 -minsize 10

        grid $itk_component(tfs) -column 5 -row 2 -sticky w
        grid $itk_component(trfr1) -column 5 -row 3 -rowspan 8 -sticky news

        grid columnconfigure $itk_component(sourcepage) 5 -weight 10
        grid columnconfigure $itk_component(sourcepage) 1 -weight 10
        grid columnconfigure $itk_component(sourcepage) 6 -minsize 10

    }

    public method AddSourceFiles {} {
        set sels [$itk_component(srctree2) curselection]
        set newfiles [build_file_list $itk_component(srctree2) tree ${sels}]

        ${b_target} AddSourceFiles ${newfiles}

        # Delete old entries
        $itk_component(srctree) delete 0 end
        $itk_component(srctree) insert 0 -text Files
        fill_file_tree -widget $itk_component(srctree) \
            -contents [${b_target} GetSourceFiles] -parent end

        # Make sure the command lines are upto date.	
        UpdateCmdLine
    }

    public method RemoveSourceFiles {} {
        set sels [$itk_component(srctree) curselection]
        set deadfiles [build_file_list $itk_component(srctree) tree ${sels}]

        ${b_target} RemoveSourceFiles ${deadfiles}

        # Delete old entries
        $itk_component(srctree) delete 0 end
        $itk_component(srctree) insert 0 -text Files
        fill_file_tree -widget $itk_component(srctree) \
          -contents [${b_target} GetSourceFiles] -parent end

        # Make sure the command lines are upto date.	
        UpdateCmdLine
    }

    public method RemoveAllSourceFiles {} {

        ${b_target} SetSourceFiles ""

        # Delete old entries
        $itk_component(srctree) delete 0 end
        $itk_component(srctree) insert 0 -text Files
        fill_file_tree -widget $itk_component(srctree) \
          -contents [${b_target} GetSourceFiles] -parent end

        # Make sure the command lines are upto date.	
        UpdateCmdLine
    }

    private method AddLibPage {} {
        # Add leafs for libraries files.

        itk_component add libpage {
            $itk_component(notebook) add lib \
                -label [get_indep String IDETELibraryFileTab] \
                -under [get_indep Pos IDETELibraryFileTab]
        }

# FIXME: change all these widget and component names to something more readable

        itk_component add liblbl {
            label $itk_component(libpage).liblbl \
                -text [get_indep String IDETELibraryLabel]
        }

        itk_component add liblbxfr {
            frame $itk_component(libpage).liblbxfr \
                -relief sunken -borderwidth 2
        }

        itk_component add liblbx {
            listbox $itk_component(liblbxfr).liblbx \
                -relief flat \
                -yscrollcommand [list $itk_component(liblbxfr).scrolly set] \
	        -xscrollcommand [list $itk_component(liblbxfr).scrollx set]
        } {}

        itk_component add liblbxfr_scrolly {
            scrollbar $itk_component(liblbxfr).scrolly \
                -orient vertical \
                -command [list $itk_component(liblbx) yview]
        }

        itk_component add liblbxfr_scrollx {
            scrollbar $itk_component(liblbxfr).scrollx \
                -orient horizontal \
                -command [list $itk_component(liblbx) xview]
        }

        grid $itk_component(liblbx) -row 0 -column 0 -sticky news
        grid $itk_component(liblbxfr_scrolly) -row 0 -column 1 -sticky ns
        grid $itk_component(liblbxfr_scrollx) -row 1 -column 0 -sticky we

        grid rowconfigure $itk_component(liblbxfr) 0 -weight 1
        grid columnconfigure $itk_component(liblbxfr) 0 -weight 1


        itk_component add libadd {
            button $itk_component(libpage).libadd \
                -width 11 \
                -command "${this} cb_add_lib" \
                -text [get_indep String IDETEAddLib] \
                -under [get_indep Pos IDETEAddLib]
        }

        itk_component add libdel {
            button $itk_component(libpage).libdel \
                -width 11 \
                -command "${this} cb_del_lib" \
                -text [get_indep String IDETERemoveLib] \
                -under [get_indep Pos IDETERemoveLib]
        }

        itk_component add libup {
            button $itk_component(libpage).libup \
                -width 11 \
                -command "${this} cb_up_lib" \
                -text [get_indep String IDETEMoveUp] \
                -under [get_indep Pos IDETEMoveUp]
        }

        itk_component add libdown {
            button $itk_component(libpage).libdown \
                -width 11 \
                -command "${this} cb_down_lib" \
                -text [get_indep String IDETEMoveDown] \
                -under [get_indep Pos IDETEMoveDown]
        }

        grid $itk_component(liblbl) -column 1 -row 2 -sticky w
        grid $itk_component(liblbxfr) -column 1 -row 3 -rowspan 5 -sticky news
        bind $itk_component(liblbx) <ButtonRelease> "${this} cb_double_click_lib"

        grid $itk_component(libadd) -column 4 -row 3 -pady 3
        grid $itk_component(libdel) -column 4 -row 4 -pady 3
        grid $itk_component(libup) -column 4 -row 5 -pady 3
        grid $itk_component(libdown) -column 4 -row 6 -pady 3

        grid rowconfigure $itk_component(libpage) 7 -weight 10
        grid columnconfigure $itk_component(libpage) 1 -weight 10

        grid rowconfigure $itk_component(libpage) 1 -minsize 10
        grid rowconfigure $itk_component(libpage) 8 -minsize 10
        grid columnconfigure $itk_component(libpage) 0 -minsize 10
        grid columnconfigure $itk_component(libpage) 3 -minsize 10
        grid columnconfigure $itk_component(libpage) 5 -minsize 10

    }

    public method import_dir {} {

        set dir [Editor&::DirDialog $itk_component(hull) -title "Import Directory"]
        set importedSrcFiles [sn_glob ${dir}]

        if {${importedSrcFiles} != ""} {
            ${b_target} AddSourceFiles ${importedSrcFiles}

            # Delete old entries
            $itk_component(srctree) delete 0 end
            $itk_component(srctree) insert 0 -text Files
            fill_file_tree -widget $itk_component(srctree) -contents\
                [${b_target} GetSourceFiles] -parent end

            # Make sure the command lines are upto date.	
            UpdateCmdLine
        }
    }

    public method import_files {} {

        set importedSrcFiles [Editor&::FileDialog $itk_component(hull)]

        if {${importedSrcFiles} != ""} {
            ${b_target} AddSourceFiles ${importedSrcFiles}

            # Delete old entries
            $itk_component(srctree) delete 0 end
            $itk_component(srctree) insert 0 -text Files
            fill_file_tree -widget $itk_component(srctree) -contents\
              [${b_target} GetSourceFiles] -parent end

            # Make sure the command lines are upto date.	
            UpdateCmdLine
        }
    }

    private method AddRulesPage {} {
        # Add page for build rules.

        itk_component add buildpage {
            $itk_component(notebook) add buildpage \
                -label [get_indep String IDETEBuildRulesTab] \
                -under [get_indep Pos IDETEBuildRulesTab]
        }

        itk_component add buildruletable {
            # We use Tree here, but in other places we use sn_treetable. What gives?
            Tree $itk_component(buildpage).buildruletable \
                -tabs {70 100 420} \
                -filter {} \
                -indentwidth 20 -tabsize 2 \
                -labels "{Status} {File Type} {Description}"
        } {}

        $itk_component(buildruletable) treebind <Double-1> "${this} ruletable_double_cb"
        $itk_component(buildruletable) treebind <ButtonRelease-1> "${this} UpdateCmdLine"
        $itk_component(buildruletable) treebind <KeyRelease> "${this} UpdateCmdLine"
        $itk_component(buildruletable) treebind <Expose> "${this} rules_tab_selected_hack"

        itk_component add buildruleedit {
            button $itk_component(buildpage).buildruleedit \
                -width 11 \
                -command "${this} ruletable_double_cb" \
                -text [get_indep String IDETEBuildRuleEdit] \
                -under [get_indep Pos IDETEBuildRuleEdit]
        }

        itk_component add buildruledisable {
            button $itk_component(buildpage).buildruledisable \
                -width 11 \
                -command "${this} cb_rule_togle" \
                -text [get_indep String IDETEBuildRuleDisable] \
                -under [get_indep Pos IDETEBuildRuleDisable]
        }

        itk_component add displaycmdline {
            text $itk_component(buildpage).displaycmdline \
                -width 1
        }

        # Grid into a 5x3 array so we can center the buttons

        grid $itk_component(buildruletable) -row 0 -column 0 \
            -columnspan 5 -sticky news

        grid $itk_component(buildruleedit) -row 1 -column 1 -pady 10 
        grid $itk_component(buildruledisable) -row 1 -column 3 -pady 10

        grid $itk_component(displaycmdline) -row 2 -column 0 \
            -columnspan 5 -sticky news

        # Expand the height of the third row when resized
        grid rowconfigure $itk_component(buildpage) 2 -weight 1

        # All the columns need to be equally weighted

        for {set col 0} {$col < 5} {incr col} {
            grid columnconfigure $itk_component(buildpage) $col -weight 1
        }

        # Make a nice border around the whole page
        $itk_component(buildpage) configure -borderwidth 10
    }

    private method AddLinkPage {} {
        # Add leafs for linking rules

        itk_component add linkpage {
            $itk_component(notebook) add link \
                -label [get_indep String IDETELinkRulesTab] \
                -under [get_indep Pos IDETELinkRulesTab]
        }

        # Binary output file name

        itk_component add lfilelbl {
            label $itk_component(linkpage).lfilelbl \
                -text [get_indep String IDETELinkOutputFile]
        }

        itk_component add lfileent {
            entry $itk_component(linkpage).lfileent
        } {}

        # User specified link flags

        itk_component add lflagslbl {
            label $itk_component(linkpage).lflagslbl \
                -text [get_indep String IDETELinkFlags]
        }

        itk_component add lflagsent {
            entry $itk_component(linkpage).lflagsent
        } {}

        # Linker executable

        itk_component add linkerlabel {
            label $itk_component(linkpage).linkerlable \
                -text [get_indep String IDETELinkerExe] \
                -under [get_indep Pos IDETELinkerExe]
        }

        itk_component add linkerentry {
            LabelEntryButton& $itk_component(linkpage).linkerentry \
		    -variable [itcl::scope linkerlocal]
        } {}

	# Try to keep the entry boxes in line.
	pack forget [$itk_component(linkerentry) component label]

# FIXME: is this "entrypoint" feature broken?
        # Entry box for entrypoint

        itk_component add enptlbl {
            label $itk_component(linkpage).enptlbl \
                -text [get_indep String IDETELinkEntryPoint]
        }

        itk_component add enptent {
            entry $itk_component(linkpage).enptent
        } {}

        itk_component add linkcmdline {
            text $itk_component(linkpage).linkcmdline \
                -width 1
        }

        # Debug/Execute settings

        itk_component add launchsetting {
            label $itk_component(linkpage).launchsetting \
                -text [get_indep String IDETELaunchSettingsLabel]
        }

        # Entry box for command that will launch the application

        itk_component add launchlabel {
            label $itk_component(linkpage).launchlabel \
                -text [get_indep String IDETELaunchCommand]
        }

        itk_component add launchcommand {
            entry $itk_component(linkpage).launchcommand
        } {}

        # Execute/Debug radio buttons.

        itk_component add executeradio {
            radiobutton $itk_component(linkpage).executeradio \
                -value 2 \
                -variable [itcl::scope rb_debug_state] \
                -command "$this cb_radio_launch" \
                -text [get_indep String IDETELaunchExecute] \
                -under [get_indep Pos IDETELaunchExecute]
        }

        itk_component add debugradio {
            radiobutton $itk_component(linkpage).debugradio \
                -value 1 \
                -variable [itcl::scope rb_debug_state] \
                -command "$this cb_radio_debug" \
                -text [get_indep String IDETELaunchDebug] \
                -under [get_indep Pos IDETELaunchDebug]
        }

# FIXME : this grid layout below is a mess, it needs to
# be ripped out an totally redone from scratch.

        grid rowconfigure $itk_component(linkpage) 1 -minsize 10
        grid columnconfigure $itk_component(linkpage) 0 -minsize 10

        grid $itk_component(lfilelbl) -row 2 -column 1 -sticky e
        grid $itk_component(lfileent) -row 2 -column 2 -sticky we

        grid rowconfigure $itk_component(linkpage) 3 -minsize 10

        grid $itk_component(lflagslbl) -row 4 -column 4 -sticky e
        grid $itk_component(lflagsent) -row 4 -column 5 -sticky we

        grid $itk_component(linkerlabel) -row 4 -column 1 -sticky e
        grid $itk_component(linkerentry) -row 4 -column 2 -sticky we

        grid $itk_component(enptlbl) -row 2 -column 4 -sticky e
        grid $itk_component(enptent) -row 2 -column 5 -sticky we

        grid $itk_component(linkcmdline) -row 8 -column 1 -columnspan 5 -sticky news -pady 14

        grid rowconfigure $itk_component(linkpage) 8 -weight 15

        grid $itk_component(launchsetting) -row 10 -column 1 -columnspan 5 -sticky w

        grid rowconfigure $itk_component(linkpage) 11 -minsize 7

        grid $itk_component(launchlabel) -row 12 -column 1 -columnspan 2 -sticky w
        grid $itk_component(launchcommand) -row 13 -column 1 -columnspan 5 -sticky we

        grid $itk_component(debugradio) -row 11 -column 4 -rowspan 2 -padx 10
        grid $itk_component(executeradio) -row 11 -column 5 -rowspan 2 -padx 10

        grid rowconfigure $itk_component(linkpage) 14 -minsize 10

        grid columnconfigure $itk_component(linkpage) 2 -weight 10
        grid columnconfigure $itk_component(linkpage) 5 -weight 10
        grid columnconfigure $itk_component(linkpage) 6 -minsize 10

    }

    public method SetOutputFilename {{file_name ""}} {
        if {${file_name} == ""} {
            # Let's try the build object.
            set file_name [${b_target} GetOutputFilename]
        } else {
            ${b_target} SetOutputFilename ${file_name}
        }
        # Clear current settings.
        $itk_component(lfileent) delete 0 end
        $itk_component(lfileent) insert 0 ${file_name}
    }

    public method SetUserLinkFlags {{flags ""}} {
        if {${flags} == ""} {
            # Let's check the build object.
            set flags [${b_target} GetUserLinkFlags]
        } else {
            if {${flags} == "%none%"} {
                set flags ""
            }
            ${b_target} SetUserLinkFlags ${flags}
        }

        # clear previous contents
        $itk_component(lflagsent) delete 0 end
        $itk_component(lflagsent) insert 0 ${flags}
    }

    public method SetLinkerEntryPoint {{entry_point ""}} {
        if {${entry_point} == ""} {
            # Let's check the build object.
            set entry_point [${b_target} GetLinkerEntryPoint]
        } else {
            if {${entry_point} == "%none%"} {
                set entry_point ""
            }
            ${b_target} SetLinkerEntryPoint ${entry_point}
        }

        # clear previous contents
        $itk_component(enptent) delete 0 end
        $itk_component(enptent) insert 0 ${entry_point}
    }

    public method SetLinkerExecutable {{linker ""}} {

        if {${linker} != ""} {
            # Store linker in build object
            ${b_target} SetLinkerLocation ${linker}
        }

        if {${linker} == ""} {
            # Get linker tool from the build objects ToolLocation.
            set linker [${b_target} GetLinkerLocation]
        }

        $itk_component(linkerentry) configure -value ${linker}

    }

    public method UpdateCmdLine {} {

        set tool_chain [$itk_component(tchaincbx) get]
        set tchain_obj [GetToolChainObject ${tool_chain}]

        set sel [$itk_component(buildruletable) curselection]
        if {${sel}==""} {
            # Nothing is selected, use 0 as default.
            set sel 0
        }

        set cmdline [GetCompilerCommandLine ${tchain_obj} $rulesindex(${sel})]

        # Clear widget
        $itk_component(displaycmdline) configure -state normal
        $itk_component(displaycmdline) delete 0.0 end
        $itk_component(displaycmdline) insert end ${cmdline}
        $itk_component(displaycmdline) configure -state disabled

        # Get linker 
        SetLinkerExecutable $linkerlocal

        # Update the linker command line
        set cmdline [GetLinkerCommandLine ${tchain_obj}]
        $itk_component(linkcmdline) configure -state normal
        $itk_component(linkcmdline) delete 0.0 end
        $itk_component(linkcmdline) insert end ${cmdline}
        $itk_component(linkcmdline) configure -state disabled

        HandleRuleTableState
    }

    method GetCompilerCommandLine {tool_chain rule_type} {

        # Get the tool
        set tool [${b_target} GetToolChainFlags ${rule_type} ToolLocation]
        if {${tool}==""} {
            set tool [${tool_chain} GetTool ${rule_type}]
        }

        # Get the basic action
        set basicaction [${tool_chain} GetBasicAction ${rule_type}]

        # Get the flags
        set flags ""
        foreach flagstype [list Debug Optimize Warning CodeGen] {
            set flagsetting [${b_target} GetToolChainFlags ${rule_type}\
              ${flagstype}]
            set flags "${flags} [${tool_chain} GetRuleFlags ${rule_type}\
              ${flagstype} ${flagsetting}]"
        }

        # Get user flags
        set userflags [${b_target} GetToolChainFlags ${rule_type} UserFlags]

        # Get the marco defs
        set defines_list [${b_target} GetUserDefines ${rule_type}]
        set defines_spec [${tool_chain} FormatDefines ${rule_type}\
          ${defines_list}]

        # Get the include paths
        set includes_list1 [${b_target} GetUserIncludes ${rule_type}]
        set includes_list2 [${b_target} GetAutoIncludes ${rule_type}]
        set includes_list [concat ${includes_list1} ${includes_list2}]
        set include_spec [${tool_chain} FormatIncludes ${rule_type}\
          ${includes_list}]

        return "${tool} ${basicaction} ${flags} ${userflags} ${defines_spec}\
          ${include_spec}"
    }

    public method GetLinkerCommandLine {tool_chain} {
        global sn_elix

        set linker [${b_target} GetLinkerLocation]
        set linkerflags [$itk_component(lflagsent) get]
        set basicflags [${tool_chain} GetExeLinkerRule]

        set entrypoint [$itk_component(enptent) get]
        if {${entrypoint} != ""} {
            set projecttype [${b_target} GuessProjectType]
            set entrypflag [${tool_chain} GetLinkerEntryPointFlag\
              ${projecttype}]
            set entrypoint "${entrypflag}${entrypoint}"
        }

        set objfiles [${tool_chain} GetObjectFileList\
          [${b_target} GetSourceFiles]]

        set libfiles [$itk_component(liblbx) get 0 end]

        set outputfile [$itk_component(lfileent) get]

# FIXME : this does not belong here, it should be part of toolchain settings

        # EL/IX change: for embedded systems, use `elix-link' to do
        # the link.
        if {${sn_elix} && [${tool_chain} GetIsEmbedded]} {
            global sn_options

            set prefix [file join [pwd] [${b_target} GetBuildDirectory]]/
            set mode ""
            if {[${b_target} GetLaunchWithDebugger]} {
                set mode "--mode debug"
            }

            set cmdline "elix-link --prefix ${prefix} --output ${outputfile}\
              ${mode} -- ${linker} ${entrypoint} ${linkerflags} ${objfiles}\
              ${libfiles}\n"
        } else {
# FIXME : this seems to be broken with respect to the Library option.
# If I choose library, this output is not updated but the snMakefile is correct.
            set cmdline "${linker} ${basicflags} ${outputfile} ${entrypoint}\
              ${linkerflags} ${objfiles} ${libfiles}\n"
        }

        return ${cmdline}

    }

    # Call back if the radio button mark debug in the linker rules
    # selected.
    public method cb_radio_debug {} {

        # Select debug.
        $itk_component(debugradio) select

        # unselect launch
        $itk_component(executeradio) deselect

        # Set to use debugger.
        ${b_target} SetLaunchWithDebugger 1
    }

    # Call back if the radio button mark debug in the linker rules
    # selected.
    public method cb_radio_launch {} {

        # unselect debug.
        $itk_component(debugradio) deselect

        # select launch
        $itk_component(executeradio) select

        # set not to use debugger.
        ${b_target} SetLaunchWithDebugger 0
    }

    public method SetLaunchCommandLine {} {
        $itk_component(launchcommand) delete 0 end
        $itk_component(launchcommand) insert 0 [${b_target} GetLaunchCommandLine]
    }

    public method SetLaunchDebugerInfo {{use_debugger ""}} {
        if {${use_debugger}==""} {
            # Get it from the build target.
            set use_debugger [${b_target} GetLaunchWithDebugger]
        } else {
            # Set it in the build target.
            ${b_target} SetLaunchWithDebugger ${use_debugger}
        }

        if {${use_debugger} == 1} {
            $itk_component(executeradio) deselect
            $itk_component(debugradio) select
        } else {
            $itk_component(executeradio) select
            $itk_component(debugradio) deselect
        }

    }

    public method SetLibraryFiles {{lib_files ""}} {
        if {${lib_files} == ""} {
            set lib_files [${b_target} GetLibraryFiles]
        } else {
            if {${lib_files} == "none"} {
                set lib_files ""
            }
            ${b_target} SetLibraryFiles ${lib_files}
        }

        $itk_component(liblbx) delete 0 end
        # FIXME: this is dangerous, lib names with special meaning could hose us!
        eval {$itk_component(liblbx) insert end} ${lib_files}
    }

    public method SetBuildDirectory {{dir ""}} {

        global sn_options

        if {${dir} == ""} {
            # Lets see if we can get for build target object
            set dir [${b_target} GetBuildDirectory]
        } else {
            # Lets write this data
            ${b_target} SetBuildDirectory ${dir}
        }

        if {${dir} == ""} {
            # Set the build dir to the project dir.
            set dir $sn_options(sys,project-dir)
            ${b_target} SetBuildDirectory ${dir}
        }

        $itk_component(bdirleb) configure -value ${dir}

    }

    public method SetTargetType {{type ""}} {

        if {${type} == ""} {
            # Lets see if we can get for build target object
            set type [${b_target} GetTargetType]
        } else {
            # Lets write this data
            ${b_target} SetTargetType ${type}
        }

        if {${type}==""} {
            # Use Executable as default 
            set type "Executable"
        }
        $itk_component(ttypecbx) configure -editable 1
        $itk_component(ttypecbx) delete 0 end
        $itk_component(ttypecbx) insert 0 ${type}
        $itk_component(ttypecbx) configure -editable 0
    }

    public method SetToolChain {{tools ""}} {

        if {${tools} == ""} {
            # Lets see if we can get for build target object
            set tools [${b_target} GetToolChain]
            if {${tools} != ""} {
                # Write it back initialise the tool chain field.
                # (Can cause side effects if we don't.)
                ${b_target} SetToolChain ${tools}
            }
        } else {
            # Lets write this data
            ${b_target} SetToolChain ${tools}
        }

        if {${tools} == ""} {
            # Use Executable as default 
            set tools "GNUPro (native)"
            ${b_target} SetToolChain ${tools}
        }
        $itk_component(tchaincbx) configure -editable 1
        $itk_component(tchaincbx) delete 0 end
        $itk_component(tchaincbx) insert 0 ${tools}
        $itk_component(tchaincbx) configure -editable 0
    }

    public method SetSourceFiles {{files ""}} {
        if {${files}==""} {
            # Lets see if we can get for build target object
            set files [${b_target} GetSourceFiles]
        } else {
            # Lets write this data
            ${b_target} SetSourceFiles ${tools}
        }

        if {${files}!=""} {
            $itk_component(srctree) delete 0 end
            $itk_component(srctree) insert 0 -text Files
            fill_file_tree -widget $itk_component(srctree) -contents ${files} -parent end
        }
    }

    public method SetBuildRules {{cls ""} {dummy ""}} {
        # Get toolchain and select the right spex.

	set tchain_obj [GetToolChainObject [$itk_component(tchaincbx) get]]

        set rules [${tchain_obj} GetRulesList]
        set rulenum -1

	# Store off selected rule (if any).
	# We can try to look neat by selecting this rule afterwards.
	set selected [$itk_component(buildruletable) curselection]

# FIXME: ARRRAGSHS$#4@!!!, we should not be using internal widget names
        # Clean rule table
        $itk_component(buildruletable).tree delete 0 end

        # Write rules...
        foreach rule ${rules} {
            incr rulenum
            set rulesindex(${rulenum}) ${rule}
            set finfo [${tchain_obj} GetFileInfo ${rule}]
            set ginfo [${tchain_obj} GetGeneralInfo ${rule}]
            set rule_status [${b_target} GetRuleStatus ${rule}]
            set info "\{${rule_status}\t${finfo}\t${ginfo}\}"
            $itk_component(buildruletable) insert end ${info}
        }

	# Select the last selected rule (if we can).
	catch {$itk_component(buildruletable) selection set $selected}
    }

    public method rules_tab_selected_hack {} {
        $itk_component(buildruletable) start_motion 2 2
        $itk_component(buildruletable) motion 2 3
        $itk_component(buildruletable) resize 2 2
    }

    public method cb_rule_togle {} {
        set tool_chain [$itk_component(tchaincbx) get]
        set tchain_obj [GetToolChainObject ${tool_chain}]

        set sel [$itk_component(buildruletable) curselection]
        if {[${b_target} GetRuleStatus $rulesindex(${sel})]=="Enabled"} {
            ${b_target} SetRuleStatus $rulesindex(${sel}) "Disabled"
        } else {
            ${b_target} SetRuleStatus $rulesindex(${sel}) "Enabled"
        }

        SetBuildRules
        $itk_component(buildruletable) selection set ${sel}
        HandleRuleTableState
    }

    public method HandleRuleTableState {} {
        set tool_chain [$itk_component(tchaincbx) get]
        set tchain_obj [GetToolChainObject ${tool_chain}]

        set sel [$itk_component(buildruletable) curselection]
        if {${sel} == ""} {
            # If nothing is selected then disable togle button.
            $itk_component(buildruledisable) configure -state disabled
            $itk_component(buildruleedit) configure -state disabled
        } else {
            $itk_component(buildruledisable) configure -state normal
            $itk_component(buildruleedit) configure -state normal

            if {[${b_target} GetRuleStatus $rulesindex(${sel})] == "Enabled"} {
                $itk_component(buildruledisable) configure \
                    -text [get_indep String IDETEBuildRuleDisable] \
                    -under [get_indep Pos IDETEBuildRuleDisable]
            } else {
                $itk_component(buildruledisable) configure \
                    -text [get_indep String IDETEBuildRuleEnable] \
                    -under [get_indep Pos IDETEBuildRuleEnable]
            }
        }
    }

    public method ruletable_double_cb {} {
        set tool_chain [$itk_component(tchaincbx) get]
        set tchain_obj [GetToolChainObject ${tool_chain}]

        set sel [$itk_component(buildruletable) curselection]

        if {${NewTarget} == "1"} {
            # Don't generate the Auto Include paths again.
            set NewTarget ""
            ${b_target} SetAutoIncludes $rulesindex(${sel})\
              [GenerateIncludePaths ${src_file_list}]
        }
        set ttdlg [snIdeToolsDlg .toolchaindlg -toolchain ${tchain_obj}\
          -rule $rulesindex(${sel}) -buildtarget [itcl::code ${b_target}]]

	$ttdlg activate

	itcl::delete object $ttdlg

        UpdateCmdLine
        HandleRuleTableState
    }

    public method HandleLibsTabState {} {

        set selected [$itk_component(liblbx) curselect]
        if {${selected}==""} {
            $itk_component(libadd) configure -state normal
            $itk_component(libdel) configure -state disabled
            $itk_component(libup) configure -state disabled
            $itk_component(libdown) configure -state disabled
        } else {
            $itk_component(libadd) configure -state normal
            $itk_component(libdel) configure -state normal

            # If the 1st lib is selected disabled "Move up"
            if {${selected} == "0"} {
                $itk_component(libup) configure -state disabled
            } else {
                $itk_component(libup) configure -state normal
            }

            # If the last lib is selected disabled "Move down"
            set tmpsel ${selected}
            incr tmpsel
            if {${tmpsel} == [$itk_component(liblbx) size]} {
                $itk_component(libdown) configure -state disabled
            } else {
                $itk_component(libdown) configure -state normal
            }
        }
    }

    public method cb_double_click_lib {} {
        HandleLibsTabState
    }

    public method cb_add_lib {} {
        set libfiles [Editor&::FileDialog $itk_component(hull)]
	# FIXME : this is dangerous, a file named foo[exit]bar.txt could exit the IDE!
        eval {$itk_component(liblbx) insert end} $libfiles
        HandleLibsTabState
    }

    public method cb_del_lib {} {
        set selected [$itk_component(liblbx) curselect]
        $itk_component(liblbx) delete ${selected}
        HandleLibsTabState
    }

    public method cb_up_lib {} {
        set selected [$itk_component(liblbx) curselect]
        set libfile [$itk_component(liblbx) get ${selected}]
        $itk_component(liblbx) delete ${selected}
        set selected [expr "${selected} - 1"]
        $itk_component(liblbx) insert ${selected} ${libfile}
        $itk_component(liblbx) selection set ${selected}
        HandleLibsTabState
    }

    public method cb_down_lib {} {
        set selected [$itk_component(liblbx) curselect]
        set libfile [$itk_component(liblbx) get ${selected}]
        $itk_component(liblbx) delete ${selected}
        incr selected
        $itk_component(liblbx) insert ${selected} ${libfile}
        $itk_component(liblbx) selection set ${selected}
        HandleLibsTabState
    }

    public method ok_cb {} {

        #TODO: check form is completed enough

        # Generate the Auto include paths if it has been done yet.
        if {${NewTarget} == "1"} {
            # Don't generate the Auto Include paths again.
            set NewTarget ""
            set sel [$itk_component(buildruletable) curselection]

            ${b_target} SetAutoIncludes $rulesindex(${sel})\
              [GenerateIncludePaths ${src_file_list}]
        }

	# Call custom remove GUI proc.
	# NB: The user should save any custom data here.
	RemoveCustomGUI

        SaveData

	deactivate 1
    }

    public method SaveData {} {
        # Write Data to build target object

        SetBuildDirectory [$itk_component(bdirleb) cget -value]
        SetToolChain [$itk_component(tchaincbx) get]
        SetTargetType [$itk_component(ttypecbx) get]

        # Pass "none" if no libraries are listed.
        if {[$itk_component(liblbx) get 0 end] == ""} {
            SetLibraryFiles none
        } else {
            SetLibraryFiles [$itk_component(liblbx) get 0 end]
        }

        SetOutputFilename [$itk_component(lfileent) get]

        # Pass "%none%" if no entry point.
        if {[$itk_component(enptent) get] == ""} {
            SetLinkerEntryPoint "%none%"
        } else {
            SetLinkerEntryPoint [$itk_component(enptent) get]
        }

        # Pass "%none%" if no user flags.
        if {[$itk_component(lflagsent) get] == ""} {
            SetUserLinkFlags "%none%"
        } else {
            SetUserLinkFlags [$itk_component(lflagsent) get]
        }

        # Save Launch command line
        ${b_target} SetLaunchCommandLine [$itk_component(launchcommand) get]

        # Save linker tool
        SetLinkerExecutable $linkerlocal

        ${b_target} SaveData
    }

    public method cancel_cb {} {
	deactivate 0
    }

    public method DestroyDialog {} {
        itcl::delete object ${b_target}
    }

}


