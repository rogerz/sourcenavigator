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
## Window with all availiable tools by SN
## Copyright (c) 1999 Cygnus Solutions, Inc.
##
################################################

## Short cuts:
## edit     == editor
## classbr  == class browser
## ctree    == Class Hierarchy
## xref     == cross reference
## incbr    == include browser
## dbg      == Debug
## retr     == Retriever
## grep     == Grep
## make     == Make
#global for the hole project

itcl::class MultiWindow& {
    global sn_options sn_elix

    inherit sourcenav::Window

    constructor {args} {
        global sn_options

	# Setup some default behavior for the MultiWindow.

        ${this} withdraw
        ${this} on_close "${this} windows_close dummy"
        # Don't close window by hitting Escape.
        ${this} bind_tk <Escape> {;}

        # Init variables to nagivate between view points.
        set view_positions(next) ""
        set view_positions(prev) ""

	eval itk_initialize $args

        # Set initial size based on user prefs.

        set height [expr {int([winfo screenheight .] *
            ($sn_options(def,window-size)*0.01))}]

        set width [expr {int([winfo screenwidth .] *
            ($sn_options(def,window-size)*0.01))}]

        $this configure -geometry ${width}x${height}

        # Add menu.
        AddMenu

        # Add the toolbar.
        AddToolbar

        # Add statusbar.
        AddStatusbar

	# Add the main notebook.
	itk_component add notebook {
	    tixNoteBook $itk_component(hull).nbook -ipadx 0 -ipady 0
	} { }

	# Configure the big bad notebook widget.
        set NotebookFrame [$itk_component(notebook) subwidget nbframe]
        ${NotebookFrame} config -inactivebackground lightgray

	# Add the pages to the notebook widget.
        $itk_component(notebook) add edit\
                -raisecmd [itcl::code ${this} RaiseEditor]\
		-label [get_indep String MultiEditor]\
		-under [get_indep Pos MultiEditor]\
		-state [tool_Exists edit]

	AddEditor $itk_component(notebook) edit

        $itk_component(notebook) add ctree\
		-raisecmd [itcl::code ${this} RaiseClassHierarchy]\
		-image tree_image\
		-label [get_indep String MultiClassHierarchy]\
		-under [get_indep Pos MultiClassHierarchy]\
		-state [tool_Exists ctree]

	AddClassHierarchy $itk_component(notebook) ctree

        $itk_component(notebook) add classbr\
		-raisecmd [itcl::code ${this} RaiseClassBrowser]\
                -label [get_indep String MultiClass]\
		-under [get_indep Pos MultiClass]\
		-state [tool_Exists classbr]

	AddClassBrowser $itk_component(notebook) classbr

        $itk_component(notebook) add xref \
		-raisecmd [itcl::code ${this} RaiseXReference]\
		-label [get_indep String MultiXRef]\
		-under [get_indep Pos MultiXRef]\
		-state [tool_Exists xref]

	AddXReference $itk_component(notebook) xref

        $itk_component(notebook) add incbr\
		-raisecmd [itcl::code ${this} RaiseInclude]\
	        -label [get_indep String MultiInclude]\
		-under [get_indep Pos MultiInclude]\
		-state [tool_Exists incbr]

	AddInclude $itk_component(notebook) incbr

        $itk_component(notebook) add retr\
		-raisecmd [itcl::code ${this} RaiseRetriever]\
		-label [get_indep String MultiRetriever]\
		-under [get_indep Pos MultiRetriever]\
		-state [tool_Exists retr]

	AddRetriever $itk_component(notebook) retr

        $itk_component(notebook) add grep \
            -raisecmd [itcl::code ${this} RaiseGrep] \
            -label [get_indep String MultiGrep] \
            -under [get_indep Pos MultiGrep] \
            -state [tool_Exists grep]

	AddGrep $itk_component(notebook) grep

        pack $itk_component(notebook) -fill both -side left -expand y -anchor nw

        $itk_component(notebook) raise $itk_option(-raise)

        #call user function
        catch {sn_rc_mainwindow $itk_component(hull) $itk_component(menu)}
        update idletasks
        after idle "${this} deiconify"
    }

    destructor {
        foreach v [::info globals "${this}-*"] {
            catch {uplevel #0 unset ${v}}
        }
    }

    protected variable Activate_executed 0

    # Activate different notebook page:
    # save filter options for the actual tool and
    # restore those for the activated tool (notebook page)

    method Activate {page paned pane tool} {
	# FIXME: Shouldn't this be in sn_options array?
        global Switch_Is_Enabled

        upvar #0 ${SymbolsFilter}-related related
        upvar #0 ${tool}-related toolrel

        incr Activate_executed

        set old_view ${ActiveWidget}

        #save related for now active tool
        if {${ActiveWidget} != ""} {
            upvar #0 ${ActiveWidget}-related rel
            set rel ${related}

            #dump current position
            history_stack_add_point ${ActiveWidget}
        }

        #save filter flags for the active widget (radios and checkbuttons)
        foreach btn [eval list $AllFilterOptions(radio)\
          $AllFilterOptions(checkbutton)] {
            set variable [${btn} cget -variable]
            upvar #0 ${variable} value
            upvar #0 ${ActiveWidget}-Filter(${btn}) var
            set var ${value}
        }

        #get selection from current tool, only when keep
        #is seted or the global variable to disable switching
        #is on
        if {${keep} && ${Switch_Is_Enabled} > 0 && ${ActiveWidget} != ""} {
            set sel [${ActiveWidget} Selection]
        } else {
            set sel ""
        }


        #deactivate current notepad
	if {$ActiveWidget != ""} {
	    $ActiveWidget deactivate
	}

        #hide editor buttons, we must hide it first,
        #to enable the icons of the next view as first
        #and if an editor is availiable we put the toolbar
        #buttons on the far right side of the toolbar
        if {[${tool} whoami] != "edit"} {
            pack forget ${FindEditFr}
        }

        #restore the information about related
        if {[catch {set related ${toolrel}}]} {
            if {[${tool} whoami] == "edit"} {
                set related 1
            } else {
                set related 0
            }
        }

        set ActiveWidget ${tool}
        set ActivePage ${page}
        set ActivePaned ${paned}
        set ActivePane ${pane}

        #restore the values of the filter checkbuttons and radios
        foreach btn [eval list $AllFilterOptions(radio)\
          $AllFilterOptions(checkbutton)] {
            catch {
                set variable [${btn} cget -variable]
		upvar #0 ${variable} var
                upvar #0 ${ActiveWidget}-Filter(${btn}) value
                set var ${value}
            }
        }

        # Delete contents of drop down list
        $itk_component(symbolcombo) configure -contents ""
        $itk_component(symbolcombo) selecttext ""

        ${ActiveWidget} activate
        #if there is a selection, goto it
        if {${sel} != ""} {
            ::eval ${ActiveWidget} gotosymbol ${sel} 0
        }
        ${ActiveWidget} SetTitle
        ${ActiveWidget} Focus

        #verify if we have an editor to display the toolbar
        #icons
        DeActivate_Editor_buttons

        #call user function
        catch {sn_rc_changeview ${this} ${ActiveWidget} ${old_view}}
    }

    #View toolbar buttons for the editor, if an editor is
    #availiable
    method DeActivate_Editor_buttons {} {
        if {[list_find_editor ${ActiveWidget}] != ""} {
            pack ${FindEditFr} -side left
        } else {
            pack forget ${FindEditFr}
        }
    }

    #####################################################
    # create editor
    #####################################################
    method AddEditor {nb page} {
        global sn_options

	itk_component add editor_page {
	    ${nb} subwidget ${page}
	} { }

        $itk_component(editor_page) configure -bg white
      
        itk_component add editorpaned {
            tixPanedWindow $itk_component(editor_page).paned\
		    -paneborderwidth 0\
		    -orientation $sn_options(def,window-alighment)
	} { }

        itk_component add editorpane {
	    $itk_component(editorpaned) add edit -size 500
	} { }

	itk_component add editor {
	    Editor& $itk_component(editorpane).editor\
		    -symbols $itk_component(symbolcombo)\
		    -symbols_filter ${SymbolsFilter}\
		    -menu $itk_component(menu)\
		    -toolbar ""\
                    -mesg_area $itk_component(message)\
		    -message_var [itcl::scope message]\
		    -linenumber_var [itcl::scope linenum] \
		    -filename ${symbolname}\
		    -findcombo ${FindCombo} \
                    -parent $this
	} { }

        pack $itk_component(editor) -fill both -expand y
        pack $itk_component(editorpaned) -fill both -expand y

        lappend AvailTools $itk_component(editor)

	# Just creating it, might not want to use it just yet.
	$itk_component(editor) deactivate
    }

    method RaiseEditor {} {
        Activate $itk_component(editor_page) $itk_component(editorpaned)\
		$itk_component(editorpane) $itk_component(editor)
    }

    #####################################################
    # Class hierarchy
    #####################################################
    method AddClassHierarchy {nb page} {
        global sn_options

	itk_component add classtree_page {
	    ${nb} subwidget ${page}
	} { }

	itk_component add classtreepaned {
	    tixPanedWindow $itk_component(classtree_page).paned\
		    -paneborderwidth 0\
		    -orientation $sn_options(def,window-alighment)
	} { }

	itk_component add classtreepane {
	    $itk_component(classtreepaned) add ctree
	} { }

	itk_component add classtree {
	    ClassTree& $itk_component(classtreepane).classtree\
		    -symbols $itk_component(symbolcombo)\
		    -symbols_filter $SymbolsFilter\
		    -menu $itk_component(classtree_menu)\
		    -toolbar $itk_component(toolbar)\
		    -message_var [itcl::scope message]\
		    -doubleclickcommand "${this} ClassHierarchyEdit" \
		    -parent $this
	} { }

        pack $itk_component(classtree) -fill both -expand y
        pack $itk_component(classtreepaned) -fill both -expand y

        lappend AvailTools $itk_component(classtree)

	# Just creating it, might not want to use it just yet.
	$itk_component(classtree) deactivate
    }

    method RaiseClassHierarchy {} {
        Activate $itk_component(classtree_page) $itk_component(classtreepaned)\
		$itk_component(classtreepane) $itk_component(classtree)
    }

    method ClassHierarchyEdit {{scope ""} {sym ""} {cls ""} {file ""} {from\
      ""} {type ""} {prm ""} {to ""} {always 1}} {
        sn_retrieve_symbol ${sym} ${scope}
    }

    #####################################################
    # Class Browser
    #####################################################
    method AddClassBrowser {nb page} {
        global sn_options

	itk_component add class_page {
	    ${nb} subwidget ${page}
	} { }

        itk_component add classpaned {
	    tixPanedWindow $itk_component(class_page).paned\
		    -paneborderwidth 0\
		    -orientation $sn_options(def,window-alighment)
	} { }

        itk_component add classpane {
	    $itk_component(classpaned) add classbr
	} { }

	itk_component add classbrowser {
	    Class& $itk_component(classpane).classbrowser\
		    -symbols $itk_component(symbolcombo)\
		    -symbols_filter ${SymbolsFilter}\
		    -menu $itk_component(classbrowser_menu)\
		    -toolbar $itk_component(toolbar)\
		    -mesg_area $itk_component(message)\
		    -class_doubleclickcommand\
		     [itcl::code ${this} ClassBrowserEditClass]\
		    -doubleclickcommand [itcl::code ${this} EditObject] \
		    -parent $this
	} { }

        pack $itk_component(classbrowser) -fill both -expand y
        pack $itk_component(classpaned) -fill both -expand y

        lappend AvailTools $itk_component(classbrowser)

	# Just creating it, might not want to use it just yet.
	$itk_component(classbrowser) deactivate
    }

    method RaiseClassBrowser {} {
        Activate $itk_component(class_page) $itk_component(classpaned)\
		$itk_component(classpane) $itk_component(classbrowser)
    }

    method ClassBrowserEditClass {class} {
        sn_retrieve_symbol ${class} "cl" "" -beg 1 1
    }

    method EditObject {{scope ""} {sym ""} {cls ""} {file ""} {from ""} {type\
      ""} {prm ""} {to ""} {always 1}} {
        global sn_sep

        #to look in the database, use sn_sep
        set lbl [string trim "${cls}${sn_sep}${sym}" " \t${sn_sep}"]

        if {${lbl} == "" && ${file} == ""} {
            bell
            return
        }

        #we must try to find the correct symbol, so we have to
        #accept the parameter list also.
        set cnt [read_matched_from_db "" ${scope} -exact ${lbl} ${type} ${prm}\
          ${file} ${from} ${to} 0]
        if {${cnt} == "" && ${file} == ""} {
            set cnt [read_matched_from_db "" ${scope} -exact ${lbl}]
        }
        if {${cnt} != ""} {
            set file [lindex [split [lindex ${cnt} 0] \t] 4]
            set from [lindex [split [lindex ${cnt} 0] \t] 5]
        }
        if {${file} != ""} {
            #use blank as separator
            set lbl [string trim "${cls} ${sym}"]
            sn_edit_file [list 0 ${lbl} ${scope}] ${file} ${from} 1
        } else {
            bell
            return
        }
    }

    #####################################################
    # X reference
    #####################################################
    method AddXReference {nb page} {
        global sn_options

	itk_component add xref_page {
	    ${nb} subwidget ${page}
	} { }

	itk_component add xrefpaned {
	    tixPanedWindow $itk_component(xref_page).paned\
		    -paneborderwidth 0\
		    -orientation $sn_options(def,window-alighment)
	} { }

	itk_component add xrefpane {
	    $itk_component(xrefpaned) add xref
	} { }

	itk_component add xref {
	    XRef& $itk_component(xrefpane).xref\
		    -symbols $itk_component(symbolcombo)\
		    -symbols_filter ${SymbolsFilter}\
		    -menu $itk_component(xref_menu)\
		    -toolbar $itk_component(toolbar)\
		    -message_var [itcl::scope message]\
		    -doubleclickcommand [itcl::code ${this} EditObject] \
		    -parent ${this}
	} { }

        pack $itk_component(xref) -fill both -expand y
        pack $itk_component(xrefpaned) -fill both -expand y

        lappend AvailTools $itk_component(xref)

	# Just creating it, might not want to use it just yet.
	$itk_component(xref) deactivate
    }

    method RaiseXReference {} {
        Activate $itk_component(xref_page) $itk_component(xrefpaned)\
		$itk_component(xrefpane) $itk_component(xref)
    }

    #####################################################
    # Include
    #####################################################
    method AddInclude {nb page} {
        global sn_options

	itk_component add include_page {
	    ${nb} subwidget ${page}
	} { }

	itk_component add includepaned {
	    tixPanedWindow $itk_component(include_page).paned\
		    -paneborderwidth 0\
		    -orientation $sn_options(def,window-alighment)
	} { }

	itk_component add includepane {
	    $itk_component(includepaned) add inc
	} { }

        itk_component add include {
	    Include& $itk_component(includepane).incbr\
		    -symbols $itk_component(symbolcombo)\
		    -symbols_filter ${SymbolsFilter}\
		    -menu $itk_component(include_menu)\
		    -toolbar $itk_component(toolbar)\
		    -mesg_area $itk_component(message) \
		    -parent $this
	} { }

        pack $itk_component(include) -fill both -expand y
        pack $itk_component(includepaned) -fill both -expand y

        lappend AvailTools $itk_component(include)

	# Just creating it, might not want to use it just yet.
	$itk_component(include) deactivate
    }

    method RaiseInclude {} {
        Activate $itk_component(include_page) $itk_component(includepaned)\
		$itk_component(includepane) $itk_component(include)
    }

    #####################################################
    # Retriever
    #####################################################
    method AddRetriever {nb page} {
        global sn_options

	itk_component add retriever_page {
	    ${nb} subwidget ${page}
	} { }

	itk_component add retrieverpaned {
	    tixPanedWindow $itk_component(retriever_page).paned\
		    -paneborderwidth 0\
		    -orientation $sn_options(def,window-alighment)
	} { }

	itk_component add retrieverpane {
	    $itk_component(retrieverpaned) add retr
	} { }

        itk_component add retriever {
	    Retr& $itk_component(retrieverpane).retr\
		    -symbols $itk_component(symbolcombo)\
		    -symbols_filter ${SymbolsFilter}\
		    -menu RetrieverMenu\
		    -toolbar $itk_component(toolbar)\
		    -mesg_area $itk_component(message) \
		    -parent $this
	} { }

        pack $itk_component(retriever) -fill both -expand y
        pack $itk_component(retrieverpaned) -fill both -expand y

        lappend AvailTools $itk_component(retriever)

	# Just creating it, might not want to use it just yet.
	$itk_component(retriever) deactivate
    }

    method RaiseRetriever {} {
        Activate $itk_component(retriever_page) $itk_component(retrieverpaned)\
		$itk_component(retrieverpane) $itk_component(retriever)
    }

    method retr {} {
        return $itk_component(retriever)
    }

    #####################################################
    # Grep
    #####################################################
    method AddGrep {nb page} {
        global sn_options

	itk_component add grep_page {
	    ${nb} subwidget ${page}
	} {}

	itk_component add greppaned {
	    tixPanedWindow $itk_component(grep_page).paned \
		    -paneborderwidth 0 \
		    -orientation $sn_options(def,window-alighment)
	} {}

        itk_component add greppane {
	    $itk_component(greppaned) add grep
	}

        itk_component add grep {
	    Grep $itk_component(greppane).grep \
		    -symbols $itk_component(symbolcombo) \
		    -symbols_filter ${SymbolsFilter} \
		    -menu GrepMenu \
		    -toolbar $itk_component(toolbar) \
		    -mesg_area $itk_component(message) \
		    -parent $this
	} {}

        pack $itk_component(grep) -fill both -expand y
        pack $itk_component(greppaned) -fill both -expand y

        lappend AvailTools $itk_component(grep)

	# Just creating it, might not want to use it just yet.
	$itk_component(grep) deactivate
    }

    method RaiseGrep {} {
        Activate $itk_component(grep_page) $itk_component(greppaned)\
		$itk_component(greppane) $itk_component(grep)
    }

    #####################################################
    # Make
    #####################################################
    # FIXME: This currently isn't used, but it should probably
    #        be used (at some point).

    method AddMake {nb page} {
        global sn_options

        itk_component add make_page {
	    ${nb} subwidget ${page}
	} { }

	itk_component add makepaned {
	    tixPanedWindow $itk_component(make_page).paned\
		    -paneborderwidth 0\
		    -orientation $sn_options(def,window-alighment)
	} { }

	itk_component add makepane {
	    $itk_component(makepaned) add make
	} { }

        itk_component add make {
	    Make $itk_component(makepane).make\
		    -symbols $itk_component(symbolcombo)\
		    -symbols_filter ${SymbolsFilter}\
		    -menu MakeMenu\
		    -toolbar $itk_component(toolbar)\
		    -mesg_area $itk_component(message)
	} { }
        pack $itk_component(make) -fill both -expand y
        pack $itk_component(makepaned) -fill both -expand y

        lappend AvailTools $itk_component(make)
    }

    method RaiseMake {} {
        Activate $itk_component(make_page) $itk_component(makepaned)\
		$itk_component(makepane) $itk_component(make)
    }

    method AddMenu {} {
        global sn_options

        set state normal

	itk_component add menu {
	    menu $itk_component(hull).menu -tearoff 0
	} {}

        ## File menu
        ####################
	
	itk_component add file_menu {
	    menu $itk_component(menu).file -tearoff 0 -postcommand\
		    [itcl::code ${this} file_post $itk_component(menu).file]
	} {}

        $itk_component(file_menu) configure -font $sn_options(def,layout-font)

        $itk_component(menu) add cascade -label [get_indep String EditFile]\
		-menu $itk_component(file_menu) -underline [get_indep Pos EditFile]

        $itk_component(file_menu) add command\
		-label [get_indep String EditNewFile]\
		-underline [get_indep Pos EditNewFile] -accelerator "Ctrl+N"\
		-command [itcl::code ${this} file_new]

        $itk_component(file_menu) add command\
		-label [get_indep String Open]\
		-underline [get_indep Pos Open] -accelerator "Ctrl+O"\
		-command [itcl::code ${this} file_open]

        $itk_component(file_menu) add separator

        #Add project menu
        AddProjectMenuEntries $itk_component(file_menu) ${this} 1

        $itk_component(file_menu) add separator

        #File commands
        $itk_component(file_menu) add command\
		-accelerator "Ctrl+S"\
		-label [get_indep String EditSave]\
		-underline [get_indep Pos EditSave]\
		-command "[itcl::code ${this} file_save]"

        $itk_component(file_menu) add command\
		-label [get_indep String EditSaveFileAs]\
		-underline [get_indep Pos EditSaveFileAs]\
		-command [itcl::code ${this} file_saveas]

        $itk_component(file_menu) add command\
		-accelerator "$sn_options(sys,alt-accelpref)+S"\
		-label [get_indep String EditFastSave]\
		-underline [get_indep Pos EditFastSave]\
		-command "[itcl::code ${this} file_fastsave]"

        $itk_component(file_menu) add command\
		-accelerator "$sn_options(sys,alt-accelpref)+A"\
		-label [get_indep String EditSaveAll]\
		-underline [get_indep Pos EditSaveAll]\
		-command "[itcl::code ${this} file_saveall]"

        $itk_component(file_menu) add command\
		-label [get_indep String Revert]\
		-underline [get_indep Pos Revert]\
		-command [itcl::code ${this} file_revert]

        $itk_component(file_menu) add separator

        $itk_component(file_menu) add command\
		-label [get_indep String Print]\
		-accelerator "Ctrl+P" -underline [get_indep Pos Print]\
		-command [itcl::code ${this} file_print]

        $itk_component(file_menu) add separator

        #close window
        $itk_component(file_menu) add command\
		-command [itcl::code ${this} windows_close $itk_component(file_menu)]\
		-label [get_indep String WindowsClose] -accelerator "Ctrl+W"\
		-underline [get_indep Pos WindowsClose]

        $itk_component(file_menu) add command -label [get_indep String Exit]\
		-underline [get_indep Pos Exit] -accelerator "Ctrl+Q"\
		-command " sn_exit "

        ## Edit menu
        #############

	itk_component add edit_menu {
	    menu $itk_component(menu).edit -tearoff 0\
		    -postcommand [itcl::code ${this} edit_post $itk_component(menu).edit]
	} {}

        $itk_component(edit_menu) configure -font $sn_options(def,layout-font)

        $itk_component(menu) add cascade -label [get_indep String EditEdit]\
		-menu $itk_component(edit_menu) -underline [get_indep Pos EditEdit]

        $itk_component(edit_menu) add command\
		-label [get_indep String EditUndo]\
		-underline [get_indep Pos EditUndo] -accelerator "Ctrl+Z"\
		-command [itcl::code ${this} edit_undo] -state ${state}

        $itk_component(edit_menu) add command\
		-label [get_indep String EditRedo]\
		-underline [get_indep Pos EditRedo] -accelerator "Ctrl+Y"\
		-command [itcl::code ${this} edit_redo] -state ${state}

        $itk_component(edit_menu) add separator

        $itk_component(edit_menu) add command\
		-label [get_indep String EditCut]\
		-underline [get_indep Pos EditCut] -accelerator "Ctrl+X"\
		-command [itcl::code ${this} edit_cut] -state ${state}

        $itk_component(edit_menu) add command\
		-label [get_indep String EditCopy]\
		-underline [get_indep Pos EditCopy] -accelerator "Ctrl+C"\
		-command [itcl::code ${this} edit_copy]

        $itk_component(edit_menu) add command\
		-label [get_indep String EditPaste]\
		-underline [get_indep Pos EditPaste] -accelerator "Ctrl+V"\
		-state ${state}\
		-command [itcl::code ${this} edit_paste]

        $itk_component(edit_menu) add command\
		-label [get_indep String EditDelete]\
		-underline [get_indep Pos EditDelete] -accelerator "Ctrl+D"\
		-command [itcl::code ${this} edit_delete] -state ${state}

        $itk_component(edit_menu) add command\
		-label [get_indep String SelectAll]\
		-underline [get_indep Pos SelectAll]\
		-command [itcl::code ${this} edit_selectall]

        $itk_component(edit_menu) add command\
		-label [get_indep String EditClear]\
		-underline [get_indep Pos EditClear]\
		-command [itcl::code ${this} edit_clear]\
		-state ${state}

        $itk_component(edit_menu) add separator

        $itk_component(edit_menu) add command\
		-label [get_indep String EditInsertFile]\
		-underline [get_indep Pos EditInsertFile]\
		-command [itcl::code ${this} edit_insert]

        $itk_component(edit_menu) add separator

        $itk_component(edit_menu) add command\
		-label [get_indep String EditIdent]\
		-underline [get_indep Pos EditIdent] -accelerator "Ctrl+>"\
		-command "[itcl::code ${this} edit_indent indent]"

        $itk_component(edit_menu) add command\
		-label [get_indep String EditOutdent]\
		-underline [get_indep Pos EditOutdent] -accelerator "Ctrl+<"\
		-command "[itcl::code ${this} edit_indent outdent]"

        $itk_component(edit_menu) add separator

        $itk_component(edit_menu) add command\
		-label [get_indep String EditPreferences]\
		-underline [get_indep Pos EditPreferences]\
		-command [itcl::code ${this} edit_preferences 0]

        #Search menu
        itk_component add search_menu {
	    menu $itk_component(menu).search -tearoff 0\
		    -postcommand [itcl::code ${this} search_post\
		     $itk_component(menu).search]
	} {}

        $itk_component(search_menu) configure -font $sn_options(def,layout-font)

        $itk_component(menu) add cascade -label [get_indep String Search]\
		-underline [get_indep Pos Search]\
		-menu $itk_component(search_menu)

        $itk_component(search_menu) add command\
		-label [get_indep String SearchFind]\
		-underline [get_indep Pos SearchFind] -accelerator "Ctrl+F"\
		-command [itcl::code ${this} search_findtext]

        $itk_component(search_menu) add command\
		-label [get_indep String SearchNext]\
		-underline [get_indep Pos SearchNext] -accelerator "F3"\
		-command [itcl::code ${this} search_next]

        $itk_component(search_menu) add command\
		-label [get_indep String SearchPrev]\
		-underline [get_indep Pos SearchPrev] -accelerator "Shift+F3"\
		-command [itcl::code ${this} search_prev]

        $itk_component(search_menu) add command\
		-label [get_indep String SearchReplace]\
		-underline [get_indep Pos SearchReplace]\
		-command [itcl::code ${this} search_replace] -state ${state}

        $itk_component(search_menu) add separator

        $itk_component(search_menu) add command\
		-accelerator "$sn_options(sys,alt-accelpref)+D" -label\
		[get_indep String SearchDefinition]\
		-underline [get_indep Pos SearchDefinition]\
		-command [itcl::code ${this} search_definition]\
		-state ${state}

        $itk_component(search_menu) add command\
		-accelerator "$sn_options(sys,alt-accelpref)+I"\
		-label [get_indep String SearchImplementation]\
		-underline [get_indep Pos SearchImplementation]\
		-command [itcl::code ${this} search_implementation]\
		-state ${state}

        $itk_component(search_menu) add separator

        $itk_component(search_menu) add command\
		-accelerator "Ctrl+Shift+G"\
		-label [get_indep String SearchGrep]\
		-underline [get_indep Pos SearchGrep]\
		-command "${this} search_grep"

        $itk_component(search_menu) add separator

        #Add a separate Goto sub menu
        itk_component add goto_menu {
	    menu $itk_component(search_menu).goto -tearoff 0\
		    -postcommand [itcl::code ${this} search_goto_post\
		     $itk_component(search_menu).goto]
	} {}

        $itk_component(search_menu) add cascade\
		-label [get_indep String Goto]\
		-underline [get_indep Pos Goto]\
		-menu $itk_component(goto_menu)

        $itk_component(goto_menu) add command\
		-label [get_indep String SearchGotoLine]\
		-underline [get_indep Pos SearchGotoLine]\
		-accelerator "Ctrl+G"\
		-command [itcl::code ${this} search_gotoline]

        $itk_component(goto_menu) add separator

        $itk_component(goto_menu) add command\
		-label [get_indep String SearchSetMark]\
		-underline [get_indep Pos SearchSetMark]\
		-accelerator "Ctrl+Space"\
		-command [itcl::code ${this} search_setmark]

        $itk_component(goto_menu) add command\
		-label [get_indep String SearchGotoMark]\
		-underline [get_indep Pos SearchGotoMark]\
		-accelerator "Ctrl+M"\
		-command [itcl::code ${this} search_gotomark]

# FIXME: targeted for removal

        $itk_component(goto_menu) add command\
		-label [get_indep String SearchGotoError]\
		-underline [get_indep Pos SearchGotoError]\
		-accelerator "Shift+Ctrl+E"\
		-command "MultiWindow&::search_gotoerror"


        ## Tools menu column
	itk_component add tools_menu {
	    menu $itk_component(menu).tools -tearoff 0\
		    -postcommand [itcl::code ${this} tools_post\
		     $itk_component(menu).tools]
	} {}

        $itk_component(tools_menu) configure -font $sn_options(def,layout-font)

        $itk_component(menu) add cascade\
		-label [get_indep String Tools]\
		-menu $itk_component(tools_menu)\
		-underline [get_indep Pos Tools]

        ############################################################
        ## Editor
        ############################################################

        ############################################################
        ## Class Hierarchy
        ############################################################

	itk_component add classtree_menu {
	    menu $itk_component(tools_menu).ctree -tearoff 0\
		    -postcommand [itcl::code ${this} tools_ctree_post\
		     $itk_component(tools_menu).ctree]
	} {}

        $itk_component(tools_menu) add cascade\
		-label [get_indep String MultiClassHierarchy]\
		-menu $itk_component(classtree_menu)\
		-underline [get_indep Pos MultiClassHierarchy]\
		-state disabled

        ############################################################
        ## Class Browser
        ############################################################

	itk_component add classbrowser_menu {
	    menu $itk_component(tools_menu).class -tearoff 0\
		    -postcommand [itcl::code ${this} tools_classbr_post\
		     $itk_component(tools_menu).class]
	} {}

        $itk_component(tools_menu) add cascade\
		-label [get_indep String MultiClass]\
		-menu $itk_component(classtree_menu)\
		-underline [get_indep Pos MultiClass] -state disabled

        ############################################################
        ## Cross reference
        ############################################################

	itk_component add xref_menu {
	    menu $itk_component(tools_menu).xref -tearoff 0\
		    -postcommand [itcl::code ${this} tools_xref_post\
		     $itk_component(tools_menu).xref]
	} {}

        $itk_component(tools_menu) add cascade\
		-label [get_indep String MultiXRef]\
		-menu $itk_component(xref_menu)\
		-underline [get_indep Pos MultiXRef] -state disabled

        ############################################################
        ## Include
        ############################################################

	itk_component add include_menu {
	    menu $itk_component(tools_menu).incbr -tearoff 0\
		    -postcommand [itcl::code ${this} tools_incbr_post\
		     $itk_component(tools_menu).incbr]
	} {}
        $itk_component(tools_menu) add cascade\
		-label [get_indep String MultiInclude]\
		-menu $itk_component(include_menu)\
		-underline [get_indep Pos MultiInclude]\
		-state disabled
    
        $itk_component(tools_menu) add separator

        ## Miscellaneous Tools and IDE features
        AddMiscSubMenu $itk_component(tools_menu)

        ## Revision control
        $itk_component(tools_menu) add separator

	itk_component add rc_menu {
	    menu $itk_component(tools_menu).rc -tearoff 0
	} {}

        $itk_component(tools_menu) add cascade\
		-label [get_indep String RCS]\
		-underline [get_indep Pos RCS]\
		-state ${state} -menu $itk_component(rc_menu)

        $itk_component(rc_menu) configure -font $sn_options(def,layout-font)

        $itk_component(rc_menu) add command\
		-label [get_indep String ChooseCheckOut]\
		-underline [get_indep Pos ChooseCheckOut]\
		-command [itcl::code ${this} Handle_Rcs sn_rcs_checkout]

        $itk_component(rc_menu) add command\
		-label [get_indep String ChooseCheckIn]\
		-underline [get_indep Pos ChooseCheckIn]\
		-command [itcl::code ${this} Handle_Rcs sn_rcs_checkin]

        $itk_component(rc_menu) add command\
		-label [get_indep String ChooseDiscard]\
		-underline [get_indep Pos ChooseDiscard]\
		-command [itcl::code ${this} Handle_Rcs sn_rcs_discard]

        $itk_component(rc_menu) add command\
		-label [get_indep String ChooseLock]\
		-underline [get_indep Pos ChooseLock]\
		-command [itcl::code ${this} Handle_Rcs\
		 "sn_rcs_lockunlockdel lock"]

        $itk_component(rc_menu) add command\
		-label [get_indep String ChooseUnlock]\
		-underline [get_indep Pos ChooseUnlock]\
		-command [itcl::code ${this} Handle_Rcs\
		 "sn_rcs_lockunlockdel unlock"]

        $itk_component(rc_menu) add separator

        $itk_component(rc_menu) add command\
		-label [get_indep String ChooseDiff]\
		-underline [get_indep Pos ChooseDiff]\
		-command [itcl::code ${this} Handle_Rcs "sn_rcs_diff ${this}"]

        #revision control editor
        $itk_component(rc_menu) add separator

        $itk_component(rc_menu) add command\
		-label [get_indep String RevisionControlEditor]\
		-underline [get_indep Pos RevisionControlEditor]\
		-command " sn_revision_ctrl "

        ## History
        AddHistMenu $itk_component(menu)

        ##Windows
        AddWindowsMenu $itk_component(menu) $itk_component(hull) 1 1

        ##Help menu
        AddHelpMenu $itk_component(menu) $itk_component(hull)

        ${this} configure -menu $itk_component(menu)
    }

    method AddToolbar {} {

	itk_component add toolbar {
	    frame $itk_component(hull).toolbar -takefocus 0\
		    -relief groove -border 2
	}

        #prev/next buttons
	itk_component add hist_prev {
	    button $itk_component(toolbar).prev\
		    -image prev_image\
		    -takefocus 0\
		    -command [itcl::code ${this} history_stack_goto_point prev]
	}

        bind $itk_component(hist_prev) <3>\
		[itcl::code ${this} toolbar_post_history_stack prev\
		 $itk_component(hist_prev)]

        balloon_bind_info $itk_component(hist_prev)\
		[get_indep String GotoPrevPosition]

        pack $itk_component(hist_prev) -side left

	itk_component add hist_next {
	    button $itk_component(toolbar).next\
		    -image next2_image\
		    -takefocus 0\
		    -command [itcl::code ${this} history_stack_goto_point next]
	}

        bind $itk_component(hist_next) <3> [itcl::code ${this}\
		toolbar_post_history_stack next $itk_component(hist_next)]
    
        balloon_bind_info $itk_component(hist_next)\
		[get_indep String GotoNextPosition]

        pack $itk_component(hist_next) -side left

        history_stack_control_buttons

        #combobox for the file symbols	
	itk_component add symbolcombo {
            Combo& $itk_component(toolbar).symbolcombo \
                -width 26 \
                -entryballoon [get_indep String ActiveSymbol] \
                -buttonballoon [get_indep String ListRelatedSymbols] \
                -selectcommand [itcl::code $this toolbar_display_selected $itk_component(toolbar).symbolcombo]
	} { }

        pack $itk_component(symbolcombo) -side left -fill x -padx 2

	# FIXME: Does this actually need to pass parameters?
        Add_Symbol_Filter $itk_component(toolbar)

        #add/create editor command buttons
        Add_Editor_Buttons $itk_component(toolbar)

        pack $itk_component(toolbar) -side top -fill x
    }

    method Add_Symbol_Filter {exp} {
        global combobox_editor_scopes

        # Add symbols filter to the right side of the combobox
	
        set extended [$itk_component(symbolcombo) component extended]

# FIXME: This SymbolsFilter, opt, itk_component(symbolfilter)
# variable duplication needs to be cleaned up
        set opt $extended.symfilter
        set SymbolsFilter ${opt}

        $itk_component(symbolcombo) configure \
		-postcommand [itcl::code $this toolbar_post_scope_menu $opt]

        if {[winfo exists ${opt}]} {
            destroy ${opt}
        }

	itk_component add symbolfilter {
	    frame ${opt}
	}

	pack ${opt} -fill both -expand true


	#Radiobuttons (Related,All)
	# FIXME: this should be a private variable (use "scope").
        uplevel #0 "set ${opt}-related 1"

        radiobutton ${opt}.related -anchor w -text [get_indep String Related]\
          -command " ${this} toolbar_symbols"\
          -variable ${opt}-related -value 1

        pack ${opt}.related -side top -fill x -expand y
        lappend AllFilterOptions(radio) ${opt}.related
        radiobutton ${opt}.all -anchor w -text [get_indep String All]\
          -command " ${this} toolbar_symbols"\
          -variable ${opt}-related -value 0
        pack ${opt}.all -side top -fill x -expand y
        lappend AllFilterOptions(radio) ${opt}.all

        #Buttons (All,None)
        frame ${opt}.btns
        button ${opt}.btns.all -anchor w -text [get_indep String All]\
          -command " ${this} toolbar_symbols 1 "
        pack ${opt}.btns.all -side left -fill x -expand y
        lappend AllFilterOptions(button) ${opt}.btns.all

        button ${opt}.btns.none -anchor w -text [get_indep String None]\
          -command " ${this} toolbar_symbols -1 "
        pack ${opt}.btns.none -side right -fill x -expand y
        lappend AllFilterOptions(button) ${opt}.btns.none

        pack ${opt}.btns -side top -fill x -expand y

        #Filter options (cl,com,fd,fu,...)
        foreach sc [lsort [array names combobox_editor_scopes]] {
            uplevel #0 "set ${opt}-${sc} ${sc}"
            checkbutton ${opt}.${sc} -anchor w -text\
              [convert_scope_to_str ${sc}] -command " ${this} toolbar_symbols\
              " -variable ${opt}-${sc} -onvalue ${sc}\
              -offvalue "off"
            pack ${opt}.${sc} -side top -fill x -expand y
            lappend AllFilterOptions(checkbutton) ${opt}.${sc}
        }
        #Add checkbutton for undefined symbols (xref only)
        checkbutton ${opt}.ud -anchor w -text [convert_scope_to_str ud]\
          -command " ${this} toolbar_symbols "\
          -variable ${opt}-ud -onvalue ud -offvalue off
        lappend AllFilterOptions(checkbutton) ${opt}.ud
        pack ${opt}.ud -side top -fill x -expand y
    }

    #add editor commands
    method Add_Editor_Buttons {exp} {
        global sn_options
        global tcl_platform

        set FindEditFr ${exp}.editfr
        pack [frame ${FindEditFr}] -fill x
        pack [frame ${FindEditFr}.grap1 -width 5] -side left

        #add new/open/save/print/copy/paste/undo
        if {$sn_options(def,edit-more-buttons)} {
            button ${FindEditFr}.new -takefocus 0 -image new_image -command\
              " ${this} file_new "
            balloon_bind_info ${FindEditFr}.new [get_indep String NewFileINFO]
            pack ${FindEditFr}.new -side left

            button ${FindEditFr}.open -image open_image -takefocus 0\
              -command " ${this} file_open "
            balloon_bind_info ${FindEditFr}.open [get_indep String\
              EINFOOpenFile]
            pack ${FindEditFr}.open -side left

            #save
            button ${FindEditFr}.save -image save_image -takefocus 0\
              -command " ${this} file_save "
            balloon_bind_info ${FindEditFr}.save [get_indep String\
              EINFOSaveChanges]
            pack ${FindEditFr}.save -side left

            if {$tcl_platform(platform) != "windows"} {
                button ${FindEditFr}.print -takefocus 0 -image print_image\
                  -command " ${this} file_print "
                balloon_bind_info ${FindEditFr}.print [get_indep String\
                  PrintFile]
                pack ${FindEditFr}.print -side left
            }

            #undo
            button ${FindEditFr}.undo -takefocus 0 -image undo_image\
              -command " ${this} edit_undo "
            balloon_bind_info ${FindEditFr}.undo [get_indep String EditUndoINFO]
            pack ${FindEditFr}.undo -side left

            #delete
            button ${FindEditFr}.delete -takefocus 0 -image waste_image\
              -command " ${this} edit_delete "
            balloon_bind_info ${FindEditFr}.delete [get_indep String\
              EINFODeleteSection]
            pack ${FindEditFr}.delete -side left

            #cut
            button ${FindEditFr}.cut -takefocus 0 -image cut_image -command\
              " ${this} edit_cut "
            balloon_bind_info ${FindEditFr}.cut [get_indep String\
              EINFOCutSelection]
            pack ${FindEditFr}.cut -side left

            #copy
            button ${FindEditFr}.copy -takefocus 0 -image copy_image\
              -command " ${this} edit_copy "
            balloon_bind_info ${FindEditFr}.copy [get_indep String\
              EINFOCopySelection]
            pack ${FindEditFr}.copy -side left

            #paste
            button ${FindEditFr}.paste -takefocus 0 -image paste_image\
              -command " ${this} edit_paste "
            balloon_bind_info ${FindEditFr}.paste [get_indep String\
              EINFOPasteSelection]
            pack ${FindEditFr}.paste -side left

            #compile
            button ${FindEditFr}.compile -takefocus 0 -image compile_image\
              -command " ${this} do_compile_file "
            balloon_bind_info ${FindEditFr}.compile [get_indep String\
              CompileINFO]
            pack ${FindEditFr}.compile -side left

        }

        #combobox for the find command, do not use filter
        Combo& ${FindEditFr}.find_hist -width 16 \
          -entryballoon [get_indep String EINFOEnterPattern]\
          -buttonballoon [get_indep String EINFOSearchHistory]\
          -selectcommand "${this} toolbar_findtext ${FindEditFr}.find_hist"\
          -postcommand "${this} toolbar_findtext_postcommand\
          ${FindEditFr}.find_hist"
        set FindCombo ${FindEditFr}.find_hist
        bind ${FindEditFr}.find_hist.entry <Control-f>\
          [bind ${FindEditFr}.find_hist.entry <Return>]
        bind ${FindEditFr}.find_hist.entry <F3>\
          [bind ${FindEditFr}.find_hist.entry <Return>]
        pack ${FindEditFr}.find_hist -side left
        #button to find next/selected entry
        button ${FindEditFr}.find -takefocus 0 -image find_image -command\
          " ${this} toolbar_search_pattern "
        balloon_bind_info ${FindEditFr}.find [get_indep String\
          SearchFindSelectionINFO]
        pack ${FindEditFr}.find -side left

        #some free space
        pack [frame ${FindEditFr}.gap -width 5] -side left
        #Retriever
        button ${FindEditFr}.definition -takefocus 0 -image search_image\
          -command " ${this} toolbar_retriever "
        bind_history ${FindEditFr}.definition retrieve
        balloon_bind_info ${FindEditFr}.definition [get_indep String\
          RetrieverINFO]
        pack ${FindEditFr}.definition -side left

        #Grep
        button ${FindEditFr}.grep -takefocus 0 -image grep_image \
            -command "${this} search_grep"

        bind_history ${FindEditFr}.grep grep
        balloon_bind_info ${FindEditFr}.grep [get_indep String INFOGrep]
        pack ${FindEditFr}.grep -side left

        #some free space
        pack [frame ${FindEditFr}.gap2 -width 5] -side left
    }

    method AddStatusbar {} {
        global sn_options

	itk_component add statusbar {
	    frame $itk_component(hull).statusbar
	}

        #use the option flag
	itk_component add reuse {
	    checkbutton $itk_component(statusbar).reuse\
		    -relief groove\
		    -text [get_indep String Reuse]\
		    -variable [itcl::scope reusable]\
		    -font $sn_options(def,layout-font)
	}

        balloon_bind_info $itk_component(reuse) [get_indep String ReusableINFO]

        pack $itk_component(reuse) -side left

        set reusable $sn_options(def,reuse-window)

        set keep $sn_options(def,window-switchable)

	itk_component add keep {
	    checkbutton $itk_component(statusbar).keep\
		    -relief groove\
		    -text [get_indep String Keep]\
		    -variable [itcl::scope keep]\
		    -font $sn_options(def,layout-font)
	}

        balloon_bind_info $itk_component(keep) [get_indep String KeepINFO]

        pack $itk_component(keep) -side left

        itk_component add linenum {
	    label $itk_component(statusbar).linenum\
		    -font $sn_options(def,layout-font)\
		    -relief groove -bd 2 -anchor e -width 8\
		    -textvar [itcl::scope linenum]
	}

        pack $itk_component(linenum) -fill y -side left

        # Create a frame with a groove and a border width of
        # 2 so that the message entry maintains the same look
        # that it had with earlier versions. This is needed
        # so that the file info can be anchored east so that
        # the file name still displays when the window is
        # too small to display the whole file name.

        set message_frame [frame $itk_component(statusbar).mf \
            -relief groove -bd 2]

	itk_component add message {
	    label $itk_component(statusbar).mf.message\
		    -font $sn_options(def,layout-font)\
		    -relief flat -bd 0 -anchor e \
		    -textvar [itcl::scope message]
	}

        pack $message_frame -expand y -fill both -side left
	pack $itk_component(message) -side left -anchor e

        pack $itk_component(statusbar) -side bottom -fill x
    }   

    method file_post {m} {
        global sn_options
        global prj_lines_num

        #call post command for project menus
        ProjectMenuEntries_post ${m} "" .prjsub

        set fast_state normal
        set new_state normal

        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            set state disabled
            set save_state disabled
            set fast_state disabled
            set rev_state disabled
        } else {
            set state normal
            if {[${ed} cget -file_changed]} {
                set save_state normal
            } else {
                set save_state disabled
                set fast_state disabled
            }
            #no revert for new files
            if {[${ed} cget -filename] == $sn_options(noname_file)} {
                set rev_state disabled
            } else {
                set rev_state ${state}
            }
        }

        #normal save is only possible, when no process is
        #running
        if {${save_state} == "normal" && [sn_processes_running]} {
            set save_state disabled
        }

        #we can't create/open files by readonly-project
        if {$sn_options(readonly)} {
            set new_state "disabled"
        }

        #file commands
        ${m} entryconfig [get_indep String EditNewFile] -state ${new_state}
        ${m} entryconfig [get_indep String Open] -state ${new_state}
        ${m} entryconfig [get_indep String EditSave] -state ${save_state}
        ${m} entryconfig [get_indep String EditSaveFileAs] -state ${state}
        ${m} entryconfig [get_indep String EditFastSave] -state ${fast_state}
        ${m} entryconfig [get_indep String Revert] -state ${rev_state}
    }

    proc post_recent_project {m topm pm} {
        #get file menu widget name
        set event_window [lmatch [winfo children [namespace tail ${topm}]]\
		{*#menu}]
        set child [lindex [winfo children ${event_window}] 0]

        #Add menu entries for project history
        return [update_project_menu_list ${m} ${pm}]
    }

    proc update_project_menu_list {menu event_window} {
        global sn_options
        global sn_projects_list

        set pf $sn_projects_list(filename)
        if {[info exists sn_projects_list(mtime)] && $sn_projects_list(mtime)\
          == [file mtime ${pf}] && ![catch {${menu} index {*1.*}}]} {
            return 0
        }

        set exist_projs [sn_read_exist_projects 1]
        set proj_name $sn_options(sys,project-file)

        set off [lsearch -exact ${exist_projs} ${proj_name}]
        if {${off} != -1} {
            set exist_projs [lreplace ${exist_projs} ${off} ${off}]
        }

        # Lets create the menu labels!
        ${menu} delete 0 end

        set menu_proj ""
        set i 1
        set idx 0
        foreach pr ${exist_projs} {
            set lb [file tail ${pr}]
            if {${i} < 10} {
                ${menu} add command -label "${i}. ${lb}" -command\
                  [list sn_open_project ${pr}] -underline 0
            } else {
                ${menu} add command -label "${i}. ${lb}" -command\
                  [list sn_open_project ${pr}]
            }
            menu_balloon_bind_info ${event_window} ${idx} ${pr}

            incr i
            incr idx
        }
        return [expr {${i} > 1}]
    }

    method file_new {} {
        if {${ActiveWidget} != $itk_component(editor)} {
            $itk_component(notebook) raise edit
        }
        [list_find_editor ${ActiveWidget}] new_file
    }

    method file_open {} {
        if {${ActiveWidget} != $itk_component(editor)} {
            $itk_component(notebook) raise edit
        }
        [list_find_editor ${ActiveWidget}] open_file
    }

    #Project callbacks
    proc file_open_project {} {
        sn_open_project
    }

    proc file_save_project {} {
        sn_save_project
    }

    proc delete_current_project {} {
        if {[sn_delete_current_project]} {
            sn_projectmanager
            delete_interp
        }
    }

    proc file_close_project {} {
        if {[sn_quit]} {
            sn_projectmanager
            delete_interp
        }
    }

    #Save managment
    method file_save {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        ${ed} save_file
    }

    method file_saveas {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        ${ed} saveas_file 0 [${ed} cget -filename]
    }

    method file_fastsave {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        ${ed} fastsave_file
    }

    method file_saveall {} {
        Editor&::SaveAll
    }

    method file_revert {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        ${ed} revert_file
    }

    method file_print {} {
        ${ActiveWidget} print
    }

    method edit_post {m} {

        set ed [list_find_editor ${ActiveWidget}]

        if {${ed} == ""} {
            set state disabled
            set modstate disabled
            set dostate disabled
        } else {
            set state normal
            if {[${ed} canModify]} {
                set modstate normal
            } else {
                set modstate disabled
            }
            if {[${ed} cget -file_changed]} {
                set dostate normal
            } else {
                set dostate disabled
            }
        }

        ${m} entryconfig [get_indep String EditUndo] -state ${dostate}
        ${m} entryconfig [get_indep String EditRedo] -state ${dostate}
        ${m} entryconfig [get_indep String EditCut] -state ${modstate}
        ${m} entryconfig [get_indep String EditCopy] -state ${state}
        ${m} entryconfig [get_indep String EditPaste] -state ${modstate}
        ${m} entryconfig [get_indep String EditDelete] -state ${modstate}
        ${m} entryconfig [get_indep String SelectAll] -state ${state}
        ${m} entryconfig [get_indep String EditInsertFile] -state ${modstate}
        ${m} entryconfig [get_indep String EditIdent] -state ${modstate}
        ${m} entryconfig [get_indep String EditOutdent] -state ${modstate}
    }

    method edit_undo {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        ${ed} Undo
    }

    method edit_redo {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        ${ed} Redo
    }

    method edit_cut {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        ${ed} Cut
    }

    method edit_copy {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        ${ed} Copy
    }

    method edit_paste {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        ${ed} Paste
    }

    method edit_delete {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        ${ed} Delete
    }

    method do_compile_file {} {
        global sn_options

        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }

        set file [${ed} cget -filename]
        if {${file} == $sn_options(noname_file)} {
            bell
            return
        }

        #get file name and convert it to an object file
        set tocmpfile [sn_object_file [file tail ${file}]]
        set tocmpdir [file dirname ${file}]

        #mask the file with "" if there are blanks in it.
        if {[string first " " ${tocmpfile}] > 1} {
            set tocmpfile "\"${tocmpfile}\""
        }

        set make [sn_make]
        ${make}.make setmakecommand\
          "$sn_options(both,make-command) ${tocmpfile}"
        ${make}.make ExecMake
    }

    proc edit_proj_preferences {m} {
        sn_project_preferences
    }

    method edit_preferences {m} {
        sn_project_preferences 0 [${ActiveWidget} whoami] ${ActiveWidget}\
          ${this}
    }

    if {${sn_elix}} {
        proc edit_elix_preferences {} {
            sn_elix_preferences
        }
    }

    method edit_insert {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        ${ed} insert_file
    }

    method edit_indent {mode} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        Editor&::Indent ${ed} ${mode}
    }

    method Handle_Rcs {cmd {prm ""}} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        eval ${cmd} ${prm} [${ed} cget -filename]
        ${ed} SetTitle
    }

    method edit_selectall {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        [${ed} editor] tag add sel 0.0 end
        [${ed} editor] tag raise sel
    }

    method edit_clear {} {
        ${ActiveWidget} clearselection
    }

    method search_post {m} {
        #if we don't have an editor in the actual view,
        #disable string manipulation menu items
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            set state disabled
        } else {
            set state normal
        }

        ${m} entryconfig [get_indep String SearchFind] -state ${state}
        ${m} entryconfig [get_indep String SearchNext] -state ${state}
        ${m} entryconfig [get_indep String SearchPrev] -state ${state}
        ${m} entryconfig [get_indep String SearchReplace] -state ${state}

        # FIXME: using sub menu widgets directly is likely to break
        # Disable goto submenu too
        ${m}.goto entryconfig [get_indep String SearchGotoLine] -state ${state}
        ${m}.goto entryconfig [get_indep String SearchSetMark] -state ${state}
        ${m}.goto entryconfig [get_indep String SearchGotoMark] -state ${state}
        ${m}.goto entryconfig [get_indep String SearchGotoError] -state ${state}

        #verify if we can retrieve something
        if {${ed} != ""} {
            set string [Get_Selection ${ed} ${ActiveWidget}]
        } else {
            set string ""
        }

        if {${string} == ""} {
            set retrstate disabled
        } else {
            set retrstate normal
        }

        ${m} entryconfig [get_indep String SearchDefinition] -state ${retrstate}
        ${m} entryconfig [get_indep String SearchImplementation]\
		-state ${retrstate}
        ${m} entryconfig [get_indep String SearchGrep] -state ${retrstate}
    }

    method search_findtext {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} != ""} {
            ${ed} FindText
        } else {
            bell
        }
    }

    method search_next {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} != ""} {
            ${ed} FindNext -forwards
        } else {
            bell
        }
    }

    method search_prev {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} != ""} {
            ${ed} FindNext -backwards
        } else {
            bell
        }
    }

    method search_replace {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} != ""} {
            ${ed} Replace
        } else {
            bell
        }
    }

    proc Get_Selection {ed active_wdg} {

        # Get selection from current editor.

        if {${ed} != ""} {
            set string [Editor&::Get_XSelection ${ed}]
        } else {
            set string ""
        }

        # Get selection from current view.

        if {${string} == "" && ${ed} != ${active_wdg}} {
            set sel [${active_wdg} Selection]
            if {${sel} != ""} {
                set string [lindex ${sel} 1]
                set cls [lindex ${sel} 2]
                if {${cls} != ""} {
                    set string "${cls} ${string}"
                }
            }
        }

        return ${string}
    }

    method search_definition {} {

        # Look for an editor.

        set ed [list_find_editor ${ActiveWidget}]

        # We use only the same editor, if the keep flag
        # is not seted.

        if {${keep} && ${ed} != ""} {
            set cmd "${ed} gotofile_cb"
        } else {
            set cmd ""
        }

        if {${ed} != ""} {
            return [Editor&::search_definition ${ed}]
        }

        set string [Get_Selection ${ed} ${ActiveWidget}]

        if {${string} == ""} {
            bell
            return
        }

        # Try to find method or function definition (don't bell)

        set ret [sn_retrieve_symbol ${string} {md fd fr} "" -exact 1 0 ${cmd}]

        # Try to find any definition (not implementation or function body)
        # (bell if nothing found)

        if {${ret} == 0} {
            sn_retrieve_symbol ${string} all "" -exact 1 1 ${cmd} "" "" "" "*"\
		    -1 {{mi md} {fu fd}}
        }
    }

    method search_implementation {} {

        set ed [list_find_editor ${ActiveWidget}]

        if {${keep} && ${ed} != ""} {
            set cmd "${ed} gotofile_cb"
        } else {
            set cmd ""
        }

        if {${ed} != ""} {
            return [Editor&::search_implementation ${ed}]
        }

        set string [Get_Selection ${ed} ${ActiveWidget}]
        if {${string} == ""} {
            bell
            return
        }

        #find implementation
        set ret [sn_retrieve_symbol ${string} {fu mi} "" -exact 1 0 ${cmd}]

        #find probably definition (correct for Tcl)
        if {${ret} == 0} {
            sn_retrieve_symbol ${string} {fd md fr} "" -exact 1 1 ${cmd}
        }
    }

    method search_gotoline {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        ${ed} GotoLine
    }

    method search_setmark {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        [${ed} editor] mark set markpos insert
    }

# FIXME: Targeted for removal.

    proc search_gotoerror {} {
        if {[catch {sn_grep_edit_file [selection get]}]} {
            bell
        }
    }

    method search_gotomark {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }

        set t [${ed} editor]
        ${t} mark set lastpos insert
        ${t} mark set insert markpos
        ${t} see insert
    }

    #Execute grep with selection, when there is no selection,
    #call grep dialog box
    method search_grep {} {
# FIXME: this is not quite right just yet. Perhaps handle
# the case where there is nothing selected????
        #set ed [list_find_editor ${ActiveWidget}]
        #if {${ed} == ""} {
        #    bell
        #    return
        #}
        #sn_grep
        $itk_component(notebook) raise grep
        $itk_component(grep) setPatternFromClipboard
	$itk_component(grep) StartGrep
    }

    method search_goto_post {m} {
    }

    proc history_post {m} {
        sn_post_history_menu ${m}
    }

    method tools_post {m} {

        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            set rcsstate disabled
        } else {
            if {[${ed} validFilename]} {
                set rcsstate normal
            } else {
                set rcsstate disabled
            }
        }
        # RCS submenu is disabled when the filename isn't valid

        $itk_component(rc_menu) entryconfig [get_indep String ChooseCheckOut] \
            -state ${rcsstate}

        $itk_component(rc_menu) entryconfig [get_indep String ChooseCheckIn] -state ${rcsstate}
        $itk_component(rc_menu) entryconfig [get_indep String ChooseDiscard] -state ${rcsstate}
        $itk_component(rc_menu) entryconfig [get_indep String ChooseLock] -state ${rcsstate}
        $itk_component(rc_menu) entryconfig [get_indep String ChooseUnlock] -state ${rcsstate}
        $itk_component(rc_menu) entryconfig [get_indep String ChooseDiff] -state ${rcsstate}
        $itk_component(rc_menu) entryconfig [get_indep String RevisionControlEditor] \
            -state ${rcsstate}
    }

    method tools_ctree_post {m} {
    }

    method tools_classbr_post {m} {
    }

    method tools_xref_post {m} {
    }

    method tools_incbr_post {m} {
    }

    #add breakpoint or execute until the specified line
    method tools_breakpoint_or_until {{until "break"}} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        set idx [[${ed} editor] index insert]
        if {${idx} == ""} {
            bell
            return
        }
        dbg_set_breakpoint [${ed} cget -filename] ${idx} ${until}
    }

    proc windows_new_window {{m ""} {page "edit"}} {
        global tkeWinNumber
        incr tkeWinNumber

        #verify if the tool command exists
        if {${page} != "edit" && [tool_Exists ${page}] != "normal"} {
            bell
            return
        }

        return [MultiWindow& .multiwindow-${tkeWinNumber} -raise ${page}]
    }

    proc windows_new_post {m} {
        set edit_state [tool_Exists edit]
        set ctree_state [tool_Exists ctree]
        set xref_state [tool_Exists xref]
        set retr_state [tool_Exists retr]
        set grep_state [tool_Exists grep]
        set inc_state [tool_Exists incbr]

        ${m} entryconfig [get_indep String WindowsNew] -state ${edit_state}
        ${m} entryconfig [get_indep String MultiClassHierarchy]\
          -state ${ctree_state}
        ${m} entryconfig [get_indep String MultiClass] -state ${edit_state}
        ${m} entryconfig [get_indep String MultiXRef] -state ${xref_state}
        ${m} entryconfig [get_indep String MultiInclude] -state ${inc_state}

        ${m} entryconfig [get_indep String WindowsNewCTreeClass]\
          -state ${ctree_state}
        ${m} entryconfig [get_indep String WindowsNewClassEditor]\
          -state ${ctree_state}
        if {${ctree_state} == "normal" && ${xref_state} == "normal"} {
            set state normal
        } else {
            set state disabled
        }
        ${m} entryconfig [get_indep String WindowsNewClassXRef] -state ${state}
        ${m} entryconfig [get_indep String WindowsNewRetrEditor]\
          -state ${retr_state}
        ${m} entryconfig [get_indep String WindowsNewFindEditor]\
          -state ${grep_state}
        ${m} entryconfig [get_indep String WindowsNewCTreeClassEditor]\
          -state ${ctree_state}

    }

    proc windows_new_ctree_class {m} {
        set win [windows_new_window "" ctree]
        update idletasks
        after idle "update idletasks; ${win} windows_splitinto_classbr"
    }

    proc windows_new_class_edit {m} {
        set win [windows_new_window "" classbr]
        update idletasks
        after idle "update idletasks; ${win} windows_splitinto_edit"
    }

    proc windows_new_class_xref {m} {
        set win [windows_new_window "" classbr]
        update idletasks
        after idle "update idletasks; ${win} windows_splitinto_xref"
    }

    proc windows_new_retr_edit {m} {
        set win [windows_new_window "" retr]
        update idletasks
        after idle "update idletasks; ${win} windows_splitinto_edit"
    }

    proc windows_new_find_edit {m} {
        set win [windows_new_window "" grep]
        update idletasks
        after idle "update idletasks; ${win} windows_splitinto_edit"
    }

    proc windows_new_ctree_class_edit {m} {
        set win [windows_new_window "" ctree]
        update idletasks
        after idle "update idletasks; ${win} windows_splitinto_classbr"
        update idletasks
        after idle "update idletasks; ${win} windows_splitinto_edit"
    }

    proc windows_new_symbr {{m ""}} {
        global tkeWinNumber
        incr tkeWinNumber
        return [SymBr& .multisymbr-${tkeWinNumber}]
    }

    #return the number of existing windows, to verify
    #if we want to exist or close the window
    proc num_Existing_windows {} {
        set len [llength [itcl::find objects "*" -class MultiWindow&]]
        set len [expr ${len} + [llength [itcl::find objects "*" -class SymBr&]]]
        return ${len}
    }

    #close window, if it is not the last window
    #exit project, if it is the last window
    method windows_close {m} {
        if {[num_Existing_windows] > 1} {
            if {[Close]} {

                #close related preferences window, if availiable
                catch {${this}-preferences exitPreferences}

                #close window
                itcl::delete object ${this}
            }
        } else {
            #exit the project
            MultiWindow&::file_close_project
        }
    }

    #iconize project
    proc windows_iconize {m topw} {
        sn_hide_show_project withdraw ${topw}
    }

    method windows_split_post {m} {
        set last [list_last ${ActiveWidget}]

        #disable deleting last pane, if no panes availiable
        if {${last} == ${ActiveWidget}} {
            set state disabled
        } else {
            set state normal
        }
        ${m} entryconfig [get_indep String SplitDeleteLast] -state ${state}

        ${m} entryconfig [get_indep String SplitIntoEditor]\
          -state [tool_Exists edit]
        ${m} entryconfig [get_indep String SplitIntoClassHierarchy]\
          -state [tool_Exists ctree]
        ${m} entryconfig [get_indep String SplitIntoClass] -state\
          [tool_Exists class]
        ${m} entryconfig [get_indep String SplitIntoXRef] -state\
          [tool_Exists xref]
        ${m} entryconfig [get_indep String SplitIntoInclude]\
          -state [tool_Exists incbr]
    }

    ############################################
    # Views SECTION
    ############################################
    proc find_mainwindow {view} {
        foreach win [itcl::find objects "*" -class MultiWindow&] {
            foreach tool [${win} tools] {
                for {set nxt ${tool}} {${nxt} != ""} {set nxt [${nxt} next]} {
                    if {${nxt} == ${view}} {
                        return [list ${win} ${tool}]
                    }
                }
            }
        }
        return ""
    }

    # Find the main page in corresponding to the given view,
    # it can be a sub view.
    method find_page {view} {
        foreach tool [tools] {
            for {set nxt ${tool}} {${nxt} != ""} {set nxt [${nxt} next]} {
                if {${nxt} == ${view}} {
                    return [${tool} whoami]
                }
            }
        }
        return ""
    }

    proc find_browserwindow {view} {
        foreach win [itcl::find objects "*" -class SymBr&] {
            if {[${win} symbr] == ${view}} {
                return ${win}
            }
        }
        return ""
    }

    proc windows_views_switchto {view} {
        global Switch_Is_Enabled

        # Look for the window in the multiviews windows.
        set win_tool [find_mainwindow ${view}]
        if {${win_tool} != ""} {
            # Disable switching.
            ::incr Switch_Is_Enabled -1

            set win [lindex ${win_tool} 0]
            set page [lindex ${win_tool} 1]

            [${win} NoteBook] raise [${page} whoami]
            ${view} Focus
            ${win} raise

            # Reenable switching.
            ::incr Switch_Is_Enabled
        } else {
            # Window not found, maybe its a retriever in the symbol browser.
            set win [find_browserwindow ${view}]
            if {${win} == ""} {
                bell
                return
            }
            ${win} raise
        }
    }

    proc windows_views_post {m} {
        global sn_options

        if {[itcl::find objects "*" -class Editor&] != ""} {
            set state normal
        } else {
            set state disabled
        }
        ${m} entryconfig [get_indep String MultiEditor] -state ${state}

        if {[itcl::find objects "*" -class ClassTree&] != ""} {
            set state normal
        } else {
            set state disabled
        }
        ${m} entryconfig [get_indep String MultiClassHierarchy] -state ${state}

        if {[itcl::find objects "*" -class Class&] != ""} {
            set state normal
        } else {
            set state disabled
        }
        ${m} entryconfig [get_indep String MultiClass] -state ${state}

        if {[itcl::find objects "*" -class XRef&] != ""} {
            set state normal
        } else {
            set state disabled
        }
        ${m} entryconfig [get_indep String MultiXRef] -state ${state}

        if {[itcl::find objects "*" -class Include&] != ""} {
            set state normal
        } else {
            set state disabled
        }
        ${m} entryconfig [get_indep String MultiInclude] -state ${state}

        # Is a retriever-view availiable in the mainwindow (only).
        set state disabled
        foreach rtr [itcl::find objects "*" -class Retr&] {
            #only in main windows
            if {[catch {set win [[winfo toplevel ${rtr}] whoami]}] || ${win}\
              != "mainw"} {
                continue
            } else {
                set state normal
                break
            }
        }
        ${m} entryconfig [get_indep String MultiRetriever] -state ${state}

# FIXME: DEAR GOD THIS IS AN UGLY HACK!
        # Is a grep-view avail. in the main window (only).
        set state disabled
        foreach gr [itcl::find objects -class Grep] {
            #only in main windows
            if {[catch {set win [[winfo toplevel ${gr}] whoami]}] || ${win} !=\
              "mainw"} {
                continue
            } else {
                set state normal
                break
            }
        }
        ${m} entryconfig [get_indep String MultiGrep] -state ${state}
    }

    proc windows_views_edit_post {m} {
        ${m} delete 0 end
        set i 1
        foreach ed [itcl::find objects "*" -class Editor&] {
            catch {
                ${m} add command -command "MultiWindow&::windows_views_switchto ${ed}" \
                    -label "${i} [${ed} Title 0]"\
                    -underline 0
                incr i
            }
        }
    }

    proc windows_views_ctree_post {m} {
        ${m} delete 0 end
        set i 1
        foreach ed [itcl::find objects "*" -class ClassTree&] {
            ${m} add command -command "MultiWindow&::windows_views_switchto ${ed}" \
                -label "${i} [${ed} Title 0]" -underline 0
            incr i
        }
    }

    proc windows_views_class_post {m} {
        ${m} delete 0 end
        set i 1
        foreach ed [itcl::find objects "*" -class Class&] {
            ${m} add command -command "MultiWindow&::windows_views_switchto ${ed}" \
                -label "${i} [${ed} Title 0]" -underline 0
            incr i
        }
    }

    proc windows_views_xref_post {m} {
        ${m} delete 0 end
        set i 1
        foreach ed [itcl::find objects "*" -class XRef&] {
            ${m} add command -command "MultiWindow&::windows_views_switchto ${ed}" \
                -label "${i} [${ed} Title 0]" -underline 0
            incr i
        }
    }

    proc windows_views_inc_post {m} {
        ${m} delete 0 end
        set i 1
        foreach ed [itcl::find objects "*" -class Include&] {
            if {[catch {set t "${i} [${ed} Title 0]"}]} {
                continue
            }
            ${m} add command -command "MultiWindow&::windows_views_switchto ${ed}" \
                -label ${t} -underline 0
            incr i
        }
    }

    proc windows_views_retr_post {m} {
        ${m} delete 0 end
        set i 1
        foreach ed [itcl::find objects "*" -class Retr&] {
            #only in main windows
            if {[catch {set win [[winfo toplevel ${ed}] whoami]}] || ${win} !=\
              "mainw"} {
                continue
            }
            ${m} add command -command "MultiWindow&::windows_views_switchto ${ed}" \
                -label "${i} [${ed} Title 0]" -underline 0
            incr i
        }
    }

    proc windows_views_grep_post {m} {
        ${m} delete 0 end
        set i 1
# FIXME: Another ugly hack!
        foreach gr [itcl::find objects -class Grep] {
            #only in main windows
            if {[catch {set win [[winfo toplevel ${gr}] whoami]}] || ${win} !=\
              "mainw"} {
                continue
            }
            ${m} add command -command "MultiWindow&::windows_views_switchto ${gr}"
                -label "${i} [${gr} Title 0]" -underline 0
            incr i
        }
    }

    proc windows_views_make_post {m} {
        ${m} delete 0 end
        set i 1
        foreach ed [itcl::find objects "*" -class Make] {
            ${m} add command -command "MultiWindow&::windows_views_switchto ${ed}" \
                -label "${i} [${ed} Title 0]" -underline 0
            incr i
        }
    }

    #########################################################
    #End of Views SECTION
    #########################################################

    # Find the first classed (class whoami) object in the chain.
    proc list_find {w whoami} {
        global last_Editor
        for {set nxt ${w}} {${nxt} != ""} {set nxt [${nxt} next]} {
            if {[${nxt} whoami] == ${whoami}} {
                return ${nxt}
            }
        }
        return ""
    }

    # Find the last accessed or the first editor in the chain.
    proc list_find_editor {w} {
        global last_Editor
        set fnd ""
        for {set nxt ${w}} {${nxt} != ""} {set nxt [${nxt} next]} {
            if {[${nxt} whoami] == "edit"} {
                if {${last_Editor} == ${nxt}} {
                    #return last accessed editor
                    return ${nxt}
                }\
                elseif {${fnd} == ""} {
                    #mark the first editor
                    set fnd ${nxt}
                }
            }
        }
        # Return the first found editor or the empty string.
        return ${fnd}
    }

    # Return all objects in the list.
    proc list_items {w} {
        set items ""
        for {set nxt ${w}} {${nxt} != ""} {set nxt [${nxt} next]} {
            lappend items ${nxt}
        }
        return ${items}
    }

    # Find the last object in the chain.
    proc list_last {w {previous ""}} {
        if {${previous} != ""} {
            upvar ${previous} prev
        }
        set prev ${w}
        set last ${w}
        for {set nxt ${w}} {${nxt} != ""} {set nxt [${nxt} next]} {
            set prev ${last}
            set last ${nxt}
        }
        return ${last}
    }

    method required_size {w} {
        global sn_options
        if {$sn_options(def,window-alighment) == "vertical"} {
            set last_height [winfo height ${w}]
        } else {
            set last_height [winfo width ${w}]
        }
        set last_height [expr ${last_height} / 2]
        return ${last_height}
    }

    method add_new_pane {last} {
        global tkeWinNumber

        set size [required_size ${last}]
        # Create the editor now.
        incr tkeWinNumber
        set pane [${ActivePaned} add pane-${tkeWinNumber} -size ${size}]
        # Give the upper pane the half size of it's
        # original size.
        set lastpane [winfo parent ${last}]
        set lastpane [string range ${lastpane} [expr [string last\
          "." ${lastpane}] + 1] end]
        ${ActivePaned} paneconfigure ${lastpane} -size ${size}

        return ${pane}
    }

    # Add an editor to current window. This is not
    # possible if we just added an editor.

    method windows_splitinto_edit {{m ""}} {
        global tkeWinNumber

        # Verify that the last widget is not an editor
        set last [list_last ${ActiveWidget}]

        set editpane [add_new_pane ${last}]

        incr tkeWinNumber
        set ed ${editpane}.edit-${tkeWinNumber}
        Editor& ${ed} -mesg_area $itk_component(message)\
		-linenumber_var [itcl::scope linenum]\
                -message_var [itcl::scope message]\
	        -findcombo ${FindCombo} \
		-parent $this

        set who [${last} whoami]

        # Configure selectcommand to be called whenever
        # the selection is changed.
        # Do this only when the previous command is not an editor.
        if {${who} == "edit"} {
            # Add the new editor into the chain.
            ${last} configure -next ${ed}

            # Edit the same file as the prev. editor.
            ${ed} configure -filename [${last} cget -filename]

            # When the active view is a retriever, bind the
            # combo box to the editor.
        }\
        elseif {${who} == "retr" || ${who} == "grep"} {
            # Add the new editor into the chain.
            ${last} configure -next ${ed} -selectcommand "${ed} gotosymbol"

            # Set related flag of the combo box to true.
            upvar #0 ${SymbolsFilter}-related related
            set related 1

# FIXME: what the heck? I was using -mesg_area $itk_component(message)

            # Bind all the environment for the editor.
            ${ed} configure -symbols $itk_component(symbolcombo)\
		    -symbols_filter ${SymbolsFilter}\
		    -menu $itk_component(edit_menu) -toolbar ""\
		    -mesg_area (itk_component(message))\
		    -message_var [itcl::scope message]\
		    -linenumber_var [itcl::scope linenum]\
		    -findcombo ${FindCombo}

            # Enable combo box.
            $itk_component(symbolcombo) configure -state normal

            # Activate editor.
            ${ed} activate

            ::eval ${ed} gotosymbol [${last} Selection]
        } else {
            ${last} configure -next ${ed} -selectcommand "${ed} gotosymbol"

            ::eval ${ed} gotosymbol [${last} Selection]
        }
        pack ${ed} -fill both -expand y

        # View editor commands in toolbar, if not availiable.
        DeActivate_Editor_buttons

        return ${ed}
    }

    # Add a hierarchy browser to current window. This is not
    # possible if we just added a class hierarchy.

    method windows_splitinto_ctree {{m ""}} {
        global tkeWinNumber

        # Verify that the last widget is not a hierarchy browser
        set last [list_last ${ActiveWidget}]
        if {[${last} whoami] == "ctree"} {
            bell
            return ""
        }

        # Create a new ctree pane.
        set ctreepane [add_new_pane ${last}]

        incr tkeWinNumber
        set ctree ${ctreepane}.ctree-${tkeWinNumber}
        ClassTree& ${ctree} \
	    -message_var [itcl::scope message] \
	    -parent $this

        # Configure selectcommand to be called, when
        # ever the selection is changed.
        ${last} configure -selectcommand "${ctree} gotosymbol" -next ${ctree}
        pack ${ctree} -fill both -expand y

        ::eval ${ctree} gotosymbol [${last} Selection]

        return ${ctree}
    }

    # Add a class browser to current window. This is not
    # possible if we just added a class browser.

    method windows_splitinto_classbr {{m ""}} {
        global tkeWinNumber

        # Verify that the last widget is not a class browser.
        set last [list_last ${ActiveWidget}]
        if {[${last} whoami] == "classbr"} {
            bell
            return ""
        }

        # Create the classbr now.
        set classbrpane [add_new_pane ${last}]
        incr tkeWinNumber
        set classbr ${classbrpane}.classbr-${tkeWinNumber}

        Class& ${classbr} \
	    -doubleclickcommand "${this} EditObject"\
            -class_doubleclickcommand "${this} ClassBrowserEditClass"\
            -mesg_area $itk_component(message) \
	    -parent $this

        # Configure selectcommand to be called, when
        # ever the selection is changed.
        ${last} configure -selectcommand "${classbr} gotosymbol" -next ${classbr}
        pack ${classbr} -fill both -expand y

        ::eval ${classbr} gotosymbol [${last} Selection]

        return ${classbr}
    }

    # Add a xref browser to current window. This is not
    # possible if we just added a xref browser.

    method windows_splitinto_xref {{m ""}} {
        global tkeWinNumber

        # Verify that the last widget is not a xref
        set last [list_last ${ActiveWidget}]
        if {[${last} whoami] == "xref"} {
            bell
            return ""
        }

        # Create the xref now
        set xrefpane [add_new_pane ${last}]
        incr tkeWinNumber
        set xref ${xrefpane}.xref-${tkeWinNumber}

        XRef& ${xref} \
            -mesg_area $itk_component(message) \
            -parent ${this}

        # Configure selectcommand to be called, when
        # ever the selection is changed.
        ${last} configure -selectcommand "${xref} gotosymbol" -next ${xref}
        pack ${xref} -fill both -expand y

        ::eval ${xref} gotosymbol [${last} Selection]

        return ${xref}
    }

    # Add an include browser to current window. This is not
    # possible if we just added an include browser.

    method windows_splitinto_incbr {{m ""}} {
        global tkeWinNumber

        # Verify that the last widget is not an include browser.
        set last [list_last ${ActiveWidget}]
        if {[${last} whoami] == "incbr"} {
            bell
            return ""
        }

        # Create the include browser now.
        set incbrpane [add_new_pane ${last}]
        incr tkeWinNumber
        set incbr ${incbrpane}.incbr-${tkeWinNumber}

        Include& ${incbr} \
	    -mesg_area $itk_component(message) \
	    -parent $this

        # Configure selectcommand to be called, when
        # ever the selection is changed.
        ${last} configure -selectcommand "${incbr} gotosymbol" -next ${incbr}
        pack ${incbr} -fill both -expand y

        ::eval ${incbr} gotosymbol [${last} Selection]

        return ${incbr}
    }

    # Add a build window to current window. This is not
    # possible if we just added a build window.

    method windows_splitinto_build {{m ""}} {
        global tkeWinNumber

        # Verify that the last widget is not a build window
        set last [list_last ${ActiveWidget}]
        if {[${last} whoami] == "make"} {
            bell
            return ""
        }

        # Create the make window now.
        set makepane [add_new_pane ${last}]
        incr tkeWinNumber
        set make ${makepane}.make-${tkeWinNumber}

        Make ${make} -mesg_area $itk_component(message)

        # Configure selectcommand to be called, when
        # ever the selection is changed.
        ${last} configure -selectcommand "${make} gotosymbol" -next ${make}

        pack ${make} -fill both -expand y -padx 3 -pady 3

        # FIXME : not clear what this would do.
        #::eval $incbr gotosymbol [$last Selection]

        return ${make}
    }

    # Deletes the last pane from a view.
    method windows_delete_last_pane {m} {
        set last [list_last ${ActiveWidget} prev]
        if {${last} == ${ActiveWidget}} {
            return 0
        }

        set force_close 0

        if {[$last whoami] == "edit"} {
            # If we have more instances of this file open
            # in different editors we don't have to worry
            # about saving it when calling Close.
            
            set file [$last cget -filename]

            if {[CountInstancesOfFile $file] != 1} {
                set force_close 1
            }
        }
    
        # Ask the object for deleting.
        if {[${last} Close $force_close] != 1} {
            return 0
        }

        # Delete pointer to the deleted widget
        # and the selectcommand.
        ${prev} configure -next "" -selectcommand ""

        # Delete object from the pane widget.
        itcl::delete object ${last}

        # Delete pane page.
        set pane [string range ${last} 0 [expr {[string last "." ${last}] - 1}]]
        set page [string range ${pane} [expr {[string last "." ${pane}] + 1}] end]
        set pane [string range ${pane} 0 [expr {[string last "." ${pane}] - 1}]]
        ${pane} del ${page}

        # Hide editor buttons, if availiable.
        DeActivate_Editor_buttons

        # If we enabled the combobox when splitting
        # the window, disable it now.
        set who [${prev} whoami]
        if {${who} == "retr" || ${who} == "grep"} {
            $itk_component(symbolcombo) configure -state disabled
        }

        return 1
    }

    method switch_to_next_view {fcs} {
        for {set w ${ActiveWidget}} {${w} != ""} {set w [${w} next]} {
            if {[string first ${w} ${fcs}] == 0} {
                set nxt [${w} next]
                if {${nxt} != ""} {
                    ${nxt} Focus
                    return
                }
            }
        }
        ${ActiveWidget} Focus
    }

    method switch_tab {op} {
        switch_tix_notebook_tab $itk_component(notebook) $op
    }

    proc windows_post {my_top w {pref 0}} {
        global sn_options

        # Delete only the menubuttons for availiable windows.
        set cnt [expr 3 + ${pref}]
        set num [${w} index end]
        if {${num} >= ${cnt}} {
            ${w} delete ${cnt} end
        }

        set proj_len [expr [string length [sn_title]] + 1]

        set toplevs(${my_top}) 1
        # Ignore the menu's toplevel window!
        set prjName [sn_title]
        set pars ""
        foreach win [winfo children .] {
            set top [winfo toplevel ${win}]

            if {[info exists toplevs(${top})] || [::wm overrideredirect\
              ${top}]} {
                continue
            }
            set toplevs(${top}) 1

            set state_ind ""
            switch [wm state ${top}] {
                "withdrawn" {
                        continue
                    }
                "iconic" {
                        set state_ind {+}
                    }
                "normal" {
                    }
                default {
                        continue
                    }
            }
            set title [wm title ${top}]

            regsub -all {[ ]+} ${title} { } title
            set off [string first ${prjName} ${title}]
            switch -- ${off} {
                -1 {
                    }
                0 {
                        incr off ${proj_len}
                        set title [string trim [string range ${title} ${off}\
                          end]]
                    }
                default {
                        set title [string trim [string range ${title} 0\
                          [expr ${off} - 1]]]
                        if {[string index ${title} 0] == "*"} {
                            set title [string trimleft ${title} "*"]
                        }
                        set title [string trimright ${title} "- "]
                    }
            }
            lappend pars [list ${title} ${state_ind} ${top}]
        }

        set i 1
        foreach p [lsort -dictionary ${pars}] {
            set title [lindex ${p} 0]
            set accel [lindex ${p} 1]
            set win [lindex ${p} 2]

            ${w} add command -label "${i} ${title}" -accelerator ${accel}\
              -underline 0 -command " raise_toplevel ${win} "
            incr i
        }
    }

    method toolbar_post_scope_menu {m} {
        global combobox_editor_scopes

        ${ActiveWidget} postcommand ${m}

        if {[${ActiveWidget} whoami] == "edit"} {
            #enable buttons (All,Related,All,None)
            foreach btn [concat $AllFilterOptions(radio)\
              $AllFilterOptions(button)] {
                ${btn} configure -state normal
            }
            foreach grp [lsort [array names combobox_editor_scopes]] {
                set st "disabled"
                set ind 0
                foreach sc $combobox_editor_scopes(${grp}) {
                    if {[::info commands paf_db_${sc}] != ""} {
                        set st "normal"
                        set ind 1
                        break
                    }
                }
                if {${ind} == 0} {
                    uplevel #0 "set ${m}-${grp} off"
                    ${m}.${grp} configure -state ${st}
                } else {
                    ${m}.${grp} configure -state ${st}
                }
            }
            # Unknown scopes (only for xref).
            ${m}.ud configure -state disabled
        }\
        elseif {[${ActiveWidget} whoami] == "xref"} {
            # Disable All, Related.
            foreach btn $AllFilterOptions(radio) {
                ${btn} configure -state disabled
            }

            # Enable buttons (All,None).
            foreach btn $AllFilterOptions(button) {
                ${btn} configure -state normal
            }

            foreach grp [lsort [array names combobox_editor_scopes]] {
                set st "disabled"
                set ind 0
                foreach sc $combobox_editor_scopes(${grp}) {
                    if {[::info commands paf_db_${sc}] != ""} {
                        set st "normal"
                        set ind 1
                        break
                    }
                }
                if {${ind} == 0} {
                    uplevel #0 "set ${m}-${grp} off"
                    ${m}.${grp} configure -state ${st}
                } else {
                    ${m}.${grp} configure -state ${st}
                }
            }

            # Unknown scopes (only for xref).
            ${m}.ud configure -state normal
        } else {
            # Enable All, Related radios.
            foreach btn $AllFilterOptions(radio) {
                ${btn} configure -state normal
            }

            # Disable filter options (buttons and checkbuttons).
            foreach btn [concat $AllFilterOptions(button)\
              $AllFilterOptions(checkbutton)] {
                ${btn} configure -state disabled
            }

            # Unknown scopes (only for xref).
            ${m}.ud configure -state disabled
        }
    }

    method history_stack_control_buttons {} {
        if {$view_positions(next) == ""} {
            set next_state disabled
        } else {
            set next_state normal
        }
        if {$view_positions(prev) == ""} {
            set prev_state disabled
        } else {
            set prev_state normal
        }
        $itk_component(hist_prev) configure -state ${prev_state}
        $itk_component(hist_next) configure -state ${next_state}
    }

    # Add into view stack
    # where: prev/next
    # wdg:   View widget (Editor, Class, Ctree, ..)
    protected variable history_stack_active 0
    method history_stack_add_point {wdg {where ""}} {
        global sn_options
        global sn_history

        # Stack managment is active.
        if {${history_stack_active}} {
            return
        }

        # Enter.
        incr history_stack_active

        if {${where} == ""} {
            set where ${stack_history_side}
        }

        # Dump the current view of the widget.
        set dump [${wdg} Dump]

        # Check if we have a restorable position at all.
        if {${dump} == ""} {
            #leave
            incr history_stack_active -1
            # No dump position availiable.
            return
        }

        # Title for the list.
        set title [${wdg} DumpTitle ${dump}]

        # Add it on the top of the stack.
        set entry [list ${wdg} ${dump} ${title}]

        # Add to global (normal history from menu) history.
        ${wdg} AddHistoryFromDump ${dump} ${title}

        # If view point is already availiable move it to top.
        set i [lsearch -exact $view_positions(${where}) ${entry}]
        if {${i} != -1} {
            set view_positions(${where}) [lreplace $view_positions(${where})\
              ${i} ${i}]
        }
        set view_positions(${where}) [linsert $view_positions(${where}) 0\
          ${entry}]

        # If we add a view point into the prev list, we
        # must delete all the next positions.
        # Add it to the end of the previous list!!
        # Do not do it when the user navigates using the history dump
        # points.
        if {${history_stack_navigate} == 0 && ${where} == "prev"} {
            set nlen [llength $view_positions(next)]
            set len [llength $view_positions(prev)]
            if {[expr ${nlen} + ${len}] > $sn_history(stacksize)} {
                set len [expr ${nlen} + ${len} - $sn_history(stacksize)]
                set minlen [expr $sn_history(stacksize) / 2]
                if {${len} < ${minlen}} {
                    set len ${minlen}
                }
                set view_positions(prev) [lrange $view_positions(prev) 0 ${len}]
            }
            eval lappend view_positions(prev) $view_positions(next)
            set view_positions(next) ""
        }

        # Limit the number of history stack dump points.
        if {[llength $view_positions(${where})] > $sn_history(stacksize)} {
            set view_positions(${where}) [lrange $view_positions(${where}) 0\
              $sn_history(stacksize)]
        }

        # Control history/stack navigation buttons.
        history_stack_control_buttons

        # Leave stack managment.
        incr history_stack_active -1
    }

    method toolbar_post_history_stack_menu {m what} {
        set i 0
        foreach point $view_positions(${what}) {
            set wdg [lindex ${point} 0]
            if {![winfo exists ${wdg}]} {
                #view removed
                continue
            }
            ${m} add command -command " ${this} history_stack_goto_point\
		    ${what} ${i} " -label [lindex ${point} 2]
            incr i
        }
    }

    # Post menu to list all availiable saved view positions
    method toolbar_post_history_stack {what btn} {
        # Nothing in the stack
        if {$view_positions(${what}) == ""} {
            return
        }

        # Set (x,y) for option-menu
        set y [expr {[winfo rooty ${btn}] + [winfo height ${btn}]}]
        set x [winfo rootx ${btn}]

        set m .view_position_pop_menu
        catch {destroy ${m}}
        menu ${m} -title "Views" -tearoff 0 -postcommand\
          "${this} toolbar_post_history_stack_menu ${m} ${what}"
        wm overrideredirect ${m} 1
        tk_popup ${m} ${x} ${y}
    }

    # True when the user navigates between the existing
    # stack dump-points.
    protected variable history_stack_navigate 0
    protected variable history_stack_goto_point_active 0

    # Restore the first view point in the stack.
    method history_stack_goto_point {where {index 0}} {
        global Switch_Is_Enabled

        if {$view_positions(${where}) == ""} {
            return
        }

        if {${history_stack_goto_point_active}} {
            bell
            return
        }
        incr history_stack_goto_point_active 1

        set point [lindex $view_positions(${where}) ${index}]

        # Fetch the widget path and verify if it exists, if not
        # delete it from the history stack.
        set wdg [lindex ${point} 0]
        while {! [winfo exists ${wdg}]} {
            catch {set view_positions(${where})\
              [lreplace $view_positions(${where}) ${index} ${index}]}

            if {$view_positions(${where}) == ""} {
                break
            }

            set point [lindex $view_positions(${where}) ${index}]
            set wdg [lindex ${point} 0]
            if {${point} == "" || ${wdg} == ""} {
                break
            }
        }

        # Stack empty.
        if {$view_positions(${where}) == "" || ${point} == "" || ${wdg} == ""} {
            incr history_stack_goto_point_active -1
            return
        }

        # We must disable auto-switching to current position.
        ::incr Switch_Is_Enabled -1

        # Navigation between stack points is active,
        # don't delete the next-stack.
        incr history_stack_navigate

        if {${where} == "prev"} {
            set stack_history_side "next"
        } else {
            set stack_history_side "prev"
        }

        # Delete (before execution) the entry from the stack list.
        catch {set view_positions(${where})\
          [lreplace $view_positions(${where}) ${index} ${index}]}

        # Find page in corresponding to the widget, it can be a sub widget.
        set page [find_page ${wdg}]

        # Restore view point.
        if {${page} != ""} {
            catch {
                # Popup view, don't raise the window.
                view ${page} 0
                ${wdg} Restore [lindex ${point} 1]
                # Focus on the restored view.
                ${wdg} Focus
            } err
            if {${err} != ""} {
                sn_log "stack goto history:${err}"
            }
        } else {
            # A sub widget is deleted, no restore possible.
            bell
        }

        history_stack_control_buttons

        # Default is adding to previous list.
        set stack_history_side "prev"

        # Navigation between stack points finished.
        incr history_stack_navigate -1

        # Reenable switching.
        ::incr Switch_Is_Enabled

        incr history_stack_goto_point_active -1
    }

    method toolbar_display_selected {combo txt} {
        ${ActiveWidget} goto ${combo} ${txt}
    }

    method toolbar_findtext {combo txt} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} != ""} {
            ${ed} FindNext -forwards ${txt}
        } else {
            bell
        }
    }

    method toolbar_findtext_postcommand {combo} {
        global sn_options
        ${combo} configure -contents $sn_options(search)
    }

    method toolbar_search_pattern {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        if {![catch {set string [selection get]}]} {
            set off [string first "\n" ${string}]
            if {${off} != -1} {
                set string [string range ${string} 0 [expr ${off} - 1]]
            }
            ${ed} FindNext -forwards ${string}
        } else {
            ${ed} FindNext -forwards [${FindCombo} cget -entrytext]
        }
    }

    method toolbar_retriever {} {
        set ed [list_find_editor ${ActiveWidget}]
        if {${ed} == ""} {
            bell
            return
        }
        ${ed} RetrieveObject
    }

    method toolbar_symbols {{all 0}} {
        ${ActiveWidget} filter ${all}
    }

    method editor {} {
        return $itk_component(editor)
    }

    method editw {} {
        if {[catch {set w [$itk_component(editor) editor]}]} {
            return ""
        }
        return ${w}
    }

    #find a reusable window
    method is_Reusable {} {
        if {![winfo exists $itk_component(reuse)] || !${reusable} ||\
          [$itk_component(reuse) cget -state] != "normal"} {
            return 0
        }
        return 1
    }

    method ActiveWidget {} {
        return ${ActiveWidget}
    }

    method NoteBook {} {
        return $itk_component(notebook)
    }

    method view {page {rs 1}} {
        set old ${Activate_executed}
        $itk_component(notebook) raise ${page}

        #Activate isn't executed, because the page is
        #already active, add dump-point into history-stack
        if {${old} == ${Activate_executed}} {
            history_stack_add_point ${ActiveWidget}
        }
        if {${rs}} {
            raise
        }
    }

    #find a window with specified class name
    proc find_Reusable {} {
        foreach win [itcl::find objects "*" -class MultiWindow&] {
            if [${win} is_Reusable] {
		# Remove the :: namespace qualifier.
                return [namespace tail ${win}]
            }
        }
        return ""
    }

    method tools {} {
        return ${AvailTools}
    }

    #Close your self
    method Close {} {
        foreach tool ${AvailTools} {
            for {set nxt ${tool}} {${nxt} != ""} {set nxt [${nxt} next]} {
                set forced_close 0
                if {[${nxt} whoami] == "edit"} {
                    # If another editor widget has the file open, then
                    # don't ask to save if the file has been modified
                    set file [$nxt cget -filename]
                    if {[CountInstancesOfFile $file] > 1} {
                        set forced_close 1
                    } else {
                        # If we already asked about saving this filename, don't ask again
                        if {[info exists alread_Asked($file)]} {
                            set forced_close 1
                        } else {
                            set alread_Asked($file) 1
                        }
                    }
                }
                if {[${nxt} Close $forced_close] != 1} {
                    # User canceled command.
                    return 0
                }
            }
        }
        # Window can be closed.
        return 1
    }

    # Verify if all buffers can be refereted.
    proc CloseAll {} {
        foreach win [itcl::find objects "*" -class Editor&] {
            if {[info exists alread_Asked([${win} cget -filename])]} {
                continue
            }
            set alread_Asked([${win} cget -filename]) 1
            if {[${win} Close] == 0} {
                return 0
            }
        }
        # Close all symbol browsers.
        return [SymBr&::CloseAll]
    }

    method Restore {win} {
        foreach page ${win} {
            set i 0
            # Views in the page
            # format "<view name> <width> <height> <view flags>"
            foreach view [lrange ${page} 1 end] {
                if {${i} == 0} {
                    $itk_component(notebook) raise [lindex ${view} 0]
                    set w ${ActiveWidget}
                } else {
                    switch -- [lindex ${view} 0] {
                        "edit" {
                                set w [windows_splitinto_edit]
                            }
                        "ctree" {
                                set w [windows_splitinto_ctree]
                            }
                        "classbr" {
                                set w [windows_splitinto_classbr]
                            }
                        "xref" {
                                set w [windows_splitinto_xref]
                            }
                        "incbr" {
                                set w [windows_splitinto_incbr]
                            }
                        default { }
                    }
                }
                ${w} Restore [lindex ${view} 3]
                incr i
            }
        }
    }

    # The function refreshes all views after updating the project
    # XRef state is handled, not the views added to xref.
    #
    # vw:  EMPTY to update all the views
    # vw:  List of view names to update
    method Control_Display {{vw ""}} {
        global combobox_editor_scopes

        if {${vw} == ""} {
            set who_views [list edit classbr ctree xref incbr retr grep]
        } else {
            set who_views xref
        }

        # Verify if the views are still availiable.
        foreach who ${who_views} {
            set page [$itk_component(notebook) subwidget ${who}]
            set state [tool_Exists ${who}]
            $itk_component(notebook) pageconfigure ${who} -state ${state}
        }

        # Delete sub windows of disabled views
        # except XRef windows, this must be updated after
        # finishing XRef process.
        foreach tool [tools] {
            set who [${tool} whoami]
            set page [$itk_component(notebook) subwidget ${who}]
            if {${vw} != "" && ${vw} != ${who}} {
                continue
            }\
            elseif {${vw} == "" && ${who} == "xref"} {
                continue
            }
            set state [tool_Exists ${who}]

            # Delete all sub views, if the view isn't more availiable.
            if {${state} == "disabled"} {
                set oldActive ${ActiveWidget}
                set ActiveWidget ${tool}
                while {1} {
                    if {[windows_delete_last_pane ""] == 0} {
                        break
                    }
                }
                set ActiveWidget ${oldActive}
            }
        }

        # If xref is the active page, then configure it's combo box.
        if {${vw} == "xref" && [${ActiveWidget} whoami] == "xref"} {
            ${ActiveWidget} Reenable
        }

        # Check wether the combobox entries are still valid.
        if {${vw} != "xref"} {
# FIXME: ugly hack uses internals of menu in combobox !
            set opt [$itk_component(symbolcombo) component menu].symfilter
            foreach sc [lsort [array names combobox_editor_scopes]] {
                if {! [winfo exists ${opt}.${sc}]} {
                    # Read the filters.
                    Add_Symbol_Filter $itk_component(toolbar)
                    break

                }
            }
        }
    }

    method whoami {} {
        return "mainw"
    }

    # This function is called, when the project state is
    # changes, like file is saved and reparsed,
    # this function update the availiable views to
    # the latest state.
    proc Refresh_YourSelf {} {

        foreach win [itcl::find objects "*" -class MultiWindow&] {
            ${win} Control_Display
        }

        # Refresh editors.
        foreach ed [itcl::find objects "*" -class Editor&] {
            ${ed} Refresh_Display
        }

        # Refresh hierarchy.
        foreach ct [itcl::find objects "*" -class ClassTree&] {
            ${ct} Refresh_Display
        }

        # Refresh class.
        foreach cl [itcl::find objects "*" -class Class&] {
            ${cl} Refresh_Display
        }

        # Refresh xref.
        foreach xr [itcl::find objects "*" -class XRef&] {
            ${xr} Refresh_Display
        }

        # Refresh include.
        foreach inc [itcl::find objects "*" -class Include&] {
            ${inc} Refresh_Display
        }

        # Refresh Retriever (exclusive mode).
        foreach r [itcl::find objects "*" -class Retr&] {
            ${r} Refresh_Display
        }

        # Refresh Symbol browsers.
        foreach sb [itcl::find objects "*" -class SymBr&] {
            ${sb} Refresh_Display
        }
    }

    # This function is called after xref process is finished.
    proc Refresh_YourSelf_After_XRef {} {
        foreach win [itcl::find objects "*" -class MultiWindow&] {
            ${win} Control_Display xref
        }

        # Refresh xref windows.
        foreach xr [itcl::find objects "*" -class XRef&] {
            ${xr} Refresh_Display
        }
    }

    # This function saves the status of all opened multiwindows
    # in the project file.
    proc SaveYourSelf {} {

        sn_log "MutliWindow& SaveYourSelf.."

        set val ""
        foreach win [itcl::find objects "*" -class MultiWindow&] {

            # Window id.
            set wdump "window"

            # Window geometry.
            lappend wdump [winfo x [${win} component hull]]
            lappend wdump [winfo y [${win} component hull]]
            lappend wdump [winfo width [${win} component hull]]
            lappend wdump [winfo height [${win} component hull]]
            lappend wdump [wm geometry [${win} component hull]]
            lappend wdump [[${win} ActiveWidget] whoami]

            # Notepad pages.
            # Note: only save the active view, don't save all views,
            # this is not nesessary.
            foreach tool [${win} ActiveWidget] {
                set sub "page"

                if {${tool} == ""} {
                    continue
                }

                # Save the tool and it's sub windows.
                foreach page [MultiWindow&::list_items ${tool}] {
                    if {${page} == ""} {
                        continue
                    }
                    set who [${page} whoami]

                    # We don't need to dump the settings for grep&retriever.
                    if {[lsearch -exact {retr grep} ${who}] != -1} {
                        continue
                    }

                    # Save geometry.
                    set subpage ""
                    lappend subpage ${who}
                    lappend subpage [winfo width ${page}]
                    lappend subpage [winfo width ${page}]

                    # Save widget settings.
                    lappend subpage [${page} Dump]
                    lappend sub ${subpage}
                }

                lappend wdump ${sub}
            }
            lappend val ${wdump}
        }
        paf_db_proj put MultiWindows ${val}

        # Save existing browsers.
        SymBr&::SaveYourSelf

        sn_log "MutliWindow& SaveYourSelf..end"
    }

    # This function restores all the windows saved in the
    # project file.
    proc RestoreYourSelf {} {
        global sn_options
        global tkeWinNumber
        global Switch_Is_Enabled

        # First disable switching.
        ::incr Switch_Is_Enabled -1

        # Let the status window be refreshed.
        update idletasks
        update

        # First restore saved symbol browsers.
        SymBr&::RestoreYourSelf

        set val [paf_db_proj get -key MultiWindows]
        if {${val} != ""} {
            catch {
                foreach win ${val} {
                    set id [lindex ${win} 0]
                    if {${id} != "window"} {
                        continue
                    }

                    set geom [lindex ${win} 5]

                    # Create window.
                    incr tkeWinNumber
                    set nwin ".multiwindow-${tkeWinNumber}"
                    MultiWindow& ${nwin} -geometry ${geom}

                    # Restore settings.
                    ${nwin} Restore [lrange ${win} 7 end]

                    # Toggle to active page.
                    ${nwin} view [lindex ${win} 6]

                    update idletasks
                }
            }
        }
        # Reenable switching.
        ::incr Switch_Is_Enabled
    }

    # Create by default, a window with the retriever containing
    # the file list.
    # Create a symbol browser by default.
    proc Default_Window {} {
        global tkeWinNumber
        global keybindings_initialized

        # Is a main window availiable.
        set wins [itcl::find objects "*" -class MultiWindow&]
        if {${wins} != ""} {
            return
        }
        # Is a symbol browser available.
        set wins [itcl::find objects "*" -class SymBr&]
        if {${wins} != ""} {
            return
        }

        # Let the status window be refreshed.
        update idletasks
        update

        windows_new_symbr ""
    }
    
    method isakeep {} {
	return keep
    }

    private variable reusable             "1"  ; # Is this window resuable.
    private variable keep                 "1"  ; # Keep window context(??).
    private variable linenum              "0"  ; # Line number.
    private variable message              ""   ; # Statusbar message.

    protected variable AllFilterOptions

    protected variable ActivePage ""
    protected variable ActivePaned ""
    protected variable ActivePane ""
    protected variable ActiveWidget ""

    protected variable SymbolsFilter ""
    protected variable FindCombo ""
    protected variable FindEditFr ""
    protected variable  AvailTools ""

    # Contains all next positions.
    # Contains all previous positions.
    protected variable view_positions
    protected variable stack_history_side "prev"

    public variable geometry ""
    public variable symbolname $sn_options(noname_file)

    itk_option define -raise raise Raise "edit"
    public variable x ""
    public variable y ""
}

proc ProjectMenuEntries_post {m {subwindow ""} {prjsub ""}} {

    # XRef process is running <==> no save & no project editor.
    if {[sn_processes_running]} {
        set xref_state disabled
    } else {
        set xref_state normal
    }

    # No project editor/delete project during xref is running.
    ${m}${prjsub} entryconfig [get_indep String ProjectEditorMenu]\
      -state ${xref_state}
    ${m}${prjsub} entryconfig [get_indep String DeleteProject]\
      -state ${xref_state}

    # Close can only called, when there is another opend
    # windows, by sub windows it must be able to close
    # the window every time.
    if {${subwindow} == "subwindow" || [MultiWindow&::num_Existing_windows]\
      > 1} {
        set closestate normal
    } else {
        set closestate disabled
    }
    ${m} entryconfig [get_indep String WindowsClose] -state ${closestate}
}

# Add project section.
proc AddProjectMenuEntries {m topw {submenu 0}} {
    global sn_options sn_elix
    set origm ${m}

    # Add project commands into a submenu.
    if {${submenu}} {
        # Project submenu...
        set prjsub ${m}.prjsub
        menu ${prjsub} -tearoff 0

        ${prjsub} configure -font $sn_options(def,layout-font)

        ${m} add cascade -label [get_indep String ProjectMenu]\
          -underline [get_indep Pos ProjectMenu] -menu ${prjsub}

        set m ${prjsub}
    }

    # Project.
    ${m} add command -label [get_indep String NewProj] -underline\
      [get_indep Pos NewProj] -command sn_new_project

    ${m} add command -label [get_indep String OpenProject]\
	    -underline [get_indep Pos OpenProject]\
	    -command " MultiWindow&::file_open_project "

    ${m} add command -label [get_indep String DeleteProject]\
	    -underline [get_indep Pos DeleteProject]\
	    -command " MultiWindow&::delete_current_project "

    if {${m} == ${origm}} {
        ${origm} add command -label [get_indep String CloseProject]\
		-underline [get_indep Pos CloseProject]\
		-command " MultiWindow&::file_close_project "
    }

    ${m} add separator

    # Project editor.
    ${m} add command -label [get_indep String ProjectEditorMenu]\
	    -underline [get_indep Pos ProjectEditorMenu]\
	    -command project_editor

    # Add preferences to the file menu (only in symbol browser).	
    ${m} add command -label [get_indep String ProjectPreferencesMenu]\
	    -underline [get_indep Pos ProjectPreferencesMenu]\
	    -command " MultiWindow&::edit_proj_preferences 0 "

    if {${sn_elix}} {
        # EL/IX config.
        ${m} add command -label [get_indep String ElixPreferences]\
          -underline [get_indep Pos ElixPreferences]\
	  -command " MultiWindow&::edit_elix_preferences "
    }

    # Recent project files.
    set recent ${origm}.recent
    menu ${recent} -tearoff 0

    ${origm} add cascade -label [get_indep String RecentProjects]\
	    -underline [get_indep Pos RecentProjects] -menu ${recent}
    ghosting_menu_item ${origm} [get_indep String RecentProjects]\
	    "MultiWindow&::post_recent_project ${recent} ${topw} ${m}"

    if {${m} != ${origm}} {
        ${origm} add command -label [get_indep String CloseProject]\
		-underline [get_indep Pos CloseProject]\
		-command " MultiWindow&::file_close_project "
    }

}

###########################################################
## Miscellaneous tools
###########################################################
proc RescanProject {} {
    global sn_options sn_product_name

    #display a loading window
    sn_loading_message [get_indep String Scanning]

    sn_parse_uptodate

    #delete the loading window
    hide_loading_message
}

proc AddMiscSubMenu {m} {
    global sn_options

    set misc ${m}

    ${misc} configure -font $sn_options(def,layout-font)

    #Refresh project
    ${misc} add command -label [get_indep String ScanProject]\
      -underline [get_indep Pos ScanProject] -command "RescanProject"

    #Reparse project
    ${misc} add command -label [get_indep String Reparse]\
	    -underline [get_indep Pos Reparse]\
	    -command "Preferences&::Reparse ask"

    ${misc} add separator

    AddDebugFunctionality ${misc}

    # Ide Target Manager.
    ${misc} add command -label [get_indep String BuildTargetManager]\
	    -underline [get_indep Pos BuildTargetManager]\
	    -command "sn_start_build_manager"

    # Make.
    ${misc} add command -label [get_indep String MultiMake]\
	    -underline [get_indep Pos MultiMake] -command "sn_make"
}

############################################################
## Debugger functionality
############################################################
proc AddDebugFunctionality {m {topw ""}} {
    ${m} add command -label [get_indep String DbgTitle] -underline\
	    [get_indep Pos DbgTitle] -command sn_debugger
}

###########################################################
## History menu
## Make History more fun
###########################################################
proc AddHistMenu {m} {
    global sn_options
    menu ${m}.history -tearoff 0\
	    -postcommand "MultiWindow&::history_post ${m}.history"

    ${m}.history configure -font $sn_options(def,layout-font)

    ${m} add cascade -label [get_indep String History] -menu ${m}.history\
	    -underline [get_indep Pos History]
}

###########################################################
## Windows menu
## Make Windows with more fun
###########################################################
proc AddWindowsMenu {m cls {add_split 0} {add_views 1}} {
    global sn_options
    set win ${m}.windows
    menu ${win} -postcommand "MultiWindow&::windows_post ${cls} ${m}.windows\
	    [expr ${add_split} + ${add_views}]" -tearoff 0
    ${m} add cascade -label [get_indep String Windows] -menu ${win}\
	    -underline [get_indep Pos Windows]

    ${m}.windows configure -font $sn_options(def,layout-font)

    # Submenu to create views.
    set new ${m}.windows.new
    menu ${new} -tearoff 0\
	    -postcommand "MultiWindow&::windows_new_post ${new}"

    ${new} configure -font $sn_options(def,layout-font)

    ${m}.windows add cascade -label [get_indep String NewWork] -menu ${new}\
	    -underline [get_indep Pos NewWork]

    # New editor.
    ${new} add command -command " MultiWindow&::windows_new_window ${new} "\
	    -label [get_indep String WindowsNew]\
	    -underline [get_indep Pos WindowsNew]\
	    -accelerator "F5"

    # New Class Hierarchy.
    ${new} add command -command " MultiWindow&::windows_new_window ${new}\
	    ctree " -label [get_indep String MultiClassHierarchy]\
	    -underline [get_indep Pos MultiClassHierarchy] -accelerator "F6"

    # New Class.
    ${new} add command -command " MultiWindow&::windows_new_window ${new}\
	    classbr " -label [get_indep String MultiClass]\
	    -underline [get_indep Pos\
	    MultiClass] -accelerator "F7"

    # New Xref.
    ${new} add command\
	    -command " MultiWindow&::windows_new_window ${new} xref "\
	    -label [get_indep String MultiXRef]\
	    -underline [get_indep Pos MultiXRef] -accelerator "F8"

    # New Include.
    ${new} add command\
	    -command " MultiWindow&::windows_new_window ${new} incbr "\
	    -label [get_indep String MultiInclude]\
	    -underline [get_indep Pos MultiInclude] -accelerator "F9"

    ${new} add separator

    # New CTree-Class.
    ${new} add command\
	    -command " MultiWindow&::windows_new_ctree_class ${new} "\
	    -label [get_indep String WindowsNewCTreeClass]\
	    -underline [get_indep Pos WindowsNewCTreeClass]

    # New Class-Edit.
    ${new} add command\
	    -command " MultiWindow&::windows_new_class_edit ${new} "\
	    -label [get_indep String WindowsNewClassEditor]\
	    -underline [get_indep Pos WindowsNewClassEditor]

    # New Class-Xref.
    ${new} add command\
	    -command " MultiWindow&::windows_new_class_xref ${new} "\
	    -label [get_indep String WindowsNewClassXRef]\
	    -underline [get_indep Pos WindowsNewClassXRef]

    # New Retriever-Edit.
    ${new} add command\
	    -command " MultiWindow&::windows_new_retr_edit ${new}"\
	    -label [get_indep String WindowsNewRetrEditor]\
	    -underline [get_indep Pos WindowsNewRetrEditor]

    # New Find-Edit.
    ${new} add command\
	    -command " MultiWindow&::windows_new_find_edit ${new}"\
	    -label [get_indep String WindowsNewFindEditor]\
	    -underline [get_indep Pos WindowsNewFindEditor]

    # New CTreeClassEditor.
    ${new} add command\
	    -command " MultiWindow&::windows_new_ctree_class_edit ${new} "\
	    -label [get_indep String WindowsNewCTreeClassEditor]\
	    -underline [get_indep Pos WindowsNewCTreeClassEditor]

    ${new} add separator

    # New Symbol browser.
    ${new} add command -command " MultiWindow&::windows_new_symbr ${new} "\
	    -label [get_indep String MultiBrowser]\
	    -underline [get_indep Pos MultiBrowser]\
	    -accelerator "F4"

    # We support now splited windows.
    if {${add_split} && ${cls} != ""} {
        set split ${win}.split
        menu ${split} -postcommand "${cls} windows_split_post ${split}"\
          -tearoff 0

        ${split} configure -font $sn_options(def,layout-font)

        ${win} add cascade -label [get_indep String SplitInto] -menu ${split}\
		-underline [get_indep Pos SplitInto]

        # Split into editor.
        ${split} add command\
		-command " ${cls} windows_splitinto_edit ${split}"\
		-label [get_indep String SplitIntoEditor]\
		-underline [get_indep Pos SplitIntoEditor]\
		-accelerator "Ctrl+F5"

        # Split into class hierarchy.
        ${split} add command\
		-command " ${cls} windows_splitinto_ctree ${split} "\
		-label [get_indep String SplitIntoClassHierarchy]\
		-underline [get_indep Pos SplitIntoClassHierarchy]\
		-accelerator "Ctrl+F6"

        # Split into class.
        ${split} add command\
		-command " ${cls} windows_splitinto_classbr ${split} "\
		-label [get_indep String SplitIntoClass]\
		-underline [get_indep Pos SplitIntoClass]\
		-accelerator "Ctrl+F7"

        # Split into xref.
        ${split} add command\
		-command " ${cls} windows_splitinto_xref ${split}"\
		-label [get_indep String SplitIntoXRef]\
		-underline [get_indep Pos SplitIntoXRef]\
		-accelerator "Ctrl+F8"

        # Split into include browser.
        ${split} add command\
		-command " ${cls} windows_splitinto_incbr ${split} "\
		-label [get_indep String SplitIntoInclude]\
		-underline [get_indep Pos SplitIntoInclude]\
		-accelerator "Ctrl+F9"

        # Split into build winow.
        ${split} add command\
		-command " ${cls} windows_splitinto_build ${split} "\
		-label [get_indep String BuildMenuItem]\
		-underline [get_indep Pos BuildMenuItem]\
		-accelerator "Ctrl+F10"

        ${split} add separator

        # Delete last pane.
        ${split} add command\
		-command " ${cls} windows_delete_last_pane ${split} "\
		-label [get_indep String SplitDeleteLast]\
		-underline [get_indep Pos SplitDeleteLast]\
		-accelerator "Ctrl+F4"
    }

    ######################################################
    #Availiable Views
    #To list the availiable views
    ######################################################

    if {${add_views}} {
        set views ${win}.views
        menu ${views}\
		-postcommand "MultiWindow&::windows_views_post ${views}"\
		-tearoff 0
        ${win} add cascade -label [get_indep String Views] -menu ${views}\
		-underline [get_indep Pos Views]

        # Editor views.
        set edview ${views}.edit
        menu ${edview}\
		-postcommand "MultiWindow&::windows_views_edit_post ${edview}"\
		-tearoff 0

        ${views} add cascade -label [get_indep String MultiEditor]\
		-menu ${edview} -underline [get_indep Pos MultiEditor]

        # Hierarchy views.
        set hview ${views}.ctree
        menu ${hview}\
		-postcommand "MultiWindow&::windows_views_ctree_post ${hview}"\
		-tearoff 0
        ${views} add cascade -label [get_indep String MultiClassHierarchy]\
		-menu ${hview} -underline [get_indep Pos MultiClassHierarchy]

        # Class views.
        set cview ${views}.class
        menu ${cview}\
		-postcommand "MultiWindow&::windows_views_class_post ${cview}"\
		-tearoff 0
        ${views} add cascade -label [get_indep String MultiClass]\
		-menu ${cview} -underline [get_indep Pos MultiClass]

        # Xref views.
        set xview ${views}.xview
        menu ${xview}\
		-postcommand "MultiWindow&::windows_views_xref_post ${xview}"\
		-tearoff 0
        ${views} add cascade -label [get_indep String MultiXRef]\
		-menu ${xview} -underline [get_indep Pos MultiXRef]

        # Include views.
        set iview ${views}.iview
        menu ${iview}\
		-postcommand "MultiWindow&::windows_views_inc_post ${iview}"\
		-tearoff 0
        ${views} add cascade -label [get_indep String MultiInclude]\
		-menu ${iview} -underline [get_indep Pos MultiInclude]

        # Retriever views.
        set rview ${views}.rview
        menu ${rview}\
		-postcommand "MultiWindow&::windows_views_retr_post ${rview}"\
		-tearoff 0
        ${views} add cascade -label [get_indep String MultiRetriever]\
		-menu ${rview} -underline [get_indep Pos MultiRetriever]

        # Grep views.
        set gview ${views}.gview
        menu ${gview}\
		-postcommand "MultiWindow&::windows_views_grep_post ${gview}"\
		-tearoff 0
        ${views} add cascade -label [get_indep String MultiGrep]\
		-menu ${gview} -underline [get_indep Pos MultiGrep]
    }
    ######################################################
    # END of availiable views
    ######################################################

    # Iconize project.
    ${m}.windows add command\
	    -command " MultiWindow&::windows_iconize ${m}.windows ${cls} "\
	    -label [get_indep String WindowsIconize]\
	    -underline [get_indep Pos WindowsIconize]
    ${m}.windows add separator
    ${m}.windows add command -label "dummy"
}

proc raise_toplevel {w} {
    global sn_force_flag
    if {[wm state ${w}] == "iconic"} {
        wm deiconify ${w}
    } else {
        # Make sure that the window has focus on windows.
        catch {
            set wdg [focus -lastfor ${w}]
            eval focus ${sn_force_flag} [${w} twidget]
            focus ${wdg}
        }
        raise ${w}
    }
}

# Help menu.
proc AddHelpMenu {m topw} {
    global sn_options

    menu ${m}.help -tearoff 0

    ${m} add cascade -label [get_indep String Help] -menu ${m}.help\
	    -underline [get_indep Pos Help]

    ${m}.help configure -font $sn_options(def,layout-font)

    ${m}.help add command -command " sn_help "\
	    -label [get_indep String HelpContents]\
	    -underline [get_indep Pos HelpContents]

    ${m}.help add command -label [get_indep String Abrav]\
	    -underline [get_indep Pos Abrav]\
	    -command " sn_show_abbrav "

    ${m}.help add separator

    ${m}.help add command -label [get_indep String Version]\
	    -underline [get_indep Pos Version]\
	    -command " focus ${topw}
                       sn_show_version_window
                       "
}

# Synchronize more than one editor with the same file name
# to contain the same data.

global SyncEditors_Disabled
global SyncEditors_Active

set SyncEditors_Disabled 0
set SyncEditors_Active 0

# whenever we exit this function prematurely we need to
# reset $SyncEditors_Active - else the famous 
# "cant save changed files" bug creeps up again.
proc SyncEditors {cls args} {
    global sn_options
    global SyncEditors_Disabled
    global SyncEditors_Active

    if {${SyncEditors_Disabled}} {
        return
    }

    if {$SyncEditors_Active} {
	return
    }

    if {[itcl::find object -isa Editor& $cls] == ""} {
	set cls [winfo parent $cls]
    }

    # Do not sync it if it's not an Editor&.
    if {[itcl::find objects -isa Editor& $cls]==""} {
        return
    }

    set SyncEditors_Active 1

    set file [${cls} cget -filename]

    # Edit a new file.
    if {${file} != $sn_options(noname_file)} {
        set editor [${cls} editor]
        set cmd [lindex ${args} 0]

        switch ${cmd} {
            "insert" {
                    set pos [${editor} index [lindex ${args} 1]]
                    set args [lreplace ${args} 1 1 ${pos}]
            	    if {[string compare [lindex ${args} 2] ""] == 0} {
			set SyncEditors_Active 0
			return
                    }
                }
            "delete" {
                    set pos1 [${editor} index [lindex ${args} 1]]
                    set args [lreplace ${args} 1 1 ${pos1}]
                    set pos2 [lindex ${args} 2]
                    if {${pos2} != ""} {
                        set pos2 [${editor} index ${pos2}]
                        set args [lreplace ${args} 2 2 ${pos2}]
                    }
                }
            default {
                    set SyncEditors_Active 0
                    return
                }
        }

        foreach obj [itcl::find objects "*" -class Editor&] {
            set cls [namespace tail $cls]
            if {${obj} != ${cls} && [${obj} cget -filename] == ${file}} {
                eval [${obj} editor] ${args}
                if {[${obj} cget -file_changed] == 0} {
                    ${obj} setmodified 1
                    ${obj} SetTitle
                }
            }
        }
    }

    if {[${cls} cget -file_changed] == 0} {
        ${cls} setmodified 1
        ${cls} SetTitle
    }
    set SyncEditors_Active 0

}

# Counts the number of Editor wigets that
# have $file currently open.

proc CountInstancesOfFile {file} {

    set filecount 0

    foreach obj [itcl::find objects "*" -class Editor&] {
        if {[$obj cget -filename] == $file && ![$obj wasClosed]} {
            incr filecount
        }
    }

    return $filecount
}

#########################################################
# Global keybindings
#########################################################

# Returns the current class from the active window.
proc get_active_class {} {
    set fcs [focus]
    if {${fcs} == ""} {
        return ""
    }
    set cls ".[lindex [split ${fcs} .] 1]"
    if {${cls} == "."} {
        return ""
    }
    return ${cls}
}

proc del_last_view {} {
    set cls [get_active_class]
    if {${cls} == ""} {
        bell
        return
    }
    catch {${cls} windows_delete_last_pane ""}
}

proc add_view {vw} {
    set cls [get_active_class]
    if {${cls} == ""} {
        bell
        return
    }
    catch {${cls} windows_splitinto_${vw}}
}

proc switch_to_view {view} {

    set cls [get_active_class]

    if {${cls} == ""} {
        bell
        return
    }

    # Verify if the view can be launched.

    if {[tool_Exists ${view}] != "normal"} {
        bell
        return
    }

    catch {${cls} view ${view}}
}

proc main_print {w} {
    set top [winfo toplevel ${w}]
    if {[itcl::find objects -isa MultiWindow& ${top}] != ""} {
        ${top} file_print
    }
}

proc close_main_window {w} {

    # This procedure is bound on Control-w, so don't
    # close window when it's the last one in the project.

    if {[MultiWindow&::num_Existing_windows] > 1} {
        catch {[winfo toplevel ${w}] windows_close dummy}
    }
}

proc find_dialog {w} {
    set top [winfo toplevel ${w}]
    if {[itcl::find objects -isa MultiWindow& ${top}] != ""} {
        ${top} search_findtext
    }
}

proc switch_to_next_view {w} {
    set top [winfo toplevel ${w}]
    if {[itcl::find objects -isa MultiWindow& ${top}] != ""} {
        ${top} switch_to_next_view ${w}
    }
}

proc switch_tab {w op} {
    set top [winfo toplevel ${w}]
    if {[itcl::find objects -isa MultiWindow& ${top}] != ""} {
        ${top} switch_tab ${op}
    }
    if {[itcl::find objects -isa Preferences& ${top}] != ""} {
        ${top} switch_tab ${op}
    }
}

proc switch_tix_notebook_tab {tixnb op} {
    # Create a list that starts with the current page
    # and ends with the previous or next page
    # (forward if op is next, backward if op is prev)
    set Pages [$tixnb pages]
    set raised [$tixnb raised]
    set ind [lsearch -exact $Pages $raised]
    if {$ind == -1} {
        error "raised page \"$raised\" not found in \{$Pages\}"
    }

    if {$op == "prev"} {
        set page_list [list]
        for {set pos [expr {$ind - 1}]} {$pos >= 0} {incr pos -1} {
            lappend page_list [lindex $Pages $pos]
        }
        for {set pos [expr {[llength $Pages] - 1}]} {$pos > $ind} {incr pos -1} {
            lappend page_list [lindex $Pages $pos]
        }
    } elseif {$op == "next"} {
        set page_list [lrange $Pages [expr {$ind + 1}] end]
        foreach page [lrange $Pages 0 [expr {$ind - 1}]] {
            lappend page_list $page
        }
    }

    # Loop over the tab pages (forwards or backwards) and
    # find the first one in a state that can be activated
    foreach page $page_list {
        if {[$tixnb pagecget $page -state] == "normal"} {
            $tixnb raise $page
            return
        }
    }
    return
}


##############################################################
## End of keybindings
##############################################################

proc sn_start_build_manager {} {

    if {[itcl::find objects .build_manager]!=""} {
        .build_manager raise
    } else {
        snIdeTargetMgr .build_manager
    }

}

# dbg_start
# Start the nominated debugger.  Pop up an error dialog if the exec fails.

proc dbg_start {} {
    global sn_options

    set cmd $sn_options(def,gdb-command)
    if {[catch {exec ${cmd} &} msg] != 0} {
        sn_error_dialog ${msg}
    }
}

# Last accessed editor.
set last_Editor ""

