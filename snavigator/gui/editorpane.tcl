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
#####################################################
##
## Editor that can be integrated in any subwindow.
##
#####################################################

option add *Editor&.width 80 widgetDefault
option add *Editor&.height 25 widgetDefault
option add *Editor&.wrap "none" widgetDefault
option add *Editor&.editsate "normal" widgetDefault

itcl::class Editor& {
    global sn_options

    inherit sourcenav::MultiChild
    constructor {args} {
	global sn_options

	set class [${this} info class]

	# Create the text editor.
	itk_component add editor {
	    text $itk_component(hull).editor \
	    	-relief raised \
	    	-bd 0 \
	   	-yscrollcommand "$itk_component(hull).yview set" \
	    	-xscrollcommand "$itk_component(hull).xview set"
        } {
	    keep -width -height -wrap
	    rename -state -editstate editState State
	}

	# Init some variables.
	set editor $itk_component(hull).editor

	set topw [winfo toplevel $itk_component(hull)]

	# Init some local/global variables.
	init_Editor

        # Init options after setting up class variables
	eval itk_initialize $args

	# Add menu entries, if availiable.
	if {$itk_option(-menu) != ""} {
	}

	# Add toolbar icons.
	if {$itk_option(-toolbar) != ""} {
	    itk_component add toolbar {
		    frame $itk_option(-toolbar).edfr
            }
	    
	    pack $itk_component(toolbar) -side left

	    # Combobox for the find command
	    # do not use filter.

	    itk_component add find_history {
		Combo& $itk_component(toolbar).find_hist \
	        	-width 20 \
	        	-selectcommand "${this} toolbar_findtext $itk_component(toolbar).find_hist" \
	        	-postcommand "${this} toolbar_findtext_postcommand \
	      			$itk_component(toolbar).find_hist"
	    } { }
	    
	    # FIXME: references to FindCombo should probably be
	    # replaced with itk_component(find_history).
	    set FindCombo $itk_component(find_history)

	    bind $itk_component(find_history).entry <Control-f> \
	      [bind $itk_component(find_history).entry <Return>]

	    balloon_bind_info $itk_component(find_history).entry [get_indep String \
	      EINFOEnterPattern]

	    balloon_bind_info [$itk_component(find_history) component arrow] [get_indep String \
	      EINFOSearchHistory]

	    pack $itk_component(find_history) \
	        -side left

	    itk_component add find {
		    button $itk_component(toolbar).find \
		        -takefocus 0 \
	        	-image find_image \
	        	-command "${this} toolbar_search_pattern"
	    }
	    
	    balloon_bind_info $itk_component(find) [get_indep String \
	      SearchFindSelectionINFO]

	    pack $itk_component(find) \
	        -side left

	    # FIXME: This is a nasty hack to pat a gap in the toolbar.
	    itk_component add gap {
	        frame $itk_component(toolbar).gap -width 5
	    }
	    
            pack $itk_component(gap) -side left
            
	    
	    # Retriever.
	    itk_component add definition {
 	        button $itk_component(toolbar).definition \
	            -takefocus 0 \
	            -image search_image \
	            -command "${this} RetrieveObject"
            }
	    
	    bind_history $itk_component(definition) retrieve
	    balloon_bind_info $itk_component(definition) [get_indep String \
	      RetrieverINFO]
	    pack $itk_component(definition) \
	        -side left

	    # Grep.
	    itk_component add grep {
	        button $itk_component(toolbar).grep \
	            -takefocus 0 \
	            -image grep_image \
	            -command "${this} toolbar_grep"
            }
	    
	    bind_history $itk_component(grep) grep
	    balloon_bind_info $itk_component(grep) [get_indep String INFOGrep]
	    pack $itk_component(grep) \
	        -side left
	}

	# Set project state to the editor.
	if {$sn_options(readonly)} {
	    $this configure -editstate disabled
	} else {
	    $this configure -editstate normal
	}
	
	# Make some initials for editor layout.
	Update_Layout

	# Make some bindings for the editor.
	make_editor_bindings $itk_component(editor)

	# Create scollbars.
	scrollbar $itk_component(hull).xview \
	    -command "$itk_component(editor) xview" \
	    -orient horizontal
	scrollbar $itk_component(hull).yview \
	    -command "$itk_component(editor) yview"

	grid $itk_component(editor) -row 0 -column 0 -sticky news
	grid $itk_component(hull).xview -row 1 -column 0 -sticky ew
	grid $itk_component(hull).yview -row 0 -column 1 -sticky ns

	grid rowconfigure $itk_component(hull) 0 -weight 1
	grid columnconfigure $itk_component(hull) 0 -weight 1

	# Call user defined procedure.
	if {[catch {sn_rc_editor $itk_component(hull) $itk_component(editor)} err]} {
	    sn_log "error while exec sn_rc_editor: ${err}"
	}
    }

    destructor {
	foreach v [::info globals "${this}-*"] {
	    catch {uplevel #0 unset ${v}}
	}
    }

    method __cget {item} {
	set item [string range ${item} 1 end]
	return [set ${item}]
    }

    method iset {item value} {
	return [set ${item} ${value}]
    }

    method init_Editor {} {
	global sn_options

	# Take last used string for search.
	set edit_StringSearch [lindex $sn_options(search) 0]

#FIXME: This is a rather lame way to init the variables.

	catch {
	    set edit_SearchNoCase $sn_options(search,nocase)
	    set edit_SearchMethod $sn_options(search,method)
	    set edit_SearchString $sn_options(search,str)
	}

	if {! [info exists edit_SearchNoCase]} {
	    set edit_SearchNoCase "-nocase"
	}
	if {! [info exists edit_SearchMethod]} {
	    set edit_SearchMethod "-exact"
	}
	if {! [info exists edit_SearchDirection]} {
	    set edit_SearchDirection "-forwards"
	}
    }

    #############################################
    # Toolbar button commands
    
    protected variable FindCombo ""

    method toolbar_findtext_postcommand {combo} {
	global sn_options
	$itk_component(find_history) configure -contents $sn_options(search)
    }
    
    method toolbar_findtext {combo txt} {
	FindNext \
	    -forwards ${txt}
    }
    
    method toolbar_search_pattern {} {
	if {![catch {set string [selection get]}]} {
	    set off [string first "\n" ${string}]
	    if {${off} != -1} {
		set string [string range ${string} 0 [expr ${off} - 1]]
	    }
	    FindNext \
	        -forwards ${string}
	} else {
	    FindNext \
	        -forwards [$itk_component(find_history) cget -entrytext]
	}
    }

    #Execute grep with selection
    method toolbar_grep {} {
	$itk_option(-parent) search_grep
    }
    ############################################

    ###################################
    #make some bindings for the editor
    ###################################
    method make_editor_bindings {t} {
	global sn_options
	global tcl_platform

	set trav $sn_options(sys,alt-traverse)

	bind ${t} <FocusIn> "${this} Focus_In"
	bind ${t} <FocusOut> "${this} Focus_Out"

	#Bingings for traversal into the editor
	foreach b [list <Left> <Right> <Up> <Down> <Shift-Left> <Shift-Right> \
	  <Shift-Up> <Shift-Down> <Control-Left> <Control-Right> <Control-Up> \
	  <Control-Down> <Shift-Control-Left> <Shift-Control-Right> \
	  <Shift-Control-Up> <Shift-Control-Down> <Prior> <Shift-Prior> \
	  <Next> <Shift-Next> <Home> \
	  <Shift-Home> <End> <Shift-End> <Control-Home> <Control-Shift-Home> \
	  <Control-End> <Control-Shift-End>] {

	    bind ${t} ${b} "[bind Text ${b}]; ${this} find_and_mark_tag; break"
	}

	bind ${t} <ButtonRelease-1> "+
			focus %W
			${this} find_and_mark_tag
		"
	bind ${t} <Control-l> "
				%W see lastpos
				%W mark set insert lastpos
				
				${this} find_and_mark_tag
				break"
	bind ${t} <Control-m> "
				tkTextExchangeMark %W
				
				${this} find_and_mark_tag
				break
				"
	# Save File.
	bind ${t} <Control-s> "${this} save_file; break"
	
	# Quick save.
	bind ${t} <${trav}-s> "${this} fastsave_file; break"
	bind ${t} <${trav}-S> [bind ${t} <${trav}-s>]

	# Redo.
	bind ${t} <Control-Shift-z> "${this} Redo; break"
	bind ${t} <Control-Shift-Z> "${this} Redo; break"
	
	# Cut.
	bind ${t} <Shift-Delete> "${this} Cut; break"
	
	# Copy.
	bind ${t} <Control-Insert> "${this} Copy; break"
	
	# Paste.
	bind ${t} <Shift-Insert> "${this} Paste; break"

	# Insert tab/return.
	bind ${t} <Tab> {Editor&::InsertTab %W; break}
	bind ${t} <Return> {Editor&::Newline %W; break}

	# Pass Control-Tab and Shift-Control-Tab up to all bindtag
	bind ${t} <Control-Tab> continue
	bind ${t} <Shift-Control-Tab> continue

	# Pass Control-Prior (PgUp) and Control-Next (PgDn) up to all bindtag
	bind ${t} <Control-Prior> continue
	bind ${t} <Control-Next> continue

	# Grep accelerator.
	bind ${t} <Shift-Control-G> "${this} toolbar_grep; break"

	# Search bindings.

	# Find-dialog.
	bind ${t} <Control-f> "${this} FindText; break"

	# Search backward.
	# <Control-F>==<Control-Shift-f>
	bind ${t} <Control-F> "${this} FindNext -backwards; break"
	bind ${t} <Control-Shift-F> [bind ${t} <Control-F>]
	bind ${t} <Shift-F3> [bind ${t} <Control-F>]
	
	# Search forward.
	bind ${t} <F3> "${this} FindNext -forwards; break"
	
	# Sun Find.
	bind ${t} <F19> [bind ${t} <Control-f>]

	bind ${t} <Control-r> "${this} Replace; break"
	bind ${t} <Control-g> "${this} GotoLine; break"
	bind ${t} <Shift-Control-R> "${this} RetrieveObject; break"

	# Definition & implementation.
	bind ${t} <${trav}-d> "Editor&::search_definition ${this}; break"
	bind ${t} <${trav}-D> [bind ${t} <${trav}-d>]

	bind ${t} <${trav}-i> "Editor&::search_implementation ${this}; break"
	bind ${t} <${trav}-I> [bind ${t} <${trav}-i>]

	bind ${t} <${trav}-h> "sn_classtree \[Editor&::Get_XSelection \
	  ${this}\]; break"
	bind ${t} <${trav}-H> [bind ${t} <${trav}-h>]

	bind ${t} <${trav}-c> "sn_classbrowser \[Editor&::Get_XSelection \
	  ${this}\]; break"
	bind ${t} <${trav}-C> [bind ${t} <${trav}-c>]

	bind ${t} <${trav}-x> "sn_xref both \[Editor&::Get_XSelection \
	  ${this}\]; break"
	bind ${t} <${trav}-X> [bind ${t} <${trav}-x>]

	# New file.
	bind ${t} <Control-n> "${this} new_file; break"
	bind ${t} <Control-o> "${this} open_file; break"

	# Bindings for Indent/Outdent.
	bind ${t} <Control-greater> "Editor&::Indent ${this} indent; break"
	bind ${t} <Control-less> "Editor&::Indent ${this} outdent; break"

	# Define the queue, how the event are handled.
	sn_add_tags $itk_component(editor) Editor 2
    }

    # Default bindings for the text editor.
    proc EditorBindings {} {
	global sn_options

	set trav $sn_options(sys,alt-traverse)
	set t Editor

	bind ${t} <Control-space> {tkTextSetMark %W insert; break}

	bind ${t} <greater> {+Editor&::Insert_Mark_Bracket %W %A}
	bind ${t} <quotedbl> {+Editor&::Insert_Mark_Bracket %W %A}
	bind ${t} <parenright> {+Editor&::Insert_Mark_Bracket %W %A}
	bind ${t} <bracketright> {+Editor&::Insert_Mark_Bracket %W %A}
	bind ${t} <braceright> {+Editor&::Insert_Mark_Bracket %W %A}

	# "{" "[" "]" "}" are bound on Alt-(7,8,9,0), so we need to bind the
	# numbers for windows nt/95, since bind <braceright> doesn't work.
	bind ${t} <KeyPress-9> {+Editor&::Insert_Mark_Bracket %W %A}
	bind ${t} <KeyPress-0> {+Editor&::Insert_Mark_Bracket %W %A}

	# Traversal on shift-tab.
	bind ${t} <Shift-Tab> {focus [tk_focusPrev %W]}

        # Sun UNDO.
	bind ${t} <F14> [bind Text <Control-z>]

	bind ${t} <Insert> "Editor&::set_overwrite %W \$tkText(%W,ovwrt);break"

	bind ${t} <Double-1><ButtonRelease-1> {
	    switch -- [%W get {insert - 1 char}] {
	        "\}" {
	            Editor&::Insert_Mark_Bracket %W "\}" 0
	        }
	        "\{" {
	            Editor&::Insert_Mark_Bracket %W "\{" 0
	        }
	        ")" {
	            Editor&::Insert_Mark_Bracket %W ")" 0
	        }
	        "(" {
	            Editor&::Insert_Mark_Bracket %W "(" 0
	        }
	        ">" {
	            Editor&::Insert_Mark_Bracket %W ">" 0
	        }
	        "<" {
	            Editor&::Insert_Mark_Bracket %W "<" 0
	        }
	        "\]" {
	            Editor&::Insert_Mark_Bracket %W "\]" 0
	        }
	        "\[" {
	            Editor&::Insert_Mark_Bracket %W "\[" 0
	        }
	        "\"" {
	            Editor&::Insert_Mark_Bracket %W "\"" 0
	        }
	        "\'" {
	            Editor&::Insert_Mark_Bracket %W "\'" 0
	        }
	    }
	    break
	}
    }

    method m3_post_menu {w X Y x y} {
	set m .sn_pop_menu_listbox

	# It has to be destroyed because we might have problems with "tk_popup"!
	catch {destroy ${m}}
	menu ${m} \
	    -tearoff 0 \
	    -postcommand "${this} m3_post_menu_update ${m} ${x} ${y}"
	wm overrideredirect ${m} 1
	tk_popup ${m} ${X} ${Y}
    }

    method m3_post_menu_update {m x y} {

	# Delete old entries.
	${m} delete 0 end

	# Add edit commands (Undo,Copy,Cut,Delete,..).
	if {$itk_option(-file_changed)} {
	    set state "normal"
	} else {
	    set state "disabled"
	}
	set str [get_indep String EditUndo]
	${m} add command \
	    -label [get_indep String EditUndo] \
	    -underline [get_indep Pos EditUndo] \
	    -accelerator "Ctrl+Z" \
	    -command "${this} Undo" \
	    -state ${state}

	if {[catch {$itk_component(editor) get sel.first}]} {
	    set state "disabled"
	} else {
	    set state "normal"
	}
	${m} add command \
	    -label [get_indep String EditCut] \
	    -underline [get_indep Pos EditCut] \
	    -accelerator "Ctrl+X" \
	    -command "${this} Cut" \
	    -state ${state}

	${m} add command \
	    -label [get_indep String EditCopy] \
	    -underline [get_indep Pos EditCopy] \
	    -accelerator "Ctrl+C" \
	    -command "${this} Copy" \
	    -state ${state}

	set str [get_indep String EditPaste]

	if {[catch {selection get \
	        -displayof $itk_component(editor) \
	        -selection CLIPBOARD}]} {
	    set state "disabled"
	} else {
	    set state "normal"
	}
	${m} add command \
	    -label [get_indep String EditPaste] \
	    -underline [get_indep Pos EditPaste] \
	    -accelerator "Ctrl+V" \
	    -command "${this} Paste" \
	    -state ${state}
	
	# NOT YET: external editor
	#${m} add separator
	#    -accelerator "Ctrl+U" \

	${m} add command \
	    -label [get_indep String ExternalEditorOpenIn] \
	    -underline 2 \
	    -command "${this} open_external_editor" \
	    -state "normal"
	${m} add separator

	# Text sensitive commands, (Retriever, Views).
	AddRetrieveMenu ${m} ${this} $itk_component(editor) ${x} ${y}
    }

    # Take only valid identifier characters
    # to retrieve valid symbols.
    proc format_for_retriever {string} {
	# Don't delete spaces in the middle of the text.
	regexp \
	    -- "\[^\t\n\r^#%*{};',`\"\\\?()\]*" ${string} string

	return [string trim ${string}]
    }

    # Verify if we stay on a symbol declaration/implementaion
    # this procedure returns the key tagname pointing to,
    # when it's true.
    proc points_to_header {ed pos} {
	global sn_scopes
	set lasttag ""
	foreach tg [${ed} tag names ${pos}] {
	    if {[string first " " ${tg}] == -1} {
		# Tags like "scope".
		set lasttag ${tg}
	    }
	}
	if {[lsearch \
	    -exact ${sn_scopes} ${lasttag}] == -1} {
	    set lasttag ""
	}
	return ${lasttag}
    }

    # Adds the option menu for editor.
    proc AddRetrieveMenu {m cls ed x y} {
	global sn_options sn_scopes

	# Verify if the user has clicked on
	# 1. a selection       <==> retrieve selection
	# 2. outside of a selected region <==> retrieve current word
	# 3. word              <==> retrieve word
	# 4. nothing or unused <==> retrieve current symbol
	# 5. No scope active   <==> disabled
	set pos [${ed} index @${x},${y}]

	# Verify if we stay on a symbol declaration/implementaion
	set scopetag [points_to_header ${ed} ${pos}]
	if {${scopetag} != ""} {
	    set file [${cls} getvalidfilename]
	} else {
	    set file ""
	}

	set selrng [${ed} tag ranges sel]
	set current 1
	set string ""
	if {${selrng} != ""} {
	    set pos1 [split ${pos} "."]
	    set by [lindex ${pos1} 0]
	    set bx [lindex ${pos1} 1]

	    set rng1 [split [lindex ${selrng} 0] "."]
	    set rng2 [split [lindex ${selrng} 1] "."]

	    set sy1 [lindex ${rng1} 0]
	    set sx1 [lindex ${rng1} 1]
	    set sy2 [lindex ${rng2} 0]
	    set sx2 [lindex ${rng2} 1]
	    # Clicked in the selection region.
	    if {${by} == ${sy1} && ${bx} >= ${sx1} &&(${sx2} >= ${bx} || \
	      ${sy2} > ${by}) || ${by} == ${sy2} && ${bx} <= ${sx2} &&(${sx1} \
	      <= ${bx} || ${sy1} < ${by}) || ${by} > ${sy1} && ${by} < ${sy2}} {
		set string [format_for_retriever [lindex [${cls} Selection] 1]]
		set current 0
	    }
	}
	if {${current} || ${string} == ""} {
	    # Current word.
	    set string [format_for_retriever [${ed} get "${pos} wordstart" \
	      "${pos} wordend"]]

	    # Take current symbol.
	    if {${string} == ""} {
		set tg [${cls} find_tag ${pos}]
		if {${tg} != ""} {
		    split_symbol ${tg} class sym scope
		    if {${class} != ""} {
			set string "${class}\:\:${sym}"
		    } else {
			set string ${sym}
		    }
		    set string [format_for_retriever ${string}]
		}
	    }
	}
	if {${string} == ""} {
	    set findstate disabled
	    set lbl ""
	} else {
	    set findstate normal
	    set lbl " [get_indep String Of] '${string}'"
	}

	# Retrieve definition.
	${m} add command \
	    -accelerator "$sn_options(sys,alt-accelpref)+D" \
	    -label "[get_indep String SearchDefinition]${lbl}" \
	    -underline [get_indep Pos SearchDefinition] \
	    -command "Editor&::search_definition ${cls} [list ${string}]" \
	    -state ${findstate}

	# Retrieve implementation.
	${m} add command \
	    -accelerator "$sn_options(sys,alt-accelpref)+I" \
	    -label "[get_indep String SearchImplementation]${lbl}" \
	    -underline [get_indep Pos SearchImplementation] \
	    -command "Editor&::search_implementation ${cls} [list ${string}]" \
	    -state ${findstate}

	${m} add separator

	set vw ""
	set vlen [string length ${vw}]

	# View Hierarchy.
	${m} add command \
	    -accelerator "$sn_options(sys,alt-accelpref)+H" \
	    -label "${vw}[get_indep String MultiClassHierarchy] ${lbl}" \
	    -underline [expr [get_indep Pos MultiClassHierarchy] + ${vlen}] \
	    -command "sn_classtree [list ${string}]" \
	    -state ${findstate}

	# View Class.
	${m} add command \
	    -accelerator "$sn_options(sys,alt-accelpref)+C" \
	    -label "${vw}[get_indep String MultiClass] ${lbl}" \
	    -underline [get_indep Pos MultiClass] \
	    -command "sn_classbrowser [list ${string}]" \
	    -state ${findstate}

	if {${scopetag} != ""} {
	    set xstring "${string}(${scopetag})"
	} else {
	    set xstring ${string}
	}

	if {${findstate} == "normal" && ! [have_xref]} {
	    set xref_state disabled
	} else {
	    set xref_state normal
	}

	# View XRef.
	${m} add command \
	    -accelerator "$sn_options(sys,alt-accelpref)+X" \
	    -label "${vw}[get_indep String MultiXRef] ${lbl}" \
	    -underline [get_indep Pos MultiXRef] \
	    -command "sn_xref both [list ${xstring}] [list ""] [list ${file}]" \
	    -state ${xref_state}
    }

    method bind_right_mouse {t} {
	global sn_options
	if {$sn_options(def,edit-rightmouse-action) == "scroll"} {
	    bind ${t} <3> { }
	    bind ${t} <B3-Motion> { }
	} else {
	    bind ${t} <3> "${this} m3_post_menu %W %X %Y %x %y; break"
	    bind ${t} <B3-Motion> {break}
	}
    }

    # If tabsize is not specified, use default tab size.
    proc set_tabsize {t {tabsize ""} {refr ""}} {
	global sn_options

	if {${tabsize} == ""} {
	    # If $wd is empty or not a valid number, take 8 !
	    if {[catch {set sn_options(def,edit-tabstop) \
	      [expr $sn_options(def,edit-tabstop) + 0]}]} {
		set sn_options(def,edit-tabstop) 8
	    }
	    set tb $sn_options(def,edit-tabstop)
	} else {
	    set tb ${tabsize}
	}

	# Don't use "-tabs" to change the tab size, it does
	# have another behavior as normal tabulators.
	${t} config \
	    -tabsize ${tb}
    }

    # If tabsize is not specified, use default tab size.
    proc set_tab {t {tabsize ""} {refr ""}} {
	global sn_options

	set font [${t} cget -font]
	set text_avg_width [font measure ${font} "M"]

	if {${tabsize} == ""} {
	    # If $wd is empty or not a valid number, take 8 !
	    if {[catch {set sn_options(def,edit-tabstop) \
	      [expr $sn_options(def,edit-tabstop) + 0]}]} {
		set sn_options(def,edit-tabstop) 8
	    }
	    set tb $sn_options(def,edit-tabstop)
	} else {
	    set tb ${tabsize}
	}

	${t} config \
	    -tabs [expr {${text_avg_width} * ${tb}}]
    }

    proc set_wrap {w} {
	global sn_options
	${w} config \
	    -wrap $sn_options(def,edit-wrap)
    }

    # Toggle or set the insert/overwrite flag.
    proc set_overwrite {w {overwrite ""} {toggle "toggle"}} {
	global sn_options
	global tkText

	if {${overwrite} == ""} {
	    set overwrite $sn_options(def,edit-overwrite-mode)
	}
	
	if {${toggle} == "toggle" || ${toggle} == "yes"} {
	    if {${overwrite}} {
		set overwrite 0
	    } else {
		set overwrite 1
	    }
	}
	
	set tkText(${w},ovwrt) ${overwrite}

	if {$tkText(${w},ovwrt)} {
	    set wd 8
	} else {
	    set wd 2
	}

	${w} config \
	    -insertwidth ${wd}
    }

    # The basic insertion routine for a tab.
    proc InsertTab {w {pos ""}} {
	global sn_options

	# Insert Spaces instead of Tabs.
	if {$sn_options(def,edit-tab-inserts-spaces)} {
	    # At least we should insert one character.

	    if {$sn_options(def,edit-tabstop) == 0} {
	        set txt " "
	    } else {
	        set txt [string repeat " " $sn_options(def,edit-tabstop)]

	        set ts $sn_options(def,edit-tabstop)
	        set l [${w} index insert]
	        set col [lindex [split ${l} {.}] 1]
	        while {${col} >= $ts} {
	            incr col -$ts
	        }
	        set spaces [expr {$ts - $col}]
	        set txt [string repeat " " ${spaces}]
	    }

	    if {${pos} == ""} {
		tkTextInsertChar ${w} ${txt}
	    } else {
		tkTextInsert ${w} ${pos} ${txt}
	    }
	    return
	}

	if {${pos} == ""} {
	    tkTextInsertChar ${w} \t
	} else {
	    tkTextInsert ${w} ${pos} \t
	}
	# NOBODY asks us to translate previous blanks to tabs when
	# the user inserts a TAB.
	return
    }

    # this function opens the external editor
    # try to catch the saving / exiting and relaunch parsing
    method open_external_editor {{line ""} {search 1}} {
#        global sn_options
#        set gvim "gvim"
#        set gvimfile $itk_option(-filename)
#        lappend gvim $itk_option(-filename)
#        #append gvim "<" $sn_options(def,null_dev)
#        sn_log "Editor command: ${gvim}"
#        set fd [open "| ${gvim}"]
#        fconfigure ${fd} \
#        -encoding $sn_options(def,system-encoding) \
#        -blocking 0
#
#        #wait until the contents are read,
#        #some times the executed program is so fast, that the
#        #following command failes.
#        catch {fileevent ${fd} readable "wait_editor_end ${fd}"}
#        after 500
#        return ""
	global sn_options
	global tkeWinNumber sn_debug
	set state "normal"
	set external_editor ""
	set w $itk_component(editor)
	set line [${w} index insert]

	# convert first the file name to be edited to a project-related name
	set file $itk_option(-filename)
	set file [sn_convert_FileName ${file}]

	# make sure we stay in project directory (different interpreters)
	catch {cd $sn_options(sys,project-dir)}

	# check if we have a read-only project
	if {${state} == ""} {
	    if {$sn_options(readonly)} {
		set state disabled
	    } else {
		set state normal
	    }
	}

	# verify if the default external editor is not empty
	if {${external_editor} == ""} {
	    set external_editor $sn_options(def,edit-external-editor)
	}

	set external_editor [sn_filecmd format -internal ${external_editor}]

	# deal with whitespace
	if {[regexp " " ${external_editor}]} {
		set external_editor "\"$external_editor\""
	}
	
	# use external editor
	if {${external_editor} != ""} {
	    if {[regexp "(emacs|gnuclient)" ${external_editor}]} {
		if {[string compare ${line} ""] == 0} {
		    set line 1.0
		}
		sn_start_emacs ${file} ${line} ${state} ${external_editor}
	    } else {
		if {${line} == ""} {
		    set line 1
		    set col 0
		} else {
		    set line [trim_text_index ${line}]
		    set pos [split ${line} {.}]
		    set line [lindex ${pos} 0]
		    set col [lindex ${pos} 1]
		}

		regsub -all {%l} ${external_editor} ${line} external_editor
		regsub -all {%c} ${external_editor} ${col} external_editor
		regsub -all {%i} ${external_editor} [tk appname] external_editor
		regsub -all {%d} ${external_editor} $sn_options(sys,project-dir) external_editor

		if {![regsub \
		    -all {%f} ${external_editor} ${file} external_editor]} {
		    lappend external_editor ${file}
		}

		# this is a vim'ism to position the cursor
	        if {[regexp "(vim)" ${external_editor}]} {
                    lappend external_editor "+call cursor($line,$col)"
	        }
		
		# terminate with (usually) "<cmd> < /dev/null"
		lappend external_editor "<" $sn_options(def,null_dev)
		
		#puts "external editor is $external_editor"
		sn_log "Editor command: ${external_editor}"

		#read the contents into editor in the background
		set fd [open "| ${external_editor}"]
		fconfigure ${fd} \
		    -encoding $sn_options(def,system-encoding) \
		    -blocking 0

		#wait until the contents are readed,
		#some times the executed program is so fast, that the
		#following command failes.
		catch {fileevent ${fd} readable "wait_editor_end ${fd}"}
		after 500
		# FIXME : This will block the GUI for 0.5 sec, that is not acceptable
	    }
	    return ""
	} else {
		# raise error that there is no editor defined
		tk_dialog auto [get_indep String ExternalEditor] [get_indep String ExternalEditorNotDefined] question_image 0 [get_indep String ok]
	}
    }

    method Undo {} {
	tkTextUndo $itk_component(editor)
    }

    method Redo {} {
	global tkText
	set tkText($itk_component(editor),prevCmd) "Redo"
	tkTextUndo $itk_component(editor)
    }

    method Cut {} {
	# Only cut when there is a valid selection.
	if {![catch {$itk_component(editor) get sel.first}]} {
	    tkTextCut $itk_component(editor)
	}
    }

    method Copy {} {
	tkTextCopy $itk_component(editor)
    }

    method Paste {} {
	tkTextPaste $itk_component(editor)
    }

    method Delete {} {
	catch {tkTextDelete $itk_component(editor) insert [tkTextPlaceChar $itk_component(editor) +] 1 0}
    }

    method FindNext {{direction ""} {string ""}} {
	global sn_options

	if {![info exists edit_SearchString]} {
	    set edit_SearchString $sn_options(search,str)
	}

	if {${direction} != ""} {
	    set edit_SearchDirection ${direction}
	}
	if {[string compare ${string} ""] == 0} {
	    set string ${edit_SearchString}
	}
	SearchForNext ${string}

	find_and_mark_tag
    }

    # Get a search string from the user and search for it in the specified 
    # text widget.
    method FindText {} {
	global sn_options

	set s ${topw}.multiedit_search
	if {[winfo exists ${s}]} {
	    if {[${s} state] != "normal"} {
		${s} deiconify
	    } else {
		${s} raise
	    }
	    ${s} move_to_mouse

	    # Set text only and return.
	    catch {set edit_SearchString [string trim \
	      [selection get]]}
	    return
	}

	# See if there is a selection to look for.
	catch {
	    set edit_SearchString [string trim [selection get]]
	}

	set s [sourcenav::Window ${s}]
	set strchk ${s}.strchk
	set strs ${strchk}.string
	set chk ${strchk}.chk
	set but ${s}.but

	${s} title [sn_title [get_indep String EditSearchFor]]
	${s} on_close "${this} Delete_Trace_SearchString ${but}.start; itcl::delete object ${s}"

	frame ${strchk}

	frame ${strs}
	label ${strs}.label \
	    -text [get_indep String SearchPattern] \
	    -underline [get_indep Pos SearchPattern] \
	    -anchor w
	entry ${strs}.entry \
	    -width 30 \
	    -bd 3 \
	    -relief sunken \
	    -exportselection no \
	    -textvariable [itcl::scope edit_SearchString]
	${strs}.entry select to end
	bind ${strs}.entry <Return> "${but}.start invoke"
	pack ${strs}.label \
	    -side left \
	    -padx 5
	pack ${strs}.entry \
	    -side left \
	    -padx 5 \
	    -fill x \
	    -expand y

	frame ${but}
	if {${edit_SearchString} == ""} {
	    set state disabled
	} else {
	    set state normal
	}
	button ${but}.start \
	    -text [get_indep String UtilSearch] \
	    -underline [get_indep Pos UtilSearch] \
	    -state ${state} \
	    -command "${this} SearchForNext"
	# Trace the button to enable/disable it, when string is (not) empty.
	Trace_SearchString ${but}.start
	button ${but}.cancel \
	    -text [get_indep String Cancel] \
	    -underline [get_indep Pos Cancel] \
	    -command "
		${this} Delete_Trace_SearchString ${but}.start
		itcl::delete object ${s}"
	pack ${but}.start \
	    -side top \
	    -fill x \
	    -padx 10 \
	    -pady 5
	pack ${but}.cancel \
	    -side top \
	    -fill x \
	    -padx 10 \
	    -pady 5

	frame ${chk}
	checkbutton ${chk}.case \
	    -text [get_indep String IgnoreCase] \
	    -underline [get_indep Pos IgnoreCase] \
	    -variable [itcl::scope edit_SearchNoCase] \
	    -onvalue "-nocase" \
	    -offvalue ""
	checkbutton ${chk}.regexp \
	    -text [get_indep String RegExp] \
	    -underline [get_indep Pos RegExp] \
	    -variable [itcl::scope edit_SearchMethod] \
	    -onvalue "-regexp" \
	    -offvalue "-exact"
	checkbutton ${chk}.forward \
	    -text [get_indep String SQLFfor] \
	    -underline [get_indep Pos SQLFfor] \
	    -variable [itcl::scope edit_SearchDirection] \
	    -onvalue "-forwards" \
	    -offvalue "-backwards"
	pack ${chk}.case \
	    -side top \
	    -anchor nw \
	    -padx 35
	pack ${chk}.regexp \
	    -side top \
	    -anchor nw \
	    -padx 35
	pack ${chk}.forward \
	    -side top \
	    -anchor nw \
	    -padx 35

	# Bind_focus_enter $s "focus $strs.entry".

	pack ${strs} \
	    -side top \
	    -fill x \
	    -pady 5 \
	    -expand y
	pack ${chk} \
	    -side top \
	    -fill x \
	    -pady 5 \
	    -expand y
	pack ${strchk} \
	    -side left \
	    -fill x \
	    -pady 20 \
	    -fill x \
	    -expand y
	pack ${but} \
	    -anchor nw \
	    -side left \
	    -padx 20 \
	    -pady 5
	focus ${strs}.entry

	${s} move_to_mouse
	catch {${s} resizable yes no}

	update idletasks
	set geom [split [lindex [split [${s} geometry] "+"] 0] "x"]
	${s} minsize [lindex ${geom} 0] [lindex ${geom} 1]
    }

    # Search from the current insertion point to EOF for a given string and
    # highlight the first occurrence found; search will wrap-around.
    method SearchForNext {{string ""} {top ""}} {
	global sn_options
	global SearchFoundLength

# FIXME: What is this SearchFoundLength used for ??
	set SearchFoundLength 0

	if {[string compare ${string} ""] != 0} {
	    set edit_SearchString ${string}
	} else {
	    set string ${edit_SearchString}
	}

	# Insert the string into the search history!
	sn_add_to_histroy_stack sn_options(search) ${string}

	sn_log "Editor::SearchForNext resetting combo text to \"$string\""	

	$itk_option(-findcombo) configure \
	    -contents $sn_options(search) \
	    -entrytext ${string}

# FIXME: Why would we check edit_SearchString again?
	# Make sure that we have the correct value.
	if {${edit_SearchString} == ""} {
	    set edit_SearchString ${string}
	}
	if {${string} == ""} {
	    set string ${edit_SearchString}
	}
	if {${string} == ""} {
	    bell
	    return
	}

	set ranges [$itk_component(editor) tag ranges sel]

	# Skip selection, if the selection is equal to the string
	#is looking for
	if {${ranges} != ""} {
	    #if nocase is on, make nocase comparing.
	    if {${edit_SearchNoCase} != ""} {
		set cmp1 [string tolower [eval $itk_component(editor) get ${ranges}]]
		set cmp2 [string tolower ${string}]
	    } else {
		set cmp1 [eval $itk_component(editor) get ${ranges}]
		set cmp2 ${string}
	    }
	    #by regexp, we can't compare the string looking for.
	    if {[string compare ${cmp1} ${cmp2}] == 0 || ${edit_SearchMethod} == \
	      "-regexp"} {
		if {${edit_SearchDirection} == "-forwards"} {
		    set idx [$itk_component(editor) index "[lindex ${ranges} 0] + 1c"]
		} else {
		    set idx [lindex ${ranges} 0]
		}
	    } else {
		set idx [$itk_component(editor) index insert]
	    }
	} else {
	    set idx [$itk_component(editor) index insert]
	}
	# Remove the current selection.
	$itk_component(editor) tag remove sel 0.0 end

	# Search !
	set cmd [list $itk_component(editor) search ${edit_SearchMethod} ${edit_SearchDirection}]
	if {${edit_SearchNoCase} != ""} {
	    lappend cmd ${edit_SearchNoCase}
	}
	lappend cmd \
	    -count SearchFoundLength \
	    -- ${string}
 
        set testcmd $cmd

        lappend cmd ${idx}
	set pos [eval ${cmd}]

        # Now, if this is that last occurrance in the file
        # we have to beeps.
        
        if {$SearchFoundLength == 0 || $pos == ""} {
            # There are no search results.
            bell
            return
        } else {
            lappend testcmd "${pos} + ${SearchFoundLength} chars" end

            # Check for more search results.        
            if {[eval $testcmd] == ""} {
                bell
                sn_log "SearchForNext: Last occurrance has been found.(beep)"
            }
        }

	if {[string compare ${pos} ""] != 0} {
	    $itk_component(editor) tag add sel ${pos} "${pos} + ${SearchFoundLength} chars"
            $itk_component(editor) tag raise sel
	    $itk_component(editor) mark set lastpos insert
	    $itk_component(editor) mark set insert ${pos}
	    $itk_component(editor) see insert

	    set df [expr abs(int([$itk_component(editor) index "@0,[winfo height \
	      $itk_component(editor)]"]) - int(${pos}))]

	    # If the target is closer to the bottom of the page than 3 lines, \
	      center it!
	    if {${df} <= 3} {
		$itk_component(editor) yview scroll [expr int([text_page_num_lines \
		  $itk_component(editor)] / 2)] units
	    }

	    if {[winfo exists ${top}]} {
		${top} transient $itk_component(editor)
	    }
	} else {
	    bell
	}
    }

    ############################################
    # Replace command
    # Get a search&replace string from the user and search for it in the
    # specified text widget. Then replace those occurrences by the
    # replace string.
    ############################################

    # Find all occurrences of the text in the file, and feed them through
    # the specified search/replace algorithm
    method ReplaceAll {w} {
	if {${edit_SearchString} == ""} {
	    bell
	    return
	}

	set pos end
	while {${pos} != ""} {
	    set pos [eval ${w} search ${edit_SearchMethod} ${edit_SearchNoCase} \
	        -backwards -count len \
	        -- [list ${edit_SearchString}] [list ${pos}] 1.0]

	    if {${pos} != ""} {
		tkTextReplace ${w} ${pos} "${pos} + ${len} char" \
		  ${edit_ReplaceString}
	    }
	}

	${w} see insert
    }

    #called when the variable is changed
    method StringModified_cb {btns name1 name2 op} {
#FIXME: This is changing the linked variable (the search text)
	upvar #0 ${name1} v
	if {![info exists v]} {
	    return
	}
	if {${v} != ""} {
	    set state normal
	} else {
	    set state disabled
	}
	foreach btn ${btns} {
	    if {[winfo exists ${btn}]} {
		${btn} config \
		    -state ${state}
	    }
	}
    }

    #add trace for search string to enable/disable related buttons
    method Trace_SearchString {btn} {
# FIXME: A trace is getting added on top of the existing edit_SearchString
# which is likely hosing the result in the search text box
	trace variable [itcl::scope edit_SearchString] w [list ${this} \
	  StringModified_cb ${btn}]
    }

    #remove tracing by closing the search window.
    method Delete_Trace_SearchString {btn} {
	trace vdelete [itcl::scope edit_SearchString] w [list ${this} \
	  StringModified_cb ${btn}]
	catch {unset edit_SearchString}
    }

    method Replace_String {btn} {
	if {${edit_ReplaceString} != ""} {
	    tkTextInsertChar $itk_component(editor) ${edit_ReplaceString}
	} else {
	    tkTextDelete $itk_component(editor) sel.first sel.last
	}
	$itk_component(editor) tag add sel "insert -[string length ${edit_ReplaceString}] \
	  char" insert
        $itk_component(editor) tag raise sel
	${btn} config \
	    -state disabled
    }

    method Replace {} {
	global sn_options
	global SearchFoundLength

	set w $itk_component(editor)

	catch {
	    set edit_SearchString [string trim [selection get]]
	}

	set s ${topw}.replace
	if {[winfo exists ${s}]} {
	    raise ${s}
	    return
	}

	set strchk ${s}.strchk
	set strs ${strchk}.strings
	set str1 ${strs}.str1
	set str2 ${strs}.str2
	set buts ${s}.but
	set chks ${strchk}.chks

	sourcenav::Window ${s}
	${s} configure -title [sn_title [get_indep String FindReplace]]
	${s} on_close "${this} Delete_Trace_SearchString ${buts}.all; itcl::delete object $s"

	set foc [focus]
	if {${foc} == ""} {
	    set foc ${w}
	}
	${s} transient ${foc}

	frame ${strchk}
	pack ${strchk} \
	    -side left \
	    -expand y \
	    -fill x

	frame ${strs}
	pack ${strs} \
	    -side top \
	    -expand y \
	    -fill x

	#find string
	frame ${str1}
	pack ${str1} \
	    -side top \
	    -expand y \
	    -fill x \
	    -pady 5 \
	    -padx 5
	label ${str1}.label \
	    -width 14 \
	    -text [get_indep String SearchPattern] \
	    -underline [get_indep Pos SearchPattern] \
	    -anchor w
	pack ${str1}.label \
	    -side left
	entry ${str1}.entry \
	    -relief sunken \
	    -bd 3 \
	    -exportselection no \
	    -textvariable [itcl::scope edit_SearchString]
	bind ${str1}.entry <Tab> "focus ${str2}.entry"
	bind ${str1}.entry <Return> "${buts}.next invoke"
	${str1}.entry select to end
	pack ${str1}.entry \
	    -side left \
	    -fill x \
	    -expand y

	Trace_SearchString ${buts}.all

	#replace with string
	frame ${str2}
	pack ${str2} \
	    -side top \
	    -expand y \
	    -fill x \
	    -pady 5 \
	    -padx 5
	label ${str2}.label \
	    -width 14 \
	    -text [get_indep String ReplacePattern] \
	    -underline [get_indep Pos ReplacePattern] \
	    -anchor w
	pack ${str2}.label \
	    -side left
	entry ${str2}.entry \
	    -relief sunken \
	    -bd 3 \
	    -exportselection no \
	    -textvariable [itcl::scope edit_ReplaceString]
	pack ${str2}.entry \
	    -side left \
	    -fill x \
	    -expand y
	bind ${str2}.entry <Return> "${buts}.next invoke"
	bind ${str2}.entry <Tab> "focus ${str1}.entry"

	#checkbuttons
	frame ${chks}
	pack ${chks} \
	    -side top \
	    -expand y \
	    -fill x \
	    -pady 10 \
	    -padx 10

	checkbutton ${chks}.case \
	    -text [get_indep String IgnoreCase] \
	    -underline [get_indep Pos IgnoreCase] \
	    -variable [itcl::scope edit_SearchNoCase] \
	    -onvalue "-nocase" \
	    -offvalue ""

	checkbutton ${chks}.regexp \
	    -text [get_indep String RegExp] \
	    -underline [get_indep Pos RegExp] \
	    -variable [itcl::scope edit_SearchMethod] \
	    -onvalue "-regexp" \
	    -offvalue "-exact"

	checkbutton ${chks}.forward \
	    -text [get_indep String SQLFfor] \
	    -underline [get_indep Pos SQLFfor] \
	    -variable [itcl::scope edit_SearchDirection] \
	    -onvalue "-forwards" \
	    -offvalue "-backwards"
	pack ${chks}.case \
	    -anchor nw \
	    -side top \
	    -padx 35
	pack ${chks}.regexp \
	    -anchor nw \
	    -side top \
	    -padx 35
	pack ${chks}.forward \
	    -anchor nw \
	    -side top \
	    -padx 35

	#buttons
	frame ${buts}
	pack ${buts} \
	    -side right \
	    -pady 5 \
	    -padx 5

	button ${buts}.cancel \
	    -text [get_indep String Cancel] \
	    -underline [get_indep Pos Cancel] \
	    -command "
		${this} Delete_Trace_SearchString ${buts}.cancel
		itcl::delete object ${s}
	    "
	button ${buts}.next \
	    -text [get_indep String UtilSearch] \
	    -underline [get_indep Pos UtilSearch] \
	    -command "
		${this} SearchForNext
		if {\$SearchFoundLength > 0} {
						${buts}.replace config -state normal
					} else {
						${buts}.replace config -state disabled
					}
	    "

	# We check whether the pattern can be found at the insertation point.
	# It must be also selected
	set pos [eval ${w} search ${edit_SearchMethod} ${edit_SearchNoCase} \
	    -count SearchFoundLength \
	    -- [list ${edit_SearchString}] insert]
	if {[string compare ${pos} ""] != 0 && [${w} compare insert == \
	  ${pos}]} {
	    if {[${w} tag ranges sel] != ""} {
		set state normal
	    } else {
		set state disabled
	    }
	} else {
	    set state disabled
	}
	button ${buts}.replace \
	    -text [get_indep String PafReplace] \
	    -underline [get_indep Pos PafReplace] \
	    -state ${state} \
	    -command "${this} Replace_String ${buts}.replace"
	if {${edit_SearchString} == ""} {
	    set state disabled
	} else {
	    set state normal
	}
	button ${buts}.all \
	    -text [get_indep String All] \
	    -state ${state} \
	    -command "${this} ReplaceAll ${w}"

	pack ${buts}.next \
	    -fill x \
	    -side top \
	    -padx 10 \
	    -pady 5
	pack ${buts}.cancel \
	    -fill x \
	    -side top \
	    -padx 10 \
	    -pady 5
	pack ${buts}.replace \
	    -fill x \
	    -side top \
	    -padx 10 \
	    -pady 5
	pack ${buts}.all \
	    -fill x \
	    -side top \
	    -padx 10 \
	    -pady 5

	focus ${str1}.entry

	${s} move_to_mouse
	catch {${s} resizable yes no}
	catch {${s} grab set}

	update idletasks
	set geom [split [lindex [split [${s} geometry] "+"] 0] "x"]
	${s} minsize [lindex ${geom} 0] [lindex ${geom} 1]
    }

    #
    # Goto a specified line in the file
    #
    method GotoLine {} {
	global sn_options

	set w $itk_component(editor)
	set pos -2
	if {[catch {set pos [string trim [selection get]]}] == 0} {
	    if {[catch {set pos [expr int(${pos})]}]} {
		set pos -1
	    }
	}

	if {${pos} < 0} {
	    set $itk_option(-linenumber_var) [${w} index insert]
	} else {
	    set $itk_option(-linenumber_var) ${pos}
	}

	set f [sourcenav::Window ${topw}.goto]
	${f} configure -title [sn_title [get_indep String EditGotoLine]]
	${f} configure -iconname [get_indep String EditGotoLine]
	${f} transient ${topw}

	frame ${f}.line
	label ${f}.line.label \
	    -text [get_indep String LineNumber] \
	    -underline [get_indep Pos LineNumber] \
	    -width 13 \
	    -anchor w

	set line 0
	set char 0

	entry ${f}.line.entry \
	    -width 14 \
	    -relief sunken \
	    -bd 3 \
	    -textvariable $itk_option(-linenumber_var) \
	    -exportselection n

	${f}.line.entry select to end

	button ${f}.line.button \
	    -text [get_indep String Goto] \
	    -underline [get_indep Pos Goto] \
	    -command "${this} proceed_gotoline ${f}"

	pack ${f}.line.label \
	    -side left
	pack ${f}.line.entry \
	    -side left
	pack ${f}.line.button \
	    -side left \
	    -pady 10 \
	    -padx 20

	bind ${f}.line.entry <Return> "${f}.line.button invoke"
	focus ${f}.line.entry

	pack ${f}.line \
	    -side top \
	    -fill both

	focus ${f}.line.entry
	${f} move_to_mouse
	catch {${f} resizable no no}

	catch {${f} grab set}
    }
    method proceed_gotoline {f} {
	global tcl_platform

        # FIXME : This is ugly, but the -linenumber_var gets
        # changed because the vwait in sn_wait will process
        # Focus_In which resets the variable. We hack this
        # by saving and resetting the variable here (ugh).
        # This fixes goto line under Win32.
        set linenum [set $itk_option(-linenumber_var)]

	itcl::delete object ${f}
	update idletasks
	if {$tcl_platform(platform) == "windows"} {
	    sn_wait 50
	}
        set $itk_option(-linenumber_var) $linenum
	${this} SetFondPos [set $itk_option(-linenumber_var)] 1 0
    }

    #
    # The basic insertion routine for brackets
    # If requested, does bracket matching
    #
    # w: 	  Editor widget
    # sb: 	  start bracket
    # eb:	  end bracket
    # insert: marks if a bracket is inserted or the region
    #		  has to selected
    proc Insert_Mark_Bracket {args} {
	after idle "Editor&::x_Insert_Mark_Bracket ${args}"
    }
    proc x_Insert_Mark_Bracket {w sb {bracket 1}} {
	global sn_options

	#by insertion, do a bracket selection
	#by double-click on a bracket, do a real selection
	if {${bracket}} {
	    set tag bracket
	} else {
	    set tag sel
	}

	#delay is invalid
	if {$sn_options(def,edit-bracket-delay) <= 0} {
	    return
	}

	set ebees "\"\'\>\]\)\}"
	set sbees "\"\'\<\[\(\{"
	set i [string first ${sb} ${ebees}]
	if {${i} != -1} {
	    set eb [string index ${sbees} ${i}]
	} else {
	    set i [string first ${sb} ${sbees}]
	    if {${i} == -1} {
		return
	    }
	    set eb [string index ${ebees} ${i}]
	}

	set str_range ""

	switch -- ${sb} {
	\" -
	\' {
		#if string tag is availiable, mark the whole reagon
		#of the string
		set tags [${w} tag names insert]
		if {[lsearch \
		    -exact ${tags} str] != -1} {
		    set str_range [${w} tag prevrange str insert+1c]
		    if {${str_range} != ""} {
			set idx [lindex ${str_range} 0]
			set end [lindex ${str_range} end]

			#somebody tries to select a region in text area
			#can he do it?
			if {[${w} compare ${idx} < insert] && [${w} compare \
			  ${end}-1c > insert]} {
			    return
			}
		    }
		}

		#do normal selection (locate a matching \"
		if {${str_range} == ""} {
		    set ob ""
		    set cb ${eb}
		    set beg_off [${w} index "insert-200c"]
		    set cont [${w} get ${beg_off} "insert-1c"]
		    set forw 0
		}
	    }
	\> -
	\] -
	\) -
	\} {
		set ob ${sb}
		set cb ${eb}
		set beg_off 0.0
		set cont [${w} get ${beg_off} insert]
		set forw 0
	    }
	\< -
	\[ -
	\( -
	\{ {
		set ob ${sb}
		set cb ${eb}
		set beg_off "insert-1c"
		set cont [${w} get ${beg_off} end]
		set forw 1
	    }
	default {
		return
	    }
	}

	if {${str_range} == ""} {
	    # brace_balance is written in C. If it does not exist just return.
	    if {[::info commands brace_balance] != ""} {
		set off [brace_balance ${cont} ${forw} ${ob} ${cb}]
	    } else {
		return
	    }
	    if {${off} == -1} {
		return
	    }
	    set idx [${w} index "${beg_off} + ${off} chars"]
	    set end [${w} index insert]
	}

	${w} tag remove ${tag} 0.0 end
	${w} see ${idx}

	if {${str_range} == ""} {
	    if {[${w} compare ${idx} > "insert"]} {
		set end ${idx}
		set idx "insert-1c"
	    } else {
		set end "insert"
	    }
	}

	after idle "${w} tag add ${tag} ${idx} ${end}"
	if {${bracket}} {
	    # If we just insert remove the selection.
	    set cmd "; ${w} tag remove ${tag} 0.0 end"
	} else {
	    set cmd ""
	}
	update idletasks
	after $sn_options(def,edit-bracket-delay) "${w} see insert ${cmd}"
    }

    #
    # The basic insertion routine for adding a new line
    #
    proc Newline {w} {
	global sn_options
	if {$sn_options(def,edit-indentwidth) <= 0} {
	    tkTextInsertChar ${w} \n
	} else {
	    set l [${w} get "insert linestart" "insert lineend"]
	    set indentStr ""
	    if {[regexp \
	            -indices "^\[ \t\]*" ${l} ind_off]} {
	        set end [lindex ${ind_off} 1]
	        set indentStr [string range ${l} 0 ${end}]
	        # Unexpand the spaces to TABS.
	        set spaces [string repeat " " $sn_options(def,edit-tabstop)]

	        regsub \
	                -all ${spaces} ${indentStr} "\t" indentStr
	        regsub \
	                -all "\[ \]+\t" ${indentStr} "\t" indentStr

	        if {$sn_options(def,edit-tab-inserts-spaces)} {
	            # we have to translate \t to the current size number of tabs
	            if {$sn_options(def,edit-tabstop) == 0} {
	                set spaces " "
	            } else {
	                set spaces [string repeat " " \
	                        $sn_options(def,edit-tabstop)]
	            }
	            regsub \
	                -all \t ${indentStr} ${spaces} indentStr
	        }
	    }
	    
	    tkTextInsertChar ${w} \n${indentStr}
	}
    }

    # The function tries to get an identifier from the text widget.
    # 1. from the selection (if that exists)
    # 2. from the cursor position
    proc get_current_word {{cls ""}} {
	global sn_all_scopes
	set w [${cls} editor]
	set name ""

	if {[catch {set name [selection get]}]} {
	    set name ""
	    foreach nm [${w} tag names insert] {
		if {[lsearch \
		    -exact ${sn_all_scopes} ${nm}] != -1} {
		    set rng [${w} tag prevrange ${nm} "insert +1c"]
		    set name [format_for_retriever [eval ${w} get ${rng}]]
		}
	    }
	    if {${name} == ""} {
		set name [format_for_retriever [${w} get "insert wordstart" \
		  "insert wordend"]]
	    }
	}

	return ${name}
    }

    proc Get_XSelection {{cls ""}} {
	if {![catch {set string [selection get]}]} {
	    set off [string first "\n" ${string}]
	    if {${off} != -1} {
		set string [string trim [string range ${string} 0 \
		  [expr ${off} - 1]]]
	    }
	} else {
	    set string ""
	}

	#look for the current symbol in the editor
	if {${string} == "" && ${cls} != ""} {
	    set sel [${cls} Selection]
	    if {${sel} != ""} {
		set string [lindex ${sel} 1]
		if {[lindex ${sel} 2] != ""} {
		    set string "[lindex ${sel} 2] ${string}"
		}
	    }
	}

	#look for the current word in the editor
	if {${string} == "" && ${cls} != ""} {
	    set string [Editor&::get_current_word ${cls}]
	}

	return ${string}
    }

    proc search_definition {cls {string ""} {file ""}} {
	search_implementation ${cls} ${string} ${file} "def"
    }
    proc search_implementation {cls {string ""} {file ""} {imp_def "imp"}} {
	global sn_scopes
	set cls [namespace tail $cls]

        if {$cls != "" && [[$cls cget -parent] isakeep] == 1} {
	    set cmd "${cls} gotofile_cb"
	} else {
	    set cmd ""
	}

	#see if cursor stays on a symbol header
	if {${cls} != ""} {
	    set editor [${cls} editor]
	    set pos [$editor index {insert wordstart}]
	}
	if {${cls} != "" && [points_to_header [${cls} editor] ${pos}] != ""} {
	    set sel [${cls} Selection]
	    set scope [lindex ${sel} 0]
	    set sym [lindex ${sel} 1]
	    set cls [lindex ${sel} 2]
	    set file [lindex ${sel} 3]
	    set type [lindex ${sel} 5]
	    set prm [lindex ${sel} 6]

	    set string [string trim "${cls}\:\:${sym}" "\:\:"]

	    set have_declaration 1
	} else {
	    if {${string} == ""} {
		set string [Get_XSelection ${cls}]
	    }
	    if {${string} == ""} {
		bell
		return
	    }
	    set from ""
	    set type ""
	    set prm ""
	    set to ""

	    set have_declaration 0
	}

	#look using the following table
	#pattern file param type
	#pattern file param ""
	#pattern ""   param type
	#pattern ""   param ""
	#pattern ""   ""    ""
	#don't look for declarations, if we already have declarations
	if {${imp_def} == "imp"} {
	    lappend scopes {fu mi}
	    if {! ${have_declaration}} {
		lappend scopes {fd md fr}
	    }
	} else {
	    lappend scopes {fd md fr}
	    #exclude implementations as like the scopes "fd md fr"
	    set rest ""
	    foreach s ${sn_scopes} {
		if {[lsearch \
		    -exact {fu mi fd md fr} ${s}] == -1} {
		    lappend rest ${s}
		}
	    }
	    if {${rest} != ""} {
		lappend scopes ${rest}
	    }
	}
	foreach scopegrp ${scopes} {
	    lappend looking_list [list ${file} ${scopegrp} ${prm} ${type}]
	    lappend looking_list [list ${file} ${scopegrp} ${prm} ""]
	    lappend looking_list [list "" ${scopegrp} ${prm} ${type}]
	    lappend looking_list [list "" ${scopegrp} ${prm} ""]
	    lappend looking_list [list "" ${scopegrp} "" ""]
	}

	#find first "mi fu", if nothing is found try to find declaration
	#look first in the same file, when nothing is found, try to find
	#in all files
	foreach l ${looking_list} {
	    set file [lindex ${l} 0]
	    set scopes [lindex ${l} 1]
	    set prm [lindex ${l} 2]
	    set type [lindex ${l} 3]

	    #make sure that a fetch hasn't been done twice
	    if {[info exists looking_arr(${file},${scopes},${prm},${type})]} {
		continue
	    }
	    set looking_arr(${file},${scopes},${prm},${type}) 1

	    #1. find implementation by accepting types and parameters
	    set ret [sn_retrieve_symbol ${string} ${scopes} ${file} \
	        -exact 1 0 ${cmd} "" ${type} ${prm}]

	    #if something found, break the selection
	    if {${ret}} {
		break
	    }
	}
	catch {unset looking_list}
	catch {unset looking_arr}
    }

    method RetrieveObject {} {
	global sn_options Parser_Info
	set case -1
	set file $itk_option(-filename)
	if {${file} != $sn_options(noname_file)} {
	    set type [sn_get_file_type ${file}]
	    set case $Parser_Info(${type},CASE)
	}
	if {[catch {set name [selection get]}]} {
	    set name ""
	}
	set name [string trim ${name}]
	if {${name} == ""} {
	    sn_error_dialog [get_indep String NoSelection] [get_indep String \
	      MultiRetriever]
	    return
	}

	if {!${case}} {
	    set name [string toupper ${name}]
	}
	sn_retrieve_symbol [list ${name}] all "" \
	    -exact 1 1
    }

    #this function set the color for the different symbols
    #displayed in the editor
    proc init_tags {t} {
	global sn_options
	global combobox_editor_scopes

	if {$sn_options(iscolor)} {
	    # Create highlight tags for text widget
	    set sc [::lunique [lsort [eval concat [array get \
	      combobox_editor_scopes]]]]
	    foreach tg [concat key rem str ${sc} lv] {
		catch {${t} tag configure ${tg} \
		        -foreground $sn_options(def,color_${tg})}
	    }
	    if {$sn_options(def,edit-xref-highlight)} {
		catch {${t} tag configure "xref_g" \
		        -foreground orange}
		catch {${t} tag configure "xref_u" \
		        -foreground green}
		catch {${t} tag configure "xref_l" \
		        -foreground "cornflower blue"}
	    }

	    ${t} tag configure bracket \
	        -background $sn_options(def,bracketmatch-bg) \
	        -foreground white

	    catch {${t} tag raise mi md}
	} else {
	    ${t} tag configure bracket \
	        -background black \
	        -foreground white
	}
	${t} tag configure grep \
	    -foreground white \
	    -background black
    }

    method color_file {{run_parse 1}} {
	if {${highlight}} {
	    UpdateHighlights ${run_parse}

	    #read symbols from DB
	    set file_syms [get_file_symbols $itk_option(-filename)]

	    #create tags for the found symbols in the editor
	    sn_db_create_symbol_tags $itk_component(editor) ${file_syms} "in iu"

	    #mark actual tag and display found tags into
	    #the combobox
	    GetFileTags 0 ${file_syms}
	}
    }

    # This proc is used to clear the editor then load and hight a file.
    # It can also be called if a file has changed and we need to revert.

    method load_n_colour {{run_parse 0}} {
        global sn_options

	if {[catch {set lastpos [$itk_component(editor) index insert]}]} {
	    set lastpos 1.0
	}
	if {[catch {set markpos [$itk_component(editor) index markpos]}]} {
	    set markpos 1.0
	}

        # Load the file from disk.
	if {[catch {set f [open $itk_option(-filename) r]} msg]} {
             sn_error_dialog "$msg $itk_option(-filename)"
             return
        }

        fconfigure ${f} \
            -encoding $sn_options(def,encoding) \
            -blocking 0
    
        $itk_component(editor) delete 1.0 end
        $itk_component(editor) insert 1.0 [read -nonewline $f]
    
        close ${f}
        $this configure -file_changed 0

 	# Revert undo list.
	tkTextUndoSetup $itk_component(editor)
   
        # Save modification time.
        set file_mtime [file mtime $itk_option(-filename)]
    
        # Color tags.
        color_file $run_parse
    
	# Mark last position.
	$itk_component(editor) mark set lastpos ${lastpos}
	$itk_component(editor) mark set markpos ${markpos}
	$itk_component(editor) mark set insert 0.0
	$itk_component(editor) tag remove sel 0.0 end
    }

    # This file is called, when ever a file must be edited.
    method editfile {f {line ""} {revert ""} {run_parse 1}} {
	global sn_options
	global SyncEditors_Disabled

	# Check we haven't been called before creation.
        if {![info exists itk_component(editor)] \
             || ![winfo exists $itk_component(editor)]} {
	     return
        }

 	if {${f} == ""} {
	    return 0
	}

	# Verify if the momentan file is modified.
	if {${revert} != "revert" && [Ask_For_Modified revert] == 0} {
	    return 0
	}

	# We should never be asked to edit a directory.
	if {[file isdirectory ${f}]} {
	    error "editfile method called for a directory \"${f}\""
	}

	# Test if file exists and readable, returns if
	# file doesn't exist.
	if {${f} != $sn_options(noname_file) &&(! [file exists ${f}] || ! \
	  [file readable ${f}])} {
	    return 0
	}

	# Parse file to see if we could add it into the project.
	if {[file exists ${f}]} {
	    # Make sure that we want to refresh a file before
	    # editing it. This could be a slow mechanism, when somebody
	    # wants to work only with "Fast Save" (without refreshing).
	    if {$sn_options(def,auto-reparse)} {
		if {! [sn_parse_uptodate [list ${f}] 0]} {
		    return 0
		}
	    }
	}

	if {$itk_option(-filename) != ${f}} {
	    set itk_option(-filename) ${f}
	}

	# Change to the project directory.
	catch {cd $sn_options(sys,project-dir)}

	if {[catch {set lastpos [$itk_component(editor) index insert]}]} {
	    set lastpos 1.0
	}
	if {[catch {set markpos [$itk_component(editor) index markpos]}]} {
	    set markpos 1.0
	}

	# Disable synchronize mode.
	incr SyncEditors_Disabled

	# Change view-mode to normal.
	set old_mode [$itk_component(editor) cget -state]
	if {${old_mode} != "normal"} {
	    $itk_component(editor) config \
	        -state normal
	}

	# Open an existing file.
	if {[file exists $itk_option(-filename)]} {
	    # Verify if there is a buffer with the same filename.
	    set other [find_Editor_with_File $itk_option(-filename) ${this}]
	    if {${revert} != "revert" && ${other} != ""} {
		# Call the friend function to copy the contents
		# from the other editor into the actual editor.
		friend_copy ${this} ${other}
	    } else {
		# Load the file from disk.
		if {[catch {set f [open $itk_option(-filename) r]} msg]} {
		    sn_error_dialog "${msg} $itk_option(-filename)"
		    return
		}
		fconfigure ${f} \
		    -encoding $sn_options(def,encoding) \
		    -blocking 0

		$itk_component(editor) delete 1.0 end
		$itk_component(editor) insert 1.0 \
                    [read -nonewline ${f}]

		close ${f}
		$this configure -file_changed 0

		# Save modification time.
		set file_mtime [file mtime $itk_option(-filename)]
	    }

	    # Color tags.
	    color_file ${run_parse}
	} else {
	    # Edit new file.
	    $itk_component(editor) delete 1.0 end

	    # When nothing is typed in, the file isn't modified!!
	    $this configure -file_changed 0
	    # No modification time.
	    set file_mtime 0

	    # Delete entries from combobox.
	    if {$itk_option(-symbols) != ""} {
                $itk_option(-symbols) configure -contents "" -entrytext ""
	    }
	}

	# Revert undo list.
	tkTextUndoSetup $itk_component(editor)

	# Mark last position.
	$itk_component(editor) mark set lastpos ${lastpos}
	$itk_component(editor) mark set markpos ${markpos}
	$itk_component(editor) mark set insert 0.0
	$itk_component(editor) tag remove sel 0.0 end

	# Goto specified position.
	if {${line} != "" && ${line} >= 0} {
	    SetFondPos ${line} 1 0
	}

	# Restore old view-mode.
	if {${old_mode} != "normal"} {
	    $itk_component(editor) config \
	        -state ${old_mode}
	}

	# Reenable synchronize mode.
	incr SyncEditors_Disabled -1

	# Configure reusable button to the modified flag.
	DispModified

	# Set filename on the title.
	SetTitle

	focus $itk_component(editor)

	# Successfull
	return 1
    }

    method get_file_symbols {file} {
	global sn_options sn_sep
	if {[::info commands paf_db_fil] == ""} {
	    return ""
	}
	return [paf_db_fil seq \
	    -col {2 3 4 1 5 6 7} "${file}${sn_sep}"]
    }

    # Do the housekeeping so we can open a file, and call tkeEdit to actually
    # do the editing
    proc EditFile {symbol file {line ""} {search 1} {state ""}} {
	global sn_options
	global tkeWinNumber sn_debug

	if {${file} == ""} {
	    return ""
	}

	#convert first the file name to be edited to a
	#project-related name
	set file [sn_convert_FileName ${file}]

	#make sure we stay in project directory (different interpeters).
	catch {cd $sn_options(sys,project-dir)}

	#verify if we have a read-only project
	if {${state} == ""} {
	    if {$sn_options(readonly)} {
		set state disabled
	    } else {
		set state normal
	    }
	}

	set external_editor [sn_external_editor ${file}]

	#verify if the default external editor is not empty
	if {${external_editor} == ""} {
	    set external_editor $sn_options(def,edit-external-editor)
	}

	set external_editor [sn_filecmd format -internal ${external_editor}]
	set external_always $sn_options(def,edit-external-always)

	# use external editor only of pref says so
	if {${external_always} != 0} {
	    if {[regexp "(emacs|gnuclient)" ${external_editor}]} {
		if {[string compare ${line} ""] == 0} {
		    set line 1.0
		}
		sn_start_emacs ${file} ${line} ${state} ${external_editor}
	    } else {
		if {${line} == ""} {
		    set line 1
		    set col 0
		} else {
		    set line [trim_text_index ${line}]
		    set pos [split ${line} {.}]
		    set line [lindex ${pos} 0]
		    set col [lindex ${pos} 1]
		}

		regsub \
		    -all {%l} ${external_editor} ${line} external_editor
		regsub \
		    -all {%c} ${external_editor} ${col} external_editor
		regsub \
		    -all {%i} ${external_editor} [tk appname] external_editor
		regsub \
		    -all {%d} ${external_editor} $sn_options(sys,project-dir) \
		  external_editor

		if {![regsub \
		    -all {%f} ${external_editor} ${file} external_editor]} {
		    lappend external_editor ${file}
		}

		lappend external_editor "<" $sn_options(def,null_dev)

		sn_log "Editor command: ${external_editor}"

		#read the contents into editor in the background
		set fd [open "| ${external_editor}"]
		fconfigure ${fd} \
		    -encoding $sn_options(def,system-encoding) \
		    -blocking 0

		#wait until the contents are readed,
		#some times the executed program is so fast, that the
		#following command failes.
		catch {fileevent ${fd} readable "wait_editor_end ${fd}"}
# FIXME : This will block the GUI for 0.5 sec, that is not acceptable
		after 500
	    }
	    return ""
	}

	#verify of there is an existing window with the
	#same file name
	set name [find_window_with_file ${file} ${state}]

	#if window is availiable, then raise it
	if {[winfo exists ${name}]} {
	    ${name} view edit
	    set center 0
	    if {${line} < 1.0} {
		set line ""
	    }
	} else {
	    # The file is not being edited, check whether is has been changed
	    # outside of the project !
	    # when the file is to update, the update window will be displayed
	    if {${search} && [::info commands paf_db_f] != ""} {
		#when auto-parsing is disabled, file has to be added, whe
		#it's not in the project.
		if {! $sn_options(def,auto-reparse)} {
		    #set f [paf_db_f get -key $file]
		    set f [paf_db_f seq \
		        -col 0 ${file}]
		}
		#reparse file if it's modified outside or the file is new.
		if {$sn_options(def,auto-reparse) || ${f} == ""} {
		    if {![sn_parse_uptodate [list ${file}]]} {
			return 0
		    }
		}
	    }

	    set center 1

	    # Now, we check whether there is a reusable window.
	    set win [MultiWindow&::find_Reusable]
	    if {${win} != ""} {

		#disable gotosymbol
		global gotosymbol_active
		set gotosymbol_active 0

		#raise editor in the multi window
		${win} view edit

		#enable gotosymbol
		set gotosymbol_active 1

		#get editor class & widget
		set ed [${win} editor]
		set edw [${win} editw]
		if {${ed} == ""} {
		    return ""
		    #Something wrong ??
		}

		#before edit the file, add the current position
		#to the prev/next stack
		${win} history_stack_add_point ${ed}

		#edit file
		${ed} editfile ${file} \
		    -1 "" 0

		#set edit position
		if {${line} == ""} {
		    ${edw} mark set insert 0.0
		    ${edw} mark set anchor insert
		}
		${ed} SetFondPos ${line} ${center} 0

		#return the edit widget
		return ${edw}
	    }

	    #create a new multiple window
	    incr tkeWinNumber
	    set name .multiwindow-${tkeWinNumber}

	    MultiWindow& ${name} \
	        -symbolname ${file} \
	        -raise edit
	    incr tkeWinNumber
	}

	set editor [${name} component editor]
	# FIXME: Should be using components.
	set editw [$editor editor]

	if {${line} != ""} {

	    #before edit the file, add the current position
	    #to the prev/next stack
	    ${name} history_stack_add_point $editor

	    if {[$editor cget -file_changed] && ${symbol} != "" && \
	      [string last " " ${symbol}] != -1} {
		set off [lindex ${symbol} 0]
		if {${off} == "" || ${off} == "dummy"} {
		    set off 0
		}
		set tg [join [lrange ${symbol} 1 end]]
		set pos [${editw} tag ranges ${tg}]
		set len [llength ${pos}]
		set off [expr ${off} * 2]

		if {${len} <= ${off}} {
		    set off [expr ${len} - 2]
		}
		$editor SetFondPos [lindex ${pos} ${off}] ${center} 0
	    } else {
		$editor SetFondPos ${line} ${center} 0
	    }
	} else {
	    $editor SetFondPos ${line} ${center} 0
	}

	#maybe readonly state
	if {${state} != ""} {
	    ${editw} config \
	        -state ${state}
	}

	return ${editw}
    }

    method new_file {} {
	global sn_options
	global prj_lines_num

	if {[Ask_For_Modified new] == 0} {
	    return 0
	}
	editfile $sn_options(noname_file) \
	    -1 revert
    }

    common last_accessed_dir ""

    proc FileDialog {w args} {
	global sn_options
	global tk_strictMotif
	global tcl_platform
	global Avail_Parsers Parser_Info

        # When called with ::.foo we need to just use .foo as the widget name
        set w [namespace tail ${w}]

	#default is project directory and not current directory, because
	#of symbolic link problems!
	if {${last_accessed_dir} == ""} {
	    if {$sn_options(sys,project-dir) != ""} {
		set last_accessed_dir $sn_options(sys,project-dir)
	    } else {
		set last_accessed_dir [pwd]
	    }
	}

	#analyze the arguments
	set cnt [llength ${args}]
	set title ""
	set script ""
	set prefix "file_dialog"
	set save_open "open"
	set initialdir ""
	set extensions ""
	set initialfile ""
	if {$tcl_platform(platform) == "windows"} {
	    #set defaultextension ".cpp"
	    set defaultextension ""
	} else {
	    #set defaultextension ".c"
	    set defaultextension ""
	}
	for {set i 0} {${i} < ${cnt}} {incr i} {
	    set arg [lindex ${args} ${i}]
	    incr i
	    set val [lindex ${args} ${i}]
	    switch -- ${arg} {
	    "-title" {
		    set title ${val}
		}
	    "-script" {
		    set script ${val}
		}
	    "-prefix" {
		    set prefix ${val}
		}
	    "-save_open" {
		    set save_open ${val}
		}
	    "-initialdir" -
	    "-dir" {
		    set initialdir ${val}
		}
	    "-extensions" {
		    set extensions ${val}
		}
	    "-defaultextension" {
		    set defaultextension ${val}
		}
	    "-initialfile" {
		    set initialdir [file dirname ${val}]
		    set initialfile [file tail ${val}]
		}
	    default {
		    bell
		    return
		}
	    }
	}
	if {${title} == ""} {
	    if {${save_open} == "open"} {
		set title [get_indep String Open]
	    } else {
		set title [get_indep String Save]
	    }
	}
	if {${initialdir} == "" || ${initialdir} == "."} {
	    set initialdir ${last_accessed_dir}
	}

	#don't call standard motif dialog box, alwayes Tk
	#on windows, we call the default dialog box
	if {$tcl_platform(platform) != "windows"} {
	    set old_motif ${tk_strictMotif}
	}

	#constract file prefixes from user defined
	if {${extensions} != ""} {
	    set types ${extensions}
	} else {
	    #use default extensions
	    lappend types [list [get_indep String AllFiles] "*"]
	    foreach type ${Avail_Parsers} {
		set newsuf ""
		foreach s $Parser_Info(${type},SUF) {
		    lappend newsuf ${s}
		}
		if {${newsuf} != ""} {
		    lappend types [list $Parser_Info(${type},TYPE) ${newsuf}]
		}
	    }
	}
	if {${save_open} == "open"} {
	    if {$tcl_platform(platform) == "unix"} {
		#don't call motif dialog box

		# FIXME: Hack to support both tk 8.1 and tk 8.3
		if {! [catch {package require Tcl 8.3}]} {
		    set tkFDialog tk::dialog::file::tkFDialog
		} else {
		    set tkFDialog tkFDialog
		}

		set f [$tkFDialog open \
		    -parent ${w} \
		    -title ${title} \
		    -initialdir ${initialdir} \
		    -defaultextension ${defaultextension} \
		    -filetypes ${types} \
		    -initialfile ${initialfile}]
	    } else {
		set f [tk_getOpenFile \
		    -parent ${w} \
		    -title ${title} \
		    -initialdir ${initialdir} \
		    -defaultextension ${defaultextension} \
		    -filetypes ${types} \
		    -initialfile ${initialfile}]
	    }
	} else {
	    if {$tcl_platform(platform) == "unix"} {
		#don't call motif dialog box

		# FIXME: Hack to support both tk 8.1 and tk 8.3
		if {! [catch {package require Tcl 8.3}]} {
		    set tkFDialog tk::dialog::file::tkFDialog
		} else {
		    set tkFDialog tkFDialog
		}

		set f [$tkFDialog save \
		    -parent ${w} \
		    -title ${title} \
		    -initialdir ${initialdir} \
		    -defaultextension ${defaultextension} \
		    -filetypes ${types} \
		    -initialfile ${initialfile}]
	    } else {
		set f [tk_getSaveFile \
		    -parent ${w} \
		    -title ${title} \
		    -initialdir ${initialdir} \
		    -defaultextension ${defaultextension} \
		    -filetypes ${types} \
		    -initialfile ${initialfile}]
	    }
	}

	#store last accessed directory
	if {${f} != ""} {
	    set ff ${f}
	    catch {set last_accessed_dir [file dirname ${ff}]}
	}

	if {${f} != "" && ${script} != ""} {
	    ::eval ${script} [list ${f}]
	}
	#always return used file name
	return ${f}
    }

    #choose a directory
    proc DirDialog {w args} {
	global sn_options
	global tcl_platform

	#default is project directory and not current directory, because
	#of symbolic link problems!
	if {${last_accessed_dir} == ""} {
	    if {$sn_options(sys,project-dir) != ""} {
		set last_accessed_dir $sn_options(sys,project-dir)
	    } else {
		set last_accessed_dir [pwd]
	    }
	}

	#analyze the arguments
	set cnt [llength ${args}]
	set title ""
	set script ""
	set initialdir ""
	set prefix "file_dialog"
	for {set i 0} {${i} < ${cnt}} {incr i} {
	    set arg [lindex ${args} ${i}]
	    incr i
	    set val [lindex ${args} ${i}]
	    switch -- ${arg} {
	    "-title" {
		    set title ${val}
		}
	    "-script" {
		    set script ${val}
		}
	    "-prefix" {
		    set prefix ${val}
		}
	    "-initialdir" -
	    "-dir" {
		    set initialdir ${val}
		}
	    default {
		    bell
		    return
		}
	    }
	}

	if {${title} == ""} {
	    set title [get_indep String SelectDirectory]
	}

	if {${initialdir} == "" || ${initialdir} == "."} {
	    set initialdir ${last_accessed_dir}
	}

	if {$tcl_platform(platform) == "windows"} {
	    # ide_get_directory doesn't like a leading :: in the
            # window name, so we remove it if any.
            set w [namespace tail $w]
	    set dir [ide_get_directory \
	        -initialdir ${initialdir} \
	        -parent ${w} \
	        -title ${title}]
	    #store last accessed directory
	    if {${dir} != ""} {
		set last_accessed_dir ${dir}
	    }

	    if {${dir} != "" && ${script} != ""} {
		::eval ${script} [list ${dir}]
		return ""
	    } else {
		return ${dir}
	    }
	} else {
# FIXME: What is this -script option doing here?
            set obj [sourcenav::DirDialog ${w}.dirdialog \
                -title ${title} \
                -script ${script} \
                -initialdir ${initialdir}]

            set dir [${obj} activate]
            itcl::delete object ${obj}
            if {${dir} != ""} {
                set last_accessed_dir ${dir}
            }
            if {${dir} != "" && ${script} != ""} {
                eval ${script} [list ${dir}]
                return ""
            } else {
                return ${dir}
            }
	}
    }

    method open_file_cb {file} {
	global sn_options
	global prj_lines_num

	#make sure we stay in project directory (different interpeters).
	catch {cd $sn_options(sys,project-dir)}

	#it can happen, that the open-dialog has changed the
	#current working directory.
	catch {cd $sn_options(sys,project-dir)}

	if {![file exists ${file}]} {
	    sn_error_dialog "[get_indep String FileNotExist]: ${file}" \
	      [get_indep String FileNotExist]
	} else {
	    catch {set file [sn_convert_FileName ${file}]}
	    editfile ${file} \
	        -1 revert
	    #remove selection, somehow the text widget have strange behavior
	    #after calling the file dialog
	    $itk_component(editor) tag remove sel 0.0 end
	}
	focus $itk_component(editor)
    }

    method open_file {} {
	global sn_options
	if {[Ask_For_Modified open] == 0} {
	    return 0
	}
	set ret [FileDialog ${this} \
	    -title [get_indep String Open] \
	    -script [list ${this} open_file_cb] \
	    -prefix open_file]
	return 1
    }

    method saveas_file_cb {fast file} {
	set file [sn_convert_FileName ${file}]
	set xref [sn_processes_running]

	#Save file as a file, that not exist in the project
	#when xref is running (fastsave only)
	if {${xref} && ${fast} == 0} {
	    #focus $editor
	    set answer [tk_dialog auto [get_indep String SaveAsFastSaveTitle] \
	      "[get_indep String XRefIsRunning]\n[get_indep String \
	      SaveAsFastSave]\n${file} ?" question_image 0 [get_indep String \
	      FastSave] [get_indep String Cancel]]
	    if {${answer} != 0} {
		return 0
	    }
	    set fast 1
	}

	return [save_file ${fast} ${file}]
    }

    # Save As ...
    method saveas_file {{fast 0} {file ""}} {
	global sn_options

	#noname has to be replaced with blank
	if {${file} == $sn_options(noname_file)} {
	    set file ""
	}
	if {${file} != ""} {
	    set initialdir [file dirname ${file}]
	    set file [file tail ${file}]
	} else {
	    set initialdir ""
	}
	set ret [FileDialog ${this} \
	    -title [get_indep String EditSaveFileAs] \
	    -script [list ${this} saveas_file_cb ${fast}] \
	    -prefix saveas_file \
	    -save_open save \
	    -initialdir ${initialdir} \
	    -initialfile ${file}]
	if {${ret} == ""} {
	    return 0
	} else {
	    return 1
	}
    }

    # Quick Save
    method fastsave_file {} {
	return [save_file 1]
    }

    method revert_file {{mode ""} {ignore_obj ""}} {
	global SyncEditors_Disabled

	if {${mode} != "revert"} {
	    if {[sn_processes_running]} {
		bell
		return
	    }
	    set answer [Ask_For_Revert revert]
	} else {
	    set answer 0
	}

	#reopen file
	#
	#Actualy we must revert all the editors with the
	#same file name
	if {${answer} != 0} {
	    return
	}
	incr SyncEditors_Disabled
	foreach cls [itcl_info objects "*" \
	    -class Editor&] {
	    if {[lsearch \
	        -exact ${ignore_obj} ${cls}] != -1} {
		continue
	    }
	    if {[${cls} cget -filename] == $itk_option(-filename)} {
		set ed [${cls} editor]
		${cls} load_n_colour
		catch {${ed} see lastpos}
		${ed} mark set insert lastpos
		${cls} find_and_mark_tag
	    }
	}
	incr SyncEditors_Disabled -1
    }

    # Save the contents of a specified text widget into a specified file
    # The previous contents of the file is saved to a ".bak" file before being 
    # overwritten
    method save_file {{fast 0} {file ""}} {
	global sn_options

	#make sure we stay in project directory (different interpeters).
	catch {cd $sn_options(sys,project-dir)}

	if {${file} != ""} {
	    set ff ${file}
	} else {
	    set ff $itk_option(-filename)
	}

	#no filename is specified
	if {${ff} == $sn_options(noname_file)} {
	    return [saveas_file ${fast}]
	}

	#file isn't modified
	if {${ff} == $itk_option(-filename) && !$itk_option(-file_changed)} {
	    return 1
	}

	#xref is running, only fastsave is possible
	#if edit mode disabled, no save possible
	if {!${fast} && [sn_processes_running] || $sn_options(readonly)} {
	    bell
	    return 0
	}

	#test if the file has been modified external
	if {[file exists ${ff}] && ${file_mtime} != 0 && ${file_mtime} != \
	  [file mtime ${ff}]} {
	    set answer [tk_dialog auto [get_indep String \
	      FileModifiedOutsideTitle] "[format [get_indep String \
	      FileModifiedOutside] ${ff}]" question_image 0 [get_indep String \
	      Overwrite] [get_indep String Cancel]]
	    if {${answer} != 0} {
		return 0
	    }
	}

	if {[sn_save_file $itk_component(editor) ${ff}] == 0} {
	    return 0
	}

	set file_mtime [file mtime ${ff}]
	#file isn't modified now.
	$this configure -file_changed 0

	#refresh filename (needed by save-as)
	if {${ff} != $itk_option(-filename)} {
	    set filename ${ff}
	}

	#recolor file, after storing the new file name
	if {!${fast}} {
	    color_file
	}

	#That's meen that it is possible that the file is
	#already availiable in other buffer and now has
	#been overridden, revert those files
	set allobj [itcl_info objects "*" \
	    -class Editor&]
	foreach other ${allobj} {
	    if {${other} == ${this} || [${other} cget -filename] != ${ff}} {
		continue
	    }
	    set i [lsearch \
	        -exact ${allobj} ${other}]
	    set ign_obj [lreplace ${allobj} ${i} ${i}]
	    if {!${fast}} {
		${other} revert_file "revert" ${ign_obj}
	    } else {
		${other} setmodified 0
	    }
	    #update modification date for all other edit views
	    #with the dame file name
	    ${other} config \
	        -file_mtime ${file_mtime}
	    ${other} DispModified
	    ${other} SetTitle
	}

	# Important !
	set tkText($itk_component(editor),prevCmd) {}

	#actualize the reuse-checkbutton and the title
	#to reset the changed flag
	DispModified
	SetTitle

	return 1
    }

    proc SaveAll {} {
	catch {unset files}
	#first fast save all files
	foreach ed [itcl_info objects "*" \
	    -class Editor&] {
	    if {[${ed} cget -file_changed]} {
		${ed} fastsave_file
		set files([${ed} cget -filename]) 1
	    }
	}
	catch {unset files}
    }

    method insert_file_cb {f} {
	global sn_options

	if {![file exists ${f}]} {
	    sn_error_dialog "[get_indep String FileNotExist]: ${f}" \
	      [get_indep String FileNotExist]
	    return 0
	}

	#open file
	if {[catch {set f [open ${f} r]} msg]} {
	    sn_error_dialog "${msg} ${f}"
	    return 0
	} else {
	    fconfigure ${f} \
	        -encoding $sn_options(def,encoding) \
	        -blocking 0
	}

	#insert text into editor
	if {![catch {$itk_component(editor) get sel.first sel.last}]} {
	    set fpos [$itk_component(editor) index sel.first]
	    tkTextReplace $itk_component(editor) [$itk_component(editor) index sel.first] \
	      [$itk_component(editor) index sel.last] [read \
	        -nonewline ${f}]
	} else {
	    set fpos [$itk_component(editor) index insert]
	    tkTextInsert $itk_component(editor) insert [read \
	        -nonewline ${f}]
	}
	close ${f}

	#sometimes there is a wrong selected block, delete it
	set epos [$itk_component(editor) index insert]
	$itk_component(editor) tag del sel
	#mark new inserted text
	$itk_component(editor) tag add sel ${fpos} ${epos}
        $itk_component(editor) tag raise sel

	#see last inserted character
	$itk_component(editor) see insert

	return 1
    }

    method insert_file {} {
	set ret [FileDialog ${this} \
	    -title [get_indep String EditInsertFile] \
	    -script [list ${this} insert_file_cb] \
	    -prefix insert_file]
	if {${ret} == ""} {
	    return 0
	} else {
	    return 1
	}
    }

    #find a window with specified filename
    proc find_window_with_file {file {state "normal"}} {
	foreach win [itcl_info objects "*" \
	    -class MultiWindow&] {
	    set ed [${win} editor]
	    set ew [${win} editw]
	    if {${ed} == "" || ${ew} == ""} {
		continue
	    }
	    if {[${ed} cget -filename] == ${file} && [${ew} cget -state] == \
	      ${state}} {
		return ${win}
	    }
	}
	return ""
    }

    method SetFondPos {pos {center 0} {find_mark 1} {select 1}} {
	global sn_options
	global sn_all_scopes

	if {${pos} == ""} {
	    set pos [$itk_component(editor) index insert]
	    set select 0
	}

	if {$sn_options(def,edit-mark-current-position) == 0} {
	    set mrk 0
	} else {
	    set mrk 1
	}

	set w $itk_component(editor)

	${w} mark set lastpos insert

	set poi_sn [string first "." ${pos}]

	set pos [trim_text_index \
	    -digit ${pos}]

	if {${pos} == ""} {
	    bell \
	        -displayof ${w}
	    return
	}

	#cut integer value to be max. 8 digits
	set line [lindex [split ${pos} .] 0]
	if {[string length ${line}] > 8} {
	    set pos "[string range ${line} 0 8].[lindex [split ${pos} .] 1]"
	}

	${w} mark set insert ${pos}
	${w} mark set anchor insert
	${w} tag remove sel 0.0 end
	if {${select}} {
	    if {${poi_sn} == -1} {
		#mark or goto first character
		if {${mrk}} {
		    ${w} tag add sel "insert linestart" "insert lineend + 1c"
                    ${w} tag raise sel
		} else {
		    #goto line, move the insert cursor to first printable
		    #character in the line
		    set txt [${w} get "insert linestart" "insert lineend"]
		    set txt1 [string trim ${txt}]
		    if {${txt} != ${txt1}} {
			set i [string first [string range ${txt1} 0 0] ${txt}]
			if {${i} > 0} {
			    ${w} mark set insert "${pos} +${i}c"
			    ${w} mark set anchor insert
			}
		    }
		}
	    } else {
		set fnd 0
		foreach nm [${w} tag names insert] {
		    if {[lsearch \
		        -exact ${sn_all_scopes} ${nm}] != -1} {
			set fnd 1
			set rng [${w} tag prevrange ${nm} "insert+1c"]
			if {${mrk}} {
			    eval ${w} tag add sel ${rng}
                            ${w} tag raise sel
			}
			break
		    }
		}

		if {${mrk} && !${fnd}} {
		    ${w} tag add sel "insert wordstart" "insert wordend"
                    ${w} tag raise sel
		}
	    }
	}

	if {${center}} {
	    set pos [expr int(${line} - [text_page_num_lines ${w}] / 2) - 1]
	    if {${pos} < 0} {
		set pos 0
	    }
	    ${w} yview ${pos}
	} else {
	    ${w} see ${pos}
	}

	if {${pos} > 1} {
	    find_and_mark_tag
	}

	focus $itk_component(editor)
    }

    method addselection {pos ranges} {
	eval $itk_component(editor) tag add sel ${pos} ${ranges}
	$itk_component(editor) mark set lastpos insert
	$itk_component(editor) mark set insert ${pos}
	$itk_component(editor) see insert
    }

    method DispModified {} {
	if {[winfo exists $itk_option(-mesg_area).reuse]} {
	    if {$itk_option(-file_changed)} {
		$itk_option(-mesg_area).reuse config \
		    -state disabled
	    } else {
		$itk_option(-mesg_area).reuse config \
		    -state normal
	    }
	}
    }

    #Find an editor with a specified file name
    proc find_Editor_with_File {file {except ""}} {
	foreach cls [itcl_info objects "*" \
	    -class Editor&] {
	    if {${cls} != ${except} && ${file} == [${cls} cget -filename]} {
		return ${cls}
	    }
	}
	return ""
    }

    method Ask_For_Revert {reason} {
	global sn_options

	#we can revert the buffer, when there is another
	#editor with the same file name
	if {${reason} != "revert" && $itk_option(-file_changed) && $itk_option(-filename) != \
	  $sn_options(noname_file) && [find_Editor_with_File $itk_option(-filename) \
	  ${this}] != ""} {
	    return 0
	}
	if {$itk_option(-file_changed)} {

	    if {$itk_option(-filename) == $sn_options(noname_file)} {
		set file "Buffer"
	    } else {
		set file $itk_option(-filename)
	    }

	    switch ${reason} {
	    "new" {
		    set txt [get_indep String HasBeenModified]
		    set title [get_indep String EditNewFile]
		    set ok [get_indep String NewFile]
		}
	    "open" {
		    set txt [get_indep String HasBeenModified]
		    set title [get_indep String OpenFile]
		    set ok [get_indep String Open]
		}
	    "revert" {
		    set txt [get_indep String HasBeenModified]
		    set title [get_indep String RevertFile]
		    set ok [get_indep String Revert]
		}
	    "another" {
		    set txt [get_indep String OpenNewBuffer]
		    set title [get_indep String RevertFile]
		    set ok [get_indep String Revert]
		}
	    default {
		    set txt [get_indep String HasBeenModified]
		    set title [get_indep String RevertFile]
		    set ok [get_indep String Revert]
		}
	    }

	    focus $itk_component(editor)
	    set answer [tk_dialog auto ${title} "${file} ${txt}" \
	      question_image 0 ${ok} [get_indep String Cancel]]
	} else {
	    #file not modified
	    set answer 0
	}
	return ${answer}
    }

    method Ask_For_Modified {reason} {
	global sn_options

	#we can revert the buffer, when there is another
	#editor with the same file name
	if {${reason} != "revert" && $itk_option(-file_changed) && $itk_option(-filename) != \
	  $sn_options(noname_file) && [find_Editor_with_File $itk_option(-filename) \
	  ${this}] != ""} {
	    return 1
	}
	if {$itk_option(-file_changed)} {

	    if {$itk_option(-filename) == $sn_options(noname_file)} {
		set file "Buffer"
	    } else {
		set file $itk_option(-filename)
	    }

	    switch ${reason} {
	    "new" {
		    set txt [get_indep String HasBeenModified]
		    set title [get_indep String EditNewFile]
		    set ok [get_indep String NewFile]
		}
	    "open" {
		    set txt [get_indep String HasBeenModified]
		    set title [get_indep String OpenFile]
		    set ok [get_indep String Open]
		}
	    "revert" {
		    set txt [get_indep String HasBeenModified]
		    set title [get_indep String RevertFile]
		    set ok [get_indep String Revert]
		}
	    "another" {
		    set txt [get_indep String OpenNewBuffer]
		    set title [get_indep String RevertFile]
		    set ok [get_indep String Revert]
		}
	    default {
		    set txt [get_indep String HasBeenModified]
		    set title [get_indep String RevertFile]
		    set ok [get_indep String Revert]
		}
	    }

	    #Is Auto-Save enabled
	    if {$sn_options(def,auto-save)} {
		set answer 1
	    } else {
		focus $itk_component(editor)
		set answer [tk_dialog auto ${title} "${file} ${txt}" \
		  question_image 0 [get_indep String Save] [get_indep String \
		  EditFastSave] [get_indep String DonotSave] \
		  [get_indep String Cancel]]
	    }
	    switch -- ${answer} {
	    0 {
		    #save
		    return [save_file]
		}
	    1 {
		    #fast save
		    return [fastsave_file]
		}
	    2 {
		    #don't save (revert)
		    #don't save file
		    return 1
		}
	    default {
		    #cancel
		    return 0
		}
	    }
	} else {
	    #file not modified
	    return 1
	}
    }

    method Title {{full 1}} {
	global sn_options

	set tf [file tail $itk_option(-filename)]
	if {$itk_option(-file_changed)} {
	    set tf "** ${tf} **"
	}
	if {[$itk_component(editor) cget -state] != "normal"} {
	    append tf " [get_indep String FileViewOnly]"
	} \
	elseif {[file exists $itk_option(-filename)] && ![file writable $itk_option(-filename)]} {
	    append tf " [get_indep String TitleReadOnly]"
	}
	if {${full}} {
	    return [sn_title ${tf}]
	} else {
	    return ${tf}
	}
    }
    method Icon {} {
	if {$itk_option(-file_changed)} {
	    return [sn_view_icon "" "** [file tail $itk_option(-filename)] **"]
	} else {
	    return [sn_view_icon [file tail $itk_option(-filename)]]
	}
    }
    method SetTitle {} {
	global sn_options sn_root

	${topw} configure -title [Title] -iconname [Icon]

	set lines 0
	scan [$itk_component(editor) index "end-1c"] %d lines

	set nm [file join $sn_options(sys,project-dir) $itk_option(-filename)]

	#display the file name in native OS conditions
	set nm [file nativename ${nm}]

	set $itk_option(-message_var) "$nm: $lines [get_indep String Lines]"
    }

    proc is_fileModified {file} {
	#we must accept that by creating a file in an empty project
	#paf_db_f is not defined.
	if {[::info commands paf_db_f] == ""} {
	    return 0
	}

	#this happens when neu file is being edited!
	if {![file exists ${file}]} {
	    return 0
	}

	#compare stored and actual file status
	set filestatus [paf_db_f get \
	    -key -col [list 0 1 2] ${file}]
	file stat ${file} st

	#modify time changed?
	if {[lindex ${filestatus} 1] != $st(mtime)} {
	    return 1
	}
	return 0
    }

    method UpdateHighlights {{run_parse 1}} {
	global sn_options
	global env
	global sn_debug
	global $itk_component(editor)-group
	upvar #0 $itk_component(editor)-group grp

	if {! [file exists $itk_option(-filename)]} {
	    return
	}

	#maybe the file is not availiable in the project
	if {${run_parse}} {
	    if {$sn_options(def,auto-reparse)} {
		if {![sn_parse_uptodate [list $itk_option(-filename)] 0]} {
		    return
		}
	    }
	}

        # Checking the database for a highlight file is commented
        # out since the parsers should never write into the database
        # directly. If in the future we can pass the -h flag to a
        # normal parse operation, we could revisit this issue.

	#if {[::info commands paf_db_f] != ""} {
	#    set highfile [paf_db_f get \
	#        -key -col {0 2} $itk_option(-filename)]
	#} else {
	#    set highfile ""
	#}
        #
	#set grp [lindex ${highfile} 0]
        set grp [sn_get_file_type $itk_option(-filename)]

	if {[lsearch \
	    -exact $sn_options(sys,builtin-highlighting) ${grp}] != -1} {
	    global sn_text_highlight_group
	    set sn_text_highlight_group($itk_component(editor)) ${grp}

	    sn_log "Highlighting -delall \"$itk_option(-filename)\", group \"${grp}\""

	    Sn_Syntax_Highlight \
	        -delall ${grp} $itk_component(editor) 1.0 end
	    return
	}

	#set highfile [lindex ${highfile} 1]

	#verify if the file is modified (modified status)
	set mod [is_fileModified $itk_option(-filename)]

	#if {!${mod} && ${highfile} != "" && [file exists ${highfile}]} {
	#    sn_log "Highlighting \"$itk_option(-filename)\": ${highfile}"
	#    sn_delete_editor_tags $itk_component(editor)
	#    if {[catch {Sn_Highlight_Text $itk_component(editor) ${highfile}} err]} {
	#	bgerror ${err}
	#    }
	#    if {$sn_options(def,edit-xref-highlight)} {
	#	catch {Sn_Highlight_Text $itk_component(editor) $itk_option(-filename).HIGH}
	#    }
	#    return
	#}

	set highcmd [sn_highlight_browser $itk_option(-filename) "h"]
	if {${highcmd} == ""} {
            # When the source code browser does not support highlighting
            # the symbols parsed from the file are still highlighted, so
            # just delete any tags currently in the editor.
	    sn_delete_editor_tags $itk_component(editor)

	    sn_log "Empty highlighter command for \"$itk_option(-filename)\""
	    return
	}

        set files_to_delete [list]

        set outf [sn_tmpFileName]
        lappend highcmd -s $outf
        lappend files_to_delete $outf

        set errf [sn_tmpFileName]
        lappend files_to_delete $errf

        if {${mod}} {
	    # FIXME: It is not really clear why this logic is needed
            # since we only highlight when opening, saving, or reverting.

            # When a file has been modified outside the IDE, save
            # the editor contents into a tmp file and highlight that.
            set savef [sn_tmpFileName]
            set fd [open ${savef} w]
            fconfigure ${fd} \
                -encoding $sn_options(def,system-encoding) \
                -blocking 0
            puts -nonewline ${fd} [$itk_component(editor) get 1.0 end]
            close ${fd}

            lappend highcmd $savef
            lappend files_to_delete $savef
        } else {
            lappend highcmd $itk_option(-filename)
        }

	sn_log "Highlighter: ${highcmd}"

	if {[catch {eval exec -- ${highcmd} 2>$errf} err]} {
		bgerror ${err}
		return
	}

        if {[file exists $errf] && [file size $errf] > 0} {
            set fd [open $errf r]
            fconfigure ${fd} \
                -encoding $sn_options(def,system-encoding) \
                -blocking 0
            set data [read -nonewline ${fd} [file size $errf]]
            close $fd
            sn_log "highlight cmd stderr was : $data"
        }

	#if {!${mod}} {
	#    set highfile [paf_db_f get \
	#        -key -col [list 2] $itk_option(-filename)]
	#} else {
	#    set highfile ${outf}
	#}

	set fd [open ${outf}]
	fconfigure ${fd} \
	    -encoding $sn_options(def,system-encoding) \
	    -blocking 0
	set highfile [gets ${fd}]
	close ${fd}

        lappend files_to_delete ${highfile}

	sn_log "Loading Highlight File: ${highfile}"

	if {${highfile} != ""} {
	    sn_delete_editor_tags $itk_component(editor)
	    if {[catch {Sn_Highlight_Text $itk_component(editor) ${highfile}} err]} {
		bgerror ${err}
	    }
	}

	if {$sn_options(def,edit-xref-highlight)} {
	    catch {Sn_Highlight_Text $itk_component(editor) $itk_option(-filename).HIGH}
	}

	#if {[info exists env(TMP)] && [file dirname ${highfile}] != $env(TMP)} {
	#    lappend files_to_delete ${highfile}
	#}

	if {!${sn_debug}} {
            foreach file $files_to_delete {
                file delete -- $file
            }
	}
    }

    #all == -1: disable all filter flags
    #all == 1: enable all filter flags
    #all == 0: current state of filter flags
    private method read_filetags {{all 0} {file_cont ""}} {
	global sn_options sn_sep combobox_editor_scopes

	if {$itk_option(-symbols) == ""} {
	    error "read_filetags should not be called with a symbol combo"
	}

	#we do have new symbols in the combo box, make sure that
	#this symbols are drawn when pulldown the combo box
	set displayed 0

	if {$itk_option(-symbols_filter) != ""} {
	    upvar #0 $itk_option(-symbols_filter)-related related
	    set qry ""
	    foreach s [array names combobox_editor_scopes] {
		upvar #0 $itk_option(-symbols_filter)-${s} value
		if {${all} == -1} {
		    uplevel #0 "set $itk_option(-symbols_filter)-${s} off"
		    continue
		}
		if {${all} ||([info exists value] && ${value} != "off")} {
		    foreach db $combobox_editor_scopes(${s}) {
			if {[::info commands paf_db_${db}] != ""} {
			    uplevel #0 "set $itk_option(-symbols_filter)-${s} \
			      ${s}"
			    lappend qry ${db}
			}
		    }
		}
	    }
	} else {
	    set qry ""
	    set related 1
	}

	if {${qry} == ""} {
	    return ""
	}

	if {${related}} {
	    #read only the symbols for the edited file
	    # Class,symbol,type,position.
	    if {${file_cont} != ""} {
		#set qry ".+ \([join $qry "|"]\) .*"
		set qry ".+${sn_sep}\([join ${qry} "|"]\)${sn_sep}.*"
		#calls a C/function
		return [sn_db_format_qry [lmatch \
		    -regexp ${file_cont} ${qry}]]
	    } else {
		#set qry ".+ \([join $qry "|"]\)\$"
		set qry ".+${sn_sep}\([join ${qry} "|"]\)\$"
		if {[::info commands paf_db_fil] != ""} {
		    set syms [paf_db_fil seq \
		        -data -col {2 3 4 1} \
		        -regexp ${qry} "$itk_option(-filename)${sn_sep}"]
		    #calls a C/function
		    return [sn_db_format_qry ${syms}]
		} else {
		    return ""
		}
	    }
	} else {
	    #read all symbols of all files
	    set combo [string range $itk_option(-symbols) 0 [expr \
	      [string last "." $itk_option(-symbols)] - 1]]
	    set syms ""
	    #set qry ".+ \([join $qry "|"]\)\$"
	    set qry ".+${sn_sep}\([join ${qry} "|"]\)\$"
	    catch {
		set syms [paf_db_fil seq \
		    -data -regexp ${qry} \
		    -col {2 3 4 1}]
	    }
	    return [sn_db_format_qry ${syms}]
	}
    }

    method GetFileTags {all {file_cont ""}} {
	#add symbols into combobox for symbol list
	#only if the related flag is setted
	upvar #0 $itk_option(-symbols_filter)-related related

	if {$itk_option(-symbols) != "" && ${related}} {
	    $itk_option(-symbols) configure -contents [read_filetags ${all} ${file_cont}]
	}

	#find the actual tag (symbol), mark it and view
	#it in the combo box
	find_and_mark_tag
    }

    method find_tag {{pos ""}} {
	global sn_options sn_sep
	if {${pos} == ""} {
	    if {[catch {set idx [$itk_component(editor) index {insert wordstart}]}]} {
		return
	    }
	} else {
	    set idx ${pos}
	}

	set tag [lindex [lmatch [$itk_component(editor) tag names ${idx}] "* *"] end]
	if {${tag} == "" || [lindex ${tag} end] == "cl"} {
	    set sec_idx [$itk_component(editor) index "${idx} lineend -1c wordstart"]
	    set sec_tag [lindex [lmatch [$itk_component(editor) tag names ${sec_idx}] \
	      "* *"] end]

	    if {${sec_tag} == ""} {
		if {${tag} == ""} {
		    $itk_component(editor) mark unset %beg% %end%
		    return
		}
	    } else {
		set tag ${sec_tag}
		set idx ${sec_idx}
	    }
	}

	set beg 0.0
	set end 0.0
	set ranges [$itk_component(editor) tag ranges ${tag}]
	for {set off 0; set max [expr [llength ${ranges}] / 2]} {${off} < \
	  ${max}} {incr off} {
	    set v [expr ${off} * 2]
	    set beg [lindex ${ranges} ${v}]
	    set end [lindex ${ranges} [expr ${v} + 1]]
	    if {[$itk_component(editor) compare ${beg} <= ${idx}] && [$itk_component(editor) compare \
	      ${end} >= ${idx}]} {
		break
	    }
	}

	if {[lindex ${tag} end] != "cl"} {
	    $itk_component(editor) mark set %beg% ${beg}
	    $itk_component(editor) mark set %end% ${end}
	} else {
	    $itk_component(editor) mark unset %beg% %end%
	}

	return [tag_with_sep ${tag}]
    }
    method find_offset {} {
	if {[catch {set idx [$itk_component(editor) index {insert wordstart}]}]} {
	    return 0
	}

	set tag [lindex [lmatch [$itk_component(editor) tag names ${idx}] "* *"] end]
	if {${tag} == "" || [lindex ${tag} end] == "cl"} {
	    set sec_idx [$itk_component(editor) index "${idx} lineend -1c wordstart"]
	    set sec_tag [lindex [lmatch [$itk_component(editor) tag names ${sec_idx}] \
	      "* *"] end]

	    if {${sec_tag} == ""} {
		if {${tag} == ""} {
		    return 0
		}
	    } else {
		set tag ${sec_tag}
		set idx ${sec_idx}
	    }
	}

	set beg 0.0
	set end 0.0
	set ranges [$itk_component(editor) tag ranges ${tag}]
	for {set off 0; set max [expr [llength ${ranges}] / 2]} {${off} < \
	  ${max}} {incr off} {
	    set v [expr ${off} * 2]
	    set beg [lindex ${ranges} ${v}]
	    set end [lindex ${ranges} [expr ${v} + 1]]
	    if {[$itk_component(editor) compare ${beg} <= ${idx}] && [$itk_component(editor) compare \
	      ${end} >= ${idx}]} {
		return ${off}
	    }
	}
	return 0
    }

    proc tag_with_sep {tag} {
	global sn_sep
	if {[llength ${tag}] < 3} {
	    set tag "# ${tag}"
	}
	#set tag [join $tag $sn_sep]
	return [sn_db_format_qry [list ${tag}]]
    }

    method find_and_mark_tag {} {
	global sn_options sn_sep sn_scopes
	if {[catch {set idx [$itk_component(editor) index {insert wordstart}]}]} {
	    return
	}

	set $itk_option(-linenumber_var) [$itk_component(editor) index insert]

	if {$itk_option(-symbols) != ""} {
	    $itk_option(-symbols) configure -entrytext ""
	}

	#verify if we stay on a symbol declaration/implementaion
	set pos_tags ""
	set tmp_tags [$itk_component(editor) tag names ${idx}]
	#sn_log "insert cursor tags : $tmp_tags"
	foreach tg $tmp_tags {
	    if {[string first " " ${tg}] != -1} {
		#tags like "symbol scope"
		lappend pos_tags ${tg}
	    }
	}

	set tag [lindex ${pos_tags} end]
	if {${tag} == "" || [lindex ${tag} end] == "cl"} {
	    set sec_idx [$itk_component(editor) index "${idx} lineend -1c wordstart"]
	    set sec_tag [lindex [lmatch [$itk_component(editor) tag names ${sec_idx}] \
	      "* *"] end]

	    if {${sec_tag} == ""} {
		if {${tag} == ""} {
		    $itk_component(editor) mark unset %beg% %end%
		    return
		}
	    } else {
		set tag ${sec_tag}
		set idx ${sec_idx}
	    }
	}

	set beg 0.0
	set end 0.0
	set ranges [$itk_component(editor) tag ranges ${tag}]
	for {set off 0; set max [expr [llength ${ranges}] / 2]} {${off} < \
	  ${max}} {incr off} {
	    set v [expr ${off} * 2]
	    set beg [lindex ${ranges} ${v}]
	    set end [lindex ${ranges} [expr ${v} + 1]]
	    if {[$itk_component(editor) compare ${beg} <= ${idx}] && [$itk_component(editor) compare \
	      ${end} >= ${idx}]} {
		break
	    }
	}

	if {[lindex ${tag} end] != "cl"} {
	    $itk_component(editor) mark set %beg% ${beg}
	    $itk_component(editor) mark set %end% ${end}
	} else {
	    $itk_component(editor) mark unset %beg% %end%
	}

	set tg [tag_with_sep ${tag}]

	if {$itk_option(-symbols) != ""} {
	    $itk_option(-symbols) selecttext [join ${tg}] ${off}
	}

	upvar #0 ${this}-off old_off

	if {${tg} == ${old_tg} && ${old_off} == ${off}} {
	    return
	}

	set old_tg ${tg}
	set old_off ${off}

	#execute selectcommand, when the tag is changed
	if {$itk_option(-selectcommand) != ""} {
	    split_symbol ${tg} cls sym scope
	    set line [$itk_component(editor) index insert]
	    #::eval $selectcommand [list $scope $sym $cls $filename $line "" \
	      "" ""]
	    ::eval $itk_option(-selectcommand) [Selection]
	}
    }

    #
    # Indent/Outdent the selected area (or the current line) of the file.
    #
    proc Indent {cls indent} {
	global sn_options
	global tkPriv
	global sn_text_highlight_group

	set editor [${cls} editor]
	set pos [$editor index @0,0]

	if {[catch {set beg [$editor index "sel.first linestart"]}]} {
	    set beg [$editor index "insert linestart"]
	    set end [$editor index "insert lineend"]
	} else {
	    #avoid to include an extra line by getting the index
	    #this can happen, when the selection is make to the
	    #end of line
	    set last_line_col [lindex [split [$editor index sel.last] \
	      "."] end]
	    if {${last_line_col} == "0"} {
		set tkPriv(selectMode) "line"
		set end [$editor index "sel.last -1c"]
	    } else {
		set end [$editor index "sel.last lineend"]
	    }
	}

	if {[info exists sn_text_highlight_group($editor)]} {
	    set group $sn_text_highlight_group($editor)
	} else {
	    set group ""
	}
	upvar #0 ${cls}-indenting_broken indenting_broken
	set indenting_broken 0

	#execute SN-internal indent/outdent procedure
	set txt [$editor get ${beg} ${end}]
	set newtxt [sn_indent_outdent \
	    -indent ${indent} \
	    -indentwidth $sn_options(def,edit-indentwidth) \
	    -tabwidth $sn_options(def,edit-tabstop) \
	    -replacetabs $sn_options(def,edit-tab-inserts-spaces) \
	    -group ${group} ${txt}]

	#nothing changed, return!
	if {${indenting_broken} || [string compare ${newtxt} ${txt}] == 0} {
	    return
	}

	#replace with reworked text
	tkTextReplace $editor ${beg} ${end} ${newtxt}

	#eventually reparse a fast-saved file
	#$cls UpdateHighlights

	#reset some tags
	$editor tag remove sel 0.0 end
	$editor tag add sel ${beg} "${end} lineend + 1c"
        $editor tag raise sel
	set ypos [expr [lindex [split ${pos} "."] 0] - 1]
	$editor yview ${ypos}

	#that's all folk
	return
    }

    method print {} {
	global tcl_platform
	if {$tcl_platform(platform) != "windows"} {
	    print_dialog $itk_component(editor)
	} else {
	    ide_print_text $itk_component(editor)
	}
    }

    proc print_file {print_dialog t} {
	global tcl_platform sn_options

	upvar #0 ${print_dialog}-ptarget target
	switch -- ${target} {
	"marked" {
		if {[catch {set lst [${t} get sel.first sel.last]} err]} {
		    #no selection?
		    sn_error_dialog ${err}
		    return
		}
	    }
	"view" {
		set end [${t} index @0,[winfo height ${t}]]
		set lst [${t} get @0,0 "${end} lineend"]
	    }
	"all" -
	default {
		set lst [${t} get 0.0 end]
	    }
	}

	if {${print_dialog} != "" &&
	        [itcl::find object ${print_dialog}] == ${print_dialog}} {
	    itcl::delete object ${print_dialog}
	}

	#we have to translate \t to the current size number of tabs
	#this have to be done always, because the pr command on unix
	#makes also indenting at the beginning of line, so that the
	#first tab could be wrong displayed
	set tabsize [${t} cget -tabsize]
	#at lease one space
	set spaces " "
	for {set i 1} {${i} < ${tabsize}} {incr i} {
	    append spaces " "
	}
	regsub \
	    -all \t ${lst} ${spaces} lst

	#write contents into file
	set tmpf [sn_tmpFileName]
	set fd [open ${tmpf} "w+"]
	fconfigure ${fd} \
	    -encoding $sn_options(def,system-encoding) \
            -blocking 0
	puts ${fd} ${lst}
	close ${fd}

	if {$tcl_platform(platform) != "windows"} {
	    upvar #0 ${print_dialog}-cmd cmd
	    # Escape [] and $ in user input for eval in sn_print_file
	    regsub -all {(\$|\[|\])} $cmd {\\&} cmd
	} else {
	    set cmd ide_winprint
	}

	sn_print_file ${cmd} ${tmpf}

	catch {file delete \
	        -- ${tmpf}}
	catch {unset target}
	catch {unset cmd}
    }

    proc print_dialog {t} {
	global sn_options
	global tcl_platform

	set topl [winfo toplevel ${t}]
	set print_dialog "${topl}.printdlg"

	if {$tcl_platform(platform) == "windows"} {
	    upvar #0 ${print_dialog}-ptarget target

	    #default print all when no selection
	    set target "all"

	    #if selection availiable ask to print selection only
	    if {! [catch {set lst [${t} get sel.first sel.last]}] && ${lst} \
	      != ""} {
		set answer [tk_dialog auto [get_indep String Print] \
		  [get_indep String PrintSelection] question_image 0 \
		  [get_indep String All] [get_indep String Marked] \
		  [get_indep String Cancel]]
		if {${answer} == 2} {
		    return
		}
		if {${answer} == 1} {
		    set target "marked"
		}
	    }
	    Editor&::print_file ${print_dialog} ${t}
	    return
	}

	if {[winfo exists ${print_dialog}]} {
	    ${print_dialog} raise
	    return
	}

	set print_dialog [sourcenav::Window ${print_dialog} \
	    -leader ${t}]

	sn_motif_buttons ${print_dialog} bottom 0 [get_indep String ok] \
	  [get_indep String cancel]
	${print_dialog}.button_0 config \
	    -command "Editor&::print_file ${print_dialog} ${t}"
	${print_dialog}.button_1 config \
		-command "itcl::delete object ${print_dialog}"

	global ${print_dialog}-ptarget ${print_dialog}-cmd
	set ${print_dialog}-ptarget "all"

	set tit [${topl} cget -title]
	set ${print_dialog}-cmd [format $sn_options(def,ascii-print-command) \
	  ${tit}]
	${print_dialog} configure -title [list [get_indep String Print] ${tit}]

	frame ${print_dialog}.command
	label ${print_dialog}.command.prompt \
	    -text [get_indep String SQLPprintercmd]
	entry ${print_dialog}.command.cmd \
	    -width 70 \
	    -textvariable ${print_dialog}-cmd
	focus ${print_dialog}.command.cmd
	bind ${print_dialog}.command.cmd <Any-Return> \
	  "${print_dialog}.button_0 invoke"
	pack ${print_dialog}.command.prompt \
	    -side left
	pack ${print_dialog}.command.cmd \
	    -side left \
	    -padx 10 \
	    -fill x \
	    -expand y
	pack ${print_dialog}.command \
	    -fill x \
	    -expand y

	radiobutton ${print_dialog}.marked \
	    -text [get_indep String Marked] \
	    -variable ${print_dialog}-ptarget \
	    -value marked
	radiobutton ${print_dialog}.all \
	    -text [get_indep String All] \
	    -variable ${print_dialog}-ptarget \
	    -value all
	#radiobutton $print_dialog.view -text [get_indep String View] \
	  #	-variable $print_dialog-ptarget -value view
	pack ${print_dialog}.marked \
	    -anchor w \
	    -padx 60
	pack ${print_dialog}.all \
	    -anchor w \
	    -padx 60
	#pack $print_dialog.view -anchor w -padx 60

	${print_dialog} move_to_mouse
	catch {${print_dialog} resizable no no}

	${print_dialog} take_focus
    }

    method filter {{all 0}} {
	if {$itk_option(-symbols) != ""} {
	    $itk_option(-symbols) configure -contents \
	        [lsort -command sn_compare [read_filetags ${all}]]
	}
    }

    #this function is called, when the symbols combobox
    #is called
    method postcommand {m} {
	if {! ${displayed}} {
	    filter
	    set displayed 1
	}
    }

    #called, when the editor should be activated
    method activate {} {
	if {$itk_option(-menu) != ""} {
	}
	if {$itk_option(-toolbar) != ""} {
	    pack $itk_component(toolbar) \
	        -side left
	}
	#by switching to the editor, disable the
	#flag to read all symbols, because it can take
	#a long time
	if {$itk_option(-symbols_filter) != ""} {
	    upvar #0 $itk_option(-symbols_filter)-related related
	    set related 1
	}
	DispModified
	GetFileTags 0 ""
	focus $itk_component(editor)
    }

    method deactivate {} {
	if {$itk_option(-toolbar) != ""} {
	    pack forget $itk_component(toolbar)
	}
	set $itk_option(-message_var) ""

	if {$itk_option(-linenumber_var) != ""} {
	    set $itk_option(-linenumber_var) ""
	}
    }

    method Focus_In {} {
	global last_Editor
	set last_Editor ${this}

        if {$itk_option(-linenumber_var) != ""} {
	    set $itk_option(-linenumber_var) [$itk_component(editor) index insert]
	}
	SetTitle
    }

    method Focus_Out {} {

	set $itk_option(-message_var) ""

	if {$itk_option(-linenumber_var) != ""} {
	    set $itk_option(-linenumber_var) ""
	}
    }

    #converts "symbol(scope) class" to "class symbol scope"
    proc split_symbol {ssym class symbol scope} {
	upvar ${class} cc
	upvar ${symbol} ss
	upvar ${scope} xs

	#convert 'foo()(md) boo' --> 'foo%%(md) boo'
	regsub \
	    -all {\(\)\(} [join ${ssym}] {%%\(} sym
	#convert 'foo%%(md) boo' --> 'foo%% md boo'
	regsub \
	    -all {\(|\)} ${sym} " " sym
	#convert 'foo%% md boo' --> 'foo() md boo'
	regsub \
	    -all {%%} ${sym} {()} sym
	set ss [lindex ${sym} 0]
	set xs [lindex ${sym} 1]
	set cc [lindex ${sym} 2]
    }

    # Figure out which symbol menus should be used in the editor!
    proc combobox_scopes {} {
	global combobox_editor_scopes sn_fortran_scopes
	set scp [list cl {cl t un} e {e ec} fu {fu fd md mi} fr {fr} gv \
	  {con gv iv} ma {ma}]

	foreach sc {com cov su} {
	    if {[::info commands paf_db_${sc}] != ""} {
		lappend scp com {com cov} su {su}
		break
	    }
	}
	array set combobox_editor_scopes ${scp}
    }

    ################################################
    #goto specified symbol
    #
    #one important information is that this function
    #should only be called from the combobox and not
    #external, except the call simulate the combobox
    #functionality
    method goto {combo ssym} {
	if {${ssym} == ""} {
	    return
	}

	#dump the current view into the history stack
	${topw} history_stack_add_point ${this}

	if {$itk_option(-symbols_filter) != ""} {
	    upvar #0 $itk_option(-symbols_filter)-related related
	} else {
	    set related 1
	}

	#convert 'foo()(md) boo' --> 'foo%%(md) boo'
	regsub \
	    -all {\(\)\(} ${ssym} {%%\(} sym
	#convert 'foo%%(md) boo' --> 'foo%% md boo'
	regsub \
	    -all {\(|\)} ${sym} " " sym
	#convert 'foo%% md boo' --> 'foo() md boo'
	regsub \
	    -all {%%} ${sym} {()} sym
	if {[llength ${sym}] == 3} {
	    set name "[lindex ${sym} 2] [lindex ${sym} 0]"
	    set scope [lindex ${sym} 1]
	} else {
	    set name [lindex ${sym} 0]
	    set scope [lindex ${sym} 1]
	}
	if {${scope} == ""} {
	    set scope cl
	}

	#get the offset of the selected text, if there
	#are more than one entry with the same text
	if {${combo} != ""} {
	    set off [${combo} offset]
	} else {
	    set off 0
	}

	if {${related}} {
	    #sn_log "editor tag dump:"
	    #foreach tag [$itk_component(editor) tag names] {
	    #    if {[$itk_component(editor) tag ranges $tag] == {}} {continue}
	    #    sn_log "tag \"$tag\" ranges are\
	    #        \{[$itk_component(editor) tag ranges $tag]\}"
	    #}

	    #get ranges with the specified scope
	    set tg "${name} ${scope}"
	    set sym_ranges [$itk_component(editor) tag ranges ${tg}]
	    # If multiple text ranges are returned, use the
	    # offset to get the correct range
	    set first_off [expr {$off * 2}]
	    set last_off [expr {$first_off + 1}]
	    set sym_range_start [lindex $sym_ranges $first_off]
	    set sym_range_end [lindex $sym_ranges $last_off]
	    sn_log "goto: sym_range for \{$tg\} is\
	            \{$sym_range_start $sym_range_end\}"
	    # Find the highlight range inside the symbol range.
	    # If no highlight is found in the symbol range, use
	    # the symbol start index (keeps older parsers working)
	    set high_range [$itk_component(editor) tag nextrange \
	        ${scope} $sym_range_start $sym_range_end]
	    if {[llength $high_range] > 0} {
	        set pos [lindex $high_range 0]
	    } else {
	        set pos $sym_range_start
	    }

	    sn_log "goto: start index for $tg is $pos"
	    SetFondPos ${pos} 0 0
	} else {
	    set pos ""
	}

	# If the given symbol is not found by searching
	# for tags in the text widget, search the DB
	# without a scope for symbols in this file.

	if {${pos} == ""} {
	    set cnt [read_matched_from_db "" "" \
	        -exact ${name} "" "" $itk_option(-filename) \
	        -1 -1 0 ${off}]
	    if {${cnt} != ""} {
		gotofile_cb "" [lindex ${cnt} 0] "" ${off}
	    }
	}
    }

    method getfilename {} {
	return $itk_option(-filename)
    }

    method getvalidfilename {} {
	global sn_options
	if {$itk_option(-filename) != $sn_options(noname_file)} {
	    return $itk_option(-filename)
	} else {
	    return ""
	}
    }

    method editor {} {
	return $itk_component(editor)
    }

    method setmodified {m {reporterror ""}} {
	# If file is readonly and first modified, warn user
	# for this.
	if {${reporterror} != "noerror" && ${m} && [file exists $itk_option(-filename)] \
	  && ![file writable $itk_option(-filename)]} {
	    sn_error_dialog "$itk_option(-filename) [get_indep String ErrorNotWrite]"
	    set ret "error"
	} else {
	    set ret ""
	}

	$this configure -file_changed ${m}
	DispModified
	return ${ret}
    }

    #tells if the current file is editable
    method canModify {} {
	global sn_options
	#no permissions
	if {[file exists $itk_option(-filename)] && ![file writable $itk_option(-filename)]} {
	    return 0
	}
	#file is opened as readonly
	if {$itk_option(-editstate) != "normal"} {
	    return 0
	}
	return 1
    }

    #tells if the file is a valid file, this is true
    #if the file name is valid
    method validFilename {} {
	global sn_options
	if {$itk_option(-filename) == $sn_options(noname_file)} {
	    return 0
	}
	return 1
    }

    #simulate Text Edit
    method mark args {
	::eval $itk_component(editor) mark ${args}
    }

    #this function is called from the retriever, when something
    #is selected
    method gotofile_cb {w line data {off 0}} {
	if {${w} != ""} {
	    set line [Retriever&::convert_to_line ${w}]
	}

	if {${line} == ""} {
	    error "Editor&::gotofile_cb empty line argument"
	}

	set line [split ${line} \t]
	set sym [lindex ${line} 0]
	set cls [lindex ${line} 1]
	set file [lindex ${line} 4]
	set pos [lindex ${line} 5]

	#get scope and symbol name
	get_key_and_scope ${sym} name scope
	set name [string trim "${name} ${cls}"]

	#probably correct filename
	set file [sn_convert_FileName ${file}]

	#add current position to history stack
	${topw} history_stack_add_point ${this}

	#if another file should be loaded and the actual
	#file is modified, ask to use another window
	if {${file} != $itk_option(-filename) && $itk_option(-file_changed)} {
	    EditFile ${name} ${file} ${pos}
	    return
	} else {
	    set answer 0
	}

	if {${answer} == 0} {
	    #load the file, if it's different
	    if {${file} != $itk_option(-filename)} {
		editfile ${file} ${pos} revert
	    } else {
		SetFondPos ${pos} 0 0
	    }
	}

	focus $itk_component(editor)
    }

    method gotosymbol {{scope ""} {sym ""} {cls ""} {file ""} {from ""} {type \
      ""} {prm ""} {to ""} {always 1}} {

	#verify if gotosymbol is disabled, this is required, because
	#it can happen, that the editfile command is called twice.	
	global gotosymbol_active
	if {[info exists gotosymbol_active] && ${gotosymbol_active} == 0} {
	    return 0
	}

	#add current position to prev positions list
	${topw} history_stack_add_point ${this}

	if {${file} != "" &&(${from} != "" || ${sym} == "")} {
	    if {${file} != $itk_option(-filename)} {
		editfile ${file} ${from}
	    } \
	    elseif {${from} != ""} {
		SetFondPos ${from} 0 0
	    }
	    return 1
	}

	#if something specified
	if {"${scope}${sym}${cls}${file}" != ""} {
	    #if we are around the area, as example a class member
	    #and the gotosymbol is in this area, don't change
	    #your area
	    if {${always} == 0 && ${file} == "" && $itk_option(-symbols) != \
	      ""} {
		set txt [$itk_option(-symbols) cget -entrytext]
		split_symbol $txt cc ss xscope
		if {${scope} == "cl" && ${sym} == ${cc} || ${scope} == \
		  ${xscope} && ${sym} == ${ss} && ${cls} == ${cc}} {
		    return 1
		}
	    }
	    if {${cls} != ""} {
		set sym "${cls} ${sym}"
	    }
	    sn_retrieve_symbol ${sym} ${scope} ${file} \
	        -exact 1 1 "${this} gotofile_cb" "" ${type} ${prm}
	    return 1
	}
	return 0
    }

    #return selected text in a readable format
    method Selection {} {
	global sn_sep
	set ranges [$itk_component(editor) tag ranges sel]
	set pos [$itk_component(editor) index {insert wordstart}]
	set off [find_offset]

	#make sure that the cursor doesn't stay in a header, when
	#this is true, we have to accept the current position,
	#NOT THE SELECTION
	if {[points_to_header $itk_component(editor) ${pos}] == "" && ${ranges} != ""} {
	    #grep can have added more than one selection region!
	    if {[llength ${ranges}] > 2} {
		set ranges [lrange ${ranges} 0 1]
	    }
	    set txt [::eval $itk_component(editor) get ${ranges}]
	    #delete new lines from the selection
	    if {[string first \n ${txt}] != -1} {
		set txt [lindex [split ${txt} \t] 0]
	    }
	    set txt [string trim ${txt}]

	    set scope ""
	    set cls ${txt}
	    set sym ""
	    set from ""
	    set type ""
	    set prm ""
	    set to ""

	    if {${txt} != ""} {
		#if insert position points to a header,
		#pass the filename also to the selection, not only this,
		#also return the exact db position of the position.
		if {[points_to_header $itk_component(editor) ${pos}] != ""} {
		    set file [getvalidfilename]

		    #get current scope
		    set tg [find_tag]
		    split_symbol ${tg} dummy dummy scope

		    #build pattern to look for
		    set pat [Retriever&::pattern ${txt}]

		    #we have to return the exact position
		    set cnt [read_matched_from_db "" ${scope} \
		        -exact ${pat} "" "" $itk_option(-filename) \
		        -1 -1 0 ${off}]
		    if {${cnt} != ""} {
			set cnt [split ${cnt} \t]
			if {[string first ${sn_sep} ${pat}] != -1} {
			    set cls [lindex [split ${pat} ${sn_sep}] 0]
			    set sym [lindex [split ${pat} ${sn_sep}] 1]
			} else {
			    set sym ${pat}
			    set cls ""
			}
			set type [lindex ${cnt} 2]
			set prm [lindex ${cnt} 3]
			set from [lindex ${cnt} 5]
			set to [lindex ${cnt} 6]
		    } else {
			set from ${pos}
			set cls ${txt}
		    }
		} else {
		    set file ""
		}
		return [list ${scope} ${sym} ${cls} ${file} ${from} ${type} \
		  ${prm} ${to}]
	    }
	} else {
	    set tg [find_tag]
	    if {${tg} != ""} {
		split_symbol ${tg} cls sym scope

		#return the exact description of the symbol
		set cnt [read_matched_from_db "" ${scope} \
		    -exact [string trim "${cls}${sn_sep}${sym}" ${sn_sep}] "" \
		  "" $itk_option(-filename) \
		    -1 -1 0 ${off}]
		if {${cnt} != ""} {
		    set cnt [split ${cnt} \t]
		    set type [lindex ${cnt} 2]
		    set prm [lindex ${cnt} 3]
		    set from [lindex ${cnt} 5]
		    set to [lindex ${cnt} 6]
		} else {
		    set type ""
		    set prm ""
		    set from ${pos}
		    set to ""
		}
		return [list ${scope} ${sym} ${cls} $itk_option(-filename) ${from} \
		  ${type} ${prm} ${to}]
	    }
	}

	#nothing is selected, return file and it's position
	return [list "" "" "" $itk_option(-filename) ${pos}]
    }

    method clearselection {} {
	$itk_component(editor) tag remove sel 0.0 end
    }

    method Focus {} {
	focus $itk_component(editor)
    }

    #make a title for the Dump position, this is usefull
    #for the views stack (prev/next)
    method DumpTitle {{dump ""}} {
	if {${dump} == ""} {
	    set dump [Dump]
	}
	set tg [find_tag]
	if {${tg} != ""} {
	    split_symbol ${tg} cls sym scope
	    return "Edit [string trim "${sym}(${scope}) ${cls}"]"
	} else {
	    return "Edit [lindex ${dump} 0] \[[lindex ${dump} 1]\]"
	}
    }

    method AddHistoryFromDump {dumpstr title} {
	set tg [find_tag]

	foreach {key val} ${dumpstr} {
	    set dump(${key}) ${val}
	}
	if {$dump(offset) == ""} {
	    set dump(offset) 0
	}

	if {${tg} != ""} {
	    split_symbol ${tg} cls sym scope
	    if {${cls} != ""} {
		set name [string trim "${cls} ${sym}"]
	    } else {
		set name [string trim ${sym}]
	    }
	    #don't store the position in the file
	    #this makes alot of conflicts
	    set data [list ${scope} ${name} $dump(offset) $dump(file)]
	    set tt ${name}
	} else {
	    set scope f
	    set data [list f $dump(file)]
	    set tt $dump(file)
	}

	#Add position into history
	sn_add_history ${scope} ${data} [sn_make_history_title edit ${scope} \
	  ${tt}]
    }

    #return the important data to restore this widget
    #in later time again (used by saving the project)
    method Dump {} {
	global sn_options
	global tkText

	if {$itk_option(-filename) == $sn_options(noname_file)} {
	    #no dump for nonamed files
	    return ""
	}

	set pos [$itk_component(editor) index insert]
	set state [$itk_component(editor) cget -state]
	set tabsize [$itk_component(editor) cget -tabsize]
	set offset [find_offset]

	if {[info exists tkText($itk_component(editor),ovwrt)]} {
	    set ovwrt $tkText($itk_component(editor),ovwrt)
	} else {
	    set ovwrt 0
	}

	#save settings as {key value}
	return [list file $itk_option(-filename) pos ${pos} state ${state} ovrwt \
	  ${ovwrt} tabsize ${tabsize} offset ${offset}]
    }

    #gets the result from the function "Dump" to
    #restore the older state (used by restoring the project)
    method Restore {str} {
	global sn_options

	foreach {key val} ${str} {
	    set restdata(${key}) ${val}
	}
	if {! [info exists restdata(pos)]} {
	    #backward compatible
	    set restdata(file) [lindex ${str} 0]
	    set restdata(pos) [lindex ${str} 1]
	    set restdata(state) [lindex ${str} 2]
	    set restdata(ovwrt) [lindex ${str} 3]
	    set restdata(tabsize) [lindex ${str} 4]
	}

	if {$restdata(file) != $itk_option(-filename)} {
	    editfile $restdata(file) -1
	}

	#don't set overwrite mode!!
	#set_overwrite $editor $restdata(ovwrt) set

	#be sure, that we use tabsize, not tabs value, tabs value
	#is in pixels and could be too large.
	if {$restdata(tabsize) > 8 || $restdata(tabsize) < 2} {
	    set restdata(tabsize) $sn_options(def,edit-tabstop)
	}

	#don't restore the state of the file
	$itk_component(editor) config \
	    -tabsize $restdata(tabsize)

	#Don't mark the position, just jump to it
	SetFondPos $restdata(pos) 1 1 0

	#set Focus to the widget!!
	#Focus
    }

    # This method is only ever called from MultiWindow&::Refresh_Yourself.
    # It must be a no-op for an editor. We would never want to revert
    # changes in an open editor or revert the contents of an editor
    # to those on disk without the user's permission.

    method Refresh_Display {} {
    }

    method Update_Layout {} {
	global sn_options

	# Actual font size and colors.
	$itk_component(editor) configure \
	    -font $sn_options(def,edit-font) \
	    -fg $sn_options(def,edit-fg) \
	    -bg $sn_options(def,edit-bg) \
	    -selectforeground $sn_options(def,select-fg) \
	    -selectbackground $sn_options(def,select-bg) \
	    -insertbackground $sn_options(def,edit-fg)

	# Actual tab size.
	set_tabsize $itk_component(editor)

	# Actualize tag colors.
	init_tags $itk_component(editor)
	
	# Wrap.
	set_wrap $itk_component(editor)

	# Overwrite flag.
	set_overwrite $itk_component(editor) "" set

	#right mouse option menu
	bind_right_mouse $itk_component(editor)

	#readonly-project
	if {$sn_options(readonly)} {
	    set st disabled
	} else {
	    set st normal
	}
	if {$itk_option(-editstate) != ${st}} {
	    set itk_option(-editstate) ${st}
	    $itk_component(editor) configure \
	        -state $itk_option(-editstate)
	}
    }

    #this function copies all the information from src to
    #dst, like contents, tags, ..
    proc friend_copy {dst src} {
	set sed [${src} editor]
	set ded [${dst} editor]

	#delete existing buffer
	${ded} delete 0.0 end

	#Add the contents of the src editor
	${ded} insert 0.0 [${sed} get 0.0 end]

	#delete the last '\n' from the destination buffer
	${ded} delete {end-1c} end

	#set the same modified flag for the new buffer
	${dst} setmodified [${src} cget -file_changed] noerror

	#set same mtime for the view
	${dst} iset file_mtime [${src} cget -file_mtime]
    }

    #called, when the widget should be closed
    #mode=0: Ask if nessecary (no other buffer
    #        with the same filename)
    #mode=1: always close
    method Close {{mode 0}} {
	if {${mode} == 0 && $itk_option(-file_changed)} {
	    if {[Ask_For_Modified revert]} {
		#can be closed
		set retval 1
	    } else {
		set retval 0
	    }
	} else {
	    set retval 1
	}
	if {$retval} {
	    set was_closed 1
	}
	return $retval
    }
    
    method wasClosed {} {
        return $was_closed
    }

    method whoami {} {
	return edit
    }

    # Old tag names
    private variable old_off ""
    private variable old_tg ""

    # Protected variables
    protected variable editor ""
    protected variable displayed 0
    protected variable gotosymbol_active 1

    #mtime for the file to prove if it has been modified
    #external during it is opened in SN.
    public variable file_mtime 0

    #common variables
    common ".multiwindow-\[0-9\]*"

    #public variables
    itk_option define -filename filename Filename $sn_options(noname_file) {
	    editfile $itk_option(-filename) -1
	    set was_closed 0
    }

 #   public variable file_changed 0

    itk_option define -file_changed file_changed File_Changed 0
    public variable highlight y
    itk_option define -findcombo findcombo Findcombo ""

    protected variable topw
    
    private variable edit_SearchNoCase
    private variable edit_SearchMethod
    private variable edit_SearchString
    private variable edit_SearchDirection
    private variable edit_ReplaceString
    private variable was_closed 0
}

proc wait_editor_end {procfd} {
    set end [gets ${procfd} msg]
    if {${end} < 0} {
	catch {close ${procfd}}
    } else {
	sn_log "wait_editor_end: ${msg}"
    }
    update idletasks
}

#bind tags to the editor
Editor&::EditorBindings

#
# Choose the current editor to edit the files
#
proc sn_edit_file {symbol file {line ""} {search 1} {state ""}} {
    global Switch_Is_Enabled
    incr Switch_Is_Enabled -1

    sn_log "edit file (${symbol}, ${file}, ${line}, ${search}, ${state})"

    set ret [Editor&::EditFile ${symbol} ${file} ${line} ${search} ${state}]

    incr Switch_Is_Enabled

    return ${ret}
}

# This function will take a list of file symbol info retrieved from the
# database and insert tags into the text widget. Two tags can be
# applied for each element in the list. For example, an input of
# {# global1 gv 000001.000 1.10 1.0 1.5} would apply the tag
# "global1 gv" to the range 1.0 -> 1.10 and the tag "gv" to the
# range 1.0 -> 1.5. The "gv" tag is used for highlight colors in the
# editor and the tag "global1 gv" is used when looking up the symbol
# under the cursor. The display list of symbols is sorted so that
# classes and functions are under other symbols like variables.
# This is needed so that the correct symbol shows up when the
# cursor is moved over a symbol.

proc sn_db_create_symbol_tags { textwidget tags_list {exclude_list {}} } {
    global sn_options

    # Insert a couple of temporary tags to help sort
    # the display order of tags. This is needed so
    # that global vars appear higher in the display
    # order than classes or functions, for example.
    $textwidget tag add type_point 1.0
    $textwidget tag add name_point 1.0

    foreach tags $tags_list {
        # format <class symbol type line.col line.col line.col line.col>
        if {[llength $tags] < 7} {
            continue
        }
        set class [lindex $tags 0]
        set symbol [lindex $tags 1]
        set type [lindex $tags 2]

        # The first index integers can be zero padded
        scan [lindex $tags 3] "%d.%d" one two
        set beg "$one.$two"

        set end [lindex $tags 4]
        set pair [split $end "."]
        set end_line [lindex $pair 0]
        set end_col [lindex $pair 1]

        set high_beg [lindex $tags 5]

        set high_end [lindex $tags 6]        
        set pair [split $high_end "."]
        set high_end_line [lindex $pair 0]
        set high_end_col [lindex $pair 1]

        # skip tag, if it is on the ignored tag list
        if {[lsearch -exact $exclude_list $type] != -1} {
            continue
        }

        if {$high_end_line >= $end_line && $high_end_col >= $end_col} {
            set end $high_end_line.$high_end_col
        }

        if {$class == "#"} {
            set tag_poi "$symbol $type"
        } else {
            set tag_poi "$class $symbol $type"
        }

        $textwidget tag add $tag_poi $beg $end
        # Move tags like classes and functions lower in the
        # display list than the other tags we just added.
        switch -exact $type {
            "cl" -
            "mi" -
            "fu" -
            "ma" {
                $textwidget tag lower $tag_poi name_point
            }
        }

	# Access variables from the sn_options array. This
	# variable is defined in sninit.tcl

        if {[info exists sn_options(def,color_$type)]} {
            $textwidget tag add $type $high_beg $high_end
        }
    }

    # Move the tags used for highlight color to a location
    # in the display list that is just before all the tags
    # we just inserted.
    foreach type {cl mi fu ma} {
        if {[$textwidget tag range $type] != {}} {
            $textwidget tag lower $type type_point
        }
    }
    $textwidget tag delete type_point
    $textwidget tag delete name_point

    return
}

# When a parser is invoked with the -h option, it
# creates a highlight file which is read in by
# this method.

proc Sn_Highlight_Text { textwidget highfile } {
    global sn_options

    set fd [open $highfile r]
    fconfigure $fd \
        -encoding $sn_options(def,encoding) \
        -blocking 0

    set data [read $fd [file size $highfile]]
    close $fd

    foreach line [split $data \n] {
        # format <num tag beg_pos end_pos>
        if {[llength $line] != 4} {
            continue
        }
        $textwidget tag add [lindex $line 1] \
                            [lindex $line 2] \
                            [lindex $line 3]
    }
}

# Delete all the symbol tags in the editor.
# Tags liks "gv" are not deleted, the
# ranges that they apply to are removed.

proc sn_delete_editor_tags { textwidget } {
    foreach tag [$textwidget tag names] {
        if {[string first " " $tag] != -1} {
            # A symbol tag like "global1 gv"
            $textwidget tag delete $tag
        } elseif {$tag == "sel"} {
            # Ignore the sel tag
        } else {
            # A tag like "gv"
            $textwidget tag remove $tag 1.0 end
        }
    }
}
