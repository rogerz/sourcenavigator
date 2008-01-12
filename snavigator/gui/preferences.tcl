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
# preferences.tcl - The preferences notebook.
# Copyright (C) 1998, 1999 Cygnus Solutions.

################################################
##
## Preferences window for all the tools that are
## availiable in SN.
##
## names:
## proj = Project
## edit = Editor
## ctree = Class Hierarchy
## classbr = Class Browser
## xref = XReference
## incbr = Include
## retr = Retriever (Find)
## parser = Parser settings
## rcs = Revision Control
## clrfont = Color&Font
## others = Other Settings (Misc)
################################################

package require Itk

itcl_class Preferences& {
    inherit sourcenav::Window

    constructor {{config ""}} {
        global sn_options

        set width 780
        set height 580

        withdraw

        set NoteBook [tixNoteBook $itk_component(hull).nbook -ipadx 0 -ipady 0\
          -borderwidth 0 -background white]

        ${NoteBook} add proj -createcmd "${this} AddProject ${NoteBook} proj"\
          -raisecmd "${this} RaiseProject" -label [get_indep String\
          ProjectMenu] -under [get_indep Pos ProjectMenu]\
          -state [tool_availiable proj 1]

        ${NoteBook} add edit -createcmd "${this} AddEditor ${NoteBook} edit"\
          -raisecmd "${this} RaiseEditor" -label [get_indep String\
          MultiEditor] -under [get_indep Pos MultiEditor]\
          -state [tool_availiable edit 1]

        #window is for class hierarchy and browser
        ${NoteBook} add ctree -createcmd "${this} AddClassHierarchy\
          ${NoteBook} ctree" -raisecmd "${this} RaiseClassHierarchy"\
          -image tree_image -label [get_indep String PrefClassAndHierarchy]\
          -under [get_indep Pos MultiClassHierarchy] -state\
          [tool_availiable ctree 1]

        ${NoteBook} add xref -createcmd "${this} AddXReference ${NoteBook}\
          xref" -raisecmd "${this} RaiseXReference" -label [get_indep String\
          MultiXRef] -under [get_indep Pos MultiXRef] -state\
          [tool_availiable xref 1]

        ${NoteBook} add incbr -createcmd "${this} AddInclude ${NoteBook}\
          incbr" -raisecmd "${this} RaiseInclude" -label [get_indep String\
          MultiInclude] -under [get_indep Pos MultiInclude]\
          -state [tool_availiable incbr 1]

        #parser
        ${NoteBook} add parser -createcmd "${this} AddParser ${NoteBook}\
          parser" -raisecmd "${this} RaiseParser" -label [get_indep String\
          Parser] -under [get_indep Pos Parser] -state [tool_availiable parser\
          1]

        #Revision Control System
        ${NoteBook} add rcs \
          -createcmd "${this} AddRcs ${NoteBook} rcs" \
          -raisecmd "${this} RaiseRcs" \
          -label [get_indep String Rcs] \
          -under [get_indep Pos Rcs] \
          -state [tool_availiable rcs ${new_project}]

        ${NoteBook} add others \
          -createcmd "${this} AddOthers ${NoteBook} others" \
          -raisecmd "${this} RaiseOthers" \
          -label [get_indep String Others] \
          -under [get_indep Pos Others] \
          -state [tool_availiable others 1]

        ${NoteBook} add clrfont \
          -createcmd "${this} AddColorAndFont ${NoteBook} clrfont" \
          -raisecmd "${this} RaiseColorAndFont" \
          -label [get_indep String ColorAndFont] \
          -under [get_indep Pos ColorAndFont] \
          -state [tool_Exists clrfont 1]

        sn_motif_buttons $itk_component(hull) bottom 0 \
          [get_indep String ok] \
          [get_indep String Apply] \
          [get_indep String SaveDefault] \
          [get_indep String cancel]

        $itk_component(hull).button_0 config \
          -command " ${this} apply "
        $itk_component(hull).button_1 config \
          -command " ${this} apply 0 " \
          -underline [get_indep Pos Apply]
        $itk_component(hull).button_2 config \
          -command " ${this} save_cb default " \
          -underline [get_indep Pos SaveDefault]
        $itk_component(hull).button_3 config \
          -command " ${this} exitPreferences "

        #bind Return to apply the current window
        bind $itk_component(hull) <Return> "${this} apply"

        #no apply by creating new project
        if {${new_project}} {
            $itk_component(hull).button_1 config -state disabled
        }

        pack ${NoteBook} -fill both -expand 1 -padx 4 -pady 4

        if {${raise} != "proj" || ${widget} != ""} {
            goto ${raise}
        } else {
            goto ${lastPage}
        }

        title [sn_title [get_indep String Preferences] ${new_project}]

        ${this} configure -geometry ${width}x${height}
        ${this} centerOnScreen
        ${this} deiconify
# FIXME: Do we really need this call to take_focus here?
        ${this} take_focus

        #call user defined function
        if {[catch {sn_rc_preferences ${this}} err]} {
            sn_log "error while exec sn_rc_preferences: ${err}"
        }

        global preferences_wait
        set preferences_wait ""
        ::tkwait variable preferences_wait
    }

    destructor {
        global preferences_wait

        if {${preferences_wait} == ""} {
            set preferences_wait cancel
        }
        foreach v [::info globals "${this}-*"] {
            catch {uplevel #0 unset ${v}}
        }
    }

    method exitPreferences {} {
        set lastPage [${NoteBook} raised]
        set preferences_wait cancel
        itcl::delete object ${this}
    }

    #####################################################
    # project preferences
    #####################################################
    method AddProject {nb page} {
        global sn_options
        global tcl_platform

        set ProjectPage [${nb} subwidget ${page}]
        set Project ${ProjectPage}

        set xstep 30

        #project frame
        set prj [tixLabelFrame ${Project}.prj \
          -label [get_indep String Project]]
        ${prj} config -background $sn_options(def,layout-bg)
        set win [${Project}.prj subwidget frame]
        set lbl [${Project}.prj subwidget label]
        pack ${prj} -side top -fill x

        #Read-Only Project
        set sn_options(opt_readonly) $sn_options(readonly)
        set ronly ${win}.ronly
        CheckButton& ${ronly} \
          -labels [list ""] \
          -balloons [list [get_indep String ReadOnly]] \
          -variables sn_options(opt_readonly) \
          -label [get_indep String ReadOnly] \
          -labelunderline [get_indep Pos ReadOnly] \
          -labelwidth ${xstep}
        pack ${ronly} -side top -anchor nw -fill x

        #Scan Project by opening
        set sn_options(opt_def,refresh-project)\
          $sn_options(def,refresh-project)
        set scan ${win}.scan
        CheckButton& ${scan} \
          -labels [list ""] \
          -balloons [list [get_indep String PrefScanProjectINFO]] \
          -variables sn_options(opt_def,refresh-project) \
          -label [get_indep String PrefScanProject] \
          -labelunderline [get_indep Pos PrefScanProject] \
          -labelwidth ${xstep}
        pack ${scan} -side top -anchor nw -fill x

        #database frame
        set db [tixLabelFrame ${Project}.db \
          -label [get_indep String Database]]
        ${db} config -background $sn_options(def,layout-bg)
        set win [${Project}.db subwidget frame]
        pack ${db} -side top -fill x

        if {${new_project}} {
            set state normal
        } else {
            set state disabled
        }

        #database directory
        set sn_options(opt_both,db-directory) $sn_options(both,db-directory)
        set dbdir ${win}.prjname
        LabelEntryButton& ${dbdir} \
          -text [get_indep String PafWdDir] \
          -underline [get_indep Pos PafWdDir] \
          -command " ${this} choose_db_dir ${dbdir} " \
          -labelwidth ${xstep} -anchor nw \
          -variable sn_options(opt_both,db-directory) \
          -native 1 \
          -buttonballoon [get_indep String ChooseINFO] \
          -state ${state}
        pack ${dbdir} -side top -anchor nw -fill x

        #database permissions
        set sn_options(opt_both,user-read) $sn_options(both,user-read)
        set sn_options(opt_both,user-write) $sn_options(both,user-write)
        set sn_options(opt_both,group-read) $sn_options(both,group-read)
        set sn_options(opt_both,group-write) $sn_options(both,group-write)
        set sn_options(opt_both,others-read) $sn_options(both,others-read)
        set sn_options(opt_both,others-write) $sn_options(both,others-write)

        foreach var {FilePermUser FilePermGroup FilePermOther} {
            lappend lbls "[get_indep String FilePermRead] [get_indep String\
              ${var}]"
            lappend lbls "[get_indep String FilePermWrite] [get_indep String\
              ${var}]"
        }

        set vars [list sn_options(opt_both,user-read)\
          sn_options(opt_both,user-write) sn_options(opt_both,group-read)\
          sn_options(opt_both,group-write) sn_options(opt_both,others-read)\
          sn_options(opt_both,others-write)]
        set balloons [list [get_indep String OwnerRead] [get_indep String\
          OwnerWrite] [get_indep String GroupRead] [get_indep String\
          GroupWrite] [get_indep String OthersRead] [get_indep String\
          OthersWrite]]
        set perms ${win}.perms
        CheckButton& ${perms} \
          -labels ${lbls} \
          -balloons ${balloons} \
          -variables ${vars} \
          -label [get_indep String Permissions] \
          -labelunderline [get_indep Pos Permissions] \
          -labelwidth ${xstep}
        pack ${perms} -side top -anchor nw

        #comment database
        set sn_options(opt_both,create-comment-db)\
          $sn_options(both,create-comment-db)
        set commentdb ${win}.commentdb
        CheckButton& ${commentdb} \
          -labels [list ""] \
          -values [list {-r ""}] \
          -variables sn_options(opt_both,create-comment-db) \
          -label [get_indep String CommentDB] \
          -labelunderline [get_indep Pos CommentDB] \
          -labelwidth ${xstep} \
          -balloons [list [get_indep String CommentDBINFO]]
        pack ${commentdb} -side top -anchor nw -fill x

        #database cache size is always editable
        set cache_state "normal"

        # database cache size
        set sn_options(opt_def,db_cachesize) $sn_options(def,db_cachesize)
        set dbcache ${win}.dbcache

        Entry& ${dbcache} \
          -width 2 \
          -labelwidth ${xstep} \
          -label [get_indep String CacheSize] \
          -underline [get_indep Pos CacheSize] \
          -textvariable sn_options(opt_def,db_cachesize) \
	  -state ${cache_state} -filter natural

        pack ${dbcache} -side top -anchor nw -fill x
        pack [label ${dbcache}.dbbytes -text [get_indep String KBytes]]\
          -side left -fill y

        #xref database cache size
        set sn_options(opt_def,xref-db-cachesize)\
          $sn_options(def,xref-db-cachesize)
        set dbxcache ${win}.dbxcache
        Entry& ${dbxcache} \
          -width 2 \
          -labelwidth ${xstep} \
          -label [get_indep String xrefCacheSize] \
          -underline [get_indep Pos xrefCacheSize] \
          -textvariable sn_options(opt_def,xref-db-cachesize) \
          -state ${cache_state} -filter natural

        pack ${dbxcache} -side top -anchor nw -fill x
        pack [label ${dbxcache}.dbbytes \
          -text [get_indep String KBytes]] \
          -side left -fill y

        #Window options
        set fr [tixLabelFrame ${Project}.projfr \
          -label [get_indep String Window]]
        ${fr} config -background $sn_options(def,layout-bg)
        set win [${Project}.projfr subwidget frame]
        pack ${fr} -side top -fill x

        #Split Windows
        set sn_options(opt_def,window-alighment)\
          $sn_options(def,window-alighment)
        set aligns [list [get_indep String AlighmentHorizontal]\
          [get_indep String AlighmentVertical]]
        Radio& ${win}.aligh \
          -variable sn_options(opt_def,window-alighment) \
          -labels ${aligns} \
          -contents {horizontal vertical} \
          -label [get_indep String SplitWindows] \
          -labelunderline [get_indep Pos SplitWindows] \
          -labelwidth ${xstep}
        pack ${win}.aligh -fill x -expand y -side top -padx ${padx}\
          -pady ${pady}

        #New Windows
        set sn_options(opt_def,reuse-window) $sn_options(def,reuse-window)
        set sn_options(opt_def,window-switchable)\
          $sn_options(def,window-switchable)
        CheckButton& ${win}.reuse \
          -label [get_indep String CreateNewWindow] \
          -labelunderline [get_indep Pos CreateNewWindow] \
          -labelwidth ${xstep} \
          -labels [list \
                    [get_indep String Reusable] \
                    [get_indep String Switchable] \
                  ] \
          -balloons [list \
                      [get_indep String NewReusableINFO] \
                      [get_indep String NewSwitchableINFO] \
                    ] \
          -variables [list \
                       sn_options(opt_def,reuse-window) \
                       sn_options(opt_def,window-switchable)]
        pack ${win}.reuse -side top -fill x -expand y -padx ${padx}\
          -pady ${pady}

        #window size
        set sn_options(opt_def,window-size) $sn_options(def,window-size)
        set winsize ${win}.winsize

        Entry& ${winsize} \
          -width 3 \
          -labelwidth ${xstep} \
          -label [get_indep String WindowSize] \
          -underline [get_indep Pos WindowSize] \
          -textvariable sn_options(opt_def,window-size) \
          -balloon [get_indep String WindowSizeINFO] \
          -expand n \
	  -filter natural

        pack ${winsize} -side top -anchor nw -fill x
        label ${winsize}.procent -text "% [get_indep String OfScreenSize]"
        pack ${winsize}.procent -side left -anchor nw

        # Internationalization

        set fr [tixLabelFrame ${Project}.intl -label [get_indep String\
          Internationalization]]
        ${fr} config -background $sn_options(def,layout-bg)
        set intlframe [${Project}.intl subwidget frame]
        set sn_options(opt_def,encoding) $sn_options(def,encoding)
        Combo& ${intlframe}.encodings \
          -label [get_indep String Encoding] \
          -labelwidth ${xstep} \
          -underline [get_indep Pos Encoding] \
          -contents [encoding names] \
          -entryvariable sn_options(opt_def,encoding)
        ${intlframe}.encodings selecttext $sn_options(def,encoding)
        pack ${fr} -side top -fill x
        pack ${intlframe}.encodings -side top -anchor w -padx 4 -pady 4

        lappend AvailTools ${Project}
    }
    method RaiseProject {} {
        #set lastPage proj
    }
    method choose_db_dir_cb {cls dir} {
        ${cls} config -value ${dir}
    }
    method choose_db_dir {cls} {
        Editor&::DirDialog ${this} \
          -title [get_indep String Open] \
          -script "${this} choose_db_dir_cb ${cls}" \
          -prefix choose_db_dir
    }

    #####################################################
    # editor preferences
    #####################################################
    method AddEditor {nb page} {
        global sn_options

        set EditorPage [${nb} subwidget ${page}]
        set Editor ${EditorPage}

        #Format
        set frmt [tixLabelFrame ${Editor}.frm \
          -label [get_indep String Format]]
        ${frmt} config -background $sn_options(def,layout-bg)
        set win [${Editor}.frm subwidget frame]
        pack ${frmt} -side top -anchor c -fill x

        #tab stop
        set sn_options(opt_def,edit-tabstop) $sn_options(def,edit-tabstop)
        set tabstop ${win}.tabstop

        Entry& ${tabstop} \
          -width 2 \
          -labelwidth 35 \
          -label [get_indep String PrefTabStop] \
          -underline [get_indep Pos PrefTabStop] \
          -textvariable sn_options(opt_def,edit-tabstop) \
	  -filter natural

        pack ${tabstop} -side top -anchor nw

        #Auto Indent delay
        set sn_options(opt_def,edit-indentwidth)\
          $sn_options(def,edit-indentwidth)
        set indent ${win}.indent

        Entry& ${indent} \
          -width 2 \
          -labelwidth 35 \
          -label [get_indep String AutoIndentWidth] \
          -underline [get_indep Pos AutoIndentWidth] \
          -textvariable sn_options(opt_def,edit-indentwidth) \
	  -filter natural

        pack ${indent} -side top -anchor nw

        #wrap
        set sn_options(opt_def,edit-wrap) $sn_options(def,edit-wrap)
        set wrap ${win}.wrap
        set lbls [list [get_indep String None] [get_indep String Char]\
          [get_indep String Word]]
        Radio& ${wrap} \
          -labelwidth 35 \
          -variable sn_options(opt_def,edit-wrap) \
          -labels ${lbls} \
          -contents {none char word} \
          -label [get_indep String Wrap] \
          -labelunderline [get_indep Pos Wrap]
        pack ${wrap} -side top -expand y -fill x

        #Work
        set work [tixLabelFrame ${Editor}.work \
          -label [get_indep String Work]]
        ${work} config -background $sn_options(def,layout-bg)
        set win [${Editor}.work subwidget frame]
        pack ${work} -side top -anchor c -fill x

        #create .bak files
        set sn_options(opt_def,edit-create-bak)\
          $sn_options(def,edit-create-bak)
        set bak ${win}.bak
        CheckButton& ${bak} \
          -labels [list ""] \
          -variables sn_options(opt_def,edit-create-bak) \
          -label [get_indep String EditCreateBackFiles] \
          -labelunderline [get_indep Pos EditCreateBackFiles] \
          -labelwidth 35
        pack ${bak} -side top -anchor nw

        #output file translation
        set ftrans ${win}.ftrans
        set sn_options(opt_def,edit-file-translation)\
          $sn_options(def,edit-file-translation)
        Radio& ${ftrans} \
          -labelwidth 35 \
          -variable sn_options(opt_def,edit-file-translation) \
          -labels [list \
                    [get_indep String KeepLF] \
                    [get_indep String AutoLF] \
                    [get_indep String UNIXLF] \
                    [get_indep String WindowsCRLF] \
                    [get_indep String MacintoshCR] \
                  ] \
          -contents {keep auto lf crlf cr} \
          -balloons [list \
                      [get_indep String EditorFileTranslationKeepINFO] \
                      [get_indep String EditorFileTranslationAutoINFO] \
                      [get_indep String EditorFileTranslationUnixINFO] \
                      [get_indep String EditorFileTranslationWindowsINFO] \
                      [get_indep String EditorFileTranslationCRINFO] \
                    ] \
          -label [get_indep String FileTrans] \
          -labelunderline [get_indep Pos FileTrans]
        pack ${ftrans} -side top -expand y -fill x

        #bracket delay
        set sn_options(opt_def,edit-bracket-delay)\
          $sn_options(def,edit-bracket-delay)
        set brack ${win}.bracket

        Entry& ${brack} \
          -width 4 \
          -labelwidth 35 \
          -label [get_indep String EditBracketMatchDelay] \
          -underline [get_indep Pos EditBracketMatchDelay] \
          -textvariable sn_options(opt_def,edit-bracket-delay) \
	  -filter natural

        pack ${brack} -side top -anchor nw

        #right mouse scroll/edit menu
        set rmouse ${win}.rmouse
        set sn_options(opt_def,edit-rightmouse-action)\
          $sn_options(def,edit-rightmouse-action)
        Radio& ${rmouse} \
          -labelwidth 35 \
          -variable sn_options(opt_def,edit-rightmouse-action) \
          -labels [list \
                    [get_indep String EditM3Menu] \
                    [get_indep String EditM3Scroll] \
                  ] \
          -contents {menu scroll} \
          -label [get_indep String EditM3Sup] \
          -labelunderline [get_indep Pos EditM3Sup]
        pack ${rmouse} -side top -expand y -fill x

        #Convert Inserted Tabs Into Spaces
        set sn_options(opt_def,edit-tab-inserts-spaces)\
          $sn_options(def,edit-tab-inserts-spaces)
        set tabspaces ${win}.tabspaces
        CheckButton& ${tabspaces} \
          -labels [list ""]\
          -variables sn_options(opt_def,edit-tab-inserts-spaces) \
          -label [get_indep String EditorTranslateTabs] \
          -labelunderline [get_indep Pos EditorTranslateTabs] \
          -labelwidth 35 \
          -balloons [list [get_indep String EditorTranslateTabsINFO]]
        pack ${tabspaces} -side top -anchor nw

        #more toolbar buttons
        set sn_options(opt_def,edit-more-buttons) $sn_options(def,edit-more-buttons)
        set morebtns ${win}.morebtns
        CheckButton& ${morebtns} \
          -labels [list ""] \
          -variables sn_options(opt_def,edit-more-buttons) \
          -label [get_indep String MoreEditorToolbarButtons] \
          -labelunderline [get_indep Pos MoreEditorToolbarButtons] \
          -labelwidth 35 \
          -balloons [list [get_indep String MoreEditorToolbarButtonsINFO]]
        pack ${morebtns} -side top -anchor nw

        # External editor settings
        set sn_options(opt_def,edit-external-always) $sn_options(def,edit-external-always)
	set extedit_always ${win}.extedit_always
	CheckButton& ${extedit_always} \
          -labels [list ""] \
          -variables sn_options(opt_def,edit-external-always) \
          -label [get_indep String ExternalEditorAlways] \
          -labelunderline [get_indep Pos ExternalEditorAlways] \
          -labelwidth 35 \
          -balloons [list [get_indep String ExternalEditorAlways]]
        pack ${extedit_always} -side top -anchor nw
	  
        set sn_options(opt_def,edit-external-editor) $sn_options(def,edit-external-editor)
        set extedit ${win}.extedit
        LabelEntryButton& ${extedit} \
          -text [get_indep String ExternalEditor] \
          -underline [get_indep Pos ExternalEditor] \
          -labelwidth 35 \
          -anchor nw \
          -variable sn_options(opt_def,edit-external-editor) \
          -native 1 \
          -extensions $sn_options(executable_ext) \
          -defaultextension $sn_options(executable_defaultext)
        pack ${extedit} -side top -anchor nw -fill x

        lappend AvailTools ${Editor}
    }
    method RaiseEditor {} {
        #set lastPage edit
    }

    #####################################################
    # Class hierarchy
    #####################################################
    method AddClassHierarchy {nb page} {
        global sn_options

        set ClassHierarchyPage [${nb} subwidget ${page}]
        set ClassHierarchy ${ClassHierarchyPage}

        #Class browser options
        set class [tixLabelFrame ${ClassHierarchy}.class \
          -label [get_indep String Class]]
        ${class} config -background $sn_options(def,layout-bg)
        set win [${ClassHierarchy}.class subwidget frame]
        pack ${class} -side top -anchor c -fill x

        #goto Def. or Impl. by selecting a method
        set sn_options(opt_def,class-goto-imp) $sn_options(def,class-goto-imp)
        set lbls [list [get_indep String Definition] [get_indep String\
          Implementation]]
        set defimp ${win}.defimp
        Radio& ${defimp} \
          -labelwidth 35 \
          -variable sn_options(opt_def,class-goto-imp) \
          -labels ${lbls} \
          -contents {def imp} \
          -label [get_indep String GotoDefImp] \
          -labelunderline [get_indep Pos GotoDefImp]
        pack ${defimp} -side top -expand y -fill x

        #vertical/horizontal
        set sn_options(opt_def,class-orientation)\
          $sn_options(def,class-orientation)
        set lbls [list [get_indep String AlighmentHorizontal]\
          [get_indep String AlighmentVertical]]
        set classorient ${win}.classorient
        Radio& ${classorient} \
          -labelwidth 35 \
          -variable sn_options(opt_def,class-orientation) \
          -labels ${lbls} \
          -contents {horizontal vertical} \
          -label [get_indep String Orientation] \
          -labelunderline [get_indep Pos Orientation]
        pack ${classorient} -side top -expand y -fill x

        #members order
        set sn_options(opt_def,members-order) $sn_options(def,members-order)
        set lbls [list [get_indep String First] [get_indep String Second]]
        set memord ${win}.memord
        Radio& ${memord} \
          -labelwidth 35 \
          -variable sn_options(opt_def,members-order) \
          -labels ${lbls} \
          -contents {first second} \
          -label [get_indep String MembersOrder] \
          -labelunderline [get_indep Pos MembersOrder]
        pack ${memord} -side top -expand y -fill x

        #Layout
        set layout [tixLabelFrame ${ClassHierarchy}.layout \
          -label [get_indep String HierarchyLayout]]
        ${layout} config -background $sn_options(def,layout-bg)
        set win [${ClassHierarchy}.layout subwidget frame]
        pack ${layout} -side top -anchor c -fill x

        #Display order
        set order ${win}.order
        set sn_options(opt_def,ctree-view-order)\
          $sn_options(def,ctree-view-order)
        Radio& ${order} \
          -labelwidth 35 \
          -variable sn_options(opt_def,ctree-view-order) \
          -labels [list \
                    [get_indep String LeftRight] \
                    [get_indep String Topdown] \
                  ] \
          -contents {0 1} \
          -label [get_indep String PrefDispOrder] \
          -labelunderline [get_indep Pos PrefDispOrder]
        pack ${order} -side top -expand y -fill x -padx ${padx} -pady ${pady}

        #Display format
        set frmt ${win}.frmt
        set sn_options(opt_def,ctree-layout) $sn_options(def,ctree-layout)
        Radio& ${frmt} \
          -labelwidth 35 \
          -variable sn_options(opt_def,ctree-layout) \
          -labels [list\
                    [get_indep String Tree] \
                    [get_indep String ISI] \
                  ] \
          -contents {isi tree} \
          -label [get_indep String PrefDispLayout] \
          -labelunderline [get_indep Pos PrefDispLayout]
        pack ${frmt} -side top -expand y -fill x -padx ${padx} -pady ${pady}

        #space vertical
        set sn_options(opt_def,ctree-vertspace)\
          $sn_options(def,ctree-vertspace)
        set svert ${win}.svert

        Entry& ${svert} \
          -width 3 \
          -labelwidth 35 \
          -label [get_indep String SpaceVertical] \
          -underline [get_indep Pos SpaceVertical] \
          -textvariable sn_options(opt_def,ctree-vertspace) \
	  -filter natural

        pack ${svert} -side top -anchor nw

        #space horizontal
        set sn_options(opt_def,ctree-horizspace)\
          $sn_options(def,ctree-horizspace)
        set shoriz ${win}.shoriz

        Entry& ${shoriz} \
          -width 3 \
          -labelwidth 35 \
          -label [get_indep String SpaceHorizontal] \
          -underline [get_indep Pos SpaceHorizontal] \
          -textvariable sn_options(opt_def,ctree-horizspace) \
	  -filter natural

        pack ${shoriz} -side top -anchor nw

        lappend AvailTools ${ClassHierarchy}
    }
    method RaiseClassHierarchy {} {
        #set lastPage ctree
    }

    #####################################################
    # X reference
    #####################################################
    method AddXReference {nb page} {
        global sn_options

        set XReferencePage [${nb} subwidget ${page}]
        set XReference ${XReferencePage}

        #XRef informations
        set xproc [tixLabelFrame ${XReference}.xproc \
          -label [get_indep String CrossReferencing]]
        ${xproc} config -background $sn_options(def,layout-bg)
        set win [${XReference}.xproc subwidget frame]
        pack ${xproc} -side top -anchor c -fill x

        #generate xref
        set sn_options(opt_both,xref-create) $sn_options(both,xref-create)
        set genxref ${win}.genxref
        CheckButton& ${genxref} \
          -balloons [list [get_indep String GenerateXRefINFO]] \
          -labels [list ""] \
          -values [list {-x ""}] \
          -variables sn_options(opt_both,xref-create) \
          -label [get_indep String GenerateXRef] \
          -labelunderline [get_indep Pos GenerateXRef] \
          -labelwidth 35 \
          -command " ${this} XRef_Enable ${win}.genlocal ${win}.xrefbell "
        pack ${genxref} -side top -anchor nw

        #Generate references to local variables
        set sn_options(opt_both,xref-locals) $sn_options(both,xref-locals)
        set genlocal ${win}.genlocal
        CheckButton& ${genlocal} \
          -labels [list ""] \
          -values [list {-l ""}] \
          -variables sn_options(opt_both,xref-locals) \
          -label [get_indep String XRefLocalVars] \
          -labelunderline [get_indep Pos XRefLocalVars] \
          -labelwidth 35
        pack ${genlocal} -side top -anchor nw

        #Bell after terminated XRef
        set sn_options(opt_def,xref-bell) $sn_options(def,xref-bell)
        set xrefbell ${win}.xrefbell
        CheckButton& ${xrefbell} \
          -variables sn_options(opt_def,xref-bell) \
          -labels [list ""] \
          -balloons [list [get_indep String XRefBellINFO]] \
          -label [get_indep String XRefBell] \
          -labelunderline [get_indep Pos XRefBell] \
          -labelwidth 35
        pack ${xrefbell} -side top -anchor nw

        XRef_Enable ${win}.genlocal ${win}.xrefbell

        #Layout
        set layout [tixLabelFrame ${XReference}.layout -label\
          [get_indep String Layout]]
        ${layout} config -background $sn_options(def,layout-bg)
        set win [${XReference}.layout subwidget frame]
        pack ${layout} -side top -anchor c -fill x

        #Accept parameters
        set sn_options(opt_both,xref-accept-param)\
          $sn_options(both,xref-accept-param)
        set aparam ${win}.aparam
        CheckButton& ${aparam} \
          -labels [list ""] \
          -variables sn_options(opt_both,xref-accept-param) \
          -label [get_indep String CrossAcceptParam] \
          -labelunderline [get_indep Pos CrossAcceptParam] \
          -labelwidth 35
        pack ${aparam} -side top -anchor nw

        #Accept static flag
        set sn_options(opt_both,xref-accept-static)\
          $sn_options(both,xref-accept-static)
        set astatic ${win}.astatic
        CheckButton& ${astatic} \
          -labels [list ""] \
          -variables sn_options(opt_both,xref-accept-static) \
          -label [get_indep String CrossAcceptStatic] \
          -labelunderline [get_indep Pos CrossAcceptStatic] \
          -labelwidth 35
        pack ${astatic} -side top -anchor nw

        #Display functions parameters
        set sn_options(opt_both,xref-disp-param)\
          $sn_options(both,xref-disp-param)
        set dspfunc ${win}.dspfunc
        CheckButton& ${dspfunc} \
          -labels [list ""] \
          -variables sn_options(opt_both,xref-disp-param) \
          -label [get_indep String CrossDispParam] \
          -labelunderline [get_indep Pos CrossDispParam] \
          -labelwidth 35
        pack ${dspfunc} -side top -anchor nw

        #Display boxes around the symbols
        set sn_options(opt_both,xref-draw-rect)\
          $sn_options(both,xref-draw-rect)
        set dspbox ${win}.dspbox
        CheckButton& ${dspbox} \
          -labels [list ""] \
          -variables sn_options(opt_both,xref-draw-rect) \
          -label [get_indep String CrossBoxes] \
          -labelunderline [get_indep Pos CrossBoxes] \
          -labelwidth 35
        pack ${dspbox} -side top -anchor nw

        #Display order
        set order ${win}.order
        set sn_options(opt_def,xref-disp-order)\
          $sn_options(def,xref-disp-order)
        Radio& ${order} \
          -labelwidth 35\
          -variable sn_options(opt_def,xref-disp-order) \
          -labels [list \
                    [get_indep String LeftRight] \
                    [get_indep String Topdown] \
                  ]\
          -contents {0 1} \
          -label [get_indep String PrefDispOrder] \
          -labelunderline [get_indep Pos PrefDispOrder]
        pack ${order} -side top -expand y -fill x -padx ${padx} -pady ${pady}

        #Display format
        set frmt ${win}.frmt
        set sn_options(opt_def,xref-layout) $sn_options(def,xref-layout)
        Radio& ${frmt} \
          -labelwidth 35 \
          -variable sn_options(opt_def,xref-layout) \
          -labels [list \
                    [get_indep String Tree] \
                    [get_indep String ISI] \
                   ] \
          -contents {isi tree} \
          -label [get_indep String PrefDispLayout] \
          -labelunderline [get_indep Pos PrefDispLayout]
        pack ${frmt} -side top -expand y -fill x -padx ${padx} -pady ${pady}

        #space vertical
        set sn_options(opt_def,xref-vertspace) $sn_options(def,xref-vertspace)
        set svert ${win}.svert

        Entry& ${svert} \
          -width 3 \
          -labelwidth 35 \
          -label [get_indep String SpaceVertical] \
          -underline [get_indep Pos SpaceVertical] \
          -textvariable sn_options(opt_def,xref-vertspace) \
	  -filter natural

        pack ${svert} -side top -anchor nw

        #space horizontal
        set sn_options(opt_def,xref-horizspace)\
          $sn_options(def,xref-horizspace)
        set shoriz ${win}.shoriz

        Entry& ${shoriz} \
          -width 3 \
          -labelwidth 35 \
          -label [get_indep String SpaceHorizontal] \
          -underline [get_indep Pos SpaceHorizontal] \
          -textvariable sn_options(opt_def,xref-horizspace) \
	  -filter natural

        pack ${shoriz} -side top -anchor nw

        lappend AvailTools ${XReference}
    }
    method RaiseXReference {} {
    }
    method XRef_Enable {genlocal xbell} {
        global sn_options
        if {$sn_options(opt_both,xref-create) != ""} {
            set state normal
        } else {
            set state disabled
            set sn_options(opt_both,xref-locals) ""
        }
        ${genlocal}.check-0 config -state ${state}
        ${xbell}.check-0 config -state ${state}
    }

    #####################################################
    # Include
    #####################################################
    method AddInclude {nb page} {
        global sn_options

        set IncludePage [${nb} subwidget ${page}]
        set Include ${IncludePage}

        #Layout
        set layout [tixLabelFrame ${Include}.layout \
          -label [get_indep String Layout]]
        ${layout} config -background $sn_options(def,layout-bg)
        set win [${Include}.layout subwidget frame]
        pack ${layout} -side top -anchor c -fill x

        #Display order
        set order ${win}.order
        set sn_options(opt_def,include-disporder)\
          $sn_options(def,include-disporder)
        Radio& ${order} \
          -labelwidth 35 \
          -variable sn_options(opt_def,include-disporder) \
          -labels [list \
                    [get_indep String LeftRight] \
                    [get_indep String Topdown] \
                  ] \
          -contents {0 1} \
          -label [get_indep String PrefDispOrder] \
          -labelunderline [get_indep Pos PrefDispOrder]
        pack ${order} -side top -expand y -fill x -padx ${padx} -pady ${pady}

        #Display format
        set frmt ${win}.frmt
        set sn_options(opt_def,include-layout) $sn_options(def,include-layout)
        Radio& ${frmt} \
          -labelwidth 35 \
          -variable sn_options(opt_def,include-layout) \
          -labels [list \
                    [get_indep String Tree] \
                    [get_indep String ISI] \
                  ] \
          -contents {isi tree} \
          -label [get_indep String PrefDispLayout] \
          -labelunderline [get_indep Pos PrefDispLayout]
        pack ${frmt} -side top -expand y -fill x -padx ${padx} -pady ${pady}

        #space vertical
        set sn_options(opt_def,include-vertspace)\
          $sn_options(def,include-vertspace)
        set svert ${win}.svert

        Entry& ${svert} \
          -width 3 \
          -labelwidth 35 \
          -label [get_indep String SpaceVertical] \
          -underline [get_indep Pos SpaceVertical] \
          -textvariable sn_options(opt_def,include-vertspace) \
	  -filter natural

        pack ${svert} -side top -anchor nw

        #space horizontal
        set sn_options(opt_def,include-horizspace)\
          $sn_options(def,include-horizspace)
        set shoriz ${win}.shoriz

        Entry& ${shoriz} \
          -width 3 \
          -labelwidth 35 \
          -label [get_indep String SpaceHorizontal] \
          -underline [get_indep Pos SpaceHorizontal] \
          -textvariable sn_options(opt_def,include-horizspace) \
	  -filter natural

        pack ${shoriz} -side top -anchor nw

        #Include directories
        set incdir [tixLabelFrame ${Include}.incdir \
          -label [get_indep String PrefInclude]]
        ${incdir} config -background $sn_options(def,layout-bg)
        set win [${Include}.incdir subwidget frame]
        pack ${incdir} -side top -anchor c -fill both -expand y

        #verify if we want to look for included header files
        #Locate Headers is used to control whether or not header files are\
          searched
        #for during parsing.  If this checkbutton is disabled, then the parsers
        #will just write out the literal filename from the source:
        #
        #		#include <stdlib.h>
        #
        #will cause just `stdlib.h' to be written to the database.
        #
        #If you enable it, a comprehensive search of all of the directories\
          listed
        #in the include directories text box will occur.  Then the result\
          might be
        #`/usr/include/stdlib.h'.
        #It is added as a quick and easy way of improving performance over NFS
        #file systems.
        set sn_options(opt_def,include-locatefiles)\
          $sn_options(def,include-locatefiles)
        set lookforinc ${win}.lookforinc
        CheckButton& ${lookforinc} \
          -labels [list ""] \
          -variables sn_options(opt_def,include-locatefiles) \
          -label [get_indep String LocateIncludeFiles] \
          -underlines [list [get_indep Pos LocateIncludeFiles]] \
          -balloons [list [get_indep String LocateIncludeFilesINFO]] \
          -labelwidth 35
        pack ${lookforinc} -side top -anchor nw

        set inc_editor ${win}.t
        scrollbar ${win}.x -orient horiz -command " ${inc_editor} xview "
        scrollbar ${win}.y -command " ${inc_editor} yview "
        text ${inc_editor} -wrap none -xscrollcommand "${win}.x set"\
          -yscrollcommand "${win}.y set"
        ${inc_editor} insert end [join $sn_options(include-source-directories)\
          "\n"]
        ${inc_editor} mark set insert 0.0
        ${inc_editor} see 0.0
        pack ${win}.y -side right -fill y
        pack ${win}.x -side bottom -fill x
        pack ${inc_editor} -side left -fill both -expand y

        # Don't dismiss entire dialog when Return key is presed in editor
        bindtags ${inc_editor} {Text all}

        lappend AvailTools ${Include}
    }
    method RaiseInclude {} {
        #set lastPage incbr
    }

    #####################################################
    # Parser
    #####################################################
    method AddParser {nb page} {
        global sn_options
        global Avail_Parsers Parser_Info
        global opt_Parser_Info

        set ParserPage [${nb} subwidget ${page}]
        set Parser ${ParserPage}

        #language extensitions
        #copy parser info
        foreach tt [array names Parser_Info] {
            set opt_Parser_Info(${tt}) $Parser_Info(${tt})
        }
        set ext [tixLabelFrame ${Parser}.ext -label [get_indep String\
          LanguageExt]]
        ${ext} config -background $sn_options(def,layout-bg)
        set win [${Parser}.ext subwidget frame]
        pack ${ext} -fill x -side top -anchor c

        label $win.label -text [get_indep String LanguageExtLanguage]

        # Figure out how many chars wide the combo should be.
        # Default to the first parser in the list.
        set max_width 5
        set default_type ""
        foreach type [lsort -dictionary ${Avail_Parsers}] {
            set width [string length $type]
            if {$width > $max_width} {
                set max_width $width
            }

            if {[string equal $default_type ""]} {
                set default_type $type
            }
        }
        incr max_width 1

        set extension_entry ${win}.ext_entry
        set editor_entry ${win}.edit_entry
        set combo ${win}.combo

        combobox::combobox $combo \
            -editable 0 \
            -width $max_width

        foreach type [lsort -dictionary ${Avail_Parsers}] {
            $combo listinsert end $type
        }

        grid $win.label -row 0 -column 0 -sticky ew
        grid $combo -row 0 -column 1 -sticky ew -pady 5

        label $win.ext_label -text [get_indep String LanguageExtFileExtensions]
        entry $extension_entry

        grid $win.ext_label -row 1 -column 0 -sticky w -pady 5
        grid $extension_entry -row 1 -column 1 -sticky we -columnspan 2

        label $win.edit_label -text [get_indep String ExternalEditor]
        entry $editor_entry

        grid $win.edit_label -row 2 -column 0 -sticky w -pady 5
        grid $editor_entry -row 2 -column 1 -sticky we -columnspan 2

        grid columnconfigure ${win} 2 -weight 1              

        #Macro files
        set macfr [tixLabelFrame ${Parser}.macros -label [get_indep String\
          MacroFiles]]
        ${macfr} config -background $sn_options(def,layout-bg)
        set win [${Parser}.macros subwidget frame]
        pack ${macfr} -side top -anchor c -fill x
        set macros ${win}.edit
        set sn_options(opt_macrofiles) $sn_options(macrofiles)
        button ${win}.add -text [get_indep String Choose] -command " ${this}\
          choose_macrofile ${macros} "
        balloon_bind_info ${win}.add [get_indep String ChooseINFO]
        pack ${win}.add -side right -anchor nw
        pack [scrollbar ${win}.y -command " ${macros} yview "] -side right\
          -fill y
        pack [text ${macros} -wrap word -height 5 -yscrollcommand\
          "${win}.y set"] -side left -fill x -expand y
        bind ${macros} <Tab> {focus [tk_focusNext %W]; break}
        bind ${macros} <Shift-Tab> {focus [tk_focusPrev %W]; break}
        ${macros} insert 0.0 [join $sn_options(opt_macrofiles) \n]
        balloon_bind_info ${macros} [get_indep String AddYourMacroFiles]

        # Set parser type selected callback and invoke it once with
        # the default type to get things in the proper state.

        $combo configure -command \
            [itcl::code parser_type_combo_changed $extension_entry $editor_entry \
                $win.add $macros]

        $combo entryset $default_type

        parser_type_combo_changed $extension_entry $editor_entry \
            $win.add $macros $combo $default_type

        lappend AvailTools ${Parser}
    }
    method RaiseParser {} {
        #set lastPage parser
    }

    # Invoked when a new type is selected in the parser combo box.
    # This will set the textvar used by the extension and editor
    # entries and disable or enable the macro widgets.

    proc parser_type_combo_changed { extension_widget editor_widget
            macro_button_widget macro_text_widget combo_widget type } {
        $extension_widget configure -textvariable opt_Parser_Info(${type},SUF)
        $editor_widget configure -textvariable opt_Parser_Info(${type},EDIT)

        if {[string equal $::opt_Parser_Info(${type},MACRO) ""]} {
            $macro_button_widget configure -state disabled
            $macro_text_widget configure -state disabled
        } else {
            $macro_button_widget configure -state normal
            $macro_text_widget configure -state normal
        }
    }

    #add macro file, if not availiable
    method choose_macrofile_cb {cls file} {
        set files [string trim [${cls} get 0.0 end]]
        foreach f [split ${files} \n] {
            if {${f} == ${file}} {
                bell
                return
            }
        }
        if {${files} == ""} {
            ${cls} insert 0.0 ${file}
        } else {
            ${cls} insert end "\n${file}"
        }
    }
    method choose_macrofile {cls} {
        Editor&::FileDialog ${this} -title [get_indep String Open]\
          -script "${this} choose_macrofile_cb ${cls}" -defaultextension ".h"
    }

# FIXME: code to manage parsing of a project should not be in the prefs !!!
    proc Reparse {{ask "ask"} {reopen "reopen"}} {
        global sn_options

        if {${ask} == "ask"} {
            set answer [tk_dialog auto [get_indep String Reparse]\
              [get_indep String DoYouWantToReparse] question_image 0\
              [get_indep String Ok] [get_indep String Cancel]]
            if {${answer} != 0} {
                return "break"
            }
        }

        sn_log "reparse project <$sn_options(sys,project-file)>"

        #make sure that we stay in the correct directory
        catch {cd $sn_options(sys,project-dir)}

        if {[sn_processes_running 1]} {
            sn_error_dialog [get_indep String BackProcsRun]
            return "break"
        }

        #delete database files
        set ret [sn_db_delete_files $sn_options(sys,project-dir)\
          $sn_options(db_files_prefix) 0]

        #databases couldn't be removed
        if {${ret} != 1} {
            set answer [tk_dialog auto [get_indep String Delete]\
              [get_indep String DatabaseFilesCouldNotBeDeleted] question_image\
              0 [get_indep String Continue] [get_indep String Cancel]]
            if {${answer} != 0} {
                return "break"
            }
            set ret 1
        }

        if {[info commands paf_db_exclude] != ""} {
            paf_db_exclude close
        }

        set_project_dir_files

        #reparse the project now
        set ret [sn_parse_uptodate]

        hide_loading_message

        if {${ret} == 0} {
            sn_error_dialog [get_indep String UnableToReparse]
            set ret "break"
        } else {
            set ret "success"
        }
        return ${ret}
    }

    #####################################################
    # Rcs
    #####################################################
    method AddRcs {nb page} {
        global sn_options

        set RcsPage [${nb} subwidget ${page}]
        set Rcs ${RcsPage}

        #Revision Control System
        set rcsys [tixLabelFrame ${Rcs}.rcsys -label [get_indep String PrefRcs]]
        ${rcsys} config -background $sn_options(def,layout-bg)
        set win [${Rcs}.rcsys subwidget frame]
        pack ${rcsys} -fill x -side top -anchor c

        #choose revision control
        set sn_options(opt_both,rcs-type) $sn_options(both,rcs-type)
        set sys ${win}.sys
        set lbls ""
        set values ""
        foreach rcs $sn_options(sys,supported-vcsystems) {
            lappend lbls [lindex ${rcs} 0]
            lappend values [lindex ${rcs} 1]
        }
        Radio& ${sys} -variable sn_options(opt_both,rcs-type) -labels ${lbls}\
          -contents ${values} -label ""
        pack ${sys} -side top -expand y -fill x

        # Ignore directories
        set ignoredir [tixLabelFrame ${Rcs}.ignoredir \
          -label [get_indep String IgnoredDirectories]]
        ${ignoredir} config -background $sn_options(def,layout-bg)
        set win [${Rcs}.ignoredir subwidget frame]
        pack ${ignoredir} -side top -anchor c -fill both
        set ign_editor ${win}.t
        scrollbar ${win}.y -command " ${ign_editor} yview "
        text ${ign_editor} -height 6 -wrap none -yscrollcommand "${win}.y set"
        #		bindtags $ign_editor [list $ign_editor Text all]
        ${ign_editor} insert end [join $sn_options(def,ignored-directories)\
          "\n"]
        ${ign_editor} mark set insert 0.0
        ${ign_editor} see 0.0
        pack ${win}.y -side right -fill y
        pack ${ign_editor} -side left -fill both -expand y

        lappend AvailTools ${Rcs}
    }
    method RaiseRcs {} {
        #set lastPage rcs
    }

    #####################################################
    # Others
    #####################################################
    method AddOthers {nb page} {
        global sn_options tcl_platform

        set OthersPage [${nb} subwidget ${page}]
        set Others ${OthersPage}

        #Make settings
        set make [tixLabelFrame ${Others}.make -label [get_indep String\
          PrefMake]]
        ${make} config -background $sn_options(def,layout-bg)
        set win [${Others}.make subwidget frame]
        pack ${make} -fill x -side top -anchor c

        #Make command
        set sn_options(opt_both,make-command) $sn_options(both,make-command)
        set makecmd ${win}.makecmd
        LabelEntryButton& ${makecmd} -text "" -anchor nw\
          -variable sn_options(opt_both,make-command) -native 1\
          -extensions $sn_options(executable_ext)\
          -defaultextension $sn_options(executable_defaultext)
        pack ${makecmd} -side top -anchor nw -fill x

        # Windows will use fault HTML viewer.
        if {$tcl_platform(platform)!="windows"} {
            #Help settings
            set help [tixLabelFrame ${Others}.help -label [get_indep String\
              PafHtmlView]]
            ${help} config -background $sn_options(def,layout-bg)
            set win [${Others}.help subwidget frame]
            pack ${help} -fill x -side top -anchor c

            #HTML viewer
            set sn_options(opt_def,html-viewer) $sn_options(def,html-viewer)
            set html ${win}.html
            LabelEntryButton& ${html} -text "" -anchor nw\
              -variable sn_options(opt_def,html-viewer)\
              -extensions $sn_options(executable_ext)\
              -defaultextension $sn_options(executable_defaultext)
            pack ${html} -side top -anchor nw -fill x
        }

        #no printer commands on windows
        if {$tcl_platform(platform) != "windows"} {
            #Printer
            set printer [tixLabelFrame ${Others}.printer -label\
              [get_indep String PrefPrinter]]
            ${printer} config -background $sn_options(def,layout-bg)
            set win [${Others}.printer subwidget frame]
            pack ${printer} -fill x -side top -anchor c

            #Ascii Print command
            set sn_options(opt_def,ascii-print-command)\
              $sn_options(def,ascii-print-command)
            set prncmd ${win}.prncmd
            LabelEntryButton& ${prncmd} -text [get_indep String\
              PrefAsciiPrintCommand] -labelwidth 25 -anchor nw\
              -variable sn_options(opt_def,ascii-print-command)\
              -extensions $sn_options(executable_ext)\
              -defaultextension $sn_options(executable_defaultext)
            pack ${prncmd} -side top -anchor nw -fill x

            #Print command
            set sn_options(opt_def,print-command)\
              $sn_options(def,print-command)
            set pcmd ${win}.pcmd
            LabelEntryButton& ${pcmd} -text [get_indep String\
              PrefPrintCommand] -labelwidth 25 -anchor nw\
              -variable sn_options(opt_def,print-command)\
              -extensions $sn_options(executable_ext)\
              -defaultextension $sn_options(executable_defaultext)
            pack ${pcmd} -side top -anchor nw -fill x
        }

        set debug [tixLabelFrame ${Others}.debug -label [get_indep String\
          DebuggerCommand]]
        ${debug} config -background $sn_options(def,layout-bg)
        set win [${Others}.debug subwidget frame]
        pack ${debug} -fill x -side top -anchor c

        #Debugger command
        set sn_options(opt_def,gdb-command) $sn_options(def,gdb-command)
        set dbgcmd ${win}.dbgcmd
        LabelEntryButton& ${dbgcmd} -text "" -labelwidth 0 -anchor nw\
          -variable sn_options(opt_def,gdb-command) -native 1\
          -extensions $sn_options(executable_ext)\
          -defaultextension $sn_options(executable_defaultext)
        pack ${dbgcmd} -side top -fill x

        #Retriever
        set retr [tixLabelFrame ${Others}.retr -label [get_indep String\
          PrefRetriever]]
        ${retr} config -background $sn_options(def,layout-bg)
        set win [${Others}.retr subwidget frame]
        pack ${retr} -fill x -side top -anchor c

        set sn_options(opt_donot_display) $sn_options(donot_display)
        set retr ${win}.retr
        CheckButton& ${retr} -labels [list [get_indep String\
          DonotDisplayWarning]] -underlines [list [get_indep Pos\
          DonotDisplayWarning]] -balloons [list [get_indep String\
          DonotDisplayWarningINFO]] -variables sn_options(opt_donot_display)\
          -label ""
        pack ${retr} -side top -anchor nw -fill x

        lappend AvailTools ${Others}
    }
    method RaiseOthers {} {
        #set lastPage others
    }

    #####################################################
    # Colors and Fonts
    #####################################################
    method AddColorAndFont {nb page} {
        global sn_options

        set ColorAndFontPage [${nb} subwidget ${page}]
        set ColorAndFont ${ColorAndFontPage}

        #Don't hide any settings on windows, even if the standard
        #options are used!!


# FIXME : I have never seen code more in need of a reformat, we should import what was on elix!

        #build schemes struct, that can be interpreted
        #from the color manager

        set schemes [list \
          [list [get_indep String Default] \
            [list \
              [list [get_indep String General] \
                [list \
                  [list [get_indep String Standard] \
                    [list def,default-font opt_def,default-font def,default-fg \
                      opt_def,default-fg def,default-bg opt_def,default-bg]] \
                  [list [get_indep String Layout] \
                    [list def,layout-font opt_def,layout-font def,layout-fg \
                      opt_def,layout-fg def,layout-bg opt_def,layout-bg]] \
                  [list [get_indep String Highlighting] \
                    [list "" "" def,highlight-fg opt_def,highlight-fg \
                      def,highlight-bg opt_def,highlight-bg]] \
                  [list [get_indep String BoldFont] \
                    [list def,bold-font opt_def,bold-font "" "" "" ""]] \
                  [list [get_indep String Marked] \
                    [list "" "" def,select-fg opt_def,select-fg def,select-bg \
                      opt_def,select-bg]] \
                ] \
              ] \
            ] \
            [list \
              [list [get_indep String Editor] \
                [list \
                  [list [get_indep String Text] \
                    [list def,edit-font opt_def,edit-font def,edit-fg \
                      opt_def,edit-fg def,edit-bg opt_def,edit-bg]] \
                  [list "[get_indep String FunctionsNoKey] (fu)" \
                    [list "" "" def,color_fu opt_def,color_fu "" ""]] \
                  [list "[get_indep String FunctionsDecNoKey] (fd)" \
                    [list "" "" def,color_fd opt_def,color_fd "" ""]] \
                  [list "[get_indep String SubroutinesNoKey] (su)" \
                    [list "" "" def,color_su opt_def,color_su "" ""]] \
                  [list [get_indep String Strings] \
                    [list "" "" def,color_str opt_def,color_str "" ""]] \
                  [list [get_indep String Comments] \
                    [list "" "" def,color_rem opt_def,color_rem "" ""]] \
                  [list [get_indep String Keywords] \
                    [list "" "" def,color_key opt_def,color_key "" ""]] \
                  [list "[get_indep String ClassesNoKey] (cl)" \
                    [list "" "" def,color_cl opt_def,color_cl "" ""]] \
                  [list "[get_indep String Typedefs] (t)" \
                    [list "" "" def,color_t opt_def,color_t "" ""]] \
                  [list "[get_indep String Enums] (e)" \
                    [list "" "" def,color_e opt_def,color_e "" ""]] \
                  [list "[get_indep String EnumConsNoKey] (ec)" \
                    [list "" "" def,color_ec opt_def,color_ec "" ""]] \
                  [list "[get_indep String MethodsNoKey] (md)" \
                    [list "" "" def,color_md opt_def,color_md "" ""]] \
                  [list "[get_indep String FriendsNoKey] (fr)" \
                    [list "" "" def,color_fr opt_def,color_fr "" ""]] \
                  [list "[get_indep String MethodImpsNoKey] (mi)" \
                    [list "" "" def,color_mi opt_def,color_mi "" ""]] \
                  [list "[get_indep String ClassVarsNoKey] (iv)" \
                    [list "" "" def,color_iv opt_def,color_iv "" ""]] \
                  [list "[get_indep String VariablesNoKey] (gv)" \
                    [list "" "" def,color_gv opt_def,color_gv "" ""]] \
                  [list "[get_indep String PafLocVars] (lv)" \
                    [list "" "" def,color_lv opt_def,color_lv "" ""]] \
                  [list "[get_indep String ConstantsNoKey] (con)" \
                    [list "" "" def,color_con opt_def,color_con "" ""]] \
                  [list "[get_indep String CommonsNoKey] (com)" \
                    [list "" "" def,color_com opt_def,color_com "" ""]] \
                  [list "[get_indep String CommonsVarsNoKey] (cov)" \
                    [list "" "" def,color_cov opt_def,color_cov "" ""]] \
                  [list "[get_indep String DefinesNoKey] (ma)" \
                    [list "" "" def,color_ma opt_def,color_ma "" ""]] \
                  [list [get_indep String Brackets] \
                    [list "" "" def,bracketmatch-bg opt_def,bracketmatch-bg "" ""]] \
                ] \
              ] \
            ] \
            [list \
             [list [get_indep String SplitIntoClass] \
               [list \
                 [list [get_indep String Standard] \
                   [list def,ctree-font opt_def,ctree-font "" "" "" ""]] \
                 [list [get_indep String AbstractClass] \
                   [list def,abstract-font opt_def,abstract-font "" "" "" ""]] \
               ] \
              ] \
            ] \
            [list \
              [list [get_indep String HierarchyNoKey] \
                [list \
                  [list [get_indep String Standard] \
                    [list def,class-font opt_def,class-font "" "" "" ""]] \
                  [list [get_indep String Public] \
                    [list def,public-font opt_def,public-font "" "" "" ""]] \
                  [list [get_indep String Protected] \
                    [list def,protected-font opt_def,protected-font "" "" "" ""]] \
                  [list [get_indep String Private] \
                    [list def,private-font opt_def,private-font "" "" "" ""]] \
                ] \
              ] \
            ] \
            [list \
              [list [get_indep String CrossReference] \
                [list \
                  [list [get_indep String StandardFont] \
                    [list def,xref-font opt_def,xref-font "" "" "" ""]] \
                  [list [get_indep String BranchFont] \
                    [list def,xref-branch-font opt_def,xref-branch-font "" "" "" ""]] \
                ] \
              ] \
            ] \
            [list \
              [list [get_indep String IncludeNoKey] \
                [list \
                  [list [get_indep String Standard] \
                    [list def,include_font opt_def,include_font "" "" "" ""]] \
                ] \
              ] \
            ] \
            [list \
              [list [get_indep String Drawing] \
                [list \
                  [list [get_indep String LinesTo] \
                    [list "" "" def,canv-line-fg opt_def,canv-line-fg "" ""]] \
                  [list [get_indep String LinesBy] \
                    [list "" "" def,xref-used-by-fg opt_def,xref-by-fg "" ""]] \
                  [list [get_indep String SelectedLines] \
                    [list "" "" def,tree-select-line opt_def,tree-select-line "" ""]] \
                  [list [get_indep String BranchToLines] \
                    [list "" "" def,xref-branch-fg opt_def,xref-branch-fg "" ""]] \
                  [list [get_indep String BranchByLines] \
                    [list "" "" def,xref-list-branch-fg opt_def,xref-list-branch-fg "" ""]] \
                ] \
              ] \
            ]\
            [list \
              [list [get_indep String MultiGrep] \
                [list \
                  [list [get_indep String Listing] \
                    [list def,grep-font opt_def,grep-font "" "" "" ""]] \
                  [list [get_indep String FoundText] \
                    [list def,grep-found-font opt_def,grep-found-font def,grep-found-fg opt_def,grep-found-fg "" ""]] \
                ] \
              ] \
            ] \
            [list \
              [list [get_indep String OthersNoKey] \
                [list \
                  [list [get_indep String Balloon] \
                    [list def,balloon-font opt_def,balloon-font \
                      def,balloon-fg opt_def,balloon-fg def,balloon-bg \
                      opt_def,balloon-bg]] \
                  [list [get_indep String Checkbutton] \
                    [list "" "" def,checkbutton-select \
                      opt_def,checkbutton-select "" ""]] \
                ] \
              ] \
            ] \
          ] \
        ]

        #store the fonts/clors into a variable to test if the
        #user has changed something.
        set font_color_variables ""
        foreach sm ${schemes} {
            #categories (editor, class, ..)
            foreach cat [join [lrange ${sm} 1 end]] {
                set cents [lindex ${cat} 1]
                #options (text, ..)
                foreach ent ${cents} {
                    foreach {ename eents} ${ent} {
                        foreach {name opt} ${eents} {
                            if {${name} != ""} {
                                lappend font_color_variables ${name}
                            }
                        }
                    }
                }
            }
            break
        }

        Color& ${ColorAndFont}.clr -schemes ${schemes}
        pack ${ColorAndFont}.clr -fill both -expand y -padx 10 -pady 10

        lappend AvailTools ${ColorAndFont}
    }
    method RaiseColorAndFont {} {
        #set lastPage clrfont
    }
    #####################################################

    method save_cb {what} {
        apply 0 ${what}
        switch -- ${what} {
            "project" {
                    sn_save_project
                }
            "default" {
                    save_default_settings
                }
            default {
                    bell
                }
        }
    }

    method goto {pane} {
        switch -- ${pane} {
            "classbr" {
                    set pane "ctree"
                }
            "retr" -
            "grep" -
            "make" {
                    set pane "others"
                }
        }
        if {[catch {${NoteBook} raise ${pane}}]} {
            ${NoteBook} raise proj
        }
    }

    protected trap1
    protected trap2
    protected trap3
    protected trap4
    protected default
    protected perms_trap
    protected xref_reenabled

    method verify_set {var trap {def 0} {strap ""}} {
        global sn_options
        if {![info exists sn_options(opt_${var})]} {
            return
        }
        if {$sn_options(opt_${var}) != $sn_options(${var})} {
            set sn_options(${var}) $sn_options(opt_${var})
            incr ${trap}
            if {${def}} {
                incr default
            }
            if {${strap} != ""} {
                incr ${strap}
            }
        }
    }

    method apply {{terminate 1} {from_save_as_default ""}} {
        global sn_options
        global tcl_platform
        global Avail_Parsers Parser_Info
        global opt_Parser_Info

        #options that can effected automaticaly
        set trap1 0
        #options that can be effected for new windows
        set trap2 0
        #options that can be effected after restart
        set trap3 0
        #options that can be effected after reparsing
        set trap4 0
        #mark if the seted variable is a part of the default settings
        set default 0

        #to verify if the database permisions
        set perms_trap 0
        #to tell user something about xref
        set xref_reenabled 0

        #options in the project page
        if {[winfo exists ${Project}]} {
            verify_set readonly trap1
            verify_set def,refresh-project trap3 1
            verify_set both,db-directory trap1 1
            verify_set def,reuse-window trap2 1
            verify_set def,window-switchable trap2 1
            verify_set def,window-alighment trap2 1

            if {$sn_options(opt_def,window-size) !=\
                    $sn_options(def,window-size)} {
                set sn_options(def,window-size)\
                  $sn_options(opt_def,window-size)
                if {[catch {set num [expr $sn_options(def,window-size) + 0]}]} {
                    set sn_options(def,window-size) 65
                }
                if {$sn_options(def,window-size) > 100} {
                    set sn_options(def,window-size) 100
                }\
                elseif {$sn_options(def,window-size) < 30} {
                    set sn_options(def,window-size) 30
                }
                incr trap2 1
                incr default
            }
            verify_set def,encoding trap4 1
            verify_set both,user-read trap1 1 perms_trap
            verify_set both,user-write trap1 1 perms_trap
            verify_set both,group-read trap1 1 perms_trap
            verify_set both,group-write trap1 1 perms_trap
            verify_set both,others-read trap1 1 perms_trap
            verify_set both,others-write trap1 1 perms_trap

            verify_set both,create-comment-db trap4 1
            verify_set def,db_cachesize trap3 1
            verify_set def,xref-db-cachesize trap4 1
        }

        #options in the editor page
        if {[winfo exists ${Editor}]} {
            verify_set def,edit-tabstop trap1 1
            verify_set def,edit-wrap trap1
            verify_set def,edit-indentwidth trap1 1
            verify_set def,edit-create-bak trap1 1
            #verify_set edit_Translate trap1 1
            verify_set def,edit-more-buttons trap2 1
            verify_set def,edit-bracket-delay trap1 1
            verify_set def,edit-rightmouse-action trap1 1
            verify_set def,edit-tab-inserts-spaces trap1 1
            verify_set def,edit-file-translation trap1 1
            verify_set def,edit-external-editor trap1 1
	    verify_set def,edit-external-always trap1 1
        }

        #options for hierarchy
        if {[winfo exists ${ClassHierarchy}]} {
            verify_set def,ctree-view-order trap1
            verify_set def,ctree-layout trap1
            verify_set def,ctree-horizspace trap1
            verify_set def,ctree-vertspace trap1
            verify_set def,class-goto-imp trap1 1
            verify_set def,class-orientation trap2 1
            verify_set def,members-order trap2 1
        }

        #options for xref
        if {[winfo exists ${XReference}]} {
            if {$sn_options(both,xref-create) !=\
              $sn_options(opt_both,xref-create)} {
                #if xref is enabled, after a project is already generated,
                #the user should reparse the hole project to actualize
                #xref information
                if {!${new_project} && $sn_options(opt_both,xref-create) !=\
                  ""} {
                    set xref_reenabled 1
                }
                verify_set both,xref-create trap1
            }
            verify_set both,xref-locals trap1 1

            verify_set both,xref-accept-param trap1
            verify_set both,xref-accept-static trap1
            verify_set both,xref-disp-param trap1
            verify_set both,xref-draw-rect trap1

            verify_set def,xref-disp-order trap1
            verify_set def,xref-layout trap1
            verify_set def,xref-horizspace trap1
            verify_set def,xref-vertspace trap1
            verify_set def,xref-bell trap1
        }

        #options for inc
        if {[winfo exists ${Include}]} {
            verify_set def,include-locatefiles trap4
            verify_set def,include-disporder trap1
            verify_set def,include-layout trap1
            verify_set def,include-horizspace trap1
            verify_set def,include-vertspace trap1

            set sn_options(opt_include-source-directories) [split\
              [${inc_editor} get 0.0 end] \n]
            if {$sn_options(include-source-directories) !=\
              $sn_options(opt_include-source-directories)} {
                #filter unknown directories
                set sn_options(include-source-directories) ""
                foreach dir $sn_options(opt_include-source-directories) {
                    if {[file isdirectory ${dir}]} {
                        ::lappend sn_options(include-source-directories) ${dir}
                    }
                }
                incr trap1 1
            }
        }

        #options for parser
        if {[winfo exists ${Parser}]} {

            #macro files, filter not existing files
            set files [split [${macros} get 0.0 end] \n]
            set sn_options(opt_macrofiles) ""
            foreach f ${files} {
                if {[file isfile ${f}]} {
                    ::lappend sn_options(opt_macrofiles) ${f}
                }
            }
            verify_set macrofiles trap4

            #language extensitions
            set test_list {1 2}
            set all_suf ""
            foreach type ${Avail_Parsers} {
                #handle file extensition
                regsub -all {[ \t]+} $opt_Parser_Info(${type},SUF) { }\
                  opt_Parser_Info(${type},SUF)
                set ok_suf ""
                foreach s $opt_Parser_Info(${type},SUF) {
                    # A rule (glob pattern) can be used only once!
                    if {[lsearch -exact ${all_suf} ${s}] != -1} {
                        continue
                    }
                    lappend all_suf ${s}
                    append ok_suf ${s} " "
                    if {[catch {string match ${s} ${test_list}} err]} {
                        sn_error_dialog "${err}: ${type} ${s}"
                        continue
                    }
                }
                set mod 0
                set opt_Parser_Info(${type},SUF) [string trimright ${ok_suf}]
                if {$opt_Parser_Info(${type},SUF) !=\
                  $Parser_Info(${type},SUF)} {
                    set Parser_Info(${type},SUF) $opt_Parser_Info(${type},SUF)
                    set mod 1
                    #after reparse
                    incr trap4
                }
                #handle external editors
                regsub -all {[ ]+} $opt_Parser_Info(${type},EDIT) { }\
                  opt_Parser_Info(${type},EDIT)
                if {$opt_Parser_Info(${type},EDIT) !=\
                  $Parser_Info(${type},EDIT)} {
                    set Parser_Info(${type},EDIT)\
                      $opt_Parser_Info(${type},EDIT)
                    set mod 1
                    incr trap1
                }
            }
        }

        if {[winfo exists ${Rcs}]} {
            verify_set both,rcs-type trap1

            set sn_options(opt_def,ignored-directories) ""
            foreach dir [split [${ign_editor} get 0.0 end] \n] {
                if {${dir} != ""} {
                    ::lappend sn_options(opt_def,ignored-directories) ${dir}
                }
            }
            verify_set def,ignored-directories trap1
        }

        if {[winfo exists ${Others}]} {
            verify_set both,make-command trap1
            verify_set def,html-viewer trap1

            #no printer commands on windows
            if {$tcl_platform(platform) != "windows"} {
                verify_set def,ascii-print-command trap1
                verify_set def,print-command trap1
            }

            verify_set def,gdb-command trap1
            verify_set donot_display trap1
        }

        #colors and fonts
        if {[winfo exists ${ColorAndFont}]} {
            #for the following colors, we need to restart SN
            foreach var {def,default-font def,default-fg def,default-bg\
              def,layout-font def,layout-fg def,layout-bg def,highlight-fg\
              def,highlight-bg def,select-fg def,select-bg\
              def,checkbutton-select} {
                if {[::info exists sn_options(opt_${var})]} {
                    verify_set ${var} trap3 1
                }
            }
            #verify if something changed in the color list
            foreach var ${font_color_variables} {
                if {[::info exists sn_options(opt_${var})]} {
                    verify_set ${var} trap1 1
                }
            }
        }

        #something is modified or preferences is called for a widget,
        #affect it to the existing objects
        if {${widget} != "" || [expr ${trap1} + ${trap2} + ${trap3} +\
          ${trap4}]} {

            if {[winfo exists ${Project}]} {
                #change database permissions
                set operms $sn_options(both,db-perms)
                sn_calc_db_perms
                if {$sn_options(both,db-perms) != ${operms}} {
                    sn_set_project_permission $sn_options(both,db-perms)
                }
            }

            if {${widget} == ""} {
                #update project

                #update editors
                foreach ed [itcl::find objects -class Editor&] {
                    ${ed} Update_Layout
                }

                #update hierarchy
                foreach ct [itcl::find objects -class ClassTree&] {
                    ${ct} Update_Layout
                }

                #update class
                foreach cls [itcl::find objects -class Class&] {
                    ${cls} Update_Layout
                }

                #update xref
                foreach xr [itcl::find objects -class XRef&] {
                    ${xr} Update_Layout
                }

                #update include
                foreach inc [itcl::find objects -class Include&] {
                    ${inc} Update_Layout
                }

                #update retriever
                foreach gr [itcl::find objects -class Retr&] {
                    ${gr} Update_Layout
                }

# FIXME: This is so not the way to do object communication !!!
                #update grep
                foreach gr [itcl::find objects -class Grep] {
                    ${gr} Update_Layout
                }

                #update Make
                foreach gr [itcl::find objects -class Make] {
                    ${gr} Update_Layout
                }

                #update Project window
                foreach pr [itcl::find objects -class Project&] {
                    ${pr} Update_Layout
                }

                #update Rcs

                #update others
            } else {
                #update the specified widget and it's sub views
                for {set nxt ${widget}} {${nxt} != ""} {set nxt [${nxt} next]} {
                    ${nxt} Update_Layout
                }
            }

            #update some other flags of the new settings
            set ret [update_global_settings]
            if {!${ret}} {
                return
            }

            #ask the user to reparse the project
            if {${xref_reenabled}} {
                set answer [tk_dialog auto [sn_title [get_indep String\
                  MultiXRef]] "[get_indep String XRefReenabled]"\
                  question_image 0 [get_indep String Reparse]\
                  [get_indep String DonotReparse]]
                if {${answer} == 0} {
                    Reparse
                }
            }
        }

        if {! ${new_project} &&(${trap2} || ${trap3} || ${trap4})} {
            set msg ""
            if {${trap2}} {
                append msg [get_indep String PrefInNewWindows]
            }
            if {${trap3}} {
                if {${msg} != ""} {
                    append msg "\n"
                }
                append msg [get_indep String PrefAfterRestart]
            }
            if {${trap4}} {
                if {${msg} != ""} {
                    append msg "\n"
                }
                append msg [get_indep String PrefAfterReparse]
            }
            sn_error_dialog ${msg} [get_indep String Preferences]
        }

        #if a variable, marked as default, changed ask the
        #user if he wants to save the new settings as default
        if {!${new_project} && ${from_save_as_default} != "default" &&\
          ${default}} {
            #set res [tk_dialog .info  #	[get_indep String Preferences] \
              #	[get_indep String SaveAsDefault]  #	question_image 0 \
              #	[get_indep String Save] [get_indep String DonotSave]]
            #if {$res != 0} {
            #	set default 0
            #}
            set default 0
        } else {
            set default 0
        }

        #save the part of the default settings (always)
        save_default_settings

        if {${terminate}} {
            #store last page
            set lastPage [${NoteBook} raised]

            global preferences_wait
            set preferences_wait "ok"
            itcl::delete object ${this}
            return
        }
    }

    #get database prefix
    #(including db-dir and database name without extension)
    proc get_database_prefix {{db_cmd "paf_db_proj"}} {
        set db_prefix [${db_cmd} get -key db_files_prefix]
        #backward compatib.
        #if {$db_prefix == ""} {
        #	set db_prefix [$db_cmd get -key db_files_prefix]
        #}
        return ${db_prefix}
    }

    #for backward compatibility
    proc check_some_variables {} {
        global sn_options
        #readonly-flag could be "normal", convert it to logical
        if {[lsearch -exact {normal disabled} $sn_options(readonly)] != -1} {
            if {$sn_options(readonly) == "normal"} {
                set sn_options(readonly) 0
            } else {
                set sn_options(readonly) 1
            }
        }
    }

    proc check_proj_settings {} {
        global sn_options
        global tkeWinNumber

        #check symbol database directory for backward compatibility
        if {$sn_options(both,db-directory) == ""} {
            set sn_options(both,db-directory) [paf_db_proj get\
              -key sn_sym_dir]
        }
        if {$sn_options(both,db-directory) == ""} {
            set sn_options(both,db-directory) "SNDB4"
        }

        #check project name (title)
        set sn_options(sys,project-name) [sn_constructe_name]

        if {![info exists tkeWinNumber]} {
            set tkeWinNumber 0
        }

        regsub -all {[ ]+} $sn_options(def,ignored-directories) { }\
          sn_options(def,ignored-directories)
        set sn_options(def,ignored-directories) [string trim\
          $sn_options(def,ignored-directories)]

        check_some_variables

        return 1
    }

    #initialize the database directory name and the database prefix
    proc init_db_files_prefix {} {
        global sn_options env tcl_platform

        if {$sn_options(both,db-directory) == ""} {
            set sn_options(both,db-directory) "SNDB4"
        }

        #Convert the name into a native name related to the OS, so
        #if you use ~/hello it is then converted into "/home../hello"
        set sn_options(both,db-directory) [file nativename\
          $sn_options(both,db-directory)]
        if {$tcl_platform(platform) == "windows"} {
            regsub -all {\\} $sn_options(both,db-directory)\
              {/} sn_options(both,db-directory)
        }

        if {$sn_options(db_files_prefix) == ""} {
            set tf [file rootname [file tail $sn_options(sys,project-file)]]

            #don't include the project extension in the database prefix.
            if {[file extension ${tf}] == ".proj"} {
                set tf [file root ${tf}]
            }
            set sn_options(db_files_prefix) [file join\
              $sn_options(both,db-directory) ${tf}]
        }

        return $sn_options(db_files_prefix)
    }

    proc create_database_dir {} {
        global sn_options

        #make sure that the db_files prefix is initialized correct
        init_db_files_prefix

        if {![file isdirectory $sn_options(both,db-directory)]} {
            if {[catch {file mkdir $sn_options(both,db-directory)} err]} {
                sn_error_dialog ${err}
                return 0
            }
        }

        #make sure that we have the correct permissions for the
        #symbols database directories
        catch {file attributes $sn_options(both,db-directory) -permission 0777}

        return 1
    }

    proc update_global_settings {} {
        global sn_options

        #make some dependences
        if {! [check_proj_settings] || ! [create_database_dir]} {
            sn_error_dialog [get_indep String PrefUnknownError]
            return 0
        }
        #parser switches
        set sn_options(sys,parser_switches) [eval list\
          $sn_options(both,create-comment-db) $sn_options(both,xref-create)\
          $sn_options(both,xref-locals)]
        return 1
    }

    proc can_modify {default v {load_save "load"}} {
        global sn_options
        global sn_user_specified

        #user modified this variable in his profile or on
        #the command line. Ignore it if it is part of the
        #default settings (not project settings
        if {[info exists sn_user_specified(${v})] && ${load_save} == "load"} {
            return 0
        }

        #don't modify system variables.
        if {[string first "sys," ${v}] == 0 || [string first "opt_" ${v}] ==\
          0} {
            return 0
        }

        #load all settings beginned with "def," or "both,"
        if {[string first "def," ${v}] == 0} {
            if {${default} == "default"} {
                return 1
            } else {
                return 0
            }
        }
        if {[string first "both," ${v}] != 0 && ${default} == "default"} {
            return 0
        }
        return 1
    }

    #save only variables that begin with "def," or "both,"
    proc save_default_settings {} {
        global sn_options
        global sn_product_version sn_project_version

        #save options as default
        set db [file join $sn_options(profile_dir) project.pre]
        if {![catch {set pref_db_fd [dbopen pref_db_cmd ${db} {RDWR CREAT\
          TRUNC} 0640 hash]}]} {

            #save all array elements of SN options (sn_options),
            foreach v [array names sn_options] {
                if {![can_modify default ${v} save]} {
                    continue
                }
                set val $sn_options(${v})
                if {${val} == ""} {
                    set val "#"
                }
                pref_db_cmd put ${v} ${val}
            }

            #we want only to save extensions and external editors
            SaveFileExtensions pref_db_cmd

            #write preferences version
            pref_db_cmd put product_version ${sn_product_version}
            pref_db_cmd put project_version ${sn_project_version}

            ide_write_global_preferences ${pref_db_fd}

            #close the preferences database
            ${pref_db_fd} close
        }
        return 1
    }

    #we always load default settings.
    proc load_default_settings {{new_project 0}} {
        global sn_options
        global sn_product_version sn_project_version
        global env

        sn_log "load default settings"

        set db [file join $sn_options(profile_dir) project.pre]

        #open default preferences database
        set ret [catch {set pref_db_fd [dbopen pref_db_cmd ${db} RDONLY 0640\
          hash]}]

        if {! ${ret} && [pref_db_cmd get -key product_version] ==\
          ${sn_product_version}} {
            foreach v [array names sn_options] {

                if {![can_modify default ${v}]} {
                    continue
                }

                set val [pref_db_cmd get -key ${v}]
                if {${val} == ""} {
                    #variable wasn't stored in the database
                    continue
                }
                if {${val} == "#"} {
                    #variable masked to be empty
                    set val ""
                }
                set sn_options(${v}) ${val}
            }

            LoadFileExtensions pref_db_cmd

            ide_read_global_preferences ${pref_db_fd}

            ${pref_db_fd} close
        }

        check_some_variables

        #make sure that the database directory is included in the ignored
        #directory list, include only the directory name
        set symdir [file tail $sn_options(both,db-directory)]
        set off [lsearch -exact $sn_options(def,ignored-directories) ${symdir}]
        if {${off} == -1} {
            ::lappend sn_options(def,ignored-directories) ${symdir}
        }

        #parser switches
        set sn_options(sys,parser_switches) [eval list\
          $sn_options(both,create-comment-db) $sn_options(both,xref-create)\
          $sn_options(both,xref-locals)]

        #set permissions for database
        sn_calc_db_perms
    }

    #save the language extensitions and the external editors
    #into the specified database
    proc SaveFileExtensions {db} {
        global sn_options
        global Avail_Parsers Parser_Info
        set txt ""
        foreach type ${Avail_Parsers} {
            lappend txt [list [list TYPE $Parser_Info(${type},TYPE)] [list SUF\
              $Parser_Info(${type},SUF)] [list EDIT $Parser_Info(${type},EDIT)]]
        }
        ${db} put lang_group_extensions ${txt}
    }

    #load only the language extensitions and the external editors
    #into the language browsers
    proc LoadFileExtensions {{db ""}} {
        global sn_options
        global sn_user_specified
        global Avail_Parsers Parser_Info

        if {${db} != ""} {
            set lang_group_extensions [${db} get -key lang_group_extensions]
            if {${lang_group_extensions} != ""} {
                foreach g ${lang_group_extensions} {
                    #get type
                    set type ""
                    foreach pair ${g} {
                        if {[lindex ${pair} 0] == "TYPE"} {
                            set type [lindex ${pair} 1]
                            break
                        }
                    }
                    if {${type} == ""} {
                        #ignore unknown types
                        continue
                    }
                    foreach pair ${g} {
                        set key [lindex ${pair} 0]
                        set val [lindex ${pair} 1]
                        if {${key} != "EDIT" && ${key} != "SUF"} {
                            continue
                        }
                        #verify if the extension is already defined
                        #on the command line or in the profile
                        if {[info exists\
                          sn_user_specified(sys,parser,${type},${key})]} {
                            sn_log "skip loading parser extension for ${type}"
                            continue
                        }
                        #only load external Editor and Extensions
                        set Parser_Info(${type},${key}) ${val}
                    }
                }
                unset lang_group_extensions
            }
        }

        #make sure that an editor exists
        foreach type ${Avail_Parsers} {
            if {! [info exists Parser_Info(${type},EDIT)]} {
                set Parser_Info(${type},EDIT) ""
            }
        }
    }

    proc load_project_settings {} {
        global sn_options
        global sn_path
        global sn_product_version sn_project_version
        global tcl_platform
        global sn_history
        global history_List

        foreach v [list prj_lines_num] {

            global ${v}
            set ${v} [paf_db_proj get -key ${v}]
        }

        #check project line number for correct setting
        if {[catch {set foo [expr ${prj_lines_num} + 0]}]} {
            set prj_lines_num 0
        }

        #load history
        foreach hscope $sn_history(scopes) {
            set history_List(${hscope}) [paf_db_proj get\
              -key history_${hscope}]
        }

        #load saved database name
        set sn_options(db_files_prefix) [paf_db_proj get -key db_files_prefix]

        #make sure that the db prefix is correct
        init_db_files_prefix

        #variables in sn_options
        foreach v [array names sn_options] {
            if {![can_modify project ${v}]} {
                continue
            }
            set val [paf_db_proj get -key ${v}]
            if {${val} == "#"} {
                #variable was empty marked.
                set val ""
            }\
            elseif {${val} == ""} {
                #it means, this variable wasn't stored in the database.
                continue
            }
            set sn_options(${v}) ${val}
        }

        if {$sn_options(def,include-locatefiles) == ""} {
            set sn_options(def,include-locatefiles) 1
        }

        #load db-perms
        sn_calc_db_perms

        #check if we loaded correct settings
        check_proj_settings

        #make sure that the db prefix is correct
        init_db_files_prefix

        #load ignored words from the c/parser
        load_ignored_words
    }

    proc load_ignored_words {} {
        global sn_options
        if {![catch {set fd [dbopen db_ign $sn_options(db_files_prefix).0\
          RDONLY 0640 hash]}]} {
            set sn_options(ignored_words) [@@lsort -dictionary [${fd} seq\
              -data]]
            ${fd} close
        } else {
            set sn_options(ignored_words) ""
        }
        return $sn_options(ignored_words)
    }

    proc save_ignored_words {} {
        global sn_options

        if {$sn_options(ignored_words) == ""} {
            catch {file delete -- $sn_options(db_files_prefix).0}
        } else {
            if {[catch {set fd [dbopen db_ign $sn_options(db_files_prefix).0\
              {RDWR CREAT TRUNC} [sn_db_perms] hash]} err]} {
                sn_error_dialog ${err}
            } else {
                foreach iword [::lunique [@@lsort\
                  -dictionary $sn_options(ignored_words)]] {
                    ${fd} put ${iword} {#}
                }
                ${fd} close
            }
        }
    }

    proc save_project_settings {{new ""}} {
        global sn_options
        global sn_product_version
        global tcl_platform
        global sn_history
        global history_List

        sn_log "save_project_settings <${new}>"

        foreach v [list prj_lines_num] {
            global ${v}
            if {[info exists ${v}]} {
                catch {paf_db_proj put ${v} [set ${v}]}
            }
        }

        #save views history
        foreach hscope $sn_history(scopes) {
            if {[info exists history_List(${hscope})]} {
                paf_db_proj put history_${hscope} $history_List(${hscope})
            } else {
                paf_db_proj put history_${hscope} ""
            }
        }

        #save variables in sn_options
        #except some files
        foreach v [array names sn_options] {
            if {![can_modify project ${v} save]} {
                continue
            }
            #something tricky, it allows to detect if a variable
            #is availiable in the database. (See load_project_settings)
            set val $sn_options(${v})
            if {${val} == ""} {
                set val "#"
            }
            catch {paf_db_proj put ${v} ${val}}
        }

        #save ignored words
        if {${new} != ""} {
            save_ignored_words
        }

        #save opened windows
        MultiWindow&::SaveYourSelf

        sn_log "save_project_settings finished"
    }

    #this procedure sets the default colors&fonts for the common
    #widgets
    proc set_color_font_attributes {} {
        global sn_options
        global sn_path tcl_platform

        #widgetDefault
        set level interactive

        if {$tcl_platform(platform) == "windows"} {
            #font settings
            option add "*Font" "$sn_options(def,layout-font)" ${level}
            option add "*insertBackground" "Black" ${level}

            #option add "*Background" "$sn_options(def,layout-bg)" $level
            #option add "*Foreground" "$sn_options(def,layout-fg)" $level

            option add "*HighlightBackground"\
              "$sn_options(def,highlight-bg)" ${level}
            option add "*HighlightForeground"\
              "$sn_options(def,highlight-fg)" ${level}

            option add "*Text.HighlightBackground"\
              "$sn_options(def,highlight-bg)" ${level}
            option add "*Text.HighlightForeground"\
              "$sn_options(def,highlight-fg)" ${level}

            option add "*Entry.HighlightBackground"\
              "$sn_options(def,highlight-bg)" ${level}
            option add "*Entry.HighlightForeground"\
              "$sn_options(def,highlight-fg)" ${level}

            option add "*Entry.Foreground"\
              "$sn_options(def,default-fg)" ${level}
            option add "*Entry.Background"\
              "$sn_options(def,default-bg)" ${level}

            option add "*Text.Background" $sn_options(def,default-bg) ${level}

            option add "*Listbox.Background"\
              "$sn_options(def,default-bg)" ${level}
            option add "*Listbox.Foreground"\
              "$sn_options(def,default-fg)" ${level}

            option add "*Canvas.Background"\
              "$sn_options(def,default-bg)" ${level}

            option add "*Table.Background"\
              "$sn_options(def,default-bg)" ${level}

            #option add *Menu.Font ansi $level

            #option add "*TreeTable.Font" "$sn_options(def,default-font)" $level

            #option add "*Listbox.Font" "$sn_options(def,default-font)" $level
            #option add "*Entry.Font" "$sn_options(def,default-font)" $level
            #option add "*Text.Font" "$sn_options(def,default-font)" $level
        } else {
            #font settings
            option add "*Font" "$sn_options(def,layout-font)" ${level}
            option add "*insertBackground" "Black" ${level}

            option add "*Background" "$sn_options(def,layout-bg)" ${level}
            option add "*Foreground" "$sn_options(def,layout-fg)" ${level}

            option add "*HighlightBackground"\
              "$sn_options(def,highlight-bg)" ${level}
            #option add "*HighlightForeground"\
              "$sn_options(def,highlight-fg)" $level

            option add "*Entry.Foreground"\
              "$sn_options(def,default-fg)" ${level}
            option add "*Entry.Background"\
              "$sn_options(def,default-bg)" ${level}

            option add "*Listbox.Background"\
              "$sn_options(def,default-bg)" ${level}
            option add "*Listbox.Foreground" "black" ${level}

            option add "*Canvas.Background"\
              "$sn_options(def,default-bg)" ${level}

            option add "*Table.Background"\
              "$sn_options(def,default-bg)" ${level}

            option add "*Text.Background" $sn_options(def,default-bg) ${level}

            #option add "*Button*Anchor" nw $level
            #option add "*Button*Font" "$sn_options(def,layout-font)" $level
            option add "*TreeTable.Font"\
              "$sn_options(def,default-font)" ${level}
            option add "*Listbox.Font" "$sn_options(def,default-font)" ${level}
            option add "*Entry.Font" "$sn_options(def,default-font)" ${level}
            option add "*Text.Font" "$sn_options(def,default-font)" ${level}
        }

        option add "*selectBackground" $sn_options(def,select-bg) ${level}
        option add "*selectForeground" $sn_options(def,select-fg) ${level}

        option add "*TreeTable.textForeground"\
          "$sn_options(def,default-fg)" ${level}
        option add "*TreeTable.bitmapBackground"\
          "$sn_options(def,default-bg)" ${level}
        option add "*TreeTable.Background"\
          "$sn_options(def,default-bg)" ${level}

        option add "*Label.Anchor" nw ${level}
        option add "*Label.TakeFocus" "0" ${level}

        option add "*Button.HighlightThickness" "1" ${level}
        #option add "*TixMeter.Font" "$sn_options(def,layout-font)" $level

        #unix-only options
        if {$tcl_platform(platform) != "windows"} {
            #select color for check- and radio-buttons
            option add "*selectColor" $sn_options(def,checkbutton-select)\
              ${level}

            #scrollbar width
            option add "*Scrollbar.Width" 12 ${level}
        }
    }

    method tool_availiable {tool new_project} {
        if {${widget} != ""} {
            set tt [${widget} whoami]
            switch -- ${tt} {
                "grep" -
                "retr" -
                "make" {
                        set tt others
                    }
                "classbr" {
                        set tt ctree
                    }
            }
            if {${tt} != ${tool}} {
                return disabled
            } else {
                return normal
            }
        } else {
            return [tool_Exists ${tool} ${new_project}]
        }
    }

    method switch_tab {op} {
        switch_tix_notebook_tab $NoteBook $op
    }

    protected AvailTools
    protected NoteBook

    protected font_color_variables ""
    protected ProjectPage ""
    protected Project ""

    protected EditorPage ""
    protected Editor ""

    protected ClassBrowserPage ""
    protected ClassBrowser ""

    protected ClassHierarchyPage ""
    protected ClassHierarchy ""

    protected XReferencePage ""
    protected XReference ""

    protected IncludePage ""
    protected Include ""
    protected inc_editor ""

    protected ParserPage ""
    protected Parser ""
    protected parser_igned ""
    protected macros ""

    protected RcsPage ""
    protected Rcs ""
    protected ign_editor ""

    protected ColorAndFont ""
    protected ColorAndFontPage ""

    protected Others ""
    protected OthersPage ""

    #last accessed page
    common lastPage "proj"

    public padx 5
    public pady 5
    public raise "proj"
    public widget ""
    public new_project 0
}

proc tool_Exists {tool {new_project 0}} {
    if {${new_project}} {
        return normal
    }
    switch -- ${tool} {
        "proj" {
                set cmd "paf_db_proj"
            }
        "grep" -
        "retr" -
        "make" {
                set cmd "paf_db_f"
            }
        "ctree" -
        "class" -
        "classbr" {
                set cmd "paf_db_cl"
            }
        "xref" {
                if {[sn_processes_running]} {
                    #return disabled
                    return normal
                }
                set cmd "paf_db_to"
            }
        "incbr" {
                set cmd "paf_db_iu"
            }
        "proj" -
        "edit" -
        default {
                set cmd "paf_db_proj"
            }
    }
    if {[info commands ${cmd}] == ""} {
        return disabled
    } else {
        return normal
    }
}

proc sn_project_preferences {{new_project 0} {page "proj"} {widget ""} {prefix\
  ".proj"}} {
    global preferences_wait

    set preferences_wait ""

    set p ${prefix}-preferences
    if {![winfo exists ${p}]} {
        Preferences& ${p} -new_project ${new_project} -raise ${page}\
          -widget ${widget}
    } else {
        ${p} raise
        ${p} goto ${page}
        if {${widget} != ""} {
            ${p} config -widget ${widget}
        }
    }
    if {${preferences_wait} == "ok"} {
        return 1
    } else {
        return 0
    }
}

#to avoid using "umask", what changes the system umask and
#influence the new created source files (bad),
#we use this way to set permissions for the database.
proc sn_calc_db_perms {} {
    global sn_options
    global env
    set um 0
    if {$sn_options(both,user-read)} {
        set um [expr ${um} | 0400]
    }
    if {$sn_options(both,user-write)} {
        set um [expr ${um} | 0200]
    }
    if {$sn_options(both,group-read)} {
        set um [expr ${um} | 0040]
    }
    if {$sn_options(both,group-write)} {
        set um [expr ${um} | 0020]
    }
    if {$sn_options(both,others-read)} {
        set um [expr ${um} | 0004]
    }
    if {$sn_options(both,others-write)} {
        set um [expr ${um} | 0002]
    }
    set sn_options(both,db-perms) [format "0%o" ${um}]
    set env(SN_DB_PERMS) $sn_options(both,db-perms)

    sn_log "database permissions: $sn_options(both,db-perms)"
    return $env(SN_DB_PERMS)
}

proc sn_db_perms {} {
    global sn_options
    return $sn_options(both,db-perms)
}


