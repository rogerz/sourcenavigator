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
# init.tcl - Miscellaneous initialisation routines.
# Copyright (C) 1998 Cygnus Solutions.

#Initialize Source-Navigator mostly for default settings
#
# Description for sninit.tcl and preferences.tcl:
#The problem here that the preferences are stored ones in project
#and possibly another time in the default preferences. This means
#that the project settings override default settings. the
#default settings are only used when you create a project.
#This is a bug. We have to define the settings that are
#	- project related
#	- global settings (like font, color, ..)
#
#I'm going to do this ASAP. For now if you want to change fonts
#in Preferences click the button "Save As Default". This will
#affect only _new_ projects.
#
#Now the Preferences loading process is described as followed:
#1. SN is launched:
#	sninit.tcl is started and default settings are
#	loaded.
#2. ~/.sn/profile is loaded
#3. SN looks afterthere for a file ~/.sn/project.pre, if
#   it is availiable, it load it's contents, now it overrides
#   the most default settings.
#4. New created projects will use the settings in 1-3.
#5. When you launch a project, it's own settings will be loaded.
#
#Every step overrides the previous one. This is correct, but
#needs to define what can be overriden and what not.
#However there are some variables that are made as exception!!
#1. Defined three levels for configuration settings.
#
#   A. System variables (sys,)
#        All parameters that are normally set by startup to define
#        platform dependent settings have the prefix "sys,".
#        This variables are not saved/restored into/from the
#        preferences settings.
#        Example: iscolor, alt-traverse
#
#   B. Global settings (def,)
#        This settings are valid for every SN session. This settings
#        are stored in the user's file ~/.sn/project.pre. They are not
#        part of the project settings.
#        Example: all color/font settings, language, parser extensions
#
#   C. Project and Default settings (both,)
#        This are settings that need to be set for projects, as example
#        by creation, but should take effect for next new projects.
#        They have all the prefix "both,". There is only a few of those.
#        This settings are stored both in global and project settings.
#        Example: xref-locals, db-directory
#
#   D. Project settings
#        All other settings without a prefix of the upove are project
#        dependent only. The most of them are used internally.
#        Example: readonly, history
#
#   The point here is that we use only one array to store all of them.
#   - This reduces mistakes and
#   - get out of failed (alot of) global initializations and
#   - Self description of the source code, first look to know
#     what kind of parameter is going on.
#   - A clear and reasonable way to follow up how things are implemented
#     in SN.
#
#2. Exported settable parameters for users
#   Before, it was possible to set every parameter in SN. It was even
#   possible to unset a parameter using the "profile" file (using a "%").
#   This is bad because
#        -SN crashes when user specify invalid parameters
#        -Not possible to test the options for valid data. 
#        -Only tcl freaks can work with this stuff (not all of them)
#        -Only SN has this way of doing things
#        -Not documented
#
#   Now we have the possibility to "export" whitch options are settable:
#        -Export parameters that can be changed from customers
#        -Don't allow changing another parameters
#        -Test settings for it's validy.
#        -Start SN always with valid data.
#
#   ***Internal an interface has been made for exporting variables:
#        proc sn_add_option \
#            {cat var {value ""} {type "string"} {trigger ""} {valid ""}}
#
#   "cat" defines how to classify the option "def, both, sys or ''".
#
#   It has the following important features:
#        -It's possible to define a callback procedure to be executed
#         when customers are trying to modify an option.
#         There are some default triggers, like sn_notempty_trigger.
#
#         Example: parser-ext is handled internal, to make possible to set
#         another values, it calls a callback procedure supporting the
#         command line argument but store the information to be used in SN:
#
#                sn_add_option sys parser-ext ""\
  "string" sn_parser_extension_trigger
#
#         The function sn_parser_extension_trigger is automatically called to
#         set the user specification and tests it for it's validy.
#
#        -Defines a set of possible values.
#
#         Example: report-bugs can be be only "www" or "mail", initial is "www"
#
#                sn_add_option def report-bugs "www" "logical" "" {www mail}
#
#         Another example, a logical variable "xref" is used to define
#         "xref-create", where the last is a command line argument for the
#         parser (-x). The user shouldn't use "xref-create=-x", but rather
#         tells "xref=yes" or "xref=no".
#
#         By default logical variables are tested for {yes no y n 1 0}
#
#        -Lists all availiable settable parameters and can provide ways to
#advice
#         the user for correct settings.#
#
#        -Open. It's open to be extended to support more functionality. As
#         Example, we can extend it to display more help for every settable
#         parameter.#
#
#        -Makes sure that the user deliver correct settings.#
#
#        -Reasonable for not tcl users.
#
#   Just now exported variables are:
#        readonly, xref, html-viewer, make-command,
#        ascii-print-command, browser-spacing, gdb-command,
#        rcs-type, language, encoding, parser-ext
#
#3. Naming confentions of variables.
#   All possible styles were used in SN to define option variables. Like
#        Using "_":
#                sn_sym_dir, paf_source_dirs
#        Using all combinations of Upper/Lower and "_":
#                Xref_CreateCrossRef, edit_TabSize
#
#   The fact that command line arguments don't have "_" and don't use    
#   Upper/Lower combinations, I have choosen the way to set variables,
#   like:
#                db-directory                    (older sn_sym_dir)
#                include-source-directories      (older paf_source_dirs)
#                select-bg                       (older sel-background)
#                xref-create                     (older Xref_CreateCrossRef)
#                ...
#
#   All (also not exported) option names have been renamed in this way.
#   I even tried to rename those to understand what this variable is
#   doing. It was alot of work!
#
#4. Load/Save Preferences
#   This was a big problem in SN. It hasn't worked correct when an option
#   has been renamed or deleted. As example integer variables could be
#   read as empty and will crash SN.
#   Now it's possible to
#        -Rename/delete every variable you want
#        -SN will discover if a variable is correct stored
#        -No need to use nasty catch commands to avoid somelike problems.
#        -Backward compatipility is guaranteed.
#         Because the Database has been changed since 4.1, we can only
#         Load projects from 4.1 (without reparsing).    
#         More older projects need to be reparsed.
#################################################################################

set copyright "Copyright (c) 1997-2000 Red Hat, Inc.\n All rights\
  reserved."

#force flag
#on windows we must force focusing the window
#on unix, let the window manager do this.
if {$tcl_platform(platform) == "windows"} {
    set sn_force_flag "-force"
} else {
    set sn_force_flag ""
}

# The standard Tcl auto_reset proc is buggy.
# NOTE: The script just lets you to use the .tcl files instead of
# the .tob ones.
proc sn_auto_reset {path} {
    global auto_execs auto_index auto_oldpath auto_path

    sn_log "auto reset ${path}"

    # Unload every known itcl class because they have to be loaded again!
    set classes [itcl_info classes]

    if {${classes} != ""} {
        catch {itcl_unload ${classes}}
    }

    set ignore {unknown pkg_mkIndex tclPkgSetup tclPkgUnknown}
    foreach p [info procs] {
        if {[info exists auto_index(${p})] && ![string match auto_* ${p}] &&\
          [string first {.tob} $auto_index(${p})] > 0 &&([lsearch\
          -exact ${ignore} ${p}] < 0)} {
            catch {rename ${p} {}}
        }
    }
    catch {unset auto_execs}
    catch {unset auto_index}
    catch {unset auto_oldpath}

    set auto_path [linsert ${auto_path} 0 [realpath -pwd [pwd] ${path}]]

    sn_log "auto_path changed to <${auto_path}>"
}

#define_font is the way to set fonts for windows. It's dynamic
#and fonts will be automatically changed in SN when the user
#changes his default font settings in Windows preferences.
proc init_some_font_attributes {} {
    global sn_options tcl_platform
    if {$tcl_platform(platform) == "windows"} {
        set sn_options(def,font-family) "*"
        set sn_options(def,font-size) "*"
        set sn_options(def,balloon-font-size) "*"
        catch {font delete global/default}
        define_font global/default -family ansi
        set sn_options(def,layout-font) ansi
        set sn_options(def,layout-fg) SystemMenuText
        set sn_options(def,layout-bg) SystemMenu
    } else {
        set sn_options(def,font-family) "Adobe"
        set sn_options(def,font-size) "120"
        set sn_options(def,balloon-font-size) "100"
        set sn_options(def,layout-font)\
          "-$sn_options(def,font-family)-Helvetica-Medium-R-Normal--*-$sn_options(def,font-size)-*-*-*-*-iso8859-1"
        set sn_options(def,layout-fg) black
        #darkgray
        set sn_options(def,layout-bg) "#c0c0c0"
    }
}

# initializes global variables.
proc sn_init_globals {} {
    uplevel #0 {
        #window counter
        set tkeWinNumber 0

        #SN scopes
        #in: Inheritences
        #iu: Include
        #iv: Instance variables
        #lv: Local variables
        set sn_general_scopes [lsort {cl con e ec fd fr fu gv iv ma md mi t un}]
        set sn_fortran_scopes [lsort {fu com con cov su}]
        set sn_local_scopes [lsort {in iv lv ud}]
        set sn_other_scopes [lsort {fil in iu lv ud}]
        set sn_current_scopes ${sn_general_scopes}
        set sn_scopes ${sn_general_scopes}
        set sn_all_scopes ""
        eval lappend sn_all_scopes ${sn_general_scopes} ${sn_fortran_scopes}\
          ${sn_other_scopes}
        set sn_all_scopes [lsort ${sn_all_scopes}]

        #init more internal variables
        set ProcessingCancelled 0
        set tkeWinNumber 0
        set sn_statistic_run 0
        if {${sn_debug}} {
            set env(SN_DBIMP) 1
        }

        # Do not allow the exec command to exec unknown commands!
        set auto_noexec 1
    }

    uplevel #0 {
        #now we use an array for SN options
        global sn_options env
        global sn_path
        global sn_root

        #make sure that env(HOME) exists
        if {! [info exists env(HOME)] || $env(HOME) == ""} {
            if {$tcl_platform(platform) == "windows"} {
                if {[info exists env(USERPROFILE)]} {
                    set env(HOME) env(USERPROFILE)
                } else {
                    set env(HOME) "C:${sn_root}"
                }
            } else {
                set env(HOME) ${sn_root}
            }
        }

        #project settings
        #export database directory to be not empty.
        sn_add_option both db-directory ".snprj" string
        #export project-readonly flag
        sn_add_option "" readonly no logical
        set sn_options(project_extensions) [list [list {Project files}\
          {*.proj}] [list {All files} {*}]]

        #extension for executables
        if {$tcl_platform(platform) == "windows"} {
            set sn_options(executable_ext) [list [list {Executables}\
              {*.exe;*.com}] [list {All files} {*}]]
            set sn_options(executable_defaultext) ".exe"
        } else {
            set sn_options(executable_ext) [list [list {All files} {*}]]
            set sn_options(executable_defaultext) ""
        }

        #database
        set sn_options(both,user-read) 1
        set sn_options(both,user-write) 1
        set sn_options(both,group-read) 1
        set sn_options(both,group-write) 1
        set sn_options(both,others-read) 0
        set sn_options(both,others-write) 0
        set sn_options(both,create-comment-db) ""
        set sn_options(def,db_cachesize) 300
        set sn_options(def,xref-db-cachesize) 3000
        sn_add_option def refresh-project "0" "logical"

        #parser switches to use.
        set sn_options(sys,parser_switches) "-x"

        if {$tcl_platform(platform) == "windows"} {
            set y [winfo screenheight .]
            if {${y} <= 480} {
                set sn_options(def,desktop-font-size) 8
            } else {
                set sn_options(def,desktop-font-size) 12
            }
        } else {
            set sn_options(def,desktop-font-size) 12
        }

        init_some_font_attributes

        if {$tcl_platform(platform) == "windows"} {
            define_font global/fixed -family ansi
            set sn_options(def,default-font) ansi
            set sn_options(def,default-fg) SystemWindowText
            set sn_options(def,default-bg) SystemWindow
            set sn_options(def,highlight-fg) SystemWindowText
            set sn_options(def,highlight-bg) SystemWindow
        } else {
            set sn_options(def,default-font)\
              "-$sn_options(def,font-family)-Courier-Medium-R-Normal--*-$sn_options(def,font-size)-*-*-*-*-iso8859-1"
            set sn_options(def,default-fg) black
            set sn_options(def,default-bg) white
            set sn_options(def,highlight-fg) black
            set sn_options(def,highlight-bg) "#c0c0c0"
        }

        #color for the border around the widget, should be the same
        #as the background

        if {$tcl_platform(platform) == "windows"} {
            #bold font
            define_font global/bold -family ansi -weight bold
            set sn_options(def,bold-font) global/bold

            set sn_options(def,select-fg) SystemHighlightText
            set sn_options(def,select-bg) SystemHighlight
        } else {
            #bold font
            set sn_options(def,bold-font)\
              "-$sn_options(def,font-family)-Courier-Bold-R-Normal--*-$sn_options(def,font-size)-*-*-*-*-iso8859-1"

            if {$sn_options(iscolor)} {
                set sn_options(def,select-fg) black
                set sn_options(def,select-bg) yellow
            } else {
                set sn_options(def,select-fg) white
                set sn_options(def,select-bg) black
            }
        }

        #select color for check- and radio-buttons
        if {$tcl_platform(platform) == "windows"} {
            set sn_options(def,checkbutton-select) white
        } else {
            set sn_options(def,checkbutton-select) Seagreen
        }

        #we use the same keybindings for unix&windows
        set sn_options(sys,alt-traverse) "Shift-Control"
        set sn_options(sys,alt-accelpref) "Ctrl+Shift"

        if {$tcl_platform(platform) == "windows"} {
            define_font global/balloon -family ansi
            set sn_options(def,balloon-font) ansi
            set sn_options(def,balloon-fg) SystemInfoText
            set sn_options(def,balloon-bg) SystemInfoBackground
        } else {
            set sn_options(def,balloon-font)\
              "-*-Helvetica-Medium-R-Normal--*-$sn_options(def,balloon-font-size)-*-*-*-*-iso8859-1"
            set sn_options(def,balloon-fg) black
            set sn_options(def,balloon-bg) lightyellow
        }

        #lines to draw between tree items
        #dark blue
        #set sn_options(def,canv-line-fg)        "#00008b"
        set sn_options(def,canv-line-fg) black
        #dark red
        set sn_options(def,xref-used-by-fg) "#8b0000"
        set sn_options(def,tree-select-line) red
        #SeaGreen
        set sn_options(def,xref-branch-fg) "#2e8b57"
        #SeaGreen
        set sn_options(def,xref-list-branch-fg) "#2e8b57"

        #text editor
        #external editor
        set sn_options(def,edit-external-editor) ""
        set sn_options(noname_file) "(noname)"
        if {$tcl_platform(platform) == "windows"} {
            # get font size from a system font.
            set sys_font_info [font actual system]
            set size [lindex ${sys_font_info} [expr ([lsearch ${sys_font_info}\
              "-size"]+1)]]
            set sn_options(def,edit-font) "{Courier} ${size}"
            set sn_options(def,edit-fg) SystemWindowText
            set sn_options(def,edit-bg) SystemWindow
        } else {
            set sn_options(def,edit-font) $sn_options(def,default-font)
            set sn_options(def,edit-fg) black
            set sn_options(def,edit-bg) white
        }
        set sn_options(def,color_fu) red
        set sn_options(def,color_fd) "indian red"
        set sn_options(def,color_su) $sn_options(def,color_fu)
        set sn_options(def,color_rem) SeaGreen

        #auto-reparse by saving or opening a file.
        set sn_options(def,auto-reparse) 1

        #auto-save a file when toggling to another location.
        #Files are then automatically saved without prompt.
        set sn_options(def,auto-save) 0

        #use by default the normal editor color for strings, customer
        #could change it.
        #using magenta as string color
        #IMPORTANT: This variables are accessed on C/Level in exint.c
        #IF YOU CHANGE THIS NAMES, YOU HAVE TO UPDATE IT IN exint.c
        set sn_options(def,color_str) "#7d007d"
        set sn_options(def,color_key) blue
        set sn_options(def,color_cont) blue
        set sn_options(def,color_cl) maroon
        set sn_options(def,color_t) "indian red"
        set sn_options(def,color_e) gold
        set sn_options(def,color_ec) "sky blue"
        set sn_options(def,color_md) $sn_options(def,color_fd)
        set sn_options(def,color_fr) "indian red"
        set sn_options(def,color_mi) $sn_options(def,color_fu)
        set sn_options(def,color_iv) brown
        set sn_options(def,color_gv) goldenrod
        set sn_options(def,color_con) goldenrod
        set sn_options(def,color_com) coral
        set sn_options(def,color_cov) "indian red"
        set sn_options(def,color_ma) "dark orange"
        set sn_options(def,bracketmatch-bg) DodgerBlue

        #default editor settings (global)
        set sn_options(def,edit-wrap) none
        set sn_options(def,edit-rightmouse-action) menu
        set sn_options(def,edit-bracket-delay) 500
        set sn_options(def,edit-more-buttons) 0

        #editor settings that affect the current project, but could be
        #stored as default for new projects.
        set sn_options(def,edit-create-bak) 0
        set sn_options(def,edit-indentwidth) 8
        set sn_options(def,edit-tabstop) 8
        set sn_options(def,edit-overwrite-mode) 0
        set sn_options(def,edit-file-translation) "keep"
        #insert Spaces on Tabs
        set sn_options(def,edit-tab-inserts-spaces) 0

        set sn_options(def,edit-mark-current-position) 1

        #editor synchronization
        set Switch_Is_Enabled 1

        #class hierarchy&browser
        #left to right
        set sn_options(def,ctree-view-order) 0
        set sn_options(def,ctree-layout) tree
        set sn_options(def,ctree-vertspace) 60
        set sn_options(def,ctree-horizspace) 10
        if {$tcl_platform(platform) == "windows"} {
            # get font size from a system font.
            # use printer friendly fonts for MS Windows
            set sys_font_info [font actual system]
            set size [lindex ${sys_font_info} [expr ([lsearch ${sys_font_info}\
              "-size"]+1)]]
            set sn_options(def,abstract-font) "{Courier} ${size} bold"
            set sn_options(def,ctree-font) "{Courier} ${size}"
        } else {
            set sn_options(def,abstract-font) $sn_options(def,bold-font)
            set sn_options(def,ctree-font) $sn_options(def,default-font)
        }
        #selecting a member goto Def. or Impl.
        set sn_options(def,class-goto-imp) "imp"

        set sn_options(def,class-font) $sn_options(def,default-font)
        set sn_options(def,public-font) $sn_options(def,default-font)
        set sn_options(def,protected-font) $sn_options(def,default-font)
        set sn_options(def,private-font) $sn_options(def,default-font)
        set sn_options(def,class-orientation) vertical
        set sn_options(def,members-order) first

        #include
        set sn_options(def,include_font) $sn_options(def,default-font)
        #left to right
        set sn_options(def,include-disporder) 0
        set sn_options(def,include-layout) tree
        set sn_options(def,include-vertspace) 60
        set sn_options(def,include-horizspace) 10
        #force SN to look for the location of included headers
        set sn_options(def,include-locatefiles) 1
        set sn_options(include-source-directories) ""

        #xref
        sn_add_option sys xref yes "logical" "sn_xref_option_trigger"
        set sn_options(both,xref-create) "-x"
        set sn_options(both,xref-locals) ""
        set sn_options(def,xref-font) $sn_options(def,default-font)
        set sn_options(def,xref-branch-font) $sn_options(def,default-font)

        #left to right
        set sn_options(def,xref-disp-order) 0
        set sn_options(def,xref-layout) isi
        set sn_options(def,xref-vertspace) 60
        set sn_options(def,xref-horizspace) 10
        set sn_options(def,edit-xref-highlight) 0
        #bell after finished xref
        set sn_options(def,xref-bell) 0

        set sn_options(both,xref-accept-param) 0
        set sn_options(both,xref-accept-static) 0
        set sn_options(both,xref-disp-param) 0
        set sn_options(both,xref-draw-rect) 0

        #grep history
        set sn_options(history,grep,file) ""
        set sn_options(def,grep-font) $sn_options(def,default-font)
        set sn_options(def,grep-found-font) $sn_options(def,default-font)
        if {$sn_options(iscolor)} {
            set sn_options(def,grep-found-fg) blue
            set sn_options(def,grep-found-bg) white
        } else {
            set sn_options(def,grep-found-fg) black
            set sn_options(def,grep-found-bg) white
        }
        #if the user wants to highlight all matches in an grep-editor
        #combination.
        set sn_options(def,grep-mark-all) 0

        #networking
        sn_add_option def html-viewer "netscape -remote openURL(%s)"

        #option needs to be there for backward compatibility.
        set sn_options(def,send-bugs-via-mail) 0

        #make
        sn_add_option both make-command "make"
        set sn_options(make-lastdir) ""

        #printer page format
        if {[info exists env(TZ)] && [regexp {.*(WET|MET).*} $env(TZ)]} {
            set sn_options(def,page-format) "A4"
            # West and midle Europe
        } else {
            set sn_options(def,page-format) "Letter"
        }
        set sn_options(def,page-formats) [join {  {Letter 8.5 11 inch} \
          {Legal 8.5 14 inch}  {A4 21 29.7 cm}  {A3 29.7 42 cm}  {A2 42 59.4\
          cm} } ";"]
        #printer colors
        set sn_options(def,print-form-bg) White
        set sn_options(def,print-form-fg) White

        if {$tcl_platform(platform) == "windows"} {
            sn_add_option def ascii-print-command {print}
            #option not availiable on windows
            set sn_options(def,print-command) ""
            sn_add_option def browser-spacing 0 "integer"

            set sn_options(def,null_dev) "NUL:"
        } else {
            sn_add_option def ascii-print-command {pr -h "%s" -l66 -o4 | lpr}
            sn_add_option def browser-spacing 2 "integer"
            sn_add_option def print-command "lpr"

            set sn_options(def,null_dev) "/dev/null"
        }

        #database settings
        set sn_options(db_files_prefix) ""
        set sn_options(db_del_type) 0
        # 1= soft-, 0=real delete

        #default geometry
        set sn_options(def,wm_geometry) 0

        set sn_options(Scann_Files_At_Once) 50
        set sn_options(def,canvas-tree-jump) 0

        #scopes supported in SN.
        set sn_options(def,ignored-directories) ""
        set sn_options(def,wm-deiconify-withdraw) 0
        # use 1 for window manager bug workaround.

        #raise make window when it's finished.
        set sn_options(def,make-raise) 1

        set sn_options(def,balloon-undisp-delay) 5000
        # in miliseconds
        set sn_options(def,balloon-disp-delay) 500
        # in miliseconds

        if {$tcl_platform(platform) == "windows"} {
            if {[info exists env(USERPROFILE)]} {
                set sn_options(profile_dir) [file join $env(USERPROFILE) .sn]
            } else {
                set sn_options(profile_dir) [file join C:/ .sn]
            }
        } else {
            set sn_options(profile_dir) [file join ${HOME} .sn]
        }
        sn_log "User profile directory <$sn_options(profile_dir)>"
        set sn_projects_list(filename) [file join $sn_options(profile_dir)\
          nav_projs.lst]
        set sn_options(def,localhost) "127.0.0.1"

        set sn_options(sys,builtin-highlighting) {c++ java tcl chill python}
        set sn_options(sys,language-with-xref) {c++ java tcl fortran chill}

        #######################################
        ## new variables for multi functional #
        #######################################
        set sn_options(def,reuse-window) 1
        set sn_options(def,window-alighment) vertical
        set sn_options(def,window-switchable) 1
        set sn_options(def,window-size) 65

        #debugger settings
        sn_add_option def gdb-command "gdb"

        set sn_options(gdb-program) ""
        set sn_options(gdb-workdir) ""
        set sn_options(gdb-xterm) ""

        #macro files
        set sn_options(macrofiles) ""
        sn_add_option sys macrofile "" "string" sn_macrofiles_trigger
        sn_add_option sys replace-macrofiles ""\
          "string" sn_rep_macrofiles_trigger

        #retriever flags
        set sn_options(ignored_words) ""
        set sn_options(donot_display) 0

        #history for make
        set sn_options(make_history_cmd) ""
        set sn_options(make_history_dir) ""

        #editor search/replace default parameters
        set sn_options(search) ""
        set sn_options(search,str) ""
        set sn_options(search,nocase) "-nocase"
        set sn_options(search,method) "-exact"

        #history & history stack
        set sn_history(stacksize) 20
        set sn_history(size) 20
        set sn_history(project_size) 10
        set sn_history(scopes) {edit ctree class xref inc retr grep}

        #levels of directories to add
        set sn_options(def,scan-recursive) -1

        #rcs
        sn_add_option both rcs-type "rcs"

        #default language
        sn_add_option def language "english" "" "" "" {sn_language}

        #default encoding
        sn_add_option def system-encoding "iso8859-1"
        sn_add_option def encoding "iso8859-1"

        #default database permissions
        set sn_options(both,db-perms) 0660

        #add project file to be in read-only mode
        sn_add_option "sys" project-file "" string sn_readonly_trigger "" "" yes

        #export project directory to be in read-only mode
        if {[catch {set curdir [pwd]}]} {
            set curdir $env(HOME)
        }
        sn_add_option "sys" project-dir ${curdir} string sn_readonly_trigger\
          "" "" yes

        #export project name
        sn_add_option "sys" project-name "" string sn_readonly_trigger "" "" yes

        #to modify the parser extension list, marked
        #as sys to be not saved (only dummy)
        sn_add_option sys parser-ext "" "string" sn_parser_extension_trigger

        #add a flag to reparse the project on startup (even in batchmode)
        sn_add_option sys reparse no logical
    }
}

catch {rename bgerror old_bgerror}

proc bgerror {err} {
    global sn_options
    global errorInfo
    global auto_path

    # Be sure to release any grabs that might be present on the
    # screen, since they could make it impossible for the user
    # to interact with the stack trace.

    if {[grab current .] != ""} {
        grab release [grab current .]
    }

    sn_log "Error string: ${err}"
    sn_log "Stack Trace: ${errorInfo}"

    display_bug_report ${err} ${errorInfo}
}

# Replacement for sn_CreateInterp in sninit.c

# Create a new interpreter and pass in the commands
# to run in the new interpreter

proc create_interp {cmds} {      
  # Create slave interp, we need to name the slave interp
  # ourselves so that interp names are unique and do not get
  # reused as slave interps are created and deleted. This
  # makes it possible for an external application to track
  # individual project interps when connected to sourcenav-main.

  # FIXME: Yeah, I know, globals are evil!
  global sn_interp_numslaves

  if {! [info exists sn_interp_numslaves]} {
      set sn_interp_numslaves 0
  } else {
      incr sn_interp_numslaves
  }

  set appname sourcenav-$sn_interp_numslaves
  set ip [interp create $appname]

  # Move vars from master to slave
  foreach var {argc argv argv0 env auto_path auto_index} {
      if {[array exists ::$var]} {
          $ip eval [list array set $var [array get ::$var]]
      } else {
          $ip eval [list set $var [set ::$var]]
      }
  }

  # We copy some global variables into the target (new) interpreter.
    
  $ip eval [list set tcl_interactive 0]

# FIXME: It is not clear that this is needed, it seems to be done in Sn_setup
#  foreach var {sn_home HOME sn_debug auto_path} {
#    $ip eval [list set $var [set ::$var]]
#  }

  # Call SN interp init method, this also loads Tk into the slave
  # This calls the C method Sn_setup_Init in the slave interp
  $ip eval [list load {} Sn_setup]

  # Alias commands that slaves will use to interact with the master.

  interp alias $ip delete_interp {} sn_master_delete_slave $ip
  interp alias $ip create_interp {} sn_master_create_interp
  interp alias $ip number_interp {} sn_master_number_slave_interps

  # FIXME: Is this right? Seems like a no
  $ip eval [list set argv0 $appname]

  # FIXME: Should we try to create a unique name for this X display ???
  # We have to create a unique interpreter name in the network.
  #set interp_name [format %s-%s-%d application_name [info hostname] interpnum]
      
  # Get the appname of the slave and return it

  set appname [$ip eval [list tk appname $appname]]

  sn_log "created slave interp $ip, its appname is $appname"

  # Eval the -e argument to this function

  $ip eval $cmds

  return $appname
}

# This method is called in the slave interpreter to create a new
# slave interpreter in the master. Each slave holds its own SN
# project, so clearly we can not create a slave in a slave.
# This method is named create_interp in slaves.

proc sn_master_create_interp { cmds } {
  sn_log "called sn_master_create_interp (in [tk appname])"
  create_interp $cmds
}

# Return the number of slave interpreters that exist in the
# master. We assume that each slave interpreter is a SN project.
# This method is named number_interp in slaves.

proc sn_master_number_slave_interps { } {
  set interps [interp slaves]
  set num_interps [llength $interps]

  sn_log "sn_master_number_slave_interps called, there are $num_interps
      slave interps (in [tk appname])"
  return $num_interps
}

# This method is called through an alias in the slave interpreter.
# It is kind of tricky as we need to delete the slave interp but
# it is not actually for an interp to delete itself. That is why
# the master must always delete the slave interp, and the slave
# must not execute any more comands after a request to delete itself.
# This method is named delete_interp in slaves.

proc sn_master_delete_slave { slave } {
  global tcl_interactive tcl_platform
  sn_log "called master_delete_slave $slave ([$slave eval tk appname]) (in [tk appname])"
  interp delete $slave

  set num_interps [sn_master_number_slave_interps]

  # tcl_interactive is always set to 1 on Windows, so ignore it.
  if {$tcl_platform(platform) != "windows" && $tcl_interactive} {
      sn_log "Not exiting: in interactive mode"
  } elseif {$num_interps > 0} {
      sn_log "Not exiting: there are $num_interps still alive"
  } else {
      sn_log "Exiting: no slave interpreters and not in interactive mode"
      exit 0
  }
}

# Called from Sn_setup_Init C function

proc sn_tcl_tk_init {} {
    global sn_options
    global sn_path
    global env sn_debug auto_path
    global tkPriv tcl_platform
    global tcl_platform
    global set

    wm withdraw .
    update
    update idletasks

    # First set up all paths.
    sn_initialize_paths

    init_some_font_attributes

    set vis [winfo screenvisual .]
    sn_log "Screenvisual: ${vis}"
    if {[string first "color" ${vis}] != -1} {
        set sn_options(iscolor) 1
    } else {
        set sn_options(iscolor) 0
    }

    set cwd [pwd]
    # It is just a test!

    if {[catch {set env(PATH) $sn_path(bindir):$env(PATH)}]} {
        set env(PATH) $sn_path(bindir)
    }

    if {$tcl_platform(platform) == "windows"} {
        if {![info exists env(LOGNAME)] && [catch {set env(LOGNAME)\
          [get_username]}]} {
            set env(LOGNAME) "navigator"
        }
        set env(USERNAME) $env(LOGNAME)
        set sn_options(def,font-family) "*"
    } else {
        set sn_options(def,font-family) "Adobe"
    }

    set tkPriv(oldGrab) ""
    # Tk bugs !
    set tkPriv(activeBg) [. cget -background]

    set bitd $sn_path(bitmapdir)

    set pixmap "pixmap"
    set suf "xpm"
    set cursor "watch"

    if {[sn_batch_mode]} {
        #make sure that the "." window isn't displayed.
        catch {wm withdraw .}
    }

    image create ${pixmap} info_image -file [file join ${bitd} info.${suf}]
    image create ${pixmap} error_image -file [file join ${bitd} stop.${suf}]
    image create ${pixmap} question_image -file [file join ${bitd}\
      question.${suf}]

    #init global variables
    sn_init_globals

    #after initialization, we need to interpret the
    #command line options, they should override all settings!
    sn_read_commandline_arguments

    # Initialise and load toolchain specs.
    InitializeToolChainInfo

    #read configuration file 'sn_prop.cfg'
    sn_read_properities

    if {![file isdirectory $sn_options(profile_dir)]} {
        sn_log "Create dir: $sn_options(profile_dir)"
        catch {file mkdir $sn_options(profile_dir)}
    }

    #load user defined variable settings from a profile
    sn_read_profiles
    set sn_options(def,system-encoding) $sn_options(def,encoding)

    #read language dependent file
    sn_log "Text file: [file join $sn_path(etcdir)\
      $sn_options(def,language).txt]"
    sn_string_init [file join $sn_path(etcdir) $sn_options(def,language).txt]

    #if user wants to see help, show it him and exit
    global sn_arguments
    if {[info exists sn_arguments(help)]} {
        if {$tcl_platform(platform) == "windows"} {
            #on windows it has to be a dialog box.
            tk_dialog auto "Valid Option Parameters" $sn_arguments(help)\
              info_image -1 [get_indep String Ok]
        } else {
            puts stdout $sn_arguments(help)
        }
        exit 0
    }

    #load image and bitmap files
    #read user saved options and init some other options
    sn_load_pixmaps

    #Now we can read the stored user default settings
    Preferences&::load_default_settings 1

    #set default color&fonts for the gui (after loading settings)
    Preferences&::set_color_font_attributes

    #bind SN events for several widgets (treetable, text, canvas, ..)
    sn_bindings

    #load user defined rc file
    sn_load_rc_file rc.tcl

    if {${sn_debug}} {
        set pltfrm ""
        foreach v [array names tcl_platform] {
            append pltfrm "${v}: $tcl_platform(${v}) "
        }
        sn_log "plattform settings: ${pltfrm}"
    }

    # Register as an editor server.  We first check to see if the
    # command exists because, if it doesn't, then we aren't
    # connected to the IDE system at all, and we don't want to run
    # this.
    global ide_running
    if {[llength [info commands ide_editor_register]]} {
        ide_editor_register ide_edit_event
        ide_event initialize {} {file-created file-changed}

        # FIXME: for now, this mapping is hard-coded.  Later
        # it should be stored in properties.
        ide_interface_autolaunch build {} [list [file join $sn_path(bindir)\
          vmake]]

        set ide_running 1
    } else {
        set ide_running 0
    }
}

proc sn_read_profiles {} {
    global sn_options
    global sn_path
    global sn_arguments

    # Execute first the general profile, then the user's!
    foreach f [list [file join $sn_path(etcdir) profile] [file join\
      $sn_options(profile_dir) profile]] {
        sn_read_profile ${f}
    }

    set sn_options(def,page-formats) [split $sn_options(def,page-formats) ";"]

    #if the user specified some arguments using "--define" option,
    #execute those here
    foreach {var value} $sn_arguments(--define) {
        sn_modify_option ${var} ${value}
    }
}

proc sn_read_profile {fname} {
    global sn_options

    sn_log "Profile file: ${fname}"
    if {[catch {set fd [open ${fname}]} err]} {
        sn_log ${err}
        return
    }

    set lines [split [read -nonewline ${fd}] "\n"]
    close ${fd}

    #Query variables to set! Format must be name:value
    #name is alphanumeric with "-".
    foreach line ${lines} {
        set line [string trim ${line}]
        if {[string first "#" ${line}] == 0} {
            continue
        }
        set off [string first ":" ${line}]
        if {${off} == -1} {
            continue
        }
        set param [string trim [string range ${line} 0 [expr ${off} - 1]]]
        set value [string trim [string range ${line} [expr ${off} + 1] end]]
        #uplevel #0 "set sn_options($param) {$value}"
        sn_modify_option ${param} ${value}
    }
}

#read configuratoin file 'sn_prop.cfg'
proc sn_read_properities {} {
    global sn_options
    global sn_path

    if {[file exists [file join $sn_options(both,db-directory) sn_prop.cfg]]} {
        set nm [file join $sn_options(both,db-directory) sn_prop.cfg]
    }\
    elseif {[file exists [file join $sn_options(profile_dir) sn_prop.cfg]]} {
        set nm [file join $sn_options(profile_dir) sn_prop.cfg]
    } else {
        set nm [file join $sn_path(etcdir) sn_prop.cfg]
    }

    sn_log "Loading parsers from: ${nm}"

    #here we load the "sn_prop.cfg" file to load the supported
    #parsers and some other user-defined stuff.
    if {[catch {uplevel #0 source [list ${nm}]} err]} {
        sn_error_dialog ${err}
        exit 1
    }
    return 1
}

proc sn_read_commandline_arguments {} {
    global sn_options
    global argc argv
    global sn_arguments
    global ProcessingCancelled
    global tcl_platform

    sn_log "process command line argc <${argc}> argv <${argv}>"

    set sn_arguments(--define) ""
    set sn_arguments(import-file) ""
    set sn_arguments(new) 0

    for {set idx 0} {${idx} < ${argc}} {incr idx} {
        set arg [lindex ${argv} ${idx}]

        switch -glob -- ${arg} {
            "--batchmode" -
            "-b" {
                    #batchmode mode
                    set sn_arguments(batchmode) 1
                    set new 1
                }
            "--projectname" -
            "-p" {
                    #project name
                    incr idx
                    set sn_arguments(projectfile) [lindex ${argv} ${idx}]
                }
            "-n" -
            "-new" -
            "--new" -
            "-c" -
            "-cr" -
            "-create" -
            "--create" {
                    #-create: create a new project queckly, don't call
                    #project selection
                    #-cr, -create: backward compatibility.
                    #-n, -new: backward compatibility.
                    set sn_arguments(new) 1
                }
            "-d" -
            "--databasedir" -
            "--dbdir" {
                    #database directory
                    incr idx
                    set sn_arguments(databasedir) [lindex ${argv} ${idx}]
                }
            "--import" -
            "-import" -
            "-i" {
                    #-import: import file
                    incr idx
                    set sn_arguments(import-file) [lindex ${argv} ${idx}]
                    if {[catch {set fd [open $sn_arguments(import-file)]}\
                      err]} {
                        if {[sn_batch_mode]} {
                            puts stderr ${err}
                        } else {
                            sn_error_dialog ${err}
                        }
                        exit 1
                    } else {
                        catch {close ${fd}}
                        set sn_arguments(new) 1
                    }
                }
            "--noxref" -
            "-x" {
                    set sn_options(both,xref-create) ""
                }
            "--def" -
            "-D" -
            "--define" {
                    incr idx
                    set pair [lindex ${argv} ${idx}]
                    set i [string first "=" ${pair}]
                    if {${i} <= 0} {
                        puts stderr "invalid option format \"${pair}\", usage:\
                          option=value"
                        puts stderr "use \"--avail-options\" to see all avail.\
                          options."
                        exit 1
                    }
                    set var [string range ${pair} 0 [expr {${i} - 1}]]
                    set value [string range ${pair} [expr {${i} + 1}] end]
                    #sn_modify_option $var $value
                    #execute those later, especially afer the user profile
                    #is read to have more priority
                    lappend sn_arguments(--define) ${var} ${value}
                }
            "--avail-opt" -
            "-o" -
            "--avail-options" {
                    set sn_arguments(help) [sn_list_options]
                    break
                }
            "--help" -
            "-h" {
                    set sn_arguments(help) "valid parameters:
    -b, --batchmode:   create a new project using batch mode
    -p, --projectname: specifies the name for a new project
    -c, --create:      creates a new project
    -d, --databasedir: specifies the directory to store the project databases
                       default is (.snprj)
    -i, --import:      imports the project list from a file
    -x, --noxref:      disables creating cross-reference information
    -o, --avail-options: lists all availiable options that can be set using\
                      \"--define\"
    -D, --define option=value
                       Defines an option, see \"--avail-options\"
    --debug:           enables debugging mode
    --home:            installation directory"

                    break
                }
            default {
                    set sn_arguments(projectfile) ${arg}
                }
        }
    }
    return 1
}

proc sn_make_font {{family ""} {type ""} {size 0} {slant "R"}} {
    global sn_options
    global tcl_platform

    if {${family} == ""} {
        set family Helvetica
    }
    if {${type} == ""} {
        set type Medium
    }
    if {${size} == 0} {
        set size [expr $sn_options(def,desktop-font-size) * 10]
    } else {
        append size 0
    }

    if {$tcl_platform(platform) == "windows"} {
        switch -glob -- ${family} {
            "*ourier*" {
                    set family {Courier New}
                    # if we're using very small font sizes, choose a more
                    # readable option
                    if {${size} <= 80} {
                        set family {Lucida Console}
                    }
                }
            "*elvetica*" {
                    set family {Times New Roman}
                }
        }
    }

    if {$tcl_platform(platform) == "windows"} {
        set cls "-*"
    } else {
        if {[string tolower ${slant}] == "i"} {
            set slant "o"
        }
        set cls "-${family}"
    }
    set font [format "%s-%s-%s-%s-Normal--*-%s%s" ${cls} ${family} ${type}\
      ${slant} ${size} "-*-*-*-*-iso8859-1"]

    if {$tcl_platform(platform) == "windows"} {
        # set up a mapping for this font, so that when we print we
        # are able to switch to Courier or Helvetica.
        global print_fontmap
        if {${family} == "Courier New" || ${family} == "Lucida Console"} {
            set fontname Courier
        } else {
            set fontname Helvetica
        }
        set tfont ${font}
        # We map the X fonts into a slightly smaller Windows font to
        # ensure text fits into boxes during printing.
        set size [expr ${size}/10 - 1]
        set print_fontmap(${tfont}) [list ${fontname} ${size}]
        set tfont [join [lreplace [split ${tfont} "-"] 4 4 o] "-"]
        set print_fontmap(${tfont}) [list "${fontname}-Oblique" ${size}]
        set tfont [join [lreplace [split ${tfont} "-"] 3 3 bold] "-"]
        set print_fontmap(${tfont}) [list "${fontname}-BoldOblique" ${size}]
        set tfont [join [lreplace [split ${tfont} "-"] 4 4 r] "-"]
        set print_fontmap(${tfont}) [list "${fontname}" ${size}]
    }

    sn_log -l 2 "Font: ${font}"

    return ${font}
}

proc sn_load_rc_file {file} {
    global sn_options
    global sn_debug HOME sn_path

    foreach file [list [file join $sn_options(profile_dir) ${file}] [file join\
      $sn_path(etcdir) ${file}]] {
        if {[file exists ${file}]} {
            sn_log "Executing... ${file}"
            catch {uplevel #0 source [list ${file}]} err
            sn_log "Result Executing ${file}:\n${err}"
            break
        }
    }

    #call as first user command 'sn_rc'
    catch {sn_rc}
}

#the following defines how a variable can be set
#cat:     categorie (def, both, sys or <empty>)
#var:     variable name
#value:   initialization
#type:    to be tested for valid values (string, integer, logical).
#trigger: user defined command to be executed when the option changes.
proc sn_add_option {cat var {value ""} {type "string"} {trigger ""} {valid ""}\
  {compatible ""} {readonly "no"}} {
    global sn_options
    global sn_user_options
    global sn_user_variables
    global sn_compatible

    if {${type} == ""} {
        set type "string"
    }

    #add categorie to variable name
    if {${cat} != ""} {
        set intvar ${cat},${var}
    } else {
        set intvar ${var}
    }
    if {${trigger} == ""} {
        if {${type} == "string"} {
            set trigger "sn_notempty_trigger"
        }
    }
    if {${valid} == "" && ${type} == "logical"} {
        set valid {yes no y n 1 0}
    }

    #compatible parameter names
    if {${compatible} != ""} {
        foreach c ${compatible} {
            set sn_compatible(${c}) ${var}
        }
    }

    set sn_options(${intvar}) ${value}

    set sn_user_variables(${var}) yes
    set sn_user_options(${var},name) ${intvar}
    set sn_user_options(${var},type) ${type}
    set sn_user_options(${var},value) ${value}
    set sn_user_options(${var},trigger) ${trigger}
    set sn_user_options(${var},valid) ${valid}
    set sn_user_options(${var},readonly) ${readonly}
}

#lists all exported options with information
#about its possible values.
proc sn_list_options {} {
    global sn_user_options
    global sn_user_variables
    global tcl_platform

    set txt_opts "Settable Parameters:
    use command line option \"--define or -D\" or add the options in your
    profile (~/.sn/profile, format variable:value) to modify the default
    settings.
    Interactive (in your ~/.sn/rc.tcl file) you can use tcl command
    \"sn_modify_option\" to change this values.
    Example: sn_modify_option gdb-command \"xterm -e gdb\"
    If you want to read the current option values, use \"sn_read_option\",
    Example: set prj \[sn_read_option project-file\]
    For more information please refer to the documentation and the README\
      file.\n"
    foreach n [lsort -dictionary [array names sn_user_variables]] {
        set txt "${n},\t"
        append txt "type=" $sn_user_options(${n},type)
        if {$sn_user_options(${n},readonly)} {
            append txt ",\tREADONLY"
        }\
        elseif {$sn_user_options(${n},value) != ""} {
            append txt ",\t" "default=" "\"$sn_user_options(${n},value)\""
        }
        if {$sn_user_options(${n},valid) != ""} {
            append txt ",\tvalid values=" "\{$sn_user_options(${n},valid)\}"
        }
        append txt_opts ${txt} "\n"
    }
    return ${txt_opts}
}

#called when the user specify option changes in the profile or
#on the command line
proc sn_modify_option {var value} {
    global sn_options
    global sn_user_options
    global sn_user_specified
    global sn_compatible

    sn_log "modify option \"${var}\" into \"${value}\""

    #if a compatible parameter has been used here, trigger it
    #to the new variable name
    if {[info exists sn_compatible(${var})]} {
        return [sn_modify_option $sn_compatible(${var}) ${value}]
    }

    if {![info exists sn_user_options(${var},name)]} {
        sn_log -stderr "invalid option: \"${var}=${value}\""
        return 0
    }

    if {$sn_user_options(${var},readonly)} {
        sn_readonly_trigger ${var} ${value}
        return 0
    }

    #internal name of the variable, like "def,foo"
    set intvar $sn_user_options(${var},name)
    set valid $sn_user_options(${var},valid)

    #verify valid data
    if {${valid} != ""} {
        if {[lsearch -exact ${valid} ${value}] == -1} {
            sn_log -stderr "specified invalid value ${value} for ${var}, valid\
              values <${valid}>."
            return 0
        }
        #checkbuttons support strings, so we need to use
        #only (0 1) for logical values
        if {${valid} == {yes no y n 1 0}} {
            if {${value}} {
                set value 1
            } else {
                set value 0
            }
        }
    }
    #verify data types
    if {$sn_user_options(${var},trigger) == ""} {
        if {$sn_user_options(${var},type) == "integer"} {
            if {[catch {set foo [expr {${value} + 0}]}]} {
                sn_log -stderr "specified an invalid value for \"${var}\""
                return 0
            }
        }
    } else {
        #execute trigger
        set ok [eval $sn_user_options(${var},trigger) [list ${var}]\
          [list ${value}]]
        if {${ok}} {
            #don't overwrite user settings
            set sn_user_specified(${intvar}) yes
        }
        return ${ok}
    }

    #correct value specified.
    set sn_options(${intvar}) ${value}

    #don't overwrite user settings
    set sn_user_specified(${intvar}) yes

    return 1
}

#get the value of an exported variable
proc sn_read_option {var} {
    global sn_options
    global sn_user_options
    global sn_user_variables
    if {![info exists sn_user_variables(${var})]} {
        sn_log -stderr "read option: option \"${var}\" unknown"
        return ""
    }
    return $sn_options($sn_user_options(${var},name))
}

#default procedure to be called when an exported
#option is requested to change. Forces values to be not empty.
proc sn_notempty_trigger {var value} {
    global sn_options
    global sn_user_options
    if {${value} == ""} {
        sn_log -stderr "option variable \"${var}\" can't be empty, option not\
          modified."
        return 0
    }

    set sn_options($sn_user_options(${var},name)) ${value}
    sn_log "option variable \"${var}\" changed to \"${value}\""

    return 1
}

#default can-empty trigger
proc sn_empty_trigger {var value} {
    global sn_options
    global sn_user_options
    set sn_options($sn_user_options(${var},name)) ${value}
    return 1
}

proc sn_readonly_trigger {var value} {
    sn_log -stderr "option \"${var}\" is readonly and can't be changed.
use \[sn_read_option ${var}\] to read it's current value."

    return 0
}

proc sn_xref_option_trigger {var value} {
    global sn_options
    global sn_user_options
    global sn_user_specified

    if {${value}} {
        set sn_options(both,xref-create) "-x"
    } else {
        set sn_options(both,xref-create) ""
    }
    #don't overwrite the user settings
    set sn_user_specified(both,xref-create) yes
    return 1
}


