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
## Class Browser that can be integrated in any subwindow.
##
#####################################################

itcl::class Class& {
    inherit sourcenav::MultiChild

    constructor {args} {
        global sn_options

        set topw [winfo toplevel $itk_component(hull)]
        set cls [info class]

	itk_component add panedwindow {
	    tixPanedWindow $itk_component(hull).paned\
		    -orientation $sn_options(def,class-orientation)
	} { }

        if {$sn_options(def,members-order) == "first"} {
	    set member_expand 0.8
	    set class_expand 0.2
	} else {
	    set member_expand 0.2
	    set class_expand 0.8
	}


        itk_component add memberpane {
	    $itk_component(panedwindow) add mem -expand $member_expand
	} { }

	itk_component add classpane {
            $itk_component(panedwindow) add cls -expand $class_expand
	} { }

        set memlist $itk_component(memberpane).list

	eval itk_initialize $args

        # Init mixer variables.
        set i 0
        foreach p ${mixer_vars} {
            upvar #0 ${this}-mixer-${p} mv
            if {${i} < 6} {
                set mv 1
            } else {
                set mv 0
            }
            incr i
        }
 
        set and 0

        # Restore old setting.
        if {${flags} != ""} {
            set i 0
            foreach p ${mixer_vars} {
                upvar #0 ${this}-mixer-${p} mv
                set mv [lindex ${flags} ${i}]
                incr i
            }
        }

        set vis class

        # Add related menu entries to the class browser.
        if {$itk_option(-menu) != ""} {
        }

        # Add toolbar buttons related to the class browser.
        if {$itk_option(-toolbar) != ""} {

            itk_component add toolbar {
		frame $itk_option(-toolbar).clsfr 
	    }

            pack $itk_component(toolbar) -side left

            # Create a combo box for the types of the class.
	    
	    itk_component add classcombo {
		Combo& $itk_component(toolbar).name_visib\
			-width 9 \
			-contents [list subclass class baseclass]\
			-entryvariable [itcl::scope visible] \
			-selectcommand [itcl::code ${this} ComboRefresh_cb]
	    } { }

            $itk_component(classcombo) selecttext ${vis}

            pack $itk_component(classcombo) -side left -fill x

            # Mixer.
	    itk_component add filter {
		button $itk_component(toolbar).mixer\
			-takefocus 0\
			-text [get_indep String Mixer]\
			-underline [get_indep Pos Mixer]\
			-pady 0\
			-command [itcl::code ${this} mixer]
	    } { }

            balloon_bind_info $itk_component(filter)\
		    [get_indep String MixerINFO]
            pack $itk_component(filter) -side left -fill y -expand n
        }

        # Create a tree for the class members.
	itk_component add memberlist {
	    Tree  $itk_component(memberpane).list\
		    -width 60 -height 15\
		    -exportselection 0 -fillselection 1\
		    -selectmode browse\
		    -tabs [list 250 150 150 400]\
		    -tabsize 3\
		    -labels [list "[get_indep String Name]"\
		     "[get_indep String Class]"\
		     "[get_indep String Type]"\
		     "[get_indep String Parameters]"]\
		    -info_label $itk_option(-mesg_area)\
		    -sortedinsertion 0\
		    -sortnocase 1\
		    -bestfit 1\
		    -truncate 1\
		    -font $sn_options(def,class-font)\
		    -propagate 1
	} { }

	$itk_component(memberlist) config \
		-fillcommand [itcl::code $this fillcbwrapper]

	itk_component add overridden {
	    checkbutton $itk_component(memberlist).filter.overridden\
		    -text [get_indep String ClassBrowOverridden]\
		    -under [get_indep Pos ClassBrowOverridden]\
		    -variable [itcl::scope overridden] \
		    -command [itcl::code ${this} fill] \
		    -relief groove
	} { }
	pack $itk_component(overridden) -side left\
		-before $itk_component(memberlist).filter.label

        set memlist_tree [$itk_component(memberlist) tree]

# FIXME: all these Tree bindings need to be replaced with something cleaner
        $itk_component(memberlist) treebind <space> [$itk_component(memberlist) treebind <Return>]
        $itk_component(memberlist) treebind <Double-1> "${this} handle_doubleclick"
        $itk_component(memberlist) treebind <ButtonRelease-1>\
		"${this} handle_click"
        $itk_component(memberlist) treebind <3>\
		"focus %W; ${this} select_post_members_menu %W %X %Y %y; break"

	itk_component add classtree {
	    sn_treetable $itk_component(classpane)\
		    -selectmode browse\
		    -indent 15\
		    -font $sn_options(def,class-font)\
		    -height 6 -borderwidth 0
	} { }

        bind $itk_component(classtree) <ButtonRelease-1>\
		"${this} handle_class_click"

        bind $itk_component(classtree) <1> "
			set tog \[$itk_component(classtree) identify %x %y\]
			set t_m_p \[$itk_component(classtree) nearest %y\]
			if {\$tog != \"\" && \$tog != \"text\" && \$t_m_p > -1} {
				${this} toggle_class \$t_m_p
			}
			unset tog; unset t_m_p
			break"

        bind $itk_component(classtree) <Control-1>\
		" set t_m_p \[%W nearest %y\]
			if {\$t_m_p > -1} {
			    ${this} tree_select \[%W get \$t_m_p\]
			}
			unset t_m_p
			break"

        bind $itk_component(classtree) <3>\
		"focus %W;${this} select_post_menu %W %X %Y %y; break"

        bind $itk_component(classtree) <Double-1>\
		"${this} edit_class %W \[%W nearest %y\]"

        bind $itk_component(classtree) <space>\
		"${this} edit_class %W; break"

        bind $itk_component(classtree) <Return>\
		"${this} edit_class %W; break"

        pack $itk_component(classtree) -fill both -expand y
        pack $itk_component(memberlist) -side top -fill both -expand y

        pack $itk_component(panedwindow) -fill both -expand y -side bottom

        # Call user defined procedure.
        catch {sn_rc_classbrowser $itk_component(hull) $itk_component(classtree)\
		$itk_component(memberlist)}

        if {$itk_option(-classname) != ""} {
            goto "" $itk_option(-classname)
        }

        ::focus $itk_component(memberlist)

        # Set title for the top window.
        SetTitle
    }

    destructor {
# FIXME: Remove when mixer vars are cleaned up.
        foreach v [::info globals "${this}-*"] {
            catch {uplevel #0 unset ${v}}
        }
    }

    # Post menu for members.
    method select_post_members_menu {w X Y y} {
        set m .cls_select_post_members_menu
        catch {destroy ${m}}
        menu ${m} -tearoff 0
        wm overrideredirect ${m} 1
        set y [${w} nearest ${y}]
        ${w} selection clear 0 end
        ${w} selection set ${y}
        set txt [Retriever&::convert_to_line $itk_component(memberlist)]
        if {${txt} != ""} {
            set txt [split ${txt} \t]
            set sym [lindex ${txt} 0]
            set cls [lindex ${txt} 1]
            set prm [lindex ${txt} 3]
            set fil [lindex ${txt} 4]
            set lin [lindex ${txt} 5]
            if {${cls} == ""} {
                set cls ${class_name}
            }
            set lbl " [get_indep String Of] '${cls}\:\:${sym}'"
            set state normal
        } else {
            set sym ""
            set cls ""
            set prm ""
            set fil ""
            set lin ""
            set lbl ""
            set state disabled
        }
        #set vw "[get_indep String View] "
        set vw ""

        #Editor
        ${m} add command -label "${vw}[get_indep String MultiEditor]${lbl}"\
          -underline [get_indep Pos MultiEditor] -command "${this} handle_doubleclick" -state ${state}

        #Class hierarchy
        ${m} add command -label "${vw}[get_indep String MultiClassHierarchy]\
          [get_indep String Of] '${cls}'" -underline [get_indep Pos\
          MultiClassHierarchy] -command "sn_classtree ${cls}" -state ${state}

        #Xref
        ${m} add command -label "${vw}[get_indep String MultiXRef]${lbl}"\
          -underline [get_indep Pos MultiXRef] -command " sn_xref both\
          [list ${cls}\:\:${sym}] [list ""] [list ${fil}] [list ${lin}] [list\
          ""] [list ${prm}] " -state ${state}

        ${m} add separator

        #add default menu entries
        $itk_component(memberlist) tree_post_menu ${m}

        tk_popup ${m} ${X} ${Y}
    }

    method select_post_menu {w X Y y} {
        global sn_options
        set m .cls_select_post_menu

        # It has to be destroyed because we might have problems with "tk_popup"!
        catch {destroy ${m}}

        menu ${m} -tearoff 0
        wm overrideredirect ${m} 1

        set y [${w} nearest ${y}]

        set str [tree_get ${w} ${y}]

        if {${str} == ""} {
            set str "selection"
            set target ""
            set state "disabled"
        } else {
            set target ${str}
            set state "normal"

            #select the entry
            ${w} selection clear 0 end
            ${w} selection set ${y}
        }
        ${m} add command -label [format [get_indep String SelectOnly] ${str}]\
          -command "${this} tree_select ${target}" -state ${state}
        ${m} add command -label [get_indep String SelectAll] -command "${this} tree_select -all" -accelerator "$sn_options(sys,alt-accelpref)+A"

        ${m} add separator

        tk_popup ${m} ${X} ${Y}
    }

    method handle_doubleclick {} {
        if {$itk_option(-doubleclickcommand) != ""} {
            ::eval $itk_option(-doubleclickcommand) [Selection]
        }
    }

    method handle_class_click {} {
        set sel [$itk_component(classtree) curselection]
        if {${sel} == ""} {
            return
        }
        set cls [$itk_component(classtree) get ${sel}]
        if {$itk_option(-selectcommand) != ""} {
            ::eval $itk_option(-selectcommand) [list cl ${cls} "" "" "" "" "" ""]
        }
    }
    method handle_click {} {
        if {$itk_option(-selectcommand) != ""} {
            ::eval $itk_option(-selectcommand) [Selection]
        }
    }

    method Title {{full 1}} {
        global sn_options
        set t "[get_indep String ClassBrowTitle]"
        if {${class_name} != ""} {
            set t "${t}: ${class_name}"
        }
        if {${full}} {
            return [sn_title ${t}]
        } else {
            return ${t}
        }
    }

    method Icon {} {
        global sn_options
        return [sn_view_icon [get_indep String ClassBrowTitle] ${class_name}]
    }

    method SetTitle {} {
        ${topw} configure -title [Title] -iconname [Icon]
    }

    #don't change the status of overridden/overloaded
    proc classbrowser_switch {frm fnc} {
        foreach fr {mem flags further} {
            foreach w [winfo children ${frm}.${fr}] {
                if {[winfo class ${w}] == "Checkbutton" && [string first\
                  "further.and" ${w}] == -1} {
                    if {[${w} cget -state] != "disabled"} {
                        ${w} ${fnc}
                    }
                }
            }
        }
    }
    method mixer {} {
        global sn_options
        global mixer_status
        set t $itk_component(hull)-mixer
        if {[winfo exists ${t}]} {
            ${t} raise
            return
        }
        sourcenav::Window ${t}
        ${t} on_close "${t}.button_0 invoke"
        ${t} withdraw
        set title [string trimright [get_indep String Mixer] "."]
        ${t} title ${title}
        ${t} configure -iconname ${title}
        sn_motif_buttons ${t} bottom 0 [get_indep String ok]
        ${t}.button_0 config -command "itcl::delete object ${t}"
        ${t} bind_tk <Return> "${t}.button_0 invoke"
        frame ${t}.chk

        #Buttons ALL,NONE,AND
        set chkfr ${t}.chk.btns
        frame ${chkfr}
        button ${chkfr}.all -text [get_indep String All] -under [get_indep Pos\
          All] -command "
                Class&::classbrowser_switch ${t}.chk select
                ${this} refresh
            "
        balloon_bind_info ${chkfr}.all [get_indep String ClassAllINFO]
        button ${chkfr}.none -text [get_indep String None] -under\
          [get_indep Pos None] -command "
                Class&::classbrowser_switch ${t}.chk deselect
                ${this} refresh
            "
        balloon_bind_info ${chkfr}.none [get_indep String ClassNoneINFO]
        grid ${chkfr}.all - ${chkfr}.none -padx 10 -pady 10 -sticky w
        pack ${chkfr} -fill x -expand y -pady 10

        #Member flags PUBLIC,PROTECTED,PRIVATE
        set chkfr ${t}.chk.mem
        frame ${chkfr}
        global ${t}-methods
        checkbutton ${chkfr}.methods -text [get_indep String Methods]\
          -under [get_indep Pos Methods] -variable ${t}-methods -onvalue "1"\
          -offvalue "0" -command " ${this} refresh " -state normal
        global ${t}-instvar
        checkbutton ${chkfr}.instvar -text [get_indep String ClassVars]\
          -under [get_indep Pos ClassVars] -variable ${t}-instvar -onvalue "1"\
          -offvalue "0" -command " ${this} refresh " -state normal
        global ${t}-friends
        checkbutton ${chkfr}.friends -text [get_indep String Friends]\
          -under [get_indep Pos Friends] -variable ${t}-friends -onvalue "1"\
          -offvalue "0" -command " ${this} refresh " -state normal
        grid ${chkfr}.methods ${chkfr}.instvar ${chkfr}.friends -padx 10\
          -sticky w
        pack ${chkfr} -fill x -expand y -pady 10

        frame ${t}.chk.line1 -relief sunken -bd 2 -height 2 -width 300
        pack ${t}.chk.line1 -fill x -expand y

        set widgets ""

        #Member FLAGS
        set chkfr ${t}.chk.flags
        frame ${chkfr}
        foreach p [lrange ${mixer_vars} 3 5] {
            global ${t}-${p}
            lappend widgets [checkbutton ${chkfr}.${p} -text ${p}\
              -variable ${t}-${p} -onvalue "1" -offvalue "0" -command\
              " ${this} refresh " -state normal]
        }
        pack ${chkfr} -fill x -expand y -pady 10
        frame ${t}.chk.line2 -relief sunken -bd 2 -height 2 -width 300
        pack ${t}.chk.line2 -fill x -expand y

        #Member Further flags
        set chkfr ${t}.chk.further
        frame ${chkfr}
        checkbutton ${chkfr}.and -text [get_indep String And]\
		-under [get_indep Pos And] -variable ${and} -onvalue "1"\
		-offvalue "0" -command " ${this} refresh " -state normal
        balloon_bind_info ${chkfr}.and [get_indep String AndINFO]
        foreach p [lrange ${mixer_vars} 6 10] {
            global ${t}-${p}
            lappend widgets [checkbutton ${chkfr}.${p} -text ${p}\
              -variable ${t}-${p} -onvalue "1" -offvalue "0" -command\
              " ${this} refresh " -state normal]
        }
        pack ${chkfr} -fill x -expand y -pady 10
        frame ${t}.chk.line3 -relief sunken -bd 2 -height 2 -width 300
        pack ${t}.chk.line3 -fill x -expand y

        #Group flags OVERRIDE,OVERLOADED
        set chkfr ${t}.chk.groups
        frame ${chkfr}
        foreach p [lrange ${mixer_vars} 11 end] {
            global ${t}-${p}
            lappend widgets [checkbutton ${chkfr}.${p} -text ${p}\
              -variable ${t}-${p} -onvalue "1" -offvalue "0" -command\
              " ${this} refresh " -state normal]
        }
        pack ${chkfr} -fill x -expand y -pady 10

        eval grid [lrange ${widgets} 0 2] -padx 10 -sticky w
        grid ${t}.chk.further.and -column 1 -padx 10 -sticky w
        eval grid [lrange ${widgets} 3 5] -padx 10 -sticky w
        eval grid [lrange ${widgets} 6 7] -padx 10 -sticky w
        eval grid [lrange ${widgets} 8 end] -padx 10 -sticky w

        pack ${t}.chk -anchor w -padx 10 -pady 10

        ${t} resizable no no
        focus ${t}
        ${t} centerOnScreen
	${t} raise
    }

    method tree_get {tr y} {
        set v [${tr} get ${y}]

        set off [string first {<} ${v}]
        if {${off} > 0} {
            set v [string range ${v} 0 [expr ${off} - 1]]
        }
        return ${v}
    }

    method tree_select {target} {
        if {[$itk_component(classtree) size] <= 0} {
            bell
            return
        }

        set off [string first {<} ${target}]
        if {${off} > 0} {
            set target [string range ${target} 0 [expr ${off} - 1]]
        }

        set plus_b enabled_image
        set minus_b undefined_image

        if {${target} == "-all"} {
            set base_classes ""

            $itk_component(classtree) selection clear 0 end

            $itk_component(classtree) itemconfig 0 -image ${plus_b}
            for {set i 1; set num [$itk_component(classtree) size]} {${i} < ${num}} {incr i} {
                set bitm [lindex [$itk_component(classtree) itemconfig ${i} -image] 4]
                if {[string first "unvisited" ${bitm}] == -1} {
                    set cls [tree_get $itk_component(classtree) ${i}]
                    lappend base_classes ${cls}
                    set class_viewed(${cls}) 1
                    $itk_component(classtree) itemconfig ${i} -image ${plus_b}
                }
            }
            set base_classes [::lunique [::lsort -command sn_compare\
              ${base_classes}]]
        } else {
            set cls [tree_get $itk_component(classtree) 0]
            if {${cls} == ${target}} {
                set base_classes ""
                $itk_component(classtree) itemconfig 0 -image ${plus_b}
            } else {
                set base_classes ${target}
                $itk_component(classtree) itemconfig 0 -image ${minus_b}
            }
            for {set i 1; set num [$itk_component(classtree) size]} {${i} < ${num}} {incr i} {
                set bitm [lindex [$itk_component(classtree) itemconfig ${i} -image] 4]
                if {[string first "unvisited" ${bitm}] == -1} {
                    set cls [tree_get $itk_component(classtree) ${i}]
                    if {${cls} == ${target}} {
                        set class_viewed(${cls}) 1
                        $itk_component(classtree) itemconfig ${i} -image ${plus_b}
                    } else {
                        set class_viewed(${cls}) 0
                        $itk_component(classtree) itemconfig ${i} -image ${minus_b}
                    }
                }
            }
        }

        ${this} refresh
    }

    method toggle_class {y} {
        set bitm [lindex [$itk_component(classtree) itemconfigure ${y} -image] 4]

        if {[string first "unvisited" ${bitm}] != -1} {
            return
            # Not defined class.
        }

        set cls [tree_get $itk_component(classtree) ${y}]

        if {[string first "enabled" ${bitm}] != -1} {
            set bitm undefined_image

            #hide members of this class
            if {${y} != 0} {
                set class_viewed(${cls}) 0
                set off [lsearch -exact ${base_classes} ${cls}]
                set base_classes [lreplace ${base_classes} ${off} ${off}]
            }
        } else {
            set bitm enabled_image

            #view members of this class
            if {${y} != 0} {
                set class_viewed(${cls}) 1
                lappend base_classes ${cls}
                set base_classes [::lunique [::lsort -command sn_compare\
                  ${base_classes}]]
            }
        }
        if {${y} == 0} {
            $itk_component(classtree) itemconfig ${y} -image ${bitm}
        } else {
            #Now, it gets a little complicated. Because in case of multiple
            #inheritance, it can happen that a base class has been included\
              twice
            #and we have to update all the appropiate bitmaps.
            for {set s [$itk_component(classtree) size]; set i 0} {${i} < ${s}} {incr i} {
                if {[tree_get $itk_component(classtree) ${i}] == ${cls}} {
                    $itk_component(classtree) itemconfig ${i} -image ${bitm}
                }
            }
        }

        ${this} refresh
    }

    method edit_class {w {y ""}} {
        if {${class_doubleclickcommand} == ""} {
            return
        }
        if {${y} == -1} {
            return
        }
        if {${y} == ""} {
            set y [lindex [${w} curselection] 0]
            if {${y} == ""} {
                return
            }
        }
        set bitm [lindex [${w} itemconfig ${y} -image] 4]
        if {[string first "unvisited" ${bitm}] != -1} {
            bell
            return
            # Not defined class.
        }
        set cls [${w} get ${y}]

        ::eval ${class_doubleclickcommand} ${cls}
    }

    proc convert_sym_to_bit {sym} {
        global SN_Scope_Bits

        set bits 0
        foreach s ${sym} {
            if {${s} != ""} {
                set bits [expr ${bits} | $SN_Scope_Bits(${s})]
            }
        }
        return ${bits}
    }

    method get_class_hier {sub_classes parent} {
        global sn_options sn_sep
        global SN_Scope_Bits

        #build the tree of the class structure
        foreach c ${sub_classes} {
            set attr $base_class_in(${c})
            foreach t [list public protected private] {
                if {[expr ${attr} & $SN_Scope_Bits(${t})]} {
                    break
                }
            }
            set cls_inf [paf_db_cl seq -data -col 0 "${c}${sn_sep}"]
            if {${cls_inf} == ""} {
                set bt unvisited_image
            } else {
                set bt enabled_image
            }

            $itk_component(classtree) insert end -text ${c}$base_class_templ(${c})\
              -parent ${parent} -image ${bt} -font $sn_options(def,${t}-font)

            if {[lsearch -exact ${base_classes} ${c}] == -1} {
                lappend base_classes ${c}
            }
            set base_class_in(${c}) ${attr}

            set sub_cls_l ""
            foreach sc [paf_db_in seq -uniq -col {1 5 8} "${c}${sn_sep}"] {
                set cls [lindex ${sc} 0]

                #verify if class refer itself
                if {${cls} == ${c}} {
                    continue
                }
                lappend sub_cls_l ${cls}
                set base_class_in(${cls}) [lindex ${sc} 1]
                set base_class_templ(${cls}) [lindex ${sc} 2]

                #this class have "c" as sub class
                set sub_classes_of(${cls}) ${c}
            }

            #this class have base classes
            set base_classes_of(${c}) ${sub_cls_l}

            #call this procedure recursively to build all parent classes
            if {${sub_cls_l} != ""} {
                get_class_hier [::lunique [::lsort -command sn_compare\
                  ${sub_cls_l}]] [expr [$itk_component(classtree) size] - 1]
            }
        }
    }

    method get_subclasses {cname file beg_line end_line} {
        global sn_options sn_sep
        set cls_name ${cname}
        #get the parent classes of the base classes
        set base_classes ""
        catch {unset base_class_in}
        catch {unset base_class_templ}
        set sub_cls ""
        if {[::info commands paf_db_in] != ""} {
            foreach i [::lunique [::lsort -command sn_compare [paf_db_in seq\
              -col [list 1 5 "2 -${beg_line}:${end_line}" 8] -end ${file}\
              "${cls_name}${sn_sep}"]]] {

                set cls [lindex ${i} 0]

                if {${cls} != ${cname}} {
                    set base_class_in(${cls}) [lindex ${i} 1]
                    set base_class_templ(${cls}) [lindex ${i} 3]
                    lappend sub_cls ${cls}
                }
            }
        }

        #add the first class on the top
        catch {$itk_component(classtree) remove 0}
        set bitm enabled_image
        $itk_component(classtree) insert end -text ${cname} -image ${bitm}

        #display the parent classes of the base class as subtree
        set base_classes_of(${cname}) ${sub_cls}
        foreach c ${sub_cls} {
            set sub_classes_of(${c}) ${cname}
        }
        get_class_hier ${sub_cls} 0

        set all_base_classes ${base_classes}

        foreach b ${base_classes} {
            set class_viewed(${b}) 1
        }
    }

    method class_members {cname loc visib type {inherit 0}} {
        global SN_Scope_Bits sn_sep

        foreach p ${mixer_vars} {
            upvar #0 ${this}-mixer-${p} mv
            if {${mv}} {
                set v${p} ${p}
            } else {
                set v${p} ""
            }
        }
        if {[string trim "${vpublic} ${vprotected} ${vprivate}"] == ""} {
            set vpublic public
            set vprotected protected
            set vprivate private
        }

        set file [lindex ${loc} 0]
        if {[catch {set beg_line [expr int([lindex ${loc} 1])]}]} {
            set beg_line 1
        }
        if {[catch {set end_line [expr int([lindex ${loc} 2])]}] ||\
          ${end_line} == 0} {
            set end_line [expr 1024 * 1024]
        }

        #read all base classes and display thim in the tree list
        if {![info exists all_base_classes]} {
            get_subclasses ${cname} ${file} ${beg_line} ${end_line}
        }

        switch ${visib} {
            "class" -
            "baseclass" {
                    set vis [convert_sym_to_bit "${vpublic} ${vprotected}\
                      ${vprivate}"]
                }
            "subclass" {
                    set vis [convert_sym_to_bit "${vpublic} ${vprotected}"]
                }
            default {
                    set vis [convert_sym_to_bit "${vpublic} ${vprotected}\
                      ${vprivate}"]
                }
        }
        set db_types ""
        foreach w ${type} {
            if {[::info commands paf_db_${w}] != ""} {
                lappend db_types ${w}
            }
        }
        set type ${db_types}

        #we make filtering in the c procedure by displaying the members
        #by using global variable
        set total ""

        #read members for the browsed class
        #we don't need to display the class name of
        #the browsed class
        set bitm [$itk_component(classtree) itemcget 0 -image]
        if {[string first "enabled" ${bitm}] != -1} {
            foreach w ${type} {
                set app [list "\(${w}\) "]
                #Line format:
                #0. Member
                #1. Parameter
                #2. Counter
                #3. Type
                #4. Class
                #5. file name
                #6. file position
                #7. flags
                foreach r [paf_db_${w} seq -end ${file} -col [list "1 ${app}"\
                  {7 "| 0000 "} {6 "| \{\} "} 3 "2 -${beg_line}:${end_line} "\
                  "5 :0:${vis} "] "${cname}${sn_sep}"] {
                    lappend total ${r}
                }
            }
        }

        #read members for every base class and add it's contents in a list
        set cou 1
        foreach c ${base_classes} {
            if {[lsearch -exact ${base_classes} ${c}] == -1} {
                continue
            }
            switch ${visib} {
                "baseclass" {
                        set vis [convert_sym_to_bit "${vpublic} ${vprotected}\
                          ${vprivate}"]
                    }
                "class" {
                        set vis [convert_sym_to_bit "${vpublic} ${vprotected}"]
                    }
                "subclass" {
                        if {$base_class_in(${c}) == $SN_Scope_Bits(private)} {
                            continue
                        }
                        set vis [convert_sym_to_bit "${vpublic} ${vprotected}"]
                    }
                default {
                        set vis [convert_sym_to_bit "${vpublic} ${vprotected}\
                          ${vprivate}"]
                    }
            }
            foreach w ${type} {
                set app [list "\(${w}\) "]
                set n [format %04d ${cou}]
                #Line format:
                #0. Member
                #2. Parameter
                #3. Counter
                #1. Type
                #4. Class
                #5. file name
                #6. file position
                #7. flags
                eval lappend total [paf_db_${w} seq -col [list "1 ${app}"\
                  "7 \"| ${n} \"" "6 \"| \"" 0 3 2 "5 :0:${vis}"]\
                  "${c}${sn_sep}"]
            }
            incr cou
        }
        return [::lsort -command sn_compare ${total}]
    }

    protected variable last_browsed_class ""

    # Refill the combobox and redisplay the class browser.
    method RefreshAll {{cls_name ""} {loc ""} {vis ""} {flags ""} {reset 0}} {
        # Reinsert the class names into combobox.
        fill_combo

        # Restore flag settings.
        if {${flags} != ""} {
            set i 0
            foreach p ${mixer_vars} {
                upvar #0 ${this}-mixer-${p} mv
                set mv [lindex ${flags} ${i}]
                incr i
            }
        }

        # Redisplay the browsed class.
        refresh ${cls_name} ${loc} ${reset}
    }

    # Because the combobox call the function with combo and with
    # the selected text, make a different function.
    method ComboRefresh_cb {classtype} {
        refresh
    }

    method refresh {{cls_name ""} {loc ""} {reset 0}} {
        # No class is selected.
        if {${cls_name} == ""} {
            set cls_name ${class_name}
        }

        if {${cls_name} == ""} {
            # No class specified.
            return 1
        }

        #verify if class is availiable and return it's position
        if {${last_browsed_class} != ${cls_name} || ${reset}} {
            if {${loc} == ""} {
                #get the location of the browsed class
                set cnt [read_matched_from_db "" cl -exact ${cls_name}]
                if {[llength ${cnt}] > 1} {
                    set cnt [lindex ${cnt} 0]
                }
                if {${cnt} == ""} {
                    #class not availiable
                    return 1
                } else {
                    set cnt [split ${cnt} \t]
                }
                set location [lrange ${cnt} 4 end]
            } else {
                set location ${loc}
            }
            catch {unset all_base_classes}
        }
        if {${cls_name} != "" && ${class_name} != ${cls_name}} {
            set class_name ${cls_name}
        }

        # Can never happen !!!
        if {${location} == ""} {
            catch {$itk_component(classtree) remove 0}
            ${this} configure -contents ""
            set last_browsed_class ""
            set base_classes ""
            set all_base_classes ""
            catch {unset base_class_in}
            catch {unset base_class_templ}
            return 1
        }

        # Get needed values from member filter.
        set int ""
        foreach p [lrange ${mixer_vars} 6 end] {
            upvar #0 ${this}-mixer-${p} mv
            if {${mv}} {
                lappend int ${p}
            }
        }

        set int [convert_sym_to_bit ${int}]
        if {"${int}" == ""} {
            set int 0
        }

        set member_filter ${int}

        # Mark last listed class.
        set last_browsed_class ${cls_name}

        # Build scopes for class members to be displayed.
        set qry ""
        upvar #0 ${this}-mixer-methods methods
        upvar #0 ${this}-mixer-instvar instvar
        upvar #0 ${this}-mixer-friends friends
        if {${methods}} {
            lappend qry md
        }
        if {${instvar}} {
            lappend qry iv
        }
        if {${friends}} {
            lappend qry fr
        }
        if {${qry} == ""} {
            set qry [list md iv fr]
        }

        # Display members into the member list.
        set membersData [${this} class_members ${cls_name}\
          ${location} ${visible} ${qry}]

	$this configure -contents $membersData

        SetTitle
        if {$itk_option(-symbols) != ""} {
            $itk_option(-symbols) selecttext ${cls_name}
        }
        return 0
    }

    method classname {} {
        return ${class_name}
    }

    #
    #procedure to call the class browser
    #####################################
    proc Browse_Class {{name ""} {gotosymbol_str ""}} {
        global sn_options
        global tkeWinNumber

        set mwin [MultiWindow&::find_Reusable]
        if {![winfo exists ${mwin}]} {
            #create a new multiple window
            incr tkeWinNumber
            set mwin .multiwindow-${tkeWinNumber}
            MultiWindow& ${mwin} -raise classbr
            set active_wdg [${mwin} ActiveWidget]
        } else {
            #toggle to the class browser and display the
            #class
            ${mwin} view classbr

            set active_wdg [${mwin} ActiveWidget]
        }

        #goto method or class
        if {${gotosymbol_str} != ""} {
            eval ${active_wdg} gotosymbol ${gotosymbol_str}
            #goto only to classes
        } else {
            ${active_wdg} gotosymbol cl ${name}
            return
            set ret [${active_wdg} goto "" ${name}]
            if {${ret}} {
                #no class with the specified name
                bell
                return
            }
            sn_add_history class ${name} [sn_make_history_title browse\
              "" ${name}] sn_classbrowser
        }
    }

    method print {} {
        $itk_component(memberlist) print_dialog_box
    }

    method filter {{all 0}} {
        if {$itk_option(-symbols) == ""} {
            return
        }
        upvar #0 $itk_option(-symbols_filter)-related related

        # Verify if there are classes availiable
        if {[::info commands paf_db_cl] != ""} {
            # Display only related classes
            if {${related}} {
                $itk_option(-symbols) configure -contents [::lunique [::lsort -command sn_compare\
                  [concat ${class_name} ${base_classes}]]]
                set displayed 0
                # Display all classes
            } else {
                $itk_option(-symbols) configure -contents [paf_db_cl seq -col 0]
                set displayed 1
            }
        } else {
            $itk_option(-symbols) configure -contents ""
        }
    }

    # This function is called, when the symbols combobox
    # is called.
    method postcommand {m} {
        if {! ${displayed}} {
            filter
            set displayed 1
        }
    }

    # That meens that the notepad is changed to this view
    # View the related toolbar buttons and enable
    # the menu entries for the class browser.
    method activate {} {
        #display the contents always by activating
        set displayed 0

        if {$itk_option(-menu) != ""} {
            set mn [string range $itk_option(-menu) 0 [expr [string last "." $itk_option(-menu)] - 1]]
            ${mn} entryconfig [get_indep String MultiClass] -state normal
        }

        if {$itk_option(-toolbar) != ""} {
            pack $itk_option(-toolbar).clsfr -side left
        }

        # Display current class name
        if {$itk_option(-symbols) != ""} {
            $itk_option(-symbols) configure -entrytext ${class_name}
        }
    }

    # The notepad is changed to a different
    # view.  Hide the related toolbar buttons and disable
    # the menu entries for the class browser.
    method deactivate {} {

        # Hide filter window, if availiable.
        if {[winfo exists ${this}-mixer]} {
            ${this}-mixer delete
        }

        if {$itk_option(-menu) != ""} {
            set mn [string range $itk_option(-menu) 0 [expr [string last "." $itk_option(-menu)] - 1]]
            ${mn} entryconfig [get_indep String MultiClass] -state disabled
        }

        if {$itk_option(-toolbar) != ""} {
            pack forget $itk_component(toolbar)
        }
    }

    method gotosymbol {{scope ""} {sym ""} {cls ""} {file ""} {from ""} {type\
      ""} {prm ""} {to ""} {always 1}} {

        #goto class
        if {${scope} == "cl" || ${scope} == "" || ${cls} != "" && ${sym} ==\
          ""} {
            return [goto "" ${sym}]
        }

        #here twice must be availiable (class and member)
        if {${sym} == "" || ${cls} == ""} {
            #something wrong!!
            return 1
        }

        #first goto class
        if {[goto "" ${cls}]} {
            #invalid class name
            return 1
        }

        #now we want to select the given member in the list
        if {${scope} == "mi"} {
            #all displayed methods are declarations
            set scope "md"
        }
        set str "${sym}(${scope})\t\t"
        set idx [[$itk_component(memberlist) tree] search -begins ${str} 0]

        if {${idx} != ""} {
            $itk_component(memberlist) selectnum ${idx}
        }

        return 0
    }

    method goto {combo cls} {

        set cls [string trim ${cls}]
        if {${cls} == ""} {
            return 1
        }

        # Already selected.
        if {${class_name} == ${cls}} {
            return 0
        }

        # Dump the current view into the history stack.
        ${topw} history_stack_add_point ${this}

        # Verify if the class is availiable.
        set res [refresh ${cls}]
        if {${res}} {
            # Invalid class name.
            return 1
        }

        # Select the current class in the upper combo box.
        if {$itk_option(-symbols) != ""} {
            $itk_option(-symbols) selecttext ${cls}
        }

        # Class displayed successfull.
        return 0
    }

    # Return selected class or selected member
    method Selection {} {
        global sn_options sn_sep

        # Is a member selected?
        set sel [$itk_component(memberlist) curselection]
        if {${sel} != ""} {
            set txt [Retriever&::convert_to_line $itk_component(memberlist)]
            set txt [split ${txt} \t]
            get_key_and_scope [lindex ${txt} 0] sym scope
            set cls [lindex ${txt} 1]
            if {${cls} == ""} {
                set cls ${class_name}
            }
            set typ [lindex ${txt} 2]
            set prm [lindex ${txt} 3]
            set fil [lindex ${txt} 4]
            set lin [lindex ${txt} 5]

            # Goto definition or implementation (optional).
            if {$sn_options(def,class-goto-imp) == "imp"} {
                if {${scope} == "md"} {
                    set scope "mi"
                    set ffil ""
                    set llin ""
                    set ccls ${cls}
                    set mod 1
                }\
                elseif {${scope} == "fr"} {
                    set scope "fu"
                    set ffil ""
                    set llin ""
                    # Friend impl. has no class name.
                    set ccls ""
                    set mod 1
                } else {
                    set mod 0
                }

                # Get file and position name, if nothing is found
                # about implementation, goto definition.
                if {${mod}} {
                    set pat [string trim "${ccls}${sn_sep}${sym}" " ${sn_sep}"]

                    # Build a search string, so that a match is looked for
                    # on the priority of the options.
                    lappend looking_list [list ${typ} ${prm} ${fil}]
                    lappend looking_list [list ${typ} ${prm} ""]
                    lappend looking_list [list "" ${prm} ${fil}]
                    lappend looking_list [list "" ${prm} ""]
                    lappend looking_list [list "" "" ""]

                    foreach look ${looking_list} {
                        set tt [lindex ${look} 0]
                        set pp [lindex ${look} 1]
                        set ff [lindex ${look} 2]

                        # Don't make the same fetch again!
                        if {[info exists looking_arr(${tt},${pp},${ff})]} {
                            continue
                        }

                        set looking_arr(${tt},${pp},${ff}) 1

                        # 1. Try to find something in the current file.
                        set cnt [read_matched_from_db "" ${scope}\
                          -exact ${pat} ${tt} ${pp} ${ff}]

                        # Give up if nothing is found!!
                        if {${cnt} != ""} {
                            set cnt [split [lindex ${cnt} 0] \t]
                            set typ [lindex ${cnt} 1]
                            set prm [lindex ${cnt} 2]
                            set fil [lindex ${cnt} 4]
                            set lin [lindex ${cnt} 5]
                            set cls ${ccls}
                            break
                        }
                    }
                    catch {unset looking_list}
                    catch {unset looking_arr}
                }
            }

            return [list ${scope} ${sym} ${cls} ${fil} ${lin} ${typ} ${prm} ""]
        }

        # Is a class selected?
        set sel [$itk_component(classtree) curselection]
        if {${sel} != ""} {
            return [list cl [$itk_component(classtree) get ${sel}] "" "" "" "" "" ""]
        }

        # Nothing is selected, return the current browsed class.
        if {${class_name} != ""} {
            return [list cl ${class_name} "" "" "" "" "" ""]
        }
        return ""
    }

    method clearselection {} {
        $itk_component(memberlist) selection clear 0 end
        $itk_component(classtree) selection clear 0 end
    }

    method Focus {} {
        focus [$itk_component(memberlist) tree]
    }

    #make a title for the Dump position, this is usefull
    #for the views stack (prev/next)
    method DumpTitle {{dump ""}} {
        if {${dump} == ""} {
            set dump [Dump]
        }
        set scope [lindex ${dump} 0]
        if {${scope} == "cl"} {
            set cls [lindex ${dump} 1]
            set mtd ""
        } else {
            set mtd [lindex ${dump} 1]
            set cls [lindex ${dump} 2]
        }
        if {${mtd} == ""} {
            return "Class ${cls}"
        } else {
            return "Class ${mtd}(${scope}) ${cls}"
        }
    }

    method AddHistoryFromDump {dump title} {
        set scope [lindex ${dump} 0]
        if {${scope} == "cl"} {
            set cls [lindex ${dump} 1]
            set mtd ""
        } else {
            set mtd [lindex ${dump} 1]
            set cls [lindex ${dump} 2]
        }
        if {${mtd} == ""} {
            set name ${cls}
        } else {
            set name "${mtd}(${scope}) ${cls}"
        }
        sn_add_history class [list "" ${dump}] [sn_make_history_title browse\
          "" ${name}] sn_classbrowser
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

    method Refresh_Display {} {
        global sn_options
        refresh ${class_name} "" 1
    }

    method Update_Layout {} {
        global sn_options
    }

    method Close {{mode 0}} {
        return 1
    }

    method whoami {} {
        return classbr
    }

    method fillcbwrapper {{cls ""} {data ""}} {
	$this fill 0
    }

    method fill {{bsy 1}} {
        global sn_options

        if {![winfo exists [namespace tail ${this}]]} {
            return
        }

        if {${bsy}} {
            $itk_component(memberlist) config -state disabled
        }

        set y [lindex [$itk_component(memberlist) yview] 0]

        set filter [$itk_component(memberlist) getfilter]

        # Check new contents to view only filtered values.
        if {${filter} != "*"} {
	    # There is no option currently for changing
	    # where the pattern filter will be case dependant
	    # or not, so for now lets assume nocase until
	    # we have other a preference option.
	    set nocase 1
            if {${nocase}} {
                set flt [Tree::nocase_glob_pattern ${filter}]
            } else {
                set flt ${filter}
            }

            set show [lmatch -glob 0 $itk_option(-contents) ${flt}]
        } else {
            set show $itk_option(-contents)
        }

        # Display contents in the browse list.
        class_browser_insert \
		-delete $itk_component(memberlist).tree ${show}\
		 [itcl::scope base_classes_of] [itcl::scope sub_classes_of] \
		 [itcl::scope class_viewed] ${overridden}\
                 ${member_filter} $sn_options(def,public-font)\
		 $sn_options(def,protected-font) $sn_options(def,private-font)\
		 ${class_name} ${and}

        $itk_component(memberlist) SyncTabs

        if {${bsy}} {
            $itk_component(memberlist) config -state normal
        }
    }

    protected variable topw
    protected variable displayed 0
    protected variable base_classes ""
    protected variable all_base_classes ""
    protected variable base_class_in ""
    protected variable base_class_templ ""
    protected variable tree ""
    protected variable memlist ""
    protected variable ClassTitle ""
    protected variable mixer_vars "methods instvar friends public private protected\
      static structor inline virtual {pure virtual} overridden overloaded"

    protected variable can

    itk_option define -classname classname Classname "" {
	goto "" $itk_option(-classname)
    }

    public variable location ""
    public variable flags ""
    public variable width 80
    public variable height 30

    public variable class_doubleclickcommand ""
    public variable class_selectcommand ""

    protected variable overridden "0"

    private variable member_filter "0"
    private variable class_name ""
    private variable and ""

    # Array that maps a class name to a boolean
    # to record if a given class has been viewed
    private variable class_viewed

    private variable visible 0

    # Array that maps a class name to a subclass name
    private variable sub_classes_of

    # Array that mapes a class name to its base classes
    private variable base_classes_of

    itk_option define -contents contents Contents "" {
         if {[info exists itk_component(memberlist)]} {
             ${this} fill 0
         }
     }
}

proc sn_classbrowser {{name ""} {gotosymbol_str ""}} {
    global Switch_Is_Enabled
    incr Switch_Is_Enabled -1

    #verify if we want to use the multi window mode?
    Class&::Browse_Class ${name} ${gotosymbol_str}

    incr Switch_Is_Enabled 1
}


