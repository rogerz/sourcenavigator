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

itcl::class PrintDialog {
    inherit sourcenav::Dialog

    global sn_options

    constructor {args} {
        global sn_options
        global ${this}-wait
        global tcl_platform

        if {${init_withdraw}} {
            ${this} withdraw
        }

        set header_font $sn_options(def,layout-font)

	eval itk_initialize $args

 #       $this configure -header_font $sn_options(def,layout-font)
      
        ${this} title [get_indep String SQLPname]

        frame $itk_component(hull).pageframe -relief {raised} -border {2}

        label $itk_component(hull).pageframe.label -relief {flat} -anchor w\
          -text [get_indep String SQLPform]

        radiobutton $itk_component(hull).pageframe.landscape -relief {flat}\
          -variable $rotate -value 1 -anchor w -text [get_indep String\
          SQLPformland]

        radiobutton $itk_component(hull).pageframe.normal -relief {flat}\
          -variable [itcl::scope rotate] -value 0 -anchor w -text [get_indep String\
          SQLPformnorm]

        #print command
        frame $itk_component(hull).printerframe -relief {raised} -border {2}

        entry $itk_component(hull).printerframe.entry \
	  -textvariable [itcl::scope PrinterCmd] \
          -relief {sunken}
        bind $itk_component(hull).printerframe.entry <1>\
          "$itk_component(hull).printerframe.label select"

        bind $itk_component(hull).printerframe.entry <Return>\
          "$itk_component(hull).actionframe.button_0 invoke"

        # Printer
        radiobutton $itk_component(hull).printerframe.label \
	  -variable [itcl::scope PrintTo] \
          -value printer -relief {flat} -anchor w -text [get_indep String\
          SQLPprintercmd] -command " $itk_component(hull).printerframe.entry config\
          -state normal "

        frame $itk_component(hull).printfile -relief {raised} -border {2}

        # Printfile
        radiobutton $itk_component(hull).printfile.label \
          -variable [itcl::scope PrintTo] \
          -value file -relief {flat} -anchor w -text [get_indep String SQLPfile]

        #Page format
        frame $itk_component(hull).pageformat -relief {raised} -border {2}
        label $itk_component(hull).pageformat.label -relief {flat} -anchor w\
          -text [get_indep String PrtPageFormat]

        set i 0
        foreach n $sn_options(def,page-formats) {
            radiobutton $itk_component(hull).pageformat.frmt${i} -relief {flat}\
              -variable sn_options(def,page-format) -value [lindex ${n} 0]\
              -anchor w -text [lindex ${n} 0] -command " ${this} pageformat\
              [lindex ${n} 0] "
            incr i
        }

        #Print into file
        entry $itk_component(hull).printfile.entry \
          -textvariable [itcl::scope file] \
          -relief {sunken}

        bind $itk_component(hull).printfile.entry <Return>\
          "$itk_component(hull).actionframe.button_0 invoke"
        bind $itk_component(hull).printfile.entry <1>\
          "$itk_component(hull).printfile.label invoke"

        frame $itk_component(hull).size -relief {raised} -border {2}

        label $itk_component(hull).size.label -relief {flat} -anchor w -text\
          [get_indep String SQLPprintwidth]

        entry $itk_component(hull).size.width${entry_pars}\
          -textvariable [itcl::scope pagewidth] -relief {sunken} -width {5}

        bind $itk_component(hull).size.width${entry_pars} <Return>\
          "$itk_component(hull).actionframe.button_0 invoke"

        label $itk_component(hull).size.w_label -relief {flat} -anchor w\
          -text [get_indep String SQLPprintheight]

        entry $itk_component(hull).size.height${entry_pars}\
          -textvariable [itcl::scope pageheight] -relief {sunken} -width {5}

        bind $itk_component(hull).size.height${entry_pars} <Return>\
          "$itk_component(hull).actionframe.button_0 invoke"

        label $itk_component(hull).size.unit -relief {flat} -anchor w\
          -textvariable [itcl::scope unit]

        frame $itk_component(hull).coords -relief {raised} -border {2}

        label $itk_component(hull).coords.leftm_label -relief {flat} -anchor w\
          -text [get_indep String SQLPprintleftm]

        entry $itk_component(hull).coords.leftm_entry${entry_pars}\
          -textvariable [itcl::scope pagex] -relief sunken -width 4

        bind $itk_component(hull).coords.leftm_entry${entry_pars} <Return>\
          "$itk_component(hull).actionframe.button_0 invoke"

        label $itk_component(hull).coords.rightm_label -relief {flat} -anchor w\
          -text [get_indep String ODDReportrightm]

        entry $itk_component(hull).coords.rightm_entry${entry_pars}\
          -textvariable [itcl::scope pager] -relief sunken -width 4

        bind $itk_component(hull).coords.rightm_entry${entry_pars} <Return>\
          "$itk_component(hull).actionframe.button_0 invoke"

        label $itk_component(hull).coords.topm_label -relief {flat} -anchor w\
          -text [get_indep String SQLPprinttopm]

        entry $itk_component(hull).coords.topm_entry${entry_pars}\
          -textvariable [itcl::scope pagey] -relief {sunken} -width 4

        bind $itk_component(hull).coords.topm_entry${entry_pars} <Return>\
          "$itk_component(hull).actionframe.button_0 invoke"

        label $itk_component(hull).coords.botm_label -relief {flat} -anchor w\
          -text [get_indep String ODDReportbotm]

        entry $itk_component(hull).coords.botm_entry${entry_pars}\
          -textvariable [itcl::scope pageb] -relief {sunken} -width 4

        bind $itk_component(hull).coords.botm_entry${entry_pars} <Return>\
          "$itk_component(hull).actionframe.button_0 invoke"

        ###########################################################
        #PostScript Factor and PageCount ##########################
        ###########################################################
        frame $itk_component(hull).ps -relief {raised} -border {2}

        label $itk_component(hull).ps.label -relief {flat} -anchor w -text\
          [get_indep String SQLPprintfactor]
        entry $itk_component(hull).ps.factor_entry \
	  -textvariable [itcl::scope factor_entry] \
          -relief {sunken} -width 4
        bind $itk_component(hull).ps.factor_entry <Leave> "${this} CalcPostScriptPages\
          \[string trim \[%W get\]\]"
        bind $itk_component(hull).ps.factor_entry <Return>\
          "$itk_component(hull).actionframe.button_0 invoke"

        label $itk_component(hull).ps.pagev_label -relief {flat} -anchor w\
          -text [get_indep String SQLPprintPageV]
        entry $itk_component(hull).ps.pagev_entry \
	  -textvariable [itcl::scope pagev_entry] \
          -relief {sunken} -width 4

        label $itk_component(hull).ps.pageh_label -relief {flat} -anchor w\
          -text [get_indep String SQLPprintPageH]
        entry $itk_component(hull).ps.pageh_entry \
	  -textvariable [itcl::scope pageh_entry]\
          -relief {sunken} -width 4

        checkbutton $itk_component(hull).ps.pr_pagenum -relief {flat}\
          -variable [itcl::scope pr_pagenum] -anchor w -text [get_indep String\
          SQLPPrintPageNum]

        ###############################################################
        #END ##########################################################
        ###############################################################

        frame $itk_component(hull).range_copy -relief {raised} -border {2}
        label $itk_component(hull).range_copy.range_label -relief {flat} -anchor w\
          -text [get_indep String SQLPprintrange]

        entry $itk_component(hull).range_copy.range_entry \
           -textvariable [itcl::scope range] \
          -relief sunken -width 4

        bind $itk_component(hull).range_copy.range_entry <Return>\
          "$itk_component(hull).actionframe.button_0 invoke"

        label $itk_component(hull).range_copy.copy_label -relief {flat} -anchor w\
          -text [get_indep String SQLPcopies]

        entry $itk_component(hull).range_copy.copy_entry%6_2_0\
          -textvariable [itcl::scope copycount] -relief sunken -width 3

        bind $itk_component(hull).range_copy.copy_entry%6_2_0 <Return>\
          "$itk_component(hull).actionframe.button_0 invoke"

        frame $itk_component(hull).colorframe -relief {raised} -border {2}

        # Farbmodell
        label $itk_component(hull).colorframe.label -relief {flat} -anchor w\
          -text [get_indep String SQLPcolormodell]

        radiobutton $itk_component(hull).colorframe.mono -relief {flat}\
          -variable [itcl::scope colormode] -value mono -anchor w -text\
          [get_indep String SQLPmono]

        radiobutton $itk_component(hull).colorframe.gray -relief {flat}\
          -variable [itcl::scope colormode] -value gray -anchor w -text\
          [get_indep String SQLPgrey]

        radiobutton $itk_component(hull).colorframe.color -relief {flat}\
          -variable [itcl::scope colormode] -value color -anchor w -text\
          [get_indep String SQLPcolor]

        frame $itk_component(hull).actionframe -relief {flat} -border {2}

        # Buttons 
        sn_motif_buttons $itk_component(hull).actionframe bottom 0 [get_indep String\
          SQLPprintit] [get_indep String cancel]

        if {${action} == ""} {
            if {${canvas} != ""} {
                $itk_component(hull).actionframe.button_0 configure -command " ${this}\
                  printit
                  [itcl::code $this deactivate printed]
                "
            } else {
                $itk_component(hull).actionframe.button_0 configure -command " set\
                  [itcl::code $this deactivate printed]"
            }
        } else {
            $itk_component(hull).actionframe.button_0 configure -command "
                    [itcl::code $this deactivate printed]
                    ${action} ${this}
                "
        }

        $itk_component(hull).actionframe.button_1 configure -command "
                [itcl::code $this deactivate]
            "

        #no printer command on windows (using standard font dialog)
        if {$tcl_platform(platform) != "windows"} {
            pack append $itk_component(hull) $itk_component(hull).printerframe {top frame center\
              fillx} $itk_component(hull).printfile {top frame center fillx}
        }

        pack append $itk_component(hull) $itk_component(hull).pageformat {top frame center fillx}\
          $itk_component(hull).size {top frame center fillx} $itk_component(hull).coords\
          {top frame center fillx}
        if {${canvas} != ""} {
            pack append $itk_component(hull) $itk_component(hull).ps {top frame center fillx}
        }
        pack append $itk_component(hull) $itk_component(hull).range_copy {top frame center fillx}\
          $itk_component(hull).pageframe {top frame center fillx}
        if {${colormode} != "none"} {
            pack append $itk_component(hull) $itk_component(hull).colorframe {top frame center\
              fillx}
        }
        pack append $itk_component(hull) $itk_component(hull).actionframe {bottom frame center\
          fillx}

        #view radiobuttons for variable number of page formats
        pack append $itk_component(hull).pageformat $itk_component(hull).pageformat.label\
          {left frame w}
        set i 0
        foreach n $sn_options(def,page-formats) {
            pack append $itk_component(hull).pageformat $itk_component(hull).pageformat.frmt${i}\
              {left frame w}
            incr i
        }

        pack append $itk_component(hull).pageframe $itk_component(hull).pageframe.label\
          {left frame w} $itk_component(hull).pageframe.landscape {left frame w}\
          $itk_component(hull).pageframe.normal {left frame w}

        pack append $itk_component(hull).printerframe $itk_component(hull).printerframe.label\
          {left frame w} $itk_component(hull).printerframe.entry {left frame e fillx\
          expand }

        pack append $itk_component(hull).printfile $itk_component(hull).printfile.label\
          {left frame w} $itk_component(hull).printfile.entry {left frame e fillx expand}

        pack $itk_component(hull).size.label -side left -anchor w
        pack $itk_component(hull).size.width${entry_pars} -side left -anchor w
        pack $itk_component(hull).size.w_label -side left -anchor w
        pack $itk_component(hull).size.height${entry_pars} -side left -anchor w
        pack $itk_component(hull).size.unit -side left -anchor w

        pack $itk_component(hull).coords.leftm_label -side left -anchor w -padx 5
        pack $itk_component(hull).coords.leftm_entry${entry_pars} -side left -anchor w
        pack $itk_component(hull).coords.rightm_label -side left -anchor w -padx 5
        pack $itk_component(hull).coords.rightm_entry${entry_pars} -side left -anchor w
        pack $itk_component(hull).coords.topm_label -side left -anchor w -padx 5
        pack $itk_component(hull).coords.topm_entry${entry_pars} -side left -anchor w
        pack $itk_component(hull).coords.botm_label -side left -anchor w -padx 5
        pack $itk_component(hull).coords.botm_entry${entry_pars} -side left -anchor w

        pack append $itk_component(hull).ps $itk_component(hull).ps.label {left frame w}\
          $itk_component(hull).ps.factor_entry {left frame w} $itk_component(hull).ps.pagev_label\
          {left frame w} $itk_component(hull).ps.pagev_entry {left frame w}\
          $itk_component(hull).ps.pageh_label {left frame w} $itk_component(hull).ps.pageh_entry\
          {left frame w} $itk_component(hull).ps.pr_pagenum {left frame w}

        pack append $itk_component(hull).colorframe $itk_component(hull).colorframe.label { left\
          frame w } $itk_component(hull).colorframe.mono { left frame w }\
          $itk_component(hull).colorframe.gray { left frame w }\
          $itk_component(hull).colorframe.color { left frame w }

        set PrinterCmd ${last_print_cmd}

        # We need the next loop to configure the global variables!
     #   set class [itcl::info class]
     #   foreach pub [itcl::info public] {
     #       if {[string first "${class}::" ${pub}] != -1} {
     #           configure -${pub} [set ${pub}]
     #       }
     #   }
        if {${canvas} != ""} {
            ${this} CalcPostScriptPages ""
        }

        ${this} take_focus

        if {!${init_withdraw}} {
            ${this} centerOnScreen

            catch {${this} resizable no no}

            update idletasks
            set geom [split [lindex [split [${this} geometry] "+"] 0] "x"]
            ${this} minsize [lindex ${geom} 0] [lindex ${geom} 1]
        }
    }

    destructor {
    }

    #deletes an entry from string array
    proc DelArrEntry {array n} {
        upvar ${array} arr

        if {! [info exist arr]} {
            set arr ""
        }

        set i [lsearch ${arr} ${n}]
        if {${i} != -1} {
            set arr [lreplace ${arr} ${i} ${i}]
        }
    }

    proc Round {num} {
        set i [string first . ${num}]
        if {${i} != -1} {
            set mnom [string range ${num} [expr ${i} + 1] end]
            set num [string range ${num} 0 [expr ${i} - 1]]
            if {${mnom} > 0.0} {
                set num [expr ${num} + 1]
            }
        }
        return ${num}
    }

    method CalcPostScriptPages {factor} {

        set fac ${factor}
        DelArrEntry fac "%"

        if {[catch {set fac [expr int(${fac}) + 0]}]} {
            set fac 100
        }

        if {${fac} > 600} {
            set fac 600
        }
        set tfac [expr ${fac} / 100.0]

        #get geometry to center textes
        if {${pr_pagenum}} {
            set id [${canvas} create text 0 0 -text "Page H0 V0"\
              -font ${header_font}]
            set tgeom [${canvas} bbox ${id}]
            set twidth [expr [lindex ${tgeom} 2] - [lindex ${tgeom} 0]]
            set theight [expr [lindex ${tgeom} 3] - [lindex ${tgeom} 1]]
            set step [expr ${theight} + 2]
            #add factor to the step
            #set step    [expr $step * $tfac]
            ${canvas} delete ${id}
        } else {
            set step 0.0
        }

        set scr [${canvas} cget -scrollregion]
        set canv_width [lindex ${scr} 2]
        set canv_height [lindex ${scr} 3]
        if {${canv_width} == ""} {
            return ""
        }

        if {${unit} == "cm"} {
            set snUnit "c"
        } else {
            set snUnit "i"
        }

        if {${rotate} == 0} {
            set pixels_per_pageh [winfo fpixels ${canvas}\
              [set pagewidth]${snUnit}]
            set pixels_per_pagev [winfo fpixels ${canvas}\
              [set pageheight]${snUnit}]
        } else {
            set pixels_per_pagev [winfo fpixels ${canvas}\
              [set pagewidth]${snUnit}]
            set pixels_per_pageh [winfo fpixels ${canvas}\
              [set pageheight]${snUnit}]
        }
        set pages_h [expr [expr ${canv_width} * ${tfac}] / ${pixels_per_pageh}]
        set pages_v [expr [expr ${canv_height} * ${tfac}] / ${pixels_per_pagev}]

        #		set pages_v [expr $pages_v * [expr $fac / 100.0]]
        #		set pages_h [expr $pages_h * [expr $fac / 100.0]]

        if {${pages_v} < 0} {
            set pages_v 1
        }
        if {${pages_h} < 0} {
            set pages_h 1
        }

        #round to next (gerade) number
        set pages_h [Round ${pages_h}]
        set pages_v [Round ${pages_v}]

        $itk_component(hull).ps.factor_entry delete 0 end
        $itk_component(hull).ps.factor_entry insert 0 "${fac}"

        #		$twindow.ps.factor_entry insert end "%"

        $itk_component(hull).ps.pagev_entry delete 0 end
        $itk_component(hull).ps.pagev_entry insert 0 "${pages_v}"

        $itk_component(hull).ps.pageh_entry delete 0 end
        $itk_component(hull).ps.pageh_entry insert 0 "${pages_h}"

        return [list ${pages_v} ${pages_h}]
    }


    method PrintPostScriptPages {} {
        global print_fontmap
        global sn_options

        #move all objects of the canvas to display a title
        if {(${pages_h} > 1 || ${pages_v} > 1) && ${pr_pagenum} != 0} {
            #			foreach it [$canvas find withtag all] {
            #				$canvas move $it 0 $step
            #			}
            ${canvas} move all 0 ${step}
        }
        #move all boxes tow pixels to upper, to center text in the boxes on\
          paper
        #that meen, the outfit is different on SCREEN and on PAPER
        #This is a workaround for a Tk bug.
        ${canvas} move %boxes% 0 -2

        set factor [expr {[set factor_entry] / 100.0}]
        set pixels_hi [expr {${pixels_per_pagev} / ${factor}}]
        set pixels_wi [expr {${pixels_per_pageh} / ${factor}}]

        set t [expr {(${pagew} * ${pixels_hi}) / ${pageh}}]
        if {((${pixels_hi} + ${step}) / ${pageh}) >
                (${pixels_wi} / ${pagew})} {
            set pixels_wi [expr {(${pagew} * ${pixels_hi}) / ${pageh}}]
        }

        set pfd [open [set file] "w+"]
        fconfigure ${pfd} -encoding $sn_options(def,system-encoding) -blocking 0
        set t "_"
        set i 0
        while {${i} < ${pages_h}} {

            set j 0
            while {${j} < ${pages_v}} {

                #horizontal pages
                set xi [expr ${pixels_wi} * ${i}]
                #vertical pages
                set yi [expr ${pixels_hi} * ${j}]
                #set pageanchor "center"

                #print a title if more than one page
                if {(${pages_h} > 1 || ${pages_v} > 1) && ${pr_pagenum} != 0} {
                    set geom [list ${xi} ${yi} [expr {${xi} + ${pixels_wi}}]\
                      [expr {${yi} + ${step} - 1}]]
                    set id2 [eval ${canvas} create rect ${geom} -outline red\
                      -fill white -outline white -tags "Printer:Dlg"]
                    set tx [expr {${xi} + ((${pixels_wi} - ${twidth}) / 2)}]
                    set ty [expr {${yi} + ((${step} - ${theight}) / 2)}]
                    set id1 [${canvas} create text ${tx} ${ty}\
                      -font ${header_font} -text "[get_indep String SQLPPage]\
                      [expr ${j} + 1]-[expr ${i} + 1]" -anchor nw\
                      -tag "Printer:Dlg"]
                    ${canvas} raise ${id1} ${id2}

                    #wait to draw the new widgets
                    update
                }

                #				eval
                puts ${pfd} [${canvas} postscript -x ${xi} -y ${yi}\
                  -width ${pixels_wi} -height [expr ${pixels_hi} + ${step}]\
                  -pagex ${leftm}${unit} -pagey ${topm}${unit} -rotate\
                  [set rotate] -colormode [set colormode]\
                  -fontmap print_fontmap -pagewidth ${pagew}${unit}\
                  -pageheight ${pageh}${unit} -pageanchor ${pageanchor}]
                #						-file [set $this-file]$j$t$i.ps
                incr j
            }
            incr i
        }
        close ${pfd}

        #move back objects of the canvas to original coordinates
        set test 0
        if {${test} == 0} {
            if {(${pages_h} > 1 || ${pages_v} > 1) && ${pr_pagenum} != 0} {
                #remove all created items on the canvas created by printer
                ${canvas} delete Printer:Dlg
                #			foreach it [$canvas find withtag all] {
                #				$canvas move $it 0 -${step}
                #			}
                ${canvas} move all 0 -${step}
            }
        }
        ${canvas} move %boxes% 0 2
    }

    method postscript_parameters {} {

        if {${rotate} == 0} {
        } else {
        }

        set rotate [set rotate]
        if {${rotate}} {
            set topm    [set pagex]
            set leftm   [set pagey]
            set rightm  [set pageb]
            set botm    [set pager]

            set pageh   [expr {([set ${this}-pagewidth] - ${topm}) - ${botm}}]
            set pagew   [expr {([set ${this}-pageheight] - ${leftm}) - ${rightm}}]
        } else {
            set topm    [expr {[set pageheight] - [set pagey]}]
           #set topm    [set pagey]
            set leftm   [set pagex]
            set rightm  [set pager]
            set botm    [set pageb]

            set pagew   [expr {([set pagewidth] - ${leftm})- ${rightm}}]
            set pageh   [expr ${topm} - ${botm}]
        }
        ##scale the returned values to Printer DPI's
        set pagew [expr ${pagew} * ${scr_factor}]
        set pageh [expr ${pageh} * ${scr_factor}]

        set pars "-colormode [set colormode] -file [set file]\
          -pagex ${leftm}${unit} -pagey ${topm}${unit} -rotate ${rotate}\
          -pageanchor ${pageanchor} -x ${x} -y ${y} "

        set scr [lindex [${canvas} config -scrollregion] 4]
        set x_size [lindex ${scr} 2]
        set y_size [lindex ${scr} 3]

        if {[winfo fpixels ${canvas} [set pageheight]${unit}] <\
          ${y_size}} {
            set pgh [expr [set pageheight] - [set pagey]]
            append pars "-width ${x_size} -height ${y_size}\
              -pageheight ${pgh}${unit}"
        }\
        elseif {[winfo fpixels ${canvas} [set pagewidth]${unit}] <\
          ${x_size}} {
            set pgh [expr [set pagewidth] - [set pagex]]
            append pars "-width ${x_size} -height ${y_size}\
              -pagewidth ${pgh}${unit}"
        } else {
            append pars "-width [set pagewidth]${unit} -height\
              [set pageheight]${unit}"
        }

        if {[CalcPostScriptPages [set factor_entry]] == ""} {
            return ""
        }

        return ${pars}
    }

    method printit {} {
        global sn_options
        global env sn_debug tcl_platform

        if {${no_show_lines} && [string first "color" [winfo visual .]] !=\
          -1 && [set colormode] == "mono"} {

            set map 1
        } else {
            set map 0
        }

        if {${map}} {
            set pars ""
            foreach it [${canvas} find withtag all] {
                set type [${canvas} type ${it}]
                switch ${type} {
                    "rectangle" {
                            lappend pars [list ${it} -outline [lindex\
                              [${canvas} itemconfigure ${it} -outline] 4]]
                            ${canvas} itemconfigure ${it}\
                              -outline $sn_options(def,print-form-fg)
                        }
                    "line" {
                            lappend pars [list ${it} -fill [lindex\
                              [${canvas} itemconfigure ${it} -fill] 4]]
                            ${canvas} itemconfigure ${it}\
                              -fill $sn_options(def,print-form-bg)
                        }
                    "text" {
                            lappend pars [list ${it} -fill [lindex\
                              [${canvas} itemconfigure ${it} -fill] 4]]
                            ${canvas} itemconfigure ${it} -fill Black
                        }
                }
            }
        }

        set ps_cmd [postscript_parameters]
        sn_log "postscript parameters: <${ps_cmd}>"

        if {$tcl_platform(platform) != "windows" && ${ps_cmd} == ""} {
            bell -displayof ${canvas}
            return
        }

        if {$tcl_platform(platform) != "windows"} {
            set print_cmd [set PrinterCmd]
            # Escape [] and $ in user input for eval in sn_print_file
            regsub -all {(\$|\[|\])} $print_cmd {\\&} print_cmd
            set file [set file]
            set file [file nativename ${file}]
        } else {
            set file [sn_tmpFileName]
            set file ${file}
            #dummy
            set print_cmd ide_winprint
        }

        #print postscript into the specified file
        ${this} PrintPostScriptPages

        if {${map}} {
            foreach cmd ${pars} {
                eval ${canvas} itemconfigure ${cmd}
            }
        }

        if {$tcl_platform(platform) == "windows"} {
            sn_print_file ${print_cmd} ${file} -postscript
            catch {file delete -force -- ${file}}
        }\
        elseif {[set PrintTo] == "printer" && [string compare\
          ${print_cmd} ""] != 0} {
            sn_print_file ${print_cmd} ${file}
        }
    }

    public variable no_show_lines 0

    public variable colormap {}
    public variable colormode {mono}
    public variable file {} {
        global sn_options

        if {[string compare ${file} ""] == 0} {
            set file [file join $sn_options(profile_dir) print.ps]
        }
    }

    public variable fontmap {} {}

    public variable pageanchor {nw} {}

    public variable pageheight {29.7} {
        global ${this}-pageheight
        set ${this}-pageheight ${pageheight}
    }

    public variable pagewidth {21} {}

    public variable pagex {0.5} {}

    public variable pager {0.5} {
        global ${this}-pager
        set ${this}-pager ${pager}
    }

    public variable pagey {1.0} {}

    public variable pageb {1.0} {}

    public variable rotate {0} {
    }

    public variable x {0}

    public variable y {0}

    public variable PrintTo {file} {}

    public variable PrinterCmd "" {
        global sn_options

        if {[string compare ${PrinterCmd} ""] == 0} {
            catch {set PrinterCmd $sn_options(def,print-command)}
        }

        if {[string compare ${PrinterCmd} ""] != 0} {
            set state normal
        } else {
            set state disabled
        }

        if {[winfo exists $itk_component(hull).printerframe.entry]} {
            $itk_component(hull).printerframe.entry config -state ${state}
        }
    }

    public variable range [get_indep String SQLPprintall] {}

    public variable copycount {1} {}

    public variable pageformat "" {
        global sn_options

        if {$sn_options(def,page-format) == ""} {
            set sn_options(def,page-format) [lindex\
              [lindex $sn_options(def,page-formats) 0] 0]
        }

        if {${pageformat} != ""} {
            set sn_options(def,page-format) ${pageformat}
        } else {
            set pageformat $sn_options(def,page-format)
        }

        pageformat ${pageformat}
    }

    method pageformat {fmt} {
        global sn_options

        upvar #0 sn_options(def,page-format) pf

        set pageformat ${pf}

        set fnd 0
        foreach n $sn_options(def,page-formats) {
            if {[lindex ${n} 0] == ${fmt}} {
                set pagewidth [lindex ${n} 1]
                set pageheight [lindex ${n} 2]
                set unit [lindex ${n} 3]
                set fnd 1
            }
        }
        if {${fnd} == 0} {
            bell
            return
        }
        if {${unit} == "cm"} {
            set unit "c"
        } else {
            set unit "i"
        }

        if {${unit} == "i"} {
            set unit "inch"
            set pagex 0.2
            set pager 0.2
            set pagey 0.4
            set pageb 0.4
        } else {
            set unit "cm"
            set pagex 0.5
            set pager 0.5
            set pagey 1.0
            set pageb 1.0
        }
	if {[winfo exists $itk_component(hull).ps.factor_entry]} {
	    CalcPostScriptPages ${factor_entry}
	}
    }

    public variable canvas {}
    public variable action {}
    public variable unit {c}
    protected variable busy {}
    protected variable entry_pars "%1_6_2"
    common last_print_cmd {}
    public variable init_withdraw 0
    public variable grab 1

    common scr_factor 1.0
    protected variable x_size
    protected variable y_size
    protected variable pixels_per_pageh
    protected variable pixels_per_pagev
    protected variable pages_v
    protected variable pages_h
    protected variable topm
    protected variable leftm
    protected variable rightm
    protected variable botm
    protected variable pageh
    protected variable pagew
    protected variable tx
    protected variable ty
    protected variable twidth
    protected variable theight
    protected variable step
    protected variable pr_pagenum 1
    common header_font ""

    private variable pagev_entry ""
    private variable pageh_entry ""
    private variable factor_entry ""
}

proc custom_pos {} {
    global lay_pos

    if {[info exists rep_txt_pos] == 1} {
        return
    }

    set lay_pos [sourcenav::Window::next_window_name]
    sourcenav::Window ${lay_pos}
    ${lay_pos} withdraw
    ${lay_pos} configure -title [get_indep String ODDReportFieldPosTitle]

    set obj ${lay_pos}.fr_pos
    frame ${obj}
    frame ${obj}.tmp -bd 5
    text ${obj}.tmp.text_pos -relief raised -bd 2\
      -yscrollcommand "${obj}.tmp.scroll set" -width 40 -height 10
    scrollbar ${obj}.tmp.scroll -command " ${obj}.tmp.text_pos yview "
    label ${obj}.tmp.l -text [get_indep String ODDReportFieldPos] -padx 10
    pack ${obj}.tmp.l -side left
    pack ${obj}.tmp.scroll -side right -fill y
    pack ${obj}.tmp.text_pos -side left -fill both -expand y
    pack ${obj}.tmp -side top -fill both -expand y
    frame ${obj}.butt -bd 5
    button ${obj}.butt.ok -text [get_indep String ODDReportTakeOver]\
      -relief raised -command { get_pos
            unset lay_pos
            unset rep_names_pos } -width 10
    button ${obj}.butt.his -text [get_indep String ODDSapHistory]\
      -relief raised -command " SQLHistory " -width 10
    button ${obj}.butt.cancel -text [get_indep String cancel] -relief raised\
      -command { itcl::delete object ${lay_pos}
            unset lay_pos
            unset rep_names_pos } -width 10
    pack ${obj}.butt.ok -side left -expand y
    pack ${obj}.butt.his -side left -expand y
    pack ${obj}.butt.cancel -side left -expand y
    pack ${obj}.butt -side top -fill x
    pack ${obj} -side bottom -fill both -expand y
    ${lay_pos} move_to_mouse
    ${lay_pos} on_close "itcl::delete object ${lay_pos}; unset lay_pos; unset rep_names_pos"
    put_field_names
}

proc put_field_names {} {
    global lay_pos
    global field_names_pos
    global rep_names_pos
    global rep_txt_pos

    set new_field_name ""
    if {${field_names_pos} != ""} {
        foreach elem ${field_names_pos} {
            set new_field_name [lappend new_field_name ${elem}\n]
        }
        regsub -all "\{" ${new_field_name} "" new_field_name
        regsub -all "\} " ${new_field_name} "" new_field_name
        regsub -all "\}" ${new_field_name} "" new_field_name
        regsub -all "," ${new_field_name} "." new_field_name
        set rep_txt_pos ${new_field_name}
        set rep_names_pos ${rep_txt_pos}
        ${lay_pos}.fr_pos.tmp.text_pos insert 0.0 ${rep_txt_pos}
    }
}

proc get_pos {} {
    global lay_pos
    global rep_txt_pos
    global rep_names_pos

    set rep_txt_pos ""
    set tmp_field_name ""
    if {[winfo exists ${lay_pos}.fr_pos.tmp.text_pos] == 1} {
        set tmp_field_name "[${lay_pos}.fr_pos.tmp.text_pos get 0.0 end]"
    }
    if {${tmp_field_name} != ""} {
        set rep_txt_pos [split ${tmp_field_name} "\n"]
        regsub -all "\{\}" ${rep_txt_pos} "" rep_txt_pos
    }
    regsub -all "," ${rep_txt_pos} "." rep_txt_pos
    set poslen [llength ${rep_txt_pos}]
    for {set anz 0} {${anz} < ${poslen}} {incr anz} {
        set elem [lindex ${rep_txt_pos} ${anz}]
        if {[llength ${elem}] != 3} {
            if {[llength ${elem}] != 1} {
                sn_error_dialog [get_indep String ReportPosError]
                return
            }
        }
        if {[llength ${elem}] == 3} {
            if {![regexp {^([ 0-9. ]+)$|^[ 0-9 ]+$} [lindex ${elem} 1]] ||\
              ![regexp {^([ 0-9. ]+)$|^[ 0-9 ]+$} [lindex ${elem} 2]] ||\
              [lindex ${elem} 1] == "." || [lindex ${elem} 2] == "."} {
                sn_error_dialog [get_indep String ReportPosError]
                return
            }
        }
    }
    set rep_names_pos ${rep_txt_pos}
    set HistString "report ${rep_txt_pos}"
    SQLHistoryAppendCommand ${HistString}
    SQLHistoryWrite

    destroy ${lay_pos}
}

proc REPClear {} {
    global lay_pos

    if {[winfo exists ${lay_pos}.fr_pos.tmp.text_pos] == 1} {
        ${lay_pos}.fr_pos.tmp.text_pos delete 1.0 end
    }
}

proc put_field_pos {field_names} {
    global lay_pos
    global rep_txt_pos
    global rep_names_pos

    set new_field_name ""
    if {${field_names} != ""} {
        set new_field_name [lappend new_field_name ${field_names}\n]
        regsub -all "\{" ${new_field_name} "" new_field_name
        regsub -all "\} " ${new_field_name} "" new_field_name
        regsub -all "\}" ${new_field_name} "" new_field_name
        regsub -all "," ${new_field_name} "." new_field_name
        set rep_txt_pos ${new_field_name}
        set rep_names_pos ${rep_txt_pos}
        ${lay_pos}.fr_pos.tmp.text_pos insert 0.0 ${rep_txt_pos}
    }
    set rep_names_pos ${rep_txt_pos}
    return ${rep_txt_pos}
}

