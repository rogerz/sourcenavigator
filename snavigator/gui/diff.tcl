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
# diff.tcl - Implement a UI on top of the familiar diff(1) tool.
# Copyright (C) 1998-1999 Cygnus Solutions.
#
#   15 April, 2007 - E M Thornber
#   Correct Prev and Next button behaviour
#

itcl::class RCSTopdiff& {
    inherit sourcenav::Window

    protected variable opts
    protected variable g
    protected variable fo
    protected variable fog
    protected variable fn

    constructor {filo filn cfiln tl opt} {
        global sn_options
        global ${this}.currdiff

        set g(cfilo) ""
        set g(cfiln) ""
        set ${this}.currdiff ""
        set g(destroy) ""

        set g(filo) ${filo}
        set g(filn) ${filn}
        set g(cfilo) ${filo}
        set g(cfiln) ${cfiln}

        regsub -all "\[\t \]+" ${opt} " " opt
        set opts(diffopt) [string trim ${opt}]
        if {[string first "color" [winfo screenvisual .]] != -1} {
            set opts(currtag) {-background white -foreground red}
            set opts(difftag) {-background white -foreground blue}
        } else {
            set opts(currtag) {-background black -foreground white}
            set opts(difftag) {-background white -foreground black}
        }
        set opts(expfilo) 1
        set opts(expfiln) 1
        set opts(cmplimt) 100

        global sn_path

        ######################################################################
        # Set up the display...
        ######################################################################
        ${this} configure -title ${tl}

        # Pack the bottom-row buttons (inside .b).
        frame $itk_component(hull).b
        pack $itk_component(hull).b -side bottom -fill x -ipady 2 -ipadx 2

        # Pack the frame that holds the text widgets and scrollbars (.f).

        PanedWindow $itk_component(hull).f -width 900 -height 600 -fraction .5\
          -orientation x -min 0
        pack $itk_component(hull).f -fill both -expand y

        set fog [lindex [$itk_component(hull).f panes] 0]
        set fn [lindex [$itk_component(hull).f panes] 1]

        set fo ${fog}.file
        frame ${fo}

        # Pack the "old" widgets (fo).

        label ${fo}.name -relief groove -bd 2 -text $g(cfilo)
        pack ${fo}.name -side top -fill x

        # Pack the "new" widgets (fn).

        label ${fn}.name -relief groove -bd 2 -text $g(cfiln)
        pack ${fn}.name -side top -fill x

        scrollbar ${fo}.scr -command " ${fo}.text yview "
        scrollbar ${fo}.xscr -command " ${fo}.text xview " -orient horizontal

        text ${fo}.text -yscroll "${this} scrolls-set" -xscroll\
          "${fo}.xscr set" -width 80 -height 40 -wrap none\
          -font $sn_options(def,default-font)

        #xxx
        # Pack the text widgets and the scrollbars.
        pack ${fo}.xscr -side bottom -fill x
        pack ${fo}.scr -side left -fill y
        pack ${fo}.text -side top -fill both -expand y

        scrollbar ${fn}.scr -command " ${fn}.text yview "
        scrollbar ${fn}.xscr -command " ${fn}.text xview " -orient horizontal

        text ${fn}.text -yscroll "${fn}.scr set" -xscroll "${fn}.xscr set"\
          -width 80 -height 40 -wrap none

        # Pack the text widgets and the scrollbars.
        pack ${fn}.xscr -side bottom -fill x
        pack ${fn}.scr -side right -fill y
        pack ${fn}.text -side top -fill both -expand y

        # Pack the meta-scrollbar (.f.scr).

        scrollbar ${fog}.scr -command " ${this} texts-yview "

        pack ${fog}.scr -side right -fill y
        pack ${fo} -side right -fill both -expand y

        # Pack the quit, help, swap and rediff buttons.

        set str [get_indep String Close]
        set len [expr [string length ${str}] + 1]
        button $itk_component(hull).b.quit -text ${str} -command " ${this} quit "\
          -width ${len}
        pack $itk_component(hull).b.quit -side left -fill x -padx 4

        # Pack the next and prev buttons.

        set str [get_indep String Prev]
        set len [expr [string length ${str}] + 1]
        button $itk_component(hull).b.prev -text ${str} -command " ${this} move -1 "\
          -width ${len}
        pack $itk_component(hull).b.prev -side left -fill x -padx 4

        set str [get_indep String Next]
        set len [expr [string length ${str}] + 1]
        button $itk_component(hull).b.next -text ${str} -command " ${this} move 1 "\
          -width ${len}
        pack $itk_component(hull).b.next -side left -fill x -padx 4

        frame $itk_component(hull).b.pos -relief raised
        pack $itk_component(hull).b.pos -side left -padx 4 -ipady 1 -ipadx 1

        menubutton $itk_component(hull).b.pos.menubutton\
          -menu $itk_component(hull).b.pos.menubutton.menu -width 5\
          -textvariable $itk_component(hull).pos -relief raised
        pack $itk_component(hull).b.pos.menubutton -side left

        switch $sn_options(def,desktop-font-size) {
            24 {
                    set fnt 18
                }
            18 {
                    set fnt 14
                }
            14 {
                    set fnt 12
                }
            12 {
                    set fnt 10
                }
            default {
                    set fnt 8
                }
        }

        menu $itk_component(hull).b.pos.menubutton.menu

        label $itk_component(hull).b.pos.nlabel -text [get_indep String Of] -relief flat
        pack $itk_component(hull).b.pos.nlabel -side left

        label $itk_component(hull).b.pos.num -textvariable ${this}.count
        pack $itk_component(hull).b.pos.num -side left

        label $itk_component(hull).b.pos.curr -textvariable ${this}.currdiff -width 30\
          -relief groove
        pack $itk_component(hull).b.pos.curr -side left

        # Give the window a name & allow it to be resized.

        ${this} on_close "$itk_component(hull).b.quit invoke"
        ${this} configure -iconbitmap @$sn_path(bitmapdir)/paf.xbm
        ${this} configure -iconname DIFF

        # Set up text tags for the 'current diff' (the one chosen by the 'next'
        # and 'prev' buttons) and any ol' diff region.	All diff regions are
        # given the 'diff' tag initially...  As 'next' and 'prev' are pressed,
        # to scroll through the differences, one particular diff region is
        # always chosen as the 'current diff', and is set off from the others
        # via the 'diff' tag -- in particular, so that it's obvious which diffs
        # in the left and right-hand text widgets match.

        eval "${fo}.text tag configure curr $opts(currtag)"
        eval "${fn}.text tag configure curr $opts(currtag)"
        eval "${fo}.text tag configure diff $opts(difftag)"
        eval "${fn}.text tag configure diff $opts(difftag)"

        ${this} rediff

        bind ${fo}.text <Double-1> " [info class]::edit_line %W $g(cfilo) "

        ${fo}.text tag raise sel
        ${fn}.text tag raise sel
    }

    proc edit_line {w file} {
        set pos [${w} index "insert linestart"]
        ${w} tag add sel "insert linestart" "insert lineend + 1c"
        selection own ${w} " "
        update idletasks
        sn_edit_file "" ${file} ${pos}
    }

    ###############################################################################
    # Throw up a modal error dialog.
    ###############################################################################

    proc do-error msg {
        sn_error_dialog ${msg}
    }

    ###############################################################################
    # Read (while untabifying) the text in the file "fn" into the variable\
      "var".
    ###############################################################################

    method quit {} {
        itcl::delete object ${this}
    }

    proc untabify {var fn} {
        upvar ${var} v
        global sn_options

        set fd [open ${fn} "r"]
        fconfigure ${fd} -encoding $sn_options(def,encoding) -blocking 0
        set v [read -nonewline ${fd}]
        close ${fd}

        #		set v [exec expand $fn]
    }
    ###############################################################################
    # Scroll all windows.  Credit to Wayne Throop...
    ###############################################################################

    method texts-yview args {
        global ${this}.count
        eval ${fo}.text yview ${args}
        eval ${fn}.text yview ${args}
    }

    ###############################################################################
    # Set all scrollbars.  Credit to Wayne Throop...
    ###############################################################################

    method scrolls-set {a1 a2} {
        catch {
            ${fo}.scr set ${a1} ${a2}
            ${fn}.scr set ${a1} ${a2}
            ${fog}.scr set ${a1} ${a2}
        }
    }

    ###############################################################################
    # Extract the start and end lines for file1 and file2 from the diff
    # stored in "line".
    ###############################################################################

    proc extract line {
        if [regexp {^([0-9]+)(a|c|d)} ${line} d digit action] {
            set s1 ${digit}
            set e1 ${digit}
        }\
        elseif [regexp {^([0-9]+),([0-9]+)(a|c|d)} ${line} d start end action] {
            set s1 ${start}
            set e1 ${end}
        }

        if [regexp {(a|c|d)([0-9]+)$} ${line} d action digit] {
            set s2 ${digit}
            set e2 ${digit}
        }\
        elseif [regexp {(a|c|d)([0-9]+),([0-9]+)$} ${line} d action start end] {
            set s2 ${start}
            set e2 ${end}
        }

        return "${line} ${s1} ${e1} ${s2} ${e2} ${action}"
    }

    ###############################################################################
    # Add a tag to a region.
    ###############################################################################

    proc add-tag {wgt tag start end type new} {
        if {${type} == "c" ||(${type} == "a" && ${new}) ||(${type} == "d" &&\
          !${new})} {
            ${wgt} tag add ${tag} ${start}.0 [expr ${end} + 1].0
        } else {
            for {set idx ${start}} {${idx} <= ${end}} {incr idx} {
                ${wgt} tag add ${tag} ${idx}.0 ${idx}.6

            }
        }
    }

    ###############################################################################
    # Move the "current" diff indicator (i.e. go to the next or previous diff
    # region if "relative" is 1; go to an absolute diff number if "relative"
    # is 0).
    ###############################################################################

    method move {value {relative 1} {setpos 1}} {
        global ${this}.currdiff
        upvar #0 ${this}.pos pos

        scan $g(pdiff,${pos}) "%s %d %d %d %d %s" dummy s1 e1 s2 e2 dt

        # Replace the 'diff' tag (and remove the 'curr' tag) on the current
        # 'current' region.

        ${fo}.text tag remove curr ${s1}.0 [expr ${e1} + 1].0
        ${fn}.text tag remove curr ${s2}.0 [expr ${e2} + 1].0

        add-tag ${fo}.text diff ${s1} ${e1} ${dt} 0
        add-tag ${fn}.text diff ${s2} ${e2} ${dt} 1

        # Bump 'pos' (one way or the other).

        if {${relative}} {
            set pos [expr ${pos} + ${value}]
        } else {
            set pos ${value}
        }

        # Range check 'pos'.

        if {${pos} > [llength $g(diff)]} {
            set pos [llength $g(diff)]
        }
        if {${pos} < 1} {
            set pos 1
        }

        # Figure out which lines we need to address...

        scan $g(pdiff,${pos}) "%s %d %d %d %d %s" currdiff s1 e1 s2 e2 dt
        set ${this}.currdiff ${currdiff}
        # Remove the 'diff' tag and add the 'curr' tag to the new 'current'
        # diff region.

        ${fo}.text tag remove diff ${s1}.0 [expr ${e1} + 1].0
        ${fn}.text tag remove diff ${s2}.0 [expr ${e2} + 1].0

        add-tag ${fo}.text curr ${s1} ${e1} ${dt} 0
        add-tag ${fn}.text curr ${s2} ${e2} ${dt} 1

        # Move the view on both text widgets so that the new region is
        # visible.

        if {${setpos}} {
            ${this} center
        }
    }

    #########################################################################
    # Center the top line of the CDR in each window.
    #########################################################################

    method center {} {
        upvar #0 ${this}.pos pos

        scan $g(pdiff,${pos}) "%s %d %d %d %d %s" dummy s1 e1 s2 e2 dt

        set h [expr int([text_page_num_lines ${fo}.text] / 2)]

        set o [expr ${s1} - ${h} -1]
        if {${o} < 0} {
            set o 0
        }
        set n [expr ${s2} - ${h} -1]
        if {${n} < 0} {
            set n 0
        }

        ${fo}.text yview ${o}
        ${fn}.text yview ${n}
    }

    ##########################################################################
    # Expand filenames by replacing $foo with $env(foo) & 
    # evaluating the result.
    ##########################################################################

    proc expand fn {
        global env

        upvar ${fn} f
        regsub -all {\$([^ /\.]+)} ${f} {$env(\1)} t_m_p
        regsub {^~$} ${t_m_p} {$env(HOME)} t_m_p
        regsub {^~/} ${t_m_p} {$env(HOME)/} t_m_p
        regsub {^~([^/]+)} ${t_m_p} {$env(HOME)/../\1} t_m_p
        catch "set f ${t_m_p}"
    }

    ###############################################################################
    # Finish off a filename if possible.  If not, and the number of\
      possibilities
    # is less than 'max', complete as much of the filename as possible.  Note
    # that the default value for 'max' is 100.
    ###############################################################################

    method finish {fn {max 100}} {
        upvar ${fn} f

        set choices [glob -nocomplain ${f}*]
        set numchoices [llength "${choices}"]

        if {${numchoices} == 1} {
            set f ${choices}
            return 1
        }\
        elseif {${numchoices} > ${max}} {
            return 0
        }

        set choice [lindex "${choices}" 0]
        set min [string length "${choice}"]
        set golden "${choice}"

        for {set idx 1} {${idx} < [llength "${choices}"]} {incr idx} {
            set choice [lindex "${choices}" ${idx}]
            set count [string length ${choice}]
            if {${count} < ${min}} {
                set min ${count}
                set golden "${choice}"
            }
        }

        while {${min} > 0} {
            set match 1
            foreach choice "${choices}" {
                if {[string range "${choice}" 0 ${min}] != "${golden}"} {
                    set match 0
                    break
                }
            }
            if {${match}} {
                if {[string length ${golden}] > 0} {
                    set f "${golden}"
                }
                break

            }
            incr min -1
            set golden [string range "${golden}" 0 ${min}]
        }

        return 0
    }

    #########################################################################
    # Do filename completion by expanding environment variables, resolving
    # directories, and then completing as much of the filename as possible.
    #########################################################################

    method complete {fn {max 100}} {
        global sn_root
        upvar ${fn} f

        expand f
        ${this} finish f ${max}

        if {[file isdirectory ${f}]} {
            set dst ${f}
            set suf {}
        } else {
            set dst [file dirname ${f}]
            set suf [file tail ${f}]
        }

        if {[file isdirectory ${dst}]} {
            set c [pwd]
            catch {cd ${dst}}
            set f [pwd]
            catch {cd ${c}}
            set f [file join ${f} ${suf}]
        }

        set f [string trimright ${f} ${sn_root}]

        if {[llength [glob -nocomplain ${f}*]] < 2 && [file isdirectory ${f}]} {
            append f ${sn_root}
        }
    }

    #########################################################################
    # Change the state on all of the diff-sensitive buttons.
    #########################################################################

    method buttons {{newstate "normal"}} {
        foreach b [list $itk_component(hull).b.pos.menubutton $itk_component(hull).b.next\
          $itk_component(hull).b.prev] {
            eval "${b} configure -state ${newstate}"
        }
    }

    #########################################################################
    # Wipe the slate clean...
    #########################################################################

    method wipe {} {
        global ${this}.pos ${this}.count

        set ${this}.pos 0
        set ${this}.count 0
        set g(cfilo) ""

        ${fo}.text configure -state normal
        ${fo}.text tag remove diff 1.0 end
        ${fo}.text tag remove curr 1.0 end
        ${fo}.text delete 1.0 end
        ${fn}.text configure -state normal
        ${fn}.text tag remove diff 1.0 end
        ${fn}.text tag remove curr 1.0 end
        ${fn}.text delete 1.0 end

        if {[string length $g(destroy)] > 0} {
            eval $g(destroy)
            set g(destroy) ""
        }

        $itk_component(hull).b.pos.menubutton.menu delete 0 last

        ${this} buttons disabled
    }

    #########################################################################
    # Ready the display...
    #########################################################################

    method ready-display {} {
        set g(cfilo) $g(filo)
    }


    #########################################################################
    # Mark difference regions and build up the jump menu.
    #########################################################################

    method mark-diffs {} {
        upvar #0 ${this}.count count

        set different 0
        set numdiff [llength [split "$g(diff)" \n]]

        # If there are <= 30 diffs, do a one-level jump menu.  If there are
        # more than 30, do a two-level jump menu with sqrt(numdiff) in each
        # level.

        if {${numdiff} <= 30} {
            set g(destroy) "$g(destroy) catch  \"eval\
              $itk_component(hull).b.pos.menubutton.menu delete 0 last\"\n"

            foreach d [split $g(diff) \n] {
                ::incr count

                set g(pdiff,${count}) [extract ${d}]

                scan $g(pdiff,${count}) "%s %d %d %d %d %s" dummy s1 e1 s2 e2 dt

                add-tag ${fo}.text diff ${s1} ${e1} ${dt} 0
                add-tag ${fn}.text diff ${s2} ${e2} ${dt} 1

                set different 1

                $itk_component(hull).b.pos.menubutton.menu add command -label [format "%-6d\
                  --> %s" ${count} ${d}] -command " ${this} move ${count} 0 "
            }
        } else {
            set target 0
            set increment [expr int(pow(${numdiff},0.5))]

            foreach d [split "$g(diff)" \n] {
                incr count

                if {${count} >= ${target}} {
                    $itk_component(hull).b.pos.menubutton.menu add cascade -label ${target}\
                      -menu $itk_component(hull).b.pos.menubutton.menu.${target}
                    menu $itk_component(hull).b.pos.menubutton.menu.${target}

                    set current ${target}
                    set target [expr ${target} + ${increment}]

                    set g(destroy) "$g(destroy) catch \"eval\
                      $itk_component(hull).b.pos.menubutton.menu.${current} delete 0\
                      last\"\n  catch \"eval destroy\
                      $itk_component(hull).b.pos.menubutton.menu.${current}\"\n"
                }

                set g(pdiff,${count}) [extract ${d}]

                scan $g(pdiff,${count}) "%s %d %d %d %d %s" dummy s1 e1 s2 e2 dt

                add-tag ${fo}.text diff ${s1} ${e1} ${dt} 0
                add-tag ${fn}.text diff ${s2} ${e2} ${dt} 1

                set different 1

                $itk_component(hull).b.pos.menubutton.menu.${current} add command\
                  -label [format "%-6d --> %s" ${count} ${d}] -command\
                  " ${this} move ${count} 0 "
            }
        }

        return ${different}
    }

    ########################################################################
    # Compute differences (start over, basically).
    ########################################################################

    method rediff {} {
        global sn_debug

        upvar #0 ${this}.count count
        upvar #0 ${this}.pos pos

        ${this} wipe
        ${this} ready-display

        # Read the files into their respective widgets & add line numbers.
        untabify otxt $g(filo)
        ${fo}.text insert 1.0 ${otxt}
        set tgt [expr [lindex [split [${fo}.text index end] .] 0] - 1]
        for {set i 1} {${i} <= ${tgt}} {incr i} {
            ${fo}.text insert ${i}.0 [format "%-8d" ${i}]
        }
        untabify ntxt $g(filn)
        ${fn}.text insert 1.0 ${ntxt}
        set tgt [expr [lindex [split [${fn}.text index end] .] 0] - 1]
        for {set i 1} {${i} <= ${tgt}} {incr i} {
            ${fn}.text insert ${i}.0 [format "%-8d" ${i}]
        }

        # Diff the two files and store the summary lines into 'diff'.

        #   set g(diff) [exec sh -c "diff $opts(diffopt) $g(filo) $g(filn) |
        #		     egrep -v '^(<|>|\-)' ; exit 0"]
        set tmpf [sn_tmpFileName]

        global sn_options sn_verctl_options
        set cmd $sn_verctl_options($sn_options(both,rcs-type),diff-command)

        catch {eval exec ${cmd} $opts(diffopt) [list $g(filo) $g(filn)] >&\
          ${tmpf}}

        if {[catch {set fd [::open ${tmpf} "r"]}]} {
            return
        }
        fconfigure ${fd} -translation binary -blocking 0

        set lst [read -nonewline ${fd}]
        close ${fd}
        file delete -- ${tmpf}

        regsub -all "\r" ${lst} "" lst
        set g(diff) [split ${lst} "\n"]
        set g(diff) [lmatch -regexp $g(diff) {^[^<>\-].*}]
        set g(diff) [join $g(diff) "\n"]

        # Mark up the two text widgets and go to the first diff (if there
        # is one).
        if {[${this} mark-diffs]} {
            set pos 1
            ${this} move 1 0
            ${this} buttons normal
        } else {
            ${this} buttons disabled
        }

        # Prevent tampering in the text widgets.

        ${fo}.text configure -state disabled
        ${fn}.text configure -state disabled
        $itk_component(hull).b.pos.num config -width [string length ${count}]

        if {!${sn_debug}} {
            file delete -- $g(filn)
        }
    }
}

#itcl_class RCSTopdiff& {
#    inherit RCSdiff& Toplevel&
#    constructor args {
###FIXME - this constructor callout will cause the parent constructor to be called twice
#	eval RCSdiff&::constructor $args
#    }
#}


