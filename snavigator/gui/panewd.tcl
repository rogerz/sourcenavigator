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
# PanedWindow-1.3.tcl
# ----------------------------------------------------------------------
# Implements a paned window widget using primitive widgets as the
# building blocks.  The PanedWindow command creates mutiple panes
# (each one a frame) that can be resized by the user.  A widget
# command exists to retrieve the names of the created panes.  See the
# USEAGE section for an example.  [incr tcl] is required to use this
# widget.
#
#   PUBLIC ATTRIBUTES:
#
#     -fraction ...... initial percentages of panes (default ".5")
#     -min ........... minimum fraction of the entire window to which
#                      a pane can shrink (default .1)
#     -width ......... width of displayed list
#     -height ........ height of displayed list
#     -number ........ number of panes (default: 2)
#     -orientation ... direction of panes [x|y] (default: y)
#     -handleSide .... end of sash upon which to place handle [begin|end]
#                      "begin" means "top" when the orientation is "x"
#                      (default: end)
#
#   METHODS:
#
#     config ....... used to change public attributes
#     panes ........ return the list of panes created
#     fractions .... return the current list of fractions
#
#   USAGE:
#
#     #!/usr/local/bin/itcl_wish -f
#     source PanedWin.tcl
#     PanedWindow .pw -width 300 -height 300 -min .15 -number 4 \
#     	  -handleSide begin
#     # OR
#     # PanedWindow .pw -width 300 -height 300 -fraction ".2 .5 .7" \
#     #     -orientation x
#
#     foreach pane [.pw panes] {
#        button $pane.b -text $pane -command {puts "[.pw fractions]"}
#        pack $pane.b -fill both -expand yes
#     }
#     pack .pw -fill both -expand yes
#     wm minsize . 0 0
#
#
#   X11 OPTION DATABASE ATTRIBUTES
#
#     ...and the rest of the usual widget attributes
#
#   KNOWN PROBLEMS:
#
#   If you set the initial fractions in conflict with the min size,
#   the initial fractions win.  This can cause problems if the main
#   paned window is smaller than $number*$min.
#
#
#   ACKNOWLEDGEMENTS:
#     James Noble
#	1993 - unknown
#     Jay Schmidgall
#	1994 - base logic posted to comp.lang.tcl
#     Mark L. Ulferts <mulferts&spd.dsccc.com>
#	1994 - added additional features and made it a class.
#     Joe Hidebrand <hildjj&fuentez.com>
#	07/25/94 - posted first multipane version to comp.lang.tcl
#	07/28/94 - added support for vertical panes
#
# ----------------------------------------------------------------------
#   AUTHOR:
#
#   Joe Hildebrand                  Fuentez Systems Concepts
#   hildjj&fuentez.com              11781 Lee-Jackson Hwy, Suite 700
#   Software Engineer               Fairfax, VA 22033
#                                   Phone:    (703)273-1447
#                                   Fax:      (703)273-2972
#                                   PGPprint: 0x06C96661
# ----------------------------------------------------------------------



itcl::class PanedWindow {
    inherit itk::Widget
    # ------------------------------------------------------------------
    #  CONSTRUCTOR - create new paned window
    # ------------------------------------------------------------------
    constructor {args} {
        # set class [${this} info class]

        frame $itk_component(hull).pw

        eval itk_initialize $args

        #
        # Make the windows
        #
        if {$itk_option(-orientation) == "y"} {
            set curs "sb_v_double_arrow"
        } else {
            set curs "sb_h_double_arrow"
        }
        for {set i 0} {${i} < $itk_option(-number)} {incr i} {
            if {${i} != 0} {
                #
                # Make the sash button
                #
                frame $itk_component(hull).handle${i} -width 10 -height 10 -borderwidth 2\
                  -relief raised -cursor ${curs}

                if {$itk_option(-orientation) == "y"} {
                    bind $itk_component(hull).handle${i} <Button-1> "${this} start-grip %y\
                      ${i}"
                    bind $itk_component(hull).handle${i} <B1-Motion> "${this} handle-grip\
                      %y ${i}"
                    bind $itk_component(hull).handle${i} <B1-ButtonRelease-1>\
                      "${this} end-grip %y ${i}"
                } else {
                    bind $itk_component(hull).handle${i} <Button-1> "${this} start-grip %x\
                      ${i}"
                    bind $itk_component(hull).handle${i} <B1-Motion> "${this} handle-grip\
                      %x ${i}"
                    bind $itk_component(hull).handle${i} <B1-ButtonRelease-1>\
                      "${this} end-grip %x ${i}"
                }
                balloon_bind_info $itk_component(hull).handle${i} [get_indep String\
                  EINFODragDrop]

                #
                # Make the separator
                #
                frame $itk_component(hull).sep${i} -height 2 -width 2 -borderwidth 1\
                  -relief sunken
            }

            frame $itk_component(hull).pane${i} -borderwidth 2 -relief raised
        }

        pack $itk_component(hull).pw -fill both -expand yes -anchor w


        #
        #  Explicitly handle configs that may have been ignored earlier
        #
        #foreach attr ${config} {
        #    config -${attr} [set ${attr}]
        #}

        #
        # also _ensure_ that the number routine is called
        #
        set frac(0) 0
        if {[array size frac] < [expr $itk_option(-number) + 1]} {
            config -number $itk_option(-number)
        }\
        elseif {[array size frac] > [expr $itk_option(-number) + 1]} {
            config -fraction $itk_option(-fraction)
        }

        replace
    }

    # ------------------------------------------------------------------
    #  METHOD:  config - used to change public attributes
    # ------------------------------------------------------------------
    method config {config} { }

    # ------------------------------------------------------------------
    #  DESTRUCTOR - destroy window containing widget
    # ------------------------------------------------------------------
    destructor {
        catch {::rename $itk_component(hull)-win- {}}
        catch {destroy ${this}}
    }

    # ------------------------------------------------------------------
    #  METHOD calc-fraction - determines the fraction for the sash
    # ------------------------------------------------------------------
    method calc-fraction {where num} {

        if {$itk_option(-orientation) == "y"} {
            set frac(${num}) [expr {(double(${where} - ${drag_start})
              / [winfo height $itk_component(hull)]) + $frac(${num})}]
        } else {
            set frac(${num}) [expr {(double(${where} - ${drag_start})
              / [winfo width $itk_component(hull)]) + $frac(${num})}]
        }

        set min $itk_option(-min)
        if {$frac(${num}) < ($frac([expr ${num} - 1]) + ${min})} {
            set frac(${num}) [expr {$frac([expr ${num} - 1]) + ${min}}]
        }
        if {$frac(${num}) > ($frac([expr ${num} + 1]) - ${min})} {
            set frac(${num}) [expr {$frac([expr ${num} + 1]) - ${min}}]
        }
    }

    # ------------------------------------------------------------------
    #  METHOD start-grip - Starts the sash drag and drop operation
    # ------------------------------------------------------------------
    method start-grip {where num} {
        grab $itk_component(hull).handle${num}
        raise $itk_component(hull).sep${num}
        raise $itk_component(hull).handle${num}
        $itk_component(hull).handle${num} configure -relief sunken
        set drag_start ${where}
    }

    # ------------------------------------------------------------------
    #  METHOD end-grip - Ends the sash drag and drop operation
    # ------------------------------------------------------------------
    method end-grip {where num} {
        calc-fraction ${where} ${num}
        $itk_component(hull).handle${num} configure -relief raised
        grab release $itk_component(hull).handle${num}
        replace
    }

    # ------------------------------------------------------------------
    #  METHOD handle-grip - Motion action for sash
    # ------------------------------------------------------------------
    method handle-grip {where num} {
        calc-fraction ${where} ${num}

        if {$itk_option(-orientation) == "y"} {
            place $itk_component(hull).sep${num} -in $itk_component(hull).pw -relx 0 -relwidth 1\
              -rely $frac(${num}) -anchor w
            place $itk_component(hull).handle${num} -in $itk_component(hull).pw -relx ${handlePos}\
              -rely $frac(${num}) -anchor center
        } else {
            place $itk_component(hull).sep${num} -in $itk_component(hull).pw -rely 0 -relheight 1\
              -relx $frac(${num}) -anchor n
            place $itk_component(hull).handle${num} -in $itk_component(hull).pw -rely ${handlePos}\
              -relx $frac(${num}) -anchor center
        }
    }

    # ------------------------------------------------------------------
    #  METHOD replace - Resets the panes of the window following
    #                   movement of the sash
    # ------------------------------------------------------------------
    method replace {} {
        if {[winfo exists $itk_component(hull).pane0]} {

            if {$itk_option(-orientation) == "y"} {
                place $itk_component(hull).pane0 -in $itk_component(hull).pw -x 0 -rely 0 -relwidth 1\
                  -relheight $frac(1)

                for {set i 1} {${i} < $itk_option(-number)} {incr i} {
                    place $itk_component(hull).sep${i} -in $itk_component(hull).pw -relx 0 -relwidth 1\
                      -rely $frac(${i}) -anchor w
                    place $itk_component(hull).handle${i} -in $itk_component(hull).pw -relx ${handlePos}\
                      -rely $frac(${i}) -anchor center
                    place $itk_component(hull).pane${i} -in $itk_component(hull).pw -x 0\
                      -rely $frac(${i}) -relwidth 1 -relheight\
                      [expr $frac([expr ${i} + 1]) - $frac(${i})]

                    lower $itk_component(hull).sep${i}
                    raise $itk_component(hull).handle${i}
                }
            } else {
                place $itk_component(hull).pane0 -in $itk_component(hull).pw -y 0 -relx 0 -relheight 1\
                  -relwidth $frac(1)

                for {set i 1} {${i} < $itk_option(-number)} {incr i} {
                    place $itk_component(hull).sep${i} -in $itk_component(hull).pw -rely 0 -relheight 1\
                      -relx $frac(${i}) -anchor w
                    place $itk_component(hull).handle${i} -in $itk_component(hull).pw -rely ${handlePos}\
                      -relx $frac(${i}) -anchor center
                    place $itk_component(hull).pane${i} -in $itk_component(hull).pw -y 0\
                      -relx $frac(${i}) -relheight 1 -relwidth\
                      [expr $frac([expr ${i} + 1]) - $frac(${i})]

                    lower $itk_component(hull).sep${i}
                    raise $itk_component(hull).handle${i}
                }
            }
        }
    }

    # ------------------------------------------------------------------
    #  METHOD panes - Return the list of panes
    # ------------------------------------------------------------------
    method panes {} {
        set panes ""
        for {set i 0} {${i} < $itk_option(-number)} {incr i} {
            lappend panes $itk_component(hull).pane${i}
        }
        return ${panes}
    }

    # ------------------------------------------------------------------
    #  METHOD fractions - Return the current list of fractions
    # ------------------------------------------------------------------
    method fractions {} {
        set fracs ""
        for {set i 1} {${i} < $itk_option(-number)} {incr i} {
            lappend fracs $frac(${i})
        }
        return ${fracs}
    }

    # ------------------------------------------------------------------
    #  ATTRIBUTE minimum - Minimum percentage of a pane
    # ------------------------------------------------------------------
    itk_option define -min min Min 0.1 {
        if {[expr $itk_option(-min) < 0.0 || $itk_option(-min) > 1.0]} {
            error "min size must be 0.0-1.0"
        }
    }

    # ------------------------------------------------------------------
    #  ATTRIBUTE fraction - Initial percentage of visible area
    # ------------------------------------------------------------------
    itk_option define -fraction fraction Fraction {0.5} {
        set i 0
        set frac(0) 0
        foreach f $itk_option(-fraction) {
            incr i
            set frac(${i}) ${f}
            if {$frac(${i}) <= $frac([expr ${i}-1])} {
                error "fractions must be in ascending order"
            }
        }
        incr i
        set frac(${i}) 1

        if {$itk_option(-number) != ${i}} {
            configure -number ${i}
        }
        replace
    }

    # ------------------------------------------------------------------
    #  ATTRIBUTE width - Set the frame width
    # ------------------------------------------------------------------
    itk_option define -width width Width 50 {
        if {[winfo exists $itk_component(hull)]} {
            $itk_component(hull).pw configure -width $itk_option(-width)
        }
    }

    # ------------------------------------------------------------------
    #  ATTRIBUTE height - Set the frame height
    # ------------------------------------------------------------------
    itk_option define -height height Height 50 {
        if {[winfo exists $itk_component(hull)]} {
            $itk_component(hull).pw config -height $itk_option(-height)
        }
    }

    # ------------------------------------------------------------------
    #  ATTRIBUTE number - Set the number of panes
    # ------------------------------------------------------------------
    itk_option define -number number Number 2 {
        if {$itk_option(-number) < 2} {
            error "number of panes must be 2 or more"
        }

        set frac(0) 0

        if {[array size frac] != [expr $itk_option(-number) + 2]} {

            #-fraction not called yet

            set part [expr 1.0 / $itk_option(-number).0]
            for {set i 1} {${i} <= $itk_option(-number)} {incr i} {
                set frac(${i}) [expr ${i} * ${part}]
            }
        }
    }

    # ------------------------------------------------------------------
    #  ATTRIBUTE orientation - Set the pane orientation.  "y" means the
    #  sashes are horizontal (the children are packed from top to
    #  bottom).  "x" gives a vertical orientation.
    # ------------------------------------------------------------------
    itk_option define -orientation orientation Orientation y {
        if {$itk_option(-orientation) != "y" && $itk_option(-orientation) != "x"} {
            error "orientation must be x or y"
        }
    }

    # ------------------------------------------------------------------
    #  ATTRIBUTE handleSide - Side of the sash on which to put the
    #  handle.
    # ------------------------------------------------------------------
    itk_option define -handleSide handleSide HandleSide end {
        if {$itk_option(-handleSide) == "begin"} {
            set handlePos 0.05
        }\
        elseif {$itk_option(-handleSide) == "end"} {
            set handlePos 0.95
        } else {
            error "handleSide must be begin or end"
        }
    }

    protected variable drag_start
    protected variable frac
    protected variable handlePos 0.95
}

