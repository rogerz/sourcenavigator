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
# bugreport.tcl - Display bug info to user
# Copyright (C) 1998 Cygnus Solutions.
#

# Popup a text widget containing the text of the encountered error.

proc display_bug_report {err info} {
    global sn_options
    global auto_path

    set w .tkerrorTrace

    if {[winfo exists ${w}]} {
        return
    }

    sourcenav::Window ${w}
    ${w} configure -title [get_indep String Error] -iconname \
        [get_indep String Error] -borderwidth 3

    set l [label ${w}.errorlab -text [get_indep String ErrorOccured]]
    set f [frame ${w}.textframe]
    set button [button ${w}.button -text [get_indep String ButtonDismiss] \
        -command [list itcl::delete object ${w}]]

    grid $l -row 0 -column 0 -sticky w
    grid $f -row 1 -column 0 -sticky news 
    grid $button -row 2 -column 0

    grid rowconfigure ${w} 1 -weight 1
    grid columnconfigure ${w} 0 -weight 1

    set text [text ${f}.text -relief sunken -bd 2 \
        -xscrollcommand [list ${f}.xscroll set] \
        -yscrollcommand [list ${f}.yscroll set] \
        -setgrid true -width 60 -height 20 -wrap none]
    $text insert 0.0 ${info}
    $text mark set insert 0.0

    set xscroll [scrollbar ${f}.xscroll -orient horizontal \
        -command [list ${f}.text xview]]
    set yscroll [scrollbar ${f}.yscroll -orient vertical \
        -command [list ${f}.text yview]]

    grid ${text} -row 0 -column 0 -sticky news
    grid ${xscroll} -row 1 -column 0 -sticky we
    grid ${yscroll} -row 0 -column 1 -sticky ns

    grid rowconfigure ${f} 0 -weight 1
    grid columnconfigure ${f} 0 -weight 1

    ${w} centerOnScreen
    ${w} grab set
}
