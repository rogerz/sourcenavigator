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

namespace eval sourcenav {}

itcl::class sourcenav::MultiChild {

    inherit itk::Widget
    constructor {args} {
        eval itk_initialize $args
    }

    itk_option define -menu menu Menu ""
    itk_option define -toolbar toolbar Toolbar ""
    itk_option define -selectcommand selectcommand Selectcommand ""
    itk_option define -doubleclickcommand doubleclickcommand Doubleclickcommand ""
    itk_option define -symbols symbols Symbols ""
    itk_option define -symbols_filter symbols_filter Symbols_filter ""
    itk_option define -mesg_area mesg_area Mesg_area ""
    
    # Contains the TextVariable for the message/status bar.
    itk_option define -message_var message_var Message_Var ""

    # Contains the TextVariable for the line number
    # on the statusbar. 
    itk_option define -linenumber_var linenumber_var LineNumber_Var ""

    # The multiwindow which is the container for the child view.
    itk_option define -parent parent Parent ""

    # Contains the next widget, when the window is spliten
    # into more than one area. Simulate a single chain.
    itk_option define -next next Next ""

    method next {} {
        return $itk_option(-next)
    }

}



