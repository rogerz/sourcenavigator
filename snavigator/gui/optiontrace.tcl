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



# This is an option tracing library that will allow
# one to create an itk class that support both
# a -value and a -variable option that will
# automatically reflect variable changes.

namespace eval sourcenav::OptionTrace {
    # This array stores the last known varvalue option
    # for an index (like _lastVar(-variable,$array) -> foo)

    variable _lastVar
}

# This method should be invoked from the -variable configure callback
# that you intend to trace a -value for.

proc sourcenav::OptionTrace::configureOptionTrace { varoption valoption option_array } {
    variable _lastVar

    # Link the local variable "option" to the passed in array
    upvar #0 $option_array option

    sn_log "configureOptionTrace $varoption $valoption \{$option_array\}"

    deleteOptionTrace $varoption $valoption $option_array

    if {$option($varoption) != ""} {
        # When a new varoption (-variable) is configured the current
        # value of the variable becomes the value in the valoption (-value).

        set _lastVar($varoption,$option_array) $option($varoption)
        upvar #0 $_lastVar($varoption,$option_array) link

        if {[info exists link]} {
            sn_log "$varoption set to \"$option($varoption)\",\
                new $valoption is \"$link\""
            set option($valoption) $link
        } else {
            sn_log "$varoption \"$option($varoption)\" is not set"
        }

        # Set up a trace that will update the external
        # variable when the valoption (-value) option changes.

        set scoped_value "${option_array}($valoption)"
        set code [list sourcenav::OptionTrace::internalValueChanged \
            $varoption $valoption $option_array]

        sn_log "running : trace variable \{$scoped_value\} w \{$code\}"
        trace variable $scoped_value w $code

        # Set up a trace that will update the internal
        # variable when the external variable changes

        set code [list sourcenav::OptionTrace::externalValueChanged \
            $varoption $valoption $option_array]

        sn_log "running : trace variable \{$_lastVar($varoption,$option_array)\} w \{$code\}"
        uplevel #0 [list trace variable $_lastVar($varoption,$option_array) w $code]
    }
}

proc sourcenav::OptionTrace::deleteOptionTrace { varoption valoption option_array } {
    variable _lastVar

    # Link the local variable "option" to the passed in array
    upvar #0 $option_array option

    sn_log "deleteOptionTrace $varoption $valoption \{$option_array\} called"

    set scoped_value "${option_array}($valoption)"
    set code [list sourcenav::OptionTrace::internalValueChanged \
        $varoption $valoption $option_array]

    sn_log "running : trace vdelete \{$scoped_value\} w \{$code\}"
    trace vdelete $scoped_value w $code

    if {[info exists _lastVar($varoption,$option_array)] &&
            $_lastVar($varoption,$option_array) != ""} {
        sn_log "deleteOptionTrace : unlinking external\
            variable \"$_lastVar($varoption,$option_array)\""
        set code [list sourcenav::OptionTrace::externalValueChanged \
            $varoption $valoption $option_array]
        sn_log "running : trace vdelete \{$_lastVar($varoption,$option_array)\} w \{$code\}"
        uplevel #0 [list trace vdelete $_lastVar($varoption,$option_array) w $code]
        unset _lastVar($varoption,$option_array)
    } else {
        sn_log "_lastVar($varoption,$option_array) not found"
    }
}


# When the valoption (-value) gets changed, we need to update
# the value of a linked varoption (-variable) option if there is one.

proc sourcenav::OptionTrace::internalValueChanged { varoption valoption option_array name name2 op } {
    # Link the local variable "option" to the passed in array
    upvar #0 $option_array option

    sn_log "internalValueChanged $varoption $valoption \{$option_array\}"

    if {$option($varoption) == ""} {
        error "No external linked variable"
    }

    upvar #0 $option($varoption) link

    sn_log "internalValueChanged updating external variable \
        \"$option($varoption)\" to \"$option($valoption)\""

    set link $option($valoption)
}


# When a linked global variable is changed, we need to update
# the internal valoption (-value) option.

proc sourcenav::OptionTrace::externalValueChanged { varoption valoption option_array name name2 op} {
    # Link the local variable "option" to the passed in array
    upvar #0 $option_array option

    # Link to the value option in the array
    upvar #0 $option($varoption) link

    sn_log "externalValueChanged $varoption $valoption \{$option_array\} : updating internal variable to \"$link\""

    set option($valoption) $link
}
