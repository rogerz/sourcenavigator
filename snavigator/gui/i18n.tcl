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
# i18n.tcl - Internationali[sz]ation routines.
# Copyright (C) 1998 Cygnus Solutions.

# Read a message catalog.

proc sn_string_init {filename} {
    global sn_options
    global sn_strings sn_accels sn_keys

    set f [open ${filename} r]
    fconfigure ${f} -encoding $sn_options(def,system-encoding) -blocking 0

    set i 1
    while {[gets ${f} line] != -1} {
        if {[string trim ${line}] == "" || [regexp "^#" ${line}]} {
            continue
        }
        if {[string first "," ${line}] == -1} {
            sn_log "error in ${filename} on line ${i}"
            continue
        }

        set key [string range ${line} 0 [expr [string first "," ${line}] - 1]]
        set key [string trim ${key}]

        if {[string compare ${key} ""] == 0} {
            continue
        }

        set value [string trim [string range ${line} [expr [string first\
          "," ${line}] + 1] end]]

        set hotkey ""
        # This can set accel to -1, as it should.
        set accel [string first \[ ${value}]

        if {${accel} != -1} {
            # Set the hotkey.
            set hotkey [string index ${value} [expr ${accel} + 1]]
            # Remove "[" from the text.
            regsub {\[} ${value} "" value
        }

        # Resolve Tcl subsitutions that may be entered in the file.
        set value [subst ${value}]

        set sn_strings(${key}) ${value}
        set sn_accels(${key}) ${accel}
        set sn_keys(${key}) ${hotkey}

        incr i
    }
    close ${f}
}

proc get_indep {Type Identifier} {

    global sn_strings
    global sn_accels
    global sn_keys

    if {${Type} == "String"} {
        if {[catch {set str $sn_strings(${Identifier})}]} {

            sn_error_dialog "String \"String\" identifier \"${Identifier}\"\
              doesn't exist"

            set sn_keys(${Identifier}) "UNKNOWN"
            set sn_strings(${Identifier}) "UNKNOWN"
            set sn_accels(${Identifier}) 0

            return "UNKNOWN"
        }
        return ${str}
    }

    if {${Type} == "Accel"} {
        if {[catch {set key $sn_keys(${Identifier})}]} {

            sn_error_dialog "String \"Accel\" identifier \"${Identifier}\"\
              doesn't exist"

            set sn_keys(${Identifier}) "UNKNOWN"
            set sn_strings(${Identifier}) "UNKNOWN"
            set sn_accels(${Identifier}) 0

            return "UNKNOWN"
        }
        return ${key}
    }

    if {${Type} == "Pos" || ${Type} == "pos"} {
        if {[catch {set pos $sn_accels(${Identifier})}]} {

            sn_error_dialog "String \"Pos\" identifier \"${Identifier}\"\
              doesn't exist"

            set sn_keys(${Identifier}) "UNKNOWN"
            set sn_strings(${Identifier}) "UNKNOWN"
            set sn_accels(${Identifier}) 0

            return 0
        }
        return ${pos}
    }

    return "Unknown"
}

