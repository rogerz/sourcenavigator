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
# hooks.tcl - Hook functions.
# Copyright (C) 1997 Cygnus Solutions.
# Written by Tom Tromey <tromey@cygnus.com>.

proc add_hook {hook command} {
    upvar \#0 ${hook} var
    lappend var ${command}
}

proc remove_hook {hook command} {
    upvar \#0 ${hook} var
    set var [lremove ${var} ${command}]
}

proc define_hook {hook} {
    upvar \#0 ${hook} var

    if {! [info exists var]} {
        set var {}
    }
}

proc run_hooks {hook args} {
    upvar \#0 ${hook} var
    foreach thunk ${var} {
        uplevel \#0 ${thunk} ${args}
    }
}

# def.tcl - Definining commands.
# Copyright (C) 1997 Cygnus Solutions.
# Written by Tom Tromey <tromey@cygnus.com>.

# Define a global array.
proc defarray {name {value {}}} {
    upvar \#0 ${name} ary

    if {! [info exists ary]} {
        set ary(_) {}
        unset ary(_)
        array set ary ${value}
    }
}

# Define a global variable.
proc defvar {name {value {}}} {
    upvar \#0 ${name} var
    if {! [info exists var]} {
        set var ${value}
    }
}

# Define a "constant".  For now this is just a pretty way to declare a
# global variable.
proc defconst {name value} {
    upvar \#0 ${name} var
    set var ${value}
}

# prefs.tcl - Preference handling.
# Copyright (C) 1997 Cygnus Solutions.
# Written by Tom Tromey <tromey@cygnus.com>.

# KNOWN BUGS:
# * When we move to the next tcl/itcl, rewrite to use namespaces and
#   possibly ensembles.

set PREFS_state(ide_running) 0

# Global state.
defarray PREFS_state {
  inhibit-event 0
  initialized 0
}

# This is called when a trace on some option fires.  It makes sure the
# relevant handlers get run.
proc PREFS_run_handlers {name1 name2 op} {
    upvar ${name1} state
    set option [lindex ${name2} 0]

    global PREFS_state
    # Notify everybody else unless we've inhibited event generation.
    if {! $PREFS_state(inhibit-event) && $PREFS_state(ide_running)} {
        ide_property set preference/${option} $state([list ${option} value])\
          global
    }

    # Run local handlers.
    run_hooks PREFS_state([list ${option} handler]) ${option} $state([list\
      ${option} value])
}

# This is run when we see a property event.  It updates our internal
# state.
proc PREFS_handle_property_event {exists property value} {
    global PREFS_state

    # If it isn't a preference property, ignore it.
    if {! [string match preference/* ${property}]} {
        return
    }
    # [string length preference/] == 11.
    set name [string range ${property} 11 end]

    if {${exists}} {
        incr PREFS_state(inhibit-event)
        set PREFS_state([list ${name} value]) ${value}
        incr PREFS_state(inhibit-event) -1
    }\
    elseif {$PREFS_state(ide_running)} then else {
        # It doesn't make sense to remove a property that mirrors some
        # preference.  So disallow by immediately redefining.  Use
        # initialize and not set because several clients are likely to run
        # this at once.
        ide_property initialize preference/${name} $PREFS_state([list ${name}\
          value]) global
    }
}

# pref define NAME DEFAULT
# Define a new option
# NAME is the option name
# DEFAULT is the default value of the option
proc PREFS_cmd_define {name default} {
    global PREFS_state

    # If the option has already been defined, do nothing.
    if {[info exists PREFS_state([list ${name} value])]} {
        return
    }

    if {$PREFS_state(ide_running)} {
        # We only store the value in the database.
        ide_property initialize preference/${name} ${default} global
        set default [ide_property get preference/${name}]
    }

    # We set our internal state no matter what.  It is harmless if our
    # definition causes a property-set event.
    set PREFS_state([list ${name} value]) ${default}
    set PREFS_state([list ${name} handler]) {}

    # Set up a variable trace so that the handlers can be run.
    trace variable PREFS_state([list ${name} value]) w PREFS_run_handlers
}

# pref get NAME
# Return value of option NAME
proc PREFS_cmd_get {name} {
    global PREFS_state
    return $PREFS_state([list ${name} value])
}

# pref getd NAME
# Return value of option NAME
# or define it if necessary and return ""
proc PREFS_cmd_getd {name} {
    global PREFS_state
    PREFS_cmd_define ${name} ""
    return [pref get ${name}]
}

# pref varname NAME
# Return name of global variable that represents option NAME
# This is suitable for (eg) a -variable option on a radiobutton
proc PREFS_cmd_varname {name} {
    return PREFS_state([list ${name} value])
}

# pref set NAME VALUE
# Set the option NAME to VALUE
proc PREFS_cmd_set {name value} {
    global PREFS_state

    # For debugging purposes, make sure the preference has already been
    # defined.
    if {! [info exists PREFS_state([list ${name} value])]} {
        error "attempt to set undefined preference ${name}"
    }

    set PREFS_state([list ${name} value]) ${value}
}

# pref setd NAME VALUE
# Set the option NAME to VALUE
# or define NAME and set the default to VALUE
proc PREFS_cmd_setd {name value} {
    global PREFS_state

    if {[info exists PREFS_state([list ${name} value])]} {
        set PREFS_state([list ${name} value]) ${value}
    } else {
        PREFS_cmd_define ${name} ${value}
    }
}

# pref add_hook NAME HOOK
# Add a command to the hook that is run when the preference name NAME
# changes.  The command is run with the name of the changed option and
# the new value as arguments.
proc PREFS_cmd_add_hook {name hook} {
    add_hook PREFS_state([list ${name} handler]) ${hook}
}

# pref remove_hook NAME HOOK
# Remove a command from the per-preference hook.
proc PREFS_cmd_remove_hook {name hook} {
    remove_hook PREFS_state([list ${name} handler]) ${hook}
}

# pref init ?IDE_RUNNING?
# Initialize the preference module.  IDE_RUNNING is an optional
# boolean argument.  If 0, then the preference module will assume that
# it is not connected to the IDE backplane.  The default is based on
# the global variable IDE.
proc PREFS_cmd_init {{ide_running "unset"}} {
    global PREFS_state IDE

    if {! $PREFS_state(initialized)} {

        if {${ide_running} == "unset"} {
            if {[info exists IDE]} {
                set ide_running ${IDE}
            } else {
                set ide_running 0
            }
        }

        set PREFS_state(initialized) 1
        set PREFS_state(ide_running) ${ide_running}
        if {${ide_running}} {
            property add_hook "" PREFS_handle_property_event
        }
    }
}

# pref list
# Return a list of the names of all preferences defined by this
# application.
proc PREFS_cmd_list {} {
    global PREFS_state

    set list {}
    foreach item [array names PREFS_state] {
        if {[lindex ${item} 1] == "value"} {
            lappend list [lindex ${item} 0]
        }
    }

    return ${list}
}

# The primary interface to all preference subcommands.
proc pref {dispatch args} {
    if {[info commands PREFS_cmd_${dispatch}] == ""} {
        error "unrecognized key \"${dispatch}\""
    }

    eval PREFS_cmd_${dispatch} ${args}
}

# font.tcl - Font handling.
# Copyright (C) 1997 Cygnus Solutions.
# Written by Tom Tromey <tromey@cygnus.com>.


# This function is called whenever a font preference changes.  We use
# this information to update the appropriate symbolic font.
proc FONT_track_change {symbolic prefname value} {
    eval font configure [list ${symbolic}] ${value}
}

# Primary interface to font handling.
# define_font SYMBOLIC_NAME ARGS
# Define a new font, named SYMBOLIC_NAME.  ARGS is the default font
# specification; it is a list of options such as those passed to `font
# create'.
proc define_font {symbolic args} {
    # We do a little trick with the names here, by inserting `font' in
    # the appropriate place in the name.
    set split [split ${symbolic} /]
    set name [join [linsert ${split} 1 font] /]

    pref define ${name} ${args}
    eval font create [list ${symbolic}] [pref get ${name}]
    pref add_hook ${name} [list FONT_track_change ${symbolic}]
}


