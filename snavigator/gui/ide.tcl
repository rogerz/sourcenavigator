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
# ide.tcl - Interface to IDE backplane.
# Copyright (C) 1997 Cygnus Solutions.
# Written by Tom Tromey <tromey@cygnus.com>.

# This is called when an edit request is fielded.  See the editor
# interface definition in libide to understand the arguments.
proc ide_edit_event {file root line column options} {
    global sn_options
    if {${root} != $sn_options(sys,project-dir)} {
        set file [file join ${root} ${file}]
    }
    sn_edit_file dummy ${file} ${line}
}

# This is called when a property request is made.  See the property
# manager interface definition in libide to understand the arguments.
proc ide_property_manager {command name {value ""} {persistent transient}} {
    global ide_db

    switch -- ${command} {
        defined {
                # Return true if the `get' method is meaningful.
                foreach p {transient project global} {
                    if {[info exists ide_db(${p},${name})]} {
                        return 1
                    }
                }
                return 0
            }
        get {
                # Return the most recently set value.  Failing that,
                # return the most transient value.  (It can fail if the
                # most recently set value has just been deleted.)
                if {[info exists ide_db(recent,${name})]} {
                    lappend p $ide_db(recent,${name})
                }
                lappend p transient project global
                foreach item ${p} {
                    if {[info exists ide_db(${item},${name})]} {
                        return $ide_db(${item},${name})
                    }
                }
                error "couldn't find property \"${name}\""
            }
        set {
                set ide_db(recent,${name}) ${persistent}
                set ide_db(${persistent},${name}) ${value}
                set ide_db(changed,${name}) 1
                return ${value}
            }
        append {
                set ide_db(recent,${name}) ${persistent}
                append ide_db(${persistent},${name}) ${value}
                set ide_db(changed,${name}) 1
                return $ide_db(${persistent},${name})
            }
        remove {
                # Always remove the "most transient" item.  Do nothing if
                # the item doesn't exist.
                foreach p {transient project global} {
                    if {[info exists ide_db(${p},${name})]} {
                        unset ide_db(${p},${name})
                        break
                    }
                }
            }
        default {
                error "bad ide_property_manager subcommand ${command}"
            }
    }
}

# This writes all the state at some level of persistence to a
# database.  Private proc.
proc ide_write_database {database persistence {change_only 0}} {
    global ide_db

    # Make `seen' into an array.
    set seen(_) {}
    unset seen(_)
    foreach item [array names ide_db ${persistence},*] {
        set key [lindex [split ${item} ,] 1]
        set seen(ide-${key}) {}
        if {! ${change_only} || $ide_db(changed,${key})} {
            ${database} put ide-${key} $ide_db(${item})
        }
    }

    # We must scan the database for any ide-related options.  That way
    # we can delete the ones that we no longer care about.
    foreach item [${database} seq -glob ide-*] {
        set key [lindex ${item} 0]
        set value [lindex ${item} 1]
        if {! [info exists seen(${key})]} {
            ${database} delete ${key}
        }
    }
}

# This writes the globally persistent properties to the prefs
# database.
proc ide_write_global_preferences {database} {
    ide_write_database ${database} global 1
}

# This writes the project preferences.
proc ide_write_project_preferences {database} {
    ide_write_database ${database} project
}

# This reads information from some database and caches it internally.
# Private proc.
proc ide_read_database {database persistence} {
    global ide_db

    foreach item [${database} seq -glob ide-*] {
        set key [lindex ${item} 0]
        set value [lindex ${item} 1]
        set key [string range ${key} 4 end]
        set ide_db(${persistence},${key}) ${value}
        set ide_db(changed,${key}) 0
    }
}

# Read information from the global database.
proc ide_read_global_preferences {database} {
    ide_read_database ${database} global
}

# Read information from the project database.
proc ide_read_project_preferences {database} {
    ide_read_database ${database} project
}

# This is like ide_property, but works even if we aren't connected to
# the IDE backplane.
proc maybe_ide_property {args} {
    global ide_running
    if {${ide_running}} {
        eval ide_property ${args}
    }
}

# This is like ide_event, but works even if we aren't connected to the
# IDE backplane.
proc maybe_ide_event {args} {
    global ide_running
    if {${ide_running}} {
        eval ide_event ${args}
    }
}

