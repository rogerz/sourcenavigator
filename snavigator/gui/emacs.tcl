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
# emacs.tcl - Routines for talking to emacs. 
# Copyright (C) 1998 Cygnus Solutions.

# Run this to make sure Emacs is running.  After calling this, the
# global variable sn_emacs_socket will hold a connection to the
# running Emacs.
proc sn_ensure_emacs_running {editcmd} {
    global sn_options
    global sn_emacs_socket
    upvar #0 sn_options(def,localhost) host

    if {![info exist sn_emacs_socket]} {
        set port [sn_create_access_handler sn_emacs_socket_accept socketfd]
        if {${port} == -1} {
            return
        }

        set emacs_connection 0
        set lisp_file [sn_search_file sn.el]
        # We use add-to-list and require because it makes
        # debugging a little easier.  Also, if multiple
        # sessions talk to a single Emacs, this makes it so
        # sn.el is loaded only once.
        set lisp_commands "(progn (add-to-list 'load-path \"[file dirname\
          ${lisp_file}]\") (require 'sn))"
        if {[regexp "gnuclient" ${editcmd}]} {

            # Unset tmp env vars while executing gnuclient
            # since these programs will fail to find unix socket files
            # when the tmp dir is set to .snprj
            set tmp [sn_unset_tmp_dir]

            # First try via gnuclient.  If that fails,
            # then start a new Emacs.

            if {! [catch {exec gnuclient -batch -eval ${lisp_commands}} err]} {
                # It worked.  So send the magic command.
                sn_log "Emacs: connected via gnuclient"
                if {[catch {exec gnuclient -batch -eval\
                        "(sn-startup nil \"${host}\" nil ${port})"} err]} {
                    sn_log "exec gnuclient startup failed : $err"
                }
                set emacs_connection 1
            } else {
                sn_log "Emacs: exec gnuclient failed : $err"
                # Assume "emacs" is in path.
                set editcmd emacs
            }

            if {$tmp != ""} {
                sn_set_tmp_dir $tmp
            }
        }

        if {! ${emacs_connection}} {
            set t_m_p [sn_tmpFileName]
            set tmpfd [open ${t_m_p} w]
            fconfigure ${tmpfd} \
                -encoding $sn_options(def,system-encoding) \
                -blocking 0	    
            puts ${tmpfd} "${lisp_commands}\n(sn-startup \"${t_m_p}\"\
              \"${host}\" \"$sn_options(sys,project-dir)\" ${port})"
            close ${tmpfd}
            sn_log "Starting:${editcmd} -l ${t_m_p} &"
            if {[catch {exec -- ${editcmd} -l ${t_m_p} &} err]} {
                close ${socketfd}

                sn_error_dialog ${err}

                return
            }
        }

        vwait sn_emacs_socket

        # Just made a new connection.  Now send Emacs a list
        # of all the files we know about.  That way if one of
        # these files is in an Emacs buffer, it will
        # automatically be put into sn-minor-mode.
        set command "(sn-mark-for-project \"$sn_options(sys,project-dir)\" '("
        foreach file [paf_db_f seq -data] {
            append command "\"${file}\" "
        }
        append command "))"
# FIXME: This generates a Tcl error when we can not find emacs in the path.
# this can happen if folks only have xemacs installed for instance.
        puts ${sn_emacs_socket} ${command}

        # This is just the socket used to listen on.  We don't
        # need it any more.
        close ${socketfd}
    }
}

proc sn_start_emacs {file line state editcmd} {
    global sn_options
    global sn_emacs_socket

    sn_ensure_emacs_running ${editcmd}

    set column [lindex [split ${line} .] 1]
    if {${column} == ""} {
        set column 0
    }
    set line [lindex [split ${line} .] 0]
    sn_log "Emacs edit:${file} ${line} ${column} ${state}"

    puts ${sn_emacs_socket} "(sn-visit \"$sn_options(sys,project-dir)\"\
      \"${file}\" ${line} ${column} \"${state}\")"
}

proc sn_emacs_socket_command {channel} {
    if {[gets ${channel} cmd] == -1} {
        global sn_emacs_socket

        unset sn_emacs_socket
        catch {close ${channel}}

        sn_log "Emacs has terminated"

        return
    }
    set cmd [string trim ${cmd}]
    sn_log "Emacs command:${cmd}"
    # If the "paf_db_f" command does not exist, we have "hidden" the
    # project, so we hace to restore it to open the database files.
    if {[info commands "paf_db_f"] == ""} {
        sn_hide_show_project deiconify
    }

    set ret [eval ${cmd}]

    update idletasks
}

proc sn_emacs_socket_accept {channel ip port} {
    global sn_emacs_socket sn_options

    set sn_emacs_socket ${channel}
    fconfigure ${channel} \
        -encoding $sn_options(def,system-encoding) \
        -blocking 0 \
        -buffering line
    fileevent ${channel} readable "sn_emacs_socket_command ${channel}"
}

# Emacs causes this proc to be run.  It searches for a symbol in every
# relevant namespace, and edits the first to be found.
proc sn_emacs_display_object {name} {
    global sn_all_scopes

    set symbol ""
    foreach scope ${sn_all_scopes} {
        # Don't want to look at files.
        if {${scope} == "f" || [info commands paf_db_${scope}] == ""} {
            continue
        }
        set symbol [paf_db_${scope} seq -data ${name}]
        if {${symbol} != ""} {
            break
        }
    }

    if {${symbol} != ""} {
        sn_display_object ${scope} ${name}
    } else {
        global sn_emacs_socket

        sn_log "Emacs:couldn't find ${name}"
    }
}

