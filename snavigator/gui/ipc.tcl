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
# ipc.tcl - Routines for doing interprocess communication (IPC).
# Copyright (C) 1998 Cygnus Solutions.

# This function is called only when the socket is writable.
# This function makes the followings:
#	1. connects to a remote interpreter
#	2. sends a command to the remote interpreter
#	3. waits for a reply from the remote interpreter
#	4. closes the connection
# During sending and receiving, the display of the application
# will be refreshed.
proc sn_send_to_project {host tcp_ip_port {cmd ""}} {
    global sn_options

    if {${host} == [info hostname]} {
        set host $sn_options(def,localhost)
    }

    sn_log "send to project host:${host}, port:${tcp_ip_port}, cmd:${cmd}"

    set fd [socket ${host} ${tcp_ip_port}]

# FIXME : why would we want to connect to and then close the socket???
    if {${cmd} == ""} {
        catch {close ${fd}}
        return ""
    }

    fconfigure ${fd} \
        -encoding $sn_options(def,system-encoding) \
        -blocking 0 \
        -buffering line

    # Send the command!
    sn_write_to_socket ${fd} ${cmd}

    # Read the reply!
    set reply [sn_read_from_socket ${fd}]

    # The first line contains the error.
    set error [lindex ${reply} 0]
    set reply [lrange ${reply} 1 end]

    # Close the socket, we don't need it!
    catch {close ${fd}}

    sn_log "send to project: ${cmd},${error},${reply}"

    return ${reply}
}

# This function reads and executes commands sent through
# the socket.
proc sn_ReadFromRemoteApplication {fd} {
    global Remote_application_reply_status

    if {[catch {set res [gets ${fd} cmd]}] || ${res} == -1} {
        catch {close ${fd}}
        # Close the socket!
        return
    }

    update idletasks

    set readfunc [fileevent ${fd} readable]
    fileevent ${fd} readable {}
    # It has to be disabled until we have not answered.

    set res ""
    set err ""
#FIXME : we are evaling data read from a socket! Not too safe
    # Execute the received tcl command!
    if {![catch {set res [uplevel #0 ${cmd}]} err]} {
        set err ""
    }
    sn_log REMOTE_CMD:${cmd},${err},${res}

    set reply "%ERROR% ${err}\n${res}\n%EOC%"

    sn_write_to_socket ${fd} ${reply}

    fileevent ${fd} readable ${readfunc}
    # Restore it!

    update idletasks
}

# Accept the conection request!
proc sn_AcceptApplication {fd ip port} {
    global sn_options

    fconfigure ${fd} \
        -encoding $sn_options(def,system-encoding) \
        -blocking 0 \
        -buffering line

    fileevent ${fd} readable "sn_ReadFromRemoteApplication ${fd}"
}

# This function tries to find a free TCP/IP port. When that
# is found a socket will be generated to get the port number
# and the socket will be closed.
# With the new port number, a "server" socket will be created
# thus clients can connet the this socket.
proc sn_create_access_handler {accept_script {socketfd ""} {use_port -1}} {
    global sn_options
    if {${socketfd} != ""} {
        upvar ${socketfd} fd
    }

    # Try to connet to the "time" service-port to get a free port number!
    if {[catch {set fd [socket $sn_options(def,localhost) 37]} err]} {
        sn_log "Socket error:${err}"
        if {${use_port} > 0} {
            #don't use a ZERO port
            set port ${use_port}
        } else {
            #default starter, COULD WE PRODUCE A RANDOM PORT, like:
            #set port [expr int(rand() * 5000)]
            set port 12600
        }
    } else {
        set port [lindex [fconfigure ${fd} -sockname] 2]
        close ${fd}
    }

    #create the server side of a socket
    for {set max [expr ${port} + 50]} {${port} < ${max}} {incr port} {
        if {![catch {set fd [socket -server ${accept_script} ${port}]}]} {
            return ${port}
        }
    }
    sn_error_dialog "Could not create socket"

    return -1
}

proc sn_write_socket_handler {fd buffer st} {
    upvar #0 ${st} status

    fileevent ${fd} writable {}

    puts ${fd} ${buffer}
    flush ${fd}

    set status "ready"
    # Terminate tkwait!
    unset status
    # It was jast a flag.

    update idletasks
}

# This function writes a buffer into the socket and it
# waits (without blocking) if the remote side is not reading.
proc sn_write_to_socket {fd buffer} {
    global Remote_application_reply_status

    # Be careful! We must not get blocked.
    # The best time to write is when we can write.
    fileevent ${fd} writable "sn_write_socket_handler ${fd} [list ${buffer}]\
      Remote_application_reply_status"

    vwait Remote_application_reply_status
}

# This function is called only when the socket is readable.
proc sn_read_socket_handler {fd v status_var} {
    upvar #0 ${v} var
    upvar #0 ${status_var} status

    if {[gets ${fd} line] == -1 || ${line} == {%EOC%}} {
        set status "ready"
        # Terminate tkwait!
        unset status
        # It was jast a flag.

        return
    }
    lappend var ${line}
}

# This function reads a reply from the socket and it
# waits (without blocking) if the remote side is not sending.
proc sn_read_from_socket {fd} {
    global Remote_application_reply_buffer Remote_application_reply_status

    # Be careful! We must not get blocked.
    # The best time to read is when we can read.
    set Remote_application_reply_buffer ""
    fileevent ${fd} readable "sn_read_socket_handler ${fd}
			Remote_application_reply_buffer  Remote_application_reply_status"

    tkwait variable Remote_application_reply_status

    # Save the reply!
    set reply ${Remote_application_reply_buffer}

    unset Remote_application_reply_buffer

    return ${reply}
}

