# This file contains commands use to connect to a remote
# interpreter and send it commands.
#
# Source-Navigator is currently implemented using
# a master interp and a per-project slave interp.
#
# A connection is made to the master interp with
# the send command. It is possible to switch from
# one target interp to another, but it is not possible
# to send to multiple target interps at the same time.
#
# Each project will have a unique name in the master,
# new Source-Navigator project will not reuse an
# old interp name.
#
# For example:
#
# (Master) -> (Slave0)
#
# To send a message to Slave0, we would connect to
# the Master and send the command to eval like so:
#
# send sourcenav-main {interp0 eval {winfo children .}}
# This might return {.t .s}


# Polling for active toplevels
#
# To find out what toplevel windows are currently
# visible in the target interp, one must poll
# and maintain a list of the visible interps
# in the local interp. This is so that we can
# figure out when the visible list changes.



# remote_connect : This is the main entry point
# that will be called to setup the connection
# to a remote interp.

proc remote_connect { } {
    global interp

    # Get the remote master

    pick interp get_remote_master_interps

    sn_log "will use interp named $interp"

    remote_connect_slave
}

# Connect to a slave interp given once connected to a master

proc remote_connect_slave { } {
    global slave

    # Get the slave

    pick slave get_remote_slave_interps

    sn_log "will use slave named $slave"

    # Init the remote interp with utility commands
    # that will be called by this interp, we just
    # send over the name of the utils file.

    send_buffer "package require eventutils"
}



# get_remote_interps : Return a list of the
# master interps that are active on this display.

proc get_remote_master_interps { } {
    set interps [winfo interps]

    # Toss out any interps whose names do not match
    # the pattern sourcenav-main*

    set l [list]
    foreach interp $interps {
        if {[string match sourcenav-main* $interp]} {
            lappend l $interp
            sn_log "keeping interp \"$interp\""
        } else {
            sn_log "ignoring interp \"$interp\""
        }
    }
    return $l
}

# get_remote_slave_interps : Return a list of the
# slave interps in the currently selected master interp

proc get_remote_slave_interps { } {
    return [send_buffer_to_master {interp slaves}]
}





# Commands to implement the actual sending
# to the remote master

proc send_buffer_to_master { buffer {async 0} } {
    # Make sure the interp is still alive, if not reconnect
    if {[catch {send $::interp {}} err]} {
        sn_log "interp $::interp died, err is \"$err\""
        remote_connect
    }

    if {$async} {
	set async -async
    } else {
	set async --
    }

    sn_log "send $async $::interp \"$buffer\""
    if {[catch {send $async $::interp eval [list $buffer]} result]} {
# FIXME: in the event of an error, we need to get the errorInfo
# variable from the other interp and report that as the error!
        sn_log "error : \{$result\}"
        error $result
    } else {
        sn_log "result : \{$result\}"
    }
    return $result
}

# FIXME: We should be able to handle this case where we
# send an event that will shutdown the entire remote
# application.

if 0 {
sandbox_send {event generate .sn_open_proj <Escape>}
send -- sourcenav-main "sourcenav-1 eval {event generate .sn_open_proj <Escape>}"
error : {target application died}
Error string: target application died
Stack Trace: target application died

"error $result"
    (procedure "send_buffer_to_master" line 17)
    invoked from within
"send_buffer_to_master $buffer $async"
    (procedure "send_buffer" line 13)
    invoked from within
"send_buffer $buffer $async"

}




# This command will send the given buffer of command(s)
# over to the remote interp. This would typically be
# called from the unknown command in the sandbox.
# It might also be called directly if the user did
# not want to wait for the typical sandbox delay.

proc send_buffer { buffer {async 0} } {
    global slave

    # See if the slave is still alive, connect to
    # new one if it is not

    if {[catch {send $::interp [list interp slaves $slave]} err]} {
        sn_log "slave $slave died, err is \"$err\""
        remote_connect_slave
    }

    set buffer [list $slave eval $buffer] 
    return [send_buffer_to_master $buffer $async]
}
