# This file contains procedures that are run in the
# master interp to setup the "sandbox".
#
# The sandbox is a place to run Tcl code (in a safe interp)
# that implements a GUI regression test system. The sandbox
# will execute known commands locally and send unknown
# commands over to a remote interpreter.

# Create a "sandbox" interp that we will run scenario scripts in
# This child interp will send "unknown" commands over to the
# remote sourcenav interp that we are connected to. That way
# we can eval a script and have most of the code execute
# locally except for the parts that need to be sent to the
# remote application.

proc init_sandbox {} {
    delete_sandbox
    interp create -safe sandbox
    setup_sandbox sandbox
}

proc delete_sandbox {} {
    catch {interp delete sandbox}
}

# This proc is used to add or remove commands from the "sandbox"
# interp. All interp initilization should be done here.

proc setup_sandbox { interp } {

    # Remove any commands that we want to be sent over to the target

    foreach cmd_to_remove {eof fblocked gets fcopy close
            read seek flush fileevent puts tell pid package} {
        interp hide $interp $cmd_to_remove
    }

    # Create any aliased commands, these are commands
    # defined in the master interp that are accessable
    # in the slave interp.

    interp alias $interp unknown {} sandbox_unknown
    interp alias $interp async {} sandbox_async
    interp alias $interp next_toplevel {} sandbox_next_toplevel
    interp alias $interp pause {} pause
    interp expose $interp source
    interp expose $interp cd
}

# Run the given set of commands in the "sandbox", this
# is a safe interp where command that are not known
# get sent over to the remote application

proc sandbox_eval { cmds } {
    sandbox eval $cmds
}


# This command is the primary interface to remotely
# sending command through the sandbox. This command
# does not actually pass commands through the sandbox
# interp, we assume that has already been done and
# that the sandbox does not know how to handle to
# given command in the buffer.

set sandbox_delay 5000

proc sandbox_send { buffer {async 0}} {
    global sandbox_delay

    sn_log "sandbox_send \{$buffer\}"
    set result [send_buffer $buffer $async]

    pause $sandbox_delay

    return $result
}


# This command is aliased to "unknown" in the
# sandbox. It will be run when an command that
# is not defined in the Tcl interpreter is evaled.

proc sandbox_unknown { args } {
    sandbox_send $args
}

# This is a special command that can be
# executed in the sandbox to execute a
# remote command asynchronously. This
# is required when a command will block
# the caller (the remote app in this case).
#
# Like so:
# async button_press .b

proc sandbox_async { args } {
    sandbox_send $args 1
}


# This command provides the primary interface
# to the toplevel widget list information
# used by scripts. It will queue up visible
# toplevel window info and return the name
# of the toplevel widget to the caller.

set sandbox_toplevel_queue {}

proc sandbox_next_toplevel { } {
    global sandbox_toplevel_queue

    if {$sandbox_toplevel_queue == {}} {
        vwait sandbox_toplevel_queue
        return [sandbox_next_toplevel]
    } else {
        set next [lindex $sandbox_toplevel_queue 0]
        set sandbox_toplevel_queue [lrange $sandbox_toplevel_queue 1 end]

        # Make sure this window still exists. If it does then
        # raise it. Otherwise discard it and get the next
        # toplevel instead

        set cmd "if {\[winfo exists $next\]} {
                     raise $next
                     set tmp 1
                 } else {
                     set tmp 0
                 }"

        if {[send_buffer $cmd]} {
            return $next
        } else {
            return [sandbox_next_toplevel]
        }
    }
}

set sandbox_visible_toplevel_list {}

proc sandbox_toplevel_list_changed_callback { toplevels } {
    # Figure out which of the toplevel that just showed up
    # are new, add the new ones to our queue

    global sandbox_visible_toplevel_list sandbox_toplevel_queue

    # If there are windows in the visible toplevel list that
    # do not appear in the new toplevel list, remove them now

    set tmp_visible_list $sandbox_visible_toplevel_list
    set sandbox_visible_toplevel_list [list]

    foreach toplevel $tmp_visible_list {
        if {[lsearch -exact $toplevels $toplevel] != -1} {
            lappend sandbox_visible_toplevel_list $toplevel
        }
    }

    # If there are windows in the next_toplevel queue that no longer
    # appear in the toplevel list, remove them now

    set tmp_queue $sandbox_toplevel_queue
    set sandbox_toplevel_queue {}

    foreach toplevel $tmp_queue {
        if {[lsearch -exact $toplevels $toplevel] != -1} {
            lappend sandbox_toplevel_queue $toplevel
        }
    }

    # Enque those toplevels that are not already in the visible
    # list, and add them to the visible list.

    foreach toplevel $toplevels {
        if {[lsearch -exact $sandbox_visible_toplevel_list $toplevel] == -1} {
            lappend sandbox_toplevel_queue $toplevel
            lappend sandbox_visible_toplevel_list $toplevel

            sn_log "added $toplevel to next_toplevel queue and visible list"
        }
    }

    sn_log "new next_toplevel queue is \{$sandbox_toplevel_queue\}"
    sn_log "new toplevel visible list is \{$sandbox_visible_toplevel_list\}"
}
