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
# gdbtk.tcl - gdbtk integration in SN
# Copyright (C) 1998, 1999 Cygnus Solutions.

#this program starts gdbtk with some (must be) startup parameters,
#like program to debug, parameters, using xterm, .
proc sn_debugger {{working_dir ""} {dbg_program ""} {gdb_command ""}} {
    global sn_options tcl_platform env
    global sn_sockets sn_elix

    #allow the user to specify some settings for
    #the started gdb process

    # if we have been passed any data use it, otherwise use
    # the last settings used if any.
    if {${working_dir}!=""} {
        set sn_options(gdb-workdir) ${working_dir}
    }
    if {${dbg_program}!=""} {
        set sn_options(gdb-program) ${dbg_program}
    }

    set flds [list [list [get_indep String DbgProgram]\
      $sn_options(gdb-program)] [list [get_indep String DbgWorkdir]\
      $sn_options(gdb-workdir) d]]
    if {$tcl_platform(platform) != "windows"} {
        lappend flds [list [get_indep String DbgProgXterm]\
          $sn_options(gdb-xterm)]
    }
    set files [sn_prompt_for_files [get_indep String DbgExecutable] ${flds}]
    if {${files} == ""} {
        return "cancel"
    }
    #save the new settings
    set sn_options(gdb-program) [lindex ${files} 0]
    set sn_options(gdb-workdir) [lindex ${files} 1]
    set sn_options(gdb-xterm) [lindex ${files} 2]

    #do we have an xterm
    if {$tcl_platform(platform) == "unix" && $sn_options(gdb-xterm) != ""} {
        set tty [Tty&::create_xterm]
    } else {
        set tty ""
    }

    #the command could be specified like "gdb --command=.. --..."
    if {${gdb_command} == ""} {
        set cmd $sn_options(def,gdb-command)
    } else {
        set cmd ${gdb_command}
    }

    #convert pathes including '\' into "/"
    #mask other special characters '[]{}$'
    if {$tcl_platform(platform) == "windows"} {
        regsub -all {\\} ${cmd} {/} cmd
        foreach s [list \[ \] \{ \} \$] {
            regsub -all \\${s} ${cmd} \\${s} cmd
        }
    }

    #enable external editor functionality
    lappend cmd --enable-external-editor

    #tty functionality
    if {${tty} != ""} {
        lappend cmd --tty=${tty}
    }

    #open a socket to listen to gdb to receive edit commands
    #Create only one socket for all running gdbtk's with "gdb"
    #as it's name
    if {! [info exists sn_sockets(gdb,port)] || $sn_sockets(gdb,port) == -1} {
        #create a socket with randomly password
        sn_socket_new gdb -password auto
        if {$sn_sockets(gdb,port) == -1} {
            sn_socket_close gdb
            bell
            return 0
        }
    }

    #generate a tcl/script to be called from gdb, only
    #the user can read or remove this file.
    set tmp [sn_tmpFileName]
    set fd [open ${tmp} w+ 0600]
# FIMXE: is this is right encoding, why don't we use the default system one?
    fconfigure ${fd} -encoding "iso8859-1"

    #working directory
    if {$sn_options(gdb-workdir) != "" && [file isdir\
      $sn_options(gdb-workdir)]} {
        puts ${fd} "cd [list $sn_options(gdb-workdir)]"
    }

    puts ${fd} "
		set external_editor_command \"Send_Command_To_SN $sn_sockets(gdb,host)\
      $sn_sockets(gdb,port) $sn_sockets(gdb,password)\"
		proc Send_Command_To_SN {host port password args} {
			global sn_socket_port
			if {!\[info exists sn_socket_port\]} {
				set ret \[catch {set sn_socket_port \[socket \$host \$port\]}\]
				if {\$ret} {
					tk_dialog .warn-sn Error \$err error_image 0 Ok
					return
				}
			}
			set ret \[catch {
					if {\[lindex \$args 0\] == \"edit\"} {
						puts \$sn_socket_port \[list gdb_edit \[list password \$password\]\
      \[list data \$args\]\]
						flush \$sn_socket_port
					}
				} err\]
			if {\$ret} {
				tk_dialog .warn-sn Error \$err error_image 0 Ok
				return
			}
		}"

    # EL/IX change: tell gdb to load our `.gdbinit' after
    # processing.  We have to do this instead of simply using `-x'
    # because gdbtk resets the target during initialization.
    if {${sn_elix} && $sn_options(gdb-workdir) != "" && [file isdir\
      $sn_options(gdb-workdir)] && [file exists [file join\
      $sn_options(gdb-workdir) .sngdb]]} {
        puts ${fd} "pref set gdb/elix/target_can_start 0"
        puts ${fd} "gdb_cmd \"source [file join $sn_options(gdb-workdir)\
          .sngdb]\""
    }

    puts ${fd} "#message read, self-destruction"
    puts ${fd} "file delete -force -- ${tmp}\n"
    close ${fd}

    lappend cmd "--tclcommand" ${tmp}

    #program name
    if {$sn_options(gdb-program) != ""} {
        lappend cmd $sn_options(gdb-program)
    }

    #execute it in the background
    lappend cmd &

    #save and unset the tcl/tk library path to avoid
    #conflicts with gdb/tk library path
    catch {
        set save_TK_LIBRARY $env(TK_LIBRARY)
        set save_TCL_LIBRARY $env(TCL_LIBRARY)
        unset env(TK_LIBRARY)
        unset env(TCL_LIBRARY)
    }

    sn_log "now to exec gdb command : ${cmd}"

    #gdb command line
    if {[catch {eval exec ${cmd}} msg] != 0} {
        sn_error_dialog ${msg} [get_indep String DbgTitle]

        #make sure that the socket is closed
        sn_socket_close gdb

        #delete temporary created file
        catch {file delete -force -- ${tmp}}

        set ret 0
    } else {
        #be sure that the temporary file is deleted after a while
        #especially when gdb launching hasn't succeeded
        after 60000 "catch {file delete -force -- ${tmp}}"

        set ret 1
    }

    #restore older path state
    catch {
        set env(TK_LIBRARY) ${save_TK_LIBRARY}
        set env(TCL_LIBRARY) ${save_TCL_LIBRARY}
    }


    return ${ret}
}

proc gdb_response {args} {
    set cmpfile [lindex ${args} 0]
    set symbol [lindex ${args} 1]
    set srcfile [lindex ${args} 2]
    set filepos [lindex ${args} 3]

    if {${srcfile} != ""} {
        sn_edit_file ${symbol} ${srcfile} ${filepos}
    }
}

itcl::class Tty& {
    proc create_xterm {} {
        global sn_options

        catch {destroy_xterm}

        #set default xterm for the GDB child
        if {$sn_options(gdb-xterm) == ""} {
            set sn_options(gdb-xterm) "xterm"
        }

        # Tricky: we exec /bin/cat so that the xterm will exit whenever we
        # close the write end of the pipe.  Note that the stdin
        # redirection must come after tty is run; tty looks at its stdin.
        set shcmd {/bin/sh -c "exec 1>&7; tty; exec /bin/cat 0<&6"}

        set fg $sn_options(def,default-fg)
        set bg $sn_options(def,default-bg)
        set xterm [list /bin/sh -c "exec $sn_options(gdb-xterm) -T 'Gdb Child\
          (do not terminate)' -n Gdb -bg ${bg} -fg ${fg} -e ${shcmd} 6<&0 7>&1"]

        # Need both read and write access to xterm process.
        sn_log "now to open a pipe to \"$xterm\""
        set xterm_fd [open "| ${xterm}" w+]
        fconfigure ${xterm_fd} \
            -encoding $sn_options(def,system-encoding) \
            -blocking 0
# FIXME: Check that adding -blocking 0 does not break things
        set tty [gets ${xterm_fd}]

        return ${tty}
    }

    proc destroy_xterm {} {
        if {[info exists xterm_fd] && ${xterm_fd} != ""} {
            close ${xterm_fd}
        }
        set xterm_fd {}
    }

    common xterm_fd ""
    common tty ""
}

#create a new socket, the interesting thing by this
#command that it creates a new enviroment with several
#informations regarding the running socket.
#A socket and it's components can be accessed using it's
#name
proc sn_socket_new {name args} {
    global sn_options
    global sn_sockets

    #init values
    set sn_sockets(${name},password) ""
    set sn_sockets(${name},port) -1

    #verify the user arguments
    set len [llength ${args}]
    for {set i 0} {${i} < ${len}} {incr i} {
        set key [lindex ${args} ${i}]
        set data [lindex ${args} [expr ${i} + 1]]
        switch -- ${key} {
            "-password" {
                    set sn_sockets(${name},password) ${data}
                    incr i
                }
            "-port" {
                    set sn_sockets(${name},port) ${data}
                    incr i
                }
            default {
                    #unknown argument
                    return -1
                }
        }
    }

    #however assign a name for the socket
    set sn_sockets(${name},name) ${name}

    #if password = auto, generate a random password
    if {$sn_sockets(${name},password) == "auto"} {
        set sn_sockets(${name},password) [sn_generate_password]
    }

    set sn_sockets(${name},host) $sn_options(def,localhost)
    set sn_sockets(${name},port) [sn_create_access_handler\
      [list sn_socket_accept ${name}] sn_sockets(gdb,fd)\
      $sn_sockets(${name},port)]

    #return new port number	
    return $sn_sockets(${name},port)
}

#display all/specified open socket(s) to the stdout
proc sn_socket_display {{name "all"}} {
    global sn_sockets

    #all sockets
    if {${name} == "all"} {
        set name ""
        foreach n [array names sn_sockets] {
            lappend name [lindex [split ${n} ","] 0]
        }
        set name [lunique [lsort ${name}]]
    }

    #display components of the socket, will be reworked
    #later to be compatible with the GUI.
    set names "name	port	fd	host	password"
    puts stdout ${names}
    foreach n ${name} {
        set str ""
        foreach s ${names} {
            append str "$sn_sockets(${n},${s})\t"
        }
        puts stdout ${str}
    }
}

proc sn_generate_password {} {
    #generate a 20-chars key that contains
    #randomly a..z A..Z and 0..9
    #the possibility to get lower,upper or number is 40:40:20%
    set lower "abcdefghijklmnqorstuwxyz"
    set upper "ABCDEFGHIJKLMNQORSTUWXYZ"
    set numbe "0123456789"
    set password ""
    for {set i 0} {${i} < 20} {incr i} {
        set cat [expr rand()]
        if {${cat} < 0.4} {
            set sett ${lower}
        }\
        elseif {${cat} < 0.8} {
            set sett ${upper}
        } else {
            set sett ${numbe}
        }
        set slen [string length ${sett}]
        set j [expr int(rand() * ${slen})]
        append password [string range ${sett} ${j} ${j}]
    }
    return ${password}
}

#this procedure closes an opened socket and release
#all set variables regarding this socket port.
proc sn_socket_close {name} {
    global sn_sockets

    #close the socket
    catch {close $sn_sockets(${name},channel)}

    #unset the related variables
    foreach a [array names sn_sockets "${name},*"] {
        catch {unset sn_sockets(${a})}
    }
}

proc sn_socket_accept {name channel ip port} {
    global sn_sockets sn_options

    #store channel value
    set sn_sockets(${name},channel) ${channel}
    set sn_sockets(${name},ip) ${ip}
    set sn_sockets(${name},port) ${port}

    #buffer lines
    fconfigure ${channel} \
        -encoding $sn_options(def,system-encoding) \
        -blocking 0 \
        -buffering line

    #execute a command by reading from the socket port
    fileevent ${channel} readable [list sn_socket_command ${name} ${channel}]

    # This is just the socket used to listen on.  We don't
    # need it any more.
    catch {close $sn_sockets(gdb,fd)}
}

proc sn_socket_command {name channel args} {
    global sn_sockets

    if {[gets ${channel} cmd] == -1} {
        sn_socket_close ${name}
        sn_log "error by reading from socket <${name}>, socket closed."
        return
    }

    #test if the socket is registered in SN
    if {![info exists sn_sockets(${name},name)] || $sn_sockets(${name},name)\
      != ${name}} {
        sn_log "Trying to access an unregistered socket <${name}>"
        return
    }

    set cmd [string trim ${cmd}]
    sn_log "Received a command request from socket ${name} <${cmd}>"

    #HERE IS THE KEY FOR SECURE COMMUNICATION. NO NEED FOR something
    #like "secure interpreter".
    #WE DEFINE HERE WHAT COMMANDS CAN BE EXECUTED
    #
    #Warning: Don't use 'eval' to avoid unwanted executions of
    #         tcl/scripts
    #
    #receive pairs of {key value}, like
    #[cmd {password ****} {data data} {misc ....} {... } ...]

    set possible_cmd [lindex ${cmd} 0]

    #extract the received pairs {key data} into an array
    foreach pairs [lrange ${cmd} 1 end] {
        set data([lindex ${pairs} 0]) [lindex ${pairs} 1]
    }

    #veriy if the socket has an assigned password
    if {$sn_sockets(${name},password) != ""} {
        if {![info exists data(password)] || $data(password) !=\
          $sn_sockets(${name},password)} {
            sn_log "wrong password, permission denied for socket <${name}>"
            bell
            return
        }
    }

    set successfull 1
    switch -- ${possible_cmd} {
        edit {
                if {[info exists data(data)]} {
                    set file [lindex $data(data) 0]
                    set line [lindex $data(data) 1]
                    sn_edit_file dummy ${file} ${line}
                } else {
                    set successfull 0
                }
            }
        gdb_edit {
                if {[info exists data(data)]} {
                    set compfile [lindex $data(data) 1]
                    set func [lindex $data(data) 2]
                    set filepath [lindex $data(data) 3]
                    set pos [lindex $data(data) 4]
                    gdb_response ${compfile} ${func} ${filepath} ${pos}
                } else {
                    set successfull 0
                }
            }
        view {
                sn_hide_show_project deiconify
            }
        hide {
                sn_hide_show_project withdraw
            }
        default {
                set successfull 0
            }
    }

    if {!${successfull}} {
        #unknown command or invalid parameter list
        bell
        sn_error_dialog [format [get_indep String SDK_UnknownRequest] ${cmd}]
    }

    catch {unset data}
}

