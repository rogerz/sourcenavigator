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
# bugreport.tcl - Send bug reports via email or the World Wide Web.
# Copyright (C) 1998 Cygnus Solutions.
#

proc mail_send_bug_report {{err {}} {info {}}} {
    global sn_options
    global auto_path

    set w .tkerrorTrace
    if {[catch {toplevel ${w} -class ErrorTrace}]} {
        return
    }
    wm minsize ${w} 1 1
    wm title ${w} [get_indep String Error]
    wm iconname ${w} [get_indep String Error]

    set s ${w}.status
    set co ${w}.comment
    if {[lsearch -glob ${auto_path} [file join * gui]] != -1} {
        set ef ${w}.error
    } else {
        set ef ""
    }

    frame ${s}

    label ${s}.to_l -text "To:" -anchor w

    entry ${s}.to_e -textvar sn_options(sys,bug-mail-address)
    bind ${s}.to_e <Return> "${s}.ok invoke"

    if {${ef} != ""} {
        button ${s}.ok -text [get_indep String ButtonMail] -command "
                mail_bug_report [list ${err}] ${ef}.text ${co}.text
                destroy ${w}
            "
    } else {
        button ${s}.ok -text [get_indep String ButtonMail] -command "
                mail_bug_report [list ${err}] [list ${info}] ${co}.text
                destroy ${w}
            "
    }
    button ${s}.cancel -text [get_indep String Cancel] -command " destroy ${w} "

    if {${ef} != ""} {
        frame ${ef}
        text ${ef}.text -relief sunken -bd 2 -yscrollcommand\
          "${ef}.scroll set" -xscrollcommand "${ef}.xscroll set" -setgrid true\
          -width 60 -height 20 -wrap none
        scrollbar ${ef}.scroll -command " ${ef}.text yview "
        scrollbar ${ef}.xscroll -command " ${ef}.text xview " -orient horiz
        ${ef}.text insert 0.0 ${info}
        ${ef}.text mark set insert 0.0
    }
    label ${w}.err -text [string range ${err} 0 100]
    frame ${co}
    label ${co}.lb -text "Comment:" -relief groove

    text ${co}.text -yscrollcommand "${co}.scroll set" -setgrid true -width 60\
      -height 5

    scrollbar ${co}.scroll -command " ${co}.text yview "
    ${co}.text mark set insert 0.0

    pack ${s} -side bottom -padx 3m -pady 2m -anchor w
    pack ${s}.to_l -side left -anchor w -padx 3m
    pack ${s}.to_e -side left -anchor w -padx 3m
    pack ${s}.ok -side left -anchor w -padx 3m
    pack ${s}.cancel -side left -anchor w -padx 3m

    pack ${w}.err -fill x

    if {${ef} != ""} {
        pack ${ef}.xscroll -side bottom -fill x
        pack ${ef}.scroll -side right -fill y
        pack ${ef}.text -side left -expand yes -fill both
        pack ${ef} -expand yes -fill both
    }
    pack ${co}.lb -fill x
    pack ${co}.scroll -side right -fill y
    pack ${co}.text -side left -expand yes -fill both
    pack ${co} -expand yes -fill both
    # Center the window on the screen.

    wm withdraw ${w}
    update idletasks
    set x [expr [winfo screenwidth ${w}]/2 - [winfo reqwidth ${w}]/2 -\
      [winfo vrootx [winfo parent ${w}]]]
    set y [expr [winfo screenheight ${w}]/2 - [winfo reqheight ${w}]/2\
      - [winfo vrooty [winfo parent ${w}]]]
    wm geom ${w} +${x}+${y}
    wm deiconify ${w}

    catch {grab ${w}}
    catch {focus ${w}}

    catch {tkwait visibility ${w}}
    catch {tkwait window ${w}}
}

proc web_send_bug_report {{errstr {}} {errtext {}}} {
    global sn_options
    global sn_home tcl_platform

    #make sure we stay in project directory (different interpeters).
    catch {cd $sn_options(sys,project-dir)}

    if {${errstr} != ""} {
        set estr "Stack Trace:\n${errtext}\nsn_home: ${sn_home}\npwd: [pwd]\n"
        append estr "project_dir: $sn_options(sys,project-dir)\n"
        append estr "project_file: $sn_options(sys,project-file)\n"

        #dump product version, the database created with
        if {[info commands paf_db_proj] != ""} {
            catch {
                append estr "db created with SN version: [paf_db_proj get\
                  -key product_version]" "\n"
                append estr "db project version: [paf_db_proj get\
                  -key project_version]" "\n"
            }
        } else {
            append estr "No db is active and no project is opened." "\n"
        }

        set pref $sn_options(db_files_prefix)
        if {${pref} != ""} {
            append estr "db_prefix: ${pref}" "\n"
            append estr "db-directory: $sn_options(both,db-directory)"

            if {![catch {file stat $sn_options(both,db-directory) st_var}]} {
                catch {
                    append estr " mode: [format "0%o" $st_var(mode)],\
                      " $st_var(type)
                }
            }
            append estr "\n"

            foreach f [glob -nocomplain ${pref}.\[A-Za-z\]*] {
                file stat ${f} st_var
                set mode $st_var(mode)
                set size $st_var(size)

                append estr "${f}, mode: [format "0%o" ${mode}], size:\
                  ${size}\n"
            }
        }

        foreach f [array names tcl_platform] {
            append estr "${f}: $tcl_platform(${f}) "
        }
        append estr "\n"

        # build up the error string and convert to URL encoding
        set query_string "?ERRSTR=${errstr}&ERRTEXT=${estr}"
        regsub -all {%} ${query_string} {%25} query_string
        regsub -all "\n" ${query_string} {%0a} query_string
        regsub -all "\t" ${query_string} {     } query_string
        regsub -all {\'} ${query_string} {%27} query_string
        regsub -all {\`} ${query_string} {%60} query_string
        regsub -all {\"} ${query_string} {%22} query_string
        regsub -all {\$} ${query_string} {%24} query_string
        regsub -all {\[} ${query_string} {%5b} query_string
        regsub -all {\]} ${query_string} {%5d} query_string
        regsub -all {\(} ${query_string} {%28} query_string
        regsub -all {\)} ${query_string} {%29} query_string
        regsub -all {,} ${query_string} {%2c} query_string
        regsub -all { } ${query_string} {+} query_string

        # send to the Web PR interface
        sn_help "http://www.cygnus.com/cgi-bin/sn-buildpr.cgi${query_string}"
    } else {
        sn_help "http://www.cygnus.com/product/sendpr.html"
    }
}

proc mail_bug_report {error_str et ec} {
    global sn_options
    global env tcl_platform sn_home
    global sn_rcsid sn_product_version sn_product_name sn_suite_name

    #make sure we stay in project directory (different interpeters).
    catch {cd $sn_options(sys,project-dir)}

    set sep "\n-----------------------------------------------------------\n"

    set estr "Error:\n${error_str}"
    append estr ${sep} "Comment:\n" [${ec} get 0.0 "end -1c"]

    catch {append estr ${sep} "Version:\t" "${sn_product_version}\n"}
    catch {append estr ${sep} "Full Name:\t" "${sn_suite_name}\n"}
    catch {append estr ${sep} "Product Name:\t:" "${sn_product_name}\n"}

    set id ""
    if {![catch {regexp {[0-9]+.*} ${sn_rcsid} id}]} {
        catch {append estr "CVS rev:\t" ${id}}
    }
    append estr ${sep} "sn_home:\t" "${sn_home}\n" "pwd:\t\t[pwd]" "\n"

    catch {
        append estr "project_dir:\t$sn_options(sys,project-dir)\n"
        append estr "project_file:\t$sn_options(sys,project-file)"
        append estr "\n"
    }

    #dump product version, the database created with
    if {[info commands paf_db_proj] != ""} {
        catch {
            append estr "db created with SN version: [paf_db_proj get\
              -key product_version]" "\n"
            append estr "db project version: [paf_db_proj get\
              -key project_version]" "\n"
        }
    } else {
        append estr "No db is active and no project is opened." "\n"
    }

    set pref $sn_options(db_files_prefix)
    if {${pref} != ""} {
        append estr "db_prefix:\t${pref}" "\n"
        append estr "db-directory:\t$sn_options(both,db-directory)"

        if {![catch {file stat $sn_options(both,db-directory) st_var}]} {
            catch {append estr "\t" "mode: [format "0%o" $st_var(mode)]"\
              "\t" $st_var(type)}
        }
        append estr "\n"

        foreach f [glob -nocomplain ${pref}.\[A-Za-z\]*] {
            file stat ${f} st_var
            set mode $st_var(mode)
            set size $st_var(size)

            append estr "${f}:" "\t" "mode: [format "0%o" ${mode}]" "\t"\
              "size: ${size}" "\n"
        }
    }

    append estr ${sep} "\n"
    foreach f [array names tcl_platform] {
        append estr "${f}: $tcl_platform(${f}) "
    }
    append estr "\n"

    if {$tcl_platform(platform) != "windows"} {
        catch {append estr ${sep} [exec df]}
    }

    if {[winfo exists ${et}]} {
        set et [${et} get 0.0 "end -1c"]
    }
    append estr ${sep} "Stack:\n" ${et}

    sn_log "\n\nMail bug report: ${estr}"

    set mdf ""
    if {$tcl_platform(platform) == "windows"} {
        if {[file isdirectory [file join c: tmp]]} {
            set mdf [file join c: tmp sn.bug]
        }\
        elseif {[file isdirectory [file join c: temp]]} {
            set mdf [file join c: temp sn.bug]
        } else {
            set mdf [file join c: sn.bug]
        }
    }

    if {${mdf} == ""} {
        if {[catch {set mdf [file join $env(HOME) sn.bug]}]} {
            if {[info exists env(TMP)]} {
                set mdf [file join $env(TMP) sn.bug]
            } else {
                set mdf [file join / tmp sn.bug]
            }
        }
    }
    if {![catch {set fd [open ${mdf} "w+"]}]} {
        fconfigure ${fd} -encoding $sn_options(def,system-encoding) -blocking 0
        puts ${fd} ${estr}
        close ${fd}
    }

    catch {sn_send_mail $sn_options(sys,bug-mail-address) ${error_str} ${estr}}
}

proc sn_drop_mail_line {fd} {
    if {[gets ${fd} line] == -1} {
        catch {close ${fd}}
    }
    update idletasks
}

# We try to make a connection to a mailhost on the TCP port 25.
# It has the advantage that this is platform independent and
# we communicate (hopefuly) to the mailhost directly (no mail
# routing).

proc sn_send_mail {to subj estr} {
    global sn_options
    global env

    set mailhost $sn_options(def,mailhost)

    if {[lsearch -exact ${mailhost} "mailhost"] == -1} {
        lappend mailhost "mailhost"
    }
    if {[lsearch -exact ${mailhost} "mail"] == -1} {
        lappend mailhost "mail"
    }

    set mfd ""
    foreach mh ${mailhost} {
        if {![catch {set mfd [socket ${mh} 25]}]} {
            break
        }
    }

    if {${mfd} != ""} {
        fileevent ${mfd} readable "sn_drop_mail_line ${mfd}"
        fconfigure ${mfd} \
            -encoding $sn_options(def,system-encoding) \
            -blocking 0 \
            -buffering line

        # Very important!

        puts ${mfd} "HELO [info hostname]"
        if {[catch {set usr [get_username]}] && [catch {set usr\
          $env(LOGNAME)}]} {
            set usr "unknown"
        }
        puts ${mfd} "MAIL FROM: ${usr}"
        puts ${mfd} "RCPT TO: ${to}"

        puts ${mfd} "DATA"
        puts ${mfd} "Subject: SN bug: ${subj}"

        regsub -all "\n\\.\n" ${estr} "\n .\n" estr
        puts ${mfd} ${estr}

        puts ${mfd} "."
        puts ${mfd} "QUIT"

        flush ${mfd}

        after 1500

        catch {close ${mfd}}

        return
    }

    set m mail
    foreach f "/bin/mail /usr/ucb/Mail /usr/bin/mailx /usr/bin/mail" {
        if {[file exists ${f}]} {
            set m ${f}
            break
        }
    }

    catch {exec ${m} [string trim $sn_options(sys,bug-mail-address)] <<\
      ${estr} |& $sn_options(def,null_dev)} err
    if {${err} != ""} {
        sn_log "sent bugreport ${err}"
    }
}

