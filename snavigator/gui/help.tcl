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
# help.tcl - Online documentation and balloon help. 
# Copyright (C) 1998 Cygnus Solutions.

proc sn_help {{file ""} {busy 1}} {
    global sn_options tcl_platform sn_path

    if {${file} != "" && $tcl_platform(platform) == "windows"} {
        set file [file attribute ${file} -shortname]
    }

    if {${file} == ""} {
        set file "index.html"
        set dir $sn_path(htmldir)
        set file [file join ${dir} ${file}]
        if {$tcl_platform(platform) == "windows"} {
            set file [file attribute ${file} -shortname]
        }
        set file "file:${file}"
    }\
    elseif {![regexp -- {^(file:|http://)} ${file}]} {
        set file file:${file}
    }

    if {$tcl_platform(os) == "Windows NT"} {
        sn_log "cmd -c start ${file}"
        set ret [catch {exec cmd /c start ${file} &} errmsg]
    }\
    elseif {$tcl_platform(os) == "Windows 95"} {
        sn_log "command -c start ${file}"
        set ret [catch {exec command /c start ${file} &} errmsg]
    } else {
        global netscape_error netscape_exit_status
        global netscape_done

        set netscape_done 0
        set html_view $sn_options(def,html-viewer)
        if {${html_view} == ""} {
            set html_view "netscape -remote openURL(%s)"
        }
        if {[string first {%s} ${html_view}] != -1} {
            set cmd [format ${html_view} ${file}]
        } else {
            set cmd [list ${html_view} ${file}]
        }
        sn_log "${cmd}"
        set ret [catch {open "|${cmd}"} netscape_fd]
        if {${ret}} {
            set errmsg ${netscape_fd}
        } else {
            fconfigure ${netscape_fd} \
                -encoding $sn_options(def,system-encoding) \
                -blocking 0
            fileevent ${netscape_fd} readable [list netscape_reader\
              ${netscape_fd}]
            vwait netscape_done
            if {${netscape_exit_status}} {
                if {![string match {*not running on display*}\
                  ${netscape_error}]} {
                    set ret 1
                    set errmsg ${netscape_error}
                }\
                elseif {[regsub {([.]*)-remote openURL\(file:([^\)]*)\)([.]*)}\
                  ${cmd} {\1\2\3} cmd] != 0} {
                    sn_log "${cmd}"
                    eval exec ${cmd} &
                } else {
                    set ret 1
                    set errmsg ${netscape_error}
                }
            }
        }
    }

    if {${ret}} {
        sn_error_dialog ${errmsg}
        return
    }
    after 3000
}

proc netscape_reader {pipe} {
    global netscape_error
    global netscape_exit_status
    global netscape_done

    if {[eof ${pipe}]} {
        set netscape_exit_status [catch {close ${pipe}} netscape_error]
        incr netscape_done
        return
    }
    gets ${pipe} line
}

# display a small window on the button of a widget to
# view the binded help text
set balloon_bind_info(screen_width) [winfo screenwidth .]
set balloon_bind_info(screen_height) [winfo screenheight .]
set balloon_bind_info(shown) 0

#use class of bindings for balloon help
proc balloon_button_bind {} {
    global balloon_bind_info

    set w SN_Balloon

    bind ${w} <Enter> {+
		catch {
			set balloon_bind_info(id) [after $balloon_bind_info(%W,delay)\
      $balloon_bind_info(%W,proc) %W [list $balloon_bind_info(%W,text)] %X %Y]
		}
	}
    bind ${w} <Leave> {+balloon_destroy}
    bind ${w} <Any-ButtonPress> {+balloon_destroy}
    bind ${w} <KeyPress> {+balloon_destroy}
}

proc balloon_destroy {} {
    global balloon_bind_info

    if {[winfo exists .cb_balloon]} {
        destroy .cb_balloon
    }
    if {[info exists balloon_bind_info(id)]} {
        after cancel $balloon_bind_info(id)
    }
    if {[info exists balloon_bind_info(id,timeout)]} {
        after cancel $balloon_bind_info(id,timeout)
    }
    set balloon_bind_info(shown) 0
}

proc balloon_menu_bind {} {
    global balloon_bind_info

    set w Menu

    bind ${w} <<MenuSelect>> {+
		set tmpidx [%W index active]
		catch {
			if {$balloon_bind_info(%W,last_menu_index) == $tmpidx ||
				$tmpidx == "none"} {
				break
			}
		}
		balloon_destroy
		if {![info exists balloon_bind_info(%W,$tmpidx,text)]} {
			catch {unset balloon_bind_info(%W,last_menu_index)}
			break
		}
		set balloon_bind_info(%W,last_menu_index) $tmpidx
		set balloon_bind_info(id) [after $balloon_bind_info(%W,delay)\
      $balloon_bind_info(%W,proc) %W {""} %X %Y]
		unset tmpidx
	}

    bind Menu <Leave> {+
		balloon_destroy
		catch {unset balloon_bind_info(%W,last_menu_index)}
	}
}

#execute balloon bindings only once!
balloon_button_bind

proc balloon_bind_info {w text {delay -1} {procedure "balloon_display_info"}} {
    global sn_options
    global balloon_bind_info

    if {${delay} < 0} {
        set delay $sn_options(def,balloon-disp-delay)
    }
    set balloon_bind_info(${w},text) ${text}
    set balloon_bind_info(${w},delay) ${delay}
    set balloon_bind_info(${w},proc) ${procedure}

    sn_add_tags ${w} SN_Balloon 0
}

proc menu_balloon_bind_info {w idx text {delay -1} {procedure\
  "balloon_display_info"}} {
    global sn_options balloon_bind_info

    if {${delay} < 0} {
        set delay $sn_options(def,balloon-disp-delay)
    }
    set balloon_bind_info(${w},${idx},text) ${text}
    set balloon_bind_info(${w},delay) ${delay}
    set balloon_bind_info(${w},proc) ${procedure}
}

proc balloon_display_info {w text rx ry} {
    global sn_options
    global balloon_bind_info

    if {$balloon_bind_info(shown)} {
        return
    }
    if {[winfo containing [winfo pointerx .] [winfo pointery .]] != ${w}} {
        return
    }
    if {[catch {set bg_color $sn_options(def,balloon-bg)}]} {
        set bg_color lightyellow
    }
    if {[catch {set fg_color $sn_options(def,balloon-fg)}]} {
        set fg_color black
    }

    if {[winfo class ${w}] == "Menu"} {
        if {[catch {set act $balloon_bind_info(${w},last_menu_index)}] ||\
          [catch {set text $balloon_bind_info(${w},${act},text)}]} {
            return
        }

        if {[${w} cget -type] == "menubar"} {
            set y [expr [winfo rooty ${w}] + [winfo height ${w}]]
            set x [winfo rootx ${w}]
        }\
        elseif {${act} == [${w} index end]} {
            set y [expr [winfo rooty ${w}] + [winfo height ${w}]]
            set x [expr [winfo rootx ${w}] + [winfo width ${w}]]
        } else {
            set y [expr [winfo rooty ${w}] + [${w} yposition [expr ${act} +1]]]
            set x [expr [winfo rootx ${w}] + [winfo width ${w}]]
        }
    } else {
        set x ${rx}
        set y [expr [winfo rooty ${w}] + [winfo height ${w}]]
    }

    balloon_destroy

    set balloon_bind_info(shown) 1
    set t .cb_balloon

    toplevel ${t} -bg ${bg_color}
    wm withdraw ${t}

    if {$sn_options(def,desktop-font-size) > 14} {
        set fsize 14
    } else {
        set fsize $sn_options(def,desktop-font-size)
    }
    pack [frame ${t}.f -bd 1 -background black]
    pack [label ${t}.f.l -text ${text} -wraplength 300 -justify left\
      -bg ${bg_color} -fg ${fg_color} -bd 0 -relief raised\
      -font $sn_options(def,balloon-font) -padx 4 -pady 4]
    wm overrideredirect ${t} 1

    if {${y} < 0} {
        set y 0
    }
    set w [expr [winfo reqwidth ${t}.f.l] + 2]
    set h [expr [winfo reqheight ${t}.f.l] + 2]

    # make help window be completely visible
    if {${x} + ${w} > $balloon_bind_info(screen_width)} {
        set x [expr $balloon_bind_info(screen_width) - ${w}]
    }
    if {${y} + ${h} > $balloon_bind_info(screen_height)} {
        set y [expr $balloon_bind_info(screen_height) - ${h}]
    }

    wm geometry ${t} +${x}+${y}
    wm deiconify ${t}

    # remove the balloon window after time-out:
    set balloon_bind_info(id,timeout) [after\
      [expr $sn_options(def,balloon-undisp-delay) + [string length ${text}] *\
      50] "catch \{destroy ${t}\}"]
}

proc canvas_rebind_info {w id text {delay -1} {procedure\
  "canvas_display_info"}} {
    global sn_options
    global balloon_bind_info

    if {${delay} < 0} {
        set delay $sn_options(def,balloon-disp-delay)
    }
    catch {
        ${w} bind ${id} <Enter> "
			set balloon_bind_info(id)  \[after ${delay} ${procedure} ${w} ${id} %X %Y\
          [list "{" ${text} "}"]\]
		"

        ${w} bind ${id} <Motion> "
			balloon_destroy
			set balloon_bind_info(id)  \[after ${delay} ${procedure} ${w} ${id} %X %Y\
          [list "{" ${text} "}"]\]
		"
    }
}

proc canvas_bind_info {w id text {delay -1} {procedure "canvas_display_info"}} {
    global sn_options
    global balloon_bind_info

    if {${delay} < 0} {
        set delay $sn_options(def,balloon-disp-delay)
    }
    #shown meens, that the info window is already displayed, don't
    #display it again
    set balloon_bind_info(shown) 0

    set balloon_bind_info(${w},${id},text) ${text}
    ${w} bind ${id} <Enter> "
		if \[info exists balloon_bind_info(${w},${id},text)\] {
		    set balloon_bind_info(id)  \[after ${delay} ${procedure}  ${w} ${id} %X\
      %Y [list "{" ${text} "}"]\]
		}
	"
    ${w} bind ${id} <Motion> "
		balloon_destroy
		if \[info exists balloon_bind_info(${w},${id},text)\] {
			set balloon_bind_info(id)  \[after ${delay} ${procedure}  ${w} ${id} %X %Y\
      [list "{" ${text} "}"]\]
		}
	"
    ${w} bind ${id} <Leave> {
		balloon_destroy
	}
    ${w} bind ${id} <Any-ButtonPress> [${w} bind ${id} <Leave>]
    ${w} bind ${id} <Any-Key> [${w} bind ${id} <Any-ButtonPress>]
}

proc canvas_display_info {w id rx ry args} {
    global sn_options
    global balloon_bind_info

    if {$balloon_bind_info(shown)} {
        return
    }
    if {[winfo containing [winfo pointerx .] [winfo pointery .]] != ${w}} {
        return
    }
    if {[catch {set bg_color $sn_options(def,balloon-bg)}]} {
        set bg_color lightyellow
    }
    if {[catch {set fg_color $sn_options(def,balloon-fg)}]} {
        set fg_color black
    }
    set balloon_bind_info(shown) 1

    catch {destroy [set t .cb_balloon]}
    toplevel ${t} -bg ${bg_color}
    wm withdraw ${t}

    if {$sn_options(def,desktop-font-size) > 14} {
        set fsize 14
    } else {
        set fsize $sn_options(def,desktop-font-size)
    }

    set text [join ${args}]
    label ${t}.l -text ${text} -wraplength 400 -justify left -bg ${bg_color}\
      -fg ${fg_color} -bd 1 -relief raised -font $sn_options(def,balloon-font)
    pack ${t}.l
    wm overrideredirect ${t} 1

    set x [winfo pointerx ${w}]
    set y [expr [winfo rooty ${w}] - [expr int([${w} canvasy 0])]]
    set bbox [${w} bbox ${id}]
    if {[catch {set y [expr ${y} + [expr [lindex ${bbox} 1] + [expr\
      [lindex ${bbox} 3] - [lindex ${bbox} 1]]]]}]} {
        set y 0
    }

    if {${y} < 0} {
        set y 0
    }
    set wdth [winfo reqwidth ${t}.l]
    set high [winfo reqheight ${t}.l]

    # make help window be completely visible
    if {${x} + ${wdth} > $balloon_bind_info(screen_width)} {
        set x [expr $balloon_bind_info(screen_width) - ${wdth}]
    }
    if {${y} + ${high} > $balloon_bind_info(screen_height)} {
        set y [expr $balloon_bind_info(screen_height) - ${high}]
    }

    wm geometry ${t} +${x}+${y}
    wm deiconify ${t}

    # remove the balloon window after 5 seconds:
    set balloon_bind_info(id,timeout) [after [expr 5000 + [string length\
      ${text}] * 50] "catch \{destroy ${t}\}"]
}

proc sn_show_abbrav {} {
    global sn_options
    global sn_scopes

    set win .sn_abbr
    set t ${win}

    if {[winfo exists ${t}]} {
        ${t} raise
        return
    }
    sourcenav::Window ${t}
    ${t} configure -title [list [get_indep String Abrav]]

    sn_motif_buttons ${t} bottom 0 [get_indep String ok]

    ${t}.button_0 config -command "itcl::delete object ${t}"

    text ${t}.a -width 50 -wrap none -spacing1 2
    set bw 0
    set max 0
    set sc_str ""

    foreach sc "${sn_scopes} lv ud" {
        set desc [convert_scope_to_plain_str ${sc}]

        set f ${t}.a.${sc}
        frame ${f}
        button ${f}.${sc} -image type_${sc}_image -bd ${bw}
        pack ${f}.${sc}
        pack ${f}

        ${t}.a window create end -window ${f}
        ${t}.a insert end " ${sc}:\t${desc}\n"
    }

    ${t}.a insert end ${sc_str}

    ${t}.a insert end "\n\t[get_indep String CrossReference]\n\n"
    ${t}.a insert end "r\t[get_indep String Read]\t\t"
    ${t}.a insert end "w\t[get_indep String Written]\n"
    ${t}.a insert end "p\t[get_indep String Passed]\t\t"
    ${t}.a insert end "u\t[get_indep String Unused]\n"

    ${t}.a insert end "\n\t[get_indep String ClassNoKey]\n\n"

    ${t}.a tag configure protected -font $sn_options(def,protected-font)
    ${t}.a tag configure public -font $sn_options(def,public-font)

    ${t}.a insert end "\t[get_indep String Private]\n"

    set idx [${t}.a index "insert linestart"]
    frame ${t}.a.p
    button ${t}.a.p.b -image cls_br_p_image -bd ${bw}
    pack ${t}.a.p.b
    pack ${t}.a.p

    ${t}.a window create end -window ${t}.a.p
    ${t}.a insert end "\t[get_indep String Protected]\n"

    ${t}.a tag add protected ${idx} insert

    set idx [${t}.a index "insert linestart"]
    frame ${t}.a.pub
    button ${t}.a.pub.b -image cls_br__image -bd ${bw}
    pack ${t}.a.pub.b
    pack ${t}.a.pub

    ${t}.a window create end -window ${t}.a.pub
    ${t}.a insert end "\t[get_indep String Public]\n"

    ${t}.a tag add public ${idx} insert

    frame ${t}.a.v
    button ${t}.a.v.v -image cls_br_v_image -bd ${bw}
    pack ${t}.a.v.v
    pack ${t}.a.v

    ${t}.a window create end -window ${t}.a.v
    ${t}.a insert end " v\t[get_indep String Virtual]\n"

    frame ${t}.a.s
    button ${t}.a.s.s -image cls_br_s_image -bd ${bw}
    pack ${t}.a.s.s
    pack ${t}.a.s

    ${t}.a window create end -window ${t}.a.s
    ${t}.a insert end " s\t[get_indep String Static]\n"

    frame ${t}.a.pl
    button ${t}.a.pl.pl -image cls_br_+_image -bd ${bw}
    pack ${t}.a.pl.pl
    pack ${t}.a.pl

    ${t}.a window create end -window ${t}.a.pl
    ${t}.a insert end " +\t[get_indep String PafAbrOverride]\n"

    frame ${t}.a.min
    button ${t}.a.min.mi -image cls_br_-_image -bd ${bw}
    pack ${t}.a.min.mi
    pack ${t}.a.min

    ${t}.a window create end -window ${t}.a.min
    ${t}.a insert end " -\t[get_indep String PafAbrOverridden]\n"

    set height [expr int([${t}.a index "end -1c"]) + 1]

    ${t}.a config -state disabled -height ${height}

    pack ${t}.a -anchor w -fill x

    ${t} move_to_mouse
    catch {${t} resizable yes no}

    tkwait visibility ${win}

    set idx [lindex [split [${t}.a index end] "."] 0]
    update idletasks

    # This is a nasty hack, dude.
    after idle [list abbr_correction ${t}.a]
}

proc abbr_correction {ed} {
    update idletasks

    if {![winfo exists ${ed}] || [${ed} yview] == "0 0"} {
        return
    }

    set steps 0
    while {[winfo exists ${ed}] && [${ed} yview] != "0 1"} {
        set height [${ed} cget -height]
        incr height +2
        ${ed} config -height ${height}
        update idletasks

        #what happens if the screen height is lesser that
        #the should be window hight. Break the loop here
        #after enough steps.
        if {${steps} > 20} {
            break
        }
        incr steps
    }
}


