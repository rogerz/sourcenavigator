# Copyright (c) 2000, 2001, Red Hat, Inc.
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

# FIXME: We will need to rewrite this class when removing Tree!
itcl_class Selector& {
    inherit Tree

    constructor {config} {
        set cmd ""
        foreach c ${config} {
            append cmd "-${c} {[virtual set ${c}]} "
        }
        eval Tree::constructor -withframe y\
          -exportselection ${exportselection} -have_filter 0 ${cmd}
    } {
        ::global ${this}-e-name

        set thull [namespace tail $this]
        if {${exec_button} == ""} {
            set exec_button ${thull}.button_0
        }

        #entry for selected directory/file
        if {${direct_ent} != ""} {
            set direct_ent ${thull}.direct_ent

            entry ${direct_ent} -relief ridge -borderwidth 2\
              -exportselection 0 -textvariable ${this}-e-name

            ::bind ${direct_ent} <KeyPress> "[info class] @@ check_input\
              ${direct_ent} ${exec_button}"

            if {${must_exist} && ${entrycommand} != ""} {
                ::bind ${direct_ent} <Return> "eval ${entrycommand} %W; break"
            }

            pack ${direct_ent} -before ${lframe} -side top -expand n -fill x\
              -padx 2 -pady 2

            focus ${direct_ent}
            set ${this}-e-name ${advised}
            ${this} direct_ent select range 0 end
        }

        frame ${thull}.filter
        label ${thull}.filter.label -text [get_indep String Pattern]

        set entry ${thull}.filter.entry
        entry ${entry} -relief sunken -exportselection 0 -width ${filter_width}

        ${entry} insert end ${filter}
        ${entry} icursor 0

        ::bind ${entry} <Return> "${this} fill; break"

        pack ${thull}.filter.label -side left
        pack ${entry} -side left -fill x -expand y -padx 3

        pack ${thull}.filter -before ${lframe} -side bottom -expand n -fill x

        if {[string compare ${contents} ""] != 0} {
            ${this} fill 0
        }

        ${this} treebind <ButtonRelease-1> "+focus %W;${this} set_selection"
    }

    destructor {
        catch {destroy ${thull}.filter}

        foreach v [::info globals "${this}-*"] {
            catch {uplevel #0 unset ${v}}
        }
    }

    method set_selection {} {
        if {${direct_ent} != ""} {
            global ${this}-e-name
            set ${this}-e-name [${this} marked]
            ${direct_ent} selection range 0 end
        }

        return [${this} marked]
    }

    public exec_button ""

    method fill {{bsy 1}} {
        if {![winfo exists ${thull}] || ![winfo exists ${entry}]} {
            return
        }

        set filter [${entry} get]
        if {[string compare ${filter} ""] == 0} {
            set filter "*"
            ${entry} insert end "*"
            ${entry} icursor 0
        }

        if {[string compare ${contents} ""] == 0} {
            ${this} delete_tk 0 end

            return
        }

        set act_idx [${this} index active]
        ${tree} delete 0 end

        if {${eval}} {
            set cmd ${contents}
            # It contains a tcl command.

            regsub {%f} ${cmd} ${filter} cmd

            if {${sort} != ""} {
                if {${nocase}} {
                    if {${imagecmd} == ""} {
                        ${tree} insert end list [::lsort -command sn_compare\
                          ${sort} [::eval ${cmd}]]
                    } else {
                        ${tree} insert end list [::lsort -command sn_compare\
                          ${sort} [::eval ${cmd}]] ${imagecmd} ${image}
                    }
                } else {
                    if {${imagecmd} == ""} {
                        ${tree} insert end list [::lsort ${sort}\
                          [::eval ${cmd}]]
                    } else {
                        ${tree} insert end list [::lsort ${sort}\
                          [::eval ${cmd}]] ${imagecmd} ${image}
                    }
                }
            } else {
                if {${imagecmd} == ""} {
                    ${tree} insert end list [::eval ${cmd}]
                } else {
                    ${tree} insert end list [::eval ${cmd}] ${imagecmd} ${image}
                }
            }
        } else {
            if {[string compare ${filter} "*"] != 0} {
                if {${nocase}} {
                    set flt [nocase_glob_pattern ${filter}]
                } else {
                    set flt ${filter}
                }
                if {${imagecmd} == ""} {
                    ${tree} insert end list [lmatch ${contents} ${flt}]
                } else {
                    ${tree} insert end list [lmatch ${contents} ${flt}]\
                      ${imagecmd} ${image}
                }
            } else {
                if {${imagecmd} == ""} {
                    ${tree} insert end list ${contents}
                } else {
                    ${tree} insert end list ${contents} ${imagecmd} ${image}
                }
            }
        }

        ${tree} activate ${act_idx}
    }

    proc nocase_glob_pattern {flt} {
        if {[string compare ${flt} "*"] == 0} {
            return ${flt}
        }
        for {set c 0; set brace_lev 0; set m ""; set glb_pat ""; set flt\
          [string tolower ${flt}]} {${c} < [string length ${flt}]} {incr c} {

            set ch [string index ${flt} ${c}]
            switch -glob -- ${ch} {
                {[A-Za-z]} {
                        if {${brace_lev} <= 0} {
                            append m \[ [string tolower ${ch}] [string toupper\
                              ${ch}] \]
                        } else {
                            append glb_pat ${ch}
                        }
                    }
                {\[} {
                        append m \[
                        set glb_pat ""
                        incr brace_lev
                    }
                {\]} {
                        for {set k 0} {${k} < [string length ${glb_pat}]}\
                          {incr k} {
                            set cc [string index ${glb_pat} ${k}]
                            set nc [string index ${glb_pat} [expr ${k} + 1]]
                            append m [string tolower ${cc}]
                            if {[string compare ${nc} "-"] == 0} {
                                incr k 2
                                set nc [string index ${glb_pat} ${k}]
                                append m "-" [string tolower ${nc}]\
                                  [string toupper ${cc}] "-" [string toupper\
                                  ${nc}]
                            } else {
                                append m [string toupper ${cc}]
                            }
                        }
                        set glb_pat ""
                        append m \]
                        incr brace_lev -1
                    }
                {\\} {
                        incr c
                        append m "\\" [string index ${flt} ${c}]
                    }
                default {
                        if {${brace_lev} <= 0} {
                            append m ${ch}
                        } else {
                            append glb_pat ${ch}
                        }
                    }
            }
        }
        return ${m}
    }

    method marked {{str 1}} {
        set sel [${this} curselection]

        if {${str}} {
            set val ""
            foreach s ${sel} {
                lappend val [${this} get ${s}]
            }
            return ${val}
        } else {
            return ${sel}
        }
    }

    method direct_ent {args} {
        if {${direct_ent} != ""} {
            return [eval ${direct_ent} ${args}]
        }
    }

    method values {} {
        return [${this} get 0 end]
    }

    method filter {{pattern ""}} {
        if {[string compare ${pattern} ""] == 0} {
            return ${filter}
        }
        ${this} config -filter ${pattern}
    }

    proc check_input {entry button} {
        if {[string compare [string trim [${entry} get]] ""] == 0} {
            set state disabled
        } else {
            set state normal
        }
        ${button} config -state ${state}
    }

    public contents {} {
        if {[winfo exists ${this}]} {
            set y [${this} nearest 0]

            if {!${eval} && ${sort} != ""} {
                if {${nocase}} {
                    set contents [::lsort ${sort} -command sn_compare\
                      ${contents}]
                } else {
                    set contents [::lsort ${sort} ${contents}]
                }
            }

            ${this} fill 0
            # This is necessary, otherwise the listbox scrolls
            # back to the top.
            ${this} yview ${y}
        }
    }

    method setSelectorContents {{data ""}} {
        if {$data == ""} {
            # Nothing to set.
            return
        }

        set contents $data
        if {[winfo exists ${this}]} {
            set y [${this} nearest 0]

            if {!${eval} && ${sort} != ""} {
                if {${nocase}} {
                    set contents [::lsort ${sort} -command sn_compare\
                      ${contents}]
                } else {
                    set contents [::lsort ${sort} ${contents}]
                }
            }

            ${this} fill 0
            # This is necessary, otherwise the listbox scrolls
            # back to the top.
            ${this} yview ${y}
        }

    }

    public filter {*} {
        if {[winfo exists ${this}]} {
            ${entry} delete 0 end
            ${entry} insert end ${filter}
            ${entry} icursor 0

            if {[string compare ${contents} ""] != 0} {
                if {${eval}} {
                    set bsy 1
                } else {
                    set bsy 0
                }
                ${this} fill ${bsy}
            }
        }
    }

    public script {}

    protected entry {}
    public sort {-increasing}
    public direct_ent {}
    public advised {}
    public exportselection 0 {
        if {[winfo exists ${this}]} {
            ${this} config_tk -exportselection ${exportselection}
        }
    }
    public nocase 1
    public filter_width 22 {
        if {[winfo exists ${entry}]} {
            ${entry} config -width ${filter_width}
        }
    }
    protected imagecmd ""
    public eval 0
    public image "" {
        set imagecmd "-image"
    }
    public bitmap "" {
        set imagecmd "-bitmap"
        set image ${bitmap}
    }
    public must_exist 0
    public entrycommand ""
    protected thull ""
}
