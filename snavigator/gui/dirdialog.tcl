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
# dirdialog.tcl - A directory dialog widget. 
# Copyright (C) 1998 Cygnus Solutions.

itcl::class sourcenav::DirDialog {
    inherit sourcenav::Dialog

    constructor { args } {}

    private method ok {}

    private method display {path {parent -1}}

    private method build_filepath {y}

    private method expand {x y}

    protected variable selection ""

    itk_option define -initialdir initialdir InitialDir "/"

    public variable script ""

    public variable width 40
    public variable height 25
}


itcl::body sourcenav::DirDialog::constructor { args } {
    global sn_options

    eval itk_initialize $args

    ${this} transient [winfo parent $itk_component(hull)]

    sn_motif_buttons $itk_component(hull) bottom 0 [get_indep String ok]\
        [get_indep String cancel]

    set ok_button $itk_component(hull).button_0

    itk_component add ok {
        set ok_button
    } {}

    set cancel_button $itk_component(hull).button_1

    itk_component add cancel {
        set cancel_button
    } {}

    itk_component add tree {
        Tree $itk_component(hull).dirtree \
            -fillselection 0 \
            -selectmode browse \
            -exportselection 0 \
            -font $sn_options(def,default-font) \
            -indentwidth 20 \
            -width ${width} \
            -height ${height} \
            -filter "" \
            -plusimage plus_image \
            -minusimage minus_image \
            -unknownimage unknown_image
    } {}

    itk_component add treew {
        $itk_component(tree) tree
    } {}

    pack $itk_component(tree) -side top -expand y -fill both -padx 5 -pady 5

    $itk_component(ok) configure -command [itcl::code ${this} ok]
    $itk_component(cancel) configure -command "$this deactivate"

    bind $itk_component(treew) <1> [itcl::code ${this} expand %x %y]

    # call glob on the path first so that
    # a path like ~/foo becomes /home/user/foo
    set path [glob -nocomplain $itk_option(-initialdir)]
    sn_log "got \"$path\" from glob of \"$itk_option(-initialdir)\""

    # If glob does not match anything then the
    # directory must not exists, just use /
    if {${path} == {}} {
        set path /
    }

    if {[file pathtype ${path}] != "absolute"} {
        cd ${path}
        set path [pwd]
    }

    sn_log "fully qualified path name is \"$path\""

    set pathlist [file split ${path}]

    set root [lindex ${pathlist} 0]
    set pathlist [lreplace ${pathlist} 0 0]

    $itk_component(treew) insert end -text ${root} -image dir_image -unknown yes

    ${this} display ${root} 0
    set parent 0

    if {[llength ${pathlist}] > 0} {
        set prefix ${root}
        foreach dir ${pathlist} {
            if {${dir} == ""} {
                error "empty path element on \"${pathlist}\""
            }
            set prefix [file join ${prefix} ${dir}]
            set ch [$itk_component(treew) children ${parent}]
            if {${ch} == ""} {
                break
            }
            foreach idx ${ch} {
                set txt [$itk_component(treew) get ${idx}]
                if {${txt} == ${dir}} {
                    ${this} display ${prefix} ${idx}
                    set parent ${idx}
                    break
                }
            }
        }
    }
    $itk_component(treew) selection set ${parent}

    # Get total number of elements in the tree
    set size [$itk_component(treew) size]

    # Get total number of elements we could display
    set height [$itk_component(treew) cget -height]

    # Figure out what index we should "see", if we can display
    # all the elements just "see" index 0. Otherwise, attempt
    # to show as many items as we can unless the "see" index
    # is below the directory we want to display.

    set see [expr {${size} - ${height}}]
    if {${see} < 0} {
        set see 0
    } elseif {${see} > ${parent}} {
        set see ${parent}
    }
    $itk_component(treew) see -top ${see}

    ${this} configure -title [get_indep String SelectDirectory]
}

itcl::body sourcenav::DirDialog::ok {} {
    set idx [$itk_component(treew) curselection]
    if {${idx} != ""} {
        set path [${this} build_filepath ${idx}]
    } else {
        set path ""
    }

    $this deactivate ${path}
}

itcl::body sourcenav::DirDialog::display {path {parent -1}} {
    global sn_options

    set selection ${path}

    set fil_list [sn_glob -dirvar dirnames -dirlevel 1\
        -ignore $sn_options(def,ignored-directories) ${path}]

    set names ""
    foreach dir ${dirnames} {
        if {${dir} == ${path}} {
            continue
        }
        lappend names [file tail ${dir}]
    }

    if {${names} == ""} {
        $itk_component(treew) itemconfig ${parent} -unknown 0
        return
    }
    $itk_component(treew) insert end list [::lsort -dictionary ${names}]\
        -parent ${parent} -image dir_image -unknown yes
    $itk_component(treew) itemconfig ${parent} -unknown 0
}

itcl::body sourcenav::DirDialog::build_filepath { y } {
    set file [$itk_component(treew) get ${y}]
    set parent [$itk_component(treew) itemcget ${y} -parent]
    while {${parent} > -1} {
        set file [file join [$itk_component(treew) get ${parent}] ${file}]
        set parent [$itk_component(treew) itemcget ${parent} -parent]
    }
    return ${file}
}

itcl::body sourcenav::DirDialog::expand {x y} {
    set id [$itk_component(treew) identify ${x} ${y}]
    set idx [$itk_component(treew) nearest ${y}]

    if {${id} == "view" || ${id} == "hide"} {
        $itk_component(treew) toggle @${x},${y}
        return
    }

    if {${id} == "text"} {
        return
    }

    set path [$this build_filepath ${idx}]
    ${this} display ${path} ${idx}
}

