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
# elix.tcl - EL/IX configuration dialog.
# Copyright (C) 1999 Cygnus Soluations.
# Written by Tom Tromey <tromey@cygnus.com>.

itcl_class Elix& {
    inherit sourcenav::Window

    constructor {{config ""}} {
        withdraw

        # Initialize EL/IX options.
        set ttys {/dev/ttyS0 /dev/ttyS1 /dev/ttyS2 /dev/ttyS3 /dev/ttyS4}
        global elix_options
        fill elix_options

        set NoteBook [tixNoteBook ${this}.nbook -ipadx 0 -ipady 0\
          -borderwidth 0 -background white]

        ${NoteBook} add target -label [get_indep String ElixTarget]\
          -under [get_indep Pos ElixTarget]

        ${NoteBook} add filesys -label [get_indep String ElixFilesys]\
          -under [get_indep Pos ElixFilesys]

        ${NoteBook} add mounts -label [get_indep String ElixMounts]\
          -under [get_indep Pos ElixMounts]

        ${NoteBook} add kernel -label [get_indep String ElixKernel]\
          -under [get_indep Pos ElixKernel]

        ${NoteBook} add debug -label [get_indep String ElixDebug]\
          -under [get_indep Pos ElixDebug]

        sn_motif_buttons ${this} bottom 0 [get_indep String ok]\
          [get_indep String Apply] [get_indep String cancel]

        ${this}.button_0 config -command " ${this} apply "
        ${this}.button_1 config -command " ${this} apply 0 "\
          -underline [get_indep Pos Apply]
        ${this}.button_2 config -command " ${this} exitPreferences "

        pack ${NoteBook} -fill both -expand 1 -padx 4 -pady 4

        AddTarget ${NoteBook} target
        AddFilesys ${NoteBook} filesys
        AddMount ${NoteBook} mounts
        AddKernel ${NoteBook} kernel
        AddDebug ${NoteBook} debug

        ${NoteBook} raise target

        title [sn_title [get_indep String ElixPreferences] 0]
        ${this} centerOnScreen
    }

    # We'd use a radiobutton-on-a-frame widget if
    # there was one, but there isn't.
    method radioframe {name args} {
        frame ${name} -relief flat -borderwidth 0

        frame ${name}.iframe -relief groove -borderwidth 2
        ::grid ${name}.iframe -row 1 -sticky news
        ::grid rowconfigure ${name} 1 -weight 1
        ::grid columnconfigure ${name} 0 -weight 1

        frame ${name}.iframe.childframe -borderwidth 4 -relief flat
        ::grid ${name}.iframe.childframe -row 1 -sticky news
        ::grid rowconfigure ${name}.iframe 1 -weight 1
        ::grid columnconfigure ${name}.iframe 0 -weight 1

        # Note that radio_update knows the name of this
        # radiobutton.
        eval radiobutton ${name}.radio -padx 2 ${args}
        set height [expr {int([winfo reqheight ${name}.radio] / 2)}]
        ::grid rowconfigure ${name} 0 -minsize ${height} -weight 0
        ::grid rowconfigure ${name}.iframe 0 -minsize ${height} -weight 0

        place ${name}.radio -in ${name}.iframe -relx 0 -x 6 -rely 0 -y -2\
          -anchor w

        return ${name}.iframe.childframe
    }

    # A helper proc used when using radioframe.  This enables or
    # disables the contents of related frames.  ENABLE is the
    # frame to enable; DISABLED lists all the frames to process
    # (including ENABLE).
    method radio_update {enable disabled} {
        foreach frame ${disabled} {
            if {${frame} == ${enable}} {
                set state normal
            } else {
                set state disabled
            }
            set wlist [winfo children ${frame}]
            while {[llength ${wlist}] > 0} {
                set w [lindex ${wlist} 0]
                set wlist [lreplace ${wlist} 0 0]

                if {${w} == "${frame}.radio"} {
                    continue
                }

                catch {${w} configure -state ${state}}

                eval lappend wlist [winfo children ${w}]
            }
        }
    }

    method AddFilesys {nb page} {
        global sn_options elix_options

        # Ugly, but S-N standard.
        set xstep 30

        set page [${nb} subwidget ${page}]

        set frame [tixLabelFrame ${page}.frame -label [get_indep String\
          ElixFilesys] -background $sn_options(def,layout-bg)]
        set child [${frame} subwidget frame]
        pack ${frame} -side top -fill x

        set frames [list ${child}.romfs ${child}.nfs ${child}.minix]
        set romfs [radioframe ${child}.romfs -text [get_indep String\
          ElixROMfs] -variable elix_options(file-system) -value romfs\
          -command [list ${this} radio_update ${child}.romfs ${frames}]]

        LabelEntryButton& ${romfs}.filesys -labelwidth ${xstep}\
          -text [get_indep String ElixFilesysROMPath]\
          -variable elix_options(file-rom-path) -directory 1
        pack ${romfs}.filesys -side top -fill x -padx 4 -pady 4

        set minix [radioframe ${child}.minix -text [get_indep String\
          ElixMinix] -variable elix_options(file-system) -value minix\
          -command [list ${this} radio_update ${child}.minix ${frames}]]

        LabelEntryButton& ${minix}.filesys -labelwidth ${xstep}\
          -text [get_indep String ElixFilesysMinixPath]\
          -variable elix_options(file-minix-path) -directory 1

        set sframes [list ${minix}.relative ${minix}.absolute]
        set rel [radioframe ${minix}.relative -text [get_indep String\
          ElixRelative] -variable elix_options(file-minix-choice)\
          -value relative -command [list ${this} radio_update\
          ${minix}.relative ${sframes}]]

        Entry& ${rel}.extra -labelwidth ${xstep} -label [get_indep String\
          ElixExtra] -textvariable elix_options(file-minix-extra)
        pack ${rel}.extra -side top -anchor w -fill x -padx 4 -pady 4


        set abs [radioframe ${minix}.absolute -text [get_indep String\
          ElixAbsolute] -variable elix_options(file-minix-choice)\
          -value absolute -command [list ${this} radio_update\
          ${minix}.absolute ${sframes}]]

        Entry& ${abs}.blocks -labelwidth ${xstep} -label [get_indep String\
          ElixBlocks] -textvariable elix_options(file-minix-blocks)
        Entry& ${abs}.inodes -labelwidth ${xstep} -label [get_indep String\
          ElixInodes] -textvariable elix_options(file-minix-inodes)

        pack ${abs}.blocks ${abs}.inodes -side top -anchor w -fill x -padx 4\
          -pady 4
        pack ${minix}.filesys ${minix}.relative ${minix}.absolute -side top\
          -fill x -padx 4 -pady 4

        set nfsframe [radioframe ${child}.nfs -text [get_indep String ElixNFS]\
          -variable elix_options(file-system) -value nfs -command\
          [list ${this} radio_update ${child}.nfs ${frames}]]

        LabelEntryButton& ${nfsframe}.filesys -labelwidth ${xstep}\
          -text [get_indep String ElixFilesysNFSPath]\
          -variable elix_options(file-nfs-path) -directory 1
        pack ${nfsframe}.filesys -side top -fill x -padx 4 -pady 4


        pack ${child}.romfs -side top -fill x -padx 4 -pady 4
        pack ${child}.minix -side top -fill x -padx 4 -pady 4
        pack ${child}.nfs -side top -fill x -padx 4 -pady 4

        switch -- $elix_options(file-system) {
            romfs {
                    radio_update ${child}.romfs ${frames}
                }
            nfs {
                    radio_update ${child}.nfs ${frames}
                }
            minix {
                    radio_update ${child}.minix ${frames}
                }
        }

        if {$elix_options(file-minix-choice) == "relative"} {
            radio_update ${minix}.relative ${sframes}
        } else {
            radio_update ${minix}.absolute ${sframes}
        }
    }

    method AddDebug {nb page} {
        global sn_options elix_options

        # Ugly, but S-N standard.
        set xstep 30

        set page [${nb} subwidget ${page}]

        set frame [tixLabelFrame ${page}.frame -label [get_indep String\
          ElixDebug] -background $sn_options(def,layout-bg)]
        set child [${frame} subwidget frame]
        pack ${frame} -side top -fill x

        set frames [list ${child}.ether ${child}.serial]

        set etherframe [radioframe ${child}.ether -text [get_indep String\
          ElixEthernet] -variable elix_options(dbg-interface) -value ethernet\
          -command [list ${this} radio_update ${child}.ether ${frames}]]

        Entry& ${etherframe}.port -width 10 -labelwidth ${xstep}\
          -label [get_indep String ElixPortnumber]\
          -textvariable elix_options(dbg-portnumber)

        pack ${etherframe}.port -side top -anchor w

        set serframe [radioframe ${child}.serial -text [get_indep String\
          ElixSerial] -variable elix_options(dbg-interface) -value serial\
          -command [list ${this} radio_update ${child}.serial ${frames}]]

        Combo& ${serframe}.tport -labelwidth ${xstep} -label [get_indep String\
          ElixTargetport] -contents ${ttys}\
          -entryvariable elix_options(dbg-targettty)
        Combo& ${serframe}.hport -labelwidth ${xstep} -label [get_indep String\
          ElixHostport] -contents ${ttys}\
          -entryvariable elix_options(dbg-hosttty)
        Combo& ${serframe}.speed -labelwidth ${xstep} -label [get_indep String\
          ElixSpeed] -contents {9600 19200 38400 56000 115200}\
          -entryvariable elix_options(dbg-speed)

        pack ${serframe}.tport -side top -anchor w
        pack ${serframe}.hport -side top -anchor w
        pack ${serframe}.speed -side top -anchor w

        pack ${child}.ether -side top -fill x -padx 4 -pady 4
        pack ${child}.serial -side top -fill x -padx 4 -pady 4

        if {$elix_options(dbg-interface) == "serial"} {
            radio_update ${child}.serial ${frames}
        } else {
            radio_update ${child}.ether ${frames}
        }
    }

    method AddKernel {nb page} {
        global sn_options elix_options

        # Ugly, but S-N standard.
        set xstep 30

        set page [${nb} subwidget ${page}]

        set frame [tixLabelFrame ${page}.frame -label [get_indep String\
          ElixKernel] -background $sn_options(def,layout-bg)]
        set child [${frame} subwidget frame]
        pack ${frame} -side top -fill x

        Entry& ${child}.args -labelwidth ${xstep} -label [get_indep String\
          ElixKernelArgs] -textvariable elix_options(kernel-args)

        LabelEntryButton& ${child}.sources -labelwidth ${xstep}\
          -text [get_indep String ElixKernelSources]\
          -variable elix_options(kernel-sources) -directory 1

        tixLabelFrame ${child}.modules -label [get_indep String\
          ElixKernelModules] -background $sn_options(def,layout-bg)
        set m [${child}.modules subwidget frame]

        radiobutton ${m}.monolith -text [get_indep String\
          ElixKernelMonolithic] -variable elix_options(kernel-modules)\
          -value monolithic -padx 4
        radiobutton ${m}.all -text [get_indep String ElixKernelAll]\
          -variable elix_options(kernel-modules) -value all -padx 4
        radiobutton ${m}.some -text [get_indep String ElixKernelSome]\
          -variable elix_options(kernel-modules) -value some -padx 4

        # S-N tradition seems to be to use a text widget and
        # not a listbox here, so that is what we do for now.
        # This will change once we are able to determine a
        # definitive list of modules somehow.
        # FIXME: format of text widget is `module,options\n'...
        # This makes the GUI ugly for now.
        tixLabelFrame ${m}.modules -label [get_indep String\
          ElixKernelModuleList] -background $sn_options(def,layout-bg)
        set module_widget [text [${m}.modules subwidget frame].modules\
          -height 10]
        ${module_widget} insert end [join $elix_options(kernel-module-list) \n]
        pack ${module_widget} -side top -fill x -padx 4 -pady 4

        ::grid ${m}.monolith ${m}.all ${m}.some -padx 4 -pady 4 -sticky wn
        ::grid ${m}.modules -sticky nsew -padx 4 -pady 4 -columnspan 3
        ::grid rowconfigure ${m} 0 -weight 0
        ::grid rowconfigure ${m} 1 -weight 1
        ::grid columnconfigure ${m} 0 -weight 1
        ::grid columnconfigure ${m} 1 -weight 1
        ::grid columnconfigure ${m} 2 -weight 1

        pack ${child}.args -side top -fill x -anchor w -padx 4 -pady 4
        pack ${child}.sources -side top -fill x -anchor w -padx 4 -pady 4
        pack ${child}.modules -side top -fill x -anchor w -padx 4 -pady 4
    }

    method AddTarget {nb page} {
        global sn_options elix_options

        # Ugly, but S-N standard.
        set xstep 30

        set page [${nb} subwidget ${page}]

        set frame [tixLabelFrame ${page}.frame -label [get_indep String\
          ElixTarget] -background $sn_options(def,layout-bg)]
        set child [${frame} subwidget frame]
        pack ${frame} -side top -fill x

        Entry& ${child}.hostname -label [get_indep String ElixTargetHostname]\
          -labelwidth ${xstep} -textvariable elix_options(target-hostname)

        LabelEntryButton& ${child}.bootimage -labelwidth ${xstep}\
          -text [get_indep String ElixFilesysBootimage]\
          -variable elix_options(file-boot-image)

        tixLabelFrame ${child}.ip -label [get_indep String ElixTargetIP]\
          -background $sn_options(def,layout-bg)
        set ip [${child}.ip subwidget frame]

        set frames [list ${ip}.static]
        # -padx value here comes from radioframe + its own
        # padding.
        radiobutton ${ip}.kernel -text [get_indep String ElixTargetKernel]\
          -variable elix_options(target-ip-sel) -value kernel -padx 10\
          -command [list ${this} radio_update {} ${frames}]
        radiobutton ${ip}.dhcp -text [get_indep String ElixTargetDHCP]\
          -variable elix_options(target-ip-sel) -value dhcp -padx 10\
          -command [list ${this} radio_update {} ${frames}]
        set s [radioframe ${ip}.static -text [get_indep String\
          ElixTargetStatic] -variable elix_options(target-ip-sel)\
          -value static -command [list ${this} radio_update ${ip}.static\
          ${frames}]]
        Entry& ${s}.ip -label [get_indep String ElixTargetStaticIP]\
          -labelwidth ${xstep} -textvariable elix_options(target-ip)
        Entry& ${s}.mask -label [get_indep String ElixTargetStaticMask]\
          -labelwidth ${xstep} -textvariable elix_options(target-mask)
        Entry& ${s}.broad -labelwidth ${xstep} -label [get_indep String\
          ElixTargetStaticBroadcast]\
          -textvariable elix_options(target-broadcast)
        Entry& ${s}.gateway -labelwidth ${xstep} -label [get_indep String\
          ElixTargetStaticGateway] -textvariable elix_options(target-gateway)

        pack ${s}.ip ${s}.mask ${s}.broad ${s}.gateway -side top -anchor w\
          -fill x -padx 4 -pady 4

        pack ${ip}.kernel ${ip}.dhcp -side top -anchor w -padx 4 -pady 4
        pack ${ip}.static -fill x -side top -anchor w -padx 4 -pady 4

        pack ${child}.hostname ${child}.bootimage ${child}.ip -fill x\
          -side top -anchor w -padx 4 -pady 4

        # FIXME: mount points here

        switch -- $elix_options(target-ip-sel) {
            kernel -
            dhcp {
                    radio_update {} ${frames}
                }
            static {
                    radio_update ${ip}.static ${frames}
                }
        }
    }

    method AddMount {nb page} {
        global sn_options elix_options

        set page [${nb} subwidget ${page}]

        set frame [tixLabelFrame ${page}.frame -label [get_indep String\
          ElixMounts] -background $sn_options(def,layout-bg)]
        set child [${frame} subwidget frame]
        pack ${frame} -side top -fill x

        # FIXME: don't hard-code text here.
        message ${child}.message -aspect 1000 -text "Enter one mount entry per\
          line.  Each entry consists of three whitespace-separated fields:\
          device, mount point, and filesystem type"

        set mount_widget [text ${child}.modules]
        ${mount_widget} insert end [join $elix_options(mount-points) \n]

        pack ${child}.message ${mount_widget} -side top -fill x -padx 4 -pady 4
    }

    # Set EL/IX defaults in project database.
    proc set_defaults {} {
        paf_db_proj put elix-dbg-interface ethernet
        paf_db_proj put elix-dbg-portnumber 9999
        paf_db_proj put elix-dbg-targettty /dev/ttyS0
        paf_db_proj put elix-dbg-hosttty /dev/ttyS0
        paf_db_proj put elix-dbg-speed 9600
        paf_db_proj put elix-kernel-args ""
        paf_db_proj put elix-kernel-sources ""
        paf_db_proj put elix-file-system romfs
        paf_db_proj put elix-file-minix-path ""
        paf_db_proj put elix-file-nfs-path ""
        paf_db_proj put elix-file-rom-path ""
        paf_db_proj put elix-file-boot-image ""
        paf_db_proj put elix-kernel-modules all
        paf_db_proj put elix-kernel-module-list ""
        paf_db_proj put elix-target-hostname ""
        paf_db_proj put elix-target-ip-sel kernel
        paf_db_proj put elix-target-ip ""
        paf_db_proj put elix-target-mask ""
        paf_db_proj put elix-target-broadcast ""
        paf_db_proj put elix-target-gateway ""
        paf_db_proj put elix-kernel-module-list ""
        paf_db_proj put elix-file-minix-choice relative
        paf_db_proj put elix-file-minix-blocks ""
        paf_db_proj put elix-file-minix-inodes ""
        paf_db_proj put elix-file-minix-extra 10
        paf_db_proj put elix-mount-points ""
    }

    # Return list of key names.
    proc keys {} {
        return {dbg-interface dbg-portnumber dbg-targettty dbg-hosttty\
          dbg-speed kernel-args kernel-sources file-system file-boot-image\
          file-nfs-path file-minix-path file-rom-path kernel-modules\
          kernel-module-list target-hostname target-ip-sel target-ip\
          target-mask target-broadcast target-gateway file-minix-choice\
          file-minix-blocks file-minix-inodes file-minix-extra mount-points}
    }

    # Fill an array with info we care about from the database.
    proc fill {an} {
        upvar ${an} vals
        foreach item [keys] {
            set vals(${item}) [paf_db_proj get -key elix-${item}]
        }
    }

    # Return path to EL/IX sources.
    proc source_path {} {
        # FIXME: is this right?
        global sn_home
        return [file join ${sn_home} share elix]
    }

    # Create the config.pl command line.
    proc make_config_command {arch builddir} {
        global sn_options sn_path

        fill options

        # FIXME: probably don't need path here.
        set command [list [file join $sn_path(bindir) config.pl]]

        lappend command --arch=${arch}

        lappend command --elix-cfg=${builddir}.elix
        lappend command --elix-path=[source_path]
        lappend command --boot-image=$options(file-boot-image)

        if {$options(file-system) == "nfs"} {
            lappend command --nfsroot=$options(file-nfs-path)
        } else {
            set ram --ramdisk
            if {$options(file-system) == "minix"} {
                append ram :template=$options(file-minix-path)
                append ram :type=minix
                if {$options(file-minix-choice) == "relative"} {
                    append ram :extra=$options(file-minix-extra)
                } else {
                    append ram :blocks=$options(file-minix-blocks)
                    append ram :inodes=$options(file-minix-inodes)
                }
            } else {
                append ram :template=$options(file-rom-path)
                append ram :type=romfs
            }
            lappend command ${ram}
        }

        # Debug page.
        set debug --debug
        append debug :$options(dbg-interface)
        append debug :gdbinit=${builddir}.sngdb
        if {$options(dbg-interface) == "serial"} {
            append debug :target=$options(dbg-targettty)
            append debug :host=$options(dbg-hosttty)
            append debug :baud=$options(dbg-speed)
        } else {
            append debug :port=$options(dbg-portnumber)
        }
        lappend command ${debug}

        # Kernel page
        set kernel --kernel
        append kernel :path=$options(kernel-sources)
        if {$options(kernel-modules) != "monolithic"} {
            if {$options(kernel-modules) == "all"} {
                append kernel :all-modules
            }
            foreach m $options(kernel-module-list) {
                if {${m} != ""} {
                    append kernel :module=${m}
                }
            }
        }
        lappend command ${kernel}
        lappend command --kernel-args=$options(kernel-args)

        set target --net
        switch -- $options(target-ip-sel) {
            kernel {
                    append target :kernel
                }
            dhcp {
                    append target :dhcp
                }
            static {
                    append target :ip=$options(target-ip)
                    append target :mask=$options(target-mask)
                    append target :broadcast=$options(target-broadcast)
                    append target :gateway=$options(target-gateway)
                }
        }
        append target :hostname=$options(target-hostname)
        lappend command ${target}

        # Mount points page.
        set mount --mount
        foreach line $options(mount-points) {
            set line [string trim ${line}]
            if {${line} == ""} {
                continue
            }
            regsub -all -- "\[ \t\]+" ${line} " " line
            set split [split ${line}]
            # Supply trailing arguments if not provided by
            # user.
            if {[llength ${split}] < 4} {
                lappend split MS_MGC_VAL
            }
            if {[llength ${split}] < 5} {
                lappend split NULL
            }
            append mount :mp=[join ${split} ,]
        }
        if {${mount} != "--mount"} {
            lappend command ${mount}
        }

        return [join ${command} " "]
    }

    method exitPreferences {} {
        # FIXME: set wait var, etc.
        itcl::delete object ${this}
    }

    method apply {{terminate 1}} {
        # FIXME: verify options.

        global elix_options

        if {${module_widget} != ""} {
            set elix_options(kernel-module-list) [split\
              [${module_widget} get 1.0 end] \n]
        }
        if {${mount_widget} != ""} {
            set elix_options(mount-points) [split [${mount_widget} get 1.0\
              end] \n]
        }

        foreach item [keys] {
            paf_db_proj put elix-${item} $elix_options(${item})
        }
        paf_db_proj sync

        if {${terminate}} {
            exitPreferences
        }
    }

    method save_cb {} {
        apply 0
        save_default_settings
    }

    # List of all tty devices we support.
    protected ttys
    # Name of text widget holding list of selected modules.
    protected module_widget ""
    # Name of text widget holding mount point information.
    protected mount_widget ""
}

proc sn_elix_preferences {} {
    set p .proj-elix
    if {! [winfo exists ${p}]} {
        Elix& ${p}
    } else {
        ${p} raise
    }
}

