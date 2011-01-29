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
# highlight.tcl - On-the-fly syntax highlighting. 
# Copyright (C) 1998 Cygnus Solutions.

proc synch_highlight {w idx} {
    global sn_text_highlight_group

    if {[catch {set grp $sn_text_highlight_group(${w})} err]} {
        return
    }
    set highligh_tags {^(rem|str|key)$}

    # Check whether we are at a tag boundary!
    set tag_at_idx [${w} dump -tag ${idx}]
    foreach {on_off name pos} ${tag_at_idx} {
        if {[lsearch -regexp ${name} ${highligh_tags}] != -1} {
            if {${on_off} == "tagoff"} {
                set col -1
                scan ${pos} {%*d %*c %d} col
                if {${col} == 0} {
                    continue
                }
                set begidx [lindex [${w} tag prevrange ${name} ${idx}] 0]
            } else {
                set begidx ${pos}
            }
            Sn_Syntax_Highlight -update ${grp} ${w} ${begidx} end
            return
        }
    }
    set begidx ${idx}
    set tag [lmatch -regexp [${w} tag names ${idx}] ${highligh_tags}]

    if {${tag} == ""} {
        set begidx [${w} index "${idx} -1c wordstart"]
        if {${grp} == "c++" && [${w} get "${begidx} -1c"] == "#"} {
            set begidx [${w} index "${begidx} -1c"]
        }
    } else {
        set nxt_rng [${w} tag nextrange ${tag} ${idx}]
        if {${nxt_rng} == "" || [lindex ${nxt_rng} 0] != ${idx}} {
            set prev_rng [${w} tag prevrange ${tag} ${idx}]
            set begidx [lindex ${prev_rng} 0]
        }
    }
    Sn_Syntax_Highlight -update ${grp} ${w} ${begidx} end
}

