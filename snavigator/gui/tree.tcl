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

# # tree.tcl - A tree widget class with tab stop support.
# Copyright (C) 1998 Cygnus Solutions.

#we need to do this to make sure that other applications as
#Source-Navigator can use this tree tool
if {! [info exists sn_options(def,default-font)]} {
    set sn_options(def,default-font) ansi
    if {$tcl_platform(platform) == "windows"} {
	set sn_options(def,select-fg) SystemHighlightText
	set sn_options(def,select-bg) SystemHighlight
	set sn_options(def,bold-font) global/bold
    } else {
	set sn_options(def,select-fg) black
	set sn_options(def,select-bg) yellow
	set sn_options(def,bold-font) "-*-Courier-Bold-R-Normal--*-120-*-*-*-*-iso8859-1"
    }
}

itcl_class Tree {
    inherit itk::Widget
    
    global sn_options

    constructor {config} {
	global sn_options
	
	#set up a short name for accessing real name path
	set thisTail [namespace tail $this]
	
	if {$withframe} {
	    set lframe $thisTail.fr
	    ::frame $lframe -bd 2 -relief sunken
	} else {
	    set lframe $thisTail
	}

	if {$font == ""} {
	    set font $sn_options(def,default-font)
	}
	if {$selectforeground == ""} {
	    set selectforeground $sn_options(def,select-fg)
	}
	if {$selectbackground == ""} {
	    set selectbackground $sn_options(def,select-bg)
	}

	set tree $lframe.tree

	scrollbar $lframe.x \
    		-orient horiz \
		-command "$tree xview"
	scrollbar $lframe.y \
    		-command "$tree yview"

	treetable $tree \
	    -yscrollcommand "$lframe.y set" \
	    -xscrollcommand "$lframe.x set" \
            -takefocus 1 \
	    -exportselection $exportselection \
	    -fillselection $fillselection \
	    -selectmode $selectmode \
	    -sortedinsertion $sortedinsertion\
	    -nocase $sortnocase \
	    -col $sortcolumn \
	    -bitmapspace $bitmapspace \
	    -bestfit $bestfit \
	    -autofit $autofit \
	    -truncate $truncate \
	    -splitlines $splitlines \
	    -tabfreespace $tabfreespace \
	    -accelerator $accelerator \
	    -truncatemethode $truncatemethode \
	    -font $font \
	    -lineforeground $lineforeground \
	    -splitlineforeground $splitlineforeground \
	    -selectforeground $selectforeground \
	    -selectbackground $selectbackground \
	    -selectborderwidth $selectborderwidth \
	    -highlightwidth $highlightwidth \
	    -highlightthickness $highlightthickness \
	    -indentwidth $indentwidth \
	    -borderwidth $borderwidth \
	    -width $width \
	    -height $height\
            -resizecommand "$this SyncTabs"
	#    -geometry ${width}x${height}
	
	if {$hiddenbitmap != ""} {
	    $tree config -hiddenbitmap $hiddenbitmap
	}
	if {$hiddenimage != ""} {
	    $tree config -hiddenimage $hiddenimage
	}
	if {$plusimage != ""} {
	    $tree config -plusimage $plusimage
	}
	if {$minusimage != ""} {
	    $tree config -minusimage $minusimage
	}
	if {$unknownimage != ""} {
	    $tree config -unknownimage $unknownimage
	}
	if {$tabs != ""} {
	    $tree config -tabs $tabs
	}
	if {$justify != ""} {
	    $tree config -justify $justify
	}

	#printing
	::bind $tree <Control-p> "$this print_dialog_box; break"
	::bind $tree <Control-c> "$this put_in_cutbuffer; break"
	::bind $tree <F16> [bind $tree <Control-c>]

	#filter
	if {$filter != ""} {
	    frame $lframe.filter
	    label $lframe.filter.label -text [get_indep String Pattern]
	    set entry $lframe.filter.entry
	    entry $entry \
		    -relief sunken \
		    -exportselection 0 \
		    -width 3 \
		    -textvariable $thisTail-filter
	    global $thisTail-filter
	    set $thisTail-filter $filter
	    $entry icursor 0

	    ::bind $entry <Return> "$this fill"
	
	    #make binding ctrl-u/l/r for filtering
	    ::bind $tree <Control-u> "
			 $this config -filter \"*\"
			 $this fill
			 focus %W
			 break
		    "
	    ::bind $tree <Control-l> [::bind $tree <Control-u>]
	    ::bind $tree <Control-r> [::bind $tree <Control-u>]
	
	    pack $lframe.filter.label -side left
	    pack $entry -side left -fill x -expand y

	    if {$filter_window} {
		grid $lframe.filter \
		        -row 3 -column 0 -columnspan 2 -sticky ew
	    }
	}

	if {$withframe} {
	    pack $lframe -side top -fill both -expand yes
	}
	
	#now create the labels for columns
	if {$tabsize > -1} {
	    create_tabs
	    view_tabs
	    grid $lframe.size -row 0 -column 0 -columnspan 2 -sticky ew
	    resize 0 [lindex [$tree cget -tabs] 0] 0
	}

        grid $tree -row 1 -column 0 -sticky news
        grid $lframe.y -row 1 -column 1 -sticky ns
        grid $lframe.x -row 2 -column 0 -sticky ew

        grid rowconfigure $lframe 1 -weight 1
        grid columnconfigure $lframe 0 -weight 1

	focus $tree
	
	if {$tabsize > -1} {
	    ::bind $lframe.x <B1-ButtonRelease> "$this replace_buttons"
	
	    ::bind $tree <3> "$this post_commands %X %Y; break"
	}	
	#add contents to the list
	contents
	
	#propagate
	if {! $propagate} {
#FIXME: what is the following code doing?
# I am currently seeing an error in this callback because the toplevel window does not exist.
#	    after idle "update idletasks; pack propagate $top $propagate"
set top [winfo toplevel $tree]
after idle "update idletasks ; if \[winfo exists $top\] \{pack propagate $top\}"
	}
    }

    destructor {
	foreach v [::info globals "$thisTail-*"] {
	    catch {uplevel #0 unset $v}
	}
    }

    method Xview {x1 x2 {x3 ""}} {
	if {$x3 != ""} {
	    $tree xview $x1 $x2 $x3
	} else {
	    $tree xview $x1 $x2
	}
	if {$tabsize > -1} {
	    resize 0 [lindex [$tree cget -tabs] 0] 0
	}
    }

    method tree {} {
	return $tree
    }
    method insert {args} {
	set idx [lindex $args 0]
	set cnt [lindex $args 1]
	set args [lrange $args 2 end]
	::eval $lframe.tree insert $idx list [list $cnt] $args
    }

    method remove {from {to ""}} {
	if {$to == ""} {
	    set to $from
	}
	$lframe.tree delete $from $to
    }

    method search args {
	return [::eval $lframe.tree search $args]
    }

    method sort_refresh {} {
	if { $actual_sortcolumn != -1 } {
	    resort $actual_sortcolumn
	}
    }

    method resort {num {var ""}} {
	global sn_options
	global $thisTail-sort
	
	$tree config -cursor watch
	set fnt [$lframe.size.btn$num cget -font]
	
	#restore old font for the last sort column
	if {$oldnum != $num} {
	    if {$oldnum != -1} {
		$lframe.size.btn$oldnum configure -font $oldfont
	    }
	    set oldnum $num
	    set oldfont $fnt
	}
	
	#change actual column font to bold
	set fnt [split $fnt "-"]
	if {[llength $fnt] > 3} {
	    set fnt [join [lreplace $fnt 3 3 "bold"] "-"]
	} else {
	    set fnt $sn_options(def,bold-font)
	}

	$lframe.size.btn$num config -cursor watch -font $fnt
	update idletasks
	
	#call sort command to resort entries
	$tree sort -nocase -col $num
	
	$lframe.size.btn$num config -cursor {}
	$tree config -cursor {}
	set oldnum $num
	set actual_sortcolumn $num
	
	set $thisTail-sort $num
    }

    #select an entry by it's index number
    method selectnum {num} {
	$tree selection clear 0 end
	$tree selection set $num
	$tree see $num
    }

    method selection {args} {
	return [::eval $tree selection $args]
    }

    method cget {args} {
	return [::eval $tree cget $args]
    }

    method config {config} {
    }

    method xview {args} {
	return [::eval $tree xview $args]
    }

    method yview {args} {
	return [::eval $tree yview $args]
    }
    #an array of column filters
    protected col_filter ""
    protected colfilters
    public filterextension ""
    method edit_column_filter {w num X Y} {
	global $thisTail-filterentry
	if {![info exists colfilters($num)]} {
	    set colfilters($num) "*"
	}
	set $thisTail-filterentry $colfilters($num)

	#if there is no label, no filter
	if {[$w cget -text] == ""} {
	    bell; return
	}

	set x [winfo rootx $w]
	set y [winfo rooty $w]
	set h [expr [winfo height $w] + 1]
	set width [winfo width $w]
	set color white
	set m $thisTail-filter_menu
	catch {destroy $m}
	menu $m -tearoff 0
	wm overrideredirect $m 1
	wm withdraw $m
	wm geometry $m ${width}x${h}+$x+$y
	pack [entry $m.l \
		-bg $color \
		-bd 0 \
		-relief raised \
		-textvariable $thisTail-filterentry] -fill both -expand y
	::bind $m.l <Return> "$this set_column_filter $w $num; tkMenuUnpost $m; break"
	::bind $m.l <Escape> "tkMenuUnpost $m; break"
	
	raise $m
	wm deiconify $m
	tk_popup $m $x $y
	focus $m.l
    }

    method set_column_filter {w num} {
	upvar #0 $thisTail-filterentry fltentry
	
	#store column filter
	set colfilters($num) $fltentry
	
	#display filter into label
	set txt [lindex $labels $num]
	if {$fltentry == "*" || $fltentry == ""} {
	    set colfilters($num) "*"
	    #label without filter
	    $w config -text $txt
	} else {
	    $w config -text "${txt}($fltentry)"
	}
	
	calculate_column_filter
	
	fill
	
	unset fltentry
    }

    method calculate_column_filter {} {
	if {$tabsize == -1} {
	    return ""
	}
	set tabs [$tree cget -tabs]
	
	#build tree filter
	set col_filter ""
	set cmp ""
	for {set i 0} {$i <= $tabsize} {incr i} {
	
	    #if label is hidden, disable it's filter
	    if {[lindex $tabs $i] <= 0} {
		set flt "*"
	    } else {
		set flt $colfilters($i)
	    }
	    if {$col_filter == ""} {
		set col_filter $flt
		set cmp "*"
	    } else {
		append col_filter "\t$flt"
		append cmp "\t*"
	    }
	}
	
	#no column filter is specified
	if {$cmp == $col_filter} {
	    set col_filter ""
	} elseif {$filterextension != ""} {
	    append col_filter $filterextension
	}

	return $col_filter
    }

    method delete_column_filters {} {
	for {set i 0} {$i <= $tabsize} {incr i} {
	    if {$colfilters($i) != "*"} {
		set colfilters($i) "*"
		change_label $i [lindex $labels $i]
	    }
	}
	
	set old_col_filter $col_filter
	
	calculate_column_filter
	
	if {$col_filter != $old_col_filter} {
	    fill
	}
    }
	
    method change_label {num txt} {
	if {$txt != "" && $colfilters($num) != "*"} {
	    set txt "${txt}($colfilters($num))"
	}
	$lframe.size.btn$num config -text $txt
    }

    #modify the tabs, eventualy delete or add new tabs
    method change_tabs {size tbs lbls} {
	set tabsize $size
	set tabs $tbs
	set labels $lbls
	
	$tree config -tabs $tabs
	if {[winfo exists $lframe.size] && $tabsize != -1} {
	    for {set i 0} {$i <= $tabsize} {incr i} {
		$lframe.size.btn$i configure -text [lindex $labels $i]
	    }
	    view_tabs
	    resize 0 [lindex [$tree cget -tabs] 0] 0
	}
    }

    method toggle_column {num {var ""} {value -1}} {
	if {$value == -1} {
	    $tree column toggle $num
	} else {
	    if {$value} {
		set cmd view
	    } else {
		set cmd hide
	    }
	    $tree column $cmd $num
	}
	
	if {$value != -1} {
	    set column_toggled($num) $value
	} else {
	    if {![info exists column_toggled($num)] || $column_toggled($num)} {
		set column_toggled($num) 0
	    } else {
		set column_toggled($num) 1
	    }
	}
	if {$var != ""} {
	    upvar #0 $var v
	    set v $column_toggled($num)
	}
    }

    method justify_column {num {var ""}} {
	set aligns [$tree cget -justify]
	if {$aligns == ""} {
	    for {set i 0} {$i <= $tabsize} {incr i} {
		lappend aligns 0
	    }
	}
	
	upvar #0 $var v
	set aligns [lreplace $aligns $num $num $v]
	$tree config -justify $aligns
    }

    method toggle_splitlines {} {
	if {$splitlines} {
	    set splitlines 0
	} else {
	    set splitlines 1
	}
	if {[winfo exists $tree]} {
	    $tree config -splitlines $splitlines
	}
    }
    method propagate {} {
	return $propagate
    }

    method size {} {
	if {[winfo exists $tree]} {
	    return [$tree size]
	}
	return 0
    }

    method curselection {} {
	return [$tree curselection]
    }

    method get {args} {
	return [::eval $tree get $args]
    }

    method itemcget {args} {
	return [::eval $tree itemcget $args]
    }

    method itemconfig {args} {
	return [::eval $tree itemconfig $args]
    }

    #we need this procedure to sort the contents by using
    #the flags in the class to insert the contents in the list
    method sortcontents {lst} {
	if {$sort != ""} {
	    if {$nocase} {
		if {$uniq} {
		    set lst [::lunique [::lsort $sort -command sn_compare $lst]]
		} else {
		    set lst [::lsort $sort -command sn_compare $lst]
		}
	    } elseif {$uniq} {
		set lst [::lunique [::lsort $sort $lst]]
	    } else {
		set lst [::lsort $sort $lst]
	    }
	}
	return $lst
    }

    method setcontents {cnt} {
	set contents $cnt
    }

    method getcontents {} {
	return $contents
    }

    method contents {} {
	if {![winfo exists $tree]} {
	    return
	}
	if {$sort != ""} {
	    if {$nocase} {
		if {$uniq} {
		    set contents [::lunique [::lsort $sort -command sn_compare $contents]]
		} else {
		    set contents [::lsort $sort -command sn_compare $contents]
		}
	    } elseif {$uniq} {
		set contents [lunique [::lsort $sort $contents]]
	    } else {
		set contents [::lsort $sort $contents]
	    }
	}

	fill
    }

    method setfilter {flt} {
	filter $contents $flt
    }

    method filter_state {state} {
	if {![winfo exists $entry]} {
	    return
	}
	if {$state == "disabled"} {
	    global $thisTail-filter
	    set $thisTail-filter "*"
	}
	$entry config -state $state
    }

    #filter and sort the contents of the tree, this command is usefull
    #to use by external 'fill' commands (See Retriever').
    proc filter {cnt {filter_str ""} {nocase 1}} {

	if {$filter_str == ""} {
	    set filter_str "*"
	}
	
	if {$filter_str == "*" || $cnt == ""} {
	    return $cnt
	}

	if {$nocase} {
	    set flt [nocase_glob_pattern $filter_str]
	} else {
	    set flt $filter_str
	}
	
	return [lmatch $cnt $flt]
    }

    method fill {{bsy 0}} {

	if {$fillcommand != ""} {
	    set res [::eval $fillcommand $this [list $contents]]
	    return $res
	}
	
	#set frst [$tree index @0,0]
	#if {$frst == ""} {
	#    set frst 0
	#}
	#set y [lindex [$tree yview] 0]
	
	set filter [getfilter]

	#delete old entries
	$tree delete 0 end

	if {$contents == ""} {
	    return
	}

	if {$filter != "*" && $filter != ""} {
	    if {$nocase} {
		set flt [nocase_glob_pattern $filter]
	    } else {
		set flt $filter
	    }
	} else {
		set flt "*"
	}
	
	if {$flt != "*"} {
	    $tree insert end list [lmatch $contents $flt]
	} else {
	    $tree insert end list $contents
	}
	
	$tree see -top 0
	#$tree yview moveto $y
    }

    method getfilter {} {
	upvar #0 $thisTail-filter flt
	
	#calculate filter always
	calculate_column_filter
	
	if {$col_filter != ""} {
	    return $col_filter
	}
	if {[winfo exists $entry]} {
	    set filter $flt
	} else {
	    set filter "*"
	}
	if {$filter == ""} {
	    set filter "*"
	
	    set $thisTail-filter "*"
	    $entry icursor 0
	}

	return $filter
    }

    #get selected entries or selected positions
    method marked {{str 1}} {
	set sel [$this curselection]
	if {$str} {
	    set val ""
	    foreach s $sel {
		lappend val [$lframe.tree get $s]
	    }
	    return $val
	} else {
	    return $sel
	}
    }

    #convert filter to usable filter for string matching
    proc nocase_glob_pattern {flt} {
	if {[string compare $flt "*"] == 0} {
	    return $flt
	}
        for {set c 0; set brace_lev 0; set m ""; set glb_pat "";\
	    set flt [string tolower $flt]} \
	    {$c < [string length $flt]} {incr c} {

	    set ch [string index $flt $c]
	    switch -glob -- $ch {
	    {[A-Za-z]} {
		if {$brace_lev <= 0} {
		    append m \[ [string tolower $ch] \
			[string toupper $ch] \]
		} else {
		    append glb_pat $ch
		}
	        }
	    {\[}    {
		append m \[
		set glb_pat ""
		incr brace_lev
	        }
	    {\]}    {
		for {set k 0} {$k < [string length $glb_pat]} \
		    {incr k} {
		    set cc [string index $glb_pat $k]
		    set nc [string index $glb_pat [expr $k + 1]]
		    append m [string tolower $cc]
		    if {[string compare $nc "-"] == 0} {
		    incr k 2
		    set nc [string index $glb_pat $k]
		    append m "-" [string tolower $nc] \
		        [string toupper $cc] "-" [string toupper $nc]
		    } else {
		    append m [string toupper $cc]
		    }
		}
		set glb_pat ""
		append m \]
		incr brace_lev -1
	    }
	    {\\}    {
		incr c
		append m "\\" [string index $flt $c]
	        }
	    default {
		if {$brace_lev <= 0} {
		    append m $ch
		} else {
		    append glb_pat $ch
		}
	        }
	    }
	}
	return $m
    }

    method SyncTabs {} {
        resize 0 [lindex [$tree cget -tabs] 0] 0
    }

    public splitwidth 1

    #this brings the split lines from the tree and the lines
    #from this class to be displayed in one line
    protected correction_factor 3

    #function is called to synchronize the widget labels with
    #the tabulators defined in the treetable.
    #it doesn't display the labels for hidden columns/tabs
    method resize {num x {realy 1}} {
	if {![winfo exists $lframe.size] || $x == ""} {
	    return
	}
	
	#end motion process
	end_motion
	
	set twidth  [winfo width $tree]
	if {$twidth <= 1} {
	    set twidth [winfo reqwidth $tree]
	}
	set frheight [winfo height $lframe.size]
	if {$frheight <= 1} {
	    set frheight [winfo reqheight $lframe.size]
	}
	if {$frheight <= 1} {
	    set frheight [winfo reqheight $lframe.size.btn0]
	}
	set xoffset [expr [$tree xoffset] - $correction_factor]
	set oldtabs $tabs
	set tabs [$tree cget -tabs]
	if {$realy} {
	    set x [expr $x - [winfo rootx $lframe.size] + $xoffset]
	}
	
	set ox 0
	set tx 0
	for {set i 0} {$i <= $tabsize} {incr i} {
	    if {$i < $tabsize} {
		if {$x >= 0 && $num == $i} {
		    set mx $ox
		    if {$x < $mx} {
			set x $mx
		    }
		    set tab  [lindex $tabs $i]
		    set diff [expr $x - [expr $tx + $tab]]
		    set tab  [expr $tab + $diff]
		    set tabs [lreplace $tabs $i $i $tab]
		    set tx $x
		} else {
		    set tx [expr $tx + [lindex $tabs $i]]
		}
		set width [lindex $tabs $i]
		if {$width == 0} {
		    #column hidden
		    place forget $lframe.size.col$i
		} else {
		    place $lframe.size.col$i \
			-y 1 \
			-x [expr $tx - $xoffset]
		}
	    } else {
		#take the rest of the window size
		set tx [expr $tx + $width]
		set width [expr $twidth - $ox + $xoffset + 20]
	    }
	    if {$width == 0} {
		#column hidden
		place forget $lframe.size.btn$i
	    } else {
		if {$i == 0} {
		    place $lframe.size.btn$i \
			-y 1 \
			-x [expr $ox - $xoffset - $correction_factor] \
			-width [expr $width + $correction_factor] \
			-height $frheight
		} else {
		    place $lframe.size.btn$i \
			-y 1 \
			-x [expr $ox - $xoffset + 1] \
			-width [expr $width - 1] \
			-height $frheight
		}
	    }
	    set ox $tx
	}

	if {$oldtabs != $tabs} {
	    $tree config -tabs $tabs
	}

	#set up height correctly for size frame and columns
	set hh [winfo reqheight $lframe.size.btn0]
	if {$frame_height != $hh} {
	    $lframe.size config -height [expr $hh + 2]
	    set frame_height $hh
	}
	if {$col_height != $hh} {
	    for {set i 0} {$i < $tabsize} {incr i} {
		$lframe.size.col$i config -height $hh
		set col_height $hh
	    }
	}
    }

    method create_tabs {} {
	global $thisTail-sort
	
	#create widgets
	::frame $lframe.size -bd 0 -relief raised -bg black
	
	#-height 30
	for {set i 0} {$i <= $tabsize} {incr i} {
	    if {$labels == ""} {
		set lbl col$i
	    } else {
		set lbl [lindex $labels $i]
	    }
	    #label and button for columns
	    if {$justify != "" && [lindex $justify $i] == "1"} {
		set anchor e
	    } else {
		set anchor w
	    }
	
	    #label filter
	    set colfilters($i) "*"
	
	    ::button $lframe.size.btn$i \
		-bd 1 \
		-text $lbl \
		-anchor $anchor \
		-relief raised \
		-command "$this resort $i"
	    ::bind $lframe.size.btn$i <B3-ButtonRelease> \
		"$this edit_column_filter %W $i %X %Y"
	
	    #if bestfit or autofit is enabled, store the button widths
	    #as default tab size for the tab stops.
	    if {$bestfit || $autofit} {
		set bwidth  [winfo width $lframe.size.btn$i]
		if {$bwidth <= 1} {
		    set bwidth [winfo reqwidth $lframe.size.btn$i]
		}
		set tabs [lreplace $tabs $i $i $bwidth]
	    }
	    balloon_bind_info $lframe.size.btn$i [get_indep String TreeButton]
	    
	    ::bind $lframe.size.btn$i <Motion> "$this button_motion %W $i %x"
	    ::bind $lframe.size.btn$i <1> "
			set $thisTail-motion \[$this button_motion %W $i %x\]
			if {\${$thisTail-motion} <= 0} {
			    $this start_motion \[expr {$i + \${$thisTail-motion}}\] %X
			    break
			} else {
			    catch {unset $thisTail-motion}
			}
			"
	    ::bind $lframe.size.btn$i <B1-Motion> "
			if {\[info exists $thisTail-motion\]} {
			    $this motion \[expr {$i + \${$thisTail-motion}}\] %X
			    break
			}
			"
	    ::bind $lframe.size.btn$i <B1-ButtonRelease> "
			if {\[info exists $thisTail-motion\]} {
			    $this resize \[expr {$i + \${$thisTail-motion}}\] %X
			    catch {unset $thisTail-motion}
			    break
			}
			"

	    if {$i < $tabsize} {
		::frame $lframe.size.col$i \
		    -relief raised \
		    -width $splitwidth \
		    -height 7 \
		    -cursor    sb_h_double_arrow
		balloon_bind_info $lframe.size.col$i [get_indep String TreeColumn]
		
		#start motion process
		::bind $lframe.size.col$i <1> "$this start_motion $i %X"
		
		#motion process
		::bind $lframe.size.col$i <B1-Motion> "$this motion $i %X"
		
		#end motion process and place the columns correctly
		::bind $lframe.size.col$i <B1-ButtonRelease>\
			"$this resize $i %X"
		
		#enable/disable split lines displaying
		::bind $lframe.size.col$i <3> "$this toggle_splitlines"
	    }
	}
	
	if {$bestfit || $autofit} {
	    $lframe.tree config -deftabs $tabs
	}
    }

    method view_tabs {} {
	set tabs [$tree cget -tabs]
	set cnt [llength $tabs]
	set last [lindex $tabs end]
	set modified 0
	
	if {$cnt > 1} {
	    set size [lindex $tabs end]
	} else {
	    set size [lindex $tabs 0]
	}
	if {$last == ""} {
	    set last [expr [font_avg_width $tree] * 8]
	}
	
	#expand tab list to count of columns
	for {set i $cnt} {$i < $tabsize} {incr i} {
	    lappend tabs $last
	    set modified 1
	}
	for {set i 0; set x 0} {$i < $tabsize} {incr i} {
	    place config $lframe.size.col$i -x $x
	    set x [expr $x + [lindex $tabs $i]]
	}
	if {$modified} {
	    $tree config -tabs $tabs
	}
	for {set $i $tabsize} {$i < 10} {incr i} {
	    if {[winfo exists $lframe.size.col$i]} {
		catch {place forget $lframe.size.col$i}
		catch {place forget $lframe.size.btn[expr $i + 1]}
	    } else {
		#no need to continue
		break
	    }
	}
    }

    method toggle_bestfit {} {
	if {$bestfit} {
	    set bestfit 0
	} else {
	    set bestfit 1
	}
	if {[winfo exists $tree]} {
	    $tree config -bestfit $bestfit
	}
    }

    method toggle_truncate {} {
	if {$truncate} {
	    set truncate 0
	} else {
	    set truncate 1
	}
	if {[winfo exists $tree]} {
	    $tree config -truncate $truncate
	}
    }

    method Truncating_Methode {methode} {
	set truncatemethode $methode
	$tree config -truncatemethode $truncatemethode
    }

    #called to post the right-mose-option menu.
    #it's realy intelegent!!
    method tree_post_menu {m} {
	upvar #0 $tree-bestfit bf
	upvar #0 $tree-truncate tr
	upvar #0 $tree-splitlines sp
	
	set bf $bestfit
	set tr $truncate
	set sp $splitlines
	
	if {$when_post_menu != ""} {
	    eval $when_post_menu $m
	}
		
	#menu delete all column filters
	$m add command \
	    -label [get_indep String DeleteColumnFilters] \
	    -command "$this delete_column_filters"
	
	#if tabulators are enabled, view those as menu buttons
	#to sort on them or to view/hide them.
	if {$tabsize >= 0} {
	    global $thisTail-sort
	
	    $m add separator
	
	    #Add sort submenu to sort the contents on the
	    #availiable columns
	    set mk $m.sort
	    $m add cascade -label [get_indep String Sort]\
		-underline [get_indep Pos Sort]\
		-menu $mk
	    menu $mk -tearoff 0
	    for {set i 0} {$i <= $tabsize} {incr i} {
		$mk add radiobutton \
		    -label [lindex $labels $i] \
		    -variable $thisTail-sort \
		    -value $i \
		    -command "$this resort $i"
	    }
	    set mk $m.hide
	    $m add cascade -label [get_indep String Toggle]\
		-underline [get_indep Pos Toggle]\
		-menu $mk
	
	    #Add the submenu to view/hide the availiable columns
	    set mk $m.hide
	    menu $mk -tearoff 0
	    for {set i 0} {$i <= $tabsize} {incr i} {
		upvar #0 $thisTail-hide-$i x
		if {![info exists column_toggled($i)] || $column_toggled($i)} {
		    set x 1
		} else {
		    set x 0
		}
		$mk add checkbutton \
		    -label [lindex $labels $i] \
		    -variable $thisTail-hide-$i \
		    -onvalue 1 -offvalue 0 \
		    -command "$this toggle_column $i $thisTail-hide-$i"
	    }
	
	    #Add the sub menu to specify the alignments (left/right)
	    #for the columns
	    #Only if there is more than two columns
	    if {$tabsize >= 2} {
		set state normal
	    } else {
		set state disabled
	    }
	    set mk $m.align
	    $m add cascade -label [get_indep String Justify]\
		-underline [get_indep Pos Justify] \
		-menu $mk \
		-state $state
	    menu $mk -tearoff 0
	    set aligns [$tree cget -justify]
	    for {set i 0} {$i <= $tabsize} {incr i} {
		upvar #0 $thisTail-align-$i x
		set right [lindex $aligns $i]
		if {$right != "" && $right == 1} {
		    set x 1
		} else {
		    set x 0
		}
		
		#don't justify the first and the last column !!
		if {$i == 0 || $i == $tabsize} {
		    set state disabled
		} else {
		    set state normal
		}
		$mk add checkbutton \
		    -label [lindex $labels $i] \
		    -variable $thisTail-align-$i \
		    -onvalue 1 -offvalue 0 \
		    -command "$this justify_column $i $thisTail-align-$i" \
		    -state $state
	    }
	
	    #sub menu to choose the truncating methode (auto, prefix, suffix)
	    if {$enable_truncating_methode_submenu} {
		if {$tr} {
		    set state normal
		} else {
		    set state disabled
		}
		set mk $m.tm
		$m add cascade \
		    -label [get_indep String TruncatingMethode]\
		    -underline [get_indep Pos TruncatingMethode] \
		    -menu $mk \
		    -state $state
		menu $mk -tearoff 0
		
		$mk add radiobutton \
			-label "Auto" \
			-variable $thisTail-truncating_methode \
			-value "auto" \
			-command "$this Truncating_Methode auto"
		$mk add radiobutton \
			-label "Prefix" \
			-variable $thisTail-truncating_methode \
			-value "path" \
			-command "$this Truncating_Methode path"
		$mk add radiobutton \
			-label "Suffix" \
			-variable $thisTail-truncating_methode \
			-value "normal" \
			-command "$this Truncating_Methode normal"
		}
	}
	
	$m add separator
	
	#best fit
	$m add checkbutton \
	    -label [get_indep String TreeBestFit] \
	    -variable $tree-bestfit \
	    -onvalue 1 -offvalue 0 \
	    -command "$this toggle_bestfit"
	#truncate
	$m add checkbutton \
	    -label [get_indep String TreeTruncate] \
	    -variable $tree-truncate \
	    -onvalue 1 -offvalue 0 \
	    -command "$this toggle_truncate"
	#split lines
	$m add checkbutton \
	    -label [get_indep String TreeViewSplitLines] \
	    -variable $tree-splitlines \
	    -onvalue 1 -offvalue 0 \
	    -command "$this toggle_splitlines"
	
	$m add separator

	#view size and selected items at the top of the popup menu
	set sel_size [llength [$tree curselection]]
	$m add command \
	    -label [format [get_indep String  ListSize]\
	    [$tree size] $sel_size]
    }
    method post_commands {x y} {
	set m .sn_pop_menu_tree
	# It has to be destroyed because we might have problems with "tk_popup"!
	catch {::destroy $m}

	menu $m -tearoff 0\
		-postcommand "$this tree_post_menu $m"
	wm overrideredirect $m 1

	tk_popup $m $x $y
    }

    proc font_avg_width {w} {
	set fnt [$w cget -font]
	set text_avg_width [font measure $fnt "M"]

	return $text_avg_width
    }

    method replace_buttons {} {
	resize 0 [lindex [$tree cget -tabs] 0] 0
    }

    method start_motion {num x} {
	set x [expr {$x - [winfo rootx $lframe.size]}]
	if {$mframe == ""} {
	    set mframe $lframe.motion
	}
	set theight [expr {[winfo height $tree] + [winfo height $lframe.size]}]
	frame $mframe -relief raised \
		    -bd 0 \
		    -width 1 \
		    -bg black \
		    -height $theight \
		    -cursor sb_h_double_arrow
	place $mframe -x $x -y 0
    }
    method motion {num x} {
	if {$mframe == ""} {
	    start_motion $num $x
	}
	set x [expr {$x - [winfo rootx $lframe.size]}]
	place $lframe.motion -x $x -y 0
    }
    method end_motion {} {
	if {$mframe != ""} {
	    catch {destroy $mframe}
	    set mframe ""
	}
    }

    #do resizing the column width even when motion arround
    #the line, this happens when the pointer moves over
    #the near button where the resize-hit-button area is
    #set to be 12 or dependent on the current column size.
    method button_motion {btn num x} {
    	set ww [winfo width $btn]
	set range 12
	if {$range > [expr {$ww / 3}]} {
	    set range [expr {$ww / 3}]
	}
	if {$num > 0 && $x <= $range} {
	    $btn config -cursor sb_h_double_arrow
	    return -1
	} elseif {$num < $tabsize && [expr {$ww - $x}] < $range} {
	    $btn config -cursor sb_h_double_arrow
	    return 0
	} else {
	    $btn config -cursor {}
	    return 1
	}
    }

    method put_in_cutbuffer {} {
	set sel [$tree curselection]
	foreach l $sel {
	    lappend res [$tree get $l]
	}
	if {![info exists res]} {
	    bell
	    return
	}
	#clipboard clear -displayof $tree
	#clipboard append -displayof $tree -- $res
	clipboard clear
	clipboard append [join $res \n]
    }

    method print_dialog_box {{sub_tit ""}} {
	global sn_options
	global tcl_platform

	if {[winfo exists $print_dialog]} {
	    $print_dialog raise
	    return
	}
	set toplw [winfo toplevel $thisTail]

	set print_dialog [sourcenav::Window $toplw.prtdlg]

	$print_dialog transient $toplw

	sn_motif_buttons $print_dialog bottom 0 [get_indep String ok]\
		[get_indep String cancel]
	$print_dialog.button_0 config -command "$this print_it"
	$print_dialog.button_1 config -command "itcl::delete object $print_dialog"

	global $print_dialog-ptarget $print_dialog-cmd
	set $print_dialog-ptarget "all"
	
	set tit [wm title $toplw]
	if {$sub_tit != ""} {
	    append tit " " $sub_tit
	}
	set $print_dialog-cmd [format $sn_options(def,ascii-print-command) $tit]
	$print_dialog title [list [get_indep String Print] $tit]

	frame $print_dialog.txt
	frame $print_dialog.txt.lbls
	frame $print_dialog.txt.entries
	
	if {$tcl_platform(platform) != "windows"} {
	    label $print_dialog.txt.lbls.prompt\
		-text [get_indep String SQLPprintercmd]

	    entry $print_dialog.txt.entries.cmd -width 70\
		-textvariable $print_dialog-cmd
		
	    ::focus $print_dialog.txt.entries.cmd
	    ::bind $print_dialog.txt.entries.cmd <Any-Return> \
		"$print_dialog.button_0 invoke"
	}
	label $print_dialog.txt.lbls.label \
		-text [get_indep String Indent]
	entry $print_dialog.txt.entries.entry -width 5\
		-textvariable $print_dialog-indent

	# Clear the entry field before inserting a value.
	$print_dialog.txt.entries.entry delete 0 end
	$print_dialog.txt.entries.entry insert 0 "2"
	
	if {$tcl_platform(platform) != "windows"} {
	    pack $print_dialog.txt.lbls.prompt -side top -anchor e
	    pack $print_dialog.txt.entries.cmd -side top -padx 10 -fill x -expand y
	}
	pack $print_dialog.txt.lbls.label -side top -anchor e
	pack $print_dialog.txt.entries.entry -side top -padx 10 -anchor w
	pack $print_dialog.txt -side top -anchor w
	
	pack $print_dialog.txt.lbls -side left -expand y
	pack $print_dialog.txt.entries -side right -fill x -expand y
	pack $print_dialog.txt -side top -expand y
	
	radiobutton $print_dialog.marked -text [get_indep String Marked]\
	    -variable $print_dialog-ptarget -value marked

	radiobutton $print_dialog.all -text [get_indep String All]\
	    -variable $print_dialog-ptarget -value all

	pack $print_dialog.all -anchor w -padx 60 -side top
	pack $print_dialog.marked -anchor w -padx 60 -side top

	$print_dialog move_to_mouse
	catch {$print_dialog resizable no no}
	
	$print_dialog take_focus
    }

    method print_it {} {
	upvar #0 $print_dialog-ptarget target
	upvar #0 $print_dialog-cmd cmd
	upvar #0 $print_dialog-indent indent
	global sn_options

	if {[catch {set indent [expr $indent + 0]}]} {
	    set indent 2
	}
	
	set spaces "                                                          "
	
	switch -- $target {
	"all" {
		set i 0
		for {set size [$tree size]} {$i < $size} {incr i} {
		    set lvl [expr [$tree levels $i] * $indent]
		    set spaces ""
		    for {set j 0} {$j <= $lvl} {incr j} {
			set spaces " $spaces"
		    }
		    lappend lst "[string range $spaces 0 [expr $lvl - 1]][$tree get $i]"
		}
	    }
	"marked" -
	default {
		set sel [$lframe.tree curselection]
		if {$sel != ""} {
		    foreach i $sel {
			lappend lst "[string range $spaces 0 [expr [$tree levels $i] * $indent - 1]][$tree get $i]"
		    }
		}
	    }
	}
	
	if {![info exists lst]} {
	    set lst ""
	}
	set lst [join $lst \n]

	itcl::delete object $print_dialog

	set tmpf [sn_tmpFileName]
	set fd [open $tmpf "w+"]
	fconfigure $fd -encoding $sn_options(def,system-encoding) -blocking 0
	puts $fd $lst
	close $fd

	# Escape [] and $ in user input for eval in sn_print_file
	regsub -all {(\$|\[|\])} $cmd {\\&} cmd

	sn_print_file $cmd $tmpf

	file delete -- $tmpf

	set print_dialog ""

	catch {unset target}
	catch {unset cmd}
    }

    ##
    ##List_Box compatiblility
    ##
    # It is a 'proc' because the performance is better.
    method see {args} {
	return [eval $tree see $args]
    }
    method delete_tk {args} {
	return [eval $tree delete $args]
    }

    method treebind {args} {
	return [eval ::bind $tree $args]
    }

    method header {args} {
	if {[winfo exists $header]} {
	    return [eval $header $args]
	}
    }
    method index {args} {
	return [eval $tree index $args]
    }
    method activate {args} {
	return [eval $tree activate $args]
    }

    method nearest {args} {
	return [eval $tree nearest $args]
    }

    proc exchange {w y} {
	set sel [$w curselection]
	if {$sel == "" || [string compare $exchange ""] == 0} {
	    return
	}

	set y [$w nearest $y]

	set len [expr {[llength $sel] - 1}]
	set first [lindex $sel 0]
	set last  [lindex $sel $len]

	if {$y >= $first && $y <= $last} {
	    return
	}

	if {$first <= $y} {
	    set y [expr {$y - $len}]
	}
	if {$y < 0} {
	    return
	}
	$w delete $first $last
	eval $w insert $y $exchange
	$w selection clear 0 end
	$w selection set $y [expr {$y + $len}]
    }
    proc exchange_mark {w} {
	set sel [$w curselection]

	set exchange ""
	foreach s [$w curselection] {
	    lappend exchange [$w get $s]
	}
    }

    protected thisTail ""
    protected lframe ""
    protected tree ""
    protected oldnum -1
    protected oldfont ""
    protected actual_sortcolumn -1
    #an array of the items
    protected column_toggled
    protected entry ""
    protected widget {}
    protected frame_height 0
    protected col_height 0
    protected mframe ""
    protected print_dialog {}

    common resizing
    common exchange {}

    public withframe no

    public state {disabled} {
	#$tree config -state $state
    }
    public tabsize -1
    public tabs {} {
	if {[winfo exists $tree] && $tabsize != -1} {
	    $tree config -tabs $tabs
	}
    }
    public labels {""} {
	if {[winfo exists $lframe.size] && $tabsize != -1} {
	    for {set i 0} {$i <= $tabsize} {incr i} {
		$lframe.size.btn$i configure -text [lindex $labels $i]
	    }
	    resize 0 [lindex [$tree cget -tabs] 0] 0
	    view_tabs
	}
    }
    public justify {} {
	if {[winfo exists $lframe.btn0] && $tabsize != -1} {
	    resize 0 [lindex [$tree cget -tabs] 0] 0
	}
    }
    public selectmode "browse" {
	if {[winfo exists $tree]} {
	    $tree config -selectmode $selectmode
	}
    }
    public exportselection {0} {
	if {[winfo exists $tree]} {
	    $tree config -exportselection $exportselection
	}
    }
    public selectforeground "" {
	if {$selectforeground != "" && [winfo exists $tree]} {
	    $tree config -selectforeground $selectforeground
	}
    }
    public selectbackground "" {
	if {$selectbackground != "" && [winfo exists $tree]} {
	    $tree config -selectbackground $selectbackground
	}
    }
    public fillselection {1} {
	if {[winfo exists $tree]} {
	    $tree config -fillselection $fillselection
	}
    }
    public sortedinsertion {0} {
	if {[winfo exists $tree]} {
	    $tree config -sortedinsertion $sortedinsertion
	}
    }
    public nocase 1 {
	if {[winfo exists $tree]} {
	    $tree config -sortnocase $sortnocase
	}
    }
    public sortnocase {0} {
	if {[winfo exists $tree]} {
	    $tree config -sortnocase $sortnocase
	}
    }
    public sortcolumn {0} {
	if {[winfo exists $tree]} {
	    $tree config -sortcolumn $sortcolumn
	}
    }
    public info_label {""} {
    }

    public truncate 1 {
	if {[winfo exists $tree]} {
	    eval $tree config -truncate $truncate
	}
    }

    public splitlines 1 {
	if {[winfo exists $tree]} {
	    eval $tree config -splitlines $splitlines
	}
    }
    public tabfreespace 8 {
	if {[winfo exists $tree]} {
	    eval $tree config -tabfreespace $tabfreespace
	}
    }

    public hiddencolumns "" {
    }

    #darkgray = "#aaaaaa"
    public lineforeground "#aaaaaa" {
	if {$lineforeground != "" && [winfo exists $tree]} {
	    $tree config -lineforeground $lineforeground
	}
    }

    public splitlineforeground gray {
	if {[winfo exists $tree]} {
	    $tree config -splitlineforeground $splitlineforeground
	}
    }
	
    public autofit 0 {
	if {[winfo exists $tree]} {
	    $tree config -autofit $autofit
	}
    }

    public bestfit 0 {
	if {[winfo exists $tree]} {
	    $tree config -bestfit $bestfit
	}
    }

    public indentwidth 15 {
	if {$indentwidth != -1 && [winfo exists $tree]} {
	    $tree config -indentwidth $indentwidth
	}
    }
    public borderwidth 1 {
	if {$borderwidth != -1 && [winfo exists $tree]} {
	    $tree config -borderwidth $borderwidth
	}
    }
    public highlightwidth 0 {
	if {$highlightwidth > -1 && [winfo exists $tree]} {
	    $tree config -highlightthickness $highlightwidth
	}
    }
    public highlightthickness 1 {
	if {$highlightthickness > -1 && [winfo exists $tree]} {
	    $tree config -highlightthickness $highlightthickness
	}
    }
    public relief "" {
	if {$relief != "" && [winfo exists $tree]} {
	    $tree config -relief $relief
	}
    }
    public takefocus "" {
	if {$takefocus != "" && [winfo exists $tree]} {
	    $tree config -takefocus $takefocus
	}
    }
    public bitmapspace 7 {
	if {$bitmapspace != -1 && [winfo exists $tree]} {
	    $tree config -bitmapspace $bitmapspace
	}
    }

    public bitmap "" {
	if {$bitmap != "" && [winfo exists $tree]} {
	    set size [$tree size]
	    for {set i 0} {$i < $size} {incr i} {
		$tree itemconfig $i -bitmap $bitmap
	    }
	}
    }

    public selectborderwidth 1 {
	if {$selectborderwidth != "" && [winfo exists $tree]} {
	    $tree config -selectborderwidth $selectborderwidth
	}
    }

    public hiddenbitmap "" {
	if {$hiddenbitmap != "" && [winfo exists $tree]} {
	    $tree config -hiddenbitmap $hiddenbitmap
	}
    }

    public hiddenimage "" {
	if {$hiddenimage != "" && [winfo exists $tree]} {
	    $tree config -hiddenimage $hiddenimage
	}
    }

    public plusimage "" {
	if {$plusimage != "" && [winfo exists $tree]} {
	    $tree config -plusimage $plusimage
	}
    }

    public minusimage "" {
	if {$minusimage != "" && [winfo exists $tree]} {
	    $tree config -minusimage $minusimage
	}
    }

    public unknownimage "" {
	if {$unknownimage != "" && [winfo exists $tree]} {
	    $tree config -unknownimage $unknownimage
	}
    }

    public width 45 {
	if {$width == ""} {
	    set width 45
	}
	if {[winfo exists $tree]} {
	    $tree config -width $width
	}
    }

    public height 12 {
	if {$height == ""} {
	    set height 12
	}
	if {[winfo exists $tree]} {
	    $tree config -height $height
	}
    }

    public font "" {
	if {$font != "" && [winfo exists $tree]} {
	    $tree config -font $font
	}
    }

    public propagate 0 {
	if {[winfo exists $tree]} {
	    grid propagate $tree $propagate
	}
    }
    public accelerator 0 {
	if {[winfo exists $tree]} {
	    $tree config -accelerator $accelerator
	}
    }

    #it's not very important to enable choosing how to truncate
    #columns (auto, prefix, suffix). "auto" seems to do the job
    public enable_truncating_methode_submenu 0
    public truncatemethode auto {
	if {[winfo exists $tree]} {
	    $tree config -truncatemethode $truncatemethode
	}
    }
    public contents {} {
	contents
    }

    public fillcommand "" {
    }

    public sort "-increasing"
    public advised {}
    public filter_window 1

    public uniq {0} {
    }

    public filter {*} {
	if {[winfo exists $entry]} {
	    global $thisTail-filter
	    set $thisTail-filter $filter
	    $entry icursor 0
	    if {$contents != ""} {
		$this fill
	    }
	}
    }

    public have_filter 1 {
	if {!$have_filter} {
	    set filter ""
	}
    }

    public bind_config 1
    public editable 0
    public header {}

    #can be set external to be executed when option menu
    #is launched.
    public when_post_menu ""
}
#############################################################################
##  END CLASS for TreeWidget with tab stop support                         ##
#############################################################################

##########################################################################
## bindings for the treetable widget                                ##
##########################################################################

#default key bindings
proc treetable_bindings {t} {
    #the keybindings to the treetable are compatible to
    #those of listbox.
    bind $t <1> {
	if [winfo exists %W] {
	    focus %W
	    tkListboxBeginSelect %W [%W index @%x,%y]
	}
    }

    bind TreeTable <Double-1> {
    }

    bind $t <B1-Motion> {
	set tkPriv(x) %x
	set tkPriv(y) %y
	catch {tkListboxMotion %W [%W index @%x,%y]}
    }
    bind $t <ButtonRelease-1> {
	tkCancelRepeat
	%W activate @%x,%y
    }
    bind $t <Shift-1> {
	tkListboxBeginExtend %W [%W index @%x,%y]
    }
    bind $t <Control-1> {
	tkListboxBeginToggle %W [%W index @%x,%y]
    }
    bind $t <B1-Leave> {
	set tkPriv(x) %x
	set tkPriv(y) %y
	tkListboxAutoScan %W
    }
    bind $t <B1-Enter> {
	tkCancelRepeat
    }

    bind $t <Up> {
	tkTreeTableUpDown %W -1
    }
    bind $t <Shift-Up> {
	tkListboxExtendUpDown %W -1
    }
    bind $t <Down> {
	tkTreeTableUpDown %W 1
    }
    bind $t <Shift-Down> {
	tkListboxExtendUpDown %W 1
    }
    bind $t <Left> {
	%W xview scroll -1 units
    }
    bind $t <Control-Left> {
	%W xview scroll -1 pages
    }
    bind $t <Right> {
	%W xview scroll 1 units
    }
    bind $t <Control-Right> {
	%W xview scroll 1 pages
    }
    bind $t <Prior> {
	%W yview scroll -1 pages
	%W activate @0,0
    }
    bind $t <Next> {
	%W yview scroll 1 pages
	%W activate @0,0
    }
    bind $t <Control-Prior> {
	%W xview scroll -1 pages
    }
    bind $t <Control-Next> {
	%W xview scroll 1 pages
    }
    bind $t <Home> {
	%W xview moveto 0
    }
    bind $t <End> {
	%W xview moveto 1
    }
    bind $t <Control-Home> {
	%W activate 0
	%W see 0
	%W selection clear 0 end
	%W selection set 0
    }
    bind $t <Shift-Control-Home> {
	tkListboxDataExtend %W 0
    }
    bind $t <Control-End> {
	%W activate end
	%W see end
	%W selection clear 0 end
	%W selection set end
    }
    bind $t <Shift-Control-End> {
	tkListboxDataExtend %W end
    }
    bind $t <F16> {
	if {[selection own -displayof %W] == "%W"} {
	    clipboard clear -displayof %W
	    clipboard append -displayof %W [selection get -displayof %W]
    	}
    }
    bind $t <space> {
	tkListboxBeginSelect %W [%W index active]
    }
    bind $t <Select> {
	tkListboxBeginSelect %W [%W index active]
    }
    bind $t <Control-Shift-space> {
	tkListboxBeginExtend %W [%W index active]
    }
    bind $t <Shift-Select> {
	tkListboxBeginExtend %W [%W index active]
    }
    bind $t <Escape> {
        if {$tkPriv(listboxPrev) != ""} {
	    tkListboxCancel %W
        }
    }
    bind $t <Control-slash> {
	tkListboxSelectAll %W
    }
    bind $t <Control-backslash> {
	if {[%W cget -selectmode] != "browse"} {
	    %W selection clear 0 end
	}
    }

    bind $t <2> {
	%W scan mark %x %y
    }
    bind $t <B2-Motion> {
	%W scan dragto %x %y
    }

    #other bindings added to default listbox bindings
    bind $t <Key> {treetable_search_region %W %A %s}
    bind $t <3> {sn_listbox_post_menu %W %X %Y}
    bind $t <Control-3> {sn_listbox_post_menu %W %X %Y}
    # Sun Home
    bind $t <Any-F27> 	[bind $t <Home>]
    # Sun End
    bind $t <Any-R13> 	[bind $t <End>]
    # Sun Next
    bind $t <Any-R15> 	[bind $t <Next>]
    # Sun Prior
    bind $t <Any-R9> 	[bind $t <Prior>]

    bind $t <Tab> {focus [tk_focusNext %W]}
    bind $t <Shift-Tab> {focus [tk_focusPrev %W]}
}

proc tkTreeTableUpDown {w amount} {
    global tkPriv
    $w activate [expr [$w index active] + $amount] $amount
    $w see active
    switch [$w cget -selectmode] {
    browse {
	    $w selection clear 0 end
	}
    extended {
	    $w selection clear 0 end
	    if {[string compare [$w index active] ""] != 0} {
		$w selection anchor active
		set tkPriv(listboxPrev) [$w index active]
	    }
	    set tkPriv(listboxSelection) {}
	}
    }
}

proc treetable_search_in_widget {w a beg {end end}} {
    if {[$w size] > 20000} {
	$w config -cursor watch
	update idletasks
    }
    set res [$w search -nocase -begins -- $a $beg $end]
    $w config -cursor {}
    if {$res == ""} {
	return -1
    }
    $w activate $res
    return $res
}

proc treetable_search_region {w a state} {

    #accept only ascii-characters
    if {[string compare $a ""] == 0 || [string length $a] > 1} {
	return -1
    }
    #returns if alt-key is pressed (reserved for menu)
    if {[expr {$state & 8}] == 8} {
	return -1
    }

    upvar #0 $w-pat pat

    append pat $a
    set srch $pat
    set off [$w index active]
    if {[string compare $off ""] == 0} {
	set sel 0
    } else {
	set sel [expr $off + 1]
    }
    # Search from the selection!
    set off [treetable_search_in_widget $w $srch $sel]

    if {$off == -1 && $sel != 0} {
	#     Search until the selection!
	set off [treetable_search_in_widget $w $srch 0 $sel]
    }
    if {$off == -1} {
	if {[string length $pat] > 1} {
	    set pat $a
	    set srch $pat

	    # Search from the selection!
	    set off [treetable_search_in_widget $w $srch $sel]
	    if {$off == -1} {
		#     Search until the selection!
		set off [treetable_search_in_widget $w $srch 0 $sel]
	    }
	}
	if {$off == -1} {
	    set pat ""
	    bell -displayof $w
	    $w selection clear 0 end
	    return 1
	}
    }
    $w selection clear 0 end
    $w activate $off
    $w activate $off
    $w yview see $off

    return 0
}

#immediatly define the treetabe default bindings
treetable_bindings TreeTable

