# labentry.tcl --
#
#	Tests the TixLabelEntry widget.
#
# Copyright (c) 1996, Expert Interface Technologies
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

proc About {} {
    return "Testing the TixLabelEntry widget"
}

proc Test {} {
    TestBlock labent-1.1 {LabelEntry focus management} {
	set t [toplevel .t]

	set w [tixLabelEntry .t.c -label "Stuff: "]
	pack $w -padx 20 -pady 10
	tixLabelEntry .t.d -label "Stuff: "
	pack .t.d -padx 20 -pady 10
	focus $w
	update

	set px [winfo pointerx $t]
	set py [winfo pointery $t]
	set W [winfo width $t]
	set H [winfo height $t]

	if {$W < 100} {
	    set W 100
	}
	if {$H < 100} {
	    set H 100
	}

	set mx [expr $px - $W / 2]
	set my [expr $py - $H / 2]

	# We must move the window under the cursor in order to test
	# the current focus
	#
	wm geometry $t $W\x$H+$mx+$my
	raise $t
	update

	Assert {[focus -lastfor $t] == [$w subwidget entry]}

	destroy $t
    }
}
