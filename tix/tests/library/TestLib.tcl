# TestLib.tcl --
#
#	Implements the procedures used by the Tix test suite.
#
# Copyright (c) 1996, Expert Interface Technologies
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

set testapp(tix,w,normal) {
    tixButtonBox tixComboBox tixControl tixDirList tixDirTree
    tixExDirSelectBox tixExFileSelectBox tixFileSelectBox tixFileEntry
    tixLabelEntry tixLabelFrame tixNoteBook tixOptionMenu
    tixPanedWindow tixScrolledHList tixScrolledListBox
    tixScrolledTList tixScrolledText tixScrolledWindow tixSelect
    tixStdButtonBox tixTree
}
set testapp(tix,w,shell) {
    tixBalloon tixDialogShell tixExFileSelectDialog tixFileSelectDialog
    tixPopupMenu tixStdDialogShell
}
set testapp(tix,w,base) {
    tixLabelWidget 
    tixPrimitive 
    tixScrolledWidget 
    tixShell 
    tixStackWindow 
    tixVResize tixVStack tixVTree 
}
set testapp(tix,w,unsupported) {
    tixMDIMenuBar 
    tixMDIWindow 
    tixMwmClient 
    tixResizeHandle 
    tixSimpleDialog 
    tixStatusBar 
}

# testConfig(VERBOSE) is the "Verbosity level" of the test suite.
#
#	 0 -- No messages except the name of the tests
#	10 -- Print out the number of each test block
#	15 -- Print out the number and name of each test block
#	20 -- Print out all kinds of messages
#	30 -- level 20, plus when an error occurs, prints out the stack trace.
#
if [info exists env(TEST_VERBOSE)] {
    if [catch {
	set testConfig(VERBOSE) [expr "int($env(TEST_VERBOSE) + 0)"]
    }] {
	set testConfig(VERBOSE) 10
    }
} else {
    set testConfig(VERBOSE) 0
}

set testConfig(errCount) 0

#----------------------------------------------------------------------
#
#	General assertion and evaluation
#
#----------------------------------------------------------------------

# Assert --
#
#	Evaulates an assertion. Output error message if the assertion is false
#
proc Assert {cond {printErrInfo 0} {abortMode abortfile}} {
    global errorInfo testConfig
    if [info exists errorInfo] {
	set errorInfo ""
    }
    uplevel 1 [list \
	if !($cond) [list \
	    TestError "Failed Assertion \"$cond\"\n   evaluated as \"[uplevel 1 subst -nocommand [list $cond]]\" :: [uplevel 1 subst [list $cond]]" $printErrInfo $abortMode
	] \
    ]
}

# TestAbort --
#
#	Aborts a single test file.
#
proc TestAbort {msg} {
    error $msg
}

# test --
#
#	Try to evaluate a command.
#
proc test {cmd {result {}} {ret {}}} {
    global testConfig

    if [catch {set ret [uplevel 1 $cmd]} err] {
	set done 0
	foreach r $result {
	    if [regexp $r $err] {
		if {$testConfig(VERBOSE) >= 20} {
		    puts "Passed (Error message is expected):"
		    puts " command        = \"$cmd\""
		    puts " expected error = \"$result\""
		    puts " actual error   = $err"
		}
		set done 1
		break
	    }
	}
	if {!$done} {
	    error $err
	}
    } else {
	if {$testConfig(VERBOSE) >= 20} {
	    puts "Passed (Execution OK):\n command = \"$cmd\""
	}
    }
    return $ret
}

# test1 --
#
#	Try to evaluate a command and make sure its error result is the same
#	as $result.
#
proc test1 {cmd {result {}}} {
    global testConfig

    set ret ""
    if [catch {set ret [uplevel 1 $cmd]} err] {
	if ![tixStrEq $err $result] {
	    error $err
	} else {
	    if {$testConfig(VERBOSE) >= 20} {
		puts "Passed (Error message is expected):"
		puts " command        = \"$cmd\""
		puts " expected error = \"$result\""
	    }
	}
    } else {
	if {$testConfig(VERBOSE) >= 20} {
	    puts "Passed (Execution OK):\n command = \"$cmd\""
	}
    }
    return $ret
}

#----------------------------------------------------------------------
#
#	Mouse event emulation routines
#
#----------------------------------------------------------------------
proc GetRoot {w x y} {
    upvar X X
    upvar Y Y

    set x0 [winfo rootx $w]
    set y0 [winfo rooty $w]

    set X [expr $x0 + $x]
    set Y [expr $y0 + $y]
}

proc MouseEvent {w type x y args} {
    set tags [bindtags $w]
    GetRoot $w $x $y

    lappend args %q
    lappend args $w
    lappend args %W
    lappend args $w
    lappend args %x
    lappend args $x
    lappend args %y
    lappend args $y
    lappend args %X
    lappend args $X
    lappend args %Y
    lappend args $Y

    set found 0
    foreach t $tags {
	set cmd [string trim [bind $t $type]]

	if {$cmd != ""} {
	    set found 1
	}
	tixForEach {sub val} $args {
	    regsub -all $sub $cmd $val cmd
	}
	uplevel #0 $cmd
    }
    if {$found == 0} {
	global testConfig
	if $testConfig(VERBOSE) {
	    puts "(testlib warning): widget $w has no bindings for $type"
	}
    }
    return $found
}

# KeyboardString --
#
#	Send a string to the widget via a list of key strokes. This does
#	NOT ensure that an entry widget has the exact content as $string.
#	You need to call $entry delete 0 end first!
#
proc KeyboardString {w string} {
    set tags [bindtags $w]

    lappend args %q
    lappend args $w
    lappend args %W
    lappend args $w

    set found 0

    foreach c [split $string ""] {
	foreach t $tags {
	    set cmd [string trim [bind $t <KeyPress>]]

	    if {$cmd != ""} {
		set found 1
	    }
	    set list $args
	    lappend list %A
	    lappend list [list $c]
	
	    tixForEach {sub val} $list {
		regsub -all $sub $cmd $val cmd
	    }

	    # This is really weird. If our char is '\', the lappend line
	    # makes it a quoted \\, but the previous regsub makes it back
	    # to a single quote. So we use regsub again to make it a \\
	    # again. But that's not enough, because uplevel will change it
	    # back to a single quote and will eventually mess us up. Hence
	    # we use quad-slashes here!
	    #
	    regsub -all {[\\]} $cmd {\\\\} cmd
	    uplevel #0 $cmd
	}
    }
    if {$found == 0} {
	puts "warning: widget $w has no bindings for $type"
    }
    return $found

}

# KeyboardEvent --
#
#	Send a special keyboard event to the widget. E.g., <Return>
#	<space>, <Escape>, <BackSpace> etc. To send ascii character
#	strings, use KeyboardString
#
proc KeyboardEvent {w type} {
    set tags [bindtags $w]

    lappend args %q
    lappend args $w
    lappend args %W
    lappend args $w

    set found 0
    foreach t $tags {
	set cmd [string trim [bind $t $type]]

	if {$cmd != ""} {
	    set found 1
	}
	tixForEach {sub val} $args {
	    regsub -all $sub $cmd $val cmd
	}
	uplevel #0 $cmd
    }
    if {$found == 0} {
	puts "warning: widget $w has no bindings for $type"
    }
    return $found
}

proc Event-Initialize {} {
    global app

    set app(X)      -1000
    set app(Y)      -1000
    set app(curWid) {}
}

proc InWidget {w} {
    global app

    return [tixWithinWindow $w $app(X) $app(Y)]
}

proc Leave {w {x -10} {y -10} args} {
    global app

    eval MouseEvent $w <Leave> $x $y $args
}

proc B1-Leave {w {x -10} {y -10} args} {
    global app

    eval MouseEvent $w <Leave> $x $y $args
}

proc RecordRoot {w x y} {
    global app

    GetRoot $w $x $y
    set app(X) $X
    set app(Y) $Y
}

proc Enter {w {x -1} {y -1} args} {
    global app

    if {$y == -1} {
	set x [expr [winfo width  $w] / 2]
	set y [expr [winfo height $w] / 2]
    }

    if {$app(curWid) != {} && [winfo exists $app(curWid)]} {
	Leave $app(curWid)
    }
    RecordRoot $w $x $y

    eval MouseEvent $w <Enter> $x $y $args
    set app(curWid) $w
}

proc Drag {w {x -1} {y -1} args} {
    global app

    if {$y == -1} {
	set x [expr [winfo width  $w] / 2]
	set y [expr [winfo height $w] / 2]
    }

    if {![InWidget $w]} {
	B1-Leave $w $x $y
    }

    eval MouseEvent $w <B1-Motion> $x $y $args
}

# Release --
#
#	Release mouse button 1 in a widget
#
proc Release  {w {x -1} {y -1} args} {
    global app

    if {$y == -1} {
	set x [expr [winfo width  $w] / 2]
	set y [expr [winfo height $w] / 2]
    }
    eval MouseEvent $w <ButtonRelease-1> $x $y $args
}

# Assumming the button was not originally down
#
proc HoldDown {w {x -1} {y -1} args} {
    global app

    if {$y == -1} {
	set x [expr [winfo width  $w] / 2]
	set y [expr [winfo height $w] / 2]
    }
    if {![InWidget $w]} {
	Enter $w $x $y
    }
    
    if {![eval MouseEvent $w <ButtonPress-1> $x $y $args]} {
	eval MouseEvent $w <1> $x $y $args
    }
}

proc Click {w {x -1} {y -1} args} {
    global app

    if {$y == -1} {
	set x [expr [winfo width  $w] / 2]
	set y [expr [winfo height $w] / 2]
    }
    eval HoldDown $w $x $y $args
    eval MouseEvent $w <ButtonRelease-1> $x $y $args
}

proc Double {w {x -1} {y -1} args} {
    global app

    if {$y == -1} {
	set x [expr [winfo width  $w] / 2]
	set y [expr [winfo height $w] / 2]
    }
    eval MouseEvent $w <Double-1> $x $y $args
}

# ClickListboxEntry --
#
#	Simulate the event where a listbox entry is clicked.
# Args:
#	w:widget	pathname of listbox
#	index:LbIndex	index of entry to be clicked.
#	mode:string	"single" or "double" indicating whether a single or
#			double click is desired.
#
proc ClickListboxEntry {w index {mode single}} {
    $w see $index
    set bbox [$w bbox $index]
    set x1 [lindex $bbox 0]
    set y1 [lindex $bbox 1]

    if {$mode == "single"} {
	Click $w $x1 $y1
    } else {
	Double $w $x1 $y1
    }
}

# ClickHListEntry --
#
#	Simulate the event where an HList entry is clicked.
# Args:
#	w:widget	pathname of HList
#	index:HLIndex	index of entry to be clicked.
#	mode:string	"single" or "double" indicating whether a single or
#			double click is desired.
#
proc ClickHListEntry {w index {mode single}} {
    $w see $index
    update
    set bbox [$w info bbox $index]
    set x1 [lindex $bbox 0]
    set y1 [lindex $bbox 1]

    if {$mode == "single"} {
	Click $w $x1 $y1
    } else {
	Double $w $x1 $y1
    }
}

# InvokeComboBoxByKey --
#
#	Simulate the event when the user types in a string into the
#	entry subwidget of a ComboBox widget and then type Return
#
proc InvokeComboBoxByKey {w string} {
    set ent [$w subwidget entry]
    $ent delete 0 end
    KeyboardString $ent $string
    KeyboardEvent $ent <Return>
    update
}

# SetComboBoxByKey --
#
#	Simulate the event when the user types in a string into the
#	entry subwidget of a ComboBox widget, *without* a subsequent
#	Return keystroke.
#
proc SetComboBoxByKey {w string} {
    set ent [$w subwidget entry]
    $ent delete 0 end
    KeyboardString $ent $string
    update
}

#----------------------------------------------------------------------
#
#			main routines
#
#----------------------------------------------------------------------

proc Done {args} {
    global testConfig

    if {$testConfig(VERBOSE) >= 20} {
	puts "------------------------done--------------------------------"
    }
}

proc Wait {msecs} {
    global Test:timer
    set Test:timer 0
    after $msecs uplevel #0 set Test:timer 1
    tkwait variable Test:timer
}

proc TestPuts {msg} {
    puts $msg
}

#----------------------------------------------------------------------
#
#			Messages
#
#----------------------------------------------------------------------
proc PutP {msg} {
    puts $msg
}
proc PutTitle {msg} {
    puts $msg
}
proc PutSubTitle {msg} {
    puts $msg
}
proc PutSubSubTitle {msg} {
    puts $msg
}
proc TestWarn {msg} {
    puts "Warning: $msg"
}
proc TestError {msg {printErrInfo 0} {abortMode cont}} {
    global testConfig
    puts "    $msg"
    case $abortMode {
	cont {
	    if {$printErrInfo || $testConfig(VERBOSE) >= 30} {
		global errorInfo
		puts "\$errorInfo = $errorInfo"
	    }
	    return
	}
	abortfile {
	    return -code 1234
	}
	abortall {
	    global errorInfo
	    puts "Aborting all test files because of the unrecoverable error:"
	    puts $errorInfo
	    exit 1
	}
    }
}

# TestBlock --
#
#	Performs a block of test. A block is mainly used to group
#	together tests that are dependent on each other. TestBlocks
#	may be nested.
#
# Args:
#	name:		Textual name of the test. E.g.: button-1.1
#	description:	Short description of the test. "Pressing button"
#	printErrInfo:	If an error occurs, should the errorInfo be printed
#			to the console. (Normally only a one-liner error
#			message is printed).
#	abortMode:	cont      -- skip this block and go to the next block
#			abortfile -- skip all other blocks in this file
#			abortall  -- skip all the Tix tests.
#
proc TestBlock {name description script {printErrInfo 0} {abortMode cont}} {
    global testConfig

    set code [catch {uplevel 1 $script} result]

    if {$testConfig(VERBOSE) >= 15} {
	set des "($description)"
    } else {
	set des ""
    }

    if {$code != 0} {
	incr testConfig(errCount)
	puts stdout "---- $name FAILED $des"
	puts "Script is"
	foreach line [split $script \n] {
	    regsub "^\[[format %s \ \n\t]\]*" $line "" line
	    puts "    $line"
	}
	puts "Error message:"
	TestError $result $printErrInfo $abortMode
	puts stdout "----"
    } elseif $testConfig(VERBOSE) {
	puts stdout "++++ $name PASSED $des"
    }
}

#----------------------------------------------------------------------
#
#			general initialization
#
#----------------------------------------------------------------------

# init the event emulation
#

# some window managers don't put the main window at a default place, this
# may be quite annoying for the user
#
wm geometry . +0+0

