# This file tests the pixmap image reader
#

proc About {} {
    return "This file performs test on the DirList widget"
}

proc Test {} {
    set w .dirlist

    tixDirList $w
    pack $w

    set h [$w subwidget hlist]

    # If we didn't specifi -value, the DirList should display the
    # current directory
    Assert {[tixStrEq [$w cget -value] [tixFSPWD]]}

    # After changing the directory, the selection and anchor should change as
    # well
    set root [$h info children ""]
    ClickHListEntry $h $root single
    Assert {[tixStrEq [$w cget -value] [$h info data $root]]}
    Assert {[tixStrEq [$h info selection] $root]}
    Assert {[tixStrEq [$h info anchor]    $root]}

    case [tix platform] {
	unix {
	    set dir1 /etc
	    set dir2 /etc
	}
	windows {
	    set dir1 C:\\Windows
	    set dir2 C:\\Backup
	}
	default {
	    return
	}
    }

    foreach dir [list $dir1 $dir2] {
	if ![file exists $dir] {
	    continue
	}

	$w config -value $dir
	Assert {[tixStrEq [$w cget -value] $dir]}
	Assert {[tixStrEq [$h info data [$h info anchor]] $dir]}
    }
}
