# This is the "Test Driver" program that sources in each test script. It
# must be invoked by the test/Test.tcl program (in Unix) or by a properly
# configured wish.exe program (in Wondows).
#

catch {
    cd [file dirname [info script]]
}

set oldglobals {}
set oldglobals [info globals]

# Some parts of the test execute tests for a specific platform. The variable
# tixPriv(test:platform) controls the tests for which platform should
# be executed. This can be controlled by the TEST_PLATFORM environment
# variable 

set tixPriv(test:platform) unix
if [info exists tcl_platform(platform)] {
    if {$tcl_platform(platform) == "windows"} {
	set tixPriv(test:platform) windows
    }
}

if [info exists env(TEST_PLATFORM)] {
    set tixPriv(test:platform) $env(TEST_PLATFORM)
}

global testConfig
if {![info exists tix]} {
    if ![info exists tcl_platform(platform)] {
	puts "ERROR: this version of wish doesn't support dynamic loading"
	exit -1
    }

    # This must have been executed by a plain wish expecting to
    # dynamic load Tix.

    puts -nonewline "trying to dynamically load Tix ... "

    global tk_version
    if {$tcl_platform(platform) == "unix"} {
	case $tk_version {
	    4.1 {
		set testConfig(dynlib) \
		    ../unix/tk4.1/libtix4.1.4.1[info sharedlibextension]
	    }
	    4.2 {
		set testConfig(dynlib) \
		    ../unix/tk4.2/libtix4.1.4.2[info sharedlibextension]
	    }
	}
    } else {
	case $tk_version {
	    4.1 {
		set testConfig(dynlib) ..\\win\\tix41.dll
	    }
	    4.2 {
		set testConfig(dynlib) ..\\win\\tix41.dll
	    }
	}
    }

    if [info exists testConfig(dynlib)] {
	load $testConfig(dynlib) Tix
    }

    if {[info exists tix]} {
	puts succeeded
    } else {
	puts failed
	exit
    }
} else {
    set testConfig(dynlib) ""
}

proc Driver:Test {name f} {
    global oldglobals errorInfo testConfig

    foreach w [winfo children .] {
	if [string comp .__top $w] {
	    destroy $w
	}
    }
	
    foreach g [info globals] {
	if {[lsearch $oldglobals $g] == -1} {
#	    uplevel #0 unset $g
	}
    }

    if {$testConfig(VERBOSE) >= 20} {
	puts ------------------------------------------------------------
	puts "Loading script $name"
    } else {
	puts $name
    }

    update
    uplevel #0 source $f
    Event-Initialize
    catch {
	wm title . [About]
	if {$testConfig(VERBOSE) >= 20} {
	    puts "  [About]"
	    puts "---------------------starting-------------------------------"
	}
    }

    set code [catch {
	Test
    } error]

    if $code {
	if {$code == 1234} {
	    puts -nonewline "Test $f is aborted"
	} else {
	    puts -nonewline "Test $f is aborted unexpectedly"
	}
	if {[info exists errorInfo] && ![tixStrEq $errorInfo ""]} {
	    puts " by the following error\n$errorInfo"
	} else {
	    puts "."
	}
    }
    Done
}

# fileList: name of the file that contains a list of test targets
# type: "dir" or "script"
#
proc Driver:GetTargets {fileList type} {
    set fd [open $fileList {RDONLY}]
    set data {}

    while {![eof $fd]} {
	set line [string trim [gets $fd]]
	if [regexp ^# $line] {
	    continue
	}
	append data $line\n
    }

    close $fd
    set files {}

    foreach item $data {
	set takeit 1

	foreach cond [lrange $item 1 end] {
	    set inverse 0
	    set cond [string trim $cond]
	    if {[string index $cond 0] == "!"} {
		set cond [string range $cond 1 end]
		set inverse 1
	    }

	    set true 1
	    case [lindex $cond 0] {
		c {
		    set cmd [lindex $cond 1]
		    if {[info command $cmd] != $cmd} {
			if ![auto_load $cmd] {
			    set true 0
			}
		    }
		}
		i {
		    if {[lsearch [image types] [lindex $cond 1]] == -1} {
			set true 0
		    }
		}
		v {
		    set var [lindex $cond 1]
		    if ![uplevel #0 info exists [list $var]] {
			set true 0
		    }
		}
		default {
		    # must be an expression
		    #
		    if ![uplevel #0 expr [list $cond]] {
			set true 0
		    }
		}
	    }

	    if {$inverse} {
		set true [expr !$true]
	    }
	    if {!$true} {
		set takeit 0
		break
	    }
	}

	if {$takeit} {
	    lappend files [lindex $item 0]
	}
    }
    return $files
}

proc Driver:Main {} {
    global argv env

    if [tixStrEq $argv "dont"] {
	return
    }

    set argvfiles  $argv
    set env(WAITTIME) 200

    set errCount 0

    set PWD [pwd]
    if {$argvfiles == {}} {
	set argvfiles [Driver:GetTargets files dir]
    }

    foreach f $argvfiles {
	Driver:Execute $f
	cd $PWD
    }
}

proc Driver:Execute {f} {
    global testConfig

    if [file isdir $f] {
	raise .
	set dir $f

	if {$testConfig(VERBOSE) >= 20} {
	    puts "Entering directory $dir ..."
	}
	cd $dir

	if [file exists pkginit.tcl] {
	    # call the package initialization file, which is
	    # something specific to the files in this directory
	    #
	    source pkginit.tcl
	}
	foreach f [Driver:GetTargets files script] {
	    set _PWD [pwd]
	    Driver:Test $dir/$f $f
	    cd $_PWD
	}
	if {$testConfig(VERBOSE) >= 20} {
	    puts "Leaving directory $dir ..."
	}
    } else {
	set dir [file dirname $f]
	if {$dir != {}} {
	    if {$testConfig(VERBOSE) >= 20} {
		puts "Entering directory $dir ..."
	    }
	    cd $dir
	    if [file exists pkginit.tcl] {
		# call the package initialization file, which is
		# something specific to the files in this directory
		#
		source pkginit.tcl
	    }
	    set f [file tail $f]
	}
	set _PWD [pwd]
	Driver:Test $f $f
	cd $_PWD

	if {$testConfig(VERBOSE) >= 20} {
	    puts "Leaving directory $dir ..."
	}
    }
}

if [tixStrEq [tix platform] "windows"] {
    # The following are a bunch of useful functions to make it more convenient
    # to run the tests on Windows inside the Tix console window.
    #

    # do --
    #
    #	Execute a test.
    #
    proc do {f} {
	set PWD [pwd]
	Driver:Execute $f
	cd $PWD
	puts "% "
    }

    # rnew --
    #
    # 	Read in all the files in the Tix library path that has been modified.
    #
    proc rnew {} {
	global lastModified filesPatterns
	foreach file [eval glob $filesPatterns] {
	    set mtime [file mtime $file]
	    if {$lastModified < $mtime} {
		set lastModified $mtime
		puts "sourcing $file"
		uplevel #0 source [list $file]
	    }
	}
    }
    
    # pk --
    #
    #	pack widgets filled and expanded
    proc pk {args} {
	eval pack $args -expand yes -fill both
    }

    # Initialize the lastModified so that rnew only loads in newly modified
    # files
    #
    set filesPatterns {../library/*.tcl Driver.tcl library/*.tcl}
    set lastModified 0
    foreach file [eval glob $filesPatterns] {
	set mtime [file mtime $file]
	if {$lastModified < $mtime} {
	    set lastModified $mtime
	}
    }

    proc ei {} {
	global errorInfo
	puts $errorInfo
    }
}


uplevel #0 source library/TestLib.tcl
uplevel #0 source library/CaseData.tcl
wm title . "Test-driving Tix"
Driver:Main

puts "$testConfig(errCount) error(s) found"

if {[tix platform] != "windows"} {
    destroy .
    catch {
	update
    }
    exit 0
} else {
    puts -nonewline "type \"exit\" to quit the test\n% "
    proc q {} {
	exit
    }
}

