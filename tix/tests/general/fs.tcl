# fs.tcl --
#
#	Test the portable file handling ("FS") routines.
#
# Copyright (c) 1996, Expert Interface Technologies
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

proc About {} {
    return "Testing portable file handling routines"
}

proc Test {} {
    global tixPriv errorInfo

    TestBlock fs-1.1 {tixFSPath command} {
	if {$tixPriv(test:platform) == "windows"} {
	    #   PATHNAME		     expected VPATH result
	    #-------------------------------------------------------
	    set list [list \
	        [list :px:\\C:	      C:\\	0] \
	        [list :px:\\c:	       ""       1] \
	    ]
	    regsub -all :px: $list $tixPriv(WinPrefix) list

	    foreach item "$list" {
		set vpath [lindex $item 0]
		set want  [lindex $item 1]
		set experr [lindex $item 2]
		

		TestBlock fs-1.1.1 "tixFSPath $vpath" {
		    set err [catch {
			set got [tixFSPath $vpath]
		    }]

		    if $experr {
			Assert {$err == $experr}
		    } else {
			Assert {[tixStrEq $want $got]}
		    }
		}
	    }
	}
    }

    TestBlock fs-1.2 {tixFSIsNorm command} {
	if {$tixPriv(test:platform) == "unix"} {

	    #   PATHNAME to TEST		     expected result
	    #-------------------------------------------------------
	    set list {
		{/home/ioi			1}
		{/foo.bar			1}
		{/.../foo			1}
		{/.../foo/bar/...		1}
		{/.../.foo/bar/...		1}
		{/.../.f./bar/...		1}
		{/.../.f./bar/...		1}
		{/..a/...			1}
		{"/. / "			1}
		{//a				0}
		{/a/b/				0}
		{/a/b//				0}
		{/a/b/.				0}
		{a/b				0}
		{a/b/.				0}
		{/./b				0}
		{/../b				0}
		{/../../b			0}
		{/./a/../b/..			0}
		{~ioi				0}
		{/~ioi				1}
		{/				1}
	    }
	} else {
	    set list {
		{C:/				0}
		{foo				0}
		{c:				0}
		{C:				1}
		{C:\\Windows			1}
		{C:\\				0}
		{C:\\..\\Windows		0}
		{C:\\...\\Windows		1}
		{C:\\.../Windows		1}
		{C:\\.\\Windows			0}
		{..				0}
		{..\\..				0}
		{..\\				0}
		{.				0}
		{.\\.				0}
		{.\\				0}
		{C:\\.				0}
		{C:Windows			0}
		{C:\\Windows\\App		1}
		{"C:\\My Programs\\~App"	1}
	    }
	}

	foreach item $list {
	    set text [lindex $item 0]
	    set want [lindex $item 1]


	    TestBlock fs-1.2.1 "tixFSIsNorm $text" {
		Assert {[tixFSIsNorm $text] == $want}
	    }
	}
    }

    TestBlock fs-1.3 {tixFSNormDir command} {
	foreach item [GetCases_FsNormDir] {
	    set text    [lindex $item 0]
	    set want    [lindex $item 1]
	    set wanterr [lindex $item 2]

	    if !$wanterr {
		# Check test case error
		Assert {[tixFSIsNorm $want]}
	    }

	    TestBlock fs-1.3.1 "tixFSNormDir $text" {
		set err [catch {
		    set got [tixFSNormDir $text]
		}]
		
		Assert {$err == $wanterr}
		if {!$err} {
		    Assert {[tixStrEq $want $got]}
		}
	    }
	}
    }

    TestBlock fs-1.4 {tixFSNorm command} {
	set list [GetCases_FSNorm]

	set appPWD [pwd]
	foreach item $list {
	    set text    [lindex $item 0]
	    set context [lindex $item 1]
	    set want    [lindex $item 2]

	    TestBlock fs-1.4.1 "tixFSNorm $context $text" {
		set lst [tixFSNorm $context $text]
		set dir [lindex $lst 1]
		Assert {[tixStrEq $want $dir]}
		Assert {[tixStrEq [pwd] $appPWD]}
	    }
	}
    }

    TestBlock fs-1.5 {tilde handling} {
	if {$tixPriv(test:platform) == "unix"} {
	    set who "nobody"
	    if {[string comp $who "nobody"] == 0} {
		catch {set who [exec whoami]}
	    }
	    if {[string comp $who "nobody"] == 0} {
		catch {set who [exec logname]}
	    }
	    set home /
	    catch {
		set home [glob ~$who]
	    }
	    set list {
		{~$who		{$home		    $home   ""  ""}}
		{~		{$home		    $home   ""  ""}}
		{~/*.*		{$home/*.*	    $home   ""  "*.*"}}
		{"~/*.* *.tcl"	{"$home/*.* *.tcl"  $home   ""  "*.* *.tcl"}}
	    }

	    foreach item $list {
		set item [subst $item]
		set text [lindex $item 0]
		set want [lindex $item 1]

		TestBlock fs-1.5.1 "tixFSNorm \[pwd\] $text" {
		    set list [tixFSNorm [pwd] $text]

		    Assert {
			[tixStrEq [lindex $list 0] [lindex $want 0]] &&
			[tixStrEq [lindex $list 1] [lindex $want 1]] &&
			[tixStrEq [lindex $list 2] [lindex $want 2]] &&
			[tixStrEq [lindex $list 3] [lindex $want 3]]
		    }
		}
	    }
	}
    }

    TestBlock fs-1.6 {tixFSVPath} {
	if {$tixPriv(test:platform) == "unix"} {

	    #   PATHNAME to TEST	    expected     Causes error for
	    #				    result	  tixFSVPath?
	    #----------------------------------------------------------------
	    set list {
		{.				""		1}
		{foo				""		1}
		{./				""		1}
	    }
	} else {
	    set list {
		{.				""		1}
	    }
	    regsub -all ^:px: $list $tixPriv(WinPrefix) list
	}

	# (ToDo): write the test
	#
    }

    TestBlock fs-2.1 {obsolete tests} {
	# Some obsolete test. Should be taken out.
	#
	if {$tixPriv(test:platform) == "unix"} {
	    set home [glob ~]
	    if {$home == "/"} {
		set homeprefix {}
	    } else {
		set homeprefix $home
	    }

	    # it shouldn't do itemname substitution
	    #
	    Assert {[tixFileIntName *] == "*"}
	    Assert {[tixFileIntName ~/*] == "$homeprefix/*"}

	    Assert {[tixFileIntName /home/ioi/../foo/bar/..] == "/home/foo"}
	}
    }
}
