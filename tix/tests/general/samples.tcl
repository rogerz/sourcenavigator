# samples.tcl --
#
#	Tests all the sample programs in the demo/samples directory.
#
#
# Copyright (c) 1996, Expert Interface Technologies
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

proc About {} {
    return "Testing all the sample programs in the demo/samples directory"
}

proc Test {} {
    global samples_dir demo_dir tix_library

    TestBlock samples-1.0 "Finding the demo directory" {
	foreach dir "$tix_library/demos $tix_library/../demos ../../demos ../demos demos" {
	    if {[file exists $dir] && [file isdir $dir]} {
		set pwd [pwd]
		cd $dir
		set demo_dir    [pwd]
		set samples_dir [pwd]/samples
		cd $pwd
		break
	    }
	}
    }

    if ![info exists samples_dir] {
	puts "Cannot find demos directory. Sample tests are skipped"
	return
    } else {
	puts "loading demos from $demo_dir"
    }

    TestBlock samples-1.1 "Running widget demo" {
	if [file exists $demo_dir/widget] {
	    uplevel #0 source [list $demo_dir/widget]
	    Widget:SelfTest
	}
    }
    if ![file exists $samples_dir/AllSampl.tcl] {
	return
    }
    uplevel #0 source [list $samples_dir/AllSampl.tcl]

    ForAllSamples root "" Test_Sample
}


proc Test_Sample {token type text dest} {
    global samples_dir tix_demo_running 

    set tix_demo_running 1

    if {$type == "f"} {
	set w .sampl_top
	TestBlock samples-2-$dest "Loading sample $dest" {
	    uplevel #0 source [list $samples_dir/$dest]
	    toplevel $w
	    wm geometry $w +100+100
	    wm title $w $text
	    RunSample $w
	    update
	}
	catch {
	    destroy $w
	}
    }
}
