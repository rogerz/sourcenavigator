# cleanup.tcl --
#
#	This program tests whether whether there is any garbage left
#	after all the test files are executed. If so, either Tix has
#	resource leak or the test suite doesn't clean up properly.
#	
# Copyright (c) 1996, Expert Interface Technologies
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

proc About {} {
    return "Testing resource leaks"
}

proc Test {} {
    global testConfig

    if {$testConfig(VERBOSE) >= 20} {
	foreach image [image names] {
	    puts "Warning: \[resource leak\] image $image of type [image type $image]"
	    foreach option [$image configure] {
		puts "  $option"
	    }
	}
    }
}
