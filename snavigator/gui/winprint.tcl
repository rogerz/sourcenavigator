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
#cancel print process
proc PRINT_cancel {} {
	sn_log "Cancel print process..."
	ide_winprint abort
}

#print-dialog
proc PRINT_pagecounter_dialog {pageno} {

	sn_log "PRINT_pagecounter_dialog: <$pageno>"

	#No page numbers by printing PostScript!!
	if {$pageno >= 0} {
		set msg [format [get_indep String NowPrintingPage] $pageno]
	} else  {
		set msg [get_indep String NowPrinting]
	}

	#Display the page number
	if {![winfo exists .error]} {
		#create the dialog box so that it doesn't wait for closing
		sn_error_dialog \
			$msg \
			[get_indep String Print] \
			"" \
			"PRINT_cancel"
	} else {
		.error.msg_bitmap.msg config -text [format [get_indep String NowPrintingPage] $pageno]
	}
}

# The query procedure passed to ide_winprint print_text.  This should
# return one of "continue", "done", or "newpage".
proc PRINT_query { } {
    global PRINT_state

	# Fetch the next line into PRINT_state(str).

	if {$PRINT_state(file) == 1} then {
		set strlen [gets $PRINT_state(fp) PRINT_state(str)]
	} else {
		set strlen [string first "\n" $PRINT_state(text)]
		if {$strlen != -1} then {
			set PRINT_state(str) \
			    [string range $PRINT_state(text) 0 [expr $strlen-1]]
			set PRINT_state(text) \
			    [string range $PRINT_state(text) [expr $strlen+1] end]
		} else {
			if {$PRINT_state(text) != ""} then {
				set strlen 0
				set PRINT_state(str) $PRINT_state(text)
				set PRINT_state(text) ""
			}
		}
	}

	if {$strlen != -1} then {

		# Expand tabs assuming tabstops every 8 spaces and a fixed
		# pitch font.  Text written to other assumptions will have to
		# be handled by the caller.
		set str $PRINT_state(str)
		while {[set i [string first "\t" $str]] >= 0} {
			set c [expr 8 - ($i % 8)]
			set spaces ""
			while {$c > 0} {
				append spaces " "
				incr c -1
			}
			set str "[string range $str 0 [expr $i - 1]]$spaces[string range $str [expr $i + 1] end]"
		}
		set PRINT_state(str) $str

		return "continue"
	} else {
		return "done"
	}
}

# The text procedure passed to ide_winprint print_text.  This should
# return the next line to print.
proc PRINT_text {} {
	global PRINT_state

	#sn_log "PRINT_text <$PRINT_state(str)>"
	return $PRINT_state(str)
}

#this procedure is invoked as first to verify what the user
#has selected in the PrintDialog
proc PRINT_init {cmd args} {
	global PRINT_state
	
	sn_log "Print Dialog Selection: $args"
	
	set len [llength $args]
	for {set i 0} {$i < $len} {incr i} {
		set op [lindex $args $i]
		if {$op == ""} {
			break
		}
		incr i
		set val [lindex $args $i]
		switch -- $op {
			"-pagefrom" {
				set PRINT_state(frompage) $val
			}
			"-pageto" {
				set PRINT_state(topage) $val
			}
			"-selection" {
				set PRINT_state(selection) $val
			}
			"-copies" {
				set PRINT_state(copies) $val
			}
			default {
				#unknown option
				return -1
			}
		}
	}
	
	#no init command is specified, 
	if {$cmd == ""} {
		return 0
	}
	
	return [eval $cmd]
}

# This page procedure passed to ide_winprint print_text.  This is
# called at the start of each page.
proc PRINT_page { pageno } {
	global PRINT_state

	sn_log "PRINT_page: <$pageno>"

	set PRINT_state(pageno) [format [get_indep String "NowPrintingPage"] $pageno]

	PRINT_pagecounter_dialog $pageno

	update
	return "continue"
}

##################################
# commands for PostScript printing
##################################
# The query procedure passed to ide_winprint print_text.  This should
# return one of "continue", "done", or "newpage".
proc PRINT_postscript_query { } {
	global PRINT_state

	# Fetch the next line into PRINT_state(str).
	if {$PRINT_state(file) == 1} then {
		#don't print the file at once, read blockwise
		set PRINT_state(str) [read $PRINT_state(fp) 10240]
	} else {
		set PRINT_state(str) ""
	}

	if {$PRINT_state(str) != ""} then {
		return "continue"
	} else {
		return "done"
	}
}


#Windows Printer Dialog
proc sn_winprint_dialog {file {text ""} {postscript ""} {cmd ""}} {
	global PRINT_state
	global sn_options
	
	sn_log "print dialog..."
	
	#init printing options
	set PRINT_state(frompage)  0
	set PRINT_state(topage)    99999
	set PRINT_state(selection) 0
	set PRINT_state(copies)    1
	
	if {$postscript == ""} {
		set PRINT_state(postscript) 0
	} else {
		set PRINT_state(postscript) 1
		set PRINT_state(done) 0
	}
	#print file or string
	if {$file != ""} {
		set PRINT_state(fp) [open $file "r"]
		fconfigure $PRINT_state(fp) -encoding $sn_options(def,system-encoding)
		set PRINT_state(file) 1
		set PRINT_state(text) ""
	} else {
		set PRINT_state(file) 0
		set PRINT_state(text) $text
	}
	
	if {$PRINT_state(postscript) == 0} {
		ide_winprint \
			print_text \
			PRINT_query \
			PRINT_text \
			-pageproc PRINT_page \
			-initproc "PRINT_init $cmd"
	} else {
		ide_winprint \
			print \
			PRINT_postscript_query \
			PRINT_text \
			-pageproc PRINT_page \
			-postscript yes \
			-initproc "PRINT_init $cmd"
	}

	if {$file != ""} {
		catch {close $PRINT_state(fp)}
	}
	catch { destroy .error }
	
	sn_log "printing process finished"
}


