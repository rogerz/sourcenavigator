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
global argv

proc VerifyNames {file1 file2} {
	set errors 0

	if {[catch {set f1 [open $file1]}]} {
		puts stderr "Cannot open $file1"
		return 1
	}
	if {[catch {set f2 [open $file2]}]} {
		puts stderr "Cannot open $file2"
		return 1
	}
	foreach fd [list $f1 $f2] {
		if {$fd == $f1} {
			set catalog catalog1
			set file $file1
		} else {
			set catalog catalog2
			set file $file2
		}

		while {1} {
			set res [gets $fd line]
			if {$res < 0} {
				# End of file.
				break
			}
			set line [string trim $line]
			if {$line == "" || [string first "#" $line] == 0} {
				# Skip blank lines and comment lines.
				continue
			}
			set line [split $line ","]
			set key [lindex $line 0]
			set value [lindex $line 1]

			# Test for ?+.
			if {[regexp {^\?+} $value]} {
				puts stderr "Warning: string for $key is incomplete in $file"
			}

			if {[info exists ${catalog}($key)]} {
				puts stderr "Warning: duplicate key $key in $file"
			}
			set ${catalog}($key) $value
		}
	}
	
	foreach key [array names catalog1] {
		if {![info exists catalog2($key)]} {
			puts stdout "Error: key $key in $file1 but not in $file2"
			set errors 1
		}
	}

	foreach key [array names catalog2] {
		if {![info exists catalog1($key)]} {
			puts stdout "Error: key $key in $file2 but not in $file1"
			set errors 1
		}
	}
	return $errors
}

if {$argc != 2} {
	puts stderr "Usage: verify_texts file1 file2"
	exit 1
}

exit [VerifyNames [lindex $argv 0] [lindex $argv 1]]

