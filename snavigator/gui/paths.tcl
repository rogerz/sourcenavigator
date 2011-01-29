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
# paths.tcl - Path handling.
# Copyright (C) 1998 Cygnus Solutions.

# Set path entries in global array, then start SN running.  This makes
# it easier to move stuff around.  Read the GNU programming standards
# to figure out what belongs where.

proc sn_initialize_paths {} {
	global sn_path sn_home

	sn_log "Initializing paths, SN home is at: $sn_home"

	# Following are extensions just for Source Navigator.
	set sn_path(bindir) [file join ${sn_home} bin]
    	if {![file exists $sn_path(bindir)]} {
        	set sn_path(bindir) [file dirname [info nameofexecutable]]
	}
	
	set sn_path(htmldir) [file join ${sn_home} share snavigator html]
	set sn_path(bitmapdir) [file join ${sn_home} share snavigator bitmaps]
	set sn_path(rundir) [file join ${sn_home} run]
	set sn_path(etcdir) [file join ${sn_home} share snavigator etc]
	set sn_path(libexecdir) [file join ${sn_home} libexec snavigator]
	set sn_path(parserdir) $sn_path(libexecdir)
	set sn_path(scriptsdir) $sn_path(libexecdir)
	set sn_path(toolchaindir) [file join ${sn_home} share snavigator etc sn_toolchains]
}

