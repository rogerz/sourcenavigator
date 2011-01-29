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
AUTOMAKE_OPTIONS = cygnus

## Process this file with automake to create Makefile.in.

SUBDIRS = misc db @SDK_DIR@ packages parsers hyper gui demo doc etc \
snavigator bitmaps

install-exec-local:
	(cd ../tcl; $(MAKE) install-libraries)
	(cd ../tk; $(MAKE) install-libraries)
	(cd ../tix; $(MAKE) install-libraries)
	(cd ../itcl; $(MAKE) install-libraries)
	(cd ../libgui; $(MAKE) install)

rootdir = $(prefix)

EXTRA_DIST = $(DATA)

flexgen:
	cd hyper ; $(MAKE) flexgen
	cd parsers/assembly ; $(MAKE) flexgen
	cd parsers/chill ; $(MAKE) flexgen
	cd parsers/m4 ; $(MAKE) flexgen
	cd parsers/python ; $(MAKE) flexgen
	cd parsers/sendmail ; $(MAKE) flexgen

bisongen:
	cd parsers/chill ; $(MAKE) bisongen
	cd parsers/fortran ; $(MAKE) bisongen
