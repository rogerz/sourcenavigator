# Copyright (c) 2004, Mo DeJong
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

toolbox = $(top_builddir)/parsers/toolbox
common = $(top_builddir)/parsers/common

libexec_PROGRAMS = exlbrowser

INCLUDES = $(WINFLAGS) \
	-I$(top_srcdir)/hyper \
	-I$(top_srcdir)/parsers/toolbox \
	-I$(top_srcdir)/parsers/common \
	-I$(top_srcdir)/misc/libutils \
	$(TCLHDIR)

exlbrowser_SOURCES = exlbrowser.c
exlbrowser_LDADD = $(toolbox)/libsnptools.a \
		$(common)/libcommon.a \
		$(top_builddir)/misc/libutils/libutils.a \
		$(TCL_BUILD_LIB_SPEC) $(TCL_LIBS)

exlbrowser_DEPENDENCIES = \
			$(common)/libcommon.a \
			$(toolbox)/libsnptools.a \
			$(top_builddir)/misc/libutils/libutils.a

# Used to regenerate .c from .l file
flexgen:
	cp $(srcdir)/exlbrowser.l .
	$(LEX) -t $(LFLAGS) exlbrowser.l > $(srcdir)/exlbrowser.c
	rm exlbrowser.l

