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

## This directory is only used when building for Windows.

if WINDOWS
bin_PROGRAMS = snavigator snsdk snpdbg
snavigator_SOURCES = snavigator.c
snavigator_LDFLAGS = $(WIN32LDAPP)
snavigator_LDADD = snavres.$(OBJEXT) $(WIN32LIBS)
snpdbg_SOURCES = snpdbg.c
snpdbg_LDFLAGS = $(WIN32LDAPP)
snpdbg_LDADD = $(WIN32LIBS)
snsdk_SOURCES = winCon.c
snsdk_LDADD = $(WIN32LIBS)
snsdk_CFLAGS =  -DAPPNAME=\"..\\\\bin\\\\hyper.exe\"


winCon.$(OBJEXT): winCon.c
	$(COMPILE) $(snsdk_CFLAGS) -c $<

if MSVC
snavres.$(OBJEXT): snavigator.rc
	cp $(srcdir)/../../bitmaps/sourcenav.ico .
	rc -i$(srcdir) -fosnavres.$(OBJEXT) $(srcdir)/snavwindres.rc
else
snavres.$(OBJEXT): snavigator.rc
	cp $(srcdir)/../../bitmaps/sourcenav.ico .
	$(WINDRES) --include $(srcdir) $(srcdir)/snavwindres.rc snavres.$(OBJEXT)
endif

else
bin_PROGRAMS=
endif

