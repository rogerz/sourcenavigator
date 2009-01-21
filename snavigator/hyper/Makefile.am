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

# FIXME some of these aren't the right thing.  Eg digging into the Tk
# directory is bad.  For instance, why do we do -Itk/unix and
# -Itk/win?  Because tkTreeTable includes default.h, which relies on a
# file in one of those directories (depending on the build).  Luckily,
# the files have different names.

if MSVC
LINK = link -DEBUG $(LDFLAGS) -OUT:$@
TCL_DIR = 
else
LINK = $(PURIFY) $(CC) $(LDFLAGS) -o $@
endif

HYPER_CFLAGS=@HYPER_CFLAGS@

LibUtilsSrc=$(top_srcdir)/misc/libutils
LibUtilsDir=../misc/libutils
LibUtils=$(LibUtilsDir)/libutils.a
DBUtilsLibDir=../db
DBUtilsLib=$(DBUtilsLibDir)/libdbutils.a

INCLUDES = $(HYPER_CFLAGS) $(WINFLAGS) -I$(LibUtilsSrc) \
$(TKHDIR) $(TKHDIR)/../unix $(TKHDIR)/../win \
$(TCLHDIR) $(TK_XINCLUDES) \
-I$(top_srcdir)/../itcl/itcl/generic -I$(top_srcdir)/../itcl/itk/generic \
-I$(DB_INCDIR) -I$(DB_LIBDIR) -I$(top_srcdir)/db -I$(top_srcdir)/../libgui/src \
-I$(top_srcdir)/misc

sdkincludedir = $(datadir)/sdk/include
sdkinclude_HEADERS = sn.h 

libexec_PROGRAMS = hyper

hyper_SOURCES = compare.h compare.c dbsym.c exint.c \
recudir.c services.c sninit.c tclxlist.c tkTreeTable.c tcl_high.c \
c_high.c py_high.c ch_high.c j_high.c highlight.c tclfileutils.c

hyper_LDFLAGS = $(WIN32LDAPP)
if MSVC
hyper_LDADD = @HYPER_MAIN@ \
$(DBUtilsLibDir)/libdbutils.a $(DB_LIBDIR)/libdb.a \
$(LibUtils) \
$(top_builddir)/../libgui/src/libgui.a \
$(WIN32LIBS) \
$(ITCL_BUILD_LIB_SPEC) $(ITK_BUILD_LIB_SPEC) \
$(TIX_BUILD_LIB_SPEC) \
$(TK_BUILD_LIB_SPEC) $(TCL_BUILD_LIB_SPEC)
else
hyper_LDADD = @HYPER_MAIN@ \
-L$(DBUtilsLibDir) -ldbutils -L$(DB_LIBDIR) -ldb \
-L$(LibUtilsDir) -lutils \
-L$(top_builddir)/../libgui/src -lgui \
$(WIN32LIBS) \
$(ITCL_BUILD_LIB_SPEC) $(ITK_BUILD_LIB_SPEC) \
$(TIX_BUILD_LIB_SPEC) $(TK_BUILD_LIB_SPEC) \
$(TCL_BUILD_LIB_SPEC) $(TK_LIBS)
endif
hyper_DEPENDENCIES = @HYPER_MAIN@ \
$(DBUtilsLib) $(DB_LIBDIR)/libdb.a \
$(LibUtils) \
$(TCL_LIB_FULL_PATH) $(TK_LIB_FULL_PATH) \
$(ITCL_LIB_FULL_PATH) $(ITK_LIB_FULL_PATH)

# Must pass TK_DEFS to these compiles.  This is the easiest (and
# arguably best) way to let these files correctly use the Tk internal
# headers.  Of course, they really shouldn't use these headers, but
# that's another problem.

tkTreeTable.$(OBJEXT): tkTreeTable.c tkTreeTable.h
	$(COMPILE) $(TK_DEFS) -c $<

# This target will regenerate .c files from .l files. it
# must never depend on a timestamp for .l files because
# this would fail to compile on an end users system that
# did not have flex installed if the timestamps got hosed.
# Note that we copy to the local dir so that include
# directives include only the file name and not a path.
flexgen:
	cp $(srcdir)/j_high.l .
	cp $(srcdir)/py_high.l .
	cp $(srcdir)/c_high.l .
	cp $(srcdir)/ch_high.l .
	cp $(srcdir)/tcl_high.l .
	$(LEX) -t $(LFLAGS) -Pjava_highlight_ j_high.l > $(srcdir)/j_high.c
	$(LEX) -t $(LFLAGS) -Ppy_highlight_ py_high.l > $(srcdir)/py_high.c
	$(LEX) -t $(LFLAGS) -Pc_highlight_ c_high.l > $(srcdir)/c_high.c
	$(LEX) -t $(LFLAGS) -Pch_highlight_ ch_high.l  > $(srcdir)/ch_high.c
	$(LEX) -t $(LFLAGS) -Ptcl_highlight_ tcl_high.l > $(srcdir)/tcl_high.c
	rm j_high.l py_high.l c_high.l ch_high.l tcl_high.l

highlight.$(OBJEXT): highlight.c
	$(COMPILE) $(TK_DEFS) -DTK_DBGX=\"$(TK_DBGX)\" -c $<

sninit.$(OBJEXT): sninit.c
	$(COMPILE) $(TK_DEFS) -c $<

tclfileutils.$(OBJEXT): tclfileutils.c
	$(COMPILE) $(TK_DEFS) -c $<

# Only use cygpath when compiling with VC++

winMain.obj: win/winMain.c
	$(COMPILE) -c `cygpath -w $<`

winMain.o: win/winMain.c
	$(COMPILE) -c $<

