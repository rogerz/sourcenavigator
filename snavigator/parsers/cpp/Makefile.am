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

common = $(top_builddir)/parsers/common

SUBDIRS = cpplib

INCLUDES = $(WINFLAGS) -I$(top_srcdir)/hyper -I$(srcdir)/cpplib \
	-I$(top_srcdir)/db -I$(top_srcdir)/misc/libutils -I$(DB_INCDIR) \
	-I$(DB_LIBDIR) -I$(top_srcdir)/parsers/common $(TCLHDIR)

libexec_PROGRAMS = cbrowser cbrowser2

cbrowser_SOURCES = cparser.c cpdefines.h skeleton.c

cbrowser_LDADD = cpplib/cpkeyw.$(OBJEXT) cpplib/cplex.$(OBJEXT) \
	cpplib/macro.$(OBJEXT) cpplib/macread.$(OBJEXT) \
	cpplib/token.$(OBJEXT) cpplib/process.$(OBJEXT) \
	cpplib/expr.$(OBJEXT) \
	$(common)/libcommon.a \
	$(top_builddir)/db/libdbutils.a \
	$(top_builddir)/misc/libutils/libutils.a \
	$(DB_LIBDIR)/libdb.a \
	$(TCL_BUILD_LIB_SPEC) $(TCL_LIBS)

cbrowser_DEPENDENCIES = cpplib/cpkeyw.$(OBJEXT) \
	cpplib/cplex.$(OBJEXT) cpplib/macro.$(OBJEXT) \
	cpplib/macread.$(OBJEXT) cpplib/token.$(OBJEXT) \
	cpplib/process.$(OBJEXT) cpplib/expr.$(OBJEXT) \
	$(common)/libcommon.a \
	$(top_builddir)/db/libdbutils.a \
	$(top_builddir)/misc/libutils/libutils.a \
	$(DB_LIBDIR)/libdb.a

cbrowser2_SOURCES = cbrowser2.c

cbrowser2_LDADD = cpplib/libcpp2.a \
	$(common)/libcommon.a \
	$(top_builddir)/db/libdbutils.a \
	$(top_builddir)/misc/libutils/libutils.a \
	$(DB_LIBDIR)/libdb.a \
	$(TCL_BUILD_LIB_SPEC) $(TCL_LIBS)

cbrowser2_DEPENDENCIES = cpplib/libcpp2.a \
	$(common)/libcommon.a \
	$(top_builddir)/db/libdbutils.a \
	$(top_builddir)/misc/libutils/libutils.a \
	$(DB_LIBDIR)/libdb.a
