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

## Process this file with automake to create .in.

bin_PROGRAMS = tbrowser tclparser

INCLUDES = $(WINFLAGS) -I$(top_srcdir)/hyper -I$(top_srcdir)/misc \
	-I$(top_srcdir)/parsers/libjcreuse $(TCLHDIR) -I$(ANTLR_H) \
	-I$(top_srcdir)/parsers/toolbox

AFLAGS = -CC -mrhoist off
DFLAGS = -C2 -i -CC

tbrowser_SOURCES = Scanner.c Parser.c Tree.c Eval.c Trafo.c itcl.c skeleton.c

tbrowser_LDADD = ../libjcreuse/libjcreuse.a $(top_builddir)/db/libdbutils.a \
	$(top_builddir)/misc/libutils/libutils.a $(DB_LIBDIR)/libdb.a \
	$(TCL_BUILD_LIB_SPEC) $(TCL_LIBS) 

tbrowser_DEPENDENCIES = ../libjcreuse/libjcreuse.a \
	$(top_builddir)/db/libdbutils.a \
	$(top_builddir)/misc/libutils/libutils.a \
	$(DB_LIBDIR)/libdb.a

#
# The all new (not quite doing Xref yet) tcl parser.
#

tclparser_SOURCES = tcl.cpp \
	sntoolkit.cpp \
	colToken.cpp \
	myPanic.cpp \
	myTokenBuffer.cpp \
	noleakTok.cpp \
	TCLParser.cpp \
	$(ANTLR_H)/AParser.cpp \
	$(ANTLR_H)/DLexerBase.cpp \
	$(ANTLR_H)/ATokenBuffer.cpp \
	DLGLexer.cpp 

tclparser_LDADD = $(top_builddir)/parsers/toolbox/libsnptools.a \
	$(TCL_BUILD_LIB_SPEC) $(TCL_LIBS) \
	$(top_builddir)/db/libdbutils.a \
	$(top_builddir)/misc/libutils/libutils.a \
	$(DB_LIBDIR)/libdb.a


tclparser_DEPENDENCIES = $(top_builddir)/parsers/toolbox/libsnptools.a \
	$(top_builddir)/db/libdbutils.a \
	$(top_builddir)/misc/libutils/libutils.a

tclparser_CFLAGS = -I$(ANTLR_H)

ANTLR_SPAWN = tcl.cpp TCLParser.cpp \
	TCLParser.h parser.dlg tckens.h

DLG_SPAWN = DLGLexer.cpp DLGLexer.h

if USE_ANTLR
$(ANTLR_SPAWN) : tcl.g
	$(ANTLR) $(AFLAGS) tcl.g

$(DLG_SPAWN) : parser.dlg
	$(DLG) $(DFLAGS) parser.dlg

endif


