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

INCLUDES = $(WINFLAGS) -I$(top_srcdir)/misc/libutils -I$(top_srcdir)/hyper \
		-I$(DB_INCDIR) -I$(DB_LIBDIR) $(TCLHDIR) -I$(top_srcdir)/misc

if MSVC
LINK = link -DEBUG $(LDFLAGS) -OUT:$@
else
LINK = $(PURIFY) $(CC) $(LDFLAGS) -o $@
endif

sdkdir = @SN_SDK_EXEC_PREFIX@/lib

libexec_PROGRAMS = dbcp dbdump dbimp dbcompact
sdk_LIBRARIES = libdbutils.a

install-data-local:
	$(INSTALL_DATA) $(top_builddir)/../db4/PORT/libdb.a $(DESTDIR)$(sdkdir)
	$(RANLIB) $(DESTDIR)$(sdkdir)/libdb.a

dbcp_SOURCES = dbcp.c
dbcp_LDADD = libdbutils.a $(DB_LIBDIR)/libdb.a
dbcp_DEPENDENCIES = libdbutils.a $(DB_LIBDIR)/libdb.a

dbdump_SOURCES = dbdump.c
dbdump_LDADD = libdbutils.a $(DB_LIBDIR)/libdb.a \
		$(top_builddir)/misc/libutils/libutils.a $(TCL_BUILD_LIB_SPEC) $(TCL_LIBS)
dbdump_DEPENDENCIES = libdbutils.a $(DB_LIBDIR)/libdb.a \
		$(top_builddir)/misc/libutils/libutils.a

dbimp_SOURCES = dbimp.c
if MSVC
dbimp_LDADD = libdbutils.a $(DB_LIBDIR)/libdb.a \
		$(top_builddir)/misc/libutils/libutils.a \
		$(TCL_BUILD_LIB_SPEC) $(TCL_LIBS) wsock32.lib
else
dbimp_LDADD = -L. -ldbutils -L$(DB_LIBDIR) -ldb \
		$(top_builddir)/misc/libutils/libutils.a \
		$(TCL_BUILD_LIB_SPEC) $(TCL_LIBS) @SOCKLIB@
endif
dbimp_DEPENDENCIES = libdbutils.a $(DB_LIBDIR)/libdb.a \
		$(top_builddir)/misc/libutils/libutils.a

dbcompact_SOURCES = dbcompact.c
dbcompact_LDADD = $(DB_LIBDIR)/libdb.a
dbcompact_DEPENDENCIES = $(DB_LIBDIR)/libdb.a

libdbutils_a_SOURCES = dbutils.c dbutils.h
