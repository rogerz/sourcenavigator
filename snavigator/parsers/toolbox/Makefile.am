AUTOMAKE_OPTIONS = cygnus

## Process this file with automake to create Makefile.in.

INCLUDES = $(WINFLAGS) $(TCLHDIR) -I$(top_srcdir)/hyper \
	-I$(top_srcdir)/parsers/common

libsnptools_a_SOURCES = snptools.c snptools.h

sdkincludedir = $(datadir)/sdk/include
sdkinclude_HEADERS = snptools.h

sdklibdir = @SN_SDK_EXEC_PREFIX@/lib
sdklib_LIBRARIES = libsnptools.a
