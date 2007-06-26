#!/bin/sh

# Run this program (./autogen.sh) after changing any of
# the files that are used to automatically generate
# other files. This includes:
#   Makefile.am
#   acinclude.m4
#   configure.in

# Input Files
#   acinclude.m4
#   configure.in
aclocal
# Output File
#   aclocal.m4

# Input Files
#   aclocal.m4
#   configure.in
autoheader
# Output File
#   config.h.in

# Input Files
#   configure.in
#   Makefile.am
if (automake --version | grep "GNU automake.*1\.4" > /dev/null) ; then
    automake --cygnus
elif (automake --version | grep "GNU automake.*1\.6" > /dev/null) ; then
    automake --cygnus --ignore-deps
else
    automake --cygnus --ignore-deps --add-missing --copy
fi
# Output File
#   Makefile.in

# Input Files
#   aclocal.m4
#   configure.in
autoconf
# Output File
#   configure

