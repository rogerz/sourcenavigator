#!/bin/sh

# Run this program (./autogen.sh) after changing any of
# the files that are used to automatically generate
# other files. This includes:
# Makefile.am
# acinclude.m4
# configure.in

aclocal
autoheader

if (automake --version | grep "GNU automake.*1\.4" > /dev/null) ; then
    automake --cygnus
elif (automake --version | grep "GNU automake.*1\.6" > /dev/null) ; then
    automake --cygnus --ignore-deps
fi

autoconf

