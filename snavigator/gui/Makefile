# This makefile should be used by a developer to
# regenerate the tclIndex file in the src dir.
# It assumes that a working install of hyper
# is on the PATH.

all:
	rm -f tclIndex
	echo "auto_mkindex . ; exit" > mkindex.tcl
	hyper mkindex.tcl
	rm -f mkindex.tcl
	chmod 644 tclIndex
