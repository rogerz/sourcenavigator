# ------------------------------------------------------------------------
#       C Compiler options 
# ------------------------------------------------------------------------

DEFINES      = 
CFLAGS       = -g
CC 	     = gcc
CPLUS = g++

# ------------------------------------------------------------------------
#       Include directives for Tcl, Tk, and X include files 
# ------------------------------------------------------------------------

# TCL_INCLUDE   = -I/usr/src/local/tcl7.3
# TK_INCLUDE    = -I/usr/src/local/tk3.6
X_INCLUDE     = -I/usr/include/X11

# ------------------------------------------------------------------------
#       Libraries directives for Tcl, Tk, X11, and BLT
# ------------------------------------------------------------------------

GLISH_LIB = ../$(ARCH)/libglish.a -L$(ISTKPLACE)/lib/$(ARCH) -lsds

X_LIB        =  -lX11
# TK_LIB       = -L/usr/src/local/tk3.6 -ltk
# TCL_LIB      = -L/usr/src/local/tcl7.3 -ltcl
TK_LIB       = -ltk
TCL_LIB      = -ltcl
EXTRA_LIBS   =  -lm


INCLUDES     = $(TK_INCLUDE) $(TCL_INCLUDE) $(X_INCLUDE)
CC_SWITCHES  = $(CFLAGS) $(DEFINES) $(INCLUDES)
LIBRARIES    = $(GLISH_LIB) $(TK_LIB) $(TCL_LIB) $(X_LIB) $(EXTRA_LIBS)

OBJS =	main.o TK_Client.o

tkclient: $(OBJS)
	$(CPLUS) $(CC_SWITCHES) $(OBJS) -o tkclient $(LIBRARIES)

main.o: main.cc TK_Client.h
	$(CPLUS) -I../include $(CC_SWITCHES) -c main.cc

TK_Client.o: TK_Client.cc TK_Client.h
	$(CPLUS) -I../include $(CC_SWITCHES) -c TK_Client.cc

clean:
	$(RM) tkclient *.o
