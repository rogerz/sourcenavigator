#------------------------------------------------------------------------
# SC_PATH_TCLCONFIG --
#
#	Locate the tclConfig.sh file and perform a sanity check on
#	the Tcl compile flags
#	Currently a no-op for Windows
#
# Arguments:
#	PATCH_LEVEL	The patch level for Tcl if any.
#
# Results:
#
#	Adds the following arguments to configure:
#		--with-tcl=...
#
#	Sets the following vars:
#		TCL_BIN_DIR	Full path to the tclConfig.sh file
#------------------------------------------------------------------------

AC_DEFUN(SC_PATH_TCLCONFIG, [
    AC_MSG_CHECKING([the location of tclConfig.sh])

# CYGNUS LOCAL
    if test -d ../../tcl8.1/win;  then
	TCL_BIN_DIR_DEFAULT=../../tcl8.1/win
    else
	TCL_BIN_DIR_DEFAULT=../../tcl/win
    fi
# END CYGNUS LOCAL
    
    AC_ARG_WITH(tcl, [  --with-tcl=DIR          use Tcl 8.3 binaries from DIR],
	    TCL_BIN_DIR=$withval, TCL_BIN_DIR=`cd $TCL_BIN_DIR_DEFAULT; pwd`)
    if test ! -d $TCL_BIN_DIR; then
	AC_MSG_ERROR(Tcl directory $TCL_BIN_DIR does not exist)
    fi
    if test ! -f $TCL_BIN_DIR/tclConfig.sh; then
	AC_MSG_ERROR(There is no tclConfig.sh in $TCL_BIN_DIR:  perhaps you did not specify the Tcl *build* directory (not the toplevel Tcl directory) or you forgot to configure Tcl?)
    fi
    AC_MSG_RESULT($TCL_BIN_DIR/tclConfig.sh)
])

#------------------------------------------------------------------------
# SC_PATH_TKCONFIG --
#
#	Locate the tkConfig.sh file
#	Currently a no-op for Windows
#
# Arguments:
#	none
#
# Results:
#
#	Adds the following arguments to configure:
#		--with-tk=...
#
#	Sets the following vars:
#		TK_BIN_DIR	Full path to the tkConfig.sh file
#------------------------------------------------------------------------

AC_DEFUN(SC_PATH_TKCONFIG, [
    AC_MSG_CHECKING([the location of tkConfig.sh])

    if test -d ../../tk8.3$1/win;  then
	TK_BIN_DIR_DEFAULT=../../tk8.3$1/win
    else
	TK_BIN_DIR_DEFAULT=../../tk8.3/win
    fi
    
    AC_ARG_WITH(tk, [  --with-tk=DIR          use Tk 8.3 binaries from DIR],
	    TK_BIN_DIR=$withval, TK_BIN_DIR=`cd $TK_BIN_DIR_DEFAULT; pwd`)
    if test ! -d $TK_BIN_DIR; then
	AC_MSG_ERROR(Tk directory $TK_BIN_DIR does not exist)
    fi
    if test ! -f $TK_BIN_DIR/tkConfig.sh; then
	AC_MSG_ERROR(There is no tkConfig.sh in $TK_BIN_DIR:  perhaps you did not specify the Tk *build* directory (not the toplevel Tk directory) or you forgot to configure Tk?)
    fi

    AC_MSG_RESULT([$TK_BIN_DIR/tkConfig.sh])
])

#------------------------------------------------------------------------
# SC_LOAD_TCLCONFIG --
#
#	Load the tclConfig.sh file
#	Currently a no-op for Windows
#
# Arguments:
#	
#	Requires the following vars to be set:
#		TCL_BIN_DIR
#
# Results:
#
#	Subst the following vars:
#		TCL_BIN_DIR
#		TCL_SRC_DIR
#		TCL_LIB_FILE
#
#------------------------------------------------------------------------

AC_DEFUN(SC_LOAD_TCLCONFIG, [
    AC_MSG_CHECKING([for existence of $TCL_BIN_DIR/tclConfig.sh])

    if test -f "$TCL_BIN_DIR/tclConfig.sh" ; then
        AC_MSG_RESULT([loading])
	. $TCL_BIN_DIR/tclConfig.sh
    else
        AC_MSG_RESULT([file not found])
    fi

    # The eval is required to do the TCL_DBGX substitution in the
    # TCL_LIB_FILE variable.

    eval TCL_LIB_FILE=${TCL_LIB_FILE}
    eval TCL_LIB_FLAG=${TCL_LIB_FLAG}

    AC_SUBST(TCL_BIN_DIR)
    AC_SUBST(TCL_SRC_DIR)
    AC_SUBST(TCL_LIB_FILE)
])

#------------------------------------------------------------------------
# SC_LOAD_TKCONFIG --
#
#	Load the tkConfig.sh file
#	Currently a no-op for Windows
#
# Arguments:
#	
#	Requires the following vars to be set:
#		TK_BIN_DIR
#
# Results:
#
#	Sets the following vars that should be in tkConfig.sh:
#		TK_BIN_DIR
#------------------------------------------------------------------------

AC_DEFUN(SC_LOAD_TKCONFIG, [
    AC_MSG_CHECKING([for existence of $TCLCONFIG])

    if test -f "$TK_BIN_DIR/tkConfig.sh" ; then
        AC_MSG_CHECKING([loading $TK_BIN_DIR/tkConfig.sh])
	. $TK_BIN_DIR/tkConfig.sh
    else
        AC_MSG_RESULT([could not find $TK_BIN_DIR/tkConfig.sh])
    fi


    AC_SUBST(TK_BIN_DIR)
    AC_SUBST(TK_SRC_DIR)
    AC_SUBST(TK_LIB_FILE)
])

#------------------------------------------------------------------------
# SC_ENABLE_SHARED --
#
#	Allows the building of shared libraries
#
# Arguments:
#	none
#	
# Results:
#
#	Adds the following arguments to configure:
#		--enable-shared=yes|no
#
#	Defines the following vars:
#		STATIC_BUILD	Used for building import/export libraries
#				on Windows.
#
#	Sets the following vars:
#		SHARED_BUILD	Value of 1 or 0
#------------------------------------------------------------------------

AC_DEFUN(SC_ENABLE_SHARED, [
    AC_MSG_CHECKING([how to build libraries])
    AC_ARG_ENABLE(shared,
	[  --enable-shared         build and link with shared libraries [--enable-shared]],
    [tcl_ok=$enableval], [tcl_ok=yes])

    if test "${enable_shared+set}" = set; then
	enableval="$enable_shared"
	tcl_ok=$enableval
    else
	tcl_ok=yes
    fi

    if test "$tcl_ok" = "yes" ; then
	AC_MSG_RESULT([shared])
	SHARED_BUILD=1
    else
	AC_MSG_RESULT([static])
	SHARED_BUILD=0
	AC_DEFINE(STATIC_BUILD)
    fi
])

#------------------------------------------------------------------------
# SC_ENABLE_THREADS --
#
#	Specify if thread support should be enabled
#
# Arguments:
#	none
#	
# Results:
#
#	Adds the following arguments to configure:
#		--enable-threads=yes|no
#
#	Defines the following vars:
#		TCL_THREADS
#------------------------------------------------------------------------

AC_DEFUN(SC_ENABLE_THREADS, [
    AC_MSG_CHECKING(for building with threads)
    AC_ARG_ENABLE(threads, [  --enable-threads        build with threads],
	[tcl_ok=$enableval], [tcl_ok=no])

    if test "$tcl_ok" = "yes"; then
	AC_MSG_RESULT(yes)
	TCL_THREADS=1
	AC_DEFINE(TCL_THREADS)
    else
	TCL_THREADS=0
	AC_MSG_RESULT([no (default)])
    fi
])

#------------------------------------------------------------------------
# SC_ENABLE_SYMBOLS --
#
#	Specify if debugging symbols should be used
#	Memory (TCL_MEM_DEBUG) and compile (TCL_COMPILE_DEBUG) debugging
#	can also be enabled.
#
# Arguments:
#	none
#	
#	Requires the following vars to be set in the Makefile:
#		CFLAGS_DEBUG
#		CFLAGS_OPTIMIZE
#		LDFLAGS_DEBUG
#		LDFLAGS_OPTIMIZE
#	
# Results:
#
#	Adds the following arguments to configure:
#		--enable-symbols
#
#	Defines the following vars:
#		CFLAGS_DEFAULT	Set to $(CFLAGS_DEBUG) if true
#				Set to $(CFLAGS_OPTIMIZE) if false
#		LDFLAGS_DEFAULT	Set to $(LDFLAGS_DEBUG) if true
#				Set to $(LDFLAGS_OPTIMIZE) if false
#		DBGX		Debug library extension
#
#------------------------------------------------------------------------

AC_DEFUN(SC_ENABLE_SYMBOLS, [
    AC_MSG_CHECKING([for build with symbols])
    AC_ARG_ENABLE(symbols, [  --enable-symbols        build with debugging symbols [--disable-symbols]],    [tcl_ok=$enableval], [tcl_ok=no])

    if test "$tcl_ok" = "no"; then
	CFLAGS_DEFAULT='$(CFLAGS_OPTIMIZE)'
	LDFLAGS_DEFAULT='$(LDFLAGS_OPTIMIZE)'
	DBGX=""
	AC_MSG_RESULT([no])
    else
	CFLAGS_DEFAULT='$(CFLAGS_DEBUG)'
	LDFLAGS_DEFAULT='$(LDFLAGS_DEBUG)'
	DBGX=d
	if test "$tcl_ok" = "yes"; then
	    AC_MSG_RESULT([yes (standard debugging)])
	fi
    fi
    AC_SUBST(CFLAGS_DEFAULT)
    AC_SUBST(LDFLAGS_DEFAULT)

    if test "$tcl_ok" = "mem" -o "$tcl_ok" = "all"; then
	AC_DEFINE(TCL_MEM_DEBUG)
    fi

    if test "$tcl_ok" = "compile" -o "$tcl_ok" = "all"; then
	AC_DEFINE(TCL_COMPILE_DEBUG)
	AC_DEFINE(TCL_COMPILE_STATS)
    fi

    if test "$tcl_ok" != "yes" -a "$tcl_ok" != "no"; then
	if test "$tcl_ok" = "all"; then
	    AC_MSG_RESULT([enabled symbols mem compile debugging])
	else
	    AC_MSG_RESULT([enabled $tcl_ok debugging])
	fi
    fi
])

#--------------------------------------------------------------------
# SC_CONFIG_CFLAGS
#
#	Try to determine the proper flags to pass to the compiler
#	for building shared libraries and other such nonsense.
#
#	NOTE: The backslashes in quotes below are substituted twice
#	due to the fact that they are in a macro and then inlined
#	in the final configure script.
#
# Arguments:
#	none
#
# Results:
#
#	Can set the following vars:
#		EXTRA_CFLAGS
#		CFLAGS_DEBUG
#		CFLAGS_OPTIMIZE
#		CFLAGS_WARNING
#		LDFLAGS_DEBUG
#		LDFLAGS_OPTIMIZE
#		LDFLAGS_CONSOLE
#		LDFLAGS_WINDOW
#		CC_OBJNAME
#		CC_EXENAME
#		CYGPATH
#		STLIB_LD
#		SHLIB_LD
#		SHLIB_LD_LIBS
#		LIBS
#		AR
#		RC
#		RES
#
#		MAKE_LIB
#		MAKE_EXE
#		MAKE_DLL
#
#		LIBSUFFIX
#		LIBPREFIX
#		VENDORPREFIX
#		LIBRARIES
#		EXESUFFIX
#		DLLSUFFIX
#
#--------------------------------------------------------------------

AC_DEFUN(SC_CONFIG_CFLAGS, [
    TCL_LIB_VERSIONS_OK=nodots

    AC_CHECK_PROG(CYGPATH, cygpath, cygpath -w, echo)

    # Check for a bug in gcc's windres that causes the
    # compile to fail when a Windows native path is
    # passed into windres. The mingw toolchain requires
    # Windows native paths while Cygwin should work
    # with both. Avoid the bug by passing a POSIX
    # path when using the Cygwin toolchain.

    if test "$GCC" = "yes" && test "$CYGPATH" != "echo" ; then
        conftest=/tmp/conftest.rc
        echo "STRINGTABLE BEGIN" > $conftest
        echo "101 \"name\"" >> $conftest
        echo "END" >> $conftest

        AC_MSG_CHECKING([for Windows native path bug in windres])
        cyg_conftest=`$CYGPATH $conftest`
        if AC_TRY_COMMAND($RC -o conftest.res.o $cyg_conftest) ; then
            AC_MSG_RESULT([no])
        else
            AC_MSG_RESULT([yes])
            CYGPATH=echo
        fi
        conftest=
        cyg_conftest=
    fi

    if test "$CYGPATH" = "echo" || test "$ac_cv_cygwin" = "yes"; then
        DEPARG='"$<"'
    else
        DEPARG='"$(shell $(CYGPATH) $<)"'
    fi

    VENDORPREFIX="rh"

    # set various compiler flags depending on whether we are using gcc or cl

    AC_MSG_CHECKING([compiler flags])
    if test "${GCC}" = "yes" ; then

	# CYGNUS LOCAL
	if test "$ac_cv_cygwin" = "yes" ; then
	    VENDORPREFIX="cyg"
	fi

	SHLIB_LD=""
	SHLIB_LD_LIBS=""
	LIBS=""
	LIBS_GUI="-lgdi32 -lcomdlg32"
	STLIB_LD="${AR} cr"
	RC_OUT=-o
	RC_TYPE=
	RC_INCLUDE=--include
	RES=res.o
	MAKE_LIB="\${STLIB_LD} \[$]@"
	POST_MAKE_LIB="\${RANLIB} \[$]@"
	MAKE_EXE="\${CC} -o \[$]@"
	LIBPREFIX="lib${VENDORPREFIX}"

	if test "${SHARED_BUILD}" = "0" ; then
	    # static
            AC_MSG_RESULT([using static flags])
	    runtime=
	    MAKE_DLL="echo "
	    LIBSUFFIX="s\${DBGX}.a"
	    LIBRARIES="\${STATIC_LIBRARIES}"
	    EXESUFFIX="s\${DBGX}.exe"
	    DLLSUFFIX=""
	else
	    # dynamic
            AC_MSG_RESULT([using shared flags])

	    # ad-hoc check to see if CC supports -shared.
	    if "${CC}" -shared 2>&1 | egrep ': -shared not supported' >/dev/null; then
		AC_MSG_ERROR([${CC} does not support the -shared option.
	        You will need to upgrade to a newer version of the toolchain.])
	    fi

	    runtime=
	    # Link with gcc since ld does not link to default libs like
	    # -luser32 and -lmsvcrt. We also need to add CFLAGS so important
	    # flags like -mno-cygwin get passed in to CC.
	    SHLIB_LD='${CC} -shared ${CFLAGS}'
	    # Add SHLIB_LD_LIBS to the Make rule, not here.
	    MAKE_DLL="\${SHLIB_LD} \$(LDFLAGS) -o \[$]@ ${extra_ldflags} \
	        -Wl,--out-implib,\$(patsubst %.dll,lib%.a,\[$]@)"
	    TK_DLL_BASE="-Wl,--image-base=0x66300000"

	    LIBSUFFIX="\${DBGX}.a"
	    DLLSUFFIX="\${DBGX}.dll"
	    EXESUFFIX="\${DBGX}.exe"
	    LIBRARIES="\${SHARED_LIBRARIES}"
	fi

	CFLAGS_DEBUG=-g
	CFLAGS_OPTIMIZE=-O

        # Don't set an opt level if an option like -O3 was set in CFLAGS
        if echo $CFLAGS | grep '\-O' > /dev/null ; then
            CFLAGS_OPTIMIZE=""
        fi


	CFLAGS_WARNING="-Wall -Wconversion"
	LDFLAGS_DEBUG=
	LDFLAGS_OPTIMIZE=

	# Specify the CC output file names based on the target name
	CC_OBJNAME="-o \[$]@"
	CC_EXENAME="-o \[$]@"

	# Specify linker flags depending on the type of app being 
	# built -- Console vs. Window.
	LDFLAGS_CONSOLE="-mconsole ${extra_ldflags}"
	LDFLAGS_WINDOW="-mwindows ${extra_ldflags}"
    else
	SHLIB_LD="link -dll -nologo"
	SHLIB_LD_LIBS="user32.lib advapi32.lib"
	LIBS="user32.lib advapi32.lib"
	LIBS_GUI="gdi32.lib comdlg32.lib"
	STLIB_LD="lib -nologo"
	RC="rc"
	RC_OUT=-fo
	RC_TYPE=-r
	RC_INCLUDE=-i
	RES=res
	MAKE_LIB="\${STLIB_LD} -out:\[$]@"
	POST_MAKE_LIB=
	MAKE_EXE="\${CC} -Fe\[$]@"
	LIBPREFIX=${VENDORPREFIX}

	if test "${SHARED_BUILD}" = "0" ; then
	    # static
            AC_MSG_RESULT([using static flags])
	    runtime=-MT
	    MAKE_DLL="echo "
	    LIBSUFFIX="s\${DBGX}.lib"
	    LIBRARIES="\${STATIC_LIBRARIES}"
	    EXESUFFIX="s\${DBGX}.exe"
	    DLLSUFFIX=""
	else
	    # dynamic
            AC_MSG_RESULT([using shared flags])
	    runtime=-MD
	    # Add SHLIB_LD_LIBS to the Make rule, not here.
	    MAKE_DLL="\${SHLIB_LD} \$(LDFLAGS) -out:\[$]@"
	    LIBSUFFIX="\${DBGX}.lib"
	    DLLSUFFIX="\${DBGX}.dll"
	    EXESUFFIX="\${DBGX}.exe"
	    LIBRARIES="\${SHARED_LIBRARIES}"
	fi

	EXTRA_CFLAGS="-YX"
	CFLAGS_DEBUG="-nologo -Z7 -Od -WX ${runtime}d"
#	CFLAGS_OPTIMIZE="-nologo -O2 -Gs -GD ${runtime}"
	CFLAGS_OPTIMIZE="-nologo -Oti -Gs -GD ${runtime}"
	CFLAGS_WARNING="-W3"
	LDFLAGS_DEBUG="-debug:full -debugtype:cv"
	LDFLAGS_OPTIMIZE="-release"

	# Specify the CC output file names based on the target name
	CC_OBJNAME="-Fo\[$]@"
	CC_EXENAME="-Fe\"\$(shell \$(CYGPATH) '\[$]@')\""

	# Specify linker flags depending on the type of app being 
	# built -- Console vs. Window.
	LDFLAGS_CONSOLE="-link -subsystem:console"
	LDFLAGS_WINDOW="-link -subsystem:windows"
    fi

    # TCL_LIB_SUFFIX is defined here and in tclConfig.sh so that macros
    # can use a single variable name for both Tcl and extensions.
    TCL_LIB_SUFFIX=$LIBSUFFIX
])

#------------------------------------------------------------------------
# SC_WITH_TCL --
#
#	Location of the Tcl build directory.
#
# Arguments:
#	none
#
# Results:
#
#	Adds the following arguments to configure:
#		--with-tcl=...
#
#	Defines the following vars:
#		TCL_BIN_DIR	Full path to the tcl build dir.
#------------------------------------------------------------------------

AC_DEFUN(SC_WITH_TCL, [
    if test -d ../../tcl8.3$1/win;  then
	TCL_BIN_DEFAULT=../../tcl8.3$1/win
    else
	TCL_BIN_DEFAULT=../../tcl8.3/win
    fi
    
    AC_ARG_WITH(tcl, [  --with-tcl=DIR          use Tcl 8.3 binaries from DIR],
	    TCL_BIN_DIR=$withval, TCL_BIN_DIR=`cd $TCL_BIN_DEFAULT; pwd`)
    if test ! -d $TCL_BIN_DIR; then
	AC_MSG_ERROR(Tcl directory $TCL_BIN_DIR does not exist)
    fi
    if test ! -f $TCL_BIN_DIR/Makefile; then
	AC_MSG_ERROR(There is no Makefile in $TCL_BIN_DIR:  perhaps you did not specify the Tcl *build* directory (not the toplevel Tcl directory) or you forgot to configure Tcl?)
    else
	echo "building against Tcl binaries in: $TCL_BIN_DIR"
    fi
    AC_SUBST(TCL_BIN_DIR)
])

#--------------------------------------------------------------------
# SC_TIME_HANLDER
#
#	Checks how the system deals with time.h, what time structures
#	are used on the system, and what fields the structures have.
#
# Arguments:
#	none
#	
# Results:
#
#	Defines some of the following vars:
#		USE_DELTA_FOR_TZ
#		HAVE_TM_GMTOFF
#		HAVE_TM_TZADJ
#		HAVE_TIMEZONE_VAR
#
#--------------------------------------------------------------------

AC_DEFUN(SC_TIME_HANDLER, [
    AC_CHECK_HEADERS(sys/time.h)
    AC_HEADER_TIME
    AC_STRUCT_TIMEZONE

    AC_MSG_CHECKING([tm_tzadj in struct tm])
    AC_TRY_COMPILE([#include <time.h>], [struct tm tm; tm.tm_tzadj;],
	    [AC_DEFINE(HAVE_TM_TZADJ)
	    AC_MSG_RESULT(yes)],
	    AC_MSG_RESULT(no))

    AC_MSG_CHECKING([tm_gmtoff in struct tm])
    AC_TRY_COMPILE([#include <time.h>], [struct tm tm; tm.tm_gmtoff;],
	    [AC_DEFINE(HAVE_TM_GMTOFF)
	    AC_MSG_RESULT(yes)],
	    AC_MSG_RESULT(no))

    #
    # Its important to include time.h in this check, as some systems
    # (like convex) have timezone functions, etc.
    #
    have_timezone=no
    AC_MSG_CHECKING([long timezone variable])
    AC_TRY_COMPILE([#include <time.h>],
	    [extern long timezone;
	    timezone += 1;
	    exit (0);],
	    [have_timezone=yes
	    AC_DEFINE(HAVE_TIMEZONE_VAR)
	    AC_MSG_RESULT(yes)],
	    AC_MSG_RESULT(no))

    #
    # On some systems (eg IRIX 6.2), timezone is a time_t and not a long.
    #
    if test "$have_timezone" = no; then
    AC_MSG_CHECKING([time_t timezone variable])
    AC_TRY_COMPILE([#include <time.h>],
	    [extern time_t timezone;
	    timezone += 1;
	    exit (0);],
	    [AC_DEFINE(HAVE_TIMEZONE_VAR)
	    AC_MSG_RESULT(yes)],
	    AC_MSG_RESULT(no))
    fi

    #
    # On some systems (eg Solaris 2.5.1), timezone is not declared in
    # time.h unless you jump through hoops.  Instead of that, we just
    # declare it ourselves when necessary.
    #
    if test "$have_timezone" = yes; then
   	AC_MSG_CHECKING(for timezone declaration)
   	changequote(<<,>>)
   	tzrx='^[ 	]*extern.*timezone'
   	changequote([,])
   	AC_EGREP_HEADER($tzrx, time.h, [
     	AC_DEFINE(HAVE_TIMEZONE_DECL)
     	AC_MSG_RESULT(found)], AC_MSG_RESULT(missing))
    fi

    #
    # AIX does not have a timezone field in struct tm. When the AIX bsd
    # library is used, the timezone global and the gettimeofday methods are
    # to be avoided for timezone deduction instead, we deduce the timezone
    # by comparing the localtime result on a known GMT value.
    #

    if test "`uname -s`" = "AIX" ; then
	AC_CHECK_LIB(bsd, gettimeofday, libbsd=yes)
	if test $libbsd = yes; then
	    AC_DEFINE(USE_DELTA_FOR_TZ)
	fi
    fi
])

