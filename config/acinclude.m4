dnl This file is included into all any other acinclude file that needs
dnl to use these macros.


dnl ====================================================================
dnl Ok, lets find the tcl source trees so we can use the headers
dnl Warning: transition of version 9 to 10 will break this algorithm
dnl because 10 sorts before 9. We also look for just tcl. We have to
dnl be careful that we don't match stuff like tclX by accident.
dnl the alternative search directory is involked by --with-tclinclude

AC_DEFUN([CYG_AC_PATH_TCLH], [
dirlist=".. ../../ ../../../ ../../../../ ../../../../../ ../../../../../../ ../../../../../../.. ../../../../../../../.. ../../../../../../../../.. ../../../../../../../../../.."
no_tcl=true
AC_MSG_CHECKING(for Tcl headers in the source tree)
AC_ARG_WITH(tclinclude, [  --with-tclinclude       directory where tcl headers are], with_tclinclude=${withval})
AC_CACHE_VAL(ac_cv_c_tclh,[
dnl first check to see if --with-tclinclude was specified
if test x"${with_tclinclude}" != x ; then
  if test -f ${with_tclinclude}/tcl.h ; then
    ac_cv_c_tclh=`(cd ${with_tclinclude}; ${PWDCMD-pwd})`
  elif test -f ${with_tclinclude}/generic/tcl.h ; then
    ac_cv_c_tclh=`(cd ${with_tclinclude}/generic; ${PWDCMD-pwd})`
  else
    AC_MSG_ERROR([${with_tclinclude} directory doesn't contain headers])
  fi
fi

dnl next check if it came with Tcl configuration file
if test x"${ac_cv_c_tclconfig}" != x ; then
  for i in $dirlist; do
    if test -f $ac_cv_c_tclconfig/$i/generic/tcl.h ; then
      ac_cv_c_tclh=`(cd $ac_cv_c_tclconfig/$i/generic; ${PWDCMD-pwd})`
      break
    fi
  done
fi

dnl next check in private source directory
dnl since ls returns lowest version numbers first, reverse its output
if test x"${ac_cv_c_tclh}" = x ; then
    dnl find the top level Tcl source directory
    for i in $dirlist; do
        if test -n "`ls -dr $srcdir/$i/tcl* 2>/dev/null`" ; then
            dnl find the exact Tcl source dir. We do it this way, cause there
            dnl might be multiple version of Tcl, and we want the most recent one.
            for j in `ls -dr $srcdir/$i/tcl* 2>/dev/null ` ; do
                if test -f $j/generic/tcl.h ; then
                    ac_cv_c_tclh=`(cd $j/generic; ${PWDCMD-pwd})`
                    break
                fi
            done
        fi
    done
fi

dnl check if its installed with the compiler
if test x"${ac_cv_c_tclh}" = x ; then
    dnl Get the path to the compiler
    ccpath=`which ${CC}  | sed -e 's:/bin/.*::'`/include
    if test -f $ccpath/tcl.h; then
        ac_cv_c_tclh=$ccpath
    fi
fi

dnl see if one is installed
if test x"${ac_cv_c_tclh}" = x ; then
   AC_MSG_RESULT(none)
   AC_CHECK_HEADER(tcl.h, ac_cv_c_tclh=installed, ac_cv_c_tclh="")
else
   AC_MSG_RESULT(${ac_cv_c_tclh})
fi
])
  TCLHDIR=""
if test x"${ac_cv_c_tclh}" = x ; then
    AC_MSG_ERROR([Can't find any Tcl headers])
fi
if test x"${ac_cv_c_tclh}" != x ; then
    no_tcl=""
    if test x"${ac_cv_c_tclh}" != x"installed" ; then
	case "${host}" in
	*windows32*)
	    tmp="`cygpath --windows ${ac_cv_c_tclh}`"
	    ac_cv_c_tclh="`echo $tmp | sed -e s#\\\\\\\\#/#g`"
	    ;;
	esac
        AC_MSG_RESULT(${ac_cv_c_tclh})
        TCLHDIR="-I${ac_cv_c_tclh}"
    fi
fi

AC_SUBST(TCLHDIR)
])

dnl ====================================================================
dnl Ok, lets find the tcl configuration
AC_DEFUN([CYG_AC_PATH_TCLCONFIG], [
dirlist=".. ../../ ../../../ ../../../../ ../../../../../ ../../../../../../ ../../../../../../.. ../../../../../../../.. ../../../../../../../../.. ../../../../../../../../../.."
dnl First, look for one uninstalled.  
dnl the alternative search directory is invoked by --with-tclconfig
if test x"${no_tcl}" = x ; then
  dnl we reset no_tcl in case something fails here
    no_tcl=true
    AC_ARG_WITH(tclconfig, [  --with-tclconfig           directory containing tcl configuration (tclConfig.sh)],
         with_tclconfig=${withval})
    AC_MSG_CHECKING([for Tcl configuration script])
    AC_CACHE_VAL(ac_cv_c_tclconfig,[

    dnl First check to see if --with-tclconfig was specified.
    if test x"${with_tclconfig}" != x ; then
        if test -f "${with_tclconfig}/tclConfig.sh" ; then
            ac_cv_c_tclconfig=`(cd ${with_tclconfig}; ${PWDCMD-pwd})`
        else
            AC_MSG_ERROR([${with_tclconfig} directory doesn't contain tclConfig.sh])
        fi
    fi

    dnl next check if it came with Tcl configuration file in the source tree
    if test x"${ac_cv_c_tclconfig}" = x ; then
        for i in $dirlist; do
            dnl need to test both unix and win directories, since 
            dnl cygwin's tkConfig.sh could be in either directory depending
            dnl on the cygwin port of tcl.
            if test -f $srcdir/$i/unix/tclConfig.sh ; then
                ac_cv_c_tclconfig=`(cd $srcdir/$i/unix; ${PWDCMD-pwd})`
	        break
            fi
            if test -f $srcdir/$i/win/tclConfig.sh ; then
                ac_cv_c_tclconfig=`(cd $srcdir/$i/win; ${PWDCMD-pwd})`
	        break
            fi
        done
    fi
    dnl check in a few other locations
    if test x"${ac_cv_c_tclconfig}" = x ; then
        dnl find the top level Tcl source directory
        for i in $dirlist; do
            if test -n "`ls -dr $i/tcl* 2>/dev/null`" ; then
                dnl find the exact Tcl dir. We do it this way, cause there
                dnl might be multiple version of Tcl, and we want the most recent one.
                for j in `ls -dr $i/tcl* 2>/dev/null ` ; do
                    dnl need to test both unix and win directories, since 
                    dnl cygwin's tclConfig.sh could be in either directory depending
                    dnl on the cygwin port of tcl.
                    if test -f $j/unix/tclConfig.sh ; then
                        ac_cv_c_tclconfig=`(cd $j/unix; ${PWDCMD-pwd})`
                        break
                    fi
                    if test -f $j/win/tclConfig.sh ; then
                        ac_cv_c_tclconfig=`(cd $j/win; ${PWDCMD-pwd})`
                        break
                    fi
                done
	    fi
        done
    fi

    dnl Check to see if it's installed. We have to look in the $CC path
    dnl to find it, cause our $prefix may not match the compilers.
    if test x"${ac_cv_c_tclconfig}" = x ; then
        dnl Get the path to the compiler
	ccpath=`which ${CC}  | sed -e 's:/bin/.*::'`/lib
        if test -f $ccpath/tclConfig.sh; then
	    ac_cv_c_tclconfig=$ccpath
        fi
    fi
    ])	dnl end of cache_val

    if test x"${ac_cv_c_tclconfig}" = x ; then
        TCLCONFIG=""
        AC_MSG_WARN(Can't find Tcl configuration definitions)
    else
        no_tcl=""
        TCLCONFIG=${ac_cv_c_tclconfig}/tclConfig.sh
        AC_MSG_RESULT(${TCLCONFIG})
     fi
fi
AC_SUBST(TCLCONFIG)
])

dnl Defined as a separate macro so we don't have to cache the values
dnl from PATH_TCLCONFIG (because this can also be cached).
AC_DEFUN([CYG_AC_LOAD_TCLCONFIG], [
    . $TCLCONFIG

dnl not used, don't export to save symbols
dnl AC_SUBST(TCL_VERSION)
dnl AC_SUBST(TCL_MAJOR_VERSION)
dnl AC_SUBST(TCL_MINOR_VERSION)
dnl AC_SUBST(TCL_CC)
    AC_SUBST(TCL_DEFS)
    AC_SUBST(TCL_DBGX)

dnl eval required to subst TCL_DBGX
    eval TCL_LIB_FILE=${TCL_LIB_FILE}
    eval TCL_LIB_FULL_PATH=${TCL_LIB_FULL_PATH}
    eval TCL_BUILD_LIB_SPEC=\"${TCL_BUILD_LIB_SPEC}\"
    eval TCL_LIB_SPEC=\"${TCL_LIB_SPEC}\"

    AC_SUBST(TCL_LIB_FILE)
    AC_SUBST(TCL_LIB_FULL_PATH)
    AC_SUBST(TCL_LIBS)
dnl not used, don't export to save symbols
dnl    AC_SUBST(TCL_PREFIX)

    AC_SUBST(TCL_CFLAGS)

dnl not used, don't export to save symbols
dnl    AC_SUBST(TCL_EXEC_PREFIX)

    AC_SUBST(TCL_SHLIB_CFLAGS)
    AC_SUBST(TCL_SHLIB_LD)
dnl don't export, not used outside of configure
dnl AC_SUBST(TCL_SHLIB_LD_LIBS)
dnl AC_SUBST(TCL_SHLIB_SUFFIX)
dnl not used, don't export to save symbols
dnl AC_SUBST(TCL_DL_LIBS)
    AC_SUBST(TCL_LD_FLAGS)
    AC_SUBST(TCL_LD_SEARCH_FLAGS)
dnl don't export, not used outside of configure
dnl AC_SUBST(TCL_COMPAT_OBJS)
    AC_SUBST(TCL_RANLIB)
    AC_SUBST(TCL_BUILD_LIB_SPEC)
    AC_SUBST(TCL_LIB_SPEC)
    AC_SUBST(TCL_BIN_DIR)
dnl AC_SUBST(TCL_LIB_VERSIONS_OK)

dnl not used, don't export to save symbols
dnl    AC_SUBST(TCL_SHARED_LIB_SUFFIX)

dnl not used, don't export to save symbols
dnl    AC_SUBST(TCL_UNSHARED_LIB_SUFFIX)
])

AC_DEFUN([CYG_AC_PATH_TKH], [
#
# Ok, lets find the tk source trees so we can use the headers
# If the directory (presumably symlink) named "tk" exists, use that one
# in preference to any others.  Same logic is used when choosing library
# and again with Tcl. The search order is the best place to look first, then in
# decreasing significance. The loop breaks if the trigger file is found.
# Note the gross little conversion here of srcdir by cd'ing to the found
# directory. This converts the path from a relative to an absolute, so
# recursive cache variables for the path will work right. We check all
# the possible paths in one loop rather than many separate loops to speed
# things up.
# the alternative search directory is involked by --with-tkinclude
#
dirlist=".. ../../ ../../../ ../../../../ ../../../../../ ../../../../../../ ../../../../../../.. ../../../../../../../.. ../../../../../../../../.. ../../../../../../../../../.."
no_tk=true
AC_MSG_CHECKING(for Tk headers in the source tree)
AC_ARG_WITH(tkinclude, [  --with-tkinclude       directory where tk headers are], with_tkinclude=${withval})
AC_CACHE_VAL(ac_cv_c_tkh,[
dnl first check to see if --with-tkinclude was specified
if test x"${with_tkinclude}" != x ; then
  if test -f ${with_tkinclude}/tk.h ; then
    ac_cv_c_tkh=`(cd ${with_tkinclude}; ${PWDCMD-pwd})`
  elif test -f ${with_tkinclude}/generic/tk.h ; then
    ac_cv_c_tkh=`(cd ${with_tkinclude}/generic; ${PWDCMD-pwd})`
  else
    AC_MSG_ERROR([${with_tkinclude} directory doesn't contain headers])
  fi
fi

dnl next check if it came with Tk configuration file
if test x"${ac_cv_c_tkconfig}" != x ; then
  for i in $dirlist; do
    if test -f $ac_cv_c_tkconfig/$i/generic/tk.h ; then
      ac_cv_c_tkh=`(cd $ac_cv_c_tkconfig/$i/generic; ${PWDCMD-pwd})`
      break
    fi
  done
fi

dnl next check in private source directory
dnl since ls returns lowest version numbers first, reverse its output
if test x"${ac_cv_c_tkh}" = x ; then
    dnl find the top level Tk source directory
    for i in $dirlist; do
        if test -n "`ls -dr $srcdir/$i/tk* 2>/dev/null`" ; then

    		dnl find the exact Tk source dir. We do it this way, cause there
    		dnl might be multiple version of Tk, and we want the most recent one.
    		for j in `ls -dr $srcdir/$i/tk* 2>/dev/null ` ; do
        		if test -f $j/generic/tk.h ; then
          			ac_cv_c_tkh=`(cd $j/generic; ${PWDCMD-pwd})`
          			break
        		fi
    		done
		fi
    done
fi

dnl see if one is installed
if test x"${ac_cv_c_tkh}" = x ; then
    AC_MSG_RESULT(none)
    dnl Get the path to the compiler. We do it this way instead of using
    dnl AC_CHECK_HEADER, cause this doesn't depend in having X configured.
    ccpath=`which ${CC}  | sed -e 's:/bin/.*::'`/include
    if test -f $ccpath/tk.h; then
	ac_cv_c_tkh=$ccpath
    fi
else
   AC_MSG_RESULT(${ac_cv_c_tkh})
fi
])
  TKHDIR=""
if test x"${ac_cv_c_tkh}" = x ; then
    AC_MSG_ERROR([Can't find any Tk headers])
fi
if test x"${ac_cv_c_tkh}" != x ; then
    no_tk=""
    if test x"${ac_cv_c_tkh}" != x"installed" ; then
	case "${host}" in
	*windows32*)
	    tmp="`cygpath --windows ${ac_cv_c_tkh}`"
	    ac_cv_c_tkh="`echo $tmp | sed -e s#\\\\\\\\#/#g`"
	    ;;
	esac
        AC_MSG_RESULT([found in ${ac_cv_c_tkh}])
        TKHDIR="-I${ac_cv_c_tkh}"
    fi
fi

AC_SUBST(TKHDIR)
])

AC_DEFUN([CYG_AC_PATH_TKCONFIG], [
dirlist=".. ../../ ../../../ ../../../../ ../../../../../ ../../../../../../ ../../../../../../.. ../../../../../../../.. ../../../../../../../../.. ../../../../../../../../../.."
dnl First, look for one uninstalled.  
dnl the alternative search directory is invoked by --with-tkconfig
if test x"${no_tk}" = x ; then
  dnl we reset no_tk in case something fails here
    no_tk=true
    AC_ARG_WITH(tkconfig, [  --with-tkconfig           directory containing tk configuration (tkConfig.sh)],
         with_tkconfig=${withval})
    AC_MSG_CHECKING([for Tk configuration script])
    AC_CACHE_VAL(ac_cv_c_tkconfig,[

    dnl First check to see if --with-tkconfig was specified.
    if test x"${with_tkconfig}" != x ; then
        if test -f "${with_tkconfig}/tkConfig.sh" ; then
            ac_cv_c_tkconfig=`(cd ${with_tkconfig}; ${PWDCMD-pwd})`
        else
            AC_MSG_ERROR([${with_tkconfig} directory doesn't contain tkConfig.sh])
        fi
    fi

    dnl next check if it came with Tk configuration file in the source tree
    if test x"${ac_cv_c_tkconfig}" = x ; then
        for i in $dirlist; do
            dnl need to test both unix and win directories, since 
            dnl cygwin's tkConfig.sh could be in either directory depending
            dnl on the cygwin port of tk.
            if test -f $srcdir/$i/unix/tkConfig.sh ; then
                ac_cv_c_tkconfig=`(cd $srcdir/$i/unix; ${PWDCMD-pwd})`
	        break
            fi
            if test -f $srcdir/$i/win/tkConfig.sh ; then
                ac_cv_c_tkconfig=`(cd $srcdir/$i/unix; ${PWDCMD-pwd})`
	        break
            fi
        done
    fi
    dnl check in a few other locations
    if test x"${ac_cv_c_tkconfig}" = x ; then
        dnl find the top level Tk source directory
        for i in $dirlist; do
            if test -n "`ls -dr $i/tk* 2>/dev/null`" ; then

        		dnl find the exact Tk dir. We do it this way, cause there
        		dnl might be multiple version of Tk, and we want the most recent one.
        		for j in `ls -dr $i/tk* 2>/dev/null ` ; do
            		dnl need to test both unix and win directories, since 
            		dnl cygwin's tkConfig.sh could be in either directory depending
            		dnl on the cygwin port of tk.
            		if test -f $j/unix/tkConfig.sh ; then
                		ac_cv_c_tkconfig=`(cd $j/unix; ${PWDCMD-pwd})`
                		break
            		fi
            		if test -f $j/win/tkConfig.sh ; then
                		ac_cv_c_tkconfig=`(cd $j/win; ${PWDCMD-pwd})`
                		break
            		fi
        		done
	    	fi
        done
    fi

    dnl Check to see if it's installed. We have to look in the $CC path
    dnl to find it, cause our $prefix may not match the compilers.
    if test x"${ac_cv_c_tkconfig}" = x ; then
        dnl Get the path to the compiler
	ccpath=`which ${CC}  | sed -e 's:/bin/.*::'`/lib
        if test -f $ccpath/tkConfig.sh; then
	    ac_cv_c_tkconfig=$ccpath
        fi
    fi
    ])	dnl end of cache_val

    if test x"${ac_cv_c_tkconfig}" = x ; then
        TKCONFIG=""
        AC_MSG_WARN(Can't find Tk configuration definitions)
    else
        no_tk=""
        TKCONFIG=${ac_cv_c_tkconfig}/tkConfig.sh
        AC_MSG_RESULT(${TKCONFIG})
     fi
fi
AC_SUBST(TKCONFIG)
])

dnl Defined as a separate macro so we don't have to cache the values
dnl from PATH_TKCONFIG (because this can also be cached).
AC_DEFUN([CYG_AC_LOAD_TKCONFIG], [
    if test -f "$TKCONFIG" ; then
      . $TKCONFIG
    fi

    AC_SUBST(TK_VERSION)
dnl not actually used, don't export to save symbols
dnl    AC_SUBST(TK_MAJOR_VERSION)
dnl    AC_SUBST(TK_MINOR_VERSION)
    AC_SUBST(TK_DEFS)

dnl not used, don't export to save symbols
    AC_SUBST(TK_LIB_FILE)
    AC_SUBST(TK_LIB_FULL_PATH)
    AC_SUBST(TK_LIBS)
dnl not used, don't export to save symbols
dnl    AC_SUBST(TK_PREFIX)

dnl eval required to subst TCL_DBGX
    eval TK_BUILD_LIB_SPEC=\"${TK_BUILD_LIB_SPEC}\"

dnl not used, don't export to save symbols
dnl    AC_SUBST(TK_EXEC_PREFIX)
    AC_SUBST(TK_BUILD_INCLUDES)
    AC_SUBST(TK_XINCLUDES)
    AC_SUBST(TK_XLIBSW)
    AC_SUBST(TK_BUILD_LIB_SPEC)
    AC_SUBST(TK_LIB_SPEC)
])

dnl ====================================================================
dnl Ok, lets find the itcl source trees so we can use the headers
dnl the alternative search directory is involked by --with-itclinclude

AC_DEFUN([CYG_AC_PATH_ITCLH], [
dirlist=".. ../../ ../../../ ../../../../ ../../../../../ ../../../../../../ ../../../../../../.. ../../../../../../../.. ../../../../../../../../.. ../../../../../../../../../.."
no_itcl=true
AC_MSG_CHECKING(for Itcl headers in the source tree)
AC_ARG_WITH(itclinclude, [  --with-itclinclude       directory where itcl headers are], with_itclinclude=${withval})
AC_CACHE_VAL(ac_cv_c_itclh,[
dnl first check to see if --with-itclinclude was specified
if test x"${with_itclinclude}" != x ; then
  if test -f ${with_itclinclude}/itcl.h ; then
    ac_cv_c_itclh=`(cd ${with_itclinclude}; ${PWDCMD-pwd})`
  elif test -f ${with_itclinclude}/src/itcl.h ; then
    ac_cv_c_itclh=`(cd ${with_itclinclude}/src; ${PWDCMD-pwd})`
  else
    AC_MSG_ERROR([${with_itclinclude} directory doesn't contain headers])
  fi
fi

dnl next check if it came with Itcl configuration file
if test x"${ac_cv_c_itclconfig}" != x ; then
  for i in $dirlist; do
    if test -f $ac_cv_c_itclconfig/$i/src/itcl.h ; then
      ac_cv_c_itclh=`(cd $ac_cv_c_itclconfig/$i/src; ${PWDCMD-pwd})`
      break
    fi
  done
fi

dnl next check in private source directory
dnl since ls returns lowest version numbers first, reverse its output
if test x"${ac_cv_c_itclh}" = x ; then
    dnl find the top level Itcl source directory
    for i in $dirlist; do
        if test -n "`ls -dr $srcdir/$i/itcl* 2>/dev/null`" ; then
	    itclpath=$srcdir/$i
	    break
	fi
    done

    dnl find the exact Itcl source dir. We do it this way, cause there
    dnl might be multiple version of Itcl, and we want the most recent one.
    for i in `ls -dr $itclpath/itcl* 2>/dev/null ` ; do
        if test -f $i/src/itcl.h ; then
          ac_cv_c_itclh=`(cd $i/src; ${PWDCMD-pwd})`
          break
        fi
    done
fi

dnl see if one is installed
if test x"${ac_cv_c_itclh}" = x ; then
   AC_MSG_RESULT(none)
   AC_CHECK_HEADER(itcl.h, ac_cv_c_itclh=installed, ac_cv_c_itclh="")
else
   AC_MSG_RESULT(${ac_cv_c_itclh})
fi
])
  ITCLHDIR=""
if test x"${ac_cv_c_itclh}" = x ; then
    AC_MSG_ERROR([Can't find any Itcl headers])
fi
if test x"${ac_cv_c_itclh}" != x ; then
    no_itcl=""
    if test x"${ac_cv_c_itclh}" != x"installed" ; then
        AC_MSG_RESULT(${ac_cv_c_itclh})
        ITCLHDIR="-I${ac_cv_c_itclh}"
    fi
fi

AC_SUBST(ITCLHDIR)
])

AC_DEFUN([CYG_AC_PATH_ITCLCONFIG], [
#
# Ok, lets find the itcl configuration
# First, look for one uninstalled.  
# the alternative search directory is invoked by --with-itclconfig
#

if test x"${no_itcl}" = x ; then
  # we reset no_itcl in case something fails here
  no_itcl=true
  AC_ARG_WITH(itclconfig, [  --with-itclconfig           directory containing itcl configuration (itclConfig.sh)],
         with_itclconfig=${withval})
  AC_MSG_CHECKING([for Itcl configuration])
  AC_CACHE_VAL(ac_cv_c_itclconfig,[

  # First check to see if --with-itclconfig was specified.
  if test x"${with_itclconfig}" != x ; then
    if test -f "${with_itclconfig}/itclConfig.sh" ; then
      ac_cv_c_itclconfig=`(cd ${with_itclconfig}; ${PWDCMD-pwd})`
    else
      AC_MSG_ERROR([${with_itclconfig} directory doesn't contain itclConfig.sh])
    fi
  fi

  # then check for a private itcl library
  if test x"${ac_cv_c_itclconfig}" = x ; then
    for i in \
		../itcl/itcl \
		`ls -dr ../itcl/itcl[[3]]* 2>/dev/null` \
		../../itcl/itcl \
		`ls -dr ../../itcl/itcl[[3]]* 2>/dev/null` \
		../../../itcl/itcl \
		`ls -dr ../../../itcl/itcl[[3]]* 2>/dev/null` ; do
      if test -f "$i/itclConfig.sh" ; then
        ac_cv_c_itclconfig=`(cd $i; ${PWDCMD-pwd})`
	break
      fi
    done
  fi
  # check in a few common install locations
  if test x"${ac_cv_c_itclconfig}" = x ; then
    for i in `ls -d ${prefix}/lib /usr/local/lib 2>/dev/null` ; do
      if test -f "$i/itclConfig.sh" ; then
        ac_cv_c_itclconfig=`(cd $i; ${PWDCMD-pwd})`
	break
      fi
    done
  fi
  # check in a few other private locations
  if test x"${ac_cv_c_itclconfig}" = x ; then
    for i in \
		${srcdir}/../itcl/itcl \
		`ls -dr ${srcdir}/../itcl/itcl[[3]]* 2>/dev/null` ; do
      if test -f "$i/itcl/itclConfig.sh" ; then
        ac_cv_c_itclconfig=`(cd $i; ${PWDCMD-pwd})`
	break
      fi
    done
  fi
  ])
  if test x"${ac_cv_c_itclconfig}" = x ; then
    ITCLCONFIG="# no itcl configs found"
    AC_MSG_WARN(Can't find itcl configuration definitions)
  else
    no_itcl=
    ITCLCONFIG=${ac_cv_c_itclconfig}/itclConfig.sh
    AC_MSG_RESULT(found $ITCLCONFIG)
  fi
fi

])

# Defined as a separate macro so we don't have to cache the values
# from PATH_ITCLCONFIG (because this can also be cached).
AC_DEFUN([CYG_AC_LOAD_ITCLCONFIG], [
    if test -f "$ITCLCONFIG" ; then
      . $ITCLCONFIG
    fi

dnl eval required to subst TCL_DBGX
    eval ITCL_BUILD_LIB_SPEC=\"${ITCL_BUILD_LIB_SPEC}\"

    AC_SUBST(ITCL_BUILD_LIB_SPEC)
    AC_SUBST(ITCL_SH)
    AC_SUBST(ITCL_LIB_FILE)
    AC_SUBST(ITCL_LIB_FULL_PATH)

])


AC_DEFUN([CYG_AC_PATH_ITKCONFIG], [
#
# Ok, lets find the itk configuration
# First, look for one uninstalled.  
# the alternative search directory is invoked by --with-itkconfig
#

if test x"${no_itk}" = x ; then
  # we reset no_itk in case something fails here
  no_itk=true
  AC_ARG_WITH(itkconfig, [  --with-itkconfig           directory containing itk configuration (itkConfig.sh)],
         with_itkconfig=${withval})
  AC_MSG_CHECKING([for Itk configuration])
  AC_CACHE_VAL(ac_cv_c_itkconfig,[

  # First check to see if --with-itkconfig was specified.
  if test x"${with_itkconfig}" != x ; then
    if test -f "${with_itkconfig}/itkConfig.sh" ; then
      ac_cv_c_itkconfig=`(cd ${with_itkconfig}; ${PWDCMD-pwd})`
    else
      AC_MSG_ERROR([${with_itkconfig} directory doesn't contain itkConfig.sh])
    fi
  fi

  # then check for a private itk library
  if test x"${ac_cv_c_itkconfig}" = x ; then
    for i in \
		../itcl/itk \
		`ls -dr ../itcl/itk[[3]]* 2>/dev/null` \
		../../itcl/itk \
		`ls -dr ../../itcl/itk[[3]]* 2>/dev/null` \
		../../../itcl/itk \
		`ls -dr ../../../itcl/itk[[3]]* 2>/dev/null` ; do
      if test -f "$i/itkConfig.sh" ; then
        ac_cv_c_itkconfig=`(cd $i; ${PWDCMD-pwd})`
	break
      fi
    done
  fi
  # check in a few common install locations
  if test x"${ac_cv_c_itkconfig}" = x ; then
    for i in `ls -d ${prefix}/lib /usr/local/lib 2>/dev/null` ; do
      if test -f "$i/itcl/itkConfig.sh" ; then
        ac_cv_c_itkconfig=`(cd $i; ${PWDCMD-pwd})`
	break
      fi
    done
  fi
  # check in a few other private locations
  if test x"${ac_cv_c_itkconfig}" = x ; then
    for i in \
		${srcdir}/../itcl/itk \
		`ls -dr ${srcdir}/../itcl/itk[[3]]* 2>/dev/null` ; do
      if test -f "$i/itkConfig.sh" ; then
        ac_cv_c_itkconfig=`(cd $i; ${PWDCMD-pwd})`
	break
      fi
    done
  fi
  ])
  if test x"${ac_cv_c_itkconfig}" = x ; then
    ITCLCONFIG="# no itk configs found"
    AC_MSG_WARN(Can't find itk configuration definitions)
  else
    no_itk=
    ITKCONFIG=${ac_cv_c_itkconfig}/itkConfig.sh
    AC_MSG_RESULT(found $ITKCONFIG)
  fi
fi

])

# Defined as a separate macro so we don't have to cache the values
# from PATH_ITKCONFIG (because this can also be cached).
AC_DEFUN([CYG_AC_LOAD_ITKCONFIG], [
    if test -f "$ITKCONFIG" ; then
      . $ITKCONFIG
    fi

dnl eval required to subst TCL_DBGX
    eval ITK_BUILD_LIB_SPEC=\"${ITK_BUILD_LIB_SPEC}\"

    AC_SUBST(ITK_BUILD_LIB_SPEC)
    AC_SUBST(ITK_LIB_FILE)
    AC_SUBST(ITK_LIB_FULL_PATH)
])

dnl originally named progversion.m4, placed here for convenience
dnl Copyright (C) 2008  Giel van Schijndel
dnl Copyright (C) 2008  Warzone Resurrection Project
dnl
dnl This file is free software; I, Giel van Schijndel give unlimited
dnl permission to copy and/or distribute it, with or without modficiations,
dnl as long as this notice is preserved.

AC_PREREQ(2.50)

AC_DEFUN([AC_PROG_VERSION_CHECK],
[
[
	ac_prog_version_$1=`$1 --version | head -n 1 | sed 's/([^)]*)//g;s/^[a-zA-Z\.\ \-\/]*//;s/ .*$//'`
	ac_prog_major_$1=`echo $ac_prog_version_$1 | cut -d. -f1`
	ac_prog_minor_$1=`echo $ac_prog_version_$1 | sed s/[-,a-z,A-Z].*// | cut -d. -f2`
	ac_prog_micro_$1=`echo $ac_prog_version_$1 | sed s/[-,a-z,A-Z].*// | cut -d. -f3`
	[ -z "$ac_prog_minor_$1" ] && ac_prog_minor_$1=0
	[ -z "$ac_prog_micro_$1" ] && ac_prog_micro_$1=0

	ac_prog_min_major_$1=`echo $2 | cut -d. -f1`
	ac_prog_min_minor_$1=`echo $2 | sed s/[-,a-z,A-Z].*// | cut -d. -f2`
	ac_prog_min_micro_$1=`echo $2 | sed s/[-,a-z,A-Z].*// | cut -d. -f3`
	[ -z "$ac_prog_min_minor_$1" ] && ac_prog_min_minor_$1=0
	[ -z "$ac_prog_min_micro_$1" ] && ac_prog_min_micro_$1=0
]

	AC_MSG_CHECKING([for $1 >= $2])

[
	if [ "$ac_prog_major_$1" -lt "$ac_prog_min_major_$1" ]; then
		ac_prog_wrong_$1=1
	elif [ "$ac_prog_major_$1" -eq "$ac_prog_min_major_$1" ]; then
		if [ "$ac_prog_minor_$1" -lt "$ac_prog_min_minor_$1" ]; then
			ac_prog_wrong_$1=1
		elif [ "$ac_prog_minor_$1" -eq "$ac_prog_min_minor_$1" -a "$ac_prog_micro_$1" -lt "$ac_prog_min_micro_$1" ]; then
			ac_prog_wrong_$1=1
		fi
	fi

	if [ ! -z "$ac_prog_wrong_$1" ]; then
]
		AC_MSG_WARN([found $ac_prog_version_$1, not ok])
	else
		AC_MSG_RESULT([found $ac_prog_version_$1, ok])
	fi

	ifelse([$3], [], , [
		for version in $3; do
		[
			ac_prog_not_major_$1=`echo $version | cut -d. -f1`
			ac_prog_not_minor_$1=`echo $version | sed s/[-,a-z,A-Z].*// | cut -d. -f2`
			ac_prog_not_micro_$1=`echo $version | sed s/[-,a-z,A-Z].*// | cut -d. -f3`
			[ -z "$ac_prog_not_minor_$1" ] && ac_prog_not_minor_$1=0
			[ -z "$ac_prog_not_micro_$1" ] && ac_prog_not_micro_$1=0
		]
			AC_MSG_CHECKING([for $1 != $version])
		[
			if [ "$ac_prog_major_$1" -eq "$ac_prog_not_major_$1" -a "$ac_prog_minor_$1" -eq "$ac_prog_not_minor_$1" -a "$ac_prog_micro_$1" -eq "$ac_prog_not_micro_$1" ]; then
		]
				AC_MSG_WARN([found $ac_prog_version_$1, not ok])
			else
				AC_MSG_RESULT([not found, good])
			fi
		done
	])
])
