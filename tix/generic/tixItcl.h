/*
 * tixItcl.h --
 *
 *	Compatibility functions and macros that allow Tix to work
 *	under Incr Tcl.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 *
 */

/*
 * With Tcl 8.0, namespaces moved from Itcl to Tcl, and so
 * the Tix hacks have to be used in any verison of 8.0,
 * regardless of the presence of Itcl...
 */

#include "tix.h"
#ifdef TK_8_0_OR_LATER

#ifndef _TCLINT
#include <tclInt.h>
#endif

/*
 * Structure to store Tcl 8.0 name space information.
 */

typedef struct _TixItclNameSp {
    Interp *iPtr;
    CallFrame *savedVarFramePtr;
} TixItclNameSp;

#define DECLARE_ITCL_NAMESP(x,i) \
    TixItclNameSp x; \
    x.iPtr = (Interp*)(i);

#ifdef BUILD_tix
# undef TCL_STORAGE_CLASS
# define TCL_STORAGE_CLASS DLLEXPORT
#endif

EXTERN int		TixItclSetGlobalNameSp _ANSI_ARGS_((
			    TixItclNameSp * nameSpPtr, Tcl_Interp * interp));
EXTERN void		TixItclRestoreGlobalNameSp _ANSI_ARGS_((
			    TixItclNameSp * nameSpPtr, Tcl_Interp * interp));

#else
#ifdef ITCL_2

#ifndef _TCLINT
#include <tclInt.h>
#endif
/*
 * Structure to store ITcl name space information.
 */
typedef struct _TixItclNameSp {
    Interp *iPtr;
    CallFrame *savedVarFramePtr;
    Itcl_ActiveNamespace nsToken;
} TixItclNameSp;

#define DECLARE_ITCL_NAMESP(x,i) \
    TixItclNameSp x; \
    x.iPtr = (Interp*)(i); \
    x.nsToken = NULL;

#ifdef BUILD_tix
# undef TCL_STORAGE_CLASS
# define TCL_STORAGE_CLASS DLLEXPORT
#endif

EXTERN int		TixItclSetGlobalNameSp _ANSI_ARGS_((
			    TixItclNameSp * nameSpPtr, Tcl_Interp * interp));
EXTERN void		TixItclRestoreGlobalNameSp _ANSI_ARGS_((
			    TixItclNameSp * nameSpPtr, Tcl_Interp * interp));

#else

#define DECLARE_ITCL_NAMESP(x,i)
#define TixItclSetGlobalNameSp(a,b)     (1)
#define TixItclRestoreGlobalNameSp(a,b)

#endif /* ITCL_2 */
#endif /* TK_8_0_OR_LATER */

# undef TCL_STORAGE_CLASS
# define TCL_STORAGE_CLASS DLLIMPORT
