/* 
 * myAppInit.c --
 *
 *	This is a demo program that shows how to link your C programs
 *	with Tcl/Tk/Tix.
 *
 *	The program created in this directory is a demo program
 *	called "myapp". You can modify the files in this directory
 *	to use in your applications.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include <tk.h>
#include <tix.h>

/*
 * The following variable is a special hack that is needed in order for
 * Sun shared libraries to be used for Tcl.
 */

extern int matherr();
int *tclDummyMathPtr = (int *) matherr;


/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	This is the main program for the application.
 *
 * Results:
 *	None: Tk_Main never returns here, so this procedure never
 *	returns either.
 *
 * Side effects:
 *	Whatever the application does.
 *
 *----------------------------------------------------------------------
 */

int
main(argc, argv)
    int argc;			/* Number of command-line arguments. */
    char **argv;		/* Values of command-line arguments. */
{
    Tk_Main(argc, argv, Tcl_AppInit);
    return 0;			/* Needed only to prevent compiler warning. */
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *	Most applications, especially those that incorporate additional
 *	packages, will have their own version of this procedure.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 * Side effects:
 *	Depends on the startup script.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_AppInit(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
    /* Initialize the Tcl, Tk and Tix packages (in this order) */

    if (Tcl_Init(interp) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Tk_Init(interp) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Tix_Init(interp) != TCL_OK) {
	return TCL_ERROR;
    }

    /*
     * If you want to use other packages, call their initialization
     * procedures here. Each call should look like this:
     *
     * if (Mod_Init(interp) != TCL_OK) {
     *     return TCL_ERROR;
     * }
     *
     * where "Mod" is the name of the module.
     *
     * For example, the intialization procedure for the BLT package is
     * BLT_Init().
     */



    /*
     * Call My_Init() to do application specific initialization.
     */

    if (My_Init(interp) != TCL_OK) {
	return TCL_ERROR;
    }

    /*
     * Specify a user-specific startup file to invoke
     */

    Tix_SetRcFileName(interp, "my");

    return TCL_OK;
}
