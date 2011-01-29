/*
 * myInit.c --
 *
 *	Initialze the Tix demo application.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include <tk.h>
#include <tix.h>

#ifndef _Windows
#  ifndef _export
#  define _export
#  endif
#endif

extern TIX_DECLARE_CMD(My_AddTwoCmd);
extern TIX_DECLARE_CMD(My_SubTwoCmd);

#ifndef MY_LIBRARY
#define MY_LIBRARY "/usr/local/myapp"
#endif


static Tix_TclCmd commands[] = {
    {"myAddTwo",           	My_AddTwoCmd},
    {"mySubTwo",           	My_SubTwoCmd},

    /*
     * Make sure this list is terminated by a NULL element
     */
    {(char *) NULL,		(int (*)()) NULL}
};

/* My_Init --
 *
 * 	This is the function to call in your Tcl_AppInit() function. It
 *	creates the commands of this application that are defined by
 *	C functions.
 */
int _export
My_Init(interp)
    Tcl_Interp * interp;
{
    /* Initialize the Tix commands */
    Tix_CreateCommands(interp, commands, (ClientData) NULL,
	(void (*)()) NULL);

    if (Tix_LoadTclLibrary(interp, "MY_LIBRARY", "my_library", 
	"Init.tcl", MY_LIBRARY, "myapp") != TCL_OK) {
	return TCL_ERROR;
    }

    return TCL_OK;
}
