/*

Copyright (c) 2000, Red Hat, Inc.

This file is part of Source-Navigator.

Source-Navigator is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2, or (at your option)
any later version.

Source-Navigator is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along
with Source-Navigator; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA.



*/

/* 
 * tclXlist.c --
 *
 *  Extended Tcl list commands.
 *-----------------------------------------------------------------------------
 * Copyright 1991-1993 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 */
#include "tcl.h"
#include <ctype.h>
#include <string.h>

/*
 * Forward declarations for procedures defined in this file:
 */

static int    Tcl_LmatchCmd(ClientData, Tcl_Interp*, int, Tcl_Obj *CONST objv[]);

void
Tcl_XListInit(interp)
Tcl_Interp    *interp;
{
    Tcl_CreateObjCommand(interp, "lmatch", (Tcl_ObjCmdProc *)Tcl_LmatchCmd,
	(ClientData) NULL, (Tcl_CmdDeleteProc *)  NULL);
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_LmatchCmd --
 *
 *    This procedure is invoked to process the "lmatch" Tcl command.
 *    See the user documentation for details on what it does.
 *
 * Results:
 *    A standard Tcl result.
 *
 * Side effects:
 *    See the user documentation.
 *
 *----------------------------------------------------------------------
 */
static int
Tcl_LmatchCmd(ClientData clientdata,Tcl_Interp *interp,int argc,Tcl_Obj *CONST objv[])
{
#define EXACT    0
#define GLOB    1
#define REGEXP    2
#define OCCURE    3
    int listArgc;
    Tcl_Obj **listArgv;
    int match, mode;
    register char *patt;
    register int cou;
    int pattArgc;
    Tcl_RegExp    regexp_ptr = 0;
    char *str_to_compare = NULL;
    int    match_field = -1;
    Tcl_Obj **pattArgv = NULL;
    Tcl_Obj *resultPtr;
    int    c;

    resultPtr = Tcl_GetObjResult(interp);
    mode = GLOB;
    if (argc == 4 || argc == 5)
    {
	if (argc == 5)
	{
	    if (Tcl_GetIntFromObj(interp, objv [2], &match_field) != TCL_OK)
		return TCL_ERROR;
	}
	if (strcmp(Tcl_GetStringFromObj (objv [1], NULL), "-exact") == 0)
	{
	    mode = EXACT;
	}
	else if (strcmp(Tcl_GetStringFromObj (objv [1], NULL), "-glob") == 0)
	{
	    mode = GLOB;
	}
	else if (strcmp(Tcl_GetStringFromObj (objv [1], NULL), "-regexp") == 0)
	{
	    mode = REGEXP;
	}
	else if (strcmp(Tcl_GetStringFromObj (objv [1], NULL), "-occure") == 0)
	{
	    mode = OCCURE;
	    if (Tcl_ListObjGetElements (interp, objv[argc-1],&pattArgc,&pattArgv) != TCL_OK)
		return TCL_ERROR;
	}
	else
	{
	    Tcl_AppendStringsToObj(resultPtr,
		"bad search mode \"",
		Tcl_GetStringFromObj (objv [0], NULL),
		"\": must be -exact, -glob, -regexp or -occure", (char *) NULL);
	    return TCL_ERROR;
	}
    }
    else if (argc != 3)
    {
	Tcl_WrongNumArgs(interp, 1, objv,"?mode? list pattern");

	return TCL_ERROR;
    }

    if (Tcl_ListObjGetElements (interp, objv [argc - 2],
	&listArgc, &listArgv) != TCL_OK)
    {
	return TCL_ERROR;
    }

    patt = Tcl_GetStringFromObj(objv[argc-1],NULL);

    switch (mode)
    {
    case GLOB:
	if (strcmp(patt,"*") == 0)
	{
	    Tcl_SetObjResult(interp, objv [argc - 2]);
	    return TCL_OK;
	}
	break;

    case REGEXP:
	if (strcmp(patt,".*") == 0)
	{
	    Tcl_SetObjResult(interp, objv [argc - 2]);
	    return TCL_OK;
	}
	regexp_ptr = Tcl_RegExpCompile(interp,patt);
	if (!regexp_ptr)
	    return TCL_ERROR;
	break;
    }

    for (c = 0; c < listArgc; c++)
    {
	if (match_field == -1)
	    str_to_compare = Tcl_GetStringFromObj(listArgv[c],NULL);
	else
	{
	    Tcl_Obj *sub_fld;

	    if (Tcl_ListObjIndex(interp,listArgv[c],match_field,&sub_fld) != TCL_OK ||
		!sub_fld)
	    {
		continue;
	    }
	    str_to_compare = Tcl_GetStringFromObj(sub_fld,NULL);
	}

	switch (mode)
	{
	case EXACT:
	    match = (*str_to_compare == *patt && strcmp(str_to_compare,patt) == 0);
	    break;

	case GLOB:
	    match = Tcl_StringMatch(str_to_compare, patt);
	    break;

	case REGEXP:
	    match = Tcl_RegExpExec(interp, regexp_ptr, str_to_compare, str_to_compare);
	    if (match < 0)
	    match = 0;
	    break;

	case OCCURE:
	    for (match = 0, cou = 0; cou < pattArgc && !match; cou++)
	    {
		char    *str = Tcl_GetStringFromObj(pattArgv[cou],NULL);
		if ((patt = strstr(str_to_compare,str)))
		{
		    match = (strcmp(str, patt) == 0);
		}
	    }
	    break;
	default:
	    match = 0;
	    break;
	}

	if (match)
	{
	    Tcl_ListObjAppendElement (interp, resultPtr,listArgv[c]);
	}
    }

    return TCL_OK;
}

