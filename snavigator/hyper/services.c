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
 * services.c
 * 
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * Implement some database services for speed.
 */

#include <stdlib.h>
#include <tcl.h>

#include "mxdefine.h"
#include "mxfuncs.h"
#include "mxlogger.h"

#include "sn.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifdef __MSVC__
#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#endif

EXTERN int	TclFindElement _ANSI_ARGS_((Tcl_Interp *interp,
	        char *list, int listLength, char **elementPtr,
	        char **nextPtr, int *sizePtr, int *bracePtr));

/* *********************************************************************
 * this command is used to realize some functions in the retriever
 * on the C/Level to be fast
 * *********************************************************************/
enum
{
    NAM_POS=0,
    CLS_POS,
    TYP_POS,
    PRM_POS,
    FIL_POS,
    FRM_POS,
    TO__POS,
    FLD_CNT
};

static int
GetNumber (char *str)
{
    char tmp[32];
    char *p=str;
    char *q=tmp;
    int i;

    for (i=0; i<32 && *p && *p != '.' && *p != '\t'; i++)
    {
	*q ++ = *p ++;
    }
    *q = 0;

    return atoi (tmp);
}

static char *
GetScope (char *str)
{
    static char tmp[5];
    int i, pos = strlen (str) - 1;

    if (pos <= 0 || str[pos] != ')')
	return "ud";

    for (pos--; pos >= 0 && str[pos] != '('; pos--);

    for (i=0; str[pos] && str[pos] != ')' && i<5; pos++)
    {
	if (str[pos] != '(')
	{
	    tmp[i++] = str[pos];
	}
    }
    tmp[i] = 0;
	
    return tmp;
}

static int
SplitLine (char *pline, int size, int *bufsize, char **line, int *item_size)
{
    char *p, *qline;
    int i, l, j, fnd = 0;

    /* split lines manually */
    for (i=l=j=0, p=qline=pline; i<size && *p; i++, p++)
    {
	if (*p == '\t')
	{
	    if (bufsize[j] < l)
	    {
		bufsize[j] = l+256;
		line[j] = ckrealloc (line[j], bufsize[j]);
	    }
	    memcpy (line[j], qline, l);
	    line[j][l] = 0;
	    item_size[j] = l;
	    l = 0;
	    qline = p+1;
	    j++;
	}
	else
	{
	    l++;
	}
	if (j>=9)
	{
	    break;
	}
    }

    memcpy (line[j], qline, l);
    line[j][l] = 0;
    item_size[j] = l;

    fnd = j+1;

    /* set the rest of array to \0 */
    for (j++;j<10;j++)
    {
	line[j][0] = 0;
	item_size[j] = 0;
    }

    return fnd;
}

int
retriever_services(ClientData clientData,
		Tcl_Interp *interp, /* Current interpreter. */
		int argc,           /* Number of arguments. */
		char **argv)        /* Argument strings.    */
{
    Tcl_CmdInfo infoPtr;
    ClientData  wcdata = 0;
    Tcl_CmdProc* wcmd = NULL;
    char *wname;
    char *command, *contents;
    int pargc, i, size, itembuf_size[10], item_size[10];
    char *p, *q, *line_items[10], *pline;
    int result, ret = TCL_OK, fnd = 0;
    int length;

    wname = argv[1];
    if (wname[0])
    {
	if (!Tcl_GetCommandInfo(interp, wname, &infoPtr))
	{
	    Tcl_AppendResult(interp, "wrong # \"",
		wname, "\" does not exist", (char *) NULL);
	    return TCL_ERROR;
	}
	wcdata = infoPtr.clientData;
	wcmd = (Tcl_CmdProc *)infoPtr.proc;
    }
    pargc = 2;
    command   = argv[pargc++];
    contents  = argv[pargc++];

    if (contents[0] == '\0')
    {
	Tcl_SetResult (interp, "", TCL_STATIC);
	return ret;
    }
    Tcl_ResetResult (interp);

    for (i=0;i<10;i++)
    {
	line_items[i] = ckalloc (256);
	itembuf_size[i] = 256;
    }

    /*
     * given a list of found symbols, insert those into the
     * treetable.
     *******************************************************/
    if ((argc == 5 || argc == 4) && *command == 'i' && strcmp (command, "insert") == 0)
    {
	int txtlen = 1024, datalen = 256, newlen, newdlen;
	int rargc = 0, slen;
	char *rargv[13];
	char *text, *data, image[64], *scope;
	int fnd;
	
	text = ckalloc (txtlen);
	data = ckalloc (datalen);
	
	rargc = 0;
	rargv[rargc++] = wname;
	rargv[rargc++] = "insert";
	rargv[rargc++] = "end";
	rargv[rargc++] = "-text";
	rargv[rargc++] = text;
	rargv[rargc++] = "-data";
	rargv[rargc++] = data;
	rargv[rargc++] = "-image";
	rargv[rargc++] = image;
	if (argc == 5)
	{
	    rargv[rargc++] = "-parent";
	    rargv[rargc++] = argv[pargc++];
	}
	
	for (length = strlen(contents), q = contents; 1;)
        {
	    char    *prevlist = q;
	    result = TclFindElement(interp, q, length, &pline, &q, &size, NULL);
	    if (result != TCL_OK || size == 0)
	    {
		break;
	    }
	    length -= q - prevlist;
	
	    /* split line into fields */
	    fnd = SplitLine (pline, size, itembuf_size, line_items, item_size);
	
	    /* use dynamic buffers for data */
	    newdlen = item_size[FIL_POS]
		    + item_size[FRM_POS]
		    + item_size[TO__POS]
		    + 10 /*scope*/ + 4;
	    if (newdlen > datalen)
	    {
		datalen = newdlen + newdlen / 2;
		data = ckrealloc (data, datalen);
		rargv[6] = data;
	    }
	
	    /* we parse files, when number of columns is 1-2 or
	     * the file filed is empty */
	    if (fnd == 2 || fnd == 3 || line_items[FIL_POS][0] == 0)
	    {
		strcpy (image, "file_+_image");
		
		if (line_items[TYP_POS][0] == '1')
		{
		    continue; /* skip directories */
		}
		/* skip first character, if there is a space */
		if (line_items[TYP_POS][0] == ' ')
		{
		    strcpy (line_items[TYP_POS], line_items[TYP_POS]+1);
		}
		
		/* mark the files entries as special entries */
		strcpy (data, "1");
	    }
	    else
	    {
		/* scope name */
		scope = GetScope (line_items[NAM_POS]);
		if (scope[0] == 0)
		{
		    scope = "ud";
		}
		slen = strlen (scope);
		/* skip 'iu' and 'in': there are ref. to includes */
		if (slen == 2 && scope[0] == 'i' && (scope[1] == 'u' || scope[1] == 'n'))
		{
		    continue;
		}
		
		/* only 'fd' 'fu' 'md' 'mi' and 'fr' have parameter list */
		if (! (slen == 2 && (
		    (scope[0] == 'm' && (scope[1] == 'i' || scope[1] == 'd')) ||
		    (scope[0] == 'f' && (scope[1] == 'd' || scope[1] == 'u' || scope[1] == 'r')))))
		{
		    line_items[PRM_POS][0] = 0;
		}
		sprintf (image, "type_%s_image", scope);
		
		sprintf (data, "%s\t%s\t%s\t%s",
			    line_items[FIL_POS],
			    line_items[FRM_POS],
			    line_items[TO__POS],
			    scope);
	    }
	
	    newlen = item_size[NAM_POS]
		    + item_size[CLS_POS]
		    + item_size[TYP_POS]
		    + item_size[PRM_POS]
		    + item_size[FIL_POS]
		    + 5;
	    if (newlen > txtlen)
	    {
		txtlen = newlen + newlen / 2;
		text = ckrealloc (text, txtlen);
		rargv[4] = text;
	    }
	    if (line_items[CLS_POS][0] == '#')
	    {
		line_items[CLS_POS][0] = 0;
	    }
	    sprintf (text, "%s\t%s\t%s\t%s\t%s",
			line_items[NAM_POS],
			line_items[CLS_POS],
			line_items[TYP_POS],
			line_items[PRM_POS],
			line_items[FIL_POS]);
	    (*wcmd)(wcdata, interp, rargc, rargv);
	}
	
	ckfree (data);
	ckfree (text);
    }
    else if (argc == 10 && *command == 'f' && strcmp (command, "filter") == 0)
    {
	char *pattern, *file, *type, *parameter;
	int from, to;
	char *buf;
	int buf_size = 512;
	int length;
	
	pattern   = argv[pargc++];
	file      = argv[pargc++];
	type      = argv[pargc++];
	parameter = argv[pargc++];
	from      = atoi (argv[pargc++]);
	to        = atoi (argv[pargc++]);
	
	buf = (char*)ckalloc (buf_size);
	
	if (*contents) for (length=strlen(contents), q = contents; 1;)
        {
	    char    *prevlist = q;
	    result = TclFindElement(interp, q, length, &pline, &q, &size, NULL);
	    if (result != TCL_OK || size == 0)
	    {
		break;
	    }
	    length -= q - prevlist;
	    SplitLine (pline, size, itembuf_size, line_items, item_size);
	
	    if (*type)
	    {
		if (item_size[TYP_POS] == 0)
		{
		    continue;
		}
		if (strncmp (type, line_items[TYP_POS], item_size[TYP_POS]) != 0)
		{
		    continue;
		}
	    }
	
	    if (*parameter)
	    {
		if (item_size[PRM_POS] == 0)
		{
		    continue;
		}
		/* delete () from parameter list, if parameter doesn't
		 * begin with '(' */
		if (*parameter != '(')
		{
		    for (p=line_items[PRM_POS]; p[1] && p[2]; p++)
		    {
			p[0] = p[1];
		    }
		    p[0] = 0;
		}
		if (strcmp (parameter, line_items[PRM_POS]) != 0)
		{
		    continue;
		}
	    }
	
	    if (*file)
	    {
		if (*file != *line_items[FIL_POS] || strcmp (file, line_items[FIL_POS]) != 0)
		{
		    continue;
		}
	    }
	
	    if (from != -1)
	    {
		if (from < GetNumber (line_items[FRM_POS]))
		{
		    continue;
		}
	    }
	    if (to != -1)
	    {
		if (to > GetNumber (line_items[TO__POS]))
		{
		    continue;
		}
	    }
	
	    if (buf_size < size+1)
	    {
		buf_size += size;
		buf = (char*)ckrealloc (buf, buf_size);
	    }
	    memcpy (buf, pline, size);
	    buf[size] = 0;
	
	    Tcl_AppendElement (interp, buf);
	    fnd = 1;
	}
	
	if (!fnd)
	{
	    Tcl_SetResult (interp, "", TCL_STATIC);
	}
	
	ckfree (buf);
    }
    else
    {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	     " tree filter contents pattern file type parameter from to |\n"
	     " tree insert contents ?parent?\"",
	     (char *) NULL);
	ret = TCL_ERROR;
    }

    for(i=0;i<10;i++)
    {
	ckfree (line_items[i]);
    }
    return ret;
}

/* ******************************************************
 * service procedure for cross reference to speed up some
 * heavy used routines
 * ******************************************************/
typedef enum SymbolsToFilter
{
  DB_SYM2 = 0,
  DB_CLS2,
  DB_SCP2,
  DB_PRM2,
  
  DB_REFA,
  DB_FILE,
  DB_LINE,
  DB_PRM1,
  DB_COUNT
} SymbolsToFilter;

typedef enum CreatedRefInfo
{
    item1_pos = 0,
    class1_pos,
    what1_pos,
    param1_pos,
    type1_pos,

    file_pos,
    file_line_pos,

    file1_pos,
    range1_from,
    range1_to,

    flags_pos,
    refart_pos,

    line_arg_count
} CreatedRefInfo;

typedef enum RefTypes
{
    REF_TO,
    REF_BY
} RefTypes;

static int
cross_is_type_with_classes (char *scope)
{
    static char * scps[] = {"md", "mi", "iv", "fr", NULL};
    int i;
    for (i=0; scps[i] != NULL; i++)
    {
	if (strcmp (scope, scps[i]) == 0)
	{
	    return 1;
	}
    }
    return 0;
}
int
cross_services(ClientData clientData,
		Tcl_Interp *interp, /* Current interpreter. */
		int argc,           /* Number of arguments. */
		char **argv)        /* Argument strings.    */
{
    Tcl_CmdInfo infoPtr;
    ClientData  wcdata;
    Tcl_CmdProc* wcmd;
    char *wname;
    char *command, *contents;
    int pargc, i, size;
    char *pline, *q;
    int result, ret = TCL_OK;

    wname = argv[1];
    if (wname[0])
    {
	if (!Tcl_GetCommandInfo(interp, wname, &infoPtr))
	{
	    Tcl_AppendResult(interp, "wrong # \"",
		wname, "\" does not exist", (char *) NULL);
	    return TCL_ERROR;
	}
	wcdata = infoPtr.clientData;
	wcmd = (Tcl_CmdProc *)infoPtr.proc;
    }
    Tcl_ResetResult (interp);

    pargc = 2;
    command   = argv[pargc++];
    contents  = argv[pargc++];

    if (argc == 12 && *command == 'f' && strcmp (command, "filter") == 0)
    {
	char *refartStr, *testline, *shown_scopes, *ref_access;
	char    *file = NULL;
	enum RefTypes refart;
	char **tfields, **lfields=NULL, **oldfields=NULL;
	char *tmpline;
	int tmpline_size = 512;
	int uniq, have, accept_static, accept_param, fsize, tsize;
	int AddRefArt=0;
	int length;
	char *line[line_arg_count], AddRefartStr[16] = {0};
	Tcl_DString res, erg;
	
	refartStr = argv[pargc++];
	testline  = argv[pargc++];
	uniq      = atoi (argv[pargc++]);
	have      = atoi (argv[pargc++]);
	accept_param = atoi (argv[pargc++]);
	accept_static= atoi (argv[pargc++]);
	shown_scopes = argv[pargc++];
	ref_access   = argv[pargc++];
	
	if (accept_static)
	{
	    /* Information to the actual scope */
	    if (Tcl_SplitList (interp, testline, &tsize, &tfields) != TCL_OK)
	    {
		return TCL_ERROR;
	    }
	    file = tfields[file1_pos];
	}
	if (strcmp (refartStr, "to") == 0)
	{
	    refart = REF_TO;
	}
	else
	{
	    refart = REF_BY;
	}
	
	/* init some variables */
	for (i=0; i<line_arg_count; i++)
	{
	    line[i] = "";
	}
	Tcl_DStringInit(&res);
	Tcl_DStringInit(&erg);
	tmpline = (char*)ckalloc (tmpline_size); tmpline[0] = 0;
	
	for (length=strlen(contents), q = contents; 1;)
        {
        	char    *prevlist = q;
	    result = TclFindElement(interp, q, length, &pline, &q, &size, NULL);
	    if (result != TCL_OK || size == 0)
	    {
		break;
	    }
	    length -= q - prevlist;
	    if (size > tmpline_size)
	    {
		tmpline_size += size;
		tmpline = ckrealloc (tmpline, tmpline_size);
	    }
	    memcpy (tmpline, pline, size);
	    tmpline[size] = 0;
	    if (Tcl_SplitList (interp, tmpline, &fsize, &lfields) != TCL_OK)
	    {
		continue;
	    }
	    if (fsize != DB_COUNT)
	    {
		ckfree ((char*)lfields);
		continue;
	    }
	
	    if (*shown_scopes && strstr (shown_scopes, lfields[DB_SCP2]) == NULL)
	    {
		continue;
	    }
	
	    if (*ref_access && strstr (ref_access, lfields[DB_REFA]) == NULL)
	    {
		continue;
	    }
	
	    if (uniq && oldfields)
	    {
		if (strcmp (oldfields[DB_CLS2], lfields[DB_CLS2]) == 0 &&
		    strcmp (oldfields[DB_SYM2], lfields[DB_SYM2]) == 0 &&
		    strcmp (oldfields[DB_SCP2], lfields[DB_SCP2]) == 0 &&
		    (! accept_param ||
		    (accept_param && strcmp (oldfields[DB_PRM2], lfields[DB_PRM2]) == 0)))
		{
		    if (!AddRefartStr[0] ||
			(lfields[DB_REFA][0] && strchr (AddRefartStr, lfields[DB_REFA][0]) == NULL))
		    {
			strcat (AddRefartStr, lfields[DB_REFA]);
		    }
		    ckfree ((char *) lfields);
		    continue;
		}
	    }

	    /* Static functions and variables */
	    if (accept_static && refart == REF_TO && lfields[DB_REFA][0] != 0 && ! cross_is_type_with_classes(lfields[DB_SCP2]))
	    {
		int attr;
		if (Tcl_GetInt(interp, lfields[DB_REFA], &attr) == TCL_OK &&
		    (! (attr&PAF_STATIC) || strcmp (lfields[DB_FILE], file) != 0))
		{
		    ckfree ((char*)lfields);
		    continue;
		}
	    }
	
	    if (have)
	    {
		ckfree ((char *) lfields);
		Tcl_DStringAppendElement (&erg, "yes");
		break;
	    }
	
	    if (AddRefArt)
	    {
		Tcl_DStringAppendElement(&res, AddRefartStr);
		Tcl_DStringAppendElement(&erg, Tcl_DStringValue(&res));
	    }
	    else
	    {
		AddRefArt = 1;
	    }
	    strcpy (AddRefartStr, lfields[DB_REFA]);
	
	    line[class1_pos] = lfields[DB_CLS2];
	    line[item1_pos]  = lfields[DB_SYM2];
	    line[what1_pos]  = lfields[DB_SCP2];
	    line[param1_pos] = lfields[DB_PRM2];

	    line[file_pos]   = lfields[DB_FILE];
	    line[file_line_pos] = lfields[DB_LINE];
	
	    Tcl_DStringFree (&res);
	    for (i=0; i<refart_pos; i++)
	    {
		Tcl_DStringAppendElement (&res, line[i]);
	    }

	    /* Store last line */
	    if (oldfields)
	    {
		ckfree ((char*)oldfields);
	    }
	    oldfields = lfields;
	}
	if (AddRefArt)
	{
	    Tcl_DStringAppendElement(&res, AddRefartStr);
	    Tcl_DStringAppendElement(&erg, Tcl_DStringValue(&res));
	    AddRefartStr[0] = 0;
	}
	Tcl_DStringFree (&res);
	
	if (accept_static)
	{
	    ckfree ((char*)tfields);
	}
	if (oldfields)
	{
	    ckfree ((char*)oldfields);
	}
	ckfree (tmpline);
	
	Tcl_DStringResult(interp, &erg);
	Tcl_DStringFree  (&erg);
    }
    else if (argc == 7 && *command == 'i' && strcmp (command, "insert") == 0)
    {
    }
    else
    {
	char tmp[32];
	sprintf (tmp, "%i", argc);
	Tcl_AppendResult(interp, "wrong # args(", tmp, "): should be \"", argv[0],
	     " filter \"\" contents RefArt line unique have accept_param accept_static shown_scopes ref_access |\n"
	     "insert widget contents RefArt id line\n",
	     "\"",
	     (char *) NULL);
	ret = TCL_ERROR;
    }
    return ret;
}

/*****************************************************************************
 * Routine to insert class members in the list based on various filter
 * selections.
 *****************************************************************************/
enum
{
    MEMBER_POS = 0,
    PARAM_POS,
    NUM_POS,
    TYPE_POS,
    CLASS_POS,
    FILENAME_POS,
    FILEPOS_POS,
    ATTR_POS,
    LIST_CNT
};

#define OVERRIDE   '+'
#define OVERRIDDEN '-'
#define CLASS(cls) (*cls ? cls : browsed_class)
int
class_browser_insert(ClientData clientData,
    Tcl_Interp *interp,             /* Current interpreter. */
    int argc,                       /* Number of arguments. */
    Tcl_Obj *objv[])            /* Argument strings. */
{
    Tcl_CmdInfo     infoPtr;
    char    *textwid;
    register void*textPtr = NULL;
    char image [64];
    char *protected_font, *public_font, *private_font, font[512];
    int del;
    Tcl_CmdProc *text_wdgcmd;

    char    *linebuf;
    int linebuf_pos, linebuf_size = 1024;

    char    *data;
    int data_pos, data_size = 1024;
    int len;
    char    *tag_name;
    char    *imageptr;
    unsigned int attr;
    
    Tcl_Obj *objlist, *next;
    int objlistc, oi;
    
    int wargc;
    char *wargv[12];
    int fld_cou;
    int j, fnd1, fnd2;
    char **flds;
    char *p, * base_classes_of, * sub_classes_of, * viewed_classes;
    char *browsed_class;
    int overridden;
    unsigned int filter, filter1;
    int flags_and;

    char **prev_flds=NULL, **actu_flds=NULL, **next_flds=NULL;

    if (argc < 13 || argc > 14)
    {
	Tcl_AppendResult(interp, "wrong # args:  should be ",
	    Tcl_GetString(objv[0]),
	    " ?-delete? textwidget list base_class_tree"
	    " sub_class_tree viewed_classes"
	    " overridden filter "
	    " protected_font private_font, public_font"
	    " browsed_class, and/or"
	    ,
	    NULL);

	return TCL_ERROR;
    }

    if (Tcl_GetString(objv[1])[0] == '-')
    {
	del = TRUE;
	argc--;
	objv++;
    }
    else
	del = FALSE;
	
    textwid         = Tcl_GetString(objv[1]);        /* tree pathname */
    objlist         = objv[2];        /* list of entries */

    base_classes_of = Tcl_GetString(objv[3]);        /* base classes filter */
    sub_classes_of  = Tcl_GetString(objv[4]);        /* sub classes filter */
    viewed_classes  = Tcl_GetString(objv[5]);        /* list of viewed classes */
    overridden      = atoi (Tcl_GetString(objv[6])); /* overridden flag */
    filter          = atoi (Tcl_GetString(objv[7])); /* member filter */
    filter1         = filter&(~(PAF_OVERRIDE|PAF_OVERLOADED)); /* flags without group flags */

    public_font     = Tcl_GetString(objv[8]);        /* font for public members */
    protected_font  = Tcl_GetString(objv[9]);        /* font for protected members */
    private_font    = Tcl_GetString(objv[10]);       /* font for private members */
    browsed_class   = Tcl_GetString(objv[11]);       /* browsed class in the browser */
    flags_and       = atoi(Tcl_GetString(objv[12])); /* Flag if all flags must be seted */

    if (!Tcl_GetCommandInfo(interp, textwid, &infoPtr))
    {
	Tcl_AppendResult(interp, "unknown widget \"",
	    textwid,"\"",NULL);
	return TCL_ERROR;
    }

    textPtr = (void*)infoPtr.clientData;
    text_wdgcmd = (Tcl_CmdProc *)infoPtr.proc;

    /* set widget state as normal */
    wargc = 0;
    wargv[wargc++] = textwid;
    wargv[wargc++] = "configure";
    wargv[wargc++] = "-state";
    wargv[wargc++] = "normal";
    (*text_wdgcmd)((ClientData)textPtr,interp,wargc,wargv);

    /* delete old items */
    if (del)
    {
	wargc = 0;
	wargv[wargc++] = textwid;
	wargv[wargc++] = "delete";
	wargv[wargc++] = "0";
	wargv[wargc++] = "end";

	(*text_wdgcmd)((ClientData)textPtr,interp,wargc,wargv);
    }

    if (Tcl_ListObjLength(interp, objlist, &objlistc) != TCL_OK)
    {
        return TCL_ERROR;
    }
    if (objlistc == 0)
    {
	return TCL_OK;
    }

    /* using of dynamic buffers */
    linebuf = ckalloc (linebuf_size);
    data    = ckalloc (data_size);

    /* options for inserting items */
    wargc = 0;
    wargv[wargc++] = textwid;
    wargv[wargc++] = "insert";
    wargv[wargc++] = "end";
    wargv[wargc++] = "-image";
    wargv[wargc++] = image;
    wargv[wargc++] = "-font";
    wargv[wargc++] = font;
    wargv[wargc++] = "-data";
    wargv[wargc  ] = data; data_pos = wargc++;
    wargv[wargc++] = "-text";
    wargv[wargc  ] = linebuf; linebuf_pos = wargc++;


    for (j=0, oi=0; oi<=objlistc; j++, oi++)
    {
	/* line scanning is complicated, because at least two lines are
	 * to be stored to compare for overloaded and overridden flags
	 */
	if (oi == objlistc)
	{
	    if (j > 1)
	    {
		if (prev_flds)
		{
		    ckfree ((char *) prev_flds);
		}
		prev_flds = actu_flds;
		actu_flds = next_flds;
		next_flds = NULL;
	    }
	    if (actu_flds == NULL)
	    {
		break;
	    }
	}
	else
	{
	    if (Tcl_ListObjIndex (interp, objlist, oi, &next) != TCL_OK)
	    {
		continue;
	    }
	    if (Tcl_SplitList(interp, Tcl_GetString(next), &fld_cou, &flds) != TCL_OK)
	    {
		continue;
	    }
	
	    if (fld_cou < LIST_CNT)
	    {
		ckfree((char *)flds);
		continue;
	    }
	    if (actu_flds == NULL)
	    {
		actu_flds = flds;
		continue;
	    }
	    if (next_flds == NULL)
	    {
		next_flds = flds;
	    }
	    else
	    {
		if (prev_flds)
		{
		    ckfree ((char *) prev_flds);
		}
		prev_flds = actu_flds;
		actu_flds = next_flds;
		next_flds = flds;
	    }
	}
	if (Tcl_GetInt(interp, actu_flds[ATTR_POS],(int *)&attr) != TCL_OK)
	{
	    continue;
	}

 	/* verify if the class is selected */
	p = Tcl_GetVar2 (interp, viewed_classes, CLASS(actu_flds[CLASS_POS]), TCL_LIST_ELEMENT);
	if (p != NULL && atoi (p) == 0)    /* class not selected */
	{
	    continue;
	}
	
	/* if filter enabled, view only selected member types */
	if (filter1)
	{
	    if (flags_and)
	    {
		if ((filter1&attr)!=filter1)
		{
		    continue;
		}
	    }
	    else
	    {
		int cnt = 0;
		if ((filter1&PAF_STATIC    )!=0 && (attr&PAF_STATIC    )!=0) cnt++;
		if ((filter1&PAF_STRUCT_DEF)!=0 && (attr&PAF_STRUCT_DEF)!=0) cnt++;
		if ((filter1&PAF_INLINE    )!=0 && (attr&PAF_INLINE    )!=0) cnt++;
		if ((filter1&PAF_VIRTUAL   )!=0 && (attr&PAF_VIRTUAL   )!=0) cnt++;
		if ((filter1&PAF_PUREVIRTUAL)==PAF_PUREVIRTUAL && (attr&PAF_PUREVIRTUAL)==PAF_PUREVIRTUAL) cnt ++;
		if (cnt == 0)
		{
		    continue;
		}
	    }
	}

	/* verif if overloaded flag is enabled */
	if (filter & PAF_OVERLOADED)
	{
	    if ((prev_flds && strcmp (actu_flds[MEMBER_POS], prev_flds[MEMBER_POS]) == 0) ||
	        (next_flds && strcmp (actu_flds[MEMBER_POS], next_flds[MEMBER_POS]) == 0))
	    {
	    }
	    else
	    {
		continue;
	    }
	}
	
	/* we need this to build correct image name */
	strcpy (image, "cls_br_");
     	imageptr = image+7;
	if (attr & PAF_PROTECTED)
	{
	   *imageptr++ = 'p';
	}
	if (attr & PAF_STATIC)
	{
	    *imageptr++ = 's';
	}
	if (attr & PAF_VIRTUAL)
	{
	    *imageptr++ = 'v';
	}
	
       /* verify if the member overides a member on the base method
         * or is being overridden by a sub class
         */
         fnd1 = fnd2 = 0;
	/* override flag */
	if (next_flds &&
	    strcmp (next_flds[MEMBER_POS], actu_flds[MEMBER_POS]) == 0 &&
	    strcmp (CLASS(next_flds[CLASS_POS]), CLASS(actu_flds[CLASS_POS])) != 0 && /* different classes */
	    strcmp (next_flds[PARAM_POS ], actu_flds[PARAM_POS ]) == 0)
	{
	    *imageptr++ = OVERRIDE;
	    fnd1 = 0;
	}
	
	/* overridden flag */
	if (prev_flds &&
	    strcmp (prev_flds[MEMBER_POS], actu_flds[MEMBER_POS]) == 0 &&
	    strcmp (CLASS(prev_flds[CLASS_POS]), CLASS(actu_flds[CLASS_POS])) != 0 && /* different classes */
	    strcmp (prev_flds[PARAM_POS ], actu_flds[PARAM_POS ]) == 0)
	{
	    *imageptr++ = OVERRIDDEN;
	    fnd2 = 1;
	}
	
	/* if we don't view the overridden members
	 * or when we view only override/overridden members
	 */
	if ((fnd2 && overridden == 0) ||
	     ((filter & PAF_OVERRIDE) && fnd1 == 0 && fnd2 == 0))
	{
	    continue;
	}

        /* A private member uses a special empty image */
        if (attr & PAF_PRIVATE) {
            strcpy (image, "cls_br_private_image");
        } else {
            /* finish image name */
            strcpy (imageptr, "_image");
        }

	/* make text */
	tag_name= strchr(actu_flds[MEMBER_POS],'(');
	/* function */
	if (tag_name && (strncmp(tag_name + 1,"md",2) == 0 ||
	    strncmp(tag_name + 1,"fr",2) == 0))
	{
	    if (tag_name[1] == 'f')      /* Friend use the private tag. */
	    {
		attr &= ~(PAF_PUBLIC|PAF_PROTECTED); 
	    }
	
	    /* using dynamic buffers */
	    len = strlen (actu_flds[MEMBER_POS]) +
		    strlen (actu_flds[CLASS_POS]) +
		    strlen (actu_flds[TYPE_POS]) +
		    strlen (actu_flds[PARAM_POS]) + 6;
	    if (len > linebuf_size)
	    {
		linebuf_size += len;
		linebuf = ckrealloc (linebuf, linebuf_size);
		wargv[linebuf_pos] = linebuf;
	    }
	    sprintf(linebuf,"%s\t%s\t%s\t(%s)",
		    actu_flds[MEMBER_POS],
		    actu_flds[CLASS_POS],
		    actu_flds[TYPE_POS],
		    actu_flds[PARAM_POS]);
	}
	/* variable */
	else
	{
	    /* using dynamic buffers */
	    len = strlen (actu_flds[MEMBER_POS]) +
		    strlen (actu_flds[CLASS_POS]) +
		    strlen (actu_flds[TYPE_POS]) + 3;
	    if (len > linebuf_size)
	    {
		linebuf_size += len;
		linebuf = ckrealloc (linebuf, linebuf_size);
		wargv[linebuf_pos] = linebuf;
	    }
	    sprintf(linebuf,"%s\t%s\t%s",
		    actu_flds[MEMBER_POS],
		    actu_flds[CLASS_POS],
		    actu_flds[TYPE_POS]);
	}
	
	/* using dynamic buffers */
	len = strlen (actu_flds[FILENAME_POS]) + strlen (actu_flds[FILEPOS_POS]) + 2;
	if (len > data_size)
	{
	    data_size += len;
	    data = ckrealloc (data, data_size);
	    wargv[data_pos] = data;
	}
	/* Add file name and position in the data section */
	sprintf (data, "%s\t%s", actu_flds[FILENAME_POS], actu_flds[FILEPOS_POS]);
	
	if (attr & PAF_PUBLIC)
	    strcpy (font, public_font);
	else if (attr & PAF_PROTECTED)
	    strcpy (font, protected_font);
	else if (attr & PAF_PRIVATE)
	    strcpy (font, private_font);
	
	/*
	 * Add line to browser list
	 */
	(*text_wdgcmd)((ClientData)textPtr,interp,wargc,wargv);	/* Insert ! */
    }

    /* free dynamic buffers */
    ckfree ((void*)linebuf);
    ckfree ((void*)data);

    if (prev_flds) ckfree ((void*)prev_flds);
    if (actu_flds) ckfree ((void*)actu_flds);
    if (next_flds) ckfree ((void*)next_flds);

    return TCL_OK;
}	

/*************************************************************************
 * Implements indent/outdent
 */
/*
 * Replace remaining blanks that fit into tabs with tabs.
 *
 * Example:
 * S=space, T=Tab
 *
 *  1234123412341234123412341234
 * "SS TS  T   TSS TSSSSSSSSSSS"
 * =result=>
 * "   T   T   T   T   T   TSSS"
 *
 * return:
 * pointer to end of string.
 */
static char *
filter_spaces_from_tabs (int tabsize, char *str)
{
    char *p, *q;
    int cnt;

    for (p=q=str, cnt=0; *p; p++)
    {
	if (*p == '\t')
	{
	    q -= cnt%tabsize;
	    *q ++ = *p;
	    cnt += tabsize - cnt%tabsize;
	}
	else
	{
	    *q ++ = *p;
	    cnt ++;
	    if ((cnt%tabsize) == 0)
	    {
		q -= tabsize;
		*q ++ = '\t';
	    }
	}
    }
    *q = 0;

    return q;
}

int
sn_indent_outdent (ClientData clientData, Tcl_Interp *interp, int argc, Tcl_Obj *objv[])
{
    char *txt;
    int indent    = 1;
    int indentsize= 8;
    int tabsize   = 8;
    int repl_tabs = 0;
    char *group   = "";
    int i, j;
    char *result, *p, *q, *beg;
    char tabstr[64], lastchar;
    int error = 0;
    Tcl_Obj *cmd = NULL;

    Tcl_Obj    *resultPtr;

    for (i=1; i<argc; i++)
    {
	if (strcmp ("-indent", Tcl_GetStringFromObj (objv[i], NULL)) == 0)
	{
	    i++;
	    if (strcmp (Tcl_GetStringFromObj (objv[i], NULL), "indent") == 0)
		indent = 1;
	    else
		indent = 0;
	}
	else if (strcmp ("-indentwidth", Tcl_GetStringFromObj (objv[i], NULL)) == 0)
	{
	    i++;
	    indentsize = atoi (Tcl_GetStringFromObj (objv[i], NULL));
	}
	else if (strcmp ("-tabwidth", Tcl_GetStringFromObj (objv[i], NULL)) == 0)
	{
	    i++;
	    tabsize    = atoi (Tcl_GetStringFromObj (objv[i], NULL));
	}
	else if (strcmp ("-replacetabs", Tcl_GetStringFromObj (objv[i], NULL)) == 0)
	{
	    i++;
	    repl_tabs  = atoi (Tcl_GetStringFromObj (objv[i], NULL));
	}
	else if (strcmp ("-group", Tcl_GetStringFromObj (objv[i], NULL)) == 0)
	{
	    i++;
	    group  = Tcl_GetStringFromObj (objv[i], NULL);
	}
	else if (strcmp ("-command", Tcl_GetStringFromObj (objv[i], NULL)) == 0)
	{
	    i++;
	    cmd  = objv[i];
	}
	else if (strcmp ("--", Tcl_GetStringFromObj (objv[i], NULL)) == 0)
	{
	    break;
	}
	else if (Tcl_GetStringFromObj (objv[i], NULL)[0] != '-')
	{
	    break;	/* end of argument list */
	}
	else
	{
	    error = 1;
	    break;
	}
    }
    if (error || i >= argc)
    {
	Tcl_WrongNumArgs (interp, 1, objv,
			     "?-indent indent|outdent? "
			     "?-indentwidth value? "
			     "?-tabwidth value? "
			     "?-replacetabs yes/no? "
			     "?-group group? "
			     "?-command command? "
			     "string");
	return TCL_ERROR;
    }
    txt = Tcl_GetStringFromObj (objv[i], NULL);

    /*
     * empty string */
    if (*txt == 0)
	return TCL_OK;

    resultPtr = Tcl_NewObj();
    result = ckalloc (8 + strlen (txt) * 8); result[0] = 0;

    /*
     * init a tabstring, based on the tabsize */
    for (i=0; i<tabsize; i++)
    {
	tabstr[i] = ' ';
    }
    tabstr[i] = 0;

    /*
     * Indent is pretty easy */
    if (indent)
    {
	for (p=txt, q=result; *p; p++)
	{
	    /*
	     * In c++ mode skip macro lines beginning with "#" */
	    if (*p == '#' && strcmp (group, "c++") == 0)
	    {
		while (*p && *p != '\n')
		{
		    *q ++ = *p++;
		}
		*q ++ = *p;
		continue;
	    }

	    beg = q;
	    /*
	     * copy remaining spaces/tabs */
	    while (*p == ' ' || *p == '\t')
	    {
		*q ++ = *p ++;
	    }
	
	    /*
	     * skip empty lines, even the lines contain only spaces or/and tabs */
	    if (*p == '\n' || *p == 0)
	    {
		*q ++ = *p;
		if (*p == 0)
		    break;
		continue;
	    }
	
	    /*
	     * insert indention */
	    if (repl_tabs == 0 && tabsize == indentsize)
	    {
		*q++ = '\t';
	    }
	    else
	    {
		for (j=0; j<indentsize; j++)
		    *q++ = ' ';
	    }
	    *q = 0;
	
	    /*
	     * replace spaces with tabs, if there are more spaces
	     * following with tab size */
	    if (! repl_tabs)
	    {
		q = filter_spaces_from_tabs (tabsize, beg);
	    }
	
	    /*
	     * copy the rest of the line */
	    while (*p && *p != '\n')
	    {
		*q ++ = *p++;
	    }
	    *q ++ = *p;
	
	    if (*p == 0)
		break;
	}
	*q = 0;
	goto done;
    }

    /*
     * do outdenting */
    for (p=txt, q=result; *p;)
    {
	beg = q;
	/*
	 * skip leading ' ' and '\t' */
	while (*p == ' ' || *p == '\t')
	    *q ++ = *p ++;
	*q = 0;
	
	/*
	 * what was the last character? */
	if (q > beg)
	    lastchar = *(q-1);
	else
	    lastchar = 0;
	
	/*
	 * translate too much blanks into tabs */
	if (! repl_tabs)
	{
	    q = filter_spaces_from_tabs (tabsize, beg);
	}
	
	/*
	 * go back on step */
	if (!repl_tabs && tabsize == indentsize && lastchar == '\t')
	{
	    q --;
	}
	else if (lastchar > 0)
	{
	    /*
	     * go back so many indent size */
	    for (i=0; i<=indentsize; i++)
	    {
		if (*q == '\t')
		{
		    /*
		     * replace tab with so many spaces like the tabsize
		     * last tab must be even replaced  */
		    memmove (q+tabsize, q+1, strlen (q));
		    strncpy (q, tabstr, tabsize);
		    q += tabsize-1;
		}
		if (i<indentsize && q > beg)
		    q --;
	    }
	}
	else
	{
	    /* line begins with strings */
	}
	
	/*
	 * copy rest of line */
	while (*p && *p != '\n')
	{
	    *q ++ = *p ++;
	}
	*q ++ = *p;
	*q = 0;
	
	if (*p != 0)
	{
	    p++;
	}
    }

done:
    Tcl_SetStringObj (resultPtr, result, -1);
    Tcl_SetObjResult (interp, resultPtr);
    ckfree (result);
    return TCL_OK;
}


