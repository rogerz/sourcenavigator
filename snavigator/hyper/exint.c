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
 * exint.c
 * 
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * Unknown.
 */

#include <stdlib.h>
#include <ctype.h>

#include "mxdefine.h"
#include "mxfuncs.h"
#include "mxlogger.h"

#include "tkText.h"

#include "sn.h"
#include "fileutils.h"

#ifndef MY_DEBUG2
#define MY_DEBUG(x)
#define MY_DEBUG2(x)
#define Output
#endif

int my_SplitList (char *str, int *num, char ***argvPtr, char sep);

static    int
brace_backslash(cur, cont)
char    *cur;
char    *cont;
{
    if ((cur - 1) >= cont && *(cur - 1) == '\\' &&
    ((cur - 2) < cont || *(cur - 2) != '\\'))
    return TRUE;

    return FALSE;
}

/* 
 * ----------------------------------------------------------------------------
 *
 * brace_balance
 *
 * Results: Offset of the matching parent. 
 *
 * -----------------------------------------------------------------------------
*/

int
brace_balance(ClientData clientData, Tcl_Interp *interp, int argc, char **argv)
{
    char    *cont;
    int	forw;
    char    open_brace;
    char    close_brace;
    int	lev = 0;
    char    *p;

    if (argc != 5)
    {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	       " string direction open_brace close_brace\"", (char *) NULL);
	return TCL_ERROR;
    }

    cont = argv[1];
    forw = atoi(argv[2]);
    open_brace = argv[3][0];
    close_brace = argv[4][0];

    if (forw)
    p = cont;
    else
    p = &cont[strlen(cont) - 1];
    for (; *p && p >= cont;)
    {
	if (*p == open_brace && !brace_backslash(p, cont))
	{
	    lev++;
	}
	else if (*p == close_brace && !brace_backslash(p, cont))
	{
	    if (--lev <= 0)
	    {
		if (forw)
		p++;
		sprintf(interp->result, "%d", p - cont);
		return TCL_OK;
	    }
	}

	if (forw)
	    p++;
	else
	    p--;
    }
    sprintf(interp->result, "%d", -1);
    return TCL_OK;
}

int
sn_db_format_qry(ClientData clientData,Tcl_Interp *interp,int argc,Tcl_Obj *CONST objv[])
{
    char	*recp;
    int	cou;
    char	*cls;
    char	*sym;
    char	*type;
    int	buflen = 1000;
    char	*fmtbuf;
    int	listLen;
    Tcl_Obj    *res_obj;
    Tcl_Obj **elemPtrs;
    int	c;
    int     num;
    char    **fields;

    fmtbuf = ckalloc(buflen);
    if (!fmtbuf)
    {
	return TCL_ERROR;
    }

    if (Tcl_ListObjGetElements(interp, objv[1], &listLen, &elemPtrs) != TCL_OK)
	return TCL_ERROR;

    Tcl_ResetResult(interp);
    res_obj = Tcl_GetObjResult(interp);
    for (c = 0; c < listLen; c++)
    {
	recp = Tcl_GetStringFromObj(elemPtrs[c], &cou);
	if (cou + 4 > buflen)
	{
	    buflen = cou + 4;    /* Keep some space for the brackets! */
	    fmtbuf = ckrealloc(fmtbuf,buflen);
	    if (!fmtbuf)
		return TCL_ERROR;
	}

	MY_DEBUG2((Output, "paf_qry_format <%s>\n", recp));

	/*
	 * Analyze string in form of
	 *
	 *     class symbol scope
	 */
	my_SplitList (recp, &num, &fields, ' ');
	if (num < 3)
	{
	    ckfree ((char*)fields);
	    continue;
	}
	cls  = fields[0];
	sym  = fields[1];
	type = fields[2];

	if (*cls == '#')
	{
	    sprintf(fmtbuf, "%s(%s)", sym, type);
	}
	else
	{
	    sprintf(fmtbuf, "%s(%s) %s", sym, type, cls);
	}

	Tcl_ListObjAppendElement (interp, res_obj, Tcl_NewStringObj (fmtbuf, -1));
	
	ckfree ((char*)fields);
    }
    ckfree(fmtbuf);

    LOGGER2((LOGFP,"%s returns <%s>\n",
	Tcl_GetStringFromObj(objv[0],(int *) NULL),
	Tcl_GetStringFromObj(res_obj,(int *) NULL)));

    return TCL_OK;
}

static  void
trim_text_index(char*pos, char*inp, int digit)
{
    char *p = pos;
    int not_empty;

    /* skip leading zeros !*/
    for (; isspace(*inp) || *inp == '0'; inp++);

    /*
     * Copy the line value !*/
    for (not_empty = 0; *inp && *inp != '.'; inp++)
    {
	if ((digit && isdigit(*inp)) || (digit == 0 && ! isspace(*inp)))
	{
	    not_empty = 1;
	    *p++ = *inp;
	}
    }

    /* line number should have at least one digit */
    if (not_empty == 0)
    {
	*p ++ = '1';
    }

    /* No column is specified, take first position (0) */
    if (*inp == '\0')
    {
	*p++ = '.';
	*p++ = '0';
	*p = '\0';
	return;
    }

    /* Copy the dot */
    *p++ = *inp++;

    /*
     * skip leading zeros and spaces*/
    for (; *inp == '0' || isspace(*inp); inp++);

    /* Copy the column value including !*/
    for (not_empty = 0; *inp && *inp != '.'; inp++)
    {
	if ((digit && isdigit(*inp)) || (digit == 0 && ! isspace(*inp)))
	{
	    not_empty = 1;
	    *p++ = *inp;
	}
    }
    /* at least one digit */
    if (not_empty == 0)
    {
	*p ++ = '0';
    }

    *p = '\0';
}

int
tk_trim_text_index(ClientData clientData,Tcl_Interp *interp,int argc,char **argv)
{
    char pos[100];
    char *digit;
    int only_digit = 0;

    if (argc < 2)
    {
	Tcl_AppendResult(interp, "wrong # args:  should be ",
	    argv[0]," index", NULL);

	return TCL_ERROR;
    }
    if (argc > 2)
    {
	if (strcmp (argv[1], "-digit") == 0)
	    only_digit = 1;
	digit = argv[2];
    }
    else
    {
	digit = argv[1];
    }

    trim_text_index (pos, digit, only_digit);

    Tcl_AppendResult (interp, pos, NULL);

    return TCL_OK;
}

/****************************************************************
 * Procedure to insert files into a treetable with building
 * the tree hierarchy
 ****************************************************************/
typedef struct
{
  char parent[20];
  char    dirparent[20];
  char *file_argv[15];
  char *dir_argv[15];
  int dir_argc;
  int file_argc;
  char    filename[MAXPATHLEN];
  char    dirname[MAXPATHLEN];
  char    *dir_bitm_name;
  char    *file_bitm_name;
  char    *tbl_name;
  Tcl_CmdProc *tbl_cmd;
  ClientData tblp;
  char    **fileargv;
  int    list_size;
  int file_cnt;
  int basedir;
} FillParams_t;

static char *
FirstBaseDir (char *pth)
{
    static char name[256];
    char *p=pth, *q=name;
    int fnd = 0;

    /* ignore first '/', if availiable ["/home/foo.c" returns "/home"] */
    if (IS_ROOT (*p))
    {
	*q ++ = *p ++;
	fnd = 1;
    }
    while (*p && ! IS_ROOT (*p))
    {
	*q++ = *p ++;
    }

    /* we can only have filenames at the end of path
     * we don't have pathes like "/usr", but "/foo.c"
     */
    if (fnd && ! IS_ROOT (*p)) {
	name[0] = 0;
    } else {
	*q = 0;
    }
    return name;
}

static int
fill_recursive (Tcl_Interp *interp, int idx, char *prefix, int list_size, char**fileargv, FillParams_t *fp)
{
    char **filelist, olddir[MAXPATHLEN], maindir[MAXPATHLEN];
    int i, prefixlen, newidx = 0;
    int fnd;
#if _WINDOWS
    int changed;
#endif

    prefixlen = strlen (prefix);

    sprintf (fp->dirparent, "%i", idx);
    sprintf (fp->parent, "%i", idx);

    /* draw files */
    fnd = 0;
    for (i = list_size, filelist = fileargv; i > 0; filelist++, i--)
    {
	if (strncmp (prefix, filelist[0], prefixlen) == 0)
	{
	    fnd = 1;
	    /*
	     * skip '/', if given as root "/foo.c"
	     */
	    if ((! IS_ROOT (filelist[0][0]) && file_firstroot (filelist[0] + prefixlen)) ||
	        (IS_ROOT (filelist[0][0]) && file_firstroot (filelist[0] + 1 + prefixlen)))
	    {
		continue; /* still a directory */
	    }
	    strcpy(fp->filename, filelist[0] + prefixlen);
	    if (*fp->filename == 0)
	    {
	        continue;
	    }
	    /*
	     * Add entry to the tree
	     */
	    (*fp->tbl_cmd)(fp->tblp, interp, fp->file_argc, fp->file_argv);
	
	    fp->file_cnt ++;
	}
	else if (fnd)
	{
	    /* at this position, the prefix was found and all of it's
	     * suffixes have been already handled, we don't need to 
	     * continue searching so we can make the process a bit
	     * faster ansted O(n^2)
	     */
	    break;
	}
    }

    /* draw sub directories */
    fnd = 0;
    olddir[0] = 0;
    for (i = list_size, filelist = fileargv; i > 0; filelist++, i--)
    {
	if (strncmp (prefix, filelist[0], prefixlen) == 0)
	{
	    fnd = 1;
	    if (! file_firstroot (filelist[0] + prefixlen))
	    {
		continue; /* it's a file, skip */
	    }
	
	    strcpy(fp->dirname, FirstBaseDir (filelist[0] + prefixlen));
	
	    if (*fp->dirname == 0 ||
		((*olddir == *fp->dirname && strcmp (olddir, fp->dirname) == 0)))
	    {
		continue; /* empty string or directory is already added */
	    }

#if _WINDOWS
	    /*
	     * On windows [file join C: foo] = "C:foo", bogus
	     *            [file join C:/ foo] = "C:\foo" correct
	     */
	    if (prefixlen == 0 && fp->dirname[1] == ':' && fp->dirname[2] == 0)
	    {
		fp->dirname[2] = '/';
		fp->dirname[3] = 0;
		changed = 1;
	    }
	    else
	    {
		changed = 0;
	    }
#endif
	
	    /*
	     * Add directory to the tree
	     */
	    (*fp->tbl_cmd)(fp->tblp, interp, fp->dir_argc, fp->dir_argv);
	
#if _WINDOWS
	    /*
	     * Restore the state
	     */
	    if (changed)
	    {
		fp->dirname[2] = 0;
	    }
#endif

	    if (interp->result)
	    {
		newidx = atoi (interp->result);
	    }
	    strcpy (olddir, fp->dirname);
	
	    /* call procedure recursivly */
	    /*sprintf (maindir, "%s%s%s", prefix, prefix[0] ? "/" : "", dirname);*/
	    sprintf (maindir, "%s%s/", prefix, fp->dirname);
	    fill_recursive (interp,    newidx, maindir, i, filelist, fp);
	
	    /* reset parent pointers, because of global variables */
	    sprintf (fp->dirparent, "%i", idx);
	    sprintf (fp->parent, "%i", idx);
	}
	else if (fnd)
	{
	    /* at this position, the prefix was found and all of it's
	     * suffixes have been already handled, we don't need to 
	     * continue searching so we can make the process a bit
	     * faster ansted O(n^2)
	     */
	    break;
	}
    }

    return idx;
}

/*
 * Given a list of files with there base directories
 * Add the files into a treetable by building the
 * directory hierarchy
 * This algorithm needs O(n*k) to proceed.
 * n <=> number of files
 * k <=> the depth of the directory hierarchy
 */
int
fill_file_tree(ClientData clientData,
		Tcl_Interp *interp, /* Current interpreter. */
		int argc,           /* Number of arguments. */
		char **argv)        /* Argument strings.    */
{
    Tcl_CmdInfo infoPtr;
    FillParams_t fp;
    char *contents=NULL;
    int parent = -1, i, error=0;
    
    /* make some inits to the procedure */
    memset (&fp, 0, sizeof (fp));
    fp.file_bitm_name = "file_image";
    fp.dir_bitm_name = "dir_image";
    
    /* parse the options */
    for (i=1; i<argc-1; i++)
    {
	int len = strlen (argv[i]);
	if (strncmp (argv[i], "-widget", len) == 0)
	{
	    i++; fp.tbl_name = argv[i];
	}
	else if (strncmp (argv[i], "-contents", len) == 0)
	{
	    i++; contents = argv[i];
	}
	else if (strncmp (argv[i], "-parent", len) == 0)
	{
	    i++; parent = atoi (argv[i]);
	}
	else if (strncmp (argv[i], "-fileimage", len) == 0)
	{
	    i++; fp.file_bitm_name = argv[i];
	}
	else if (strncmp (argv[i], "-dirimage", len) == 0)
	{
	    i++; fp.dir_bitm_name = argv[i];
	}
	else if (strncmp (argv[i], "-basedir", len) == 0)
	{
	    i++;
	    if (argv[i][0] == 'y' || argv[i][0] == '1')
	    {
		fp.basedir = 1; /* display basedirectory on the column list */
	    }
	    else
	    {
		fp.basedir = 0; /* don't display basedirectory on the column list */
	    }
	}
	else
	{
	    error = 1;
	    break;
	}
    }

    if (error || fp.tbl_name == NULL || contents == NULL)
    {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	     " -widget treewidget -contents list ?-fileimage image? ?-dirimage image? ?-parent parent?\"",
	     (char *) NULL);
	return TCL_ERROR;
    }
    if (!Tcl_GetCommandInfo(interp, fp.tbl_name, &infoPtr))
    {
	Tcl_AppendResult(interp, "wrong # \"",
	    fp.tbl_name, "\" does not exist", (char *) NULL);
	return TCL_ERROR;
    }

    if (Tcl_SplitList(interp, contents, &fp.list_size, &fp.fileargv) != TCL_OK)
	return TCL_ERROR;

    fp.tblp = infoPtr.clientData;
    fp.tbl_cmd = (Tcl_CmdProc *)infoPtr.proc;

    fp.file_argc = 0;
    fp.file_argv[fp.file_argc++] = fp.tbl_name;
    fp.file_argv[fp.file_argc++] = "insert";
    fp.file_argv[fp.file_argc++] = "end";
    fp.file_argv[fp.file_argc++] = "-image";
    fp.file_argv[fp.file_argc++] = fp.file_bitm_name;
    fp.file_argv[fp.file_argc++] = "-text";
    fp.file_argv[fp.file_argc++] = fp.filename;
    fp.file_argv[fp.file_argc++] = "-parent";
    fp.file_argv[fp.file_argc++] = fp.parent;

    fp.dir_argc = 0;
    fp.dir_argv[fp.dir_argc++] = fp.tbl_name;
    fp.dir_argv[fp.dir_argc++] = "insert";
    fp.dir_argv[fp.dir_argc++] = "end";
    fp.dir_argv[fp.dir_argc++] = "-image";
    fp.dir_argv[fp.dir_argc++] = fp.dir_bitm_name;
    fp.dir_argv[fp.dir_argc++] = "-text";
    fp.dir_argv[fp.dir_argc++] = fp.dirname;
    fp.dir_argv[fp.dir_argc++] = "-parent";
    fp.dir_argv[fp.dir_argc++] = fp.dirparent;


    /* New algorithm to add the files correctly into the treetable */
    fill_recursive (interp,    parent, "", fp.list_size, fp.fileargv, &fp);

    ckfree ((char *)fp.fileargv);
    return TCL_OK;
}

/*
 * This function implements the "tk_text_insert" Tcl-Tk
 * command. It is useful to insert large Tcl lists
 * into Tk text widgets.
 */
int
sn_tk_text_insert(ClientData clientData,
		Tcl_Interp *interp, /* Current interpreter. */
		int objc,           /* Number of arguments. */
		Tcl_Obj *CONST objv[])        /* Argument strings.    */
{
    char    *textw;
    TkTextIndex index1;
       register TkText *textPtr = NULL;
    int    i;
    int    listLen;
    int    strLen;
    char    *textargv[20];
    Tcl_CmdProc *text_wdgcmd;
    Tcl_Obj **elemPtrs;
    Tcl_CmdInfo     infoPtr;

    if (objc != 4)
    {
	Tcl_WrongNumArgs(interp, 1, objv, "widget index list");

	return TCL_ERROR;
    }
    textw = Tcl_GetStringFromObj(objv[1], NULL);
    if (!Tcl_GetCommandInfo(interp, textw, &infoPtr))
    {
	Tcl_AppendStringsToObj(Tcl_GetObjResult(interp),
	    "widget \"",
	    textw,
	    "\" does not exist",
	    NULL);

	return TCL_ERROR;
    }
    textPtr = (TkText *)infoPtr.clientData;
    text_wdgcmd = (Tcl_CmdProc *)infoPtr.proc;

    if (TkTextGetIndex(interp, textPtr,
	Tcl_GetStringFromObj(objv[2], NULL), &index1) != TCL_OK)
    {
	return TCL_ERROR;
    }

    if (Tcl_ListObjGetElements(interp, objv[3], &listLen, &elemPtrs) != TCL_OK)
    {
	return TCL_ERROR;
    }

    TkTextIndexForwChars(&index1,-1,&index1);
    for (i = 0; i < listLen - 1; i++)
    {
	TkBTreeInsertChars(&index1, Tcl_GetStringFromObj(elemPtrs[i], &strLen));
	TkTextIndexForwChars(&index1,strLen,&index1);
	TkBTreeInsertChars(&index1, "\n");
	TkTextIndexForwChars(&index1,1,&index1);
    }

    /* The last line is inserted this way, to get the internal staff
     * done too.
     */
    if (listLen)
    {
	i = 0;

	textargv[i++] = textw;
	textargv[i++] = "insert";
	textargv[i++] = "end";
	textargv[i++] = Tcl_GetStringFromObj(elemPtrs[listLen - 1], NULL);

	return (*text_wdgcmd)((ClientData)textPtr,interp,i,textargv);
    }
    return TCL_OK;
}

