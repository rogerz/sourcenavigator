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

/********************************************************************
 * recudir.c, Implements "sn_glob"
 * Copyright (C) 1998 Cygnus Solutions.

We have our own "glob" function because it allows the following
functionality:

- A very important functionality is, it doesn't get
  confused by symbolic links. It never adds a file
  more than one time, as example:
    ls -l .
        foo.c
        boo.c     -> foo.c
        f2.c      -> ./dir1/f1.c
        dir1/f1.c -> ../boo.c
        dir2      -> ../dir3
        dir3      -> .

  SN detects the links regardless of it's complexity and adds only one
  file from the above links. This is implemented in recudir.c.
- IDLE-functionality
        User can cancel the scanning process. This is very important
        we had alot of user-complains before this was not implemented.
- Supports extensions to specify several filters (*.[Cc] *.tcl ....)
        Used to scann all the files with extensions specified in the
        parsers.
- Ignore-functionality
        To ignore RCS, CVS, ...
- Returned results
        It returns the files and directories separated from each other.
        So we don't need to scan the result again to filter files from
        directories.
- Recursive
        To scan a directory recursive
- Fast
        Pretty fast to add a huge amount of files from the directory
        hierarchy. It scans thousands of files in short time.

The tcl/glob function acts similar like csh glob functionality,
what sounds for me to be less usable for an application.
**********************************************************************/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "mxdefine.h"
#include "mxlogger.h"

#include "tclInt.h"
#include "tcl.h"

#include "fileutils.h"

#ifndef MY_DEBUG2
#define MY_DEBUG(x)
#define MY_DEBUG2(x)
#define Output
#endif

#ifdef HAVE_DIRENT_H
#define HAVE_READLINK
#include <dirent.h>
#else
#ifdef _WIN32
#define S_ISDIR(m) (((m) & _S_IFDIR) != 0)

/* Directory entry (file) struct - Compatible with UNIX */
struct dirent
{
    unsigned long   d_fileno;
    unsigned short  d_namlen;
    char            d_name[_MAX_PATH+1];
};

typedef struct __dirdesc
{
    char          dir_name[_MAX_PATH+1];
    HANDLE        dir_handle;
    int           dir_fileCount;
    struct dirent dir_fileUnix;
#ifdef __MSVC__
    WIN32_FIND_DATA*    lpFindData;
    int		    found;
#endif /* ifdef __MSVC__ */
} DIR;

DIR*           opendir(char*);
struct dirent* readdir(DIR*);
void           rewinddir(DIR* );
int            closedir(DIR*);
long           telldir(DIR*);
void           seekdir(DIR*, long);

#endif /* _WIN32  */
#endif /* HAVE_DIRENT_H */

typedef struct _Glob_Type
{
    Tcl_Interp *interp;
    char *file;
    int show_dirs;
    int show_files;
    char *dir_suff;
    Tcl_Obj *dirlist;
    Tcl_Obj *filelist;
    int dirlevel;
    int showhidden;
    int ignored_dirs_argc;
    Tcl_Obj **ignored_dirs_argv;
    int match_argc;
    Tcl_Obj **match_argv;
    int rec_glob_count;
    int dircounter;
    Tcl_RegExp regexp_ptr;
    int all_dirs;
    Tcl_Obj *realdirlist;
    int nocomplain;

    Tcl_Obj *filevar;
    Tcl_Obj *dirvar;

    /* to avoid scanning all the found directories, even when
     * no symbolic links have been found yet */
    int link_found;

    /*
     * update command to refresh the screen
     */
    char * updatecommand;
    int initcommand;
} Glob_Type;

static char *updcmd = NULL;

#ifndef _WINDOWS
static int
scan_list (Glob_Type *rp, Tcl_Obj *dirlist, char *dir)
{
    Tcl_Obj *next = NULL;
    int i, num;
    if (dir == NULL || dir[0] == 0)
    {
	return 0;
    }
    if (Tcl_ListObjLength (rp->interp, dirlist, &num) != TCL_OK)
	return 0;
    for (i=0; i<num; i++)
    {
	if (Tcl_ListObjIndex (rp->interp, dirlist, i, &next) != TCL_OK)
	{
	    break;
	}
	if (strcmp (dir, Tcl_GetStringFromObj (next, NULL)) == 0)
	{
	    MY_DEBUG(("scan_list dir<%s> already scanned\n", dir));

	    return 1;     /* path already visited */
	}
    }
    return 0;
}
static int
path_already_scanned (Glob_Type *rp, char *dr, char *realp)
{
    MY_DEBUG(("path_already_scanned verify <%s> <%s>\n", dr, realp));

    /*
     * No symbolic links have been found yet, no search needed */
    if (!rp->link_found)
    {
	return 0;
    }
    /*
     * Test realpath list */
    if (scan_list (rp, rp->realdirlist, realp))
    {
	return 1;
    }
    if (scan_list (rp, rp->realdirlist, dr))
    {
	return 1;
    }

    /*
     * test alreay scanned directories */
    if (scan_list (rp, rp->dirlist, dr))
    {
	return 1;
    }
    if (scan_list (rp, rp->dirlist, realp))
    {
	return 1;
    }

    return 0; /* path wasn't visited */
}
#endif /* ifndef _WINDOWS */

static int
is_dir(char * dr, int *is_link, Glob_Type *rp)
{
    struct stat st;

#ifdef S_ISLNK
    if(lstat(dr, &st) == -1)
    {
	return FALSE;
    }

    if (S_ISLNK(st.st_mode))
    {
	stat(dr, &st);	/* Check whether it points to a directory. */
	*is_link = TRUE;
	
	/*
	 * flag to mark that we have now symbolic links and up now
	 * we must test every directory if it is duplicated */
	rp->link_found++;
    }
    else
	*is_link = FALSE;
#else
    if(stat(dr, &st) == -1)
    {
	return FALSE;
    }
    *is_link = FALSE;
#endif /* S_ISLNK */

    if (S_ISDIR (st.st_mode))
      {
	return TRUE;
    }

    return FALSE;
}

static int
recursive_glob(Glob_Type *rp)
{
    DIR * dir;
    struct dirent *d;
    char fbuf[MAXPATHLEN];
    int dirflg;
    int    fnd_file = FALSE;
    int    is_link = FALSE;
    Tcl_DString ignoredDir;
    Tcl_DString nativeStr;

    rp->rec_glob_count++;

    /*
     * Refresh the screen and execute some user-defined commands
     */
    if ((rp->rec_glob_count % 5) == 0)
    {
	Tcl_DoOneEvent(TCL_DONT_WAIT);    /* Refresh the screen ! */
    }

    if (rp->file[0] == '.' && IS_ROOT (rp->file[1]))
    {
	rp->file += 2;
    }

    dirflg = is_dir (rp->file, &is_link, rp);

    if (dirflg)
    {
	char dirbuf[MAXPATHLEN];
#ifndef _WINDOWS
	char realp[MAXPATHLEN];
#endif
	int    filecou = 0, scanned = 0;

	if (rp->ignored_dirs_argv)
	{
	    Tcl_Obj    **ip;
	    char    *dn;
	    char    *dirp = file_lastroot (rp->file);
	    int    cou;

	    if (dirp)
		dirp++;
	    else
		dirp = rp->file;

	    for (ip = rp->ignored_dirs_argv, cou = rp->ignored_dirs_argc; cou-- > 0; ip++)
	    {
		Tcl_UtfToExternalDString(NULL, Tcl_GetString(*ip), -1, &ignoredDir);
		dn = Tcl_DStringValue(&ignoredDir);

		if (*dn == *dirp && strcmp(dn,dirp) == 0)
		{
		    Tcl_DStringFree(&ignoredDir);
		    return fnd_file;
		}
		Tcl_DStringFree(&ignoredDir);
	    }
	}

	if (rp->showhidden || *rp->file != '.')
	{
	    sprintf (dirbuf, "%s%s", rp->file, rp->dir_suff);
	}
	else
	{
	    dirbuf[0] = 0;
	}

#ifndef _WINDOWS
	if (!is_link || ! realpath (rp->file, realp))
	{
	    realp[0] = 0;
	}
	/*
	 * Look if we already visited this directory! */
	if (path_already_scanned(rp, dirbuf, realp))
	{
	    scanned = 1; /* scan this directory */
	}
	
	/*
	 * Add it's realpath to the real path list to avoid recursion
	 * and duplications (NOW) */
	if (realp[0])
	{
	    char realdirbuf[MAXPATHLEN];
	    Tcl_DString realDir;

	    sprintf (realdirbuf, "%s%s", realp, rp->dir_suff);
	    Tcl_ExternalToUtfDString(NULL, realdirbuf, -1, &realDir);
	    Tcl_ListObjAppendElement(rp->interp,rp->realdirlist,
		    Tcl_NewStringObj(Tcl_DStringValue(&realDir),
		    Tcl_DStringLength(&realDir)));
	    Tcl_DStringFree(&realDir);
		
	    MY_DEBUG(("add real path <%s>\n", realdirbuf));
	}
#endif /* ifndef _WINDOWS */

	/* When we create a project, we want to find only directories
	 * in that we have found at least one matching file it
	 * matching is required.
	 */
	if (dirbuf[0] && (filecou || rp->all_dirs))
	{
	    Tcl_DString dirbufDStr;

	    Tcl_ExternalToUtfDString(NULL, dirbuf, -1, &dirbufDStr);
	    Tcl_ListObjAppendElement(rp->interp,rp->dirlist,
		    Tcl_NewStringObj(Tcl_DStringValue(&dirbufDStr),
		    Tcl_DStringLength(&dirbufDStr)));
	    Tcl_DStringFree(&dirbufDStr);
	}
	
	/*
	 * display current scanned directory
	 */
	if (rp->updatecommand != NULL && ((rp->dircounter ++ % 3) == 0))
	{
	    Tcl_DString utfFileName;

	    int res;
	    if (updcmd == NULL)
	    {
		updcmd = ckalloc (MAXPATHLEN*2);
	    }
	    /*
	     * if the initial-phase hasn't been called, make it yet happen
	     */
	    if (!rp->initcommand)
	    {
		sprintf (updcmd, "%s init", rp->updatecommand);
		Tcl_Eval (rp->interp, updcmd);
		rp->initcommand = 1;
	    }
	    Tcl_ExternalToUtfDString(NULL, rp->file, -1, &utfFileName);
	    sprintf (updcmd, "%s dir \"%s\"", rp->updatecommand, Tcl_DStringValue(&utfFileName));
	    res = Tcl_Eval (rp->interp, updcmd);
	    Tcl_DStringFree(&utfFileName);
	    if (res == TCL_OK && Tcl_GetStringResult(rp->interp)[0] != '0')
	    {
		return -1;  /* canceled by user */
	    }
	}
	
	/*
	 * Don't scan resource directories, if showhidden is not true,
	 * ignore resource directories too */
	if (dirbuf[0] && !scanned && rp->dirlevel && (dir = opendir(rp->file)))
	{
	    char *oldfile;
	    while ((d = readdir(dir)) != NULL)
	    {
		/* skip "." and ".." */
		if (d->d_name[0] == '.' &&
		    (d->d_name[1] == '\0' || (d->d_name[1] == '.' && d->d_name[2] == 0)))
		{
		    continue;
		}
		/* skip resource directories (what's about Windows?)*/
		if (d->d_name[0] == '.' && !rp->showhidden)
		{
		    continue;
		}
		sprintf(fbuf,"%s/%s",rp->file,d->d_name);

		rp->dirlevel--;

		oldfile = rp->file;
		rp->file = fbuf;
		
		switch (recursive_glob(rp))
		{
		case 0:
		    break;		    /* do nothing */
		case -1:
		    return -1;		/* procedure has been canceled */
		default:
		    filecou++;		/* It is not a directory. */
		    break;
		}
		
		rp->file = oldfile;
		rp->dirlevel++;
	    }
	    closedir( dir);
	}
    }
    else if (rp->show_files && (rp->showhidden || rp->file[0] != '.'))
    {
	if (rp->regexp_ptr)
	{
	    Tcl_DString utfName;

	    Tcl_ExternalToUtfDString(NULL, rp->file, -1, &utfName);
	    switch (Tcl_RegExpExec(rp->interp,rp->regexp_ptr,rp->file,
		Tcl_DStringValue(&utfName)))
	    {
	    case -1:
	    case 0:                 /* Not found ! */
		break;

	    default:
		Tcl_ListObjAppendElement(rp->interp,rp->filelist,
			Tcl_NewStringObj(Tcl_DStringValue(&utfName),
			Tcl_DStringLength(&utfName)));
		fnd_file = TRUE;
		break;
	    }
	    Tcl_DStringFree(&utfName);
	}
	else if (rp->match_argv)
	{
	    Tcl_Obj    **mp;
	    char    *p;
	    int    cou;

	    Tcl_ExternalToUtfDString(NULL, rp->file, -1, &nativeStr);

	    for (mp = rp->match_argv, cou = rp->match_argc; cou-- > 0; mp++)
	    {
		p = Tcl_GetStringFromObj(*mp, NULL);
		if (Tcl_StringMatch(Tcl_DStringValue(&nativeStr),p))
		{
		    Tcl_ListObjAppendElement(rp->interp,rp->filelist,
			Tcl_NewStringObj(Tcl_DStringValue(&nativeStr),
			Tcl_DStringLength(&nativeStr)));
		    fnd_file = TRUE;
		    break;
		}
	    }
	    Tcl_DStringFree(&nativeStr);
	}
	else
	{
	    Tcl_ExternalToUtfDString(NULL, rp->file, -1, &nativeStr);
	    Tcl_ListObjAppendElement(rp->interp,rp->filelist,
		    Tcl_NewStringObj(Tcl_DStringValue(&nativeStr),
		    Tcl_DStringLength(&nativeStr)));
	    fnd_file = TRUE;
	    Tcl_DStringFree(&nativeStr);
	}
    }

    return fnd_file;
}

/*
 * Reimplements the tcl/standard glob procedure.
 */
int
sn_glob(ClientData clientData,Tcl_Interp *interp,int argc,Tcl_Obj *CONST objv[])
{
    char    *p;
    int    i;
    int    ret_code = TCL_ERROR;
    Tcl_Obj    *resultPtr;
    Glob_Type rec_params;
    Tcl_DString dirSufDStr;
    Tcl_DString fileVarDStr;
    Tcl_DString dirVarDStr;
    Tcl_DString fileNameDStr;

    Tcl_DStringInit(&dirSufDStr);
    Tcl_DStringInit(&fileVarDStr);
    Tcl_DStringInit(&dirVarDStr);

    /*
     * create a new object to return as result ansted to use an existing one
     * this will cause synchrnonization problems (core dumps) when a tcl/function
     * is called during this process */
    resultPtr = Tcl_NewObj();

    /*
     * Init structure to pass for recursive calling, rather than to
     * path therteen parameters to this function */
    memset (&rec_params, 0, sizeof(rec_params));
    rec_params.interp     = interp;
    rec_params.show_dirs  = TRUE;
    rec_params.show_files = TRUE;
    rec_params.dir_suff   = "";
    rec_params.dirlist    = NULL;
    rec_params.filelist   = NULL;
    rec_params.dirlevel   = -1;
    rec_params.showhidden = 0;
    rec_params.ignored_dirs_argc = 0;
    rec_params.ignored_dirs_argv = NULL;
    rec_params.match_argc = 0;
    rec_params.match_argv = NULL;
    rec_params.rec_glob_count = 0;
    rec_params.regexp_ptr = NULL;
    rec_params.all_dirs   = TRUE;
    rec_params.realdirlist = NULL;
    rec_params.updatecommand = NULL;

    for (i = 1; i < argc; i++)
    {
	p = Tcl_GetStringFromObj(objv[i], NULL);
	if (*p != '-')
	    break;

	if (strcmp(p,"-dirsuf") == 0 && i < argc -1)
	{
	    i++;
	    Tcl_UtfToExternalDString(NULL, Tcl_GetString(objv[i]), -1, &dirSufDStr);
	    if (Tcl_DStringLength(&dirSufDStr) == 0) 
	    {
		rec_params.dir_suff = NULL;
	    } else {
		rec_params.dir_suff = Tcl_DStringValue(&dirSufDStr);
	    }
	}
	else if (strcmp(p,"-dir") == 0)
	{
	    rec_params.show_dirs = FALSE;
	}
	else if (strncmp(p,"-no_empty",4) == 0)
	{
	    rec_params.all_dirs = FALSE;
	}
	else if (strcmp(p,"-file") == 0)
	{
	    rec_params.show_files = FALSE;
	}
	else if (strcmp(p,"-match") == 0 && i < argc -1)
	{
	    i++;
	    if (Tcl_ListObjGetElements(interp,objv[i],
		&rec_params.match_argc,&rec_params.match_argv) != TCL_OK)
	    {
		goto ret;
	    }
	    if (rec_params.match_argc == 0)
		rec_params.match_argv = NULL;
	}
	else if (strcmp(p,"-filevar") == 0 && i < argc -1)
	{
	    i++;
	    rec_params.filevar = objv[i];
	}
	else if (strcmp(p,"-dirvar") == 0 && i < argc -1)
	{
	    i++;
	    rec_params.dirvar = objv[i];
	}
	else if (strcmp(p,"-ignore") == 0 && i < argc -1)
	{
	    i ++;
	    if (Tcl_ListObjGetElements(interp,objv[i],
		&rec_params.ignored_dirs_argc,&rec_params.ignored_dirs_argv) != TCL_OK)
	    {
		goto ret;
	    }
	    if (rec_params.ignored_dirs_argc == 0)
		rec_params.ignored_dirs_argv = NULL;
	}
	else if (strcmp(p,"-showhidden") == 0)
	{
	    rec_params.showhidden = TRUE;
	}
	else if (strcmp(p,"-dirlevel") == 0 && i < argc -1)
	{
	    i ++;
	    if (Tcl_GetIntFromObj(interp,objv[i],&rec_params.dirlevel) != TCL_OK)
	    {
		goto ret;
	    }
	    if (rec_params.dirlevel == 0)
		rec_params.dirlevel = -1;
	}
	else if (strcmp(p,"-regexp") == 0 && i < argc -1)
	{
	    i ++;
	    p = Tcl_GetStringFromObj(objv[i], NULL);
	    if (*p)
	    {
		rec_params.regexp_ptr = Tcl_RegExpCompile(interp,p);
		if (!rec_params.regexp_ptr)
		{
		    goto ret;
		}
	    }
	}
	else if (strcmp (p, "-nocomplain") == 0)
	{
	    rec_params.nocomplain = 1;
	}
	else if (strcmp (p, "-updatecommand") == 0 && i < argc -1)
	{
	    i ++;
	    rec_params.updatecommand = Tcl_GetStringFromObj(objv[i], NULL);
	}
	else if (strcmp (p, "--") == 0)
	{
	    i++;
	    break;
	}
	else
	{
	    Tcl_Obj *errm;

	    errm = Tcl_NewStringObj("?-dirsuf? ?-dir? ?-file? ?-dirlevel level?",-1);
	    Tcl_AppendStringsToObj(errm,
		" ?-match pattern?",
		" ?-regexp pattern?",
		" ?-ignored_dirs list?",
		" ?-showhidden?",
		" ?-dirvar var? ?-filevar var? dir1 dir2 ...",
		NULL);

	    Tcl_AppendStringsToObj (errm,
		"unknown argument \"", p, "\"", NULL);

	    Tcl_WrongNumArgs(interp, 1, objv,Tcl_GetStringFromObj(errm, NULL));

	    Tcl_DecrRefCount(errm);

	    goto ret;
	}
    }

    rec_params.dirlist = Tcl_NewObj();
    rec_params.filelist = Tcl_NewObj();
    rec_params.realdirlist = Tcl_NewObj();

    for (; i < argc; i++)
    {
	Tcl_UtfToExternalDString(NULL, Tcl_GetString(objv[i]), -1, &fileNameDStr);
	LOGGER((LOGFP,"Read DIR:<%s>, level:%d\n",Tcl_DStringValue(&fileNameDStr),rec_params.dirlevel));
	rec_params.file = Tcl_DStringValue(&fileNameDStr);
	recursive_glob(&rec_params);
	Tcl_DStringFree(&fileNameDStr);
    }

    /*
     * Return directory list, if no variable is availiable */
    if (rec_params.dirvar)
    {
#if (TCL_MAJOR_VERSION >= 8) && (TCL_MINOR_VERSION == 1)
            char *dirvar = Tcl_GetStringFromObj(rec_params.dirvar, NULL);
	Tcl_SetObjVar2(interp, dirvar, NULL, rec_params.dirlist, 0);
#else
	Tcl_ObjSetVar2(interp, rec_params.dirvar, NULL, 
	           rec_params.dirlist, 0);
#endif
    }
    else
    {
	if (Tcl_ListObjLength(interp,rec_params.dirlist,&i) == TCL_OK && i > 0)
	{
	    Tcl_ListObjAppendList(interp,resultPtr,rec_params.dirlist);
	}
    }

    /*
     * Return file list, if no variable is availiable */
    if (rec_params.filevar)
    {
#if (TCL_MAJOR_VERSION >= 8) && (TCL_MINOR_VERSION == 1)
            char *filevar = Tcl_GetStringFromObj(rec_params.filevar, NULL);
	Tcl_SetObjVar2(interp, filevar, NULL, rec_params.filelist, 0);
#else
	Tcl_ObjSetVar2(interp,rec_params.filevar,NULL,
	           rec_params.filelist,0);
#endif
    }
    else
    {
	if (Tcl_ListObjLength(interp,rec_params.filelist,&i) == TCL_OK &&
	    i > 0)
	{
	    Tcl_ListObjAppendList(interp,resultPtr,rec_params.filelist);
	}
    }

    /*
     * close updating
     */
    if (rec_params.updatecommand != NULL)
    {
	if (updcmd==NULL)
	{
	    updcmd = ckalloc (MAXPATHLEN*2);
	}
	sprintf (updcmd, "%s close", rec_params.updatecommand);
	Tcl_Eval (rec_params.interp, updcmd);
    }

    /*
     * delete real path directory list (not more needed) */
    Tcl_DecrRefCount(rec_params.realdirlist);

    /*
     * pass the new pointer to the result */
    Tcl_SetObjResult (interp, resultPtr);

    ret_code = TCL_OK;
ret:
    Tcl_DStringFree(&dirSufDStr);
    return ret_code;
}

#ifdef __MSVC__

/*

This piece of WIN32 is to for fill the function of:

  DIR*           opendir(char*);
  struct dirent* readdir(DIR*);
  int            closedir(DIR*);

which are not implemented in the win32api.
*/

/*static WIN32_FIND_DATA* lpFindData;
static int found;*/

DIR*           opendir(char* pathname)
{
    /* init the DIR structure */
    DIR *findMarker = (DIR *) ckalloc(sizeof(DIR));
    char tempPathname[MAXPATHLEN];

    /* search directory - not find directory */
    sprintf(tempPathname,"%s/*",pathname);

    findMarker->lpFindData = (LPWIN32_FIND_DATA) ckalloc(sizeof(WIN32_FIND_DATA));
    findMarker->dir_handle = FindFirstFile(tempPathname,findMarker->lpFindData);

    findMarker->found = 0; /* ignore the first one since it will be the current dir(?). */


    return findMarker;

}

struct dirent* readdir(DIR* findMarker)
{
    struct dirent * retVal = (struct dirent *) ckalloc(sizeof(struct dirent));
    /* Find next valid directory or file, if any. */
    if (!findMarker->found)
	if (FindNextFile(findMarker->dir_handle,findMarker->lpFindData))
	    findMarker->found = 1; else findMarker->found = 0;


    /* If there is a directory send data to user otherwise send NULL. */

    if (findMarker->found==0)
	/* Nothing was found */
	return NULL;

    findMarker->found=0;

    sprintf(retVal->d_name,findMarker->lpFindData->cFileName);

    return retVal;
}

int closedir(DIR* findMarker)
{
    FindClose(findMarker->dir_handle);
    ckfree((char *) findMarker->lpFindData);
    ckfree((char *) findMarker);
    return 1;
}
#endif /* ifdef __MSVC__ */

