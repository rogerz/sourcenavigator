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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>

#ifdef __MSVC__
#include <shlobj.h>
#endif

#include <ctype.h>
#include <signal.h>
#include <time.h>
#include <locale.h>

#include <tcl.h>

#include "sn.h"
#include "mxdefine.h"
#include "mxlogger.h"
#include "fileutils.h"

/* Bleah. */
char *SN_StrDup(char*);

/*
 * Realizes routines regarding path names
 *
 * Calling format:
 * sn_filecmd
 *      format ?-internal? ?-unix? ?-window? ?-native? ?-sn? <path>
 *
 *             -unix, -internal, -sn: return the path in a unix (internal) format
 *             -windows: returns the path in a windows format
 *             -native:  returns path related to the current OS
 */
int
sn_filecmd (ClientData clientData,Tcl_Interp *interp,int argc,char **argv)
{
	char *p1, *p2;
	char *newpath, *path;
	char retint[32];
	int i;
	Tcl_DString nativeName, newNativeName;
	Tcl_DString path1, path2;

	Tcl_ResetResult (interp);

	if (argc < 2)
	{
		goto error;
	}
	/*
	 * format ?-unix? ?-window? ?-native? path
	 */
	if (strncmp (argv[1], "format", strlen(argv[1])) == 0)
	{
		int pformat = SN_PATH_NATIVE;
		for (i=2; i<argc && argv[i][0] == '-'; i++)
		{
			int len = strlen (argv[i]);
			if (strcmp (argv[i], "--") == 0)
			{
				break;
			}
			else if (strncmp (argv[i], "-unix", len) == 0 ||
				 strncmp (argv[i], "-internal", len) == 0 ||
				 strncmp (argv[i], "-sn", len) == 0)
			{
				pformat = SN_PATH_UNIX;
			}
			else if (strncmp (argv[i], "-windows", len) == 0)
			{
				pformat = SN_PATH_WINDOWS;
			}
			else if (strncmp (argv[i], "-native", len) == 0)
			{
				pformat = SN_PATH_NATIVE;
			}
			else
			{
				goto error;
			}
		}
		if (i>=argc)
		{
			goto error;
		}
		path = argv[i];
		if (pformat == SN_PATH_NATIVE)
		{
#if _WINDOWS
			pformat = SN_PATH_WINDOWS;
#else
			pformat = SN_PATH_UNIX;
#endif
		}
		Tcl_UtfToExternalDString(NULL, path, -1, &nativeName);
		newpath = SN_StrDup (Tcl_DStringValue(&nativeName));
		Tcl_DStringFree(&nativeName);
		sn_internal_convert_path (newpath, pformat);
		Tcl_ExternalToUtfDString(NULL, newpath, -1, &newNativeName);
		Tcl_SetResult(interp, Tcl_DStringValue(&newNativeName), TCL_VOLATILE);
		Tcl_DStringFree(&newNativeName);
	}
	else if (strncmp (argv[1], "compare", strlen(argv[1])) == 0)
	{
		i = 2;
		if ((i+1) >= argc)
		{
			goto error;
		}
		Tcl_UtfToExternalDString(NULL, argv[i++], -1, &path1);
		Tcl_UtfToExternalDString(NULL, argv[i], -1, &path2);
		
		sprintf (retint, "%i", native_compare_paths (
		    Tcl_DStringValue(&path1), 
		    Tcl_DStringValue(&path2), -1));
		Tcl_DStringFree(&path1);
		Tcl_DStringFree(&path2);
		Tcl_SetResult(interp, retint, TCL_VOLATILE);
	}
	else if (strncmp (argv[1], "isequal", strlen(argv[1])) == 0)
	{
		i = 2;
		if ((i+1) >= argc)
		{
			goto error;
		}
		Tcl_UtfToExternalDString(NULL, argv[i++], -1, &path1);
		Tcl_UtfToExternalDString(NULL, argv[i], -1, &path2);
		
		sprintf (retint, "%i", native_compare_paths (
		    Tcl_DStringValue(&path1), 
		    Tcl_DStringValue(&path2), -1) == 0);
		Tcl_DStringFree(&path1);
		Tcl_DStringFree(&path2);
		Tcl_SetResult(interp, retint, TCL_VOLATILE);
	}
	/*
	 * Looks in path2 to find a string matching for path1 (strstr)
	 */
	else if (strncmp (argv[1], "first", strlen (argv[1])) == 0)
	{
		int len;
		i = 2;
		if ((i+1) >= argc)
		{
			goto error;
		}
		Tcl_UtfToExternalDString(NULL, argv[i++], -1, &path1);
		Tcl_UtfToExternalDString(NULL, argv[i], -1, &path2);
		len   = Tcl_DStringLength(&path1);
		p1 = Tcl_DStringValue(&path1);
		p2 = Tcl_DStringValue(&path2);
		
		strcpy (retint, "-1");
#if _WINDOWS
		/*
		 * it uses the worst case comparing, no problem!
		 */
		for (i=0; *p2; p2++, i++)
		{
			if (native_compare_paths (p1, p2, len) == 0)
			{
				sprintf (retint, "%i", i);
				break;
			}
		}
#else
		if ((newpath = strstr (p2, p1)) != NULL)
		{
			sprintf (retint, "%i", newpath - p2);
		}
#endif
		Tcl_SetResult(interp, retint, TCL_VOLATILE);
		Tcl_DStringFree(&path1);
		Tcl_DStringFree(&path2);
	}
	/*
	 * Looks in path2 to find a string matching for path1 (strstr),
	 * returns 1 or 0
	 */
	else if (strncmp (argv[1], "begins", strlen (argv[1])) == 0)
	{
		int len;
		i = 2;
		if ((i+1) >= argc)
		{
			goto error;
		}
		Tcl_UtfToExternalDString(NULL, argv[i++], -1, &path1);
		Tcl_UtfToExternalDString(NULL, argv[i], -1, &path2);
		len   = Tcl_DStringLength(&path1);
		
		if (native_compare_paths (Tcl_DStringValue(&path1), 
			Tcl_DStringValue(&path2), len) == 0)
		{
			strcpy (retint, "1");
		}
		else
		{
			strcpy (retint, "0");
		}
		Tcl_SetResult(interp, retint, TCL_VOLATILE);
		Tcl_DStringFree(&path1);
		Tcl_DStringFree(&path2);
	}
	else
	{
		goto error;
	}
	
	return TCL_OK;
	
error:
	Tcl_AppendResult (interp,
		"usage:", argv[0], " format ?-unix? ?-windows? ?-sn? ?-internal? ?-native? path or\n",
		"\tcompare file1 file2 or\n"
		"\tisequal file1 file2 or\n"
		"\tfirst file1 file2 or \n"
		"\tbegins file1 file2",
		(char *) NULL);
	return TCL_ERROR;
}

/*
 *-----------------------------------------------------------------------------
 *
 *   Implements the TCL realpath command:
 *
 * Results:
 *   Standard TCL results, may return the system error message.
 *
 *-----------------------------------------------------------------------------
 */
int
sn_realpath(ClientData clientData,Tcl_Interp *interp,int argc,char **argv)
{
	char	realnm[MAXPATHLEN + 1];
#if _WINDOWS
	int	short_name = FALSE;
#else
	char *pwd = NULL; /* to get the full path without losing the symbolic links */
#endif /* _WINDOWS */
	Tcl_DString  fnm;
	int	i, j, one_path=1;
	char	*p;
	char	*argv0 = argv[0];
	Tcl_DString nativeFileName;
	Tcl_DString nativePwd;
	Tcl_DString realName;

	Tcl_DStringInit(&nativeFileName);
	Tcl_DStringInit(&nativePwd);

	for (j=1; j < argc && argv[j][0] == '-'; j++)
	{
		if (strncmp(argv[j], "-short", 6) == 0)
		{
#if _WINDOWS
			short_name = TRUE;	/* Useful under Windows to get short filenames. */
#endif /* _WINDOWS */
		}
		else if (strncmp(argv[j], "-pwd", 4) == 0)
		{
			if (j+1 >= argc)
			{
				goto error;
			}
			j++;
#ifndef _WINDOWS
			Tcl_UtfToExternalDString(NULL, argv[j], -1, &nativePwd);
			pwd = Tcl_DStringValue(&nativePwd);
#endif
		}
		else
		{
			goto argsError;
		}
	}
	Tcl_DStringInit(&fnm);
 
 	/*
	 * no parameters or more than one parameter specified
	 */
 	if (j == argc)
	{
		goto argsError;
	}
	
	/*
	 * If only one argument is given, then return a string, else
	 * return a list of the results
	 */
	if (argc-j > 1)
	{
		one_path = 0;
	}
 
	for (i = j; i < argc; i++)
	{
		if (!(p = Tcl_TranslateFileName (interp, argv[i], &fnm)))
		{
			Tcl_DStringFree (&fnm);
			goto error;
		}
		Tcl_UtfToExternalDString(NULL, p, Tcl_DStringLength(&fnm), &nativeFileName);
		p = Tcl_DStringValue(&nativeFileName);

#if _WINDOWS
		if (!win32_realpath (p, realnm))
#else
		if (! absolutepath (p, realnm, Tcl_DStringValue(&nativePwd)))
#endif
		{
			Tcl_AppendResult (interp, argv0, " failed for \"",
				p,"\",",Tcl_PosixError(interp), (char *) NULL);
			Tcl_DStringFree (&fnm);
			goto error;
		}

#if _WINDOWS
		/* might fail? - Good, maybe some one will fix properly */
		if (short_name)
		{
			int len;
#ifdef __MSVC__
			/* On MSVC, convert the path to a native windows path
			 * before calling the function
			 */
			sn_internal_convert_path (realnm, SN_PATH_WINDOWS);
#endif
			len = GetShortPathName(realnm, realnm, sizeof(realnm)-1);
			
			/*
			 * VERY IMPORTAN, when the command fails, this could mean
			 * that the path could be a network path, so network
			 * pathes are not supported by this command!
			 *
			 * So if the command fails, return the normal path name
			 */
			if (len > 0)
			{
				realnm[len] = '\0';
			}
			sn_internal_convert_path (realnm, SN_PATH_UNIX);
		}
#endif /* _WINDOWS */

		if (one_path)
		{
			/*
			 * return a string and not a list
			 */
			Tcl_ExternalToUtfDString(NULL, realnm, -1, &realName);
			Tcl_SetResult (interp, Tcl_DStringValue(&realName), TCL_VOLATILE);
			Tcl_DStringFree(&realName);
		}
		else
		{
			/*
			 * Append the result as a tcl/list element
			 */
			Tcl_ExternalToUtfDString(NULL, realnm, -1, &realName);
			Tcl_AppendElement (interp, Tcl_DStringValue(&realName));
			Tcl_DStringFree(&realName);
		}
	}
	Tcl_DStringFree (&fnm);
 	
	Tcl_DStringFree(&nativeFileName);
	Tcl_DStringFree(&nativePwd);
	return TCL_OK;
	
argsError:
	Tcl_AppendResult (interp,
			"usage: ", argv0, " ?-pwd pwd? ?-short? path1 path2 ...",
			(char *) NULL);
error:
	Tcl_DStringFree(&nativeFileName);
	Tcl_DStringFree(&nativePwd);
	return TCL_ERROR;
}

