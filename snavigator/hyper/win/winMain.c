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

#define	SN_INITIALISATION	1

/* 
 * winMain.c --
 *
 *	Main entry point for wish and other Tk-based applications.
 *
 * Copyright (c) 1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) winMain.c 1.28 96/07/23 16:58:12
 */

#include <tclInt.h> /* For TclInitEncodingSubsystem */
#include <tk.h>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#include <ctype.h>
#include <signal.h>
#include <locale.h>
#include <time.h>
#include <config.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "mxlogger.h"
#include "mxdefine.h"
#include "mxfuncs.h"

/*
 * Forward declarations for procedures defined later in this file:
 */

static void WishPanic _ANSI_ARGS_(TCL_VARARGS(char *,format));
static char *create_log_file(char *prefix, char *filename, char*ext);

#ifdef TK_TEST
EXTERN int		Tktest_Init _ANSI_ARGS_((Tcl_Interp *interp));
#endif /* TK_TEST */


static int useWinConsole = FALSE;

/*
 *----------------------------------------------------------------------
 *
 * TkpWinConsoleTest --
 *
 * Check if there are valid file STD_*_HANDLE handles. If there are any
 * we use them for console if not we create a console window.
 *
 * Win95 and NT4 behave different. Win95 returns INVALID_HANDLE_VALUE if 
 * no handle is available. NT4 returns a value which looks valid. Win95 
 * and NT4 return FILE_TYPE_UNKNOWN. So we better check both informations 
 * and hope that it works with Win98, Win02, WinCE, ..... too.
 *
 * The same test is used for STD_ERROR_HANDLE in winMain.c:WishPanic()
 * and tkWinInit.c:TkpDisplayWarning() too.
 *
 * Results:
 *	TRUE if any STD_*_HANDLE is valid
 *
 * Side effects:
 *  May block if called after TkpConsoleCreate(), because GetFileType() 
 *  blocks on NT if another Thread uses the file.
 *
 *----------------------------------------------------------------------
 */

int
TkpWinConsoleTest()
{
    if ( (GetStdHandle(STD_INPUT_HANDLE)  != INVALID_HANDLE_VALUE &&
          GetFileType(GetStdHandle(STD_INPUT_HANDLE)) != FILE_TYPE_UNKNOWN) ||
         (GetStdHandle(STD_OUTPUT_HANDLE) != INVALID_HANDLE_VALUE &&
          GetFileType(GetStdHandle(STD_OUTPUT_HANDLE)) != FILE_TYPE_UNKNOWN) ||
         (GetStdHandle(STD_ERROR_HANDLE)  != INVALID_HANDLE_VALUE &&
          GetFileType(GetStdHandle(STD_ERROR_HANDLE)) != FILE_TYPE_UNKNOWN) )
	return TRUE;
    return FALSE;
}


/*
 *----------------------------------------------------------------------
 *
 * TkpWinConsoleCreate --
 *
 * 	Create the console channels and install them as the standard
 * 	channels.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Creates the console channel and installs it as the standard
 *	channels.
 *
 *----------------------------------------------------------------------
 */

void 
TkpWinConsoleCreate()
{
	TclInitEncodingSubsystem();

    Tcl_GetStdChannel(TCL_STDIN);
    Tcl_GetStdChannel(TCL_STDOUT);
    Tcl_GetStdChannel(TCL_STDERR);
}


/*
 *----------------------------------------------------------------------
 *
 * WinMain --
 *
 *	Main entry point from Windows.
 *
 * Results:
 *	Returns false if initialization fails, otherwise it never
 *	returns. 
 *
 * Side effects:
 *	Just about anything, since from here we call arbitrary Tcl code.
 *
 *----------------------------------------------------------------------
 */

#ifndef NT_BATCH_MODE
int APIENTRY
WinMain(hInstance, hPrevInstance, lpszCmdLine, nCmdShow)
    HINSTANCE hInstance;
    HINSTANCE hPrevInstance;
    LPSTR lpszCmdLine;
    int nCmdShow;
#else
int main(int argc2,char **argv2)
#endif 
{
    char **argv, **argvlist, *p;
    int argc, size, i;
    char buffer[MAX_PATH];

    /*
     * Set up the default locale to be standard "C" locale so parsing
     * is performed correctly.
     */

    setlocale(LC_ALL, "C");


    /*
     * Increase the application queue size from default value of 8.
     * At the default value, cross application SendMessage of WM_KILLFOCUS
     * will fail because the handler will not be able to do a PostMessage!
     * This is only needed for Windows 3.x, since NT dynamically expands
     * the queue.
     */
    SetMessageQueue(64);

    /*
     * Create the console channels and install them as the standard
     * channels.  All I/O will be discarded until Tcl_CreateConsoleWindow is
     * called to attach the console to a text widget.
     */

    useWinConsole = TkpWinConsoleTest();
    if ( useWinConsole ) {
        TkpWinConsoleCreate();
    }

    /*
     * Precompute an overly pessimistic guess at the number of arguments
     * in the command line by counting non-space spans.  Note that we
     * have to allow room for the executable name and the trailing NULL
     * argument.
     */
#ifndef NT_BATCH_MODE
    for (size = 3, p = lpszCmdLine; *p != '\0'; p++) {
	if (isspace(*p)) {
	    size++;
	    while (isspace(*p)) {
		p++;
	    }
	    if (*p == '\0') {
		break;
	    }
	}
    }
    argvlist = (char **) ckalloc((unsigned) (size * sizeof(char *)));
    argv = argvlist;

    /*
     * Parse the Windows command line string.  If an argument begins with a
     * double quote, then spaces are considered part of the argument until the
     * next double quote.  The argument terminates at the second quote.  Note
     * that this is different from the usual Unix semantics.
     */

    for (i = 1, p = lpszCmdLine; *p != '\0'; i++) {
	while (isspace(*p)) {
	    p++;
	}
	if (*p == '\0') {
	    break;
	}
	if (*p == '"') {
	    p++;
	    argv[i] = p;
	    while ((*p != '\0') && (*p != '"')) {
		p++;
	    }
	} else {
	    argv[i] = p;
	    while (*p != '\0' && !isspace(*p)) {
		p++;
	    }
	}
	if (*p != '\0') {
	    *p = '\0';
	    p++;
	}
    }
    argv[i] = NULL;
    argc = i;

    /*
     * Since Windows programs don't get passed the command name as the
     * first argument, we need to fetch it explicitly.
     */

    GetModuleFileName(NULL, buffer, sizeof(buffer));
    argv[0] = buffer;

    Tk_Main(argc, argv, Tcl_AppInit);
#else
    Tk_Main(argc2, argv2, Tcl_AppInit);
#endif
    return 1;
}

/*
 * Returns a file name for the log messages that doesn't exist,
 * if file exists, a file with the following format will be
 * created:
 * prefix/filename99.ext
 */
char *create_log_file(char *prefix, char *filename, char*ext)
{
	static char buf[MAXPATHLEN];
	struct stat st;
	char *parg, *p;
	int i = 0;
	if (prefix==NULL)
	{
		parg = getenv("TMP");
		if (!parg)
			parg = getenv("TEMP");
	}
	else
	{
		parg = prefix;
	}
	do {
		char tmp[16];
		if (i > 0)
		{
			sprintf (tmp, "%02i", i);
			p = tmp;
		}
		else
		{
			p = "";
		}
		sprintf (buf, "%s/%s%s.%s", parg ? parg : "", filename, p, ext);
		
		/* make sure that the file doesn't exist */
		if (stat (buf, &st))
		{
			break;
		}
		i++;
	} while (1);
	return buf;
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

#if SN_INITIALISATION
int     Sn_setup_Init   (Tcl_Interp *interp);
#endif /* SN_INITIALISATION */

int
Tcl_AppInit(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
     char *sn_win_debug = getenv("SN_DEBUG");
 
	/* Taken from 'tclsql.c' to  activate debug logging. [ITR:12:01:97] */
	if (sn_win_debug==0)
		debug =0;
	else
		debug = atoi(sn_win_debug);

    if( debug>0 && LOGFP == (FILE *)0 )
    {
	time_t	clock;
	char	*logfilename;
	int	logfd;

	logfilename = create_log_file (NULL, "zznavig", "log");
	/*
	 * We must not use fd 0, try to assing 1 to stdout and
	 * 2 to stderr.
	 */
	if ((logfd = open(logfilename,O_WRONLY|O_CREAT|O_TRUNC,0666)) <= 2)
	{
	    int	newfd;

  	    for (newfd = 1; newfd < 10; newfd++)
	    {
		if (dup2(logfd,newfd) == 0)
			break;
	    }
	    close (logfd);
	    logfd = newfd;
	    if (logfd == 1)
	    dup2(logfd,2);
	}
	if ((LOGFP = fdopen(logfd,"w+")) == NULL)
	{
	    return TCL_ERROR;
	}
	    time (&clock);
	    LOGGER((LOGFP,"Started: %sdebug level: %d\n",
		ctime(&clock),(int)debug));
	    LOGGER((LOGFP,"tcl_version: %s, tk_version: %s\n",
		TCL_VERSION,TK_VERSION));
	    LOGGER((LOGFP,"PID: %lu, logfileid: %d\n",
		(unsigned long)getpid(),fileno(LOGFP)));
    }

#if SN_INITIALISATION
    if (Sn_setup_Init(interp) == TCL_ERROR) {
        goto error;
    }
#else
    if (Tcl_Init(interp) == TCL_ERROR) {
	goto error;
    }
    if (Tk_Init(interp) == TCL_ERROR) {
	goto error;
    }
    Tcl_StaticPackage(interp, "Tk", Tk_Init, (Tcl_PackageInitProc *) NULL);
#endif /* mutiX */

    /*
     * Initialize the console only if we are running as an interactive
     * application.
     */

    if ((strcmp(Tcl_GetVar(interp, "tcl_interactive", TCL_GLOBAL_ONLY), "1")
	    == 0) || ( ! useWinConsole )) {
	if (Tk_CreateConsoleWindow(interp) == TCL_ERROR) {
	goto error;
	}
#ifdef WIN32
	else {
		Tcl_SetVar(interp, "tcl_interactive", "1", TCL_GLOBAL_ONLY);
	}
#endif /* WIN32 */
    }




#ifdef TK_TEST
    if (Tktest_Init(interp) == TCL_ERROR) {
	goto error;
    }
    Tcl_StaticPackage(interp, "Tktest", Tktest_Init,
            (Tcl_PackageInitProc *) NULL);
#endif /* TK_TEST */

    Tcl_SetVar(interp, "tcl_rcFileName", "~/wishrc.tcl", TCL_GLOBAL_ONLY);
    return TCL_OK;

error:
    WishPanic(interp->result);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * WishPanic --
 *
 *	Display a message and exit.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Exits the program.
 *
 *----------------------------------------------------------------------
 */

void
WishPanic TCL_VARARGS_DEF(char *,arg1)
{
    va_list argList;
    char buf[1024];
    char *format;
    
    format = TCL_VARARGS_START(char *,arg1,argList);
    vsprintf(buf, format, argList);

    MessageBeep(MB_ICONEXCLAMATION);
    MessageBox(NULL, buf, "Fatal Error in Wish",
	    MB_ICONSTOP | MB_OK | MB_TASKMODAL | MB_SETFOREGROUND);
    ExitProcess(1);
}

