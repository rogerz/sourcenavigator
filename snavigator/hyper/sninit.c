/*

Copyright (c) 2000, 2001, Red Hat, Inc.

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

#ifdef __MSVC__
#include <shlobj.h>
#endif

#include <ctype.h>
#include <signal.h>
#include <time.h>
#include <locale.h>
#include <stdlib.h>
#define _STDLIB

#define	MAIN_MODULE

#include "mxdefine.h"

#include "tclInt.h"

#include "mxfuncs.h"
#include "compare.h"
#include "highlight.h"

#ifdef __CYGWIN32__
/* Avoid redefinition of struct timezone in Tk internal header.  */
#define timezone tk_timezone
#endif

#include "tkInt.h"
#include "itk.h"
#include "sn.h"

#include "mxlogger.h"
#include "fileutils.h"
#include "guitcl.h"

/* Do we include EL/IX features or not? */
static int sn_elix = SN_ELIX;

/* Installation directory for the application  */
static char installdir[MAXPATHLEN];

static int batchmode = 0;

extern int	Tix_Init (Tcl_Interp *interp);
extern int      Ttk_Init (Tcl_Interp *interp);

int
fill_file_tree(ClientData clientData,Tcl_Interp *interp,int argc,char **argv);

int sn_db_format_qry(ClientData clientData,Tcl_Interp *interp,int argc,Tcl_Obj *CONST objv[]);

int tk_trim_text_index(ClientData clientData,Tcl_Interp *interp,int argc,char **argv);

int class_browser_insert(ClientData clientData,Tcl_Interp *interp,int argc, Tcl_Obj *CONST argv[]);

int sn_glob(ClientData clientData, Tcl_Interp *interp, int argc, Tcl_Obj *CONST objv[]);

int	sn_tk_text_insert(ClientData cli, Tcl_Interp *in, int objc, Tcl_Obj *CONST objv[]);

static	int	sn_check_num_of_prj_files_lines(ClientData cli, Tcl_Interp *in, int argc, Tcl_Obj *CONST objv[]);

static	void	mx_Tcl_ExitHandler(ClientData cli);
int	Sn_setup_Init(Tcl_Interp *interp);

int  retriever_services(ClientData clientData,
				Tcl_Interp *interp, int argc, char **argv);
int  cross_services(ClientData clientData,
				Tcl_Interp *interp, int argc, char **argv);
/* FIXME: Why don't we use the declaration in tkTreeTable.h ??? */
int	 create_treetable_command(Tcl_Interp *interp);
int	 _Paftcldb_Init(Tcl_Interp *in);
void	ide_create_xpm_image_type();
int  ide_create_win_choose_font_command (Tcl_Interp *interp);

/* File commands. */
int sn_realpath    (ClientData cli, Tcl_Interp *in, int argc, char **argv);
int sn_filecmd (ClientData clientData,Tcl_Interp *interp,int argc,char **argv);
int sn_indent_outdent (ClientData clientData, Tcl_Interp *interp, int argc, Tcl_Obj *objv[]);

/* command from libide */
int ide_create_get_directory_command (Tcl_Interp *interp);
int ide_create_winprint_command (Tcl_Interp *);

static	int
sn_log(ClientData clientData, Tcl_Interp *interp,int argc,char **argv)
{
	int std_error = 0;
	int	lev = 1;
	
	if (argc == 1) {
                /* FIXME: need to provide error */
		/*Tcl_WrongNumArgs(interp, 1, objv, "msg ?msg ...?");*/
		return TCL_ERROR;
	}
	
	if (strncmp(argv[1],"-l",2) == 0)
	{
		lev = atoi(argv[2]);
		argv += 2;
		argc -= 2;
	}
	/* print error also on the stderr, even if debugging is disabled. */
	if (strncmp(argv[1],"-stderr",4) == 0)
	{
		/* just ignore it on windows */
#if !_WINDOWS
		std_error = 1;
#endif
		argv ++;
		argc --;
	}
	
	if (lev > debug)
	{
		if (std_error)
		{
			for (argv++; argc-- > 1; argv++)
			{
				Tcl_DString sys;
				Tcl_DStringInit(&sys);
				Tcl_UtfToExternalDString(NULL, *argv, -1, &sys);
				fprintf (stderr, "%s", Tcl_DStringValue(&sys));
				Tcl_DStringFree(&sys);
			}
			fprintf(stderr,"\n");
		}
		return TCL_OK;
	}

	for (argv++; argc-- > 1; argv++)
	{
		Tcl_DString sys;
		Tcl_DStringInit(&sys);
		Tcl_UtfToExternalDString(NULL, *argv, -1, &sys);
		LOGGER((LOGFP,"%s",Tcl_DStringValue(&sys)));
#if !_WINDOWS
		fprintf(stderr,"%s",Tcl_DStringValue(&sys));
#endif /* !_WINDOWS */
		Tcl_DStringFree(&sys);
	}
#if !_WINDOWS
	fprintf(stderr,"\n");
	fflush(stderr);
#endif /* !_WINDOWS */
	LOGGER((LOGFP,"\n"));
	
	return TCL_OK;
}

static	int
sn_tk_restore_cursors(ClientData clientData, Tcl_Interp *interp,int argc,Tcl_Obj *CONST objv[])
{
	char	*p;
	int	reset = FALSE;
	char	*fld;
	int	listLen;
	int	cou;
	Tcl_Obj	**elemPtrs;

	if (argc > 1 &&
		strncmp(Tcl_GetStringFromObj(objv[1],(int *) NULL),"-reset",5) == 0)
	{
		reset = TRUE;
		argc--;
	}

	if (argc != 2)
	{
		Tcl_WrongNumArgs(interp, 1, objv,
			"?-reset_x_handler? window_list");

		return TCL_ERROR;
	}

	if (Tcl_ListObjGetElements(interp,objv[2],&listLen, &elemPtrs) != TCL_OK)
		return TCL_ERROR;

	for (cou = 0; cou < listLen; cou++)
	{
		fld = Tcl_GetStringFromObj(elemPtrs[cou++], NULL);
		p = Tcl_GetStringFromObj(elemPtrs[cou], NULL);
		if (*p == '\0')
			p = "{}";

		LOGGER3((LOGFP,"Restore %s cursor: <%s>\n",fld,p));

		Tcl_VarEval(interp,fld," config -cursor ",p,NULL);
	}

	if (reset)
	{
		/*SN_tk_x_events((ClientData)0,interp,0,(char **)0);*/
	}

	Tcl_ResetResult(interp);

	return TCL_OK;
}

static	void
mx_save_and_set_cursor(Tcl_Interp *interp,char *tkpath,char *cursor,Tcl_Obj *res_obj)
{
	Tcl_Obj	*old_cursor = NULL;

	if (cursor)
	{
		if (Tcl_VarEval(interp,tkpath," cget -cursor",NULL) != TCL_OK)
			return;

		old_cursor = Tcl_NewStringObj(interp->result,-1);

		if (Tcl_VarEval(interp,tkpath," configure -cursor ",cursor,NULL) != TCL_OK)
			return;
	}
	Tcl_ListObjAppendElement(interp,res_obj,Tcl_NewStringObj(tkpath,-1));

	if (old_cursor)
		Tcl_ListObjAppendElement(interp,res_obj,old_cursor);
}

/*
 *	This function queries the children of a window.
 *
 *	Warning: the returned list contains the mother too !
 */
static	void
mx_tk_getchildren(Tcl_Interp *interp,Tk_Window window,int only_mapped,char *cursor,Tcl_Obj *chld_obj)
{
	register TkWindow *winPtr;
	int	main_window;
	int	mapped = TRUE;

	main_window = (window == Tk_MainWindow(interp));

	if (main_window || !only_mapped || (mapped = Tk_IsMapped(window)))
	{
		mx_save_and_set_cursor(interp,
			Tk_PathName(window),cursor,chld_obj);	/* The mother itself. */
	}

	if (only_mapped && !mapped && !main_window)
		return;

	for (winPtr = ((TkWindow *)window)->childList; winPtr; winPtr = winPtr->nextPtr)
	{
		if (winPtr->childList)
		{
			mx_tk_getchildren(interp,(Tk_Window)winPtr,only_mapped,cursor,chld_obj);
		}
		else
		{
			mx_save_and_set_cursor(interp,winPtr->pathName,cursor,chld_obj);
		}
	}
}

static	int
mx_tk_wchildren(ClientData clientData, Tcl_Interp *interp,int argc,Tcl_Obj *CONST objv[])
{
	Tk_Window window;
	int	only_mapped = FALSE;
	char	*cursor = NULL;
	Tcl_Obj	*chld_obj = Tcl_NewObj();
	char	*par;
	int	i;

	if (argc < 2 || argc > 5)
	{
		Tcl_WrongNumArgs(interp, 1, objv,
			"?-mapped? ?-cursor cursor? window");

		return TCL_ERROR;
	}

	for (i = 1, par = NULL; i < argc; i++)
	{
		par = Tcl_GetStringFromObj(objv[i],(int *) NULL);
		if (*par != '-')
			break;

		switch (par[1])
		{
		case 'm':
			only_mapped = TRUE;
			break;

		case 'c':
			cursor = Tcl_GetStringFromObj(objv[++i],(int *) NULL);
			break;
		}
	}

	window = Tk_NameToWindow(interp,par,Tk_MainWindow(interp));
	if (!window)
	{
		return TCL_ERROR;
	}

	mx_tk_getchildren(interp,window,only_mapped,cursor,chld_obj);

	Tcl_SetObjResult(interp,chld_obj);

	return TCL_OK;
}

/* FIXME: We need a better of handling locks on project files. */
static	int
isfileused(ClientData clientData,Tcl_Interp *interp,int argc,char **argv)
{
#if _WINDOWS
  HANDLE	fd;
  char	tmp[20];
  char	*filename;
  Tcl_DString nativeName;
#endif /* _WINDOWS */

  if (argc != 2)
    {
      Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		       " filename", (char *) NULL);
      return TCL_ERROR;
    }
#if _WINDOWS
  Tcl_UtfToExternalDString(NULL, argv[1], -1, &nativeName);
  filename = Tcl_DStringValue(&nativeName);
  
  fd = CreateFile(filename,GENERIC_READ|GENERIC_WRITE,
		  0,NULL,OPEN_EXISTING,0,NULL);
  Tcl_DStringFree(&nativeName);
  if (fd == INVALID_HANDLE_VALUE)
    {
      sprintf(tmp,"%d",(int)GetLastError());
      LOGGER((LOGFP,"CreateFile for <%s> error: %s\n",filename,tmp));
      
      Tcl_AppendResult(interp, tmp, (char *) NULL);
      
      return TCL_OK;
    }
  CloseHandle(fd);
#endif /* _WINDOWS */
  
  Tcl_AppendResult(interp, "0", (char *) NULL);
  
  return TCL_OK;
}

#if !_WINDOWS
extern int SN_donot_call_motif_filedialog_box;
#endif /* !_WINDOWS */

static	void
sn_init_mycommands(Tcl_Interp *interp,ClientData main_win)
{
        char grabRename[] = "rename grab tcl_tk_grab";

	/*
	 * use the same separator as in the database
	 */
 	Tcl_SetVar(interp, "sn_sep", DB_FLDSEP_STR, TCL_GLOBAL_ONLY);
 	Tcl_SetVar2(interp, "sn_options", "sn_sep", DB_FLDSEP_STR, 0);
	
	/*
	 * Use OS dependent root character
	 * (NOT NOW BECAUSE TCL/TK DOESN'T SUPPORT '\\' in file names
	 */
 	Tcl_SetVar (interp, "sn_root",              "/",  TCL_GLOBAL_ONLY);
 	Tcl_SetVar2(interp, "sn_options", "sn_sep", "/",  0);

	Tcl_CreateCommand(interp, "trim_text_index",
		(Tcl_CmdProc *)tk_trim_text_index,
		(ClientData)NULL, (Tcl_CmdDeleteProc *) NULL);

	Tcl_CreateCommand(interp, "Sn_Syntax_Highlight",
		(Tcl_CmdProc *)Sn_Syntax_Highlight,
		(ClientData)NULL, (Tcl_CmdDeleteProc *) NULL);

	Tcl_CreateCommand(interp, "brace_balance",
		(Tcl_CmdProc *)brace_balance,
		(ClientData)NULL, (Tcl_CmdDeleteProc *) NULL);

     	Tcl_CreateCommand(interp, "isfileused",
			  (Tcl_CmdProc *)isfileused,
			  (ClientData)NULL,(Tcl_CmdDeleteProc *) NULL);

	_Paftcldb_Init(interp);

	(void)Tcl_XListInit(interp);

	Tcl_CreateObjCommand(interp, "tk_text_insert",
		(Tcl_ObjCmdProc *)sn_tk_text_insert,
		(ClientData)NULL, (Tcl_CmdDeleteProc *) NULL);

	Tcl_CreateObjCommand(interp, "sn_db_format_qry",
		(Tcl_ObjCmdProc *)sn_db_format_qry,
		(ClientData)NULL, (Tcl_CmdDeleteProc *) NULL);

	Tcl_CreateObjCommand(interp, "sn_compare",
		(Tcl_ObjCmdProc *) sn_compare,
		(ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

	Tcl_CreateCommand(interp, "sn_log",
		(Tcl_CmdProc *)sn_log,
		(ClientData)NULL,(Tcl_CmdDeleteProc *) NULL);

	create_treetable_command(interp);

	Tcl_CreateObjCommand(interp, "mx_get_children",
		(Tcl_ObjCmdProc *)mx_tk_wchildren,
		(ClientData)main_win,(Tcl_CmdDeleteProc *) NULL);
	
	Tcl_CreateObjCommand(interp, "tk_restore_cursors",
		(Tcl_ObjCmdProc *)sn_tk_restore_cursors,
		(ClientData)NULL,(Tcl_CmdDeleteProc *) NULL);

	Tcl_CreateObjCommand(interp, "class_browser_insert",
		(Tcl_ObjCmdProc *)class_browser_insert,
		(ClientData)NULL,(Tcl_CmdDeleteProc *) NULL);

	Tcl_CreateCommand(interp, "fill_file_tree",
		(Tcl_CmdProc *)fill_file_tree,
		(ClientData)NULL,(Tcl_CmdDeleteProc *) NULL);

	Tcl_CreateObjCommand(interp, "sn_glob",
		(Tcl_ObjCmdProc *)sn_glob,
		(ClientData)NULL,(Tcl_CmdDeleteProc *) NULL);

	Tcl_CreateCommand(interp, "realpath",
		(Tcl_CmdProc *)sn_realpath,
		(ClientData)NULL,(Tcl_CmdDeleteProc *) NULL);

	Tcl_CreateCommand(interp, "sn_filecmd",
		(Tcl_CmdProc *)sn_filecmd,
		(ClientData)NULL,(Tcl_CmdDeleteProc *) NULL);

	Tcl_CreateObjCommand(interp, "sn_check_num_of_prj_files_lines",
		(Tcl_ObjCmdProc *)sn_check_num_of_prj_files_lines,
		(ClientData)NULL,(Tcl_CmdDeleteProc *) NULL);

	Tcl_CreateCommand(interp, "retriever_services",
		(Tcl_CmdProc *)retriever_services,
		(ClientData)NULL,(Tcl_CmdDeleteProc *) NULL);

	Tcl_CreateCommand(interp, "cross_services",
		(Tcl_CmdProc *)cross_services,
		(ClientData)NULL,(Tcl_CmdDeleteProc *) NULL);

	Tcl_CreateObjCommand(interp, "sn_indent_outdent",
		(Tcl_ObjCmdProc *)sn_indent_outdent,
		(ClientData)NULL,(Tcl_CmdDeleteProc *) NULL);

	/* create the command (from libide) to select a directory */
	ide_create_get_directory_command (interp);
	create_graph_command(interp);
#ifdef _WIN32
	/* create printer command for windows */
	ide_create_winprint_command (interp);
	/* create command to print text widgets */
	ide_create_print_text_command(interp);
	/* create command to print canvas widgets */
	ide_create_printcanvas_command(interp);
	/* create choose font command for windows [itr] */
	ide_create_win_choose_font_command(interp);
#endif
	
#if (TCL_MAJOR_VERSION <= 8) && (TCL_MINOR_VERSION < 1)
#ifndef _WINDOWS
	/* Don't call the motif dialog box */
	SN_donot_call_motif_filedialog_box = 1;
#endif
#endif
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

static char *user_home=NULL;
static char **application_argv=NULL;

int
run_application(int argc,char *argv[])
{
	int	i;
	char	*ins_dir = NULL;
	char	*pi;
	int	new_argc=0;

	if (application_argv)
	{
		ckfree ((char *)application_argv);
	}
	application_argv = (char**)ckalloc((argc+1) * sizeof(char *));
	for (i=0; i<=argc; i++)
		application_argv[i] = NULL;

	for (i = 0, new_argc = 0; i < argc; i++)
	{
		if(strcmp(argv[i],"-debug") == 0 || strcmp (argv[i], "--debug") == 0)
		{
			if (++i < argc)
			{
				debug = (short)atoi(argv[i]);
			}
		}
		else if(strcmp(argv[i],"-home") == 0 || strcmp(argv[i],"--home") == 0)
		{
			if (++i < argc)
			{
				ins_dir = argv[i];
			}
		}
		else if(strcmp(argv[i],"-uhome") == 0)
		{
			if (++i < argc)
			{
				user_home = argv[i];
			}
		}
		else if (strcmp (argv[i], "--batchmode") == 0 || strcmp (argv[i], "-b") == 0)
		{
			batchmode = 1;
		}
		else
		{
			application_argv[new_argc++] = argv[i];
		}
	}
	application_argv[new_argc] = NULL;
	
	/*
	 * accept env. variable SN_DEBUG, in the case when "-debug"
	 * isn't specified
	 */
	if (debug == 0 && getenv ("SN_DEBUG") != NULL)
	{
		debug = atoi(getenv("SN_DEBUG"));
	}
	
	pi = ins_dir ? ins_dir : application_argv[0];
	strcpy (installdir, pi);
	sn_internal_convert_path (installdir, SN_PATH_UNIX);

	/* Remove the last two '/' to get the installation directory if
	 * it has not been specified in the command line! */
	if (!ins_dir)
	{
		pi = file_lastroot(installdir);
		if (pi)
		{
			*pi = '\0';
			pi = file_lastroot(installdir);
			if (pi)
				*pi = '\0';
		}
	}
	
#if DEBUG
	if(debug && LOGFP == (FILE *)0)
	{
		time_t	clock;
		char	*logfilename;
#if _WINDOWS
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
#else
		logfilename = create_log_file (".", "zznavig", "log");
		if ((LOGFP = fopen (logfilename, "w+")) == (FILE *) NULL)
		{
			fprintf(stderr,"couldn't creat file \"%s\", errno=%d\n", logfilename, errno);
			return TCL_ERROR;
		}
		fcntl(fileno(LOGFP), F_SETFD, FD_CLOEXEC);
		chmod(logfilename, 0666);
#endif /* _WINDOWS */
		time (&clock);
		LOGGER((LOGFP,"Started: %sdebug level: %d\n",
			ctime(&clock),(int)debug));
		LOGGER((LOGFP,"tcl_version: %s, tk_version: %s\n",
			TCL_VERSION,TK_VERSION));
		LOGGER((LOGFP,"PID: %lu, logfileid: %d\n",
			(unsigned long)getpid(),fileno(LOGFP)));
		LOGGER((LOGFP,"Installation directory: <%s>\n",installdir));
	}
#endif /* DEBUG */

#if !_WINDOWS
	if (!user_home)
	{
		user_home = GetHomeDir();
	}
#endif /* !_WINDOWS */

	setlocale(LC_ALL,"C");

	application_argv = (char**)ckrealloc((char*)application_argv, argc * sizeof(char *) + 1);

	Tk_Main(new_argc, application_argv, Sn_setup_Init);

	if (application_argv)
	{
		ckfree ((char *)application_argv);
		application_argv = NULL;
	}

	return TCL_OK;
}

#if !_WINDOWS
/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *      This is the main program for the application.
 *
 * Results:
 *      None: Tk_Main never returns here, so this procedure never
 *      returns either.
 *
 * Side effects:
 *      Whatever the application does.
 *
 *----------------------------------------------------------------------
 */

int
main(int argc,char **argv)
{
      run_application(argc,argv);
      return 0;               /* Needed only to prevent compiler warning. */
}
#endif /* !_WINDOWS */

#if _WINDOWS
#define	CHECK_OPEN_MODE	(O_BINARY|O_RDONLY)
#else
#define	CHECK_OPEN_MODE	O_RDONLY
#endif /* _WINDOWS */

/*
 * parameters:
 *			files: as a tcl/list of files
 *			-refresh: when unequal to "no" it calculates the line number
 *					  of all files
 * return value:
 *			a tcl/list that contain two values:
 *				1. number of files that can be add to the project.
 *				2. number of lines of those files.
 *
 * As Example:
 *		sn_check_num_of_prj_files_lines [list file1 file2 .. file202]
 *
 * coud produce
 *		{120 99129}
 * This means that the file list has to be thrinked to the
 * first 120 files. Those 120 files have 99129 lines of
 * source code.
 *
 * We don't use "fopen, fclose, fget" for file operations, this are
 * too slowly.
 * Ansted use "open, close, read"
 */
static int
sn_check_num_of_prj_files_lines (ClientData clientData, Tcl_Interp *interp,
	int argc,
	Tcl_Obj *CONST objv[])
{
	int		fd;
	int		prj_file_num = 0;
	int		file_lines;
	int		prj_line_num = 0;
	char	buf [10240];
	int		i, j, len, num;
	int		calc_allfiles = 0;
	Tcl_DString	fileName;
	
	Tcl_Obj *objFiles, *next;
	Tcl_Obj	*resultPtr = Tcl_NewObj();

	for (i=1; i<argc; i++)
	{
		char * p = Tcl_GetStringFromObj(objv[i], NULL);
		if (strcmp (p, "--") == 0)
		{
			break;
		}
		else if (strcmp (p, "-linenumber") == 0 || strcmp (p, "--number") == 0)
		{
			i++;
			prj_line_num += atoi (Tcl_GetStringFromObj(objv[i], NULL));
		}
		else if (strcmp (p, "-filenumber") == 0)
		{
			i++;
			prj_file_num += atoi (Tcl_GetStringFromObj(objv[i], NULL));
		}
		else if (strcmp (p, "-refresh") == 0)
		{
			i++;
			p = Tcl_GetStringFromObj(objv[i], NULL);
			if (strcmp (p, "no") != 0)
			{
				calc_allfiles = 1;
			}
			else
			{
				calc_allfiles = 0;
			}
		}
		else
		{
			break;
		}
	}
	if (i >= argc)
	{
		Tcl_AppendResult(interp,"wrong # args: should be \"",
							Tcl_GetStringFromObj(objv[0], NULL),
							" ?-linenumber <num>? ?-filenumber num? files\"",
							(char *)NULL);

		return(TCL_ERROR);
	}
	objFiles = objv[i];
	
	Tcl_ListObjLength (interp, objFiles, &num);
	
	for (j=0; j < num; prj_file_num++, j++)
	{
		if (Tcl_ListObjIndex (interp, objFiles, j, &next) != TCL_OK)
		{
			break;
		}
		
		Tcl_UtfToExternalDString(NULL, Tcl_GetString(next), -1, &fileName);
		fd = open (Tcl_DStringValue(&fileName), CHECK_OPEN_MODE);
		Tcl_DStringFree(&fileName);
		if (fd < 0)
		{
			continue;
		}

		if (prj_file_num % 5 == 0)
			Tcl_DoOneEvent(TCL_DONT_WAIT);		/* Refresh the screen ! */

		for (file_lines=0; (len = read (fd, buf, sizeof(buf))) > 0; )
		{
			for (i=0; i<len; i++)
			{
				if (buf[i] == '\n')
				{
					file_lines ++;
				}
			}
		}
		close(fd);
			
		/*
		 * increment project line number
		 */
		prj_line_num += file_lines;
	}

	sprintf (buf, "%d", prj_file_num);
	Tcl_ListObjAppendElement (interp, resultPtr, Tcl_NewStringObj(buf,-1));
	
	sprintf (buf, "%d", prj_line_num);
	Tcl_ListObjAppendElement (interp, resultPtr, Tcl_NewStringObj(buf,-1));
	
	/*
	 * pass the new pointer to the result */
	Tcl_SetObjResult (interp, resultPtr);

	return TCL_OK;
}

static void
SN_set_scope_bits(Tcl_Interp *interp)
{
	char	*var="SN_Scope_Bits";
	char	val[100];

	sprintf(val,"%d",PAF_PRIVATE);
	Tcl_SetVar2(interp,var,"private",val,TCL_GLOBAL_ONLY);

	sprintf(val,"%d",PAF_PROTECTED);
	Tcl_SetVar2(interp,var,"protected",val,TCL_GLOBAL_ONLY);

	sprintf(val,"%d",PAF_PUBLIC);
	Tcl_SetVar2(interp,var,"public",val,TCL_GLOBAL_ONLY);

	sprintf(val,"%d",PAF_FINAL);
	Tcl_SetVar2(interp,var,"final",val,TCL_GLOBAL_ONLY);

	sprintf(val,"%d",PAF_STATIC);
	Tcl_SetVar2(interp,var,"static",val,TCL_GLOBAL_ONLY);

	sprintf(val,"%d",PAF_ABSTRACT);
	Tcl_SetVar2(interp,var,"abstract",val,TCL_GLOBAL_ONLY);

	sprintf(val,"%d",PAF_FINAL);
	Tcl_SetVar2(interp,var,"final",val,TCL_GLOBAL_ONLY);

	sprintf(val,"%d",PAF_NATIVE);
	Tcl_SetVar2(interp,var,"native",val,TCL_GLOBAL_ONLY);

	sprintf(val,"%d",PAF_VOLATILE);
	Tcl_SetVar2(interp,var,"volatile",val,TCL_GLOBAL_ONLY);

	sprintf(val,"%d",PAF_VIRTUAL);
	Tcl_SetVar2(interp,var,"virtual",val,TCL_GLOBAL_ONLY);

	sprintf(val,"%d",PAF_PUREVIRTUAL);
	Tcl_SetVar2(interp,var,"pure virtual",val,TCL_GLOBAL_ONLY);

	sprintf(val,"%d",PAF_INLINE);
	Tcl_SetVar2(interp,var,"inline",val,TCL_GLOBAL_ONLY);

	sprintf(val,"%d",PAF_CONSTRUCTOR);
	Tcl_SetVar2(interp,var,"structor",val,TCL_GLOBAL_ONLY);
	
	sprintf(val,"%d",PAF_OVERRIDE);
	Tcl_SetVar2(interp,var,"overridden",val,TCL_GLOBAL_ONLY);

	sprintf(val,"%d",PAF_OVERLOADED);
	Tcl_SetVar2(interp,var,"overloaded",val,TCL_GLOBAL_ONLY);
}

/*
 *----------------------------------------------------------------------
 *
 * Sn_setup_Init --
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
Sn_setup_Init(Tcl_Interp *interp)		/* Interpreter for application. */
{
	static	int	first = 1;
	Tk_Window	main_win;
	int		cmd_ret = TCL_ERROR;
	char	tmp[3000];
	char	*parg;
	char	*eval_exp = NULL;
	char	sn_interp_name[200];
	int		tcl_interactive;
	char	old_auto_path[2000];
#if _WINDOWS
	char	*null_dev = "NULL:";
#else
	char	*null_dev = "/dev/null";
#endif /* _WINDOWS */
	char pwdCmd[] = "pwd";
	char initCmd[] = "sn_tcl_tk_init";

	/* Take the auto_path variable of the master interpreter!
	 * It is convenient, if we work with the Tcl sources, but
	 * not neccessary.
	 */
	old_auto_path[0] = '\0';
	parg = Tcl_GetVar(interp,"auto_path",TCL_GLOBAL_ONLY);
	if (parg)
	{
		int	len = strlen(parg) + 1;

		if (len <= sizeof(old_auto_path))
		{
			memcpy(old_auto_path,parg,len);
		}
	}

	sprintf(tmp,"%d",first);
/* FIXME: This needs to be removed, we need to be able to call Sn_setup_Init in other interps */
	Tcl_SetVar(interp,"SN_first_interp",tmp,TCL_GLOBAL_ONLY);

	if ((parg = Tcl_GetVar (interp, "tcl_interactive", TCL_GLOBAL_ONLY)))
		tcl_interactive = atoi(parg);
	else
		tcl_interactive = 0;

	/* Now we have to set the HOME variables. It must be done
	 * for secondary interpreters too ! */
	if (!user_home)
	{
#if (TCL_MINOR_VERSION >= 1)
	        Tcl_DString envDString;

		Tcl_DStringInit(&envDString);
		TclGetEnv("HOME", &envDString);
		user_home = ckalloc(Tcl_DStringLength(&envDString)+1);
		strcpy(user_home, Tcl_DStringValue(&envDString));
		Tcl_DStringFree(&envDString);
#else
		user_home = TclGetEnv("HOME");
#endif
	}
	sn_internal_convert_path (user_home, SN_PATH_UNIX);
	parg = user_home;

	/*
	 * set the batchmode flag
	 */
	if (batchmode)
	{
		Tcl_SetVar2 (interp, "sn_arguments", "batchmode", "1", TCL_GLOBAL_ONLY);
	}

	/*
	 * If the current directory is not readable we take the 'HOME' directory.
	 */
	if ((cmd_ret = Tcl_Eval(interp,pwdCmd)) != TCL_OK)
	{
		if (chdir (user_home) != 0)
			return (cmd_ret);
	}

	Tcl_SetVar2(interp,"env","HOME",user_home,TCL_GLOBAL_ONLY);

	if (!parg || strcmp(parg,"/") == 0)
	{
		Tcl_SetVar(interp,"HOME","",TCL_GLOBAL_ONLY);
	}
	else
	{
		Tcl_SetVar(interp,"HOME",parg,TCL_GLOBAL_ONLY);
	}

	if (!Tcl_GetVar(interp,"sn_debug",TCL_GLOBAL_ONLY))
	{
		sprintf(tmp,"%d",(int)debug);
		Tcl_SetVar(interp,"sn_debug",tmp,TCL_GLOBAL_ONLY);
	}

	/* As you see, sn_home  contains the installation directory.*/
	if (!Tcl_GetVar (interp, "sn_home", TCL_GLOBAL_ONLY)) {

	    Tcl_Obj *commandObj, *resultObj;
	    char *shareDir;
	    int result;

	    static char findShareScript[] =  "set installDir {}\n\
		    set exeName [info nameofexecutable]\n\
		    set pathElements [file split $exeName]\n\
		    set listSize [llength $pathElements]\n\
		    while {$listSize > 0} {\n\
			set pathElements [lrange $pathElements 0 [expr $listSize - 1]]\n\
			set newDirectory [eval file join $pathElements share]\n\
			if {[file exists $newDirectory] && [file isdirectory $newDirectory]} {\n\
			    set installDir [eval file join $pathElements]\n\
			    break\n\
			}\n\
			incr listSize -1\n\
		    }\n\
		    return $installDir\n";

	    commandObj = Tcl_NewStringObj(findShareScript, -1);
	    Tcl_IncrRefCount(commandObj);
/* Be compatible with tcl8.1. */
#if (TCL_MAJOR_VERSION >= 8) && (TCL_MINOR_VERSION == 1)            
	    result = Tcl_EvalObj(interp, commandObj, 0);
#else
	    result = Tcl_EvalObj(interp, commandObj);
#endif
	    Tcl_DecrRefCount(commandObj);
	    if (TCL_ERROR != result) {
		resultObj = Tcl_GetObjResult(interp);
		shareDir = Tcl_GetStringFromObj(resultObj, NULL);
		strcpy(installdir, shareDir);

		Tcl_SetVar (interp, "sn_home", installdir, TCL_GLOBAL_ONLY);
	    }
	}

	parg = Tcl_GetVar(interp,"sn_home",TCL_GLOBAL_ONLY);
#if DEBUG
	LOGGER((LOGFP,"sn_home: <%s>\nUser home: <%s>\n",
		parg,
		Tcl_GetVar(interp,"HOME",TCL_GLOBAL_ONLY)))
#endif /* DEBUG */
 
	/*
	 * Standard tcl scripts.
	 *
	 * Add tcl path into auto_index, tcl_library and env
	 */
 	sprintf(installdir, "%s/share/tcl%s", parg, TCL_VERSION);
	Tcl_SetVar (interp, "auto_path", installdir, TCL_GLOBAL_ONLY|TCL_LIST_ELEMENT);
	Tcl_SetVar (interp, "tcl_library", installdir, TCL_GLOBAL_ONLY);
	Tcl_SetVar2(interp, "env", "TCL_LIBRARY", installdir, TCL_GLOBAL_ONLY);

	/*
	 * Program name of SN
	 */
	Tcl_SetVar (interp, "sn_product_name", PRODUCT_NAME, TCL_GLOBAL_ONLY);
	
	/*
	 * The suite this is in.
	 */
	Tcl_SetVar (interp, "sn_suite_name", SUITE_NAME, TCL_GLOBAL_ONLY);

	/*
	 * Set also current Source-Navigator version
	 */
	Tcl_SetVar (interp, "sn_product_version", VERSION, TCL_GLOBAL_ONLY);
	/*
	 * set even a project version that is build with 5 numeric 
	 * numbers, the number is built from the product version number
	 * (without ".")
	 *
	 * 1. Remove all "."
	 * 2. If there is a "B" at the end, delete it and
	 *    set a variable to true.
	 * 3. If #(result) < 4, fill the rest with "0"
	 * 4. If there was a "B" at the end of the version number
	 *    add 0 else 1.
	 *
	 * As example:
	 * 4.0.7B ==> 40700
	 * 4.0.7  ==> 40701
	 *
	 * 4.1B   ==> 41000
	 * 4.1    ==> 41001
	 * 4.1.1  ==> 41101
	 *
	 * 5.0.0B ==> 50000
	 * 5.0.0  ==> 50001
	 * 5.0.0.1==> 50011
	 *
	 * (correct order)
	 */
	Tcl_SetVar (interp, "sn_project_version", "41001", TCL_GLOBAL_ONLY);

	LOGGER((LOGFP,"auto_path1: <%s>\n",
		Tcl_GetVar(interp,"auto_path",TCL_GLOBAL_ONLY)));

	if (Tcl_Init(interp) == TCL_ERROR)
	{
#if !_WINDOWS
		fprintf(stderr,"%s\n",interp->result);
#endif /* !_WINDOWS */

		return TCL_ERROR;
	}

	/* Standard tcl scripts, Tcl_Init() changes auto_path,
	 * thus we have to set it again !
	 * If you change it please be very careful!
	 */
 	sprintf (tmp, "%s/share/tcl%s", parg, TCL_VERSION);
	Tcl_SetVar (interp, "auto_path", tmp,
					TCL_GLOBAL_ONLY | TCL_LIST_ELEMENT);
	/*
	 * Standard tk scripts.
	 */
 	sprintf(tmp,"%s/share/tk%s",parg,TK_VERSION);
	Tcl_SetVar (interp, "tk_library", tmp, TCL_GLOBAL_ONLY);
	Tcl_SetVar (interp, "auto_path", tmp,
		TCL_GLOBAL_ONLY|TCL_LIST_ELEMENT|TCL_APPEND_VALUE);
	Tcl_SetVar2(interp,"env","TK_LIBRARY",tmp,TCL_GLOBAL_ONLY);

	LOGGER((LOGFP,"auto_path 2: <%s>\n",
		Tcl_GetVar(interp,"auto_path",TCL_GLOBAL_ONLY)));

	if (Tk_Init(interp) == TCL_ERROR)
	{
#if !_WINDOWS
		fprintf(stderr,"%s\n",interp->result);

		exit(2);
#else
		return TCL_ERROR;
#endif /* !_WINDOWS */
	}

	setlocale(LC_ALL,"C");	/* On SCO X11 might change it, thus
				 * we set it again. */

	Tcl_SetVar(interp,"tk_strictMotif","1",TCL_GLOBAL_ONLY);

	cmd_ret = Itcl_Init(interp);
	if(cmd_ret != TCL_OK)
	{
		LOGGER((LOGFP,"Itcl Error: %s\n",interp->result));
		return cmd_ret;
	}

	cmd_ret = Itk_Init(interp);
	if(cmd_ret != TCL_OK)
	{
		LOGGER((LOGFP,"Itk Error: %s\n",interp->result));
		return cmd_ret;
	}

	LOGGER((LOGFP,"auto_path 3: <%s>\n",
		Tcl_GetVar(interp,"auto_path",TCL_GLOBAL_ONLY)));

	main_win = Tk_MainWindow(interp);

	/*
	 * Init the tix library *
	 ************************/
	cmd_ret = Tix_Init(interp);
	if (cmd_ret != TCL_OK) {
		return cmd_ret;
	}

	/*
	 * Is this an EL/IX specific feature?
	 */

	strcpy(tmp, "sn_elix");
	Tcl_LinkVar(interp, tmp, (char *) &sn_elix,
		    TCL_LINK_INT | TCL_LINK_READ_ONLY);
	
	/*
	 * Image types are shared between interps, so this only
	 * needs to be done once.
	 */
	 
/* This first value is not getting set properly, getting run on second load */
	if (first)
	{
	        ide_create_xpm_image_type();
	}

	/*
	 * Init libgui tclIndex path
	 */
	ide_initialize_paths (interp, (char*)Tk_Name(main_win));

	sn_init_mycommands (interp, (ClientData)main_win);

	if (first)
	{
		Tcl_CreateExitHandler((Tcl_ExitProc *)mx_Tcl_ExitHandler,
			(ClientData)NULL);
	}

	/*
	 * SN tcl scripts
	 */
	if (old_auto_path[0])
	{
		/* Take "auto_path" of the master interpreter! */
		Tcl_SetVar (interp, "auto_path", old_auto_path, TCL_GLOBAL_ONLY);
	}
	else
	{
		char *b1, *b2;
		/*
		 * If SN path contains blanks, add brackets to it
		 */
		if (strchr (parg, ' '))
		{
			b1 = "{";
			b2 = "}";
		}
		else
		{
			b1 = "";
			b2 = "";
		}
	 	sprintf (tmp, "%s%s/share/sourcenav/gui%s %s%s/share/sourcenav/packages%s %s",
						b1, parg, b2, b1, parg, b2,
						Tcl_GetVar(interp,"auto_path", TCL_GLOBAL_ONLY));
		Tcl_SetVar (interp, "auto_path", tmp, TCL_GLOBAL_ONLY);
	}
	LOGGER((LOGFP,"auto_path 4: <%s>\n",
		Tcl_GetVar(interp,"auto_path",TCL_GLOBAL_ONLY)));

	cmd_ret = Tcl_Eval(interp, initCmd);
	if(cmd_ret != TCL_OK)
	{
		LOGGER((LOGFP,"sn_tcl_tk_init Error: %s\n",interp->result));

		if ((parg = Tcl_GetVar(interp,"errorInfo",TCL_GLOBAL_ONLY)))
		{
			LOGGER((LOGFP,"  errorInfo <%s>\n",parg));
		}
#if !_WINDOWS
		fprintf(stderr,"Error: %s\n",interp->result);
		exit(2);
#endif /* !_WINDOWS */
		return TCL_ERROR;
	}
	strcpy(sn_interp_name,(char *)Tk_Name(main_win));

	LOGGER((LOGFP,"SN interp: %s\n",sn_interp_name));

	Tcl_SetVar(interp,"sn_interp_name", sn_interp_name, TCL_GLOBAL_ONLY);
	SN_set_scope_bits(interp);
	
	LOGGER((LOGFP,"first: %d, pid: %lu\n",
		first, (unsigned long)getpid()));
		
	if (first)
	{
		int	fd;

		first = FALSE;

		/* We need the highest file descriptor. */
		fd = open(null_dev,O_RDONLY);
		if (fd != -1)
		{
			close(fd);
		}
		/* The currently open files should not be accessible
		 * in child processes. */
		for (; --fd > 2; fd--)
		{
#ifndef _WINDOWS
			fcntl(fd, F_SETFD, FD_CLOEXEC);
#endif
		}
	}

	if (tcl_interactive)
	{
		eval_exp = NULL;
	}
	if (eval_exp)
	{
		LOGGER((LOGFP,"Evaluating: <%s>\n",eval_exp));
		cmd_ret = Tcl_Eval(interp,eval_exp);
		if (cmd_ret != TCL_OK)
		{
			LOGGER((LOGFP,"Tcl Error: %s\n",interp->result));
		}
	}

	Tcl_StaticPackage(interp, "Tk", (Tcl_PackageInitProc *)Tk_Init,
		(Tcl_PackageInitProc *) NULL);

	Tcl_StaticPackage(interp, "Sn_setup", (Tcl_PackageInitProc *)Sn_setup_Init,
		(Tcl_PackageInitProc *) NULL);

	LOGGER((LOGFP,"App_Init returns: %d\n",cmd_ret));

	return cmd_ret;
}

static void
mx_Tcl_ExitHandler(ClientData clientData)
{
	LOGGER((LOGFP,"Exit handler\n"));

#if DEBUG
	if (LOGFP)
	{
		fclose(LOGFP);
		LOGFP = (FILE *)0;
		debug = 0;
	}
#endif /* DEBUG */
}
