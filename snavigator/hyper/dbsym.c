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

#if DB_DLL
#define	MAIN_MODULE
#endif /* DB_DLL */

#include <config.h>

#include <stdlib.h>
#include <ctype.h>

#include "mxdefine.h"
#include "mxfuncs.h"
#include "mxlogger.h"
#include "db_185.h"

#include "tcl.h"
#include "sn.h"
#include "fileutils.h"
#include "dbutils.h"

#include <compat.h>

#if DB_DLL
#define	DB_DLL_EXPORT	__declspec(dllexport)
#define	DB_TCL_OPEN	db_dll_open
DB_DLL_EXPORT int _Paftcldb_Init(Tcl_Interp *interp);
#else
#define	DB_DLL_EXPORT
#define	DB_TCL_OPEN	dbtclopen
#endif /* DB_DLL */

#if WIN32 && ! defined (__CYGWIN32__)
#define MX_STAT _stat
#else
#define MX_STAT stat
#endif /* WIN32 */

enum
{
	DB_MATCH_NONE,
	DB_MATCH_GLOB,
	DB_MATCH_REGEXP,
	DB_MATCH_OCCURE,
	DB_MATCH_BEG,
	DB_MATCH_END
};

#define MAX_FORMAT_COLS 256

typedef struct
{
	DB      *dbp;
	char    *filename;
	char    *tclcmd;
	int     mode;
	int     prot;
	DBTYPE  dbtype;
	union
	{
		BTREEINFO       br;
		HASHINFO        ha;
		RECNOINFO       re;
	} info;
	DB      *db_exclude;            /* PAF file table. */
	time_t  mtime;                  /* Modification time */
} tcldbpars;

extern int db_compare_nocase;
extern int db_action_is_fetching;
int db_case_compare(const DBT *, const DBT *);
int db_no_case_compare(const DBT *, const DBT *);
int get_db_permission();

DB_DLL_EXPORT int DB_TCL_OPEN(ClientData cli,Tcl_Interp *in,int argc,char **argv);
static int sn_put_project_dir_files(ClientData cli, Tcl_Interp *in, int argc, char **argv);

static DBTYPE dbtype(char *s);
static char *GetOpenMode(Tcl_Interp *in, char *string, int *modePtr);
static char *glob_nocase_pattern(char *p);
static int dbDispCmd(ClientData cli, Tcl_Interp *in, int argc, Tcl_Obj *CONST objv[]);
static int dbtclclose(Tcl_Interp *in,tcldbpars *pars, char *cmd);
static int dbtcldel(Tcl_Interp *in,tcldbpars *pars, int argc, char **argv);
static int dbtclfetch(Tcl_Interp *in,tcldbpars *pars, int argc, char **argv);
static int dbtclsync(Tcl_Interp *in, tcldbpars *pars, int flag);
static int excluded(char *file_name, int size, DB *db_exclude);
static int setflags(char *s);
static int dbtcldestroy(ClientData cli);
static void setinfo(DBTYPE type, char *s, void *infop);

int my_SplitList (char *str, int *num, char ***argvPtr, char sep);
static char * sn_my_JoinStr (char *str);

#if DB_DLL
typedef	char *(*GETVAR)(Tcl_Interp *interp,char *part1,int flags);
GETVAR SN_Tcl_GetVar;
#define	Tcl_GetVar SN_Tcl_GetVar

typedef Tcl_Command (*CREATE_CMD)(Tcl_Interp *interp,char *cmdName, Tcl_CmdProc *proc,ClientData clientData,Tcl_CmdDeleteProc *deleteProc);
static CREATE_CMD	SN_Tcl_CreateCommand;
#define	Tcl_CreateCommand SN_Tcl_CreateCommand

typedef	int (*DELETE_CMD) (Tcl_Interp *interp,char *cmdName);
static DELETE_CMD	SN_Tcl_DeleteCommand;
#define	Tcl_DeleteCommand SN_Tcl_DeleteCommand

typedef void (*STRING_INIT)(Tcl_DString *dsPtr);
static STRING_INIT	SN_Tcl_DStringInit;
#define	Tcl_DStringInit SN_Tcl_DStringInit

typedef	char *(*TRANS_FILENAME)(Tcl_Interp *interp,char *name,Tcl_DString *bufferPtr);
static TRANS_FILENAME SN_Tcl_TranslateFileName;
#define	Tcl_TranslateFileName SN_Tcl_TranslateFileName

typedef	char *(*POSIX_ERROR)(Tcl_Interp *interp);
static POSIX_ERROR SN_Tcl_PosixError;
#define	Tcl_PosixError SN_Tcl_PosixError

typedef	void (*APPEND_RESULT)TCL_VARARGS(Tcl_Interp *,interp);
static APPEND_RESULT SN_Tcl_AppendResult;
#define	Tcl_AppendResult SN_Tcl_AppendResult

typedef	void (*SET_RESULT)(Tcl_Interp *interp,char *string,Tcl_FreeProc *freeProc);
static SET_RESULT SN_Tcl_SetResult;
#define	Tcl_SetResult SN_Tcl_SetResult;

typedef	void (*STRING_FREE)(Tcl_DString *dsPtr);
static STRING_FREE SN_Tcl_DStringFree;
#define	Tcl_DStringFree SN_Tcl_DStringFree

typedef	void (*MY_FREE)(char *ptr);
static MY_FREE SN_Tcl_Free;
#define	Tcl_Free SN_Tcl_Free

typedef	int (*SPLIT_LIST)(Tcl_Interp *interp,char *list,int *argcPtr,char ***argvPtr);
static SPLIT_LIST SN_Tcl_SplitList;
#define	Tcl_SplitList SN_Tcl_SplitList

typedef	void (*RESET_RESULT)(Tcl_Interp *interp);
static RESET_RESULT SN_Tcl_ResetResult;
#define	Tcl_ResetResult SN_Tcl_ResetResult

typedef	int (*GET_INT)(Tcl_Interp *interp,char *string,int *intPtr);
static GET_INT SN_Tcl_GetInt;
#define	Tcl_GetInt SN_Tcl_GetInt

typedef	int (*GET_COMMAND)(Tcl_Interp *interp,char *cmdName, Tcl_CmdInfo *infoPtr);
static GET_COMMAND SN_Tcl_GetCommandInfo;
#define	Tcl_GetCommandInfo SN_Tcl_GetCommandInfo

typedef	void (*APPEND_ELEMENT)(Tcl_Interp *interp,char *string);
static APPEND_ELEMENT SN_Tcl_AppendElement;
#define	Tcl_AppendElement SN_Tcl_AppendElement

typedef	int (*STRING_MATCH)(char *string,char *pattern);
static STRING_MATCH SN_Tcl_StringMatch;
#define	Tcl_StringMatch SN_Tcl_StringMatch

typedef	char *(*DSTRING_APPEND)(Tcl_DString *dsPtr,char *string, int length);
static DSTRING_APPEND SN_Tcl_DStringAppend;
#define	Tcl_DStringAppend SN_Tcl_DStringAppend

typedef	char *(*DSTRING_APPEND_ELEMENT)(Tcl_DString *dsPtr, char *string);
static DSTRING_APPEND_ELEMENT SN_Tcl_DStringAppendElement;
#define	Tcl_DStringAppendElement SN_Tcl_DStringAppendElement

typedef	int (*REGEXPEXEC)(Tcl_Interp *interp,Tcl_RegExp regexp,char *string,char *start);
static REGEXPEXEC SN_Tcl_RegExpExec;
#define	Tcl_RegExpExec SN_Tcl_RegExpExec

typedef	Tcl_RegExp	(*REGEXPCOMPILE)(Tcl_Interp *interp,char *string);
static REGEXPCOMPILE SN_Tcl_RegExpCompile;
#define	Tcl_RegExpCompile SN_Tcl_RegExpCompile

typedef	void (*ADDERRORINFO)(Tcl_Interp *interp,char *message);
static ADDERRORINFO SN_Tcl_AddErrorInfo;
#define	Tcl_AddErrorInfo SN_Tcl_AddErrorInfo

static int
paf_load_tcl_lib_cmd()
{
	HINSTANCE	tcldll;
	char	tmp[200];

	if (SN_Tcl_CreateCommand)		/* We have already run !!! */
		return TRUE;

	sprintf(tmp,"Tcl%d%d.dll",TCL_MAJOR_VERSION,TCL_MINOR_VERSION);
	tcldll = GetModuleHandle(tmp);
	if (!tcldll)
	{
		tcldll = GetModuleHandle("Tcl.dll");
		if (!tcldll)
			return FALSE;
	}


	SN_Tcl_GetVar = (GETVAR)GetProcAddress(tcldll,"Tcl_GetVar");
	if (!SN_Tcl_GetVar)
		return FALSE;

	SN_Tcl_CreateCommand = (CREATE_CMD)GetProcAddress(tcldll,"Tcl_CreateCommand");
	if (!SN_Tcl_CreateCommand)
		return FALSE;

	SN_Tcl_DeleteCommand = (DELETE_CMD)GetProcAddress(tcldll,"Tcl_DeleteCommand");
	if (!SN_Tcl_DeleteCommand)
		return FALSE;

	SN_Tcl_DStringInit = (STRING_INIT)GetProcAddress(tcldll,"Tcl_DStringInit");
	if (!SN_Tcl_DStringInit)
		return FALSE;

	SN_Tcl_TranslateFileName = (TRANS_FILENAME)GetProcAddress(tcldll,"Tcl_TranslateFileName");
	if (!SN_Tcl_TranslateFileName)
		return FALSE;

	SN_Tcl_PosixError = (POSIX_ERROR)GetProcAddress(tcldll,"Tcl_PosixError");
	if (!SN_Tcl_PosixError)
		return FALSE;

	SN_Tcl_AppendResult = (APPEND_RESULT)GetProcAddress(tcldll,"Tcl_AppendResult");
	if (!SN_Tcl_AppendResult)
		return FALSE;

	SN_Tcl_SetResult = (SET_RESULT)GetProcAddress(tcldll,"Tcl_SetResult");
	if (!SN_Tcl_SetResult)
		return FALSE;

	SN_Tcl_DStringFree = (STRING_FREE)GetProcAddress(tcldll,"Tcl_DStringFree");
	if (!SN_Tcl_DStringFree)
		return FALSE;

	SN_Tcl_Free = (MY_FREE)GetProcAddress(tcldll,"Tcl_Free");
	if (!SN_Tcl_Free)
		return FALSE;

	SN_Tcl_SplitList = (SPLIT_LIST)GetProcAddress(tcldll,"Tcl_SplitList");
	if (!SN_Tcl_SplitList)
		return FALSE;

	SN_Tcl_ResetResult = (RESET_RESULT)GetProcAddress(tcldll,"Tcl_ResetResult");
	if (!SN_Tcl_ResetResult)
		return FALSE;

	SN_Tcl_GetInt = (GET_INT)GetProcAddress(tcldll,"Tcl_GetInt");
	if (!SN_Tcl_GetInt)
		return FALSE;

	SN_Tcl_GetCommandInfo = (GET_COMMAND)GetProcAddress(tcldll,"Tcl_GetCommandInfo");
	if (!SN_Tcl_GetCommandInfo)
		return FALSE;

	SN_Tcl_AppendElement = (APPEND_ELEMENT)GetProcAddress(tcldll,"Tcl_AppendElement");
	if (!SN_Tcl_AppendElement)
		return FALSE;

	SN_Tcl_StringMatch = (STRING_MATCH)GetProcAddress(tcldll,"Tcl_StringMatch");
	if (!SN_Tcl_StringMatch)
		return FALSE;

	SN_Tcl_DStringAppend = (DSTRING_APPEND)GetProcAddress(tcldll,"Tcl_DStringAppend");
	if (!SN_Tcl_DStringAppend)
		return FALSE;

	SN_Tcl_DStringAppendElement = (DSTRING_APPEND_ELEMENT)GetProcAddress(tcldll,"Tcl_DStringAppendElement");
	if (!SN_Tcl_DStringAppendElement)
		return FALSE;

	SN_Tcl_RegExpExec = (REGEXPEXEC)GetProcAddress(tcldll,"Tcl_RegExpExec");
	if (!SN_Tcl_RegExpExec)
		return FALSE;

	SN_Tcl_RegExpCompile = (REGEXPCOMPILE)GetProcAddress(tcldll,"Tcl_RegExpCompile");
	if (!SN_Tcl_RegExpCompile)
		return FALSE;

	SN_Tcl_AddErrorInfo = (ADDERRORINFO)GetProcAddress(tcldll,"Tcl_AddErrorInfo");
	if (!SN_Tcl_AddErrorInfo)
		return FALSE;


	return TRUE;
}
#endif /* DB_DLL */

char *SN_StrDup(char*);

static  char    *
show_flag(int flag)
{
	switch (flag)
	{
	case 0:
		return "default";
		break;
		
	case    R_CURSOR:
		return "R_CURSOR";
		break;

	case    R_FIRST:
		return "R_FIRST";
		break;

	case    R_IAFTER:
		return "R_IAFTER";
		break;

	case    R_IBEFORE:
		return "R_IBEFORE";
		break;

	case    R_LAST:
		return "R_LAST";
		break;

	case    R_NEXT:
		return "R_NEXT";
		break;

	case    R_NOOVERWRITE:
		return "R_NOOVERWRITE";
		break;

	case    R_PREV:
		return "R_PREV";
		break;

	case    R_SETCURSOR:
		return "R_SETCURSOR";
		break;
	}
	return ("unknown");
}

static  int
setflags(char *s)
{
	char *p;

	for (; isspace(*s); ++s);
	if (*s == '\n' || *s == '\0')
		return (0);
	if ((p = strchr(s, '\n')) != NULL)
		*p = '\0';
	if (!strcmp(s, "R_CURSOR"))             return (R_CURSOR);
	if (!strcmp(s, "R_FIRST"))              return (R_FIRST);
	if (!strcmp(s, "R_IAFTER"))             return (R_IAFTER);
	if (!strcmp(s, "R_IBEFORE"))            return (R_IBEFORE);
	if (!strcmp(s, "R_LAST"))               return (R_LAST);
	if (!strcmp(s, "R_NEXT"))               return (R_NEXT);
	if (!strcmp(s, "R_NOOVERWRITE"))        return (R_NOOVERWRITE);
	if (!strcmp(s, "R_PREV"))               return (R_PREV);
	if (!strcmp(s, "R_SETCURSOR"))          return (R_SETCURSOR);

	return 0;
}

/* Function returns whether a file is hidden. */

static  int
excluded(char *file_name,int name_size,DB *db_exclude)
{
	DBT     data;
	DBT     key;
	int     excl;

	key.data = file_name;
	key.size = name_size;
	if (db_exclude && db_exclude->get(db_exclude,&key,&data,0) == 0)
	{
		excl = TRUE;
	}
	else
		excl = FALSE;
	LOGGER2((LOGFP,"\"%s\" exclude flag: %d\n",file_name,excl));

	return excl;
}

/*
 * This function checks return TRUE if the table is empty
 * otherwise FALSE.
 */
static  int
dbisempty(tcldbpars *pars)
{
	DB      *dbp = pars->dbp;
	DB      *db_exclude = pars->db_exclude;
	DBT     data;
	DBT     key;
	int     ret = -1;
	int     flag;
	int     cou;
	int     len;
	char    *keyvalue;

	key.data = NULL;
	key.size = 0;
	data.data = NULL;
	data.size = 0;
	for (cou = 0, flag = R_FIRST;
		cou == 0 && (ret = dbp->seq(dbp,&key,&data,flag)) == 0; flag = R_NEXT)
	{
		LOGGER2((LOGFP,"dbisempty key.size: %lu data.size: %lu\n",
			(unsigned long)key.size,(unsigned long)data.size));
		if (key.size == 0 || !key.data)         /* Should never happen !! */
			continue;

		keyvalue = key.data;
		if (db_exclude)
		{
			char	*pn;

			for (pn = &keyvalue[key.size - 1]; pn >= keyvalue && *pn != DB_FLDSEP_CHR; pn--);
			if (pn >= keyvalue)
			{
				pn++;
				len = key.size - (int)(pn - keyvalue);
			}
			else
			{
				pn = keyvalue;  /* Only one field in the record. */
				len = key.size;
			}

			if (excluded(pn, len, db_exclude))
			{
				continue;
			}
		}
		cou++;
	}
	if (ret == -1)
	{
		LOGGER((LOGFP,"dbisempty error: %s\n",strerror(errno)));

		return -1;
	}
	LOGGER((LOGFP,"%s isempty: %d\n",
		pars->filename, cou ? FALSE : TRUE));

	return cou ? FALSE : TRUE;
}

static  char *
glob_nocase_pattern(char *p)
{
	static char glob_pat[1024];
	char *s;

	for (s = glob_pat; *p; )
	{
		if (*p == '\\')
		{
			*s++ = *p++;
			if (*p)
				*s++ = *p++;
		}
		else if (*p == '[')
		{
			while (*p != '\0' && *p != ']')
				*s++ = *p++;
		}
		else if (islower(*p))
		{
			*s++ = '[';
			*s++ = toupper(*p);
			*s++ = *p++;
			*s++ = ']';
		}
		else if (isupper(*p))
		{
			*s++ = '[';
			*s++ = tolower(*p);
			*s++ = *p++;
			*s++ = ']';
		}
		else
		{
			*s++ = *p++;
		}

		if (s >= glob_pat + sizeof(glob_pat) -5)
			return NULL;
	}
	*s = '\0';

	return glob_pat;
}

/*
 * SN batch, the file command can't work with list elements, so a
 * command like "file exists {file contains blanks.c}" won't work
 *
 * We must make sure that the file is converted to a string before
 * it has been accessed.
 *
 * Join a tcl/list to a string, because we could have spaces in
 * filenames, it could happen that a file is given as "{space hello.c}",
 * this must be converted to a string "space hello.c"
 */
static char *
sn_my_JoinStr (char *str)
{
	static char *buf=NULL;
	char *p, *q;
	
	if (buf)
	{
		ckfree(buf);
		buf = NULL;
	}
	if (*str != '{')
	{
		return str;
	}
	
	buf = SN_StrDup (str);
	
	for (p=q=buf; *p; p++)
	{
		if (*p == '{' || *p == '}')
		{
			continue;
		}
		*q ++ = * p;
	}
	*q = 0;
	
	return buf;
}

/*
 * Convert strings to tcl/lists, something like:
 *
 * "foo|c files/hello.c|123|{int hello}" ==> "foo {c files/hello.c} 123 {int hello}"
 */
static char *
SN_convert_separator_to_blank (char *str, int len)
{
	static char *buf=NULL;
	static int buflen = -1;
	int i, contains_sep;
	
	contains_sep = strchr (str, DB_FLDSEP_CHR) != NULL;
	
	/*
         * enlarge the static buffer if needed to
         */
	if (len*2+2 > buflen)
	{
		buflen = len*2+2;
		if (buf)
		{
			ckfree (buf);
		}
		buf = ckalloc (buflen);
	}
		
	/*
	 * we need to duplicate the string before changing it,
	 * because it could be part of the db cache
	 */
	if (contains_sep && strchr (str, ' '))
	{
		char *begPtr, *q, *p=str;
		int have_blank=0;
	
		for (begPtr=p, q=buf, i=0; i<=len; i++, p++)
		{
			if (*p == ' ')
			{
				have_blank = 1;
			}
			else if (*p == '{' || *p == '}')
			{
				have_blank = 0;
			}
			else if (*p == DB_FLDSEP_CHR || *p==0)
			{
				if (have_blank)
				{
					*q ++ = '{';
				}
				memcpy (q, begPtr, p-begPtr);
				q += p-begPtr;
				if (have_blank)
				{
					*q ++ = '}';
				}
				if (*p != 0)
				{
					*q ++ = ' ';
				}
				
				begPtr = p+1;
				have_blank = 0;
			}
		}
		*q = 0;
		
		return buf;
	}
	else if (contains_sep)
	{
	    	char *q, *p;
		for (q=buf, p=str, i=0; i<len; i++, p++, q++)
		{
			if (*p == DB_FLDSEP_CHR)
			{
				*q = ' ';
			}
		        else
			{
	    		    *q = *p;
			}
		}
		*q = 0;
		return buf;
    	}
	/*
	 * return the same string as long as it's not changed
	 */
	return str;
}

void ViewArgs (char *reason, int argc, char *argv[], int mode);

#define UPDATE_COMMAND

static  int
dbtclfetch(Tcl_Interp *interp,tcldbpars *pars,int argc,char **argv)
{
	DB      *dbp = pars->dbp;
	DB      *db_exclude = pars->db_exclude;
	DBT     data;
	DBT     key;
	int     flag = 0;
	char    *pattern = NULL;
	char    *keyvalue;
	int     (*ftch)();
	unsigned int    pattern_len = 0;
	int     get_all;
	int     fld_num = 0;
	int     col_num = 0;
	char    **fields = NULL;
	char    **sub_list = NULL;
	char    *formats[MAX_FORMAT_COLS];
	int     columns[MAX_FORMAT_COLS];
	int     cou;
	int     ret;
	char    *result_filter = NULL;
#if 0
	char    *nocase_pattern = NULL;
#endif
	char    *strstr_pattern = NULL;
	char    *end_pattern = NULL;
	char    *regexp_pattern = NULL;
	char    *beg_pattern = NULL;
	char    *glob_pattern = NULL;
	char    *format_str = NULL;
	char    *last_space = NULL;
	Tcl_DString     resulting;
	Tcl_DString     record;
	Tcl_RegExp      regexp_ptr = 0;
	int match_type = DB_MATCH_NONE;
	int     add_keys = TRUE;
	int     add_data = TRUE;
	int     target_field = 0;
	int     max_num = -1;
	long    counter = 0;
	long    scan_counter = 0;
	int     beg_pattern_size = 0;
	int     only_first = 0;
	char    format_buf[1000];
	int     uniq = FALSE;
	int     last_len = 0;
	int		last_returned_bufsize = 0;
	char    *last_returned_record = NULL;
	int		cross_format_bufsize = 0, oldxref_bufsize = 0;
	char		*cross_format_buf = NULL, *oldxref_buf = NULL;
	int     cross_format = FALSE;
	int		skip_record;
	int     single_list = FALSE;
	Tcl_Obj	*res_obj = NULL;
	
#ifdef UPDATE_COMMAND
	char *update_command = NULL;
	int update_stips = 500; /* update every 500 records */
#endif

	if (db_compare_nocase)
	{
		db_compare_nocase = FALSE;
	}
	db_action_is_fetching = 1;
	
	Tcl_DStringInit(&record);
	Tcl_DStringInit(&resulting);

	memset((char *)formats,0,sizeof(formats));

	if (argv[1][0] == 'g')
	{
		get_all = FALSE;
		ftch = dbp->get;
	}
	else
	{
		get_all = TRUE;
		flag = R_CURSOR;
		ftch = dbp->seq;
	}

	for (;argc > 2 && argv[2][0] == '-'; argv++, argc--)
	{
		if (strncmp(argv[2],"-columns",4) == 0)
		{
			if(Tcl_SplitList(interp,argv[3],&col_num,&fields) != TCL_OK)
				goto error;

			memset((char *)formats,0,sizeof(formats));

			for (cou = 0; cou < col_num; cou++)
			{
				if(Tcl_SplitList(interp,fields[cou],&ret,&sub_list) != TCL_OK)
				{
					columns[cou] = 0;
					goto error;
				}

				if (Tcl_GetInt(interp,sub_list[0],&columns[cou]) != TCL_OK)
				{
					columns[cou] = 0;
					goto error;
				}

				if (columns[cou] >= MAX_FORMAT_COLS)
					columns[cou] = MAX_FORMAT_COLS - 1;
				else if (columns[cou] < 0)
					columns[cou] = 0;

				if (ret > 1)
				{
					if (formats[columns[cou]])
						ckfree(formats[columns[cou]]);

					ret = strlen(sub_list[1]) + 1;
					formats[columns[cou]] = (char *)ckalloc(ret);
					memcpy(formats[columns[cou]],sub_list[1],ret);
				}

				ckfree((char *)sub_list);
				sub_list = NULL;
			}
			ckfree ((char *)fields);
			fields = NULL;
			argv++;
			argc--;
		}
		else if (strncmp(argv[2],"-glob",4) == 0)
		{
			glob_pattern = argv[3];
			if (*glob_pattern && strcmp(glob_pattern,"*") != 0)
				match_type = DB_MATCH_GLOB;
			else
				glob_pattern = NULL;
			argv++;
			argc--;
		}
		else if (strncmp(argv[2],"-result_filter",4) == 0)
		{
			result_filter = argv[3];
			if (*result_filter == '\0' || strcmp(result_filter,"*") == 0)
				result_filter = NULL;
			else
				result_filter = glob_nocase_pattern(result_filter);
			argv++;
			argc--;
		}
		else if (strncmp(argv[2],"-beg",4) == 0)
		{
			beg_pattern = argv[3];
			if (*beg_pattern)
			{
				beg_pattern_size = strlen(beg_pattern);
				match_type = DB_MATCH_BEG;
			}
			else
				beg_pattern = NULL;
			argv++;
			argc--;
		}
		else if (strncmp(argv[2],"-end",4) == 0)
		{
			end_pattern = argv[3];
			if (*end_pattern)
			{
				match_type = DB_MATCH_END;
			}
			else
				end_pattern = NULL;
			argv++;
			argc--;
		}
		else if (strncmp(argv[2],"-regexp",4) == 0)
		{
			regexp_pattern = argv[3];
			if (*regexp_pattern)
			{
				regexp_ptr = Tcl_RegExpCompile(interp,regexp_pattern);
				if (!regexp_ptr)
				{
					goto error;
				}
				match_type = DB_MATCH_REGEXP;
			}
			else
				regexp_pattern = NULL;
			argv++;
			argc--;
		}
		else if (strncmp(argv[2],"-strstr",4) == 0)
		{
			strstr_pattern = argv[3];
			if (*strstr_pattern)
			{
				match_type = DB_MATCH_OCCURE;
			}
			else
				strstr_pattern = NULL;
			argv++;
			argc--;
		}
		else if (strncmp(argv[2],"-nocase",4) == 0)
		{
			db_compare_nocase = TRUE;
#if 0
			nocase_pattern = argv[3];
			if (*nocase_pattern)
			{
				nocase_pattern = glob_nocase_pattern(nocase_pattern);
				match_type = DB_MATCH_GLOB;
			}
			else
				nocase_pattern = NULL;
			argv++;
			argc--;
#endif
		}
		else if (strncmp (argv[2], "-exact", 4) == 0)
		{
			/* nothing to do, to exchange "-exact" with "-nocase" using a flag */
		}
		else if (strncmp(argv[2],"-key",4) == 0)
		{
			add_keys = FALSE;
		}
		else if (strncmp(argv[2],"-data",4) == 0)
		{
			add_data = FALSE;
		}
		else if (strncmp(argv[2],"-first",4) == 0)
		{
			only_first = TRUE;
		}
		else if (strncmp(argv[2],"-format",4) == 0)
		{
			format_str = argv[3];
			if (!*format_str)
				format_str = NULL;
			argv++;
			argc--;
		}
		else if (strncmp(argv[2],"-uniq",4) == 0)
		{
			uniq = TRUE;
			if (!last_returned_record)
			{
				last_returned_bufsize = 2000;
				last_returned_record = ckalloc(last_returned_bufsize);
				if (!last_returned_record)
				{
					LOGGER((LOGFP,"could not alloc memory"));
					goto error;
				}
				last_returned_record[0] = '\0';
			}
		}
		else if (strncmp(argv[2],"-cross",4) == 0)
		{
			cross_format = TRUE;
			match_type = DB_MATCH_NONE;     /* switch it off! */
			cross_format_bufsize = 3000;
			cross_format_buf = ckalloc(cross_format_bufsize);
			
			if (!cross_format_buf)
			{
				LOGGER((LOGFP,"could not alloc memory"));
				goto error;
			}
		}
		else if (strncmp(argv[2],"-list",5) == 0)
		{
			single_list = TRUE;
		}
#ifdef UPDATE_COMMAND
		/* Khamis: Fetching can be canceled */
		else if (strcmp (argv[2], "-updatecommand") == 0)
		{
			update_command = argv[3];
			argv++;
			argc--;
		}
		/* Khamis: call update command every therteen records */
		else if (strcmp (argv[2], "-update") == 0)
		{
			update_stips = atoi (argv[3]);
			argv++;
			argc--;
		}
#endif
		else
		{
			Tcl_AppendResult(interp, argv[0]," ", argv[1],
				" ?-columns list? ?-glob|-end|-regexp|-result_filter|-nocase|-beg|-strstr pattern?",
				" ?-key? ?-data? ?-append str? ?-uniq? are supported",
				NULL);

			goto error;
		}
	}

	if (col_num == 0)
	{
		for (cou = 0; cou < sizeof(columns) / sizeof(*columns); cou++)
		{
			columns[cou] = cou;
		}
	}

	if (argc > 2)
	{
		pattern = sn_my_JoinStr (argv[2]);
	}
	else
	{
		pattern = NULL;
	}

	if (!pattern || *pattern == '\0')
	{
		pattern = NULL;
		key.data = NULL;
		key.size = 0;
		pattern_len = 0;
		/*
		 * Sequence
		 */
		if (get_all)
			flag = R_FIRST;
	}
	else
	{
		pattern_len = strlen(pattern);
		key.data = pattern;
		key.size = pattern_len + 1;             /* Include '\0' too ! */
	}

	if (argc > 3)
		flag = setflags(argv[3]);

	LOGGER((LOGFP,"db_%s <%s> "
				  "key: <%s> "
				  "pattern <%s> "
				  "result_filter: <%s> "
				  "flag: %s "
				  "match_type: %d\n"
				  "\tglob_pattern: <%s>\n"
				  "\tend_pattern <%s>\n"
				  "\tbeg_pattern <%s>\n"
				  "\tstrstr_pattern <%s>\n"
				  "\tregexp_pattern <%s>\n"
				  "\n",
		get_all ? "seq" : "get",
		pars->filename ? pars->filename : "memory",
		key.data ? (char *)key.data : "",
		pattern ? pattern : "<EMPTY>",
		result_filter ? result_filter : "",
		show_flag(flag),
		match_type,
		glob_pattern ? glob_pattern : "",
		end_pattern ? end_pattern : "",
		beg_pattern ? beg_pattern : "",
		strstr_pattern ? strstr_pattern : "",
		regexp_pattern ? regexp_pattern : ""
		));

	Tcl_ResetResult(interp);
	/*res_obj = Tcl_GetObjResult(interp);*/
	res_obj = Tcl_NewObj();

	counter = 0;
	scan_counter = 0;
	do
	{
		skip_record = FALSE;
		data.data = NULL;
		data.size = 0;
		ret = (*ftch)(dbp,&key,&data,flag);
		if (ret != 0)
		{
			break;
		}
#if 0
		LOGGER((LOGFP,"key_size: %d, data.size: %d key: <%s>\n",
			key.size,data.size,key.data));
#endif
		if (key.size == 0 || !key.data)         /* Should never happen !! */
		{
			LOGGER((LOGFP,"ERROR Empty key: %lu\n",
				(unsigned long)key.size));
			continue;
		}
		
		if (pattern && (key.size < pattern_len ||
			memcmp(pattern,key.data,pattern_len) != 0))
		{
			break;
		}
		scan_counter++;

		flag = R_NEXT;

		keyvalue = (char *)key.data;

		if (db_exclude)
		{
			char	*pn;

			for (pn = &keyvalue[key.size - 1]; pn >= keyvalue && *pn != DB_FLDSEP_CHR; pn--);
			if (pn >= keyvalue)
			{
				pn++;
				max_num = key.size - (int)(pn - keyvalue);
			}
			else
			{
				pn = keyvalue;  /* Only one field in the record. */
				max_num = key.size;
			}

			if (excluded(pn, max_num, db_exclude))
			{
				continue;
			}
		}

		if (match_type)
		{
			if (beg_pattern && strncmp(keyvalue,beg_pattern,beg_pattern_size) != 0)
			{
				continue;
			}
			
			if (strstr_pattern && !strstr(keyvalue,strstr_pattern))
			{
				continue;
			}
			
			if (glob_pattern && !Tcl_StringMatch(keyvalue, glob_pattern))
			{
				continue;
			}

			if (regexp_pattern)
			{
				switch (Tcl_RegExpExec(interp,regexp_ptr,keyvalue,keyvalue))
				{
				case -1:
					goto error;
					break;

				case 0:                 /* Not found ! */
					continue;
					break;
				}
			}

			if (end_pattern)
			{
				last_space = strrchr (keyvalue, DB_FLDSEP_CHR);
				if (last_space == NULL)
					last_space = keyvalue;
				else
					last_space++;

				if (strcmp(last_space, end_pattern) != 0)
				{
					continue;
				}
			}
		}

		if (only_first && counter > 0)
			break;
			
#ifdef UPDATE_COMMAND
		if (update_command != NULL && (counter%update_stips)==0 && counter > 0)
		{
			int res;
			res = Tcl_Eval (interp, update_command);
			if (res == TCL_OK && (interp->result && interp->result[0] == '0'))
			{
				break;  /* canceled by user */
			}
		}
#endif
			
		/* KHAMIS 05/28/97
		 * Format for the flagg -cross is now changed, lock below.
		 */
		if (cross_format)
		{
			Tcl_Obj * objp;
			char    *type_p;
			char    *class_p;
			char    *sym_p;

			if(my_SplitList (keyvalue, &fld_num, &fields, DB_FLDSEP_CHR) != TCL_OK)
			{
				continue;
			}
			if (fld_num < 6)
			{
				ckfree ((char*) fields);
				continue;
			}
			
			class_p = SN_StrDup (fields[3]);
			sym_p   = SN_StrDup (fields[4]);
			type_p  = SN_StrDup (fields[5]);

			if (!strstr_pattern || strstr (strstr_pattern, type_p))
			{
				/*
				 * we need at least the parameter list to identify functions
				 * format: symbol(??) class parameter
				 */
				char *param, **datas;
				char *p;
				int data_num, have_param, paramlen;

				if (my_SplitList ((char*)data.data, &data_num, &datas, DB_FLDSEP_CHR) != TCL_OK)
				{
					datas = NULL;
					data_num = 0;
				}
				if (data_num != 2)
				{
					param = "";
					paramlen = 0;
				}
				else
				{
					param = datas[1];
					paramlen = strlen (param);
				}
							
				/* 2560 bytes will be enough for "symbol(??) class" */
				if (2560 + paramlen > cross_format_bufsize)
				{
					cross_format_bufsize += 2560 + paramlen;
					cross_format_buf = ckrealloc(cross_format_buf,cross_format_bufsize);
				}

				/*
				 * Only fu,fd,md,mi have parameters
				 * or param isn't empty (usually by 'ud')
				 */
				if (param[0] ||
					(type_p[0] == 'f' && (type_p[1] == 'u' || type_p[1] == 'd')) ||
					(type_p[0] == 'm' && (type_p[1] == 'i' || type_p[1] == 'd')))
				{
					have_param = 1;
				}
				else
				{
					have_param = 0;
				}

				/*
				 * Build a list element of the following format:
				 *
				 *     Symbol(scope) ?Class? ?(Parameter)?
				 */
				p = cross_format_buf;
				strcpy (p, sym_p);
				p = p + strlen (p);
				/*
				 * (scope)
				 */
				*p ++ = '(';
				strcpy (p, type_p);
				p += strlen (type_p);

				*p ++ = ')';
				*p ++ = '\t';

				/*
				 * Class
				 */
				if (*class_p != '#')
				{
					strcpy (p, class_p);
					p += strlen (class_p);
				}
				*p ++ = '\t';
				
				/*
				 * Parameters
				 */
				if (have_param)
				{
					*p ++ = '(';
					strcpy (p, param);
					p +=strlen(param);
					*p ++ = ')';
				}
				/*
				 * Terminate string
				 */
				*p = 0;

				/* Add element to the result */
				if (oldxref_buf == NULL || strcmp (oldxref_buf, cross_format_buf) != 0)
				{
					int crosslen = strlen (cross_format_buf);
					
					objp = Tcl_NewStringObj(cross_format_buf, crosslen);
					Tcl_ListObjAppendElement(interp, res_obj, objp);
					
					if (oldxref_buf == NULL || crosslen >= oldxref_bufsize)
					{
						oldxref_bufsize += crosslen+1;
						oldxref_buf = ckrealloc (oldxref_buf, oldxref_bufsize);
					}
					strcpy (oldxref_buf, cross_format_buf);
				}
				
				if (datas)
				{
					ckfree ((char*)datas);
					datas = NULL;
				}
			}
			ckfree (class_p);
			ckfree (sym_p);
			ckfree (type_p);
			ckfree ((char *)fields);
			fields = NULL;

			counter++;
			if (only_first && counter > 0)
				break;
			
			continue;
		}
		
		Tcl_DStringFree(&resulting);

		if (add_keys)
		{
			if (col_num || !add_data || single_list)
			{
				Tcl_DStringAppend (&resulting, key.data, -1);
			}
			else
			{
				/*
				 * we have to convert the string into a tcl/list before
				 * adding it as an element
				 */
				char * p = SN_convert_separator_to_blank(key.data, strlen (key.data));
				Tcl_DStringAppendElement (&resulting, p);
			}
		}
		if (add_data && data.size > 1)
		{
			if (!add_keys)
			{
				Tcl_DStringAppend (&resulting, data.data, -1);
			}
			else if (col_num || single_list)
			{
				Tcl_DStringAppend (&resulting, DB_FLDSEP_STR, -1);
				Tcl_DStringAppend (&resulting, data.data, -1);
			}
			else
			{
				/*
				 * we have to convert the string into a tcl/list before
				 * adding it as an element
				 */
				char * p = SN_convert_separator_to_blank(data.data, strlen(data.data));
				Tcl_DStringAppendElement (&resulting, p);
			}
		}

		/*
		 * Get all columns, no columns are specified
		 */
		if (!col_num)
		{
			/*
			 * we must first convert the separator
			 */
			char * p = SN_convert_separator_to_blank (Tcl_DStringValue(&resulting),
							Tcl_DStringLength (&resulting));

			if (!result_filter || Tcl_StringMatch (Tcl_DStringValue(&resulting), result_filter))
			{
				LOGGER2 ((LOGFP,"fetched <%s>\n", p));
				if (get_all)
				{
					Tcl_Obj *objp = Tcl_NewStringObj (p, strlen (p));
					Tcl_ListObjAppendElement (interp, res_obj, objp);
				}
				else
				{
					Tcl_AppendToObj (res_obj, p, strlen (p));
				}
			}
			
			counter++;
			if (only_first && counter > 0)
				break;
			
			continue;
		}

		if (my_SplitList (Tcl_DStringValue(&resulting), &fld_num, &fields, DB_FLDSEP_CHR) != TCL_OK)
		{
			continue;
		}

		if (fld_num == 0)
		{
			ckfree ((char *)fields);
			fields = NULL;
			continue;
		}

		if (col_num != 0 && col_num < fld_num)
			max_num = col_num;
		else
			max_num = fld_num;

		for (Tcl_DStringFree(&record), cou = 0; !skip_record && cou < max_num; cou++)
		{
			target_field = columns[cou];
			if (target_field >= fld_num)
				continue;

			if (col_num)
			{
				char    *f;

				f = formats[target_field];
				if (f)
				{
					char    *p, *rest;
					char    tmp[100];
					unsigned long bits = 0;
					unsigned long low = 0;
					unsigned long bitv = 0;
					int	qry_val = 0;
					int	field_val = 0;
					int	max_val = 0;

					switch (*f)
					{
					case ' ':
						if (*(f + 1) == '\0')
						{
							Tcl_DStringAppendElement (&record, fields[target_field]);
							break;
						}
					default:
						if (*f)
						{
							Tcl_DStringAppend (&record, fields[target_field], -1);
							Tcl_DStringAppend (&record, f, -1);
						}
						else
						{
							Tcl_DStringAppendElement (&record, fields[target_field]);
						}
						break;

					case '|':
						Tcl_DStringAppendElement(&record, fields[target_field]);
						if (*(++f))
						{
							Tcl_DStringAppend (&record, f, -1);
						}
						break;

					case '\0':
						Tcl_DStringAppendElement (&record, fields[target_field]);
						break;

					case '#':	/* Return: filename directory ! */
						p = file_lastroot (fields[target_field]);
						if (f[1] == 0)
						{
							rest = DB_FLDSEP_STR;
						}
						else
						{
							rest = f+1;
						}
						/*
						 * It must produce something like
						 * like: "...file|dir|...."
						 * or  : "...file?dir?...."
						 */
						if (p)
						{
							int rooti = p - fields[target_field];

							/*
							 * filename
							 */
							Tcl_DStringAppend (&record, p+1, -1);
							/*
							 * second string
							 */
							Tcl_DStringAppend (&record, rest, -1);
							/*
							 * directory name
							 */

#if _WINDOWS /* preserve '/' if file is in root of a drive ( e.g 'F:/' instead of 'F:' )*/
							if (( rooti == 2 ) && (p[-1]==':'))
								rooti++;
#endif /* _WINDOWS */
							Tcl_DStringAppend (&record,
												fields[target_field],
												rooti);
						}
						else
						{
							Tcl_DStringAppend (&record, fields[target_field],-1);
							Tcl_DStringAppend (&record, rest, -1); /* add second string again */
						}
						if (cou < max_num-1)
						{
							Tcl_DStringAppend (&record, rest, 1);
						}
						break;
						
					case '/':          /* Info after the last '/'. */
						p = strrchr(fields[target_field],'/');
						if (p)
							p++;
						else
							p = fields[target_field];
						Tcl_DStringAppend(&record,p,-1);
						Tcl_DStringAppend(&record,f + 1,-1);
						break;

					case '%':
						sprintf(format_buf,f,
							fields[target_field]);
						Tcl_DStringAppend(&record,
							format_buf,-1);
						break;

					case '&':     /* Justify ! */
						strcpy(tmp,f);
						tmp[0] = '%'; /* Overwrites '&' */
						sprintf(format_buf,tmp,
							Tcl_DStringValue(&record));
						Tcl_DStringFree(&record);

						Tcl_DStringAppend(&record,format_buf,-1);
						Tcl_DStringAppend(&record,
							fields[target_field],-1);
						break;

					case ':':	/* Bit operations. */
						sscanf(f + 1,"%lu %*c %lu",&bits,&low);
						f = &fields[target_field][0];
						bitv = strtoul(&fields[target_field][0],NULL,0);
#if 0
LOGGER((LOGFP,"BITS: (%X %X=%X) (%X %X=%X) <%s>\n",bitv,low,bitv & low,
			bitv,bits,bitv & bits,fields[target_field]));
#endif
						if ((bitv & low) && (bitv & bits) == bits)
						{
							Tcl_DStringAppendElement(&record,
								fields[target_field]);
						}
						else
						{
							/* Skip record ! */
							Tcl_DStringFree(&record);
							skip_record = TRUE;
						}
						break;

					case '-':	/* In-range operation. */
						sscanf(f + 1,"%d %*c %d",&qry_val,&max_val);
						sscanf(fields[target_field],"%d",&field_val);

						if (field_val >= qry_val && field_val <= max_val)
						{
							Tcl_DStringAppendElement(&record,
								fields[target_field]);
						}
						else
						{
							/* Skip record ! */
							Tcl_DStringFree(&record);
							skip_record = TRUE;
						}
						break;

					case '=':			/* Such as: =567 */
						if (strcmp(f + 1,fields[target_field]) == 0)
						{
							Tcl_DStringAppendElement(&record,fields[target_field]);
						}
						else
						{
							Tcl_DStringFree(&record);
							skip_record = TRUE;
						}
						break;
						
					case '+': /* just compare, don't add this field */
						if (strcmp(f + 1,fields[target_field]) == 0)
						{
							/* ignore record, just comparing */
						}
						else
						{
							Tcl_DStringFree(&record);
							skip_record = TRUE;
						}
						break;
						
					case '<':			/* Such as: >567 */
						qry_val = strtol(f,NULL,0);
						field_val = strtol(fields[target_field],NULL,0);
						if (qry_val > field_val)
						{
							Tcl_DStringAppendElement(&record,fields[target_field]);
						}
						else
						{
							Tcl_DStringFree(&record);
							skip_record = TRUE;
						}
						break;

					case '>': /* Such as: <567 */
						qry_val = strtol(f + 1,NULL,0);
						field_val = strtol(fields[target_field],NULL,0);
						if (qry_val < field_val)
						{
							Tcl_DStringAppendElement(&record,fields[target_field]);
						}
						else
						{
							Tcl_DStringFree(&record);
							skip_record = TRUE;
						}
						break;
					}
				}
				else
				{
					/*
					 * If only one item is to be returned, don't add the
					 * string as an element, we could get a result like
					 * "{{space foo}}" by fields that contain blanks
					 */
					if (max_num == 1)
					{
						Tcl_DStringAppend (&record, fields[target_field], -1);
					}
					else
					{
						Tcl_DStringAppendElement(&record,fields[target_field]);
					}
				}
			}
			else
			{
				Tcl_DStringAppendElement(&record,fields[target_field]);
			}
		}
		if (skip_record)
		{
			ckfree ((char *)fields);
			fields = NULL;
			continue;
		}

		if (!result_filter ||
			Tcl_StringMatch(Tcl_DStringValue(&record),result_filter))
		{
			if (get_all)
			{
				char    *rp;
				Tcl_Obj	*obj;
				char *p;

				p = SN_convert_separator_to_blank (Tcl_DStringValue(&record), Tcl_DStringLength(&record));
				if (format_str)
				{
					sprintf(format_buf, format_str, p);
					rp = format_buf;
					obj = Tcl_NewStringObj(format_buf,-1);
				}
				else
				{
					rp = p;
					obj = Tcl_NewStringObj(p, strlen(p));
				}

				if (uniq)
				{
					int len = strlen(rp);
					if (len == last_len && memcmp(last_returned_record,rp,len) == 0)
					{
						ckfree ((char *)fields);
						fields = NULL;
						continue;
					}
					last_len = len;
					if (last_returned_bufsize < last_len + 1)
					{
						last_returned_bufsize = last_len + 1;
						last_returned_record = ckrealloc(last_returned_record,
							last_returned_bufsize);
					}
					memcpy(last_returned_record,rp,last_len + 1);
				}
				Tcl_ListObjAppendElement(interp, res_obj, obj);
			}
			else
			{
				char *p = SN_convert_separator_to_blank (Tcl_DStringValue (&record), Tcl_DStringLength(&record));
				Tcl_AppendToObj(res_obj, p, strlen(p));
			}
		}

		ckfree ((char *)fields);
		fields = NULL;
		
		counter++;
		if (only_first && counter > 0)
			break;
			
	} while(ret == 0 && get_all);

	LOGGER((LOGFP,"%ld records have been fetched and %ld scanned\n",
		counter,scan_counter));

	if (ret == -1)
	{
		Tcl_ResetResult(interp);

		Tcl_AppendResult (interp, argv[0], " ", argv[1], " error:", strerror(errno),NULL);
	}

	ret = TCL_OK;
	goto ok;

error:
	Tcl_PosixError(interp);

	ret = TCL_ERROR;
ok:

	/* Set object to result */
	if (res_obj)
		Tcl_SetObjResult (interp, res_obj);
	
	if (last_returned_record)
		ckfree(last_returned_record);

	Tcl_DStringFree (&resulting);
	Tcl_DStringFree (&record);

	if (fields)
		ckfree ((char *)fields);

	if (sub_list)
		ckfree((char *)sub_list);

	if (cross_format_buf)
		ckfree(cross_format_buf);

	if (col_num)
	{
		char    **f;

		for (cou = 0; cou < col_num; cou++)
		{
			f = &formats[columns[cou]];
			if (*f)
			{
				ckfree(*f);
				*f = NULL;
			}
		}
	}

	db_action_is_fetching = 0;

	return ret;
}

static  int
dbtclsync(Tcl_Interp *interp,tcldbpars *pars,int flag)
{
	DB      *dbm;

	LOGGER((LOGFP,"dbsync %s\n",
		pars->filename ? pars->filename : "memory"));

	dbm = pars->dbp;
	if (dbm->sync(dbm,flag) == -1)
	{
		LOGGER((LOGFP,"dbsync error: %s\n",strerror(errno)));
		Tcl_AppendResult (interp, " sync error:", strerror(errno), NULL);

		return -1;
	}

	return TCL_OK;
}

static  int
dbtclclose(Tcl_Interp *interp,tcldbpars *pars,char *cmd)
{
	if (Tcl_DeleteCommand(interp,cmd) != TCL_OK)
	{
		Tcl_AppendResult(interp," close error:", strerror(errno),NULL);

		Tcl_PosixError(interp);

		return TCL_ERROR;
	}

	return TCL_OK;
}

/*
 * Usage: obj del -glob|-begin|-end|-strstr|-regexp pattern? ?keys? ?flag?
 */
static int
dbtcldel(Tcl_Interp *interp,tcldbpars *pars,int argc,char **argv)
{
	DB      *dbp = pars->dbp;
	DBT key;
	DBT data;
	char    *tcl_cmd = argv[0];
	int     ret;
	int     flag = 0;
	char    *glob_pattern = NULL;
	int     pattern_size = 0;
	char    *key_pattern = NULL;
	int     all = FALSE;
	unsigned int    key_pattern_size = 0;
	long    counter = 0;
	int     match_type = DB_MATCH_NONE;
	Tcl_RegExp      regexp_ptr = 0;

	if (argc < 2 || argc > 6)
	{
		Tcl_AppendResult(interp, "wrong # args:  should be \"",
			tcl_cmd, " del ?-all pattern key flag?",        NULL);
	}

	argv += 2;
	argc -= 2;

	if (strcmp(*argv,"-glob") == 0)
	{
		argc--;
		argv++;
		if (argc > 0)
		{
			glob_pattern = *argv++;
			if (*glob_pattern == '\0' || strcmp(glob_pattern,"*") == 0)
				glob_pattern = NULL;
			else
			{
				match_type = DB_MATCH_GLOB;
			}
			argc--;
		}
		all = TRUE;
	}
	else if (strncmp(*argv,"-beg",3) == 0)
	{
		argc--;
		argv++;
		if (argc > 0)
		{
			glob_pattern = *argv++;
			if (*glob_pattern == '\0')
				glob_pattern = NULL;
			else
			{
				match_type = DB_MATCH_BEG;
				pattern_size = strlen(glob_pattern);
			}
			argc--;
		}
		all = TRUE;
	}
	else if (strncmp(*argv,"-end",3) == 0)
	{
		argc--;
		argv++;
		if (argc > 0)
		{
			glob_pattern = *argv++;
			if (*glob_pattern == '\0')
				glob_pattern = NULL;
			else
			{
				match_type = DB_MATCH_END;
			}
			argc--;
		}
		all = TRUE;
	}
	else if (strncmp(*argv,"-regexp",3) == 0)
	{
		argc--;
		argv++;
		if (argc > 0)
		{
			glob_pattern = *argv++;
			if (*glob_pattern == '\0')
				glob_pattern = NULL;
			else
			{
				match_type = DB_MATCH_REGEXP;
				regexp_ptr = Tcl_RegExpCompile(interp,glob_pattern);
				if (!regexp_ptr)
				{
					return -1;
				}
			}
			argc--;
		}
		all = TRUE;
	}
	else if (strncmp(*argv,"-strstr",3) == 0)
	{
		argc--;
		argv++;
		if (argc > 0)
		{
			glob_pattern = *argv++;
			if (*glob_pattern == '\0')
				glob_pattern = NULL;
			else
			{
				match_type = DB_MATCH_OCCURE;
			}
			argc--;
		}
		all = TRUE;
	}
	if (argc > 0)
	{
		argc--;
		key_pattern = *argv++;
		if (*key_pattern == '\0')
		{
			key_pattern = NULL;
			key_pattern_size = 0;
		}
		else
			key_pattern_size = strlen(key_pattern);
			
		key.data = key_pattern;
		key.size = key_pattern_size;
		key_pattern_size = key.size;
		if (!all && key_pattern)
			key.size++;
	}

	if (argc > 0)
	{
		argc--;
		flag = setflags(*argv++);
	}
	else
		flag = 0;

	if (!key_pattern)
	{
		if (flag != R_CURSOR)           /* If no key is given with the R_CURSOR */
			all = TRUE;                                     /* flag we can delete the current record. */
		key.data = NULL;
		key.size = 0;
		key_pattern_size = 0;
	}

	if (all)
		flag = key_pattern ? R_CURSOR : R_FIRST;

	LOGGER((LOGFP,"dbdel %s all: %d key: <%s> size: %d expression: <%s> match type: %d flag: %s\n",
		pars->filename ? pars->filename : "memory",
		all,
		key.data ? (char *)key.data : "",
		key.size,
		glob_pattern ? glob_pattern : "",
		match_type,
		show_flag(flag)));

	counter = 0;
	if (all)
	{
		long    scan_counter;
		long    rec_counter;
		char    *keyp;
		char    *last_space;

		for (rec_counter = 0, scan_counter = 0;
			(ret = dbp->seq(dbp,&key,&data,flag)) == 0;
			flag = R_NEXT, rec_counter++)
		{
			scan_counter++;
			if ((key_pattern_size && key.size < key_pattern_size) ||
				memcmp(key_pattern,key.data,key_pattern_size) != 0)
			{
				break;
			}
			LOGGER2((LOGFP,"rec: %d,%d,<%s>,<%s>\n",
				key.size,data.size,(char *)key.data,
				data.size > 0 ? (char *)data.data : ""));

			keyp = key.data;

			switch (match_type)
			{
			case DB_MATCH_END:
				if (!(last_space = strrchr (keyp, DB_FLDSEP_CHR)))
					continue;

				if (strcmp(++last_space,glob_pattern) != 0)
					continue;
				break;

			case DB_MATCH_NONE:
				break;

			case DB_MATCH_REGEXP:
				switch (Tcl_RegExpExec(interp,regexp_ptr,keyp,keyp))
				{
				case -1:
					return -1;              /* Error */
					break;

				case 0:                 /* Not found ! */
					continue;
					break;
				}
				break;

			case DB_MATCH_OCCURE:
				if (!strstr(keyp,glob_pattern))
					continue;
				break;

			case DB_MATCH_BEG:
				if (strncmp(keyp,glob_pattern,pattern_size) != 0)
					continue;
				break;

			case DB_MATCH_GLOB:
				if (!Tcl_StringMatch(keyp,glob_pattern))
					continue;
				break;
			}

			if ((ret = dbp->del(dbp,&key,R_CURSOR)) == -1)
			{
				break;
			}
			if (ret != 0)
			{
				break;          /* Should never happen. */
			}

			counter++;
		}
		LOGGER((LOGFP,"%ld records have been scanned from %ld\n",
			scan_counter,rec_counter));
	}
	else
	{
		ret = dbp->del(dbp,&key,flag);
		if (ret == 0)
			counter++;
	}
	if (ret == -1)
	{
		LOGGER((LOGFP,"dbdel error: %s\n",strerror(errno)));

		Tcl_AppendResult(interp, tcl_cmd,
			" delete error:", strerror(errno), NULL);

		return -1;
	}
	LOGGER((LOGFP,"%ld records have been deleted\n",counter));

	return counter;
}

static  int
dbexclude(Tcl_Interp *interp,tcldbpars *pars,char *ex_cmd)
{
	Tcl_CmdInfo infoPtr;
	tcldbpars       *sub_pars;

	pars->db_exclude = NULL;
	if (!ex_cmd || *ex_cmd == '\0')
	{
		return TCL_OK;
	}

	infoPtr.objClientData = NULL;
	if (Tcl_GetCommandInfo(interp,ex_cmd,&infoPtr))
	{
		sub_pars = (tcldbpars *)infoPtr.objClientData;
		pars->db_exclude = sub_pars->dbp;

		LOGGER((LOGFP,"dbexclude: <%s> DB: %lx\n",
			ex_cmd,
			(unsigned long)pars->db_exclude));
	}
	else
	{
		LOGGER((LOGFP,"dbexclude command \"%s\" not found\n",ex_cmd));

		Tcl_AppendResult(interp,"unknown db object \"", ex_cmd,"\"",
			(char *) NULL);

		return TCL_ERROR;
	}
	return TCL_OK;
}

static  void
setdb_mtime(tcldbpars *pars)
{
	struct  MX_STAT stb;

	if (pars->filename && MX_STAT(pars->filename,&stb) == 0)
		pars->mtime = stb.st_mtime;
}

static  int
dbreopen(Tcl_Interp *interp,tcldbpars *pars,char **argv)
{
	int     mode;

	if (pars->dbp->close(pars->dbp) != 0)
	{
		Tcl_AppendResult(interp, argv[0],
			" dbclose error: ", strerror(errno), (char *) NULL);

		Tcl_PosixError(interp);

		return TCL_ERROR;
	}

	mode = pars->mode & ~ (O_TRUNC);

	LOGGER((LOGFP,"dbreopen %s mode: 0x%x protection: 0x%x, type: %d flags: %ld\n",
		pars->filename ? pars->filename : "memory",
		mode,pars->prot,pars->dbtype,
		pars->info.br.flags));

	if (!(pars->dbp = dbopen (pars->filename, mode,
		pars->prot, pars->dbtype, &pars->info)))
	{
		Tcl_AppendResult(interp, argv[0],
			" error: ", pars->filename,":",
			strerror(errno), (char *) NULL);

		Tcl_PosixError(interp);

		return TCL_ERROR;
	}

	setdb_mtime(pars);

	return TCL_OK;
}

/*
 * Database defined command (paf_db_*)
 */
static int
dbDispCmd(ClientData clientData,Tcl_Interp *interp,int argc,Tcl_Obj *CONST objv[])
{
	tcldbpars *pars = (tcldbpars *)clientData;
	int     flag = 0;
	int     ret;
	DBT data, key;
	char    tmp[200];
	struct  MX_STAT stb;
#define	DISP_ARGS_NUM	100
	char	*argv[DISP_ARGS_NUM];

	if (argc >= DISP_ARGS_NUM)
	{
		Tcl_AppendResult(interp, Tcl_GetStringFromObj(objv[0],(int *) NULL),
			" # too many arguments",NULL);

		return TCL_ERROR;
	}

	for (ret = 0; ret < argc; ret++)
	{
		argv[ret] = Tcl_GetStringFromObj(objv[ret],(int *) NULL);
	}

	if (strcmp(argv[1],"close") == 0)
	{
		return dbtclclose(interp,pars,argv[0]);
	}

	if (strcmp(argv[1],"reopen") == 0)
	{
		return dbreopen(interp,pars,argv);
	}

	if (strcmp(argv[1],"get") == 0 || strcmp(argv[1],"seq") == 0 ||
		strcmp(argv[1],"all") == 0)
	{
		/* Before we fetch the database we check whether it has been changed
		 * by an other process.
		 */
		if (pars->filename && MX_STAT(pars->filename,&stb) == 0 &&
			pars->mtime != stb.st_mtime)
		{
			LOGGER((LOGFP,"%s fetch forced reopen: %lu, %lu\n",
				pars->filename, pars->mtime, stb.st_mtime));

			if (dbreopen(interp,pars,argv) != TCL_OK)
				return TCL_ERROR;
		}
		return dbtclfetch(interp,pars,argc,argv);
	}

	if (strncmp(argv[1],"del",3) == 0)
	{
		ret = dbtcldel(interp,pars,argc,argv);
		if (ret == -1)
		{
			Tcl_PosixError(interp);

			return TCL_ERROR;
		}

		sprintf(tmp,"%d",ret);
		Tcl_AppendResult(interp, tmp, NULL);
		
		setdb_mtime(pars);

		return TCL_OK;
	}
	
	if (strcmp(argv[1],"put") == 0)
	{
		if (argc < 3 || argc > 5)
		{
			Tcl_AppendResult(interp, "wrong # args:  should be \"",
				argv[0], " put key ?data? ?flag?", NULL);

			return TCL_ERROR;
		}
		key.data = argv[2];
		key.size = strlen(key.data) + 1;
		if (key.size == 1)
		{
			Tcl_AppendResult(interp, "wrong # args:  should be \"",
				argv[0], " put key ?data? ?flag?", NULL);

			return TCL_ERROR;
		}

		if (argc > 3)
		{
			data.data = argv[3];
			data.size = strlen(data.data) + 1;
		}
		else
		{
			data.data = tmp;
			tmp[0] = '\0';
			data.size = 1;
		}
		if (argc > 4)
		{
			flag = setflags(argv[4]);
		}
		LOGGER2((LOGFP,"dbput %s key: <%s> flag: %s data: <%s>\n",
			pars->filename ? pars->filename : "memory",
			(char *)key.data, show_flag(flag), (char *)data.data));

		ret = pars->dbp->put(pars->dbp,&key,&data,flag);
		if (ret == -1)
		{
			LOGGER((LOGFP,"dbput error: %s\n",strerror(errno)));
			Tcl_AppendResult(interp, argv[0],
				" put error:", strerror(errno), NULL);

			Tcl_PosixError(interp);

			return TCL_ERROR;
		}

		sprintf(tmp,"%d",ret);
		Tcl_AppendResult(interp, tmp, NULL);

		setdb_mtime(pars);

		return TCL_OK;
	}

	if (strcmp(argv[1],"sync") == 0)
	{
		if (argc < 2 || argc > 3)
		{
			Tcl_AppendResult(interp, "wrong # args:  should be \"",
				argv[0], " sync ?flag?",NULL);

			return TCL_ERROR;
		}

		if (argc == 3)
		{
			flag = setflags(argv[3]);
		}
		if ((ret = dbtclsync(interp,pars,flag)) == -1)
		{
			Tcl_PosixError(interp);

			return TCL_ERROR;
		}

		sprintf(tmp,"%d",ret);
		Tcl_AppendResult(interp,tmp, NULL);
		
		setdb_mtime(pars);

		return TCL_OK;
	}
	if (strncmp(argv[1],"isempty",3) == 0)
	{
		/* Before we fetch the database we check whether it has been changed
		 * by an other process.
		 */
		if (pars->filename && MX_STAT(pars->filename,&stb) == 0 &&
			pars->mtime != stb.st_mtime)
		{
			LOGGER((LOGFP,"%s isempty forced reopen: %lu, %lu\n",
				pars->filename, pars->mtime, stb.st_mtime));

			if (dbreopen(interp,pars,argv) != TCL_OK)
				return TCL_ERROR;
		}
		if ((ret = dbisempty(pars)) == -1)
		{
			Tcl_PosixError(interp);

			return TCL_ERROR;
		}
		sprintf(tmp,"%d",ret);
		Tcl_AppendResult(interp,tmp, NULL);

		return TCL_OK;
	}
	if (strncmp(argv[1],"exclude",3) == 0)
	{
		if (argc < 2 || argc > 3)
		{
			Tcl_AppendResult(interp, "wrong # args:  should be \"",
				argv[0], " exclude ?db_object?", NULL);

			return TCL_ERROR;
		}

		if (dbexclude(interp,pars,argc == 3 ? argv[2]: NULL) == TCL_ERROR)
			return TCL_ERROR;

		return TCL_OK;
	}
	Tcl_AppendResult(interp, argv[0],
		" only close,del,exclude,get,isempty,put,reopen,seq and sync can be used",
		NULL);

	return TCL_ERROR;
}

static int
dbtcldestroy(ClientData clientData)
{
	tcldbpars       *pars = (tcldbpars *)clientData;

	LOGGER2((LOGFP,"dbclose %s; deleting command: %s\n",
		pars->filename ? pars->filename : "memory",pars->tclcmd));

	if (pars->dbp->close(pars->dbp) == -1)
	{
		LOGGER((LOGFP,"dbclose error: %s\n",strerror(errno)));

		return TCL_ERROR;
	}

	ckfree (pars->tclcmd);

	if (pars->filename)
		ckfree (pars->filename);

	ckfree ((char *)pars);

	return TCL_OK;
}

static  void
setinfo(DBTYPE type,char *s,void *infop)
{
	BTREEINFO *ib;
	HASHINFO *ih;
	RECNOINFO *rh;
	char *eq;

	if ((eq = strchr(s, '=')) == NULL)
		return;

	*eq++ = '\0';
	if (!isdigit(*eq))
		return;
		
	switch (type) {
	case DB_BTREE:
		ib = (BTREEINFO *)infop;
		if (!strcmp("flags", s)) {
			ib->flags = atoi(eq);
			return;
		}
		if (!strcmp("cachesize", s)) {
			ib->cachesize = atoi(eq);
			return;
		}
		if (!strcmp("maxkeypage", s)) {
			ib->maxkeypage = atoi(eq);
			return;
		}
		if (!strcmp("minkeypage", s)) {
			ib->minkeypage = atoi(eq);
			return;
		}
		if (!strcmp("lorder", s)) {
			ib->lorder = atoi(eq);
			return;
		}
		if (!strcmp("psize", s)) {
			ib->psize = atoi(eq);
			return;
		}
#if 0
		if (!strcmp("nocase", s)) {
			if (atoi(eq))
				ib->compare = db_no_case_compare;
			return;
		}
#endif
		break;
	case DB_HASH:
		ih = (HASHINFO *)infop;
		if (!strcmp("bsize", s)) {
			ih->bsize = atoi(eq);
			return;
		}
		if (!strcmp("ffactor", s)) {
			ih->ffactor = atoi(eq);
			return;
		}
		if (!strcmp("nelem", s)) {
			ih->nelem = atoi(eq);
			return;
		}
		if (!strcmp("cachesize", s)) {
			ih->cachesize = atoi(eq);
			return;
		}
		if (!strcmp("lorder", s)) {
			ih->lorder = atoi(eq);
			return;
		}
		break;
	case DB_RECNO:
		rh = (RECNOINFO *)infop;
		if (!strcmp("flags", s)) {
			rh->flags = atoi(eq);
			return;
		}
		if (!strcmp("cachesize", s)) {
			rh->cachesize = atoi(eq);
			return;
		}
		if (!strcmp("lorder", s)) {
			rh->lorder = atoi(eq);
			return;
		}
		if (!strcmp("reclen", s)) {
			rh->reclen = atoi(eq);
			return;
		}
		if (!strcmp("bval", s)) {
			rh->bval = atoi(eq);
			return;
		}
		if (!strcmp("psize", s)) {
			rh->psize = atoi(eq);
			return;
		}
		break;
	}
}

static  DBTYPE
dbtype(char *s)
{
	if (!strcmp(s, "btree"))
		return (DB_BTREE);
	if (!strcmp(s, "hash"))
		return (DB_HASH);
	if (!strcmp(s, "recno"))
		return (DB_RECNO);

	return DB_BTREE;
}

/*
 *----------------------------------------------------------------------
 *
 * GetOpenMode --
 *
 *      description.
 *
 * Results:
 *      Normally, sets *modePtr to an access mode for passing to "open",
 *      and returns a string that can be used as the access mode in a
 *      subsequent call to "fdopen".  If an error occurs, then returns
 *      NULL and sets interp->result to an error message.
 *
 * Side effects:
 *      None.
 *
 * Special note:
 *      This code is based on a prototype implementation contributed
 *      by Mark Diekhans.
 *
 *----------------------------------------------------------------------
 */

static char *
GetOpenMode(Tcl_Interp *interp,char *string,int *modePtr)
{
    int mode, modeArgc, c, i, gotRW;
    char **modeArgv, *flag;
#define RW_MODES (O_RDONLY|O_WRONLY|O_RDWR)

    /*
     * Check for the simpler fopen-like access modes (e.g. "r").  They
     * are distinguished from the POSIX access modes by the presence
     * of a lower-case first letter.
     */

    mode = 0;
    if (islower(string[0])) {
		switch (string[0]) {
	    	case 'r':
				mode = O_RDONLY;
				break;
	    	case 'w':
				mode = O_RDWR|O_CREAT|O_TRUNC;
				break;
		    case 'a':
				mode = O_RDWR|O_CREAT|O_APPEND;
				break;
		    default:
			error:
				Tcl_AppendResult(interp,
				"illegal access mode \"", string, "\"", (char *) NULL);
				return NULL;
		}
		if (string[1] == '+') {
		    mode &= ~(O_RDONLY|O_WRONLY);
	    	mode |= O_RDWR;
		    if (string[2] != 0) {
				goto error;
	    	}
		} else if (string[1] != 0) {
		    goto error;
		}
		*modePtr = mode;
		return string;
    }

    /*
     * The access modes are specified using a list of POSIX modes
     * such as O_CREAT.
     */

    if (Tcl_SplitList(interp, string, &modeArgc, &modeArgv) != TCL_OK) {
		Tcl_AddErrorInfo(interp, "\n    while processing open access modes \"");
		Tcl_AddErrorInfo(interp, string);
		Tcl_AddErrorInfo(interp, "\"");
		return NULL;
    }
    gotRW = 0;
    for (i = 0; i < modeArgc; i++) {
		flag = modeArgv[i];
		c = flag[0];
		if (islower(c))
			c = toupper(c);
		if ((c == 'R') && (strcmp(flag, "RDONLY") == 0)) {
		    mode = (mode & ~RW_MODES) | O_RDONLY;
		    gotRW = 1;
		} else if ((c == 'W') && (strcmp(flag, "WRONLY") == 0)) {
		    mode = (mode & ~RW_MODES) | O_RDWR;
		    gotRW = 1;
		} else if ((c == 'R') && (strcmp(flag, "RDWR") == 0)) {
		    mode = (mode & ~RW_MODES) | O_RDWR;
		    gotRW = 1;
		} else if ((c == 'A') && (strcmp(flag, "APPEND") == 0)) {
		    mode |= O_APPEND;
		} else if ((c == 'C') && (strcmp(flag, "CREAT") == 0)) {
		    mode |= O_CREAT;
		} else if ((c == 'E') && (strcmp(flag, "EXCL") == 0)) {
		    mode |= O_EXCL;
		} else if ((c == 'N') && (strcmp(flag, "NOCTTY") == 0)) {
#ifdef O_NOCTTY
		    mode |= O_NOCTTY;
#else
		    Tcl_AppendResult(interp, "access mode \"", flag,
			    "\" not supported by this system", (char *) NULL);
		    ckfree((char *) modeArgv);
		    return NULL;
#endif
		} else if ((c == 'N') && (strcmp(flag, "NONBLOCK") == 0)) {
#ifdef O_NONBLOCK
		    mode |= O_NONBLOCK;
#else
	    	mode |= O_NDELAY;
#endif
		} else if ((c == 'T') && (strcmp(flag, "TRUNC") == 0)) {
		    mode |= O_TRUNC;
		} else {
		    Tcl_AppendResult(interp, "invalid access mode \"", flag,
			    "\": must be RDONLY, WRONLY, RDWR, APPEND, CREAT",
		    	" EXCL, NOCTTY, NONBLOCK, or TRUNC", (char *) NULL);
		    ckfree((char *) modeArgv);
		    return NULL;
		}
    }
    ckfree((char *) modeArgv);
	if (!gotRW) {
		Tcl_AppendResult(interp, "access mode must include either",
			" RDONLY, WRONLY, or RDWR", (char *) NULL);
		return NULL;
	}
	*modePtr = mode;

    /*
     * The calculation of fdopen access mode below isn't really correct,
     * but it doesn't have to be.  All it has to do is to disinguish
     * read and write permissions, plus indicate append mode.
     */

    i = mode & RW_MODES;
    if (i == O_RDONLY) {
		return "r";
    }
    if (mode & O_APPEND) {
		if (i == O_WRONLY) {
		    return "a";
		} else {
		    return "a+";
		}
    }
    if (i == O_WRONLY) {
		return "w";
    }
    return "r+";
}

/*
 *	We have to conver '\\' to '/'. Thats all.
 */
static	char	*
mxtempnam(char *dir,char *pref)
{
	char *nm, *cknm;

	if (dir && *dir == '\0')
		dir = NULL;
	nm = tempnam(dir,pref);
	if (nm)
	{
		unlink(nm);		/* Just to be sure. */
	}
#if _WINDOWS
	if (nm)
	{
		sn_internal_convert_path (nm, SN_PATH_UNIX);
	}
#endif /* _WINDOWS */

	/* use ckalloc to dup it, to be able to use ckfree */
	cknm = SN_StrDup (nm);
	
	/* don't use ckfree */
	free(nm);
	
	return cknm;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_TempNam --
 *     Implements the TCL tempnam command:
 *         tempnam dir pfx
 *
 * Results:
 *  Standard TCL results, may return the UNIX system error message.
 *
 *-----------------------------------------------------------------------------
 */
static int
Tcl_TempNam(ClientData clientData,Tcl_Interp *interp,int argc,char **argv)
{
    char        *dir;
    char	*ret;
    Tcl_DString  dirbuf;
    Tcl_DString  nativeName;
    Tcl_DString  prefixName;
    Tcl_DString  resultName;

    if (argc != 3) {
        Tcl_AppendResult (interp, "wrong # args: ", argv [0], 
                          " dir pfx", (char *) NULL);
        return TCL_ERROR;
    }

    Tcl_DStringInit (&dirbuf);

    if (argv[1] == NULL || *argv == '\0')
    	dir = NULL;
    else
    {
	if ((dir = Tcl_TranslateFileName (interp, argv [1], &dirbuf)) == NULL)
		return TCL_ERROR;
    }

    Tcl_UtfToExternalDString(NULL, dir, -1, &nativeName);
    Tcl_UtfToExternalDString(NULL, argv[2], -1, &prefixName);
    if ((ret = mxtempnam(Tcl_DStringValue(&nativeName),Tcl_DStringValue(&prefixName))) == NULL)
    {
	interp->result = Tcl_PosixError (interp);
	Tcl_DStringFree(&nativeName);
	Tcl_DStringFree(&prefixName);
	return TCL_ERROR;
    }
    Tcl_DStringFree (&dirbuf);
    Tcl_DStringFree (&nativeName);
    Tcl_DStringFree (&prefixName);

    Tcl_ExternalToUtfDString(NULL, ret, -1, &resultName);
    ckfree(ret);
    Tcl_SetResult(interp,Tcl_DStringValue(&resultName),TCL_VOLATILE);
    Tcl_DStringFree (&resultName);
    
    return TCL_OK;
}

DB_DLL_EXPORT int Paftcldb_Deinit(Tcl_Interp *interp)
{
	if (!interp)
		return TCL_OK;

	Tcl_DeleteCommand(interp,"dbopen");
	Tcl_DeleteCommand(interp,"tempnam");

	return TCL_OK;
}

DB_DLL_EXPORT int _Paftcldb_Init(Tcl_Interp *interp)
{
#if DB_DLL
	char	tcl_version[10];
	char	*var;
#include <time.h>

	struct tm	*curr_t;
	time_t	t;
	time(&t);

	curr_t = localtime(&t);

	if (curr_t->tm_year <= 1900)
			curr_t->tm_year += 1900;

	if (curr_t->tm_year > 1996 || curr_t->tm_mon > 9)
	{
		char	*msg = "Please contact Cygnus at http://support.cygnus.com";

		Tcl_AppendResult(interp,msg,NULL);

		return TCL_ERROR;
	}
#endif /* DB_DLL */
	if (!interp)
		return TCL_ERROR;

#if DB_DLL
	if (!paf_load_tcl_lib_cmd())
	{
		return TCL_ERROR;
	}

	sprintf(tcl_version,"%d.%d",TCL_MAJOR_VERSION,TCL_MINOR_VERSION);
	var = Tcl_GetVar(interp,"tcl_version",TCL_GLOBAL_ONLY);
	if (!var || strcmp(var,tcl_version) != 0)
		return TCL_ERROR;
#endif /* DB_DLL */

	Tcl_CreateCommand(interp, "dbopen",
		(Tcl_CmdProc *)DB_TCL_OPEN,
		(ClientData)NULL, (Tcl_CmdDeleteProc *) NULL);

	Tcl_CreateCommand(interp, "tempnam",
		(Tcl_CmdProc *)Tcl_TempNam,
		(ClientData)NULL, (Tcl_CmdDeleteProc *) NULL);

	Tcl_CreateCommand(interp, "sn_put_project_dir_files",
		(Tcl_CmdProc *)sn_put_project_dir_files,
		(ClientData)NULL, (Tcl_CmdDeleteProc *) NULL);

	return TCL_OK;
}

/*
 * Command syntax: set db [dbopen new_cmd file mode prot type ?info filedb?]
 */
DB_DLL_EXPORT int
DB_TCL_OPEN(ClientData clientData,Tcl_Interp *interp,int argc,char **argv)
{
	int         ret;
	tcldbpars       *pars = NULL;
	char    *p;
	char    *argv0 = *argv++;
	char    *cmd = NULL;
	char    *info_flags=NULL;

	if (argc < 6 || argc > 11)
	{
		Tcl_AppendResult(interp, "wrong # args:  should be \"",
			argv0, " new_command file mode protection type ?openinfo?\"",
			(char *) NULL);

		return TCL_ERROR;
	}

	if ((pars = (tcldbpars *)ckalloc(sizeof(tcldbpars)))== NULL)
	{
		goto error;
	}
	memset((char *)pars,0,sizeof(tcldbpars));

	pars->mode = O_RDONLY;
	pars->prot = 0666;

	cmd = *argv++;
	argc--;

	if (**argv == '\0' || strcmp(*argv,"NULL") == 0)
	{
		pars->filename = NULL;
	}
	else
	{
		char	realnm[MAXPATHLEN + 1];
		Tcl_DString  dbfile;
		Tcl_DString  nativeName;

		Tcl_DStringInit(&dbfile);
		if (!(p = Tcl_TranslateFileName (interp, *argv, &dbfile)))
		{
			Tcl_DStringFree (&dbfile);

			goto error;
		}

		/* It is important to use absolute pathnames, to ensure that
		 * the file can be reopen independtly of the working directory.
		 */
		Tcl_UtfToExternalDString(NULL, Tcl_DStringValue(&dbfile),
			Tcl_DStringLength(&dbfile), &nativeName);
		p = Tcl_DStringValue(&nativeName);
#ifdef WIN32
		if (win32_realpath(p,realnm))
#else
		if (realpath(p,realnm))
#endif /* WIN32 */
		{
			p = realnm;
		}
		ret = strlen(p) + 1;
		pars->filename = ckalloc(ret);
		
		memcpy(pars->filename,p,ret);

		Tcl_DStringFree (&nativeName);
		Tcl_DStringFree (&dbfile);
	}
	argv++;
	argc--;

	if (!GetOpenMode(interp, *argv++, &pars->mode))
	{
		goto error;
	}
	argc--;

	if (Tcl_GetInt(interp, *argv++, &pars->prot) != TCL_OK)
	{
		goto error;
	}
	argc--;

	pars->dbtype = dbtype(*argv++);
	argc--;

	if (pars->dbtype == DB_BTREE)
	{
#if _WINDOWS
		/*
		 * database for files "*.f" has to be lower/upper case
		 * consistent on windows
		 */
		ret = strlen(pars->filename);
		if (ret > 2 &&
			tolower (pars->filename[ret-1]) == 'f' &&
			pars->filename[ret-2] == '.')
		{
			pars->info.br.compare = db_no_case_compare;
		}
		else
		{
			pars->info.br.compare = db_case_compare;
		}
#else
		pars->info.br.compare = db_case_compare;
#endif
	}
	if (argc > 1)
	{
		argc--;
		info_flags = *argv++;
		LOGGER ((LOGFP, "dbopen string flags: <%s>\n", info_flags));
		for (p = strtok(info_flags, ", \t"); p != NULL; p = strtok(NULL, ", \t"))
		{
			LOGGER2((LOGFP,"dbopen flag: <%s>\n",p));
			setinfo(pars->dbtype, p, &pars->info);
		}
	}
	LOGGER((LOGFP,"dbopen %s mode: 0x%x protection: 0%o type: %d flags: %lu cmp: 0x%lx\n",
		pars->filename ? pars->filename : "memory",
		pars->mode,
		pars->prot,
		pars->dbtype,
		pars->info.br.flags,
		(unsigned long)pars->info.br.compare));

	/*
	 * open the database file
	 */
	pars->dbp = dbopen(pars->filename,
						pars->mode,
						pars->prot,
						pars->dbtype,
						info_flags ? &pars->info : NULL);
	
	/*
	 * if file couldn't be opened as a btree open it as a hash table
	 */
	if (!pars->dbp /*&& pars->dbtype == DB_BTREE*/ )
	{
		LOGGER ((LOGFP, "open <%s> as a %s db\n",
						pars->filename,
						pars->dbtype == DB_BTREE ? "btree" : "hash"));
		
		pars->dbtype = (pars->dbtype == DB_BTREE ? DB_HASH : DB_BTREE);
		/*
		 * reread the flags
		 */
		if (pars->dbtype == DB_BTREE)
		{
			memset ((char*)&pars->info, 0, sizeof (BTREEINFO));
		}
		else
		{
			memset ((char*)&pars->info, 0, sizeof (HASHINFO));
		}
		if (info_flags != NULL)
		{
			for (p = strtok(info_flags, ", \t"); p != NULL; p = strtok(NULL, ", \t"))
			{
				LOGGER2((LOGFP,"dbopen flag: <%s>\n",p));
				setinfo(pars->dbtype, p, &pars->info);
			}
		}
		pars->dbp = dbopen(pars->filename,
							pars->mode,
							pars->prot,
							pars->dbtype,
							info_flags ? &pars->info : NULL);
	}
	
	if (!pars->dbp)
	{
		LOGGER((LOGFP,"dbopen error: %s\n",strerror(errno)));

		Tcl_PosixError(interp);

		Tcl_AppendResult(interp, argv0,
			" error: ", pars->filename ? pars->filename : "",
			":", strerror(errno), (char *) NULL);

		goto error;
	}

	LOGGER2((LOGFP,"\"%s\" has been open, DB: %lx\n",
		pars->filename,
		(unsigned long)pars->dbp));

	if (argc > 1)
	{
		argc--;
		if (dbexclude(interp,pars,*argv++) == TCL_ERROR)
			goto error;
	}

	Tcl_CreateObjCommand(interp, cmd, (Tcl_ObjCmdProc *)dbDispCmd,
	    (ClientData)pars,(Tcl_CmdDeleteProc *)dbtcldestroy);

	ret = strlen(cmd) + 1;
	pars->tclcmd = ckalloc(ret);
	memcpy(pars->tclcmd,cmd,ret);

	LOGGER2((LOGFP,"command \"%s\" has been created\n",cmd));

	setdb_mtime(pars);

	Tcl_ResetResult(interp);

	Tcl_AppendResult(interp, cmd, NULL);

	return TCL_OK;

error:
	if (pars->filename)
		ckfree(pars->filename);

	if (pars)
		ckfree((char*)pars);

	return TCL_ERROR;
}
/*
 *-----------------------------------------------------------------------------
 *
 * sn_put_project_dir_files
 * 	writes the input list into a btree database that is used by
 *	parser to resolve the include directives.
 *
 * Results:
 *  Standard TCL results, may return the database system error message.
 *
 *-----------------------------------------------------------------------------
 */
static int
sn_put_project_dir_files(ClientData clientData,Tcl_Interp *interp,int argc,char **argv)
{ 
	char	*file_list;
	char	*filename;
	BTREEINFO	db_inf;
	DB	*db;
	DBT	key;
	DBT	data;
	int	cache_size;
	char	*fp;
	char	key_buf[MAXPATHLEN + 2];
	char	tmp[MAXPATHLEN + 1];
	FILE	*ffp;
	int	len;
	Tcl_DString	fn_buf;
	Tcl_DString	filename_buf;
	Tcl_DString	file_list_buf;

	Tcl_DStringInit(&fn_buf);

	if (argc != 4)
	{
		Tcl_AppendResult (interp, "wrong # args " , argv [0],
			" database cache_size filename",(char *) NULL);
		return TCL_ERROR;
	}

	filename = Tcl_TranslateFileName(interp,argv[1],&fn_buf);
	if (!filename)
		return TCL_ERROR;
	Tcl_UtfToExternalDString(NULL, filename, Tcl_DStringLength(&fn_buf), &filename_buf);
	filename = Tcl_DStringValue(&filename_buf);
	Tcl_DStringFree(&fn_buf);
	cache_size = atoi(argv[2]);
	Tcl_UtfToExternalDString(NULL, argv[3], -1, &file_list_buf);
	file_list = Tcl_DStringValue(&file_list_buf);

	memset((char *)&db_inf,0,sizeof(db_inf));
	db_inf.cachesize = (u_int)cache_size;
	db_inf.compare = db_case_compare;

	db = dbopen(filename, O_RDWR|O_CREAT|O_TRUNC, get_db_permission(), DB_BTREE,
		&db_inf);
	if (!db)
	{
		Tcl_AppendResult (interp,
			"dbopen error in ", argv[0],	": \"",
			strerror(errno), "\":", filename, (char *) NULL);

		Tcl_DStringFree (&filename_buf);
		Tcl_DStringFree (&file_list_buf);

		return TCL_ERROR;
	}

	ffp = fopen(file_list,"r");
	if (!ffp)
	{
		db->close(db);
		Tcl_AppendResult (interp, "fopen error in ",argv[0],
			" \"",strerror(errno),"\":",file_list,(char *) NULL);
		Tcl_DStringFree (&fn_buf);
		Tcl_DStringFree (&file_list_buf);

		return TCL_ERROR;
	}

	for (data.data = "#", data.size = 2, key.data = (void *)key_buf; fgets(tmp,sizeof(tmp),ffp); )
	{
		len = strlen(tmp);
		tmp[len - 1] = '\0';
		fp = (void *)file_lastroot(tmp);
		if (fp)
		{
			*fp = '\0';
			sprintf (key_buf, "%s%c%s", fp + 1, DB_FLDSEP_CHR, tmp);
			*fp = '/';
		}
		else
		{
			sprintf(key_buf,"%s%c.", tmp, DB_FLDSEP_CHR);
		}
		key.size = strlen(key_buf) + 1;

		db->put(db,&key,&data,R_NOOVERWRITE);
	}
	db->close(db);

	fclose(ffp);

	Tcl_DStringFree (&fn_buf);
	Tcl_DStringFree (&file_list_buf);

	return TCL_OK;
}

