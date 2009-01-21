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
 * parser.c
 *
 * This file contains support routines used by SN parsers. These
 * routines typically output data in a format that is then read
 * by dbimp.
 */

#include "parser.h"

#include <tcl.h>

#include <sn.h>

/* This seems to conflict with a define in longstr.h so include first! */
#include "mxdefine.h"
#include "mxfuncs.h"

#include "longstr.h"

#include "fileutils.h" /* for SN_PATH_UNIX */

#include <config.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <errno.h>

static  char    *acc_strings[] = {
	"r","w","p","u"
};

#ifndef WIN32
typedef FILE*   HANDLE;
#define INVALID_HANDLE_VALUE    (FILE *)0
#endif /* WIN32 */

static  HANDLE  pipe_handle = INVALID_HANDLE_VALUE;

/* These global vars are accessed directly from the parsers */
int  comment_database = FALSE;
FILE *cross_ref_fp;
int	 report_local_vars = FALSE;

static int Paf_Pipe_Write MX_VARARGS(char *,str);
static int Paf_Pipe_Flush ();

#ifndef MY_DEBUG2
#define MY_DEBUG(x)
#define MY_DEBUG2(x)
#define MY_DEBUG(x)
#endif

void
Paf_Pipe_Create(char *incl_to_pipe)
{
#ifdef WIN32
	pipe_handle = GetStdHandle(STD_OUTPUT_HANDLE);
#else
	pipe_handle = stdout;
#endif /* WIN32 */

	if (incl_to_pipe)
	{
		FILE    *ifp;
		char    tmp[1024];

		if ((ifp = fopen(incl_to_pipe,"r")) == NULL)
		{
			fprintf(stderr,"Error: couldn't load \"%s\", error: \"%s\"\n",
				incl_to_pipe, strerror(errno));
			fflush(stderr);
			exit(1);
		}
		while (fgets(tmp,sizeof(tmp)-1,ifp))
		{
			Paf_Pipe_Write(tmp);
		}
		Paf_Pipe_Flush();

		fclose(ifp);

		unlink(incl_to_pipe);   /* Nobody needs it. */
	}
}

int
Paf_Pipe_Close()
{
	if (pipe_handle == INVALID_HANDLE_VALUE)
		panic("pipe was not opened before Paf_Pipe_Close call");

	pipe_handle = INVALID_HANDLE_VALUE;

	return 0;
}

static int
Paf_Pipe_Write MX_VARARGS_DEF(char *, arg1)
{
	va_list args;
	char    *fmt;
	char    tmp[10000];
	int     len;
#ifdef WIN32
	DWORD     cou;
#else
	int     cou;
#endif
	Tcl_DString utfBuffer;

	if (pipe_handle == INVALID_HANDLE_VALUE)
		return -1;

	fmt = (char *)MX_VARARGS_START(char *,arg1,args);

	len = vsprintf(tmp,fmt, args);
	for (fmt = tmp; len > 0;)
	{
		cou = 0;
		Tcl_ExternalToUtfDString(NULL, tmp, len, &utfBuffer);
#ifdef _WINDOWS
		if (WriteFile(pipe_handle,Tcl_DStringValue(&utfBuffer),
		    Tcl_DStringLength(&utfBuffer),&cou,NULL) == FALSE)
		{
			Tcl_DStringFree(&utfBuffer);
			return FALSE;
		}
#else
		cou = fprintf(pipe_handle,"%s",Tcl_DStringValue(&utfBuffer));
		if (cou == -1) {
			Tcl_DStringFree(&utfBuffer);
			return FALSE;
		}
#endif

		if (cou > 0)
		{
			fmt += cou;
			len -= cou;
		}
		Tcl_DStringFree(&utfBuffer);
	}
	return TRUE;
}


static int
Paf_Pipe_Flush()
{
	if (pipe_handle == INVALID_HANDLE_VALUE)
		return -1;
#ifdef  WIN32
	return (int)FlushFileBuffers(pipe_handle);
#else
	return fflush(pipe_handle);
#endif /* __MSVC__ */
}

/* This functions writes out a file parsing status line.
 * It is invoked by the various parser routines to write
 * out a line of input that is parsed by the
 * Source-Navigator procudure event_LoadPipeInput.
 */

void
put_status_parsing_file(char *fname) {
    Paf_Pipe_Write("Status: Parsing: %s\n", fname);
    Paf_Pipe_Flush();
}


/*
 * This functions prints data for a file to the output pipe.
 * This function is only called by the parsers.
 */

int
put_file(char *file_name,char *group,char *highlight_file)
{
	if (!file_name)
	{
		fprintf(stderr,"put_file: empty file name\n");
		return -1;
	}

	if (pipe_handle == INVALID_HANDLE_VALUE)
	{
		panic("put_file called when pipe is not open");
	}

	Paf_Pipe_Write("%d%c%s%c%s%c%s\n",
		PAF_FILE,      KEY_DATA_SEP_CHR,
		file_name,     KEY_DATA_SEP_CHR,
		group,         DB_FLDSEP_CHR,
		highlight_file ? highlight_file : "");

	return Paf_Pipe_Flush();       /* 'dbimp' can start deleting. */
}


/*
 * This functions prints symbol data to the output pipe.
 * This function is only called by the parsers.
 */

int
put_symbol(
int     sym_type,
char    *scope_name,
char    *symbol_name,
char    *file_name,
int     start_lineno,
int     start_colpos,
int     end_lineno,
int     end_colpos,
unsigned long   attr,
char    *ret,
char    *arg_types,
char    *args,
char    *comment,
int     high_start_lineno,
int     high_start_colpos,
int     high_end_lineno,
int     high_end_colpos)
{
	char	*sym_str_type = SN_GetSymbolType(sym_type);

	if (!file_name)
	{
		fprintf(stderr,"Error: put_symbol argument file_name must not be NULL\n");
		fflush(stderr);
		return -1;
	}
	for (; isspace(*file_name); file_name++);

	if (!sym_str_type)
	{
		fprintf(stderr,"Error: put_symbol unknown type: %d file: %s\n",
			sym_type,file_name);
		fflush(stderr);
		return -1;
	}

	if (!symbol_name || !*symbol_name)
	{
		fprintf(stderr,
			"Error: put_symbol argument #3 must not be empty, type: (%s), line: %d file: %s\n",
			sym_str_type,start_lineno,file_name);
			
		fprintf(stderr,
			"Dump: scope_name: %s, symbol_name: %s, file_name: %s, start_lineno: %d, start_colpos: %d, end_lineno: %d, end_colpos: %d, " \
			"attr: %d, ret: %s, arg_types: %s, args: %s, comment: %s\n",
			scope_name, symbol_name, file_name,
			start_lineno, start_colpos, end_lineno, end_colpos,
			attr,
			ret, arg_types, args, comment
		       );
		 
		fflush(stderr);
		return -1;
	}

	for (; isspace(*symbol_name); symbol_name++);

	/* If args is just a string we take a special name. */
	if (args && *args == '"')
	{
		args = "%STRING%";
	}

	if (comment_database && comment && *comment)
	{
		register char *p;

		for (p = comment; *p; p++)
		{
			switch (*p)
			{
			case    '\n':
				*p = (char )0xff;
				break;

			case    '{':
			case    '}':
				*p = DB_FLDSEP_CHR;
				break;
			}
		}
	}
	else
		comment = "";

	/* Make sure no '\n' appears in the symbol name */
	{
		register char *p;

		for (p = symbol_name; *p; p++)
		{
			if (*p == '\n') {
			    *p = (char )0xff;
			}
		}
	}

	switch (sym_type)
	{
	case	PAF_TYPE_DEF:
	case	PAF_CLASS_DEF:
	case	PAF_ENUM_DEF:
	case	PAF_CONS_DEF:
	case	PAF_MACRO_DEF:
	case	PAF_FUNC_DEF:
	case	PAF_GLOB_VAR_DEF:
	case	PAF_FUNC_DCL:
	case	PAF_UNION_DEF:
	case	PAF_ENUM_CONST_DEF:
		scope_name = NULL;
		break;
	}

	/* Skip leading blanks */
	for (;scope_name && isspace(*scope_name); scope_name++);

	if (!scope_name || *scope_name == '\0')
	{
		scope_name = NULL;

		switch (sym_type)
		{
		case PAF_MBR_FUNC_DEF:
		case PAF_MBR_VAR_DEF:
		case PAF_COMMON_MBR_VAR_DEF:
		case PAF_CLASS_INHERIT:
		case PAF_MBR_FUNC_DCL:
/*		case PAF_ENUM_CONST_DEF: */
			fprintf(stderr,
				"Error: put_symbol argument #2 must not be empty, type: (%s), line: %d file: %s\n",
				sym_str_type,start_lineno,file_name);
			fflush(stderr);
			return -1;
			break;
		}
	}

	if (high_start_lineno == 0)	/* Take the symbol definition. */
	{
		high_start_lineno = start_lineno;
		high_start_colpos = start_colpos;
		high_end_lineno = start_lineno;
		high_end_colpos = start_colpos + strlen(symbol_name);
	}

	if (pipe_handle == INVALID_HANDLE_VALUE)
	{
		panic("put_symbol called when pipe is not open");
	}

	Paf_Pipe_Write("%d%c%s%s%s%c%06d.%03d%c%s%c%d.%d%c0x%x%c{%s}%c{%s}%c{%s}%c{%s}\n",
		sym_type,                        KEY_DATA_SEP_CHR,
		scope_name ? scope_name : "",
		scope_name ? DB_FLDSEP_STR : "",
		symbol_name,                     DB_FLDSEP_CHR,
		high_start_lineno,
                    high_start_colpos,           DB_FLDSEP_CHR,
		file_name,                       KEY_DATA_SEP_CHR,
		high_end_lineno, high_end_colpos,DB_FLDSEP_CHR,
		attr,                            DB_FLDSEP_CHR,
		ret ? ret : "",                  DB_FLDSEP_CHR,
		arg_types ? arg_types : "",      DB_FLDSEP_CHR,
		args ? args : "",                DB_FLDSEP_CHR,
		comment
		);

	Paf_Pipe_Write("%d%c%s%c%06d.%03d%c%s%c%s%c%s%c%d.%d%c%d.%d%c%d.%d%c{%s}\n",
		PAF_FILE_SYMBOLS,                     KEY_DATA_SEP_CHR,
		file_name,                            DB_FLDSEP_CHR,
		start_lineno, start_colpos,           DB_FLDSEP_CHR,
		scope_name ? scope_name : "#",        DB_FLDSEP_CHR,
		symbol_name,                          DB_FLDSEP_CHR,
		sym_str_type,                         KEY_DATA_SEP_CHR,
		end_lineno,        end_colpos,        DB_FLDSEP_CHR,
		high_start_lineno, high_start_colpos, DB_FLDSEP_CHR,
		high_end_lineno,   high_end_colpos,   DB_FLDSEP_CHR,
		arg_types ? arg_types : ""
		);

	return 0;
}

/*
 * This functions prints comment info to the xref file.
 * This function is only called by the parsers.
 */

int
put_comment(char *classn,char *func,char *filename,char *comment,int beg_line,int beg_char)
{
	register unsigned char  *p;

	if (!comment_database || !comment || !cross_ref_fp)
	{
		return 0;
	}

	/* We use cross_ref_fp because the comments should be inserted
	 * only during the second phase with the cross reference together.
	 */
	for (p = (unsigned char *)comment; *p; p++)
	{
		if (*p == '\n')
		{
			*p = 0xff;
		}
	}

	MY_DEBUG((Output, "put comment into file <%s>\n", filename));

	fprintf(cross_ref_fp,
		"%d%c%s%c%06d.%03d%c%s%c%s%c%s\n",
		PAF_COMMENT_DEF,                  KEY_DATA_SEP_CHR,
		filename,                         DB_FLDSEP_CHR,
		beg_line, beg_char,               DB_FLDSEP_CHR,
		classn && *classn ? classn : "#", DB_FLDSEP_CHR,
		func && *func ? func : "#",       KEY_DATA_SEP_CHR,
		comment);

	return 0;
}

int
put_cross_ref(
int     type,
int     scope_type,
int     scope_lev,
char    *fnc_cls,              /* caller class */
char    *fnc,                   /* caller function/method */
char	*fnc_arg_types,         /* caller function/method argument types */
char    *scope,                 /* referenced class */
char	*what,                  /* referenced member */
char	*arg_types,		/* referenced function/method argument types */
char	*file,
int	lineno,
int	acc)
{
	LongString	key_value;
	LongString	data_value;
	char	lineno_buf[10];

	if (!cross_ref_fp || !fnc || *fnc == '\0' ||
		(scope_lev == PAF_REF_SCOPE_LOCAL && !report_local_vars))
	{
		return -1;
	}

	MY_DEBUG2 ((Output, "put_cross_ref (%i, %i, %i, %s, %s, %s, %s, %s, %s, %s, %i, %i)\n",
					type, scope_type, scope_lev,
					fnc_cls?fnc_cls:"?",
					fnc?fnc:"?",
					fnc_arg_types?fnc_arg_types:"?", scope?scope:"?", what?what:"?",
					arg_types?arg_types:"?",	file?file:"?", lineno, acc));

	if (scope && *scope == '\0')
		scope = NULL;
	else if (scope)
	{
		char *_p;
		for (; isspace(*scope); scope++);
		
		/*
		 * It can happen, that the scope contains "class fld ",
		 * so then terminate the rest after "class" */
		if ((_p=strchr (scope, DB_FLDSEP_CHR)))
		{
			*_p = 0;
		}
		/**/
		if (*scope == '\0')
			scope = NULL;
	}
	if (fnc_cls && *fnc_cls == '\0')
		fnc_cls = NULL;
	else if (fnc_cls)
	{
		for (; isspace(*fnc_cls); fnc_cls++);
	}

	if (!file)
	{
		fprintf(stderr,"Filename must not be NULL\n");
		return -1;
	}
	if (!what || !*what)
	{
		fprintf(stderr,"Input parameter (#8) must not be NULL file: %s line: %d\n",
				file,lineno);
		return -1;
	}

	for (; isspace(*what); what++);
	for (; isspace(*file); file++);

	if (type <= 0 || type > PAF_REF_UNDEFINED)
	{
		fprintf(stderr,"Input parameter (#1) %d is not allowed\n",
			type);
		return -1;
	}

	if (scope_type <= 0 || scope_type >= PAF_VAR_DCL)
	{
		fprintf(stderr,"Input parameter (#2) %d is not allowed\n",
			scope_type);
		return -1;
	}

	LongStringInit(&key_value,0);
	LongStringInit(&data_value,0);

	if (type == PAF_MBR_FUNC_DCL)
		type = PAF_MBR_FUNC_DEF;

	sprintf(lineno_buf,"%06d",lineno);
	key_value.copystrings(&key_value,
		fnc_cls ? fnc_cls : "#",            DB_FLDSEP_STR,
		fnc,                                DB_FLDSEP_STR,
		SN_GetSymbolType(scope_type),       DB_FLDSEP_STR,
		scope ? scope : "#",                DB_FLDSEP_STR,
		what,                               DB_FLDSEP_STR,
		SN_GetSymbolType(type),             DB_FLDSEP_STR,
		acc_strings[acc],                   DB_FLDSEP_STR,
		lineno_buf,                         DB_FLDSEP_STR,
		file,
		NULL);
	data_value.copystrings(&data_value,
		fnc_arg_types ? fnc_arg_types : "", DB_FLDSEP_STR,
		arg_types ? arg_types : "",
		NULL);

	if(!scope || *scope != '?')
	{
		fprintf(cross_ref_fp, "%d%c%s%c%s\n",
			PAF_CROSS_REF, KEY_DATA_SEP_CHR,
			key_value.buf, KEY_DATA_SEP_CHR,
			data_value.buf);
	}

	key_value.free(&key_value);
	data_value.free(&data_value);

	return 0;
}

/*
 *      We have to conver '\\' to '/'. That's all.
 */
char *
Paf_tempnam(char *dir,char *pref)
{
	static	char	tmpnm[MAXPATHLEN + 1];
	char    *nm;

	nm = tempnam(dir,pref);

	if (!nm)
		return nm;

	unlink(nm);     /* Just to be very sure !!! */

	strcpy (tmpnm, nm);
	sn_internal_convert_path (tmpnm, SN_PATH_UNIX);
	
	/* DON'T USE ckfree */
	free(nm);

	return tmpnm;
}

