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
 * cbrowser2.c
 *
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * Pass #2 of the C/C++ parser, this pass will generate xrefs that are
 * processed by dbimp.
 */

#include "dbutils.h"
#include <setjmp.h>
#include <signal.h>

#include "longstr.h"

#include <tcl.h>

#ifdef __MSVC__
#include <io.h>
#endif

/* Not used, but we need to define one due to us linking against libcpp2. */
Tcl_Encoding encoding = NULL;

extern	int	optind;

#ifdef WIN32
typedef const char	*ARGTYPE;
#else
typedef char	*ARGTYPE;
#endif /* WIN32 */

extern ARGTYPE	optarg;

extern	char	*filename_g;
extern	int	yylineno,yycharno;
extern	int	report_local_vars;
extern	int	yyfd;
extern	jmp_buf	BAD_IMPL_jmp_buf;
extern FILE *cross_ref_fp;

static	int	killed;

#define	TTY_TRACE	0	/* Normally 0 */
#if TTY_TRACE
static	FILE	*tty;
#endif /* TTY_TRACE */

static	void
my_panic(int sign)
{
	killed = -1;

	fprintf(stderr,"cbrowser2 panic; signal %d received",sign);

	if (filename_g)
	{
		fprintf(stderr,", in file: %s, at line: %d column: %d\n",
			filename_g,yylineno,yycharno);
	}
	fprintf(stderr,"\n");
	fflush(stderr);

#ifdef SIGABRT
	signal(SIGABRT,SIG_IGN);
#endif /* SIGABRT */

	abort();
}

static  void
term_catch(int sign)
{
	signal(sign,term_catch);

	killed = TRUE;
#if TTY_TRACE
	if (tty)
	{
		fprintf(tty,"Received signal: %d (pid: %d)\n",
			sign,(int)getpid());
	}
#endif /* TTY_TRACE */
}

static void
set_signals()
{
#ifdef SIGHUP
	signal(SIGHUP,my_panic);
#endif /* SIGHUP */

#ifdef SIGINT
	signal(SIGINT,term_catch);
#endif /* SIGINT */

#ifdef SIGQUIT
	signal(SIGQUIT,my_panic);
#endif /* SIGQUIT */

#ifdef SIGILL
	signal(SIGILL,my_panic);
#endif /* SIGILL */

#ifdef SIGABRT
	signal(SIGABRT,my_panic);
#endif /* SIGABRT */

#ifdef SIGBUS
	signal(SIGBUS,my_panic);
#endif /* SIGBUS */

#ifdef SIGSEGV
	signal(SIGSEGV,my_panic);
#endif /* SIGSEGV */

#ifdef SIGSYS
	signal(SIGSYS,my_panic);
#endif /* SIGSYS */

#ifdef SIGPIPE
	signal(SIGPIPE,term_catch);
#endif /* SIGPIPE */

#ifdef SIGTERM
	signal(SIGTERM,term_catch);
#endif /* SIGTERM */

#ifdef SIGURG
	signal(SIGURG,term_catch);
#endif /* SIGURG */

#ifdef SIGXCPU
	signal(SIGXCPU,my_panic);
#endif /* SIGXCPU */

#ifdef SIGXFSZ
	signal(SIGXFSZ,my_panic);
#endif /* SIGXFSZ */

#ifdef SIGUSR1
	signal(SIGUSR1,my_panic);
#endif /* SIGUSR1 */

#ifdef SIGUSR2
	signal(SIGUSR2,my_panic);
#endif /* SIGUSR2 */

#ifdef SIGPROF
	signal(SIGPROF,my_panic);
#endif /* SIGPROF */

#ifdef SIGDANGER
	signal(SIGDANGER,my_panic);
#endif /* SIGDANGER */

#ifdef SIGPRE
	signal(SIGPRE,my_panic);
#endif /* SIGPRE */
}

#define	MAX_MACRO_FILES 500

void Paf_Cpp_Cross_Ref_Clean();

int
main(int argc, char **argv)
{
        char    *data;
        char    *key;
	char	*bufp;
	char	save_c;
	long	type;
	int	linenum;
        char    cpp_xref[10];
        int     cpp_xref_length;
	ARGTYPE	cache = NULL;
	ARGTYPE	cross_cache = NULL;
	char  *db_prefix = NULL;
	FILE	*logfp = NULL;
	FILE	*infp = NULL;
	ARGTYPE	xref_file = NULL;
	int	first_xref = TRUE;
	ARGTYPE omode = NULL;
	int	set_sgns = FALSE;
	LongString	buf;
	ARGTYPE macro_file[MAX_MACRO_FILES];
	int macro_file_num = 0;

	LongStringInit(&buf,0);
	Tcl_FindExecutable(argv[0]);

#if !WIN32 && TTY_TRACE
	tty = fopen("/dev/tty","w");
#endif /* !WIN32 && TTY_TRACE */

	while((type = getopt(argc,argv,"lc:C:O:M:sm:n:")) != EOF)
	{
		switch (type)
		{
		case 'c':
			cache = optarg;
			break;

		case 'C':
			cross_cache = optarg;
			break;

		case 'l':
			report_local_vars = TRUE;
			break;

		case 'M':
			omode = optarg;
			break;

		case 's':
			set_sgns = FALSE;
			break;

		case 'm':
			if (macro_file_num < MAX_MACRO_FILES - 1)
			{
				macro_file[macro_file_num++] = optarg;
			}
			MacroReadFile(optarg);
			break;
		case 'n':
			db_prefix = optarg;
			break;
		}
	}

	if (optind < argc)
	{
		xref_file = argv[optind];
	}

	if (!xref_file || !db_prefix)
	{
		fprintf(stderr, "Usage: %s ?-l? ?-m macrofile? -n db_prefix xref_file\n",
			argv[0]);
		fflush(stderr);
		exit(2);
	}

	if (set_sgns)
		set_signals();

	killed = FALSE;

	if (xref_file)
	{
		FILE	*in;

		in = fopen(xref_file,"r");
		if (in) {
			infp = in;
		} else {
		    fprintf(stderr, "xref_file \"%s\" does not exist\n", xref_file);
		    exit(3);
		}
	}

        Paf_db_init_tables(db_prefix,cache,cross_cache);

	/*SN_DBIMP is set interactive in the tcl init procedure!*/
	if (getenv("SN_DBIMP"))
	{
		char    tmp[MAXPATHLEN];
		char    *e;

		e = getenv("TMPDIR");

		sprintf(tmp,"%s/cbrowser2_%lu.tmp",
			e ? e : "/tmp",
			(unsigned long)getpid());

		logfp = fopen(tmp,"w+");
		if (logfp)
		{
			chmod(tmp,0666);
			fprintf(logfp,"#cbrowser2 (pid: %lu) started\n",
				(unsigned long)getpid());
			fprintf(logfp,"local_vars: %d\n",
				report_local_vars);
			if (macro_file_num)
			{
				int cou;

				for (cou = 0; cou < macro_file_num; cou++)
				{
					fprintf(logfp,"macro file %d: <%s>\n",
						cou + 1,
						macro_file[cou]);
				}
			}
			fflush(logfp);
		}
	}

	linenum = 0;
	type = -999;

	switch (setjmp(BAD_IMPL_jmp_buf))
	{
	case PAF_PANIC_SOFT:
		killed = TRUE;

		fprintf(stderr,"cbrowser2 (soft) panic");

		if (filename_g)
		{
			fprintf(stderr,", in file: %s, at line: %d column: %d\n",
				filename_g,yylineno,yycharno);
		}
		fprintf(stderr,"\n");
		fflush(stderr);
		break;

	case PAF_PANIC_SIMPLE:
		killed = TRUE;
		break;

	case PAF_PANIC_EMERGENCY:
		killed = -1;

		fprintf(stderr,"Run time error: %s\n",strerror(errno));
		fflush(stderr);
		break;
	}

        cross_ref_fp = stdout;

        snprintf(cpp_xref, sizeof(cpp_xref), "%d;", PAF_CROSS_REF_CPP);
        cpp_xref_length = strlen(cpp_xref);

	while (!killed && (bufp = buf.fgets(&buf,infp)))
	{
		if (logfp)
		{
			fputs(bufp,logfp);
			fputs("\n",logfp);
			fflush(logfp);
		}

		if (strncmp(bufp, cpp_xref, cpp_xref_length) == 0) {
			if (first_xref) {
				first_xref = FALSE;
				open_tables_for_cross_ref();
			}
			Paf_insert_cross_ref_qry(bufp);
		} else {
			fprintf(stdout, "%s\n", bufp);
			continue;
		}
	}

#if TTY_TRACE
	if (tty)
	{
		fprintf(tty,"END (pid: %d) killed: %d eof: %d\n",
			(int)getpid(),killed,feof(infp));
	}
#endif /* TTY_TRACE */

	buf.free(&buf);

	if (!first_xref)
	{
		Paf_Cpp_Cross_Ref_Clean();
	}

	if (Paf_db_close_tables() == -1)
	{
		fprintf(stderr,"Database closing error: %s\n",strerror(errno));
		fflush(stderr);

		killed = -1;
	}

	if (yyfd >= 0)
	{
		close(yyfd);
		yyfd = -1;
	}

	fclose(infp);

	if (logfp)
	{
		fprintf(logfp,"#cbrowser2 (pid: %lu) exited with status %d\n",
                    (unsigned long)getpid(), (killed == -1 ? 2 : 0));
		fclose(logfp);
	}

	exit(killed == -1 ? 2 : 0);

	return 0;
}

