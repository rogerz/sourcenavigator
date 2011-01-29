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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <config.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <tcl.h>

#include "sn.h"
#include "parser.h"

#include <compat.h>

#ifdef WIN32
#define       OPEN_MODE   "rb"
#else
#define       OPEN_MODE   "r"
#endif /* WIN32 */

extern	FILE	*yyin;

extern int report_local_vars;
extern FILE *cross_ref_fp;
FILE *hig_fp;
static FILE *out_fp;
static int	highlight;

Tcl_Encoding encoding = NULL;
static char	*group = "java";

void start_parser _ANSI_ARGS_ ((char *fname,int cpp,FILE *hfp,int hig));

static int
log_symbol_filename(FILE *fp, char *fname)
{
	char	*outfile = NULL;

        if (fname == NULL) {
	    fprintf(stderr, "log_symbol_filename called with NULL fname\n");
	    exit(1);
	}

        if (yyin) {
	  fclose(yyin);
	}
	
	yyin = fopen(fname,OPEN_MODE);
	if (!yyin)
	{
		fprintf(stderr, "Error: unable to open file \"%s\",errno: %d\n",
			fname,errno);
		return 1;
	}

	if (highlight)
	{
		if (hig_fp)
		{
			fclose(hig_fp);
		}

		outfile = Paf_tempnam(NULL,"hj");
		if (fp)
		{
			fprintf(fp,"%s\n",outfile);
		}

		hig_fp = fopen(outfile,"w+");
	}
	put_status_parsing_file(fname);
	put_file(fname,group,outfile);

	return 0;
}

int
main(int argc, char *argv[])
{
	extern int optind;
	extern char *optarg;
	int	opt;
	char	tmp[MAXPATHLEN];
	char	*fname;
	char	*incl_to_pipe = NULL;
	int	case_flag = TRUE;
	FILE	*list_fp = NULL;
	FILE	*include_fp = NULL;
	char	*cross_ref_file = NULL;

	/* Character set encoding (as defined by Tcl). */
	Tcl_FindExecutable(argv[0]);

	while((opt = getopt(argc,argv,"e:s:n:hy:I:g:i:ltx:Cr:O:m:")) != EOF)
	{
		switch (opt)
   		{
		case 's':
			break;

		case 'n':
			/* FIXME: Remove db prefix option later */
			break;

		case 'e':
		  if ((encoding = Tcl_GetEncoding(NULL, optarg)) == NULL)
		    {
		      fprintf(stderr, "Unable to locate `%s' encoding\n", optarg);
		      return 1;
		    }
		  break;

		case 'h':
			break;

		case 'y':
			list_fp = fopen(optarg,"r");
			if (!list_fp) {
			    fprintf(stderr, "Could not open: \"%s\", %s\n",
			            optarg, strerror(errno));
			    exit(2);
			}
			break;

		case 'I':	/* include path ignored */
			include_fp = fopen(optarg,"r");
			break;

		case 'i':
			incl_to_pipe = optarg;
			break;

		case 'C': /* Parser *.h *.c as C++ */
			break;

		case 'l': /* local variables (ignored) */
			report_local_vars = TRUE;
			break;

		case 'x': /* cross reference file */
			cross_ref_file = optarg;
			break;

		case 'm': /* Macro file. */
		case 'r': /* Comment support. */
		case 't': /* Drop /usr files. */
		case 'g':
			break;

		}
	}

	if (cross_ref_file)
	{
		if (!(cross_ref_fp = fopen(cross_ref_file,"a")))
		{
			fprintf(stderr, "Error: (open) \"%s, errno: %d\"\n",
				cross_ref_file,errno);
			exit(1);
		}
	}

	if (optind < argc || list_fp)
	{
		Paf_Pipe_Create(incl_to_pipe);

		if (list_fp)
		{
		/* This part is called when the project is beeing created. */
			while (fgets(tmp,sizeof(tmp) -1,list_fp))
			{
				if ((fname = strchr(tmp,'\n')))
				{
					*fname = '\0';
				}
				if (!*tmp || *tmp == '#')
				    continue;

				if (log_symbol_filename(out_fp,tmp) == 0)
				{
					start_parser(tmp,0,NULL,0);
				}
			}
			fclose(list_fp);
			list_fp = NULL;
		}
		else
		{
			/* This part is called when a file has been saved,
			 * thus we parse the file and provide highlighting.
			 */
			fname = argv[optind];
			if (!log_symbol_filename(out_fp,fname))
			{
				start_parser(fname,0,hig_fp,highlight);
			}
		}
	}
	else
	{
		fprintf(stderr, "-y or file name required\n");
		exit(1);
	}

	if (yyin)
		fclose(yyin);

	if (out_fp)
		fclose(out_fp);

	if (hig_fp && hig_fp != out_fp)
		fclose(hig_fp);

	Paf_Pipe_Close();

	if (cross_ref_fp)
		fclose(cross_ref_fp);

	if (encoding) {
	    Tcl_FreeEncoding(encoding);
	    Tcl_Finalize();
	}

	return 0;
}

