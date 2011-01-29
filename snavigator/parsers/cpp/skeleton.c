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
 * skeleton.c
 *
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * Startup code for the C and C++ parser.
 */

#include <stdio.h>
#include <stdlib.h>
#include <tcl.h>
#include "dbutils.h"
#include "parser.h"

#ifdef WIN32
#define  OPEN_MODE   O_RDONLY|O_BINARY
#else
#define  OPEN_MODE   O_RDONLY
#endif /* WIN32 */

/* Tcl encoding to translate from. The default (when equal to NULL) is to
   do no translation. */
Tcl_Encoding encoding = NULL;

extern   int yyfd;

void start_parser(char *fname,int parse_cplpl,FILE *highl_fp,int highlight);
void MacroReadFile(const char *Filename);
void MacroFreeTable();
void free_lex_buffers();
void free_token_buffers();

static int parse_cplpl = TRUE;

extern   int   comment_database;
extern int report_local_vars;
extern FILE *cross_ref_fp;
FILE *hig_fp;
static FILE *out_fp;
static int  highlight;

static int
log_symbol_filename(FILE *fp,char *fname)
{
   char  *outfile = NULL;
   char  *group;

   if (fname == NULL) {
       fprintf(stderr, "log_symbol_filename called with NULL filename\n");
       exit(1);
   }

   if (yyfd != -1)
       close(yyfd);

   yyfd = open(fname,OPEN_MODE);
   if (yyfd == -1)
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

       outfile = Paf_tempnam(NULL,"hc");
       if (fp)
       {
           fprintf(fp,"%s\n",outfile);
       }

       hig_fp = fopen(outfile,"w+");
  }
  put_status_parsing_file(fname);

  if (parse_cplpl)
  {
      group = "c++";
  }
  else
  {
      group = "c";
  }
  put_file(fname,group,outfile);

  return 0;
}

int
main(int argc, char *argv[])
{
   extern int optind;
   extern char *optarg;
   int   opt;
   char  tmp[MAXPATHLEN];
   char  *fname;
   char  *db_prefix = NULL;
   char  *incl_to_pipe = NULL;
   char  *list_file = NULL;
   char  *include_files = NULL;
   char  *cross_ref_file = NULL;

   /* Character set encoding (as defined by Tcl). */
   Tcl_FindExecutable(argv[0]);

   while((opt = getopt(argc,argv,"e:s:n:hy:I:g:i:ltx:Cr:O:m:")) != EOF)
   {
      switch (opt)
      {
      case 's':
         break;

      case 'n':
         db_prefix = optarg;
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
         list_file = optarg;
         break;

      case 'I':
         include_files = optarg;
         break;

      case 'i':
         incl_to_pipe = optarg;
         break;

      case 'C': /* Parser files as C and not as C++! */
         parse_cplpl = FALSE;
         break;

      case 'l': /* local variables (ignored) */
         report_local_vars = TRUE;
         break;

      case 'x': /* cross reference file */
         cross_ref_file = optarg;
         break;

      case 'r': /* Remark (comment support) */
         comment_database = TRUE;
         break;

      case 'm':
         MacroReadFile(optarg);
         break;

      /* ignore switches: */
      case 't': /* Drop /usr files. */
      case 'g':   /* group */
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

   if (optind < argc || list_file)
   {
      Paf_Pipe_Create(incl_to_pipe);

      if (include_files)
      {
         /* include_fp will be closed !! */
         Paf_Open_Include_Dirs(include_files,db_prefix);
      }

      if (list_file)
      {
         FILE  *list_fp = fopen(list_file,"r");

         if (!list_fp)
         {
            fprintf(stderr,"Could not open: \"%s\", %s\n",
               list_file,
               strerror(errno));

            exit(2);
         }

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
               start_parser(tmp,parse_cplpl,NULL,0);
            }
         }
         fclose(list_fp);
      }
      else
      {
      /* This part is called when a file has been saved, thus
       * we parse the file and provide highlighting.
       */
         fname = argv[optind];
         if (!log_symbol_filename(out_fp,fname))
         {
            start_parser(fname,parse_cplpl,hig_fp,highlight);
         }
      }

      MacroFreeTable();

      Paf_Close_Include_Dirs();
   }
   else
   {
      fprintf(stderr, "-y or file name required\n");
      exit(1);
   }

   if (yyfd != -1)
      close(yyfd);

   if (out_fp)
      fclose(out_fp);

   if (hig_fp && hig_fp != out_fp)
      fclose(hig_fp);

   Paf_Pipe_Close();

   if (cross_ref_fp)
      fclose(cross_ref_fp);

   free_lex_buffers();
   free_token_buffers();

   if (encoding != NULL)
	{
		Tcl_FreeEncoding(encoding);
		Tcl_Finalize();
	}
   return 0;
}

