/*
 * snptools.c
 *
 * Copyright (C) 1997 Cygnus Solutions, Inc.
 *
 * Description:
 * Implementation of the Source-Navigator parser toolbox library.
 */

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <config.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <sys/stat.h>

#ifdef WIN32
#define OPEN_MODE "rb"
#ifndef MAXPATHLEN
#define MAXPATHLEN _MAX_PATH
#endif /* MAXPATHLEN */
#else /* UNIX */
#define OPEN_MODE "r"
#endif /* WIN32 */

#include <tcl.h>
#include "snptools.h"
#include "parser.h"

/* For platforms that do not have getopt globals in their headers. */
extern char *optarg;
extern int optind;

Tcl_Encoding encoding = NULL;

extern FILE *cross_ref_fp;
static FILE *includelist = NULL;

static long line = 1;
static long column = 0;

static long savedLine;
static long savedColumn;

static char *group = NULL;
static char *incl_to_pipe = NULL;
static char *includename = NULL;
static char *xref_filename = NULL;
static char *dump_tokens_file = NULL;

static char includebuf[512];
static char currentFilename[MAXPATHLEN];


/*
 * boolean values (with their defaults) for command line switches
 */
static int case_sensitive = 1;
extern int comment_database;
static int dialect = 0;
extern int report_local_vars;
static int drop_usr_headers = 0;
static int treat_as_cplusplus = 0;

static FILE *listfp = NULL;
static FILE *outfp = NULL;

static int highlight = 0;
static int highlight_number = 0;
static FILE *highlightfp = NULL;


/*
 * Return the first include path from the list (or NULL if the list is empty).
 */
char * sn_includepath_first()
{
	if (includelist != NULL)
	{
		fclose(includelist);
	}

	includelist = fopen(includename, "r");

	if (includelist == NULL)
	{
		return(NULL);
	}

	return sn_includepath_next();
}


/*
 * Returns the next include path from the list (or NULL if there are no more).
 */
char * sn_includepath_next()
{
	char *p;

	if (includelist == NULL)
	{
		return sn_includepath_first();
	}

	p = fgets(includebuf, sizeof(includebuf), includelist);
	if (p == NULL)
	{
		return(p);
	}
	else
	{
		p = strchr(includebuf, '\n');
		if (p != NULL)
		{
			*p = '\0';	/* strip off newline */
		}

		if (strlen(includebuf) > 0 && includebuf[strlen(includebuf) - 1] != '/')
		{
			strcat(includebuf, "/"); /* ensure path ends in a slash */
		}
	}
	return(includebuf);
}


/*
 * Make the executable panic and return an errorcode of 2.
 */
void sn_panic()
{
	exit(2);
}


/*
 display all possible command line options
 useful for debugging parsers or manually using them
 
 made from the string "I:n:s:hy:g:x:i:luB:e:tCrDS:O:T:" found in sn_process_options()
 */
static void sn_parser_help()
{
	sn_error("Source Navigator - generic parser options:\n" \
		 "-C          treat as C++\n" \
		 "-g <grp>    set group or language string to <grp>\n" \
                 "-h          highlight\n" \
		 "-i <pipe>   incl_to_pipe <pipe>\n" \
		 "-I <name>   includename <name>\n" \
		 "-e <enc>    use TCL encoding <enc>\n" \
		 "-l          report local variables\n" \
		 "-r          comment database\n" \
		 "-s <file>   output to <file>\n" \
		 "-T <file>   dump tokens to <file> and exit\n" \
		 "-t          drop headers in /usr\n" \
		 "-u          be case-insensitive\n" \
		 "-x          Xref filename\n" \
		 "-y          list of files to parse\n" \
                 "\n" \
		 "Ignored options:\n" \
                 "-n          remove db prefix - not implemented\n" \
		 "-B          silently ignored\n" \
		 "-D          silently ignored\n" \
		 "-S          silently ignored\n" \
		 "\n"
		);
}


/*
 * A pseudo-main function that handles command line processing, opening of
 * source files and invoking the parser on those files.
 */
int sn_main(int argc, char *argv[], char * group, FILE ** lexstream, int (*lexer)(), void (*reset)())
{
	sn_set_group(group);

	sn_process_options(argc, argv);

	if (optind < argc || sn_getopt(SN_OPT_LISTFILE))
	{
		sn_init();

		if ((char *) sn_getopt(SN_OPT_LISTFILE) != NULL)
		{
			/* This part is called when the project is being created. */
			sn_parse_all(lexstream, lexer, reset);
		}
		else
		{
			/*
			 * This part is called when a file has been saved, thus we parse the
			 * file.
			 */

			if (sn_register_filename(lexstream, argv[optind]) == 0)
			{
				reset();
				lexer();
			}
		}
	}
	else
	{
		sn_parser_help();
		sn_error("-y or file name required\n");
		return(1);
	}
	sn_close();
	return(0);
}


/*
 * Get the value of any options set on the command line (e.g. -c 300 sets
 * cachesize to "300".
 */
void * sn_getopt(enum sn_options opt)
{
	switch (opt)
	{
	case SN_OPT_CASE_SENSITIVE:
		return (void *) case_sensitive;
	case SN_OPT_COMMENTS:
		return (void *) comment_database;
	case SN_OPT_DIALECT:
		return (void *) dialect;
	case SN_OPT_DUMP_TOKENS:
		return (void *) dump_tokens_file;
	case SN_OPT_DROP_USR_HEADERS:
		return (void *) drop_usr_headers;
	case SN_OPT_GROUP:
		return group;
	case SN_OPT_HIGHLIGHT:
		return (void *) highlight;
	case SN_OPT_INCL_TO_PIPE:
		return incl_to_pipe;
	case SN_OPT_INCLUDE_LIST:
		return (void *) includename;
	case SN_OPT_LOCAL_VARS:
		return (void *) report_local_vars;
	case SN_OPT_LISTFILE:
		return listfp;
	case SN_OPT_TREAT_AS_CPLUSPLUS:
		return (void *) treat_as_cplusplus;
	case SN_OPT_XREF_FILENAME:
		return xref_filename;
	default:
		assert(0);
		break;
	}
	return 0;
}


/*
 * Process the command line options and set the relevant static variables
 * for later reference using sn_getopt().
 */
void sn_process_options(int argc, char *argv[])
{
	int opt;

	/* Character set encoding (as defined by Tcl). */
	Tcl_FindExecutable(argv[0]);

	while ((opt = getopt(argc, argv, "I:n:s:hy:g:x:i:luB:e:tCrDS:O:T:")) != EOF)
	{
		switch (opt)
		{
		case 'B':
			/* silently ignore according to zkoppany */
			break;

		case 'C':
			treat_as_cplusplus = 1;
			break;

		case 'D':
			/* silently ignore according to zkoppany */
			break;

		case 'e':
			if ((encoding = Tcl_GetEncoding(NULL, optarg)) == NULL)
			{
				sn_error("Unable to locate `%s' encoding\n", optarg);
				sn_exit();
			}
			break;

		case 'g':
			group = optarg;
			break;

		case 'h':
			highlight = 1;
			break;

		case 'i':
			incl_to_pipe = optarg;
			break;

		case 'I':
			includename = optarg;
			break;

		case 'l':
			report_local_vars = 1;
			break;

		case 'n':
			/* FIXME: Remove db prefix option later */
			break;

		case 'r':
			comment_database = 1;
			break;

		case 's':
			if ((outfp = fopen(optarg, "a")) == NULL)
			{
				sn_error("could not create %s\n", optarg);
				sn_exit();
			}
			break;

		case 'S':
			/* silently ignore according to zkoppany */
			break;

		case 'T':
			/* Dump tokens to a file and exit */
			dump_tokens_file = optarg;
			break;

		case 't':
			drop_usr_headers = 1;
			break;

		case 'u':
			case_sensitive = 0;
			break;

		case 'x':
			xref_filename = optarg;
			break;

		case 'y':
			listfp = fopen(optarg, "r");
			if (listfp == NULL)
			{
				sn_error("Could not open \"%s\", %s\n", optarg, strerror(errno));
				sn_panic();
			}
			break;

		default:
			assert(0);
			break;
		}
	}
}


/*
 * Print an error message.
 */
int sn_error(char * format, ...)
{
	int i;
	va_list ap;

	va_start(ap, format);
	i = vfprintf(stderr, format, ap);
	va_end(ap);

	fflush(stderr);
	return(i);
}


/*
 * Print a diagnostic message on the S-N processing dialog.
 */
int sn_message(char * format, ...)
{
	int i;
	va_list ap;

	va_start(ap, format);
	i = vfprintf(stdout, format, ap);
	va_end(ap);

	fflush(stdout);
	return(i);
}


/*
 * Write highlight info to a file that will be read by
 * the IDE and used to add highlight tags to the editor.
 * This function should only be called when the -h
 * option has been passed to the browser.
 */
void sn_highlight(enum sn_highlights type,
		  long start_line, int start_column,
		  long end_line, int end_column)
{
	char * tag;
	if (!highlight) return;

	switch (type) {
	case SN_HIGH_COMMENT:
		tag = "rem";
		break;
	case SN_HIGH_KEYWORD:
		tag = "key";
		break;
	case SN_HIGH_STRING:
		tag = "str";
		break;
	case SN_HIGH_VAR_GLOBAL:
		tag = "gv";
		break;
	case SN_HIGH_VAR_LOCAL:
		tag = "lv";
		break;
	case SN_HIGH_FUNCTION:
		tag = "fu";
		break;
	case SN_HIGH_CLASSDEF:
		tag = "cl";
		break;
	default:
		sn_error("Unknown highlight type %d\n", type);
		sn_panic();
	}

	fprintf(highlightfp, "%d %s %d.%d %d.%d\n",
		highlight_number++, /* Ignored by Sn_Highlight_Text */
		tag,
		start_line, start_column, end_line, end_column);
}


/*
 * Make the executable exit due to some error with the error code expected by
 * S-N.
 */
void sn_exit()
{
	if (encoding) {
		Tcl_FreeEncoding(encoding);
		Tcl_Finalize();
	}
	exit(1);
}


/*
 * Override the group (or language) string that must be passed into sn_main.
 */
void sn_set_group(char *newGroup)
{
	group = newGroup;
}


/*
 * Initialise the connection to the project database.
 */
int sn_init()
{
	Paf_Pipe_Create(incl_to_pipe);

	if (xref_filename != NULL && !(cross_ref_fp = fopen(xref_filename, "a")))
	{
		sn_message("Open error: %s\n", xref_filename);
		sn_exit();
	}

	return(0);
}


/*
 * Close the database connection.
 */
int sn_close_db()
{
	return(Paf_Pipe_Close());
}


/*
 * Register a new source file in the project.
 */
int sn_register_filename(FILE ** lexstream, char * filename)
{
	if (*lexstream)
	{
		fclose(*lexstream);
	}

	*lexstream = fopen(filename, OPEN_MODE);

	if (!(*lexstream))
	{
		sn_message("Error: unable to open file %s\n", filename);
		return(1);
	}
	else
	{
		char * highlight_fname = NULL;

		/*
		 * If the -h option was passed then create a tmp file
		 * and save highlight info into the file. The -s option
		 * is used in conjunction with -h to indicate a file
		 * that the name of the highlight file will be saved in.
		 */

		if (highlight) {
			if (highlightfp) {
				fclose(highlightfp);
			}
			highlight_fname = Paf_tempnam(NULL,"hj");
			if (outfp) {
				fprintf(outfp,"%s\n",highlight_fname);
			}
			highlightfp = fopen(highlight_fname,"w+");
			highlight_number = 1;
		}

		strcpy(currentFilename, filename);
		put_status_parsing_file(filename);
		put_file(filename, group, highlight_fname);
	}
	return(0);
}


/*
 * Set the current column to a new position.
 */
void sn_set_column(long c)
{
	column = c;
}


/*
 * Reset the column position.
 */
void sn_reset_column()
{
	sn_set_column(0);
}


/*
 * Advance the column position by `num' positions.
 */
void sn_advance_column(int num)
{
	column += num;
}


/*
 * Set the current line to a new position.
 */
void sn_set_line(long l)
{
	line = l;
}


/*
 * Reset the line position.
 */
void sn_reset_line()
{
	sn_set_line(1);
}


/*
 * Advance to the next line position.
 */
void sn_advance_line()
{
	line++;
}


/*
 * Retrieve the current line position.
 */
long sn_line()
{
	return line;
}


/*
 * Retrieve the current column position.
 */
long sn_column()
{
	return column;
}


/*
 * Save the current line position on a stack.
 */
void sn_push_line()
{
	savedLine = sn_line();
}


/*
 * Pop the stored line position off the stack.
 */
long sn_pop_line()
{
	long result = savedLine;
	savedLine = -1;
	return result;
}


/*
 * Push the current column position onto the stack.
 */
void sn_push_column()
{
	savedColumn = sn_column();
}


/*
 * Pop the stored column position off the stack.
 */
long sn_pop_column()
{
	long result = savedColumn;

	savedColumn = -1;
	return result;
}


/*
 * Parse all the files listed in the source file list specified using -y
 * on the command line.
 */
void sn_parse_all(FILE ** lexstream, int (*parse)(), void (*reset)())
{
	char filename[512];
	char * temp;

	if (listfp == NULL) return;

	while (fgets(filename, sizeof(filename) - 1, listfp))
	{
		if ((temp = strchr(filename, '\n')))
		{
			*temp = 0; /* null terminate the string */
		}
		if (!*filename || *filename == '#')
			continue;

		if (sn_register_filename(lexstream, filename) == 0)
		{
			parse();
			reset();
		}
	}
}


/*
 * Close all files and the database connection.
 */
void sn_close()
{
	sn_close_db();

	if (listfp != NULL)
	{
		fclose(listfp);
	}

	if (outfp != NULL)
	{
		fclose(outfp);
	}

	if (highlightfp != NULL)
	{
		fclose(highlightfp);
	}

	if (cross_ref_fp != NULL)
	{
		fclose(cross_ref_fp);
	}
}


/*
 * Count the number of line and column advancements in a null-terminated
 * buffer.
 */
void sn_count_chars(char *buf, int length)
{
	char *p;
	int i;

	for (p = buf, i = length; i > 0; i--, p--)
	{
		if (*p == '\n')
		{
			sn_advance_line();
			sn_reset_column();
		}
		else
		{
			sn_advance_column(1);
		}
	}
}


/*
 * Return the filename of the current source file.
 */
char * sn_current_file()
{
	return currentFilename;
}


/*
 * Are we meant to be generating cross-referencing information?
 * Returns 1 if so; 0 if not.
 */
int sn_cross_referencing()
{
	return (cross_ref_fp != NULL);
}


/*
 * Returns a pointer into `buf' indicating where the beginning of the last
 * non-whitespace region begins.  See `snptools.h' for more information.
 */
char * sn_afterlastwspace(char * buf)
{
	char * p;
	int len;

	if ((len = strlen(buf)) == 0)
	{
		return(buf);
	}

	for (p = &buf[len - 1]; p >= buf && !isspace((int) *p); p--);
	return(p + 1);
}


/*
 * Insert a symbol into the project database.
 * See the API documentation for detailed information.
 */
int sn_insert_symbol(int id_type, char *classname, char *identifier,
		 char *filename, int start_lineno, int start_colpos,
		 int end_lineno, int end_colpos, unsigned long attr,
		 char *ret, char *arg_types, char *arg_names, char *comment,
		 int high_start_lineno, int high_start_colpos,
		 int high_end_lineno, int high_end_colpos)
{
	return(put_symbol(id_type, classname, identifier, filename, start_lineno,
			  start_colpos, end_lineno, end_colpos, attr, ret, arg_types,
			  arg_names, comment, high_start_lineno, high_start_colpos,
			  high_end_lineno, high_end_colpos));
}


/*
 * Insert cross-referencing information into the project database.
 * See the API documentation for detailed information.
 */
int sn_insert_xref(int type, int scope_type, int scope_level,
		   char *classname, char *funcname, char *argtypes,
		   char *refclass, char *refsymbol, char *ref_arg_types,
		   char *filename, int lineno, int acc)
{
	if (sn_cross_referencing())
	{
		/* Use special "GLOBAL" namespace, funcname should be NULL.
		 * Currently, scope_type is changed to a "fu" but it really
		 * should be "na". Namespace support in the IDE needs to
		 * be fixed up so that xrefs in namespaces work before
		 * "na" can be passed.
		 */
		if (scope_type == SN_GLOBAL_NAMESPACE) {
			assert(funcname == NULL);
			funcname = "GLOBAL";
			scope_type = SN_FUNC_DEF;
		}

		return put_cross_ref(type, scope_type, scope_level, classname, funcname,
				     argtypes, refclass, refsymbol, ref_arg_types,
				     filename, lineno, acc);
	}
	return(0);
}


/*
 * Insert a comment into the project database.
 * See the API documentation for detailed information.
 */
int sn_insert_comment(char *classname, char *funcname, char *filename,
		      char *comment, int beg_line, int beg_col)
{
	if (sn_getopt(SN_OPT_COMMENTS))
	{
		return put_comment(classname, funcname, filename, comment, beg_line, beg_col);
	}
	return(0);
}


/*
 * Search the include path for an include file.
 */
int sn_find_file(char * filename, char * buf)
{
	struct stat unused;
	char absfilename[MAXPATHLEN];
	char * path;

	/* Try the working directory. */
	if (stat(filename, &unused) == 0)
	{
		strcpy(buf, filename);
		return 0;
	}

	/* Failed; try the search path. */
	path = sn_includepath_first();
	while (path != NULL)
	{
		sprintf(absfilename, "%s%s", path, filename);
		if (stat(absfilename, &unused) == 0)
		{
			strcpy(buf, absfilename);
			return 0;
		}
		path = sn_includepath_next();
	}

	/* Couldn't find the file anywhere! */
	return 1;
}
