// $Header$

#ifndef input_h
#define input_h

#include <stdio.h>

#include "Glish/glish.h"

class Sequencer;
extern Sequencer* current_sequencer;

// Whether to use the editline library.
#define USE_EDITLINE 1

// Line number to associate with the current expression/statement.
extern int line_num;

extern char* input_file_name;
extern FILE* yyin;
extern int interactive;
extern int statement_can_end;
extern int first_line;
extern char* yytext;
extern const char* glish_init[];

extern int in_func_decl;

#ifndef AIX_YACC
extern int yyparse();
extern int yylex();
#else
extern "C" {
	int yyparse();
	int yylex();
}
#endif
extern void restart_yylex( FILE* input_file );
extern void scan_strings( const char** strings );
extern int interactive_read( FILE* file, const char prompt[], char buf[],
				int max_size );

#endif	/* input_h */
