/*

Copyright (c) 2003, Mo DeJong

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
 * phpbrowser.l
 *
 * Copyright (C) 2003 Mo DeJong
 *
 * Description:
 * Lex input file for an php language processor.
 */

%{

#include <ctype.h>
#include <stdio.h>
#include "snptools.h"
#include "lexinput.h"
#include "longstr.h"

#undef yywrap
#define YY_SKIP_YYWRAP

#undef YY_INPUT
#define YY_INPUT(buf,r,ms) (r = sn_encoded_input(buf, ms))

static char group[] = "php";

#define MAX_SIZE 512

/* FIXME: Can't pass NULL or "" as current function (core dump) */
static char current_function[MAX_SIZE] = {"GLOBAL"};
static char current_function_args[MAX_SIZE] = {""};
static int  current_function_highlight_line;

/* in "function fog() {}" column of 'f' in "fog" */
static int  current_function_highlight_column_start;

/* in "function fog() {}" column of 'g' in "fog" */
static int  current_function_highlight_column_end;

/* line where "function" appears */
static int  current_function_line_start;

/* line where closing brace of function appears */
static int  current_function_line_end;

/* in "function fog() {}" column of 'f' in "function" */
static int  current_function_column_start;

/* in "function fog() {}" column of '}' in "{}" */
static int  current_function_column_end;

static int  current_function_brace_count;

static int result;

/* Stores a comment buffer that could be built over multiple lines/rules. */
LongString cbuff;
int cbuff_start_line;
int cbuff_start_column;
#define COMMENT_DUMP 0

/*
 * Flush any function definitions that might be outstanding (ie. if its
 * label appears _last_ in a file.  When we reach EOF, check to see if its
 * defn needs to be flushed to the database.  Normally the occurence of
 * another label causes the defn to be stored.
 * 
 * Return 1 so flex will keep playing.
 */

int
yywrap()
{
  return(1);
}

%}

%option stack
%x COMMENT
%x PHP
%x FUNCTION

ws		[ \t]
wsn		[ \t\n]
symbol		[a-zA-Z0-9_]+
array-notation  (\[\]|\[\"{symbol}\"\]|\[\'{symbol}\'\])

%%

<FUNCTION>\{ {
  sn_advance_column(yyleng);
  current_function_brace_count++;
}

<FUNCTION>\} {
  sn_advance_column(yyleng);
  current_function_brace_count--;
  if (current_function_brace_count == 0) {
    current_function_line_end = sn_line();
    current_function_column_end = sn_column();

    result = sn_insert_symbol(SN_FUNC_DEF, NULL,
            current_function,
            sn_current_file(), 
            current_function_line_start, current_function_column_start,
            current_function_line_end, current_function_column_end,
            0 /* attribute */,
            NULL /* return type */,
            NULL /* argument types */,
            current_function_args /* argument names */,
            NULL /* comment */,
            current_function_highlight_line,
            current_function_highlight_column_start,
            current_function_highlight_line,
            current_function_highlight_column_end );

    assert(result == 0);

    strcpy(current_function, "GLOBAL");

    yy_pop_state(); /*BEGIN(PHP);*/
  }
}

<INITIAL,PHP,FUNCTION>.		{ sn_advance_column(yyleng); /* eat text */ }

<PHP,FUNCTION>\\\$		{ sn_advance_column(yyleng); /* ignore \$ */ }


<INITIAL,PHP,FUNCTION>\n		{ sn_advance_line(); sn_reset_column(); }

<INITIAL>"<?" { /* HTML -> PHP mode */
    yy_push_state(PHP); /*BEGIN(PHP);*/
    sn_advance_column(2);
}

<PHP,FUNCTION>"?>" { /* PHP -> HTML mode */
    yy_pop_state(); /*BEGIN(INITIAL);*/
    sn_advance_column(2);
}

<PHP,FUNCTION>"/*"		{ /* A C style multi-line comment, just like this! */
    yy_push_state(COMMENT); /*BEGIN(COMMENT);*/
    sn_advance_column(2);
    LongStringInit(&cbuff,0);
    cbuff_start_line = sn_line();
    cbuff_start_column = sn_column();
}

<PHP,FUNCTION>{ws}*\/\/.*\n { /* A C++ style line comment -> // This function is quite nice. <- */
  char * x = (char *) yytext;
  char * y = x + yyleng - 1;

  while (*x != '/') { x++; }
  x++;
  assert(*x == '/');
  x++;
  sn_advance_column(x - yytext);

  assert(*y == '\n');
  *y = '\0';

  sn_insert_comment(
    /* classname */ NULL,
    /* funcname */ NULL,
    sn_current_file(),
    x,
    sn_line(),
    sn_column());

  sn_advance_line();
  sn_reset_column();
}

<PHP>\${symbol}{array-notation}? {
  char * x = (char *) yytext;

  /* Trim leading $ off the front of the symbol */

  assert(*x == '$');
  x++;

  /* When reading a global variable, one simply needs to add an xref
     for the symbol in question. */

  result = sn_insert_xref(SN_REF_TO_GLOB_VAR,
                 SN_REF_TO_GLOB_VAR, /* PASSING 0 causes function to fail, what is the global var setting ??? */
		                     /* This SN_REF_TO_GLOB_VAR thing does not jive with the docs */
                 SN_REF_SCOPE_GLOBAL,
                 NULL,
                 "GLOBAL", /* FIXME: A global var would not have a current function ??? */
                 NULL,
                 NULL,
                 x, /* refsymbol */
                 NULL,
                 sn_current_file(),
                 sn_line(),
                 SN_REF_READ);

  assert(result == 0);
  sn_advance_column(yyleng);
}

<PHP>\${symbol}{ws}*= {
  char * x = (char *) yytext;
  char * y;
  
  /* Trim the '$' off the front of the string */
  assert(*x == '$');
  x++;
  sn_advance_column(1);

  /* Trim the '=' off the end of the string */
  y = x + strlen(x) - 1;
  assert(*y == '=');
  y--;

  /* Trim white space off end of string */

  for (; y>x ; y--) {
      if ((*y == '\t') || (*y == ' ')) {
          /* no-op */
      } else {
          break;
      }
  }

  y++;
  *y = 0;

  /* Setting a global variable should emit
     a symbol declaration. I am not really
     sure if we should emit just one symbol
     declaration, or multiple ones. Multiple
     ones breaks xref but it does present
     the symbol multiple times in the
     file's symbol list which would be
     nice if it worked correctly */

  result = sn_insert_symbol(SN_GLOB_VAR_DEF,
                            NULL,
			    x,
			    sn_current_file(),
			    sn_line(), sn_column(),
			    sn_line(), sn_column() + (y - x),
			    0,
			    NULL,
			    NULL,
			    NULL,
			    NULL,
			    sn_line(), sn_column(),
			    sn_line(), sn_column() + (y - x));

  assert(result == 0);

  /* Add a read xref for this symbol name */

  result = sn_insert_xref(SN_REF_TO_GLOB_VAR,
                 SN_REF_TO_GLOB_VAR, /* PASSING 0 causes function to fail, what is the global var setting ??? */
		                     /* This SN_REF_TO_GLOB_VAR thing does not jive with the docs */
                 SN_REF_SCOPE_GLOBAL,
                 NULL,
                 "GLOBAL", /* FIXME: A global var would not have a current function ??? */
                 NULL,
                 NULL,
                 x, /* refsymbol */
                 NULL,
                 sn_current_file(),
                 sn_line(),
                 SN_REF_WRITE);

  assert(result == 0);
  sn_advance_column(yyleng);
}

<PHP>^{ws}*"function"{wsn}+{symbol}{wsn}*\([^\)]*\){wsn}+\{ {
  char * x = (char *) yytext;
  char * y;
  char * tmp1, * tmp2;

  current_function_line_start = sn_line();
  current_function_column_start = sn_column();

  /* Skip {ws} leading up to function token */
  for ( ; *x == ' ' || *x == '\t' ; x++) {}

  /* Skip the "function" token */
  assert(*x == 'f');
  x += strlen("function");
  sn_advance_column(x - yytext);

  /* Skip any {wsn} chars leading up to the function name */
  for ( ; *x ; x++) {
      if (*x == '\n') {
          sn_advance_line();
          sn_reset_column();
      } else if (*x == ' ' || *x == '\t') {
          sn_advance_column(1);
      } else {
          break;
      }
  }

  /* x now points to the start of the function name, point
   * y at the char just after the function name
   */

  for (y=x ; *y ; y++) {
      if (*y == '(' || *y == '\t' || *y == ' ' || *y == '\n') {
          break;
      }
  }

  assert((y - x) < MAX_SIZE);
  strncpy(current_function, x, (y - x));
  current_function[y - x] = 0;

  current_function_highlight_line = sn_line();
  current_function_highlight_column_start = sn_column();
  current_function_highlight_column_end = sn_column() + (y - x);

  /* Save function arguments in current_function_args */

  for ( ; *y != '(' ; y++) {}
  tmp1 = y + 1;
  tmp2 = current_function_args;

  for (; *tmp1 != ')'; ) {
    /* Start argument variable name */
    if (*tmp1 == '$') {
        for (tmp1++; *tmp1 != ')'; tmp1++) {
            /*fprintf(stderr, "var loop '%s'\n", tmp1);*/
            if (*tmp1 == ',' || *tmp1 == '=' || *tmp1 == ' ' ||
                    *tmp1 == '\t' || *tmp1 == '\n') {
                break;
            } else {
                /*fprintf(stderr, "appended '%c'\n", *tmp1);*/
                *tmp2++ = *tmp1;
            }

            assert((tmp2 - current_function_args) < MAX_SIZE);
        }
    } else if (*tmp1 == ',') {
        /*fprintf(stderr, "append ',' in outer loop '%s'\n", tmp1);*/
        *tmp2++ = ',';
        tmp1++;
    } else {
        /*fprintf(stderr, "skip '%s'\n", tmp1);*/
        tmp1++;
    }
  }
  *tmp2 = 0;
  /*fprintf(stderr, "current_function args are \"%s\"\n", current_function_args);*/

  /* Advance from function name up to just past the '{' character */
  for (; *x; x++) {
    if (*x == '\n') {
        sn_advance_line();
        sn_reset_column();
    } else {
        sn_advance_column(1);
    }
  }

  /*fprintf(stderr, "advanced to end of match at %d.%d\n", sn_line(), sn_column());*/

  current_function_brace_count = 1;

  yy_push_state(FUNCTION); /*BEGIN(FUNCTION);*/
}

<PHP,FUNCTION>{wsn}*{symbol}{wsn}*\( {
  char * x = (char *) yytext;
  char * y;
  char saved;

  /* Skip {wsn} leading up to symbol */

  for ( ; *x ; x++) {
      if (*x == '\n') {
          sn_advance_line();
          sn_reset_column();
      } else if (*x == ' ' || *x == '\t') {
          sn_advance_column(1);
      } else {
          break;
      }
  }

  for (y=x; *y ; y++) {
      if (*y == '(' || *y == ' ' || *y == '\t' || *y == '\n') {
          saved = *y;
          *y = 0;
          break;
      }
  }

  /* x is now the name of the function to be invoked */

  result = sn_insert_xref(SN_REF_TO_FUNCTION,
                 SN_FUNC_DEF,
                 SN_REF_SCOPE_GLOBAL,
                 NULL,
                 current_function,
                 NULL,
                 NULL,
                 x,
                 NULL, 
                 sn_current_file(),
                 sn_line(),
                 SN_REF_PASS);

  assert(result == 0);

  *y = saved;

  for ( ; *x ; x++) {
      if (*x == '\n') {
          sn_advance_line();
          sn_reset_column();
      } else {
          sn_advance_column(1);
      }
  }
}

<COMMENT>{
  [^\*\n]* {
    #if COMMENT_DUMP
    fprintf(stderr, "comment(1) \"%s\", %d\n", yytext, yyleng);
    #endif
    cbuff.append( &cbuff,
        yytext, yyleng );
    sn_advance_column(yyleng);
    }
  [^\*\n]*\n {
    #if COMMENT_DUMP
    fprintf(stderr, "comment(2) \"%s\", %d\n", yytext, yyleng);
    #endif

    cbuff.append( &cbuff,
        yytext, yyleng );
    sn_advance_line();
    sn_reset_column();
    }
  \*+[^\*/\n]* {
    #if COMMENT_DUMP
    fprintf(stderr, "comment(3) \"%s\", %d\n", yytext, yyleng);
    #endif

    cbuff.append( &cbuff,
        yytext, yyleng );
    sn_advance_column(yyleng);
  }
  \*+[^\*/\n]*\n {
    #if COMMENT_DUMP
    fprintf(stderr, "comment(4) \"%s\", %d\n", yytext, yyleng);
    #endif

    cbuff.append( &cbuff,
        yytext, yyleng );
    sn_advance_line();
    sn_reset_column();
  }
  "*"+"/" {
    char * comment;

    /* Include multiple '*' characters in the comment */
    if (yyleng > 2) {
        comment = (char *) yytext + yyleng - 1;
        assert(*comment == '/');
        comment--;
        assert(*comment == '*');
        *comment = '0';
        yyleng -= 2;
        cbuff.append( &cbuff,
            yytext, yyleng );
    }

    comment = cbuff.buf;

    #if COMMENT_DUMP
    fprintf(stderr, "eating comment \"%s\"\n", comment);
    #endif

    sn_insert_comment(
        /* classname */ NULL,
        /* funcname */ NULL,
        sn_current_file(),
        comment,
        cbuff_start_line,
        cbuff_start_column);

    cbuff.free(&cbuff);
    sn_advance_column(yyleng);
    yy_pop_state(); /* BEGIN(INITIAL); */
  }
}

%%
  
void
reset()
{
  sn_reset_line();
  sn_reset_column();
}

int
main(int argc, char *argv[])
{
  return sn_main(argc, argv, group, &yyin, yylex, reset);
}
