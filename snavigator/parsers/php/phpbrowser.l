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

static char current_function[512] = {"GLOBAL"}; /* "" causes assert core dump */
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

%x COMMENT
%x PHP

ws		[ \t]
symbol		[a-zA-Z0-9_]+
array-notation  (\[\]|\[\"{symbol}\"\]|\[\'{symbol}\'\])

%%

<INITIAL,PHP>.		{ sn_advance_column(yyleng); /* eat text */ }

<PHP>\\\$		{ sn_advance_column(yyleng); /* ignore \$ */ }


<INITIAL,PHP>\n		{ sn_advance_line(); sn_reset_column(); }

<INITIAL>"<?" { /* HTML -> PHP mode */
    BEGIN(PHP);
    sn_advance_column(2);
}

<PHP>"?>" { /* PHP -> HTML mode */
    BEGIN(INITIAL);
    sn_advance_column(2);
}

<PHP>"/*"		{ /* A C style multi-line comment, just like this! */
    BEGIN(COMMENT);
    sn_advance_column(2);
    LongStringInit(&cbuff,0);
    cbuff_start_line = sn_line();
    cbuff_start_column = sn_column();
}

<PHP>{ws}*\/\/.*\n { /* A C++ style line comment -> // This function is quite nice. <- */
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

<PHP>^{ws}*"function"{ws}+{symbol}\( {
  char * x = (char *) yytext;
  char * y;

  /* Skip ahead to the '(' character */

  for ( ; *x ; x++) {
      if (*x == '(') {
          *x = 0;
          y = x;
          x--;
          break;
      }
  }

  /* Walk back to the start of the function name */

  for ( ; 1 ; x--) {
      if (*x == ' ' ||
          *x == '\t') {
          x++;
          break;
      }
  }

  /* x now points to the start of the function name */

  sn_advance_column(x - yytext);

  result = sn_insert_symbol(SN_FUNC_DEF, NULL, x, sn_current_file(), 
		   sn_line(), sn_column(),
                   sn_line(), sn_column() + (y - x),
                   0 /* attribute */, NULL /* return type */,
                   NULL /* argument types */, NULL /* argument names */,
                   NULL /* comment */,
                   sn_line(), sn_column(),
                   sn_line(), sn_column() + (y - x) );

  assert(result == 0);

  /* FIXME : need to declare function too, can that be done at the
     same time or is this a bug in SN ?? */

  sn_advance_column(y - x);
}

<PHP>{symbol}\( {
  char * x = (char *) yytext;
  char * y;

  for (y=x; *y ; y++) {
      if (*y == '(') {
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

  sn_advance_column(yyleng);
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
    BEGIN(INITIAL);
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
