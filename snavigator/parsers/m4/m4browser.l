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

/*
 * m4browser.l
 *
 * Copyright (C) 2000 Red Hat Inc.
 *
 * Description:
 * Lex input file for an M4 macro language processor.
 */

%{

#include <ctype.h>
#include <stdio.h>
#include "snptools.h"
#include "lexinput.h"

#undef yywrap
#define YY_SKIP_YYWRAP

#undef YY_INPUT
#define YY_INPUT(buf,r,ms) (r = sn_encoded_input(buf, ms))

static char group[] = "m4";

#define MAX_SIZE 512

static char current_function[MAX_SIZE] = {""};

/* line number where highlight starts and ends */
static int  current_function_highlight_line;

/* in "AC_DEFUN([MACRO], [])" column of 'M' in "MACRO" */
static int  current_function_highlight_column_start;

/* in "AC_DEFUN([MACRO], [])" column of 'O' in "MACRO" */
static int  current_function_highlight_column_end;

/* line where macro declaration begins */
static int  current_function_line_start;

/* line where closing paren of macro appears */
static int  current_function_line_end;

/* in "AC_DEFUN([MACRO], [])" column of 'A' in "AC_DEFUN" */
static int  current_function_column_start;

/* in "AC_DEFUN([MACRO], [])" column of ')' in "[])" */
static int  current_function_column_end;

static int  current_function_paren_count;

static int result;

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

void dump_function_definition();

void matched_pattern(char * pattern, char * text);

static void emit_var_access(char * varname, int acc);
static void emit_func_call(char * funcname);

%}

%x FUNCTION

ws		[ \t]
symbol		[a-zA-Z_][a-zA-Z0-9_]+
quoted-symbol	({symbol}|\[{symbol}\])
func-decl	("AU_DEFUN"|"AC_DEFUN"|"m4_define")
macro-name	[A-Z][A-Z0-9_]+

%%

<FUNCTION>\( {
  matched_pattern("(", yytext);
  sn_advance_column(yyleng);
  current_function_paren_count++;
}

<FUNCTION>\) {
  matched_pattern(")", yytext);
  sn_advance_column(yyleng);
  assert(current_function_paren_count > 0);
  current_function_paren_count--;
  if (current_function_paren_count == 0) {
    current_function_line_end = sn_line();
    current_function_column_end = sn_column();

    dump_function_definition();

    BEGIN(INITIAL);
  }
}

<INITIAL,FUNCTION>.		{
  matched_pattern(".", yytext);
  sn_advance_column(yyleng); /* eat text */
}

<INITIAL,FUNCTION>\\\$		{
  matched_pattern("\\$", yytext);
  sn_advance_column(yyleng); /* ignore \$ */
}

<INITIAL,FUNCTION>\n		{
  matched_pattern("\\n", yytext);
  sn_advance_line();
  sn_reset_column();
}

<INITIAL,FUNCTION>{ws}*("#"|"dnl"{ws}).*\n {
  char * x = (char *) yytext;
  char * y = x + yyleng - 1;

  matched_pattern("{ws}*(#|dnl{ws}).*\\n", yytext);

  while (*x == '\t' || *x == ' ') { x++; }

  /* Insert comment highlight */

  sn_highlight(SN_HIGH_COMMENT,
      sn_line(),
      sn_column() + (x - yytext),
      sn_line(),
      sn_column() + yyleng - 1);

  if (*x == '#') {
    x++;
  } else if (*x == 'd') {
    x += 3;
  }
  while (*x == '\t' || *x == ' ') { x++; }
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

<INITIAL,FUNCTION>"$"({symbol}|"{"{symbol}"}") {
  char * x = (char *) yytext;

  matched_pattern("${symbol}", yytext);

  /* Add global var highlight */
  
  sn_highlight(SN_HIGH_VAR_GLOBAL,
      sn_line(), sn_column(),
      sn_line(), sn_column() + yyleng);

  /* Trim leading $ off the front of the symbol
   * and ignore '{' and '}' characters if there */

  assert(*x == '$');
  x++;
  if (*x == '{') {
    x++;
    *(yytext + yyleng - 1) = '\0';
  }

  emit_var_access(x, SN_REF_READ);
  sn_advance_column(yyleng);
}

<INITIAL,FUNCTION>"${#"{symbol}"}" {
  char * x = (char *) yytext;

  matched_pattern("$\\{#{symbol}\\}", yytext);

  /* Trim leading ${# of the front */

  assert(*x == '$');
  x++;
  assert(*x == '{');
  x++;
  assert(*x == '#');
  x++;
  *(yytext + yyleng - 1) = '\0';

  sn_highlight(SN_HIGH_VAR_GLOBAL,
      sn_line(), sn_column() + (x - yytext),
      sn_line(), sn_column() + (yyleng - 1));

  emit_var_access(x, SN_REF_READ);
  sn_advance_column(yyleng);
}

<INITIAL,FUNCTION>"${"{symbol}("-"|":-"|"+"|":+"|"?"|":?"|":"|"#"|"##"|"%"|"%%"|"="|":="|"/")[^\}]*"}" {
  char * x = (char *) yytext;
  char * y;
  int is_read_write = 0;

  matched_pattern("$\\{{symbol}(-|+|?|=|#|%){symbol}\\}", yytext);

  /* Trim leading $ off the front of the symbol
   * and ignore '{' and '}' characters if there */

  assert(*x == '$');
  x++;
  assert(*x == '{');
  x++;
  *(yytext + yyleng - 1) = '\0';

  /* Find separator symbol */
  
  for (y=x ; *y ; y++) {
      if (*y == '-' || 
          *y == '+' ||
          *y == '?' ||
          *y == '#' ||
          *y == '%' ||
          *y == '/') {
          break;
      }
      if ((*y == '=') ||
              ((*y == ':') &&
               (*(y+1) == '='))) {
          is_read_write = 1;
          break;
      }
      if (*y == ':') {
          break;
      }
  }
  *y = 0;

  sn_highlight(SN_HIGH_VAR_GLOBAL,
      sn_line(), sn_column() + (x - yytext),
      sn_line(), sn_column() + (y - yytext));

  emit_var_access(x, SN_REF_READ);
  if (is_read_write) {
    emit_var_access(x, SN_REF_WRITE);
  }
  sn_advance_column(yyleng);
}

<INITIAL,FUNCTION>{symbol}= {
  char * x = (char *) yytext;
  char * y;
  
  matched_pattern("{symbol}=", yytext);

  /* Add global var highlight */

  sn_highlight(SN_HIGH_VAR_GLOBAL,
      sn_line(), sn_column(),
      sn_line(), sn_column() + yyleng - 1);

  /* Trim the '=' off the end of the string */
  
  for (y=x; *y ; y++) {
      if (*y == '=') {
          *y = 0;
	  break;
      }
  }

  emit_var_access(x, SN_REF_WRITE);
  sn_advance_column(yyleng);
}

<INITIAL>^{ws}*{func-decl}\({ws}*{quoted-symbol}{ws}*, {
  char * x = (char *) yytext;
  char * y;

  matched_pattern("^{ws}*{func-decl}\({ws}*{quoted-symbol}{ws}*,", yytext);

  current_function_line_start = sn_line();
  current_function_column_start = sn_column();

  /* Skip {ws} */

  for ( ; *x ; x++) {
      if (*x != ' ' &&
          *x != '\t') {
          break;
      }
  }

  /* Find '(' character after AC_DEFUN */
  
  for (y=x ; *y ; y++) {
      if (*y == '(') {
          break;
      }
  }

  /* Highlight AC_DEFUN as a keyword, not a macro invocation */

  sn_highlight(SN_HIGH_KEYWORD,
      sn_line(), sn_column() + (x - yytext),
      sn_line(), sn_column() + (y - yytext));

  x = y;
  x++;

  /* Skip {ws} and optional [ */

  for ( ; *x ; x++) {
      if (*x != ' ' &&
          *x != '\t' &&
          *x != '[') {
          break;
      }
  }

  /* x now points to the start of the macro name, find the end */

  sn_advance_column(x - yytext);

  for (y=x ; *y ; y++) {
      if (*y == ' ' ||
          *y == '\t' ||
          *y == ']' ||
          *y == ',') {
          *y = 0;
          break;
      }
  }

  /* x is now the name of the macro that is being defined */

  strncpy(current_function, x, MAX_SIZE-1);

  current_function_highlight_line = sn_line();
  current_function_highlight_column_start = sn_column();
  current_function_highlight_column_end = sn_column() + (y - x);

  sn_advance_column((yytext + yyleng) - x);

  current_function_paren_count = 1;

  BEGIN(FUNCTION);
}


<INITIAL,FUNCTION>{symbol}\( {
  char * x = (char *) yytext;
  char * y;

  matched_pattern("{symbol}(", yytext);

  for (y=x; *y ; y++) {
      if (*y == '(') {
          *y = 0;
          break;
      }
  }

  emit_func_call(x);

  sn_highlight(SN_HIGH_FUNCTION,
      sn_line(), sn_column() + (x - yytext),
      sn_line(), sn_column() + (y - yytext));

  sn_advance_column(yyleng);

  current_function_paren_count++;
}

<INITIAL,FUNCTION>^{macro-name}$ {
  matched_pattern("^{macro-name}$", yytext);

  emit_func_call(yytext);

  sn_highlight(SN_HIGH_FUNCTION,
      sn_line(), sn_column(),
      sn_line(), sn_column() + yyleng);

  sn_advance_column(yyleng);
}

<INITIAL,FUNCTION>"AC_REQUIRE(["{symbol}"])" {
  char * x = (char *) yytext;
  char * y;

  matched_pattern("AC_REQUIRE([{symbol}])", yytext);

  /* Insert a function call xref for the AC_REQUIRE call */

  for (y=x; *y ; y++) {
      if (*y == '(') {
          *y = 0;
          break;
      }
  }

  emit_func_call(x);

  sn_highlight(SN_HIGH_FUNCTION,
      sn_line(), sn_column() + (x - yytext),
      sn_line(), sn_column() + (y - yytext));

  /* Insert a function call xref for the macro argument */

  for (y+=2, x=y; *y ; y++) {
      if (*y == ']') {
          *y = 0;
          break;
      }
  }

  emit_func_call(x);

  sn_highlight(SN_HIGH_FUNCTION,
      sn_line(), sn_column() + (x - yytext),
      sn_line(), sn_column() + (y - yytext));

  sn_advance_column(yyleng);
}

%%

void dump_function_definition() {
    result = sn_insert_symbol(SN_FUNC_DEF, NULL,
            current_function,
            sn_current_file(), 
            current_function_line_start, current_function_column_start,
            current_function_line_end, current_function_column_end,
            0 /* attribute */,
            NULL /* return type */,
            NULL /* argument types */,
            NULL /* argument names */,
            NULL /* comment */,
            current_function_highlight_line,
            current_function_highlight_column_start,
            current_function_highlight_line,
            current_function_highlight_column_end );

    assert(result == 0);

    current_function[0] = '\0';
}

void emit_var_access(char * varname, int acc) {
  int ref_from_scope_type;

  if (current_function[0] == '\0') {
    ref_from_scope_type = SN_GLOBAL_NAMESPACE;
  } else {
    ref_from_scope_type = SN_FUNC_DEF;
  }

  result = sn_insert_xref(SN_REF_TO_GLOB_VAR,
                 ref_from_scope_type,
                 SN_REF_SCOPE_GLOBAL,
                 NULL,
                 (current_function[0] == '\0') ? NULL : current_function,
                 NULL,
                 NULL,
                 varname, /* refsymbol */
                 "UNDECLARED",
                 sn_current_file(),
                 sn_line(),
                 acc);

  assert(result == 0);
}

void emit_func_call(char * funcname) {
  int ref_from_scope_type;

  if (current_function[0] == '\0') {
    ref_from_scope_type = SN_GLOBAL_NAMESPACE;
  } else {
    ref_from_scope_type = SN_FUNC_DEF;
  }

  result = sn_insert_xref(SN_REF_TO_FUNCTION,
                 ref_from_scope_type,
                 SN_REF_SCOPE_GLOBAL,
                 NULL,
                 (current_function[0] == '\0') ? NULL : current_function,
                 NULL,
                 NULL,
                 funcname,
                 NULL, 
                 sn_current_file(),
                 sn_line(),
                 SN_REF_PASS);

  assert(result == 0);
}

/* Helper method that will print matches as they are made */

void matched_pattern(char * pattern, char * text) {
    char * mode = "UNKNOWN";
    int imode = YY_START;

    if (imode == INITIAL) {
        mode = "INITIAL";
    } else if (imode == FUNCTION) {
        mode = "FUNCTION";
    }

    if (0) {
    fprintf(stderr, "Matched \"%s\", with text \"%s\", in mode \"%s\" (%d.%d)\n",
        pattern, text, mode, sn_line(), sn_column());
    }
}

void
reset()
{
  sn_reset_line();
  sn_reset_column();
  sn_reset_encoding();
}

int
main(int argc, char *argv[])
{
  return sn_main(argc, argv, group, &yyin, yylex, reset);
}

