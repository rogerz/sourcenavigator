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
#include <stdlib.h>
#include <stdio.h>
#include "snptools.h"
#include "lexinput.h"
#include "longstr.h"
#include "srchtbl.h"
#include "tcl.h"

#undef YY_INPUT
#define YY_INPUT(buf,r,ms) (r = sn_encoded_input(buf, ms))

#undef yywrap
#define YY_SKIP_YYWRAP

typedef enum {
    OPEN_PAREN,    /* '(' */
    CLOSE_PAREN,   /* ')' */
    OPEN_BRACE,    /* '{' */
    CLOSE_BRACE,   /* '}' */
    OPEN_BRACKET,  /* '[' */
    CLOSE_BRACKET, /* ']' */
    SEMICOLON,     /* ';' */
    COMMA,         /* ',' */

    VARIABLE,      /* $var */
    ASSIGNMENT_OPERATOR, /* '=' */
    ASSIGNMENT_OPERATORS, /* = -= += and so on */
    COMPARISON_OPERATORS, /* == === != >= and so on */

    SOMEWORD,          /* a sequence of text */
    KEYWORD,       /* a key sequence in this language */
    FUNCTION_KEYWORD, /* The keyword "function" */
    GLOBAL_KEYWORD, /* The keyword "global" */
    INCLUDE_KEYWORD, /* The keyword "include" */

    DOUBLE_QUOTED_STRING, /* "I am a double quoted string" */
    SINGLE_QUOTED_STRING,  /* 'I am a double quoted string' */

    UNKNOWN        /* Some unknown or unimportant text */
} TokenType;

typedef struct Token {
    TokenType type;
    char * strval; /* String value of token, NULL if single character */
    long start_line;
    int start_column;
    long end_line;
    int end_column;
    struct Token* next;
} Token;

/* Uncomment this to see debug output in token parsing stage */
/* #define TOKEN_DEBUG */

#ifdef TOKEN_DEBUG
FILE* tokenout = NULL;
#endif

Token* tokens_head = NULL;
Token* tokens_tail = NULL;
int token_index = 0; /* current offset from original token_head */

static char group[] = "php";

#define MAX_SIZE 512

/* FIXME: Can't pass NULL or "" as current function (core dump) */
static char* current_function = (char *) NULL;
static char* current_function_args = (char *) NULL;

/* line number where highlight starts and ends */
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

static SearchTable * global_var_table = (SearchTable *) NULL;

/* Stores the contents of a special processing mode over
 * multiple lines/rules.
 */
LongString mode_buff;
long mode_start_line;
int mode_start_column;

#define COMMENT_DUMP 0
#define DQSTRING_DUMP 0
#define SQSTRING_DUMP 0
#define HDSTRING_DUMP 0

static YY_BUFFER_STATE original_buffer;

static char* heredoc_id = (char *) NULL; /* End marker for a heredoc string */

static void matched_pattern(char * pattern, char * text);

static char* modestring();

static void FreeGlobalEntry(SearchEntry *entry);

void FreeToken(Token* tok);
void free_head_token();
void append_token(TokenType type,
                  char* strval,
                  long start_line,
                  int start_column,
                  long end_line,
                  int end_column);

char * TokenTypeToString(Token *tok);

void emit_function_declaration();
void emit_comment();
void emit_dqstring();
void emit_sqstring();
void emit_hdstring();

int yywrap() { return 1; }

%}

%x COMMENT
%x DQSTRING
%s SQSTRING
%s HDSTRING
%x PHP
%x TOKEN

ws		[ \t]
wsn		[ \t\n]
symbol		[a-zA-Z0-9_]+

varname		{symbol}
someword	{symbol}
token		{symbol}
heredoc-id	{symbol}

assignment-operators	("+="|"-="|"*="|"/="|".="|"%="|"&="|"|="|"^="|"<<="|">>=")
comparison-operators	("=="|"==="|"!="|"!=="|"<>"|"<"|">"|"<="|">=")

any-tokens	(" "|" "{symbol}" "|" "{symbol}" "({symbol}" ")*)

include-keywords	("include"|"include_once"|"require"|"require_once")

keywords	("and"|"or"|"xor")


dq-string-token  "DOUBLE_QUOTED_STRING"
sq-string-token  "SINGLE_QUOTED_STRING"
string-token  ({dq-string-token}|{sq-string-token})
literal-token ({someword}|{dq-string-token}|{sq-string-token})

function-argument-token  ("VARIABLE"|"VARIABLE ASSIGNMENT_OPERATOR "{literal-token})
function-argument-tokens (" "|" "{function-argument-token}" "({function-argument-token}" ")*)

variable-tokens	(" VARIABLE "|" VARIABLE "("VARIABLE ")+)


var-array-index-token ("SOMEWORD"|"DOUBLE_QUOTED_STRING"|"SINGLE_QUOTED_STRING"|"VARIABLE")
var-array-empty-tokens "OPEN_BRACKET CLOSE_BRACKET"
var-array-index-tokens "OPEN_BRACKET "{var-array-index-token}" CLOSE_BRACKET")
var-array-token ({var-array-empty-tokens}|{var-array-index-tokens}

var-array-tokens (" "|" "{var-array-token}" "({var-array-token}" ")*)

%%

<INITIAL>"<?" { /* HTML -> PHP mode */
    matched_pattern("<?", yytext);
    BEGIN(PHP);
    sn_advance_column(2);
}

<PHP>"?>" { /* PHP -> HTML mode */
    matched_pattern("?>", yytext);
    BEGIN(INITIAL);
    sn_advance_column(2);
}

<PHP>{ws}*("//"|"#").*\n {
  char * x = (char *) yytext;
  char * y;
  int newline = 0;

  matched_pattern("{ws}*(//|#).*\\n", yytext);

  while (*x == ' ' || *x == '\t') { x++; }
  assert(*x == '#' || (*x == '/' && *(x+1) == '/'));
  if (*x == '#') {
    x++;
  } else if (*x == '/') {
    x += 2;
  }
  sn_advance_column(x - yytext);

  /* Check for ?> in the text leading up to the newline.
   * PHP has this special rule where a line comment
   * can be terminated before the newline.
   */

  for (y=x; *y; y++) {
    if (*y == '?' && *(y+1) == '>') {
        int offset = (int) (y - yytext);
        yyless(offset); /* send everything after ?> back */
        break;
    }
  }

  y = yytext + yyleng - 1;
  if (*y == '\n') {
    newline = 1;
    *y = 0;
  }

  sn_insert_comment(
    /* classname */ NULL,
    /* funcname */ NULL,
    sn_current_file(),
    x,
    sn_line(),
    sn_column());

  if (newline) {
    sn_advance_line();
    sn_reset_column();
  } else {
    sn_advance_column(yyleng - (x - yytext));
  }
}

<PHP>"/*"		{ /* A C style multi-line comment, just like this! */
    matched_pattern("/*", yytext);
    BEGIN(COMMENT);
    sn_advance_column(2);
    LongStringInit(&mode_buff,0);
    mode_start_line = sn_line();
    mode_start_column = sn_column();
}

<COMMENT>{
  [^\*\n]* {
    matched_pattern("[^\\*\\n]*", yytext);

    #if COMMENT_DUMP
    fprintf(stderr, "comment(1) \"%s\", %d\n", yytext, yyleng);
    #endif
    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_column(yyleng);
  }
  [^\*\n]*\n {
    matched_pattern("[^\\*\\n]*\\n", yytext);

    #if COMMENT_DUMP
    fprintf(stderr, "comment(2) \"%s\", %d\n", yytext, yyleng);
    #endif

    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_line();
    sn_reset_column();
  }
  \*+[^\*/\n]* {
    matched_pattern("\\*+[^\\*/\\n]*", yytext);

    #if COMMENT_DUMP
    fprintf(stderr, "comment(3) \"%s\", %d\n", yytext, yyleng);
    #endif

    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_column(yyleng);
  }
  \*+[^\*/\n]*\n {
    matched_pattern("\\*+[^\\*/\\n]*\\n", yytext);

    #if COMMENT_DUMP
    fprintf(stderr, "comment(4) \"%s\", %d\n", yytext, yyleng);
    #endif

    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_line();
    sn_reset_column();
  }
  "*"+"/" {
    matched_pattern("\\*+/", yytext);

    /* Include multiple '*' characters in the comment */
    if (yyleng > 2) {
        int to_append = yyleng;
        char *comment = (char *) yytext + yyleng - 1;
        assert(*comment == '/');
        comment--;
        assert(*comment == '*');
        *comment = '0';
        to_append -= 2;
        mode_buff.append( &mode_buff,
            yytext, to_append );
    }

    emit_comment();
    sn_advance_column(yyleng);
    BEGIN(PHP);
  }
}

<PHP>\"\" {
  matched_pattern("\\\"\\\"", yytext);
  /* FIXME: Can we pass NULL instead of "" after length issues worked out ? */
  append_token(DOUBLE_QUOTED_STRING, "",
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<PHP>\" {
  matched_pattern("\\\"", yytext);
#if DQSTRING_DUMP
  fprintf(stderr, "dqstring started at (%d.%d)\n", sn_line(), sn_column());
#endif
  LongStringInit(&mode_buff,0);
  mode_start_line = sn_line();
  mode_start_column = sn_column();
  sn_advance_column(yyleng);
  BEGIN(DQSTRING);
}

<DQSTRING>{
  [^\"\n\\]* {
    matched_pattern("[^\\\"\\n\\\\]*", yytext);

    #if DQSTRING_DUMP
    fprintf(stderr, "dqstring(1) \"%s\", %d\n", yytext, yyleng);
    #endif
    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_column(yyleng);
  }
  [^\"\n\\]*\n {
    matched_pattern("[^\\\"\\n\\\\]*\\n", yytext);

    #if DQSTRING_DUMP
    fprintf(stderr, "dqstring(2) \"%s\", %d\n", yytext, yyleng);
    #endif
    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_line();
    sn_reset_column();
  }
  (\\\")+[^\"/\n\\]* {
    matched_pattern("\\\"+[^\\\"/\\n\\\\]*", yytext);

    #if DQSTRING_DUMP
    fprintf(stderr, "dqstring(3) \"%s\", %d\n", yytext, yyleng);
    #endif

    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_column(yyleng);
  }
  (\\\")+[^\"\n\\]*\n {
    matched_pattern("\\\"+[^\\\"\\n\\\\]*\\n", yytext);

    #if DQSTRING_DUMP
    fprintf(stderr, "dqstring(4) \"%s\", %d\n", yytext, yyleng);
    #endif

    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_line();
    sn_reset_column();
  }
  \\ {
    matched_pattern("\\\\", yytext);

    #if DQSTRING_DUMP
    fprintf(stderr, "dqstring(5) \"%s\", %d\n", yytext, yyleng);
    #endif

    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_column(yyleng);
  }
  \" {
    matched_pattern("\\\"", yytext);
    sn_advance_column(yyleng);
    emit_dqstring();
    BEGIN(PHP);
  }
}

<PHP>\'\' {
  matched_pattern("\\\'\\\'", yytext);
  /* FIXME: Can we pass NULL instead of "" after length issues worked out ? */
  append_token(SINGLE_QUOTED_STRING, "",
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<PHP>\' {
  matched_pattern("\\\'", yytext);
#if SQSTRING_DUMP
  fprintf(stderr, "sqstring started at (%d.%d)\n", sn_line(), sn_column());
#endif
  LongStringInit(&mode_buff,0);
  mode_start_line = sn_line();
  mode_start_column = sn_column();
  sn_advance_column(yyleng);
  BEGIN(SQSTRING);
}

<SQSTRING>{
  [^\'\n\\]* {
    matched_pattern("[^\\\'\\n\\\\]*", yytext);

    #if SQSTRING_DUMP
    fprintf(stderr, "sqstring(1) \"%s\", %d\n", yytext, yyleng);
    #endif
    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_column(yyleng);
  }
  [^\'\n\\]*\n {
    matched_pattern("[^\\\'\\n\\\\]*\\n", yytext);

    #if SQSTRING_DUMP
    fprintf(stderr, "sqstring(2) \"%s\", %d\n", yytext, yyleng);
    #endif

    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_line();
    sn_reset_column();
  }
  (\\\')+[^\'/\n\\]* {
    matched_pattern("\\\'+[^\\\'/\\n\\\\]*", yytext);

    #if SQSTRING_DUMP
    fprintf(stderr, "sqstring(3) \"%s\", %d\n", yytext, yyleng);
    #endif

    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_column(yyleng);
  }
  (\\\')+[^\'\n\\]*\n {
    matched_pattern("\\\'+[^\\\'\\n\\\\]*\\n", yytext);

    #if SQSTRING_DUMP
    fprintf(stderr, "sqstring(4) \"%s\", %d\n", yytext, yyleng);
    #endif

    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_line();
    sn_reset_column();
  }
  \\ {
    matched_pattern("\\\\", yytext);

    #if SQSTRING_DUMP
    fprintf(stderr, "sqstring(5) \"%s\", %d\n", yytext, yyleng);
    #endif

    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_column(yyleng);
  }
  \' {
    matched_pattern("\\\'", yytext);
    sn_advance_column(yyleng);
    emit_sqstring();
    BEGIN(PHP);
  }
}

<PHP>"<<<"{ws}*{heredoc-id}\n {
  char *x = yytext;
  char *y;

  matched_pattern("<<<{ws}*{heredoc-id}\\n", yytext);

  mode_start_line = sn_line();
  mode_start_column = sn_column();

  x += 3;
  while(*x == ' ' || *x == '\t') { x++; }
  y = x;
  while(*y != '\n') { y++; }
  *y = 0;

  LongStringInit(&mode_buff,0);
  mode_buff.append( &mode_buff,
          x, -1 );
  mode_buff.append( &mode_buff,
          ";\n", -1 );
  heredoc_id = SN_StrDup(mode_buff.buf);
  mode_buff.free(&mode_buff);

#if HDSTRING_DUMP
  fprintf(stderr, "hdstring started at (%d.%d)\n", sn_line(), sn_column());
  fprintf(stderr, "hdstring id is \"%s\"\n", heredoc_id);
#endif

  LongStringInit(&mode_buff,0);

  sn_advance_line();
  sn_reset_column();

  BEGIN(HDSTRING);
}

<HDSTRING>.*\n {
  matched_pattern(".*\n", yytext);
  #if HDSTRING_DUMP
  fprintf(stderr, "hdstring(1) \"%s\", %d\n", yytext, yyleng);
  #endif

  if (strcmp(yytext, heredoc_id) == 0) {
  #if HDSTRING_DUMP
      fprintf(stderr, "hdstring end matched\n");
  #endif

      sn_advance_column(yyleng-2); /* advance to ; */
      emit_hdstring();
      sn_advance_line();
      sn_reset_column();
      BEGIN(PHP);
  } else {
      if (mode_buff.len != 0) {
          /* Only append newline if more than one line in heredoc */
          mode_buff.append( &mode_buff, "\n", 1 );
      }
      mode_buff.append( &mode_buff, yytext, yyleng-1 );
      sn_advance_line();
      sn_reset_column();
  }
}

<PHP>"(" {
  matched_pattern("(", yytext);
  append_token(OPEN_PAREN, NULL,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<PHP>")" {
  matched_pattern(")", yytext);
  append_token(CLOSE_PAREN, NULL,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<PHP>"[" {
  matched_pattern("[", yytext);
  append_token(OPEN_BRACKET, NULL,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<PHP>"]" {
  matched_pattern("]", yytext);
  append_token(CLOSE_BRACKET, NULL,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<PHP>"{" {
  matched_pattern("{", yytext);
  append_token(OPEN_BRACE, NULL,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<PHP>"}" {
  matched_pattern("}", yytext);
  append_token(CLOSE_BRACE, NULL,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<PHP>";" {
  matched_pattern("}", yytext);
  append_token(SEMICOLON, NULL,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<PHP>"=" {
  matched_pattern("=", yytext);
  append_token(ASSIGNMENT_OPERATOR, NULL,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<PHP>{comparison-operators} {
  matched_pattern("{comparison-operators}", yytext);
  append_token(COMPARISON_OPERATORS, yytext,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<PHP>{assignment-operators} {
  matched_pattern("{assignment-operators}", yytext);
  append_token(ASSIGNMENT_OPERATORS, yytext,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<PHP>"function" {
  matched_pattern("function", yytext);
  append_token(FUNCTION_KEYWORD, yytext,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<PHP>"global" {
  matched_pattern("global", yytext);
  append_token(GLOBAL_KEYWORD, yytext,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<PHP>{include-keywords} {
  matched_pattern("{include-keywords}", yytext);
  append_token(INCLUDE_KEYWORD, yytext,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<PHP>{keywords} {
  matched_pattern("{keywords}", yytext);
  append_token(KEYWORD, yytext,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<PHP>{someword} {
  matched_pattern("{someword}", yytext);
  append_token(SOMEWORD, yytext,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<PHP>\\\$		{
  matched_pattern("\\$", yytext);
  sn_advance_column(yyleng); /* ignore \$ */
}

<PHP>\${varname} {
  matched_pattern("${varname}", yytext);
  sn_advance_column(1);
  append_token(VARIABLE, yytext+1,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng - 1);
  sn_advance_column(yyleng-1);
}

<INITIAL,PHP>\n		{
  matched_pattern("\\n", yytext);
  sn_advance_line();
  sn_reset_column();
}

<INITIAL,PHP>.		{
  matched_pattern(".", yytext);
  sn_advance_column(yyleng); /* eat text */
}



<TOKEN>"FUNCTION_KEYWORD SOMEWORD OPEN_PAREN"{function-argument-tokens}"CLOSE_PAREN OPEN_BRACE " {
  int parens, noargs;
  LongString abuff;

#ifdef TOKEN_DEBUG
  fprintf(tokenout, "function declaration at token %d\n", token_index);
  fprintf(tokenout, "match text was \"%s\"\n", yytext);
#endif
  assert(tokens_head->type == FUNCTION_KEYWORD);

  current_function_line_start = tokens_head->start_line;
  current_function_column_start = tokens_head->start_column;

  free_head_token(); /* FUNCTION_KEYWORD */

  /* FIXME: It would be better to create a state stack and push
     the current state onto that (class, function, namespace) */
  assert(current_function == NULL);
  current_function = SN_StrDup(tokens_head->strval);

  current_function_highlight_line = tokens_head->start_line;
  current_function_highlight_column_start = tokens_head->start_column;
  current_function_highlight_column_end = tokens_head->end_column;

  free_head_token(); /* SOMEWORD */
  free_head_token(); /* OPEN_PAREN */

  /* Save function arguments in current_function_args */  
  parens = 1;
  noargs = 1;
  LongStringInit(&abuff,0);
  while (parens > 0) {
    if (tokens_head->type == OPEN_PAREN) {
      parens++;
    } else if (tokens_head->type == CLOSE_PAREN) {
      parens--;
    } else if (tokens_head->type == VARIABLE) {
        /* We just ignore default values for arguments */
        if (!noargs) {
            abuff.append( &abuff, ",", -1);
        }
        abuff.append( &abuff, tokens_head->strval, -1);
        noargs = 0;
    }

    free_head_token(); /* ... */
  }

  assert(current_function_args == NULL);
  if (!noargs) {
      current_function_args = SN_StrDup(abuff.buf);
  }
  abuff.free(&abuff);

  free_head_token(); /* OPEN_BRACE */

  current_function_brace_count = 1;
}

<TOKEN>"OPEN_BRACE" {
  if (current_function) {
    current_function_brace_count++;
  }
  free_head_token(); /* OPEN_BRACE */
}

<TOKEN>"CLOSE_BRACE" {
  if (current_function && (--current_function_brace_count == 0)) {
#ifdef TOKEN_DEBUG
    fprintf(tokenout, "end of function %s at token %d\n",
        current_function, token_index);
#endif

    current_function_line_end = tokens_head->end_line;
    current_function_column_end = tokens_head->end_column;

    emit_function_declaration();
  }

  free_head_token(); /* CLOSE_BRACE */
}

<TOKEN>("INCLUDE_KEYWORD "{string-token}" SEMICOLON")|("INCLUDE_KEYWORD OPEN_PAREN "{string-token}" CLOSE_PAREN SEMICOLON") {
  int line_start, line_end, column_start, column_end;
  char * filename;

#ifdef TOKEN_DEBUG
  fprintf(tokenout, "include statement begins at token %d\n", token_index);
#endif

  free_head_token(); /* INCLUDE_KEYWORD */

  if (tokens_head->type == OPEN_PAREN) {
    free_head_token(); /* OPEN_PAREN */
  }

  assert((tokens_head->type == SINGLE_QUOTED_STRING) ||
           (tokens_head->type == DOUBLE_QUOTED_STRING));

  line_start = tokens_head->start_line;
  column_start = tokens_head->start_column + 1;
  line_end = tokens_head->end_line;
  column_end = tokens_head->end_column - 1;

  filename = tokens_head->strval;

  result = sn_insert_symbol(SN_INCLUDE_DEF,
          /* classname */ NULL,
          filename,
          sn_current_file(),
          line_start, column_start,
          line_end, column_end,
          /* attr */ 0,
          /* ret */ NULL,
          /* arg_types */ NULL,
          /* arg_names */ NULL,
          /* comment */ NULL,
          line_start, column_start,
          line_end, column_end);

  assert(result == 0);

  free_head_token(); /* SINGLE_QUOTED_STRING || DOUBLE_QUOTED_STRING */

  if (tokens_head->type == CLOSE_PAREN) {
    free_head_token(); /* CLOSE_PAREN */
  }

  free_head_token(); /* SEMICOLON */
}

<TOKEN>("GLOBAL_KEYWORD"{variable-tokens}"SEMICOLON") {
  SearchEntry entry;

#ifdef TOKEN_DEBUG
  fprintf(tokenout, "global statement found at %d\n", token_index);
#endif

  free_head_token(); /* GLOBAL_KEYWORD */

  if (current_function && (global_var_table == NULL)) {
    global_var_table = SearchTableCreate(100, SEARCH_HASH_TABLE, FreeGlobalEntry);
  }

  /* Insert the variable name into the function's global table */

  while (tokens_head->type == VARIABLE) {
    if (current_function) {
        entry.key = tokens_head->strval;
        entry.key_len = -1;

        if (global_var_table->search( &global_var_table, entry ) == NULL)
        {
            entry.data = NULL;
            entry.data_len = 0;
            entry.flag = SEARCH_DUP_KEY; /* add copy of entry.key */
            global_var_table->add( &global_var_table, entry );

#ifdef TOKEN_DEBUG
  fprintf(tokenout, "added global \"%s\"\n", entry.key);
#endif
        }
    }

    free_head_token(); /* VARIABLE */
  }

  free_head_token(); /* SEMICOLON */
}

<TOKEN>"SOMEWORD OPEN_PAREN" {
  char* fname;
  int line;

#ifdef TOKEN_DEBUG
  fprintf(tokenout, "found function call tokens at %d\n", token_index);
  fprintf(tokenout, "match text was \"%s\"\n", yytext);
#endif

  fname = tokens_head->strval;
#ifdef TOKEN_DEBUG
  fprintf(tokenout, "function name is \"%s\"\n", fname);
#endif
  line = tokens_head->start_line;

  result = sn_insert_xref(SN_REF_TO_FUNCTION,
                 SN_FUNC_DEF,
                 SN_REF_SCOPE_GLOBAL,
                 NULL,
                 (current_function == NULL) ? "GLOBAL" : current_function,
                 NULL,
                 NULL,
                 fname,
                 NULL, 
                 sn_current_file(),
                 line,
                 SN_REF_PASS);

  assert(result == 0);

  free_head_token(); /* SOMEWORD */
  free_head_token(); /* OPEN_PAREN */
}

<TOKEN>"VARIABLE"{var-array-tokens}("ASSIGNMENT_OPERATOR"|"ASSIGNMENT_OPERATORS") {
  char* varname = tokens_head->strval;
  SearchEntry entry;
  int ref_to_symbol_type, ref_to_symbol_scope, symbol_def_type;
  int line_start, line_end, column_start, column_end;
  int offset;

  line_start = tokens_head->start_line;
  column_start = tokens_head->start_column;
  line_end = tokens_head->end_line;
  column_end = tokens_head->end_column;

#ifdef TOKEN_DEBUG
  fprintf(tokenout, "variable write at token %d\n", token_index);
#endif

  /*
   * If we are not in a function or the variable is in the
   * function's global table then it is a global.
   */

  entry.key = varname;
  entry.key_len = -1;

  if ((current_function == NULL) ||
      (global_var_table &&
        (global_var_table->search( &global_var_table, entry ) != NULL))) {
    ref_to_symbol_type = SN_REF_TO_GLOB_VAR;
    ref_to_symbol_scope = SN_REF_SCOPE_GLOBAL;
    symbol_def_type = SN_GLOB_VAR_DEF;
#ifdef TOKEN_DEBUG
    fprintf(tokenout, "global var \"%s\"\n", varname);
#endif
  } else {
    ref_to_symbol_type = SN_REF_TO_LOCAL_VAR;
    ref_to_symbol_scope = SN_REF_SCOPE_LOCAL;
    symbol_def_type = SN_LOCAL_VAR_DEF;
#ifdef TOKEN_DEBUG
    fprintf(tokenout, "local var \"%s\"\n", varname);
#endif
  }

  /* I think setting a global variable should emit
     a symbol declaration. I am not really
     sure if we should emit just one symbol
     declaration, or multiple ones. Multiple
     ones breaks xref but it does present
     the symbol multiple times in the
     file's symbol list which would be
     nice if it worked correctly */

  result = sn_insert_symbol(symbol_def_type,
                            NULL,
			    varname,
			    sn_current_file(),
			    line_start, column_start,
			    line_end, column_end,
			    0,
			    NULL,
			    NULL,
			    NULL,
			    NULL,
			    line_start, column_start,
			    line_end, column_end
                            );
  assert(result == 0);

  /* FIXME: The second argument to sn_insert_xref must be one of SN_FUNC_DEF,
   * SN_MBR_FUNC_DEF, or SN_SUBR_DEF. This is too restrictive, there
   * needs to be an option to pass when not in a function. Currently,
   * we just pretend to be in a function named "GLOBAL".
   */

  if ((ref_to_symbol_type == SN_REF_TO_GLOB_VAR) ||
      ((ref_to_symbol_type == SN_REF_TO_LOCAL_VAR) &&
          ((int) sn_getopt(SN_OPT_LOCAL_VARS) != 0))) {

    result = sn_insert_xref(ref_to_symbol_type,
                 SN_FUNC_DEF, /* Not really in a function definition */
                 ref_to_symbol_scope,
                 NULL,
                 (current_function == NULL) ? "GLOBAL" : current_function,
                 NULL,
                 NULL,
                 varname,
                 NULL,
                 sn_current_file(),
                 line_start,
                 SN_REF_WRITE);

    assert(result == 0);
  }

  /* Eat the variable token and send the rest of the tokens
   * back for further matching. This is needed so that
   * expressions like "$var[$one] = 1;" generate both
   * a write xref for $var and a read xref for $one.
   */

  free_head_token(); /* VARIABLE */
  offset = strlen("VARIABLE"); /* send back everything after the VARIABLE token */
  yyless(offset);
}

<TOKEN>"VARIABLE" {
  char* varname = tokens_head->strval;
  SearchEntry entry;
  int line_num = tokens_head->start_line;
  int ref_to_symbol_type, ref_to_symbol_scope;

#ifdef TOKEN_DEBUG
  fprintf(tokenout, "variable read at token %d\n", token_index);
#endif

  /*
   * If we are not in a function or the variable is in the
   * function's global table then it is a global.
   */

  entry.key = varname;
  entry.key_len = -1;

  if ((current_function == NULL) ||
      (global_var_table &&
        (global_var_table->search( &global_var_table, entry ) != NULL))) {
    ref_to_symbol_type = SN_REF_TO_GLOB_VAR;
    ref_to_symbol_scope = SN_REF_SCOPE_GLOBAL;
#ifdef TOKEN_DEBUG
    fprintf(tokenout, "global var \"%s\"\n", varname);
#endif
  } else {
    ref_to_symbol_type = SN_REF_TO_LOCAL_VAR;
    ref_to_symbol_scope = SN_REF_SCOPE_LOCAL;
#ifdef TOKEN_DEBUG
    fprintf(tokenout, "local var \"%s\"\n", varname);
#endif
  }

  /* FIXME: The second argument to sn_insert_xref must be one of SN_FUNC_DEF,
   * SN_MBR_FUNC_DEF, or SN_SUBR_DEF. This is too restrictive, there
   * needs to be an option to pass when not in a function. Currently,
   * we just pretend to be in a function named "GLOBAL".
   */

  if ((ref_to_symbol_type == SN_REF_TO_GLOB_VAR) ||
      ((ref_to_symbol_type == SN_REF_TO_LOCAL_VAR) &&
          ((int) sn_getopt(SN_OPT_LOCAL_VARS) != 0))) {

    result = sn_insert_xref(ref_to_symbol_type,
                 SN_FUNC_DEF, /* Not really in a function definition */
                 ref_to_symbol_scope,
                 NULL,
                 (current_function == NULL) ? "GLOBAL" : current_function,
                 NULL,
                 NULL,
                 varname,
                 NULL,
                 sn_current_file(),
                 line_num,
                 SN_REF_READ);

    assert(result == 0);
  }

  /* FIXME: Could add {var-array-tokens} to this match but it is not
   * clear why this is needed right now. Also, a handy way to count
   * the number of tokens in a match (given yytext) would make it
   * easy to know the number of tokens to free.
   */

  free_head_token(); /* VARIABLE */
}

<TOKEN>{token} {
#ifdef TOKEN_DEBUG
  fprintf(tokenout, "ate token %d %s", token_index,
          TokenTypeToString(tokens_head));
  if (tokens_head->strval) {
      fprintf(tokenout, " \"%s\"", tokens_head->strval);
  }
  fprintf(tokenout, "\n");
#endif

  free_head_token(); /* ... */
}

<TOKEN>" "

<TOKEN>.		{
#ifdef TOKEN_DEBUG
    fprintf(tokenout, "matched unknown character \"%s\"\n", yytext);
#endif
}

<TOKEN><<EOF>> {
#ifdef TOKEN_DEBUG
    fprintf(tokenout, "reached EOF in TOKEN buffer\n");
#endif

    /* A function closing brace was not found before we hit EOF */
    if (current_function) {
        current_function_line_end = sn_line();
        current_function_column_end = sn_column();

#ifdef TOKEN_DEBUG
        fprintf(tokenout, "found unfinished function at EOF in %s\n", sn_current_file());
#endif

        emit_function_declaration();
    }

    assert(!tokens_head); /* all tokens were processed */

    yy_delete_buffer( YY_CURRENT_BUFFER );
    yy_switch_to_buffer( original_buffer );
    BEGIN(INITIAL);
    yyterminate();
}

<INITIAL,PHP,COMMENT,DQSTRING,SQSTRING,HDSTRING><<EOF>> {
  LongString token_buffer;
  char *base;
  int i;
  Token* tok;
  yy_size_t size;
  YY_BUFFER_STATE yybs;

#ifdef TOKEN_DEBUG
  tokenout = fopen("tokens.out", "a");
  fprintf(tokenout, "reached EOF in lex input buffer in mode %s\n", modestring());
#endif

  /* See if we ran off the end of the lex input buffer in a special mode */
  switch (YY_START) {
    case COMMENT:
      emit_comment();
      break;
    case DQSTRING:
      emit_dqstring();
      break;
    case SQSTRING:
      emit_sqstring();
      break;
    case HDSTRING: 
      emit_hdstring();
      break;
  }

  /* If no tokens were generated, then quit now */
  if (tokens_head == NULL) {
#ifdef TOKEN_DEBUG
    fprintf(tokenout, "no TOKENs generated\n");
#endif
    yyterminate();
  }

  LongStringInit(&token_buffer,0);

  /* Print token info to in memory buffer and then reload
     the input state machine and start out in the TOKEN mode. */

  for (i=0, tok = tokens_head ; tok ; tok = tok->next, i++) {
#ifdef TOKEN_DEBUG
        fprintf(tokenout, "token %d %s", i, TokenTypeToString(tok));
        if (tok->strval) {
            fprintf(tokenout, " \"%s\"", tok->strval);
        }
        fprintf(tokenout, " (%d.%d -> %d.%d)",
            tok->start_line,
            tok->start_column,
            tok->end_line,
            tok->end_column
        );
        fprintf(tokenout, "\n");
#endif
        
        token_buffer.append( &token_buffer, TokenTypeToString(tok), -1);
        token_buffer.append( &token_buffer, " ", -1);
  }

#ifdef TOKEN_DEBUG
  fprintf(tokenout, "token buffer data is \"%s\"\n", token_buffer.buf);
#endif

  original_buffer = YY_CURRENT_BUFFER;
  yy_switch_to_buffer( yy_scan_string(token_buffer.buf) );

  token_buffer.free(&token_buffer);

#ifdef TOKEN_DEBUG
  fprintf(tokenout, "switching to token mode\n");
#endif

  BEGIN(TOKEN);
}

%%

/* Return a string that describes the current mode */

static char* modestring() {
    char* mode = "UNKNOWN";

    switch (YY_START) {
        case INITIAL:
            mode = "INITIAL";
            break;
        case PHP:
            mode = "PHP";
            break;
        case COMMENT:
            mode = "COMMENT";
            break;
        case DQSTRING:
            mode = "DQSTRING";
            break;
        case SQSTRING:
            mode = "SQSTRING";
            break;
        case HDSTRING:
            mode = "HDSTRING";
            break;
        case TOKEN:
            mode = "TOKEN";
            break;
    }
    return mode;
}

/* Helper method that will print matches as they are made */

static void matched_pattern(char * pattern, char * text) {
    char * mode = modestring();

    if (0) {
    fprintf(stderr, "Matched \"%s\", with text \"%s\", in mode \"%s\" (%d.%d)\n",
        pattern, text, mode, sn_line(), sn_column());
    }
}

static void FreeGlobalEntry(SearchEntry *entry) {}


void FreeToken(Token* tok) {
    if (tok->strval != NULL) {
        ckfree(tok->strval);
    }
    ckfree((char *) tok);
}

void append_token(TokenType type,
                  char* strval,
                  long start_line,
                  int start_column,
                  long end_line,
                  int end_column) {
    Token* tok;
    tok = (Token*) ckalloc(sizeof(Token));
    tok->type = type;
    if (strval)
        tok->strval = SN_StrDup(strval);
    else
        tok->strval = NULL;

    tok->start_line = start_line;
    tok->start_column = start_column;

    tok->end_line = end_line;
    tok->end_column = end_column;
    tok->next = NULL;

    /* append to token list */

    if (tokens_tail == NULL) {
        tokens_head = tokens_tail = tok;
    } else {
        tokens_tail->next = tok;
        tokens_tail = tok;
    }
}

void free_head_token() {
    Token* tok;
    assert(tokens_head);
    tok = tokens_head;
    if (tokens_head == tokens_tail) {
        tokens_head = tokens_tail = (Token*) NULL;
    } else {
        tokens_head = tokens_head->next;
    }
    FreeToken(tok);
    token_index++;
}

char * TokenTypeToString(Token *tok) {
    switch(tok->type) {
        case OPEN_PAREN:
            return "OPEN_PAREN";
        case CLOSE_PAREN:
            return "CLOSE_PAREN";
        case OPEN_BRACE:
            return "OPEN_BRACE";
        case CLOSE_BRACE:
            return "CLOSE_BRACE";
        case OPEN_BRACKET:
            return "OPEN_BRACKET";
        case CLOSE_BRACKET:
            return "CLOSE_BRACKET";
        case SEMICOLON:
            return "SEMICOLON";
        case COMMA:
            return "COMMA";
        case VARIABLE:
            return "VARIABLE";
        case ASSIGNMENT_OPERATOR:
            return "ASSIGNMENT_OPERATOR";
        case ASSIGNMENT_OPERATORS:
            return "ASSIGNMENT_OPERATORS";
        case COMPARISON_OPERATORS:
            return "COMPARISON_OPERATORS";
        case SOMEWORD:
            return "SOMEWORD";
        case KEYWORD:
            return "KEYWORD";
        case FUNCTION_KEYWORD:
            return "FUNCTION_KEYWORD";
        case GLOBAL_KEYWORD:
            return "GLOBAL_KEYWORD";
        case INCLUDE_KEYWORD:
            return "INCLUDE_KEYWORD";
        case DOUBLE_QUOTED_STRING:
            return "DOUBLE_QUOTED_STRING";
        case SINGLE_QUOTED_STRING:
            return "SINGLE_QUOTED_STRING";
        case UNKNOWN:
            return "UNKNOWN";
        default:
            return "TOKEN_NOT_MATCHED";
    }
}

/* Called when the closing brace of a function is found
 * or when we hit EOF without finding the end of the
 * function.
 */

void emit_function_declaration() {
    result = sn_insert_symbol(SN_FUNC_DEF,
            NULL,
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

    ckfree(current_function);
    current_function = NULL;
    ckfree(current_function_args);
    current_function_args = NULL;

    if (global_var_table) {
      global_var_table->destroy( &global_var_table );
      global_var_table = NULL;
    }
}

void emit_comment() {
    char* comment = mode_buff.buf;

#if COMMENT_DUMP
    fprintf(stderr, "emit comment \"%s\"\n", comment);
#endif

    sn_insert_comment(
        /* classname */ NULL,
        /* funcname */ NULL,
        sn_current_file(),
        comment,
        mode_start_line,
        mode_start_column);

    mode_buff.free(&mode_buff);
}

void emit_dqstring() {
    char* dqstring = mode_buff.buf;

#if DQSTRING_DUMP
    fprintf(stderr, "creating dqstring token \"%s\"\n", dqstring);
#endif

    append_token(DOUBLE_QUOTED_STRING, dqstring,
            mode_start_line,
            mode_start_column,
            sn_line(),
            sn_column());

    mode_buff.free(&mode_buff);
}

void emit_sqstring() {
    char* sqstring = mode_buff.buf;

#if SQSTRING_DUMP
    fprintf(stderr, "creating sqstring token \'%s\'\n", sqstring);
#endif

    append_token(SINGLE_QUOTED_STRING, sqstring,
            mode_start_line,
            mode_start_column,
            sn_line(),
            sn_column());

    mode_buff.free(&mode_buff);
}

void emit_hdstring() {
    char* hdstring = mode_buff.buf;

#if HDSTRING_DUMP
    fprintf(stderr, "creating hdstring token \"%s\"\n", hdstring);
#endif

    /* sn_column() should be at the ; at the end of a heredoc */

    append_token(DOUBLE_QUOTED_STRING, hdstring,
            mode_start_line,
            mode_start_column,
            sn_line(),
            sn_column());

    /* A heredoc string is followed by a SEMICOLON */

    append_token(SEMICOLON, NULL,
            sn_line(),
            sn_column(),
            sn_line(),
            sn_column()+1);

    mode_buff.free(&mode_buff);
}
 
void
reset()
{
  assert(!current_function);
  sn_reset_line();
  sn_reset_column();
}

int
main(int argc, char *argv[])
{
  return sn_main(argc, argv, group, &yyin, yylex, reset);
}
