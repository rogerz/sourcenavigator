/*

Copyright (c) 2004, Mo DeJong

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
 * exlbrowser.l
 *
 * Copyright (C) 2004 Mo DeJong
 *
 * Description:
 * Lex input file for a simple little example language.
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
    SEMICOLON,     /* ';' */

    ASSIGNMENT_OPERATOR, /* '=' */

    LITERAL,           /* An integer literal like 1 */
    SOMEWORD,          /* a sequence of text */
    KEYWORD,           /* any keyword not handled below */
    DECLARE_KEYWORD, /* The keyword "declare" */
    GLOBAL_KEYWORD, /* The keyword "global" */

    DOUBLE_QUOTED_STRING, /* "I am a double quoted string" */

    UNKNOWN,        /* Token that parser does not know about */
    COMMENT,        /* Emit COMMENT token only when debugging */
} TokenType;

typedef enum {
    VAR_READ,
    VAR_WRITE,
    VAR_READWRITE
} VarAccess;

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
/*#define TOKEN_DEBUG*/

#ifdef TOKEN_DEBUG
FILE* tokenout = NULL;
#endif

static char * token_dump_file = NULL;
static int highlight_file = 0;

Token* tokens_head = NULL;
Token* tokens_tail = NULL;
int token_index = 0; /* current offset from original token_head */

static char group[] = "exl";

#define MAX_SIZE 512

/* FIXME: Can't pass NULL or "" as current function (core dump) */
static char* current_function = (char *) NULL;

/* line number where highlight starts and ends */
static int  current_function_highlight_line;

/* in "declare fog() {}" column of 'f' in "fog" */
static int  current_function_highlight_column_start;

/* in "declare fog() {}" column of 'g' in "fog" */
static int  current_function_highlight_column_end;

/* line where "declare" appears */
static int  current_function_line_start;

/* line where closing brace of function appears */
static int  current_function_line_end;

/* in "declare fog() {}" column of 'd' in "declare" */
static int  current_function_column_start;

/* in "declare fog() {}" column of '}' in "{}" */
static int  current_function_column_end;

static int  current_function_brace_count;

static Token* current_array_token = NULL;

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
#define MATCH_DUMP 0

static YY_BUFFER_STATE original_buffer;

#if MATCH_DUMP
static void matched_pattern(char * pattern, char * text);
#endif

static char* modestring();

static void FreeGlobalEntry(SearchEntry *entry);

void FreeToken(Token* tok);
Token* pop_head_token();
void free_head_token();
void append_token(TokenType type,
                  char* strval,
                  long start_line,
                  int start_column,
                  long end_line,
                  int end_column);

void append_dqstring_token(char* strval,
                           long start_line,
                           int start_column,
                           long end_line,
                           int end_column);

char * TokenTypeToString(Token *tok);

void emit_function_declaration();
void emit_comment();
void emit_dqstring();
void emit_var_access(Token* tok, VarAccess acc);

int yywrap() { return 1; }

%}

%x COMMENT_MODE
%x EXAMPLE
%x DQSTRING
%x TOKEN

ws		[ \t]
wsn		[ \t\n]
symbol		[a-zA-Z_][a-zA-Z0-9_]*
literal		0|[1-9][0-9]*

someword	{symbol}
token		[a-zA-Z0-9_]+

%%
    /* Start in EXAMPLE mode */
    highlight_file = (int) sn_getopt(SN_OPT_HIGHLIGHT);
    token_dump_file = (char *) sn_getopt(SN_OPT_DUMP_TOKENS);
    BEGIN(EXAMPLE);

<EXAMPLE>"/*"		{ /* A C style multi-line comment, just like this! */
#if MATCH_DUMP
    matched_pattern("/*", yytext);
#endif
    BEGIN(COMMENT_MODE);
    sn_advance_column(2);
    LongStringInit(&mode_buff,0);
    mode_start_line = sn_line();
    mode_start_column = sn_column();
}

<COMMENT_MODE>{
  [^\*\n]* {
    #if MATCH_DUMP
    matched_pattern("[^\\*\\n]*", yytext);
    #endif

    #if COMMENT_DUMP
    fprintf(stderr, "comment(1) \"%s\", %d\n", yytext, yyleng);
    #endif
    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_column(yyleng);
  }
  [^\*\n]*\n {
    #if MATCH_DUMP
    matched_pattern("[^\\*\\n]*\\n", yytext);
    #endif

    #if COMMENT_DUMP
    fprintf(stderr, "comment(2) \"%s\", %d\n", yytext, yyleng);
    #endif

    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_line();
    sn_reset_column();
  }
  \*+[^\*/\n]* {
    #if MATCH_DUMP
    matched_pattern("\\*+[^\\*/\\n]*", yytext);
    #endif

    #if COMMENT_DUMP
    fprintf(stderr, "comment(3) \"%s\", %d\n", yytext, yyleng);
    #endif

    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_column(yyleng);
  }
  \*+[^\*/\n]*\n {
    #if MATCH_DUMP
    matched_pattern("\\*+[^\\*/\\n]*\\n", yytext);
    #endif

    #if COMMENT_DUMP
    fprintf(stderr, "comment(4) \"%s\", %d\n", yytext, yyleng);
    #endif

    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_line();
    sn_reset_column();
  }
  "*"+"/" {
    #if MATCH_DUMP
    matched_pattern("\\*+/", yytext);
    #endif

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
    BEGIN(EXAMPLE);
  }
  /* Uncomment the next rule if you want to check to make sure
   * the above rules cover all possible input. A warning
   * "rule cannot be matched" should be printed by flex.
   */
  /*. {}*/
}

<EXAMPLE>\"\" {
#if MATCH_DUMP
  matched_pattern("\\\"\\\"", yytext);
#endif
  /* FIXME: Can we pass NULL instead of "" after length issues worked out ? */
  append_dqstring_token("",
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<EXAMPLE>\" {
#if MATCH_DUMP
  matched_pattern("\\\"", yytext);
#endif

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
  [^\"\n]* {
    #if MATCH_DUMP
    matched_pattern("[^\\\"\\n]*", yytext);
    #endif

    #if DQSTRING_DUMP
    fprintf(stderr, "dqstring(1) \"%s\", %d\n", yytext, yyleng);
    #endif
    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_column(yyleng);
  }
  [^\"\n]*\n {
    #if MATCH_DUMP
    matched_pattern("[^\\\"\\n]*\\n", yytext);
    #endif

    #if DQSTRING_DUMP
    fprintf(stderr, "dqstring(2) \"%s\", %d\n", yytext, yyleng);
    #endif
    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_line();
    sn_reset_column();
  }
  (\\\")+ {
    #if MATCH_DUMP
    matched_pattern("(\\\")+", yytext);
    #endif

    #if DQSTRING_DUMP
    fprintf(stderr, "dqstring(3) \"%s\", %d\n", yytext, yyleng);
    #endif

    mode_buff.append( &mode_buff,
        yytext, yyleng );
    sn_advance_column(yyleng);
  }
  \" {
    #if MATCH_DUMP
    matched_pattern("\\\"", yytext);
    #endif

    sn_advance_column(yyleng);
    emit_dqstring();
    BEGIN(EXAMPLE);
  }
  /* Uncomment the next rule if you want to check to make sure
   * the above rules cover all possible input. A warning
   * "rule cannot be matched" should be printed by flex.
   */
  /*. {}*/
}

<EXAMPLE>"(" {
#if MATCH_DUMP
  matched_pattern("(", yytext);
#endif

  append_token(OPEN_PAREN, NULL,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<EXAMPLE>")" {
#if MATCH_DUMP
  matched_pattern(")", yytext);
#endif
  append_token(CLOSE_PAREN, NULL,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<EXAMPLE>"{" {
#if MATCH_DUMP
  matched_pattern("{", yytext);
#endif
  append_token(OPEN_BRACE, NULL,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<EXAMPLE>"}" {
#if MATCH_DUMP
  matched_pattern("}", yytext);
#endif
  append_token(CLOSE_BRACE, NULL,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<EXAMPLE>";" {
#if MATCH_DUMP
  matched_pattern(";", yytext);
#endif
  append_token(SEMICOLON, NULL,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<EXAMPLE>"=" {
#if MATCH_DUMP
  matched_pattern("=", yytext);
#endif
  append_token(ASSIGNMENT_OPERATOR, NULL,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<EXAMPLE>"declare" {
#if MATCH_DUMP
  matched_pattern("declare", yytext);
#endif

  append_token(DECLARE_KEYWORD, NULL,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<EXAMPLE>"global" {
#if MATCH_DUMP
  matched_pattern("global", yytext);
#endif
  append_token(GLOBAL_KEYWORD, NULL,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<EXAMPLE>"return" {
#if MATCH_DUMP
  matched_pattern("return", yytext);
#endif
  append_token(KEYWORD, yytext,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<EXAMPLE>{literal} {
#if MATCH_DUMP
  matched_pattern("{literal}", yytext);
#endif
  append_token(LITERAL, yytext,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<EXAMPLE>{someword} {
#if MATCH_DUMP
  matched_pattern("{someword}", yytext);
#endif

  append_token(SOMEWORD, yytext,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);
  sn_advance_column(yyleng);
}

<EXAMPLE>{wsn}+ {
  char* x, *y;
  #if MATCH_DUMP
  matched_pattern("{wsn}+", yytext);
  #endif

  for (x=yytext, y=NULL; *x ; x++) {
    if (*x == '\n') {
        y=x+1;
        sn_advance_line();
    }
  }
  if (y == NULL) {
    sn_advance_column(yyleng);
  } else {
    sn_reset_column();
    sn_advance_column(x-y);
  }
}

<EXAMPLE>.		{
#if MATCH_DUMP
  matched_pattern(".", yytext);
#endif

  /* Add an UNKNOWN token for each
   * character that we don't know
   * how to deal with.
   */

  append_token(UNKNOWN, yytext,
          sn_line(),
          sn_column(),
          sn_line(),
          sn_column() + yyleng);

  /*fprintf(stderr, "adding unknown token for \"%s\"\n", yytext);*/

  sn_advance_column(yyleng); /* eat text */
}

<TOKEN>"DECLARE_KEYWORD SOMEWORD OPEN_PAREN CLOSE_PAREN OPEN_BRACE " {
  int parens, noargs;
  LongString abuff;

#ifdef TOKEN_DEBUG
  fprintf(tokenout, "declaration at token %d\n", token_index);
  fprintf(tokenout, "match text was \"%s\"\n", yytext);
#endif
  assert(tokens_head->type == DECLARE_KEYWORD);

  current_function_line_start = tokens_head->start_line;
  current_function_column_start = tokens_head->start_column;

  sn_highlight(SN_HIGH_KEYWORD,
          tokens_head->start_line, tokens_head->start_column,
          tokens_head->end_line, tokens_head->end_column);

  free_head_token(); /* DECLARE_KEYWORD */

  if (current_function != NULL) {
    panic("Can't declare function inside another function");
  }
  current_function = SN_StrDup(tokens_head->strval);

  current_function_highlight_line = tokens_head->start_line;
  current_function_highlight_column_start = tokens_head->start_column;
  current_function_highlight_column_end = tokens_head->end_column;

  free_head_token(); /* SOMEWORD */
  free_head_token(); /* OPEN_PAREN */
  free_head_token(); /* CLOSE_PAREN */
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

<TOKEN>"GLOBAL_KEYWORD SOMEWORD SEMICOLON" {
  SearchEntry entry;

#ifdef TOKEN_DEBUG
  fprintf(tokenout, "global statement found at %d\n", token_index);
#endif

  /* FIXME: If global keyword is used outside of a function,
     should we mark it as a keyword? We could mark it as
     a keyword and a syntax error also */
  sn_highlight(SN_HIGH_KEYWORD,
          tokens_head->start_line, tokens_head->start_column,
          tokens_head->end_line, tokens_head->end_column);

  assert(tokens_head->type == GLOBAL_KEYWORD);
  free_head_token(); /* GLOBAL_KEYWORD */

  if (current_function && (global_var_table == NULL)) {
    global_var_table = SearchTableCreate(100, SEARCH_HASH_TABLE, FreeGlobalEntry);
  }

  /* Insert the variable name into the function's global table */
  assert(tokens_head->type == SOMEWORD);

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

    sn_highlight(SN_HIGH_VAR_GLOBAL,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);
  }

  free_head_token(); /* SOMEWORD */
  free_head_token(); /* SEMICOLON */
}

<TOKEN>"SOMEWORD OPEN_PAREN" {
  char* fname;
  int line;
  int ref_from_scope_type;

#ifdef TOKEN_DEBUG
  fprintf(tokenout, "found function call tokens at %d\n", token_index);
  fprintf(tokenout, "match text was \"%s\"\n", yytext);
#endif

  fname = tokens_head->strval;
#ifdef TOKEN_DEBUG
  fprintf(tokenout, "function name is \"%s\"\n", fname);
#endif
  line = tokens_head->start_line;

  /*
   * Pass SN_GLOBAL_NAMESPACE if the xref is outside of
   * a function, otherwise pass SN_FUNC_DEF.
   */   

  if (current_function == NULL) {
    ref_from_scope_type = SN_GLOBAL_NAMESPACE;
  } else {
    ref_from_scope_type = SN_FUNC_DEF;
  }

  result = sn_insert_xref(SN_REF_TO_FUNCTION,
                 ref_from_scope_type,
                 SN_REF_SCOPE_GLOBAL,
                 NULL,
                 current_function,
                 NULL,
                 NULL,
                 fname,
                 NULL, 
                 sn_current_file(),
                 line,
                 SN_REF_PASS);

  assert(result == 0);

  sn_highlight(SN_HIGH_FUNCTION,
      tokens_head->start_line, tokens_head->start_column,
      tokens_head->end_line, tokens_head->end_column);

  free_head_token(); /* SOMEWORD */
  free_head_token(); /* OPEN_PAREN */
}

<TOKEN>"SOMEWORD ASSIGNMENT_OPERATOR" {
  int offset;

#ifdef TOKEN_DEBUG
  fprintf(tokenout, "variable assignment at token %d\n", token_index);
#endif

  emit_var_access(tokens_head, VAR_WRITE);

  free_head_token(); /* SOMEWORD */
  free_head_token(); /* ASSIGNMENT_OPERATOR */
}

<TOKEN>"SOMEWORD" {
#ifdef TOKEN_DEBUG
  fprintf(tokenout, "variable read at token %d\n", token_index);
#endif

  emit_var_access(tokens_head, VAR_READ);

  free_head_token(); /* SOMEWORD */
}

<TOKEN>{token} {
  enum sn_highlights type;

#ifdef TOKEN_DEBUG
  fprintf(tokenout, "ate token %d %s", token_index,
          TokenTypeToString(tokens_head));
  if (tokens_head->strval) {
      fprintf(tokenout, " \"%s\"", tokens_head->strval);
  }
  fprintf(tokenout, "\n");
#endif

  if (highlight_file) {
      switch (tokens_head->type) {
          case DOUBLE_QUOTED_STRING:
              type = SN_HIGH_STRING;
              break;
          case KEYWORD:
              type = SN_HIGH_KEYWORD;
              break;
          default:
              type = 0;
      }
      if (type != 0) {
          sn_highlight(type,
                  tokens_head->start_line, tokens_head->start_column,
                  tokens_head->end_line, tokens_head->end_column);
      }
  }

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
    BEGIN(EXAMPLE);
    yyterminate();
}

<EXAMPLE,COMMENT_MODE,DQSTRING><<EOF>> {
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
    case COMMENT_MODE:
      emit_comment();
      break;
    case DQSTRING:
      emit_dqstring();
      break;
  }

  /* If no tokens were generated, then quit now */
  if (tokens_head == NULL) {
#ifdef TOKEN_DEBUG
    fprintf(tokenout, "no TOKENs generated\n");
#endif
    BEGIN(EXAMPLE);
    yyterminate();
  }

  /*
   * If the -T command line option was passed,
   * dump all tokens to a file, skip the token
   * matching phase and go on to the next file.
   */

  if (token_dump_file) {
    FILE * dump_tokens = fopen(token_dump_file, "a");

    for (i=0, tok = tokens_head ; tok ; tok = tok->next, i++) {
        fprintf(dump_tokens, "%d %s", i, TokenTypeToString(tok));
        if (tok->strval == NULL) {
            fprintf(dump_tokens, " \"\"");
        } else {
            char *x;
            fprintf(dump_tokens, " \"");
            for (x=tok->strval; *x; x++) {
                if (*x == '\n') {
                    fprintf(dump_tokens, "\\n");
                } else if (*x == '\\') {
                    fprintf(dump_tokens, "\\\\");
                } else if (*x == '\"') {
                    fprintf(dump_tokens, "\\\"");
                } else {
                    fprintf(dump_tokens, "%c", *x);
                }
            }
            fprintf(dump_tokens, "\"");
        }
        fprintf(dump_tokens, " %d.%d %d.%d",
            tok->start_line,
            tok->start_column,
            tok->end_line,
            tok->end_column
        );
        fprintf(dump_tokens, "\n");
    }

    fclose(dump_tokens);

    BEGIN(EXAMPLE);
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
        case EXAMPLE:
            mode = "EXAMPLE";
            break;
        case COMMENT_MODE:
            mode = "COMMENT_MODE";
            break;
        case DQSTRING:
            mode = "DQSTRING";
            break;
        case TOKEN:
            mode = "TOKEN";
            break;
    }
    return mode;
}

#if MATCH_DUMP

/* Helper method that will print matches as they are made.
 * This method is typically used in the token generation phase.
 */

static void matched_pattern(char * pattern, char * text) {
    char * mode = modestring();

    fprintf(stderr, "Matched \"%s\", with text \"%s\", in mode \"%s\" (%d.%d)\n",
        pattern, text, mode, sn_line(), sn_column());
}

#endif /* MATCH_DUMP */

static void FreeGlobalEntry(SearchEntry *entry) {}


void FreeToken(Token* tok) {
    if (tok->strval != NULL) {
        ckfree(tok->strval);
    }
    ckfree((char *) tok);
}

void append_dqstring_token(char* strval,
                           long start_line,
                           int start_column,
                           long end_line,
                           int end_column) {
    append_token(DOUBLE_QUOTED_STRING,
                     strval,
                     start_line,
                     start_column,
                     end_line,
                     end_column);
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

Token* pop_head_token() {
    Token* tok;
    assert(tokens_head);
    tok = tokens_head;
    if (tokens_head == tokens_tail) {
        tokens_head = tokens_tail = (Token*) NULL;
    } else {
        tokens_head = tokens_head->next;
    }
    token_index++;
    return tok;
}

void free_head_token() {
    FreeToken(pop_head_token());
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
        case SEMICOLON:
            return "SEMICOLON";
        case ASSIGNMENT_OPERATOR:
            return "ASSIGNMENT_OPERATOR";
        case LITERAL:
            return "LITERAL";
        case SOMEWORD:
            return "SOMEWORD";
        case KEYWORD:
            return "KEYWORD";
        case DECLARE_KEYWORD:
            return "DECLARE_KEYWORD";
        case GLOBAL_KEYWORD:
            return "GLOBAL_KEYWORD";
        case DOUBLE_QUOTED_STRING:
            return "DOUBLE_QUOTED_STRING";
        case UNKNOWN:
            return "UNKNOWN";
        case COMMENT:
            return "COMMENT";
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
            NULL /* argument names */,
            NULL /* comment */,
            current_function_highlight_line,
            current_function_highlight_column_start,
            current_function_highlight_line,
            current_function_highlight_column_end );

    assert(result == 0);

    ckfree(current_function);
    current_function = NULL;

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

  /* If dumping tokens, emit a special COMMENT token.
   * Otherwise, insert a comment symbol and a highlight.
   */
  if (token_dump_file) {
    append_token(COMMENT, comment,
            mode_start_line,
            mode_start_column - 2,
            sn_line(),
            sn_column() + 2);
  } else {
    sn_insert_comment(
        /* classname */ NULL,
        /* funcname */ NULL,
        sn_current_file(),
        comment,
        mode_start_line,
        mode_start_column);

    sn_highlight(SN_HIGH_COMMENT,
            mode_start_line,
            mode_start_column - 2,
            sn_line(),
            sn_column() + 2);
  }

  mode_buff.free(&mode_buff);
}

void emit_dqstring() {
    char* dqstring = mode_buff.buf;
    char * x;
    char * var;

#if DQSTRING_DUMP
    fprintf(stderr, "creating dqstring token \"%s\"\n", dqstring);
#endif

    append_dqstring_token(dqstring,
            mode_start_line,
            mode_start_column,
            sn_line(),
            sn_column());

    mode_buff.free(&mode_buff);
}

/* This method is invoked when a var read or write operation is found
 * in the token stream.
 */

void emit_var_access(Token *tok, VarAccess acc) {
  char* varname = tok->strval;
  SearchEntry entry;
  int ref_to_symbol_type, ref_to_symbol_scope;
  int ref_from_scope_type;
  int line_start, line_end, column_start, column_end;

  line_start = tok->start_line;
  column_start = tok->start_column;
  line_end = tok->end_line;
  column_end = tok->end_column;

  /*
   * A var is global if not currently in a function,
   * if the variable is in the super global table,
   * or if in the global table.
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

  /*
   * Pass SN_GLOBAL_NAMESPACE if the xref is outside of
   * a function, otherwise pass SN_FUNC_DEF.
   */   

  if (current_function == NULL) {
    ref_from_scope_type = SN_GLOBAL_NAMESPACE;
  } else {
    ref_from_scope_type = SN_FUNC_DEF;
  }

  if ((ref_to_symbol_type == SN_REF_TO_GLOB_VAR) ||
      ((ref_to_symbol_type == SN_REF_TO_LOCAL_VAR) &&
          ((int) sn_getopt(SN_OPT_LOCAL_VARS) != 0))) {

    if ((acc == VAR_READ) || (acc == VAR_READWRITE)) {
        result = sn_insert_xref(ref_to_symbol_type,
                 ref_from_scope_type,
                 ref_to_symbol_scope,
                 NULL,
                 current_function,
                 NULL,
                 NULL,
                 varname,
                 "UNDECLARED",
                 sn_current_file(),
                 line_start,
                 SN_REF_READ);

        assert(result == 0);
    }

    if ((acc == VAR_WRITE) || (acc == VAR_READWRITE)) {
        result = sn_insert_xref(ref_to_symbol_type,
                 ref_from_scope_type,
                 ref_to_symbol_scope,
                 NULL,
                 current_function,
                 NULL,
                 NULL,
                 varname,
                 "UNDECLARED",
                 sn_current_file(),
                 line_start,
                 SN_REF_WRITE);

        assert(result == 0);
    }
  }

  if (ref_to_symbol_type == SN_REF_TO_GLOB_VAR) {
      sn_highlight(SN_HIGH_VAR_GLOBAL,
          line_start, column_start,
          line_end, column_end);
  } else {
      sn_highlight(SN_HIGH_VAR_LOCAL,
          line_start, column_start,
          line_end, column_end);
  }
}

void
reset()
{
  assert(!current_function);
  sn_reset_line();
  sn_reset_column();
  sn_reset_encoding();
}

int
main(int argc, char *argv[])
{
  return sn_main(argc, argv, group, &yyin, yylex, reset);
}
