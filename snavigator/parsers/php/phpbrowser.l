/* vim: set expandtab tabstop=4 shiftwidth=4 softtabstop=4 foldmethod=marker: */

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
* Lex input file for PHP version 5 language processor.
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
    PERIOD,        /* '.' */
    COLON_CHAR,    /* ':' open brace-like, or part of conditional, i.e., ?: */
    ATSIGN_CHAR,   /* '@' function call error suppression char */

    VARIABLE,      /* $var */
    ASSIGNMENT_OPERATOR, /* '=' */
    ASSIGNMENT_OPERATORS, /* = -= += and so on */
    COMPARISON_OPERATORS, /* == === != >= and so on */
    INCREMENT_OPERATORS,  /* ++ and -- */
    REFERENCE_OPERATOR,   /* & */
    UNI_OPERATORS,   /* -,+,!,~ */
    CLASS_SCOPE_OPERATOR,   /* :: */
    CLASS_MEMBER_OPERATOR,   /* -> */
    MAPPING_OPERATOR,   /* =>  usually used in array init */

    SOMEWORD,          /* a sequence of text */
    KEYWORD,       /* a key sequence in this language */
    FUNCTION_KEYWORD, /* The keyword "function" */
    GLOBAL_KEYWORD, /* The keyword "global" */
    INCLUDE_KEYWORD, /* The keyword "include" */
    CLASS_KEYWORD, /* declaration of a class */
    CLASS_EXTENDS_KEYWORD, /* declaration of a derived class */
    NEW_KEYWORD, /* instantiation of a class into an object */
    ARRAY_KEYWORD, /* keyword "array" */
    DEFINE_KEYWORD, /* define(constant-stringname,mixval,bool) */
    ABSTRACT_KEYWORD,  /* abstract class or member function  */
    TRY_KEYWORD, /* try {} catch() {} catch() {} ... */
    CATCH_KEYWORD, /* try {} catch() {} catch() {} .. .*/
    PUBLIC_KEYWORD,  /* visible declared public  */
    PROTECTED_KEYWORD,  /* visiblity declared protected */
    PRIVATE_KEYWORD,  /* visiblity declared private */
    STATIC_KEYWORD,  /* deref using class scope only */
    CONST_KEYWORD,  /* class constants only */
    CLASS_IMPLEMENTS_KEYWORD,  /* class declaring implementation */

    THIS_VARIABLE, /* Special class $this variable */
    PARENT_VARIABLE, /* Special class $parent variable */

    VDOUBLE_QUOTED_STRING, /* "I am a $variable string" */
    DOUBLE_QUOTED_STRING, /* "I am a double quoted string" */
    SINGLE_QUOTED_STRING,  /* 'I am a double quoted string' */

    COMMENT_TOKEN,        /* Emit COMMENT token only when debugging */
    HTML_TOKEN,            /* Emit HTML token only when debugging */


    UNKNOWN_TOKEN        /* Token that parser does not know about */

} TokenType;

typedef enum {
    VAR_READ,
    VAR_WRITE,
    VAR_READWRITE,
    VAR_CLASS_DEREF
} VarAccess;

typedef struct Token {
    TokenType type;
    char * strval; /* String value of token, NULL if single character */
    long start_line;
    int start_column;
    long end_line;
    int end_column;
    struct Token* vars; /* for variables embedded in a DOUBLE_QUOTED_STRING token */
    struct Token* next;
} Token;

int recent_vis;    /* recently consolidated visibility spec for class member */
int recent_vis_toknum;    /* last token number of consolidated visibility spec */

typedef struct NestedBracket {
    int count;
    int prefix_incr;
    struct Token* var;
    struct NestedBracket* next;
} NestedBracket;

typedef struct NestedFunction {
    char* name;
    char* args;
    int   high_line;
    int   high_col_start;
    int   high_col_end;
    int   line_start;
    int   line_end;
    int   col_start;
    int   col_end;
    struct NestedFunction* next;
} NestedFunction;

typedef struct NestedBrace {
    TokenType btype;
    struct NestedBrace* next;
} NestedBrace;


/* Uncomment this to see debug output in token parsing stage */
/* #define TOKEN_DEBUG */

#ifdef TOKEN_DEBUG
FILE* tokenout = NULL;
#endif

static char * token_dump_file = NULL;
static int highlight_file = 0;

Token* tokens_head = NULL;
Token* tokens_tail = NULL;
int token_index = 0; /* current offset from original token_head */

static char group[] = "php";

#define MAX_SIZE 512

/* FIXME: Can't pass NULL or "" as current function (core dump) */
static char* current_function = (char *) NULL;
static char* current_function_args = (char *) NULL;
/* more workaround for forcing non-null function name */
static char* current_refblock = (char *) NULL;

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

/* (abstract)?(public|protected|private)?(static)? */
static int  current_function_vis;

static NestedFunction* function_nest_head = NULL;
static NestedFunction* function_nest_tail = NULL;

static NestedBrace* brace_nest_head = NULL;
static NestedBrace* brace_nest_tail = NULL;


static int current_array_bracket_count = 0;
static Token* current_array_token = NULL;
static int current_array_prefix_incr = 0;
static NestedBracket* array_nest_head = NULL;
static NestedBracket* array_nest_tail = NULL;

static char* current_class = (char *) NULL;
static char* current_parent = (char *) NULL;

static int result;

static SearchTable * global_var_table = (SearchTable *) NULL;
static SearchTable * super_global_var_table;

/* Stores the contents of a special processing mode over multiple lines/rules.  */
LongString mode_buff;
long mode_start_line;
int mode_start_column;

#define COMMENT_DUMP 0
#define DQSTRING_DUMP 0
#define SQSTRING_DUMP 0
#define HDSTRING_DUMP 0
#define HTML_DUMP 0
#define MATCH_DUMP 0

static YY_BUFFER_STATE original_buffer;

static char* heredoc_id = (char *) NULL; /* End marker for a heredoc string */

static Token* embedded_dq_string_vars_head = NULL;
static Token* embedded_dq_string_vars_tail = NULL;

#if MATCH_DUMP
static void matched_pattern(char * pattern, char * text);
#endif

void enter_html_mode();

static char* modestring();

static void FreeGlobalEntry(SearchEntry *entry);

void FreeToken(Token* tok);
Token* pop_head_token();
void free_head_token();
void push_bracket_counter();
void pop_bracket_counter();
void push_brace(TokenType braceType);
TokenType pop_brace();
void push_function();
void pop_function();
void append_token(TokenType type, char* strval, long start_line,
    int start_column, long end_line, int end_column);

void append_dqstring_var_token(char* var, long start_line, int start_column,
    long end_line, int end_column);

void append_dqstring_token(char* strval, long start_line, int start_column,
    long end_line, int end_column);

void emit_var_access_for_dqstring(Token* tok);

int from_what_scope();
void reset_vis();
void set_vis();

char * TokenTypeToString(Token *tok);

int get_num_tokens_matched(char *match);

void emit_function_declaration();
void emit_comment();
void emit_dqstring();
void emit_sqstring();
void emit_hdstring();
void emit_html();
void emit_var_access(Token* tok, VarAccess acc);

int yywrap() { return 1; }

%}

%x COMMENT_MODE
%x HTML_MODE
%x DQSTRING
%s SQSTRING
%s HDSTRING
%x PHP
%x TOKEN

ws        [ \t]
wsn        [ \t\n]
symbol        [a-zA-Z0-9_]+

varname        {symbol}
someword    {symbol}
token        {symbol}
heredoc-id    {symbol}

assignment-operators    ("+="|"-="|"*="|"/="|".="|"%="|"&="|"|="|"^="|"<<="|">>=")
comparison-operators    ("=="|"==="|"!="|"!=="|"<>"|"<"|">"|"<="|">=")
increment-operators     ("++"|"--")
uni-operators    ("+"|"!"|"-"|"~")

any-tokens    (" "|" "{symbol}" "|" "{symbol}" "({symbol}" ")*)

include-keywords    ("include"|"include_once"|"require"|"require_once")

keywords    ("and"|"or"|"xor"|"as"|"break"|"case"|"cfunction"|"clone"|"continue"|"declare"|"default"|"die"|"do"|"echo"|"else"|"elseif"|"empty"|"enddeclare"|"endfor"|"endforeach"|"endif"|"endswitch"|"endwhile"|"eval"|"exit"|"false"|"final"|"for"|"foreach"|"if"|"instanceof"|"isset"|"list"|"null"|"old_function"|"print"|"return"|"switch"|"throw"|"true"|"unset"|"use"|"var"|"while"|"__FUNCTION__"|"__FILE__"|"__CLASS__"|"__LINE__")

dq-string-token  "DOUBLE_QUOTED_STRING"
sq-string-token  "SINGLE_QUOTED_STRING"
literal-token ({someword}|{someword}" CLASS_SCOPE_OPERATOR "{someword}|{dq-string-token}|{sq-string-token})
include-string-token ({dq-string-token}|{sq-string-token})

class-extends-tokens "CLASS_EXTENDS_KEYWORD "{someword}
class-implements-tokens ("CLASS_IMPLEMENTS_KEYWORD "{someword}(" COMMA "{someword})*)
class-clause-tokens ("CLASS_KEYWORD "{someword}(" "{class-extends-tokens})?(" "{class-implements-tokens})?" OPEN_BRACE")

function-argument-token  ("VARIABLE"|"REFERENCE_OPERATOR VARIABLE"|"UNI_OPERATORS VARIABLE"|"VARIABLE ASSIGNMENT_OPERATOR "{literal-token}|"VARIABLE ASSIGNMENT_OPERATOR UNI_OPERATORS "{literal-token}|"VARIABLE ASSIGNMENT_OPERATOR ARRAY_KEYWORD OPEN_PAREN CLOSE_PAREN"|"SOMEWORD VARIABLE"|{literal-token})
function-argument-tokens (" "|" "{function-argument-token}" "("COMMA "{function-argument-token}" ")*)

variable-tokens    (" VARIABLE "|" VARIABLE "("COMMA VARIABLE ")+)


%%
    /* Start in HTML mode */
    highlight_file = (int) sn_getopt(SN_OPT_HIGHLIGHT);
    token_dump_file = (char *) sn_getopt(SN_OPT_DUMP_TOKENS);
    enter_html_mode();

<PHP>"?>" { /* PHP -> HTML mode */
    #if MATCH_DUMP
    matched_pattern("?>", yytext);
    #endif

    enter_html_mode();

    if (token_dump_file) {
        mode_buff.append(&mode_buff, yytext, yyleng);
    }

    sn_advance_column(2);
}

<HTML_MODE>{
    [^\<\n]* {
        #if MATCH_DUMP
        matched_pattern("[^\\<\\n]*", yytext);
        #endif

        #if HTML_DUMP
        fprintf(stderr, "html(1) \"%s\", %d\n", yytext, yyleng);
        #endif

        if (token_dump_file) {
            mode_buff.append(&mode_buff, yytext, yyleng);
        }
        sn_advance_column(yyleng);
    }
    [^\<\n]*\n {
        #if MATCH_DUMP
        matched_pattern("[^\\<\\n]*\\n", yytext);
        #endif

        #if HTML_DUMP
        fprintf(stderr, "html(2) \"%s\", %d\n", yytext, yyleng);
        #endif

        if (token_dump_file) {
            mode_buff.append(&mode_buff, yytext, yyleng);
        }
        sn_advance_line();
        sn_reset_column();
    }
    "<" {
        #if MATCH_DUMP
        matched_pattern("<", yytext);
        #endif

        #if HTML_DUMP
        fprintf(stderr, "html(3) \"%s\", %d\n", yytext, yyleng);
        #endif

        if (token_dump_file) {
            mode_buff.append(&mode_buff, yytext, yyleng);
        }
        sn_advance_column(yyleng);
    }
    ("<?"|"<?="|"<?php"|"<?PHP") {
        /* HTML -> PHP mode switch */
        #if MATCH_DUMP
        matched_pattern("(<?|<?=|<?php|<?PHP)", yytext);
        #endif
        sn_advance_column(yyleng);

        if (token_dump_file) {
            mode_buff.append(&mode_buff, yytext, yyleng);
        }

        emit_html();
        BEGIN(PHP);
    }
    /* Uncomment the next rule if you want to check to make sure
    * the above rules cover all possible input. A warning
    * "rule cannot be matched" should be printed by flex.
    */
    /*. {}*/
}

<PHP>("//"|"#").*("\n"|"?>") {
    char * x = (char *) yytext;
    char * y;
    int newline = 0;

    #if MATCH_DUMP
    matched_pattern("(//|#).*\\n", yytext);
    #endif

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

    /* If dumping tokens, emit a special COMMENT token.
    * Otherwise, insert a comment symbol and a highlight.
    */
    if (token_dump_file) {
        append_token(COMMENT_TOKEN, x, sn_line(), sn_column() - (x - yytext),
            sn_line(), sn_column() + strlen(x));
    } else {
        sn_insert_comment(
            /* classname */ NULL,
            /* funcname */ NULL,
            sn_current_file(),
            x,
            sn_line(),
            sn_column());

        sn_highlight(SN_HIGH_COMMENT,
            sn_line(),
            sn_column() - (x - yytext),
            sn_line(),
            sn_column() + strlen(x));
    }

    if (newline) {
        sn_advance_line();
        sn_reset_column();
    } else {
        sn_advance_column(yyleng - (x - yytext));
    }
}

<PHP>"/*"    { /* A C style multi-line comment, just like this! */
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
        mode_buff.append(&mode_buff, yytext, yyleng);
        sn_advance_column(yyleng);
    }
    [^\*\n]*\n {
        #if MATCH_DUMP
        matched_pattern("[^\\*\\n]*\\n", yytext);
        #endif

        #if COMMENT_DUMP
        fprintf(stderr, "comment(2) \"%s\", %d\n", yytext, yyleng);
        #endif

        mode_buff.append(&mode_buff, yytext, yyleng);
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

        mode_buff.append(&mode_buff, yytext, yyleng);
        sn_advance_column(yyleng);
    }
    \*+[^\*/\n]*\n {
        #if MATCH_DUMP
        matched_pattern("\\*+[^\\*/\\n]*\\n", yytext);
        #endif

        #if COMMENT_DUMP
        fprintf(stderr, "comment(4) \"%s\", %d\n", yytext, yyleng);
        #endif

        mode_buff.append(&mode_buff, yytext, yyleng);
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
            mode_buff.append(&mode_buff, yytext, to_append);
        }

        emit_comment();
        sn_advance_column(yyleng);
        BEGIN(PHP);
    }
    /* Uncomment the next rule if you want to check to make sure
    * the above rules cover all possible input. A warning
    * "rule cannot be matched" should be printed by flex.
    */
    /*. {}*/
}

<PHP>\"\" {
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

<PHP>\" {
    #if MATCH_DUMP
    matched_pattern("\\\"", yytext);
    #endif

    #if DQSTRING_DUMP
    fprintf(stderr, "dqstring started at (%d.%d)\n", sn_line(), sn_column());
    #endif
    LongStringInit(&mode_buff,0);
    mode_start_line = sn_line();
    mode_start_column = sn_column();
    embedded_dq_string_vars_head = NULL;
    embedded_dq_string_vars_tail = NULL;
    sn_advance_column(yyleng);
    BEGIN(DQSTRING);
}

<DQSTRING>{
    [^\"\n\\\$]* {
        #if MATCH_DUMP
        matched_pattern("[^\\\"\\n\\\\]*", yytext);
        #endif

        #if DQSTRING_DUMP
        fprintf(stderr, "dqstring(1) \"%s\", %d\n", yytext, yyleng);
        #endif
        mode_buff.append(&mode_buff, yytext, yyleng);
        sn_advance_column(yyleng);
    }
    [^\"\n\\\$]*\n {
        #if MATCH_DUMP
        matched_pattern("[^\\\"\\n\\\\]*\\n", yytext);
        #endif

        #if DQSTRING_DUMP
        fprintf(stderr, "dqstring(2) \"%s\", %d\n", yytext, yyleng);
        #endif
        mode_buff.append(&mode_buff, yytext, yyleng);
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

        mode_buff.append(&mode_buff, yytext, yyleng);
        sn_advance_column(yyleng);
    }
    \\\\ {
        #if MATCH_DUMP
        matched_pattern("\\\\", yytext);
        #endif

        #if DQSTRING_DUMP
        fprintf(stderr, "dqstring(4) \"%s\", %d\n", yytext, yyleng);
        #endif

        mode_buff.append(&mode_buff, yytext, yyleng);
        sn_advance_column(yyleng);
    }
    \\ {
        #if MATCH_DUMP
        matched_pattern("\\", yytext);
        #endif

        #if DQSTRING_DUMP
        fprintf(stderr, "dqstring(5) \"%s\", %d\n", yytext, yyleng);
        #endif

        mode_buff.append(&mode_buff, yytext, yyleng);
        sn_advance_column(yyleng);
    }
    \${varname} {
        #if MATCH_DUMP
        matched_pattern("${varname}", yytext);
        #endif

        append_dqstring_var_token(yytext+1,
            sn_line(),
            sn_column(),
            sn_line(),
            sn_column() + yyleng);

        mode_buff.append(&mode_buff, yytext, yyleng);

        sn_advance_column(yyleng);
    }
    \$ {
        #if MATCH_DUMP
        matched_pattern("$", yytext);
        #endif

        mode_buff.append(&mode_buff, yytext, yyleng);
        sn_advance_column(yyleng);
    }
    \" {
        #if MATCH_DUMP
        matched_pattern("\\\"", yytext);
        #endif

        sn_advance_column(yyleng);
        emit_dqstring();
        BEGIN(PHP);
    }
    /* Uncomment the next rule if you want to check to make sure
    * the above rules cover all possible input. A warning
    * "rule cannot be matched" should be printed by flex.
    */
    /*. {}*/
}

<PHP>\'\' {
    #if MATCH_DUMP
    matched_pattern("\\\'\\\'", yytext);
    #endif

    /* FIXME: Can we pass NULL instead of "" after length issues worked out ? */
    append_token(SINGLE_QUOTED_STRING, "",
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>\' {
    #if MATCH_DUMP
    matched_pattern("\\\'", yytext);
    #endif

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
        #if MATCH_DUMP
        matched_pattern("[^\\\'\\n\\\\]*", yytext);
        #endif

        #if SQSTRING_DUMP
        fprintf(stderr, "sqstring(1) \"%s\", %d\n", yytext, yyleng);
        #endif
        mode_buff.append(&mode_buff, yytext, yyleng);
        sn_advance_column(yyleng);
    }
    [^\'\n\\]*\n {
        #if MATCH_DUMP
        matched_pattern("[^\\\'\\n\\\\]*\\n", yytext);
        #endif

        #if SQSTRING_DUMP
        fprintf(stderr, "sqstring(2) \"%s\", %d\n", yytext, yyleng);
        #endif

        mode_buff.append(&mode_buff, yytext, yyleng);
        sn_advance_line();
        sn_reset_column();
    }
    (\\\')+[^\'/\n\\]* {
        #if MATCH_DUMP
        matched_pattern("\\\'+[^\\\'/\\n\\\\]*", yytext);
        #endif

        #if SQSTRING_DUMP
        fprintf(stderr, "sqstring(3) \"%s\", %d\n", yytext, yyleng);
        #endif

        mode_buff.append(&mode_buff, yytext, yyleng);
        sn_advance_column(yyleng);
    }
    (\\\')+[^\'\n\\]*\n {
        #if MATCH_DUMP
        matched_pattern("\\\'+[^\\\'\\n\\\\]*\\n", yytext);
        #endif

        #if SQSTRING_DUMP
        fprintf(stderr, "sqstring(4) \"%s\", %d\n", yytext, yyleng);
        #endif

        mode_buff.append(&mode_buff, yytext, yyleng);
        sn_advance_line();
        sn_reset_column();
    }
    \\\\ {
        #if MATCH_DUMP
        matched_pattern("\\\\", yytext);
        #endif

        #if SQSTRING_DUMP
        fprintf(stderr, "sqstring(5) \"%s\", %d\n", yytext, yyleng);
        #endif


        mode_buff.append(&mode_buff, yytext, yyleng);
        sn_advance_column(yyleng);
    }
    \\ {
        #if MATCH_DUMP
        matched_pattern("\\", yytext);
        #endif

        #if SQSTRING_DUMP
        fprintf(stderr, "sqstring(6) \"%s\", %d\n", yytext, yyleng);
        #endif


        mode_buff.append(&mode_buff, yytext, yyleng);
        sn_advance_column(yyleng);
    }
    \' {
        #if MATCH_DUMP
        matched_pattern("\\\'", yytext);
        #endif

        sn_advance_column(yyleng);
        emit_sqstring();
        BEGIN(PHP);
    }
    /* Uncomment the next rule if you want to check to make sure
    * the above rules cover all possible input. A warning
    * "rule cannot be matched" should be printed by flex.
    */
    /*. {}*/
}

<PHP>"<<<"{ws}*{heredoc-id}\n {
    char *x = yytext;
    char *y;

    #if MATCH_DUMP
    matched_pattern("<<<{ws}*{heredoc-id}\\n", yytext);
    #endif

    mode_start_line = sn_line();
    mode_start_column = sn_column();

    x += 3;
    while(*x == ' ' || *x == '\t') { x++; }
    y = x;
    while(*y != '\n') { y++; }
    *y = 0;

    LongStringInit(&mode_buff,0);
    mode_buff.append( &mode_buff, x, -1 );
    mode_buff.append( &mode_buff, ";\n", -1 );
    heredoc_id = SN_StrDup(mode_buff.buf);
    mode_buff.free(&mode_buff);

    #if HDSTRING_DUMP
    fprintf(stderr, "hdstring started at (%d.%d)\n", sn_line(), sn_column());
    fprintf(stderr, "hdstring id is \"%s\"\n", heredoc_id);
    #endif

    LongStringInit(&mode_buff,0);

    embedded_dq_string_vars_head = NULL;
    embedded_dq_string_vars_tail = NULL;

    sn_advance_line();
    sn_reset_column();

    BEGIN(HDSTRING);
}

<HDSTRING>{
    ^{heredoc-id};\n {
        #if MATCH_DUMP
        matched_pattern("^{heredoc-id};\n", yytext);
        #endif

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
            mode_buff.append(&mode_buff, yytext, yyleng);
        }
    }
    [^\$\n]+ {
        #if MATCH_DUMP
        matched_pattern("[^;$\n]+", yytext);
        #endif

        #if HDSTRING_DUMP
        fprintf(stderr, "hdstring(2) \"%s\", %d\n", yytext, yyleng);
        #endif

        mode_buff.append(&mode_buff, yytext, yyleng);

        sn_advance_column(yyleng);
    }
    [^\$\n]*\n {
        #if MATCH_DUMP
        matched_pattern("[^;$\n]*\n", yytext);
        #endif

        #if HDSTRING_DUMP
        fprintf(stderr, "hdstring(3) \"%s\", %d\n", yytext, yyleng);
        #endif

        mode_buff.append(&mode_buff, yytext, yyleng);

        sn_advance_line();
        sn_reset_column();
    }
    \${varname} {
        #if MATCH_DUMP
        matched_pattern("${varname}", yytext);
        #endif

        #if HDSTRING_DUMP
        fprintf(stderr, "hdstring(4) \"%s\", %d\n", yytext, yyleng);
        #endif

        append_dqstring_var_token(yytext+1,
            sn_line(),
            sn_column(),
            sn_line(),
            sn_column() + yyleng);

        mode_buff.append(&mode_buff, yytext, yyleng);

        sn_advance_column(yyleng);
    }
    \$ {
        #if MATCH_DUMP
        matched_pattern("$", yytext);
        #endif

        #if HDSTRING_DUMP
        fprintf(stderr, "hdstring(5) \"%s\", %d\n", yytext, yyleng);
        #endif

        mode_buff.append(&mode_buff, yytext, yyleng);
        sn_advance_column(yyleng);
    }
    /* Uncomment the next rule if you want to check to make sure
    * the above rules cover all possible input. A warning
    * "rule cannot be matched" should be printed by flex.
    */
    /*. {}*/
}

<PHP>"(" {
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

<PHP>")" {
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

<PHP>"[" {
    #if MATCH_DUMP
    matched_pattern("[", yytext);
    #endif
    append_token(OPEN_BRACKET, NULL,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"]" {
    #if MATCH_DUMP
    matched_pattern("]", yytext);
    #endif
    append_token(CLOSE_BRACKET, NULL,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"{" {
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

<PHP>":" {
    #if MATCH_DUMP
    matched_pattern(":", yytext);
    #endif
    append_token(COLON_CHAR, NULL,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"}" {
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

<PHP>";" {
    #if MATCH_DUMP
    matched_pattern("}", yytext);
    #endif
    append_token(SEMICOLON, NULL,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"@" {
    #if MATCH_DUMP
    matched_pattern("@", yytext);
    #endif
    append_token(ATSIGN_CHAR, NULL,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"." {
    #if MATCH_DUMP
    matched_pattern(".", yytext);
    #endif
    append_token(PERIOD, NULL,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"," {
    #if MATCH_DUMP
    matched_pattern(",", yytext);
    #endif
    append_token(COMMA, NULL,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"=>" {
    #if MATCH_DUMP
    matched_pattern("=>", yytext);
    #endif
    append_token(MAPPING_OPERATOR, NULL,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"=" {
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

<PHP>"&" {
    #if MATCH_DUMP
    matched_pattern("&", yytext);
    #endif
    append_token(REFERENCE_OPERATOR, NULL,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"::" {
    #if MATCH_DUMP
    matched_pattern("::", yytext);
    #endif
    append_token(CLASS_SCOPE_OPERATOR, NULL,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"->" {
    #if MATCH_DUMP
    matched_pattern("->", yytext);
    #endif
    append_token(CLASS_MEMBER_OPERATOR, NULL,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>{uni-operators} {
    #if MATCH_DUMP
    matched_pattern("{uni-operators}", yytext);
    #endif

    append_token(UNI_OPERATORS, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>{comparison-operators} {
    #if MATCH_DUMP
    matched_pattern("{comparison-operators}", yytext);
    #endif

    append_token(COMPARISON_OPERATORS, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>{assignment-operators} {
    #if MATCH_DUMP
    matched_pattern("{assignment-operators}", yytext);
    #endif

    append_token(ASSIGNMENT_OPERATORS, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>{increment-operators} {
    #if MATCH_DUMP
    matched_pattern("{increment-operators}", yytext);
    #endif

    append_token(INCREMENT_OPERATORS, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"function" {
    #if MATCH_DUMP
    matched_pattern("function", yytext);
    #endif

    append_token(FUNCTION_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"global" {
    #if MATCH_DUMP
    matched_pattern("global", yytext);
    #endif
    append_token(GLOBAL_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"class" {
    #if MATCH_DUMP
    matched_pattern("class", yytext);
    #endif
    append_token(CLASS_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"extends" {
    #if MATCH_DUMP
    matched_pattern("extends", yytext);
    #endif
    append_token(CLASS_EXTENDS_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"implements" {
    #if MATCH_DUMP
    matched_pattern("extends", yytext);
    #endif
    append_token(CLASS_IMPLEMENTS_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}


<PHP>"interface" {
    #if MATCH_DUMP
    matched_pattern("interface", yytext);
    #endif
    append_token(CLASS_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}


<PHP>"new" {
    #if MATCH_DUMP
    matched_pattern("new", yytext);
    #endif
    append_token(NEW_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"array" {
    #if MATCH_DUMP
    matched_pattern("array", yytext);
    #endif
    append_token(ARRAY_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"define" {
    #if MATCH_DUMP
    matched_pattern("define", yytext);
    #endif
    append_token(DEFINE_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}


<PHP>"try" {
    #if MATCH_DUMP
    matched_pattern("try", yytext);
    #endif
    append_token(TRY_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"catch" {
    #if MATCH_DUMP
    matched_pattern("catch", yytext);
    #endif
    append_token(CATCH_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"abstract" {
    #if MATCH_DUMP
    matched_pattern("public", yytext);
    #endif
    append_token(ABSTRACT_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"public" {
    #if MATCH_DUMP
    matched_pattern("public", yytext);
    #endif
    append_token(PUBLIC_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"protected" {
    #if MATCH_DUMP
    matched_pattern("protected", yytext);
    #endif
    append_token(PROTECTED_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"private" {
    #if MATCH_DUMP
    matched_pattern("private", yytext);
    #endif
    append_token(PRIVATE_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"static" {
    #if MATCH_DUMP
    matched_pattern("static", yytext);
    #endif
    append_token(STATIC_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>"const" {
    #if MATCH_DUMP
    matched_pattern("const", yytext);
    #endif
    append_token(CONST_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>{include-keywords} {
    #if MATCH_DUMP
    matched_pattern("{include-keywords}", yytext);
    #endif
    append_token(INCLUDE_KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>{keywords} {
    #if MATCH_DUMP
    matched_pattern("{keywords}", yytext);
    #endif
    /* NOTE: verify that words used in type casting not reserved? */
    append_token(KEYWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>{someword} {
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

<PHP>\$"this" {
    #if MATCH_DUMP
    matched_pattern("$this", yytext);
    #endif
    append_token(THIS_VARIABLE, yytext+1,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>\$"parent" {
    #if MATCH_DUMP
    matched_pattern("$parent", yytext);
    #endif
    append_token(PARENT_VARIABLE, yytext+1,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>\${varname} {
    #if MATCH_DUMP
    matched_pattern("${varname}", yytext);
    #endif
    append_token(VARIABLE, yytext+1,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);
        sn_advance_column(yyleng);
}

<PHP>{wsn}+ {
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

<PHP>\\\$ {
    #if MATCH_DUMP
    matched_pattern("\\$", yytext);
    #endif

    append_token(SOMEWORD, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);

    sn_advance_column(yyleng);
}

<PHP>. {
    #if MATCH_DUMP
    matched_pattern(".", yytext);
    #endif

    /* Add an UNKNOWN_TOKEN for each
    * character that we don't know
    * how to deal with.
    */

    append_token(UNKNOWN_TOKEN, yytext,
        sn_line(),
        sn_column(),
        sn_line(),
        sn_column() + yyleng);

    /*fprintf(stderr, "adding unknown token for \"%s\"\n", yytext);*/

    sn_advance_column(yyleng); /* eat text */
}

<TOKEN>"DEFINE_KEYWORD OPEN_PAREN "{include-string-token} {
    int line_start, line_end, column_start, column_end;

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "define token %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif
    sn_highlight(SN_HIGH_KEYWORD,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);

    free_head_token(); /* define */
    free_head_token(); /* OPEN_PAREN */

    sn_highlight(SN_HIGH_STRING,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);

    line_start = tokens_head->start_line;
    column_start = tokens_head->start_column + 1;
    line_end = tokens_head->end_line;
    column_end = tokens_head->end_column - 1;
    /* defines are considered global, even within class definitions */
    result = sn_insert_symbol(SN_MACRO_DEF,
        /* classname */ NULL,
        tokens_head->strval,
        sn_current_file(),
        line_start, column_start,
        line_end, column_end,
        /* attr */ SN_PUBLIC,
        /* ret type */ NULL,
        /* arg_types */ NULL,
        /* arg_names */ NULL,
        /* comment */ NULL,
        line_start, column_start,
        line_end, column_end);
    assert(result == 0);

    free_head_token(); /* STRING */
}


<TOKEN>"ARRAY_KEYWORD OPEN_PAREN" {
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "array init tokens %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif
    sn_highlight(SN_HIGH_KEYWORD,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);
        free_head_token(); /* array keyword */
        free_head_token(); /* OPEN_PAREN */
}

<TOKEN>{literal-token}" MAPPING_OPERATOR" {
    int line_start, line_end, column_start, column_end;
    TokenType prevToken;
    int ref_from_scope_type;
    char *clsname = NULL;
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "array mapping token %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif
    prevToken = UNKNOWN_TOKEN;
    while (tokens_head->type != MAPPING_OPERATOR) {
        if ((tokens_head->type == DOUBLE_QUOTED_STRING) ||
            (tokens_head->type == SINGLE_QUOTED_STRING)) {
                sn_highlight(SN_HIGH_STRING,
                    tokens_head->start_line, tokens_head->start_column,
                    tokens_head->end_line, tokens_head->end_column);
        } else if (tokens_head->type == SOMEWORD) {
            /* cheat with token lookback to highlight class references */
            line_start = tokens_head->start_line;
            column_start = tokens_head->start_column;
            line_end = tokens_head->end_line;
            column_end = tokens_head->end_column;
            if (strcmp(tokens_head->strval, "parent") && strcmp(tokens_head->strval, "self"))
                clsname = SN_StrDup(tokens_head->strval);
        } else if (tokens_head->type == CLASS_SCOPE_OPERATOR) {
            if ((prevToken == SOMEWORD) && clsname) {
                sn_highlight(SN_HIGH_CLASSDEF, line_start, column_start, line_end, column_end);
                ref_from_scope_type = from_what_scope();

                /* NOTE: we use '#' as the refclass so the GUI will point to the class def */
                result = sn_insert_xref(SN_REF_TO_CLASS,
                    ref_from_scope_type,
                    SN_REF_SCOPE_GLOBAL,
                    current_class,
                    current_refblock,
                    NULL,
                    "#",
                    clsname,
                    NULL, 
                    sn_current_file(),
                    line_start,
                    SN_REF_READ);

                assert(result == 0);
                ckfree(clsname);
                clsname = NULL;
            }
        }
        prevToken = tokens_head->type;
        free_head_token();
    }
    free_head_token(); /* MAPPING_OPERATOR */
    if (clsname)
    ckfree(clsname);
}


<TOKEN>"FUNCTION_KEYWORD "("REFERENCE_OPERATOR ")?"SOMEWORD OPEN_PAREN"{function-argument-tokens}"CLOSE_PAREN "("OPEN_BRACE"|"SEMICOLON") {
    int parens, noargs;
    LongString abuff;

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "function at token %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif
    assert(tokens_head->type == FUNCTION_KEYWORD);

    sn_highlight(SN_HIGH_KEYWORD,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);

    free_head_token(); /* FUNCTION_KEYWORD */

    if (tokens_head->type == REFERENCE_OPERATOR)
        free_head_token(); /* REFERENCE_OPERATOR */

    sn_highlight(SN_HIGH_FUNCTION,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);

    if (current_function != NULL) {
        push_function();
    }
    current_function = SN_StrDup(tokens_head->strval);

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "start of function %s at token %d\n", current_function, token_index);
    #endif

    current_function_line_start = tokens_head->start_line;
    current_function_column_start = tokens_head->start_column;
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
                abuff.append(&abuff, ",", -1);
            }
            abuff.append(&abuff, tokens_head->strval, -1);
            noargs = 0;
            #ifdef ALLOW_LOCAL_VAR_HIGHLIGHT
            sn_highlight(SN_HIGH_VAR_LOCAL,
                tokens_head->start_line, tokens_head->start_column,
                tokens_head->end_line, tokens_head->end_column);
            #endif
        } else if ((tokens_head->type == DOUBLE_QUOTED_STRING) ||
            (tokens_head->type == SINGLE_QUOTED_STRING)) {
                sn_highlight(SN_HIGH_STRING,
                    tokens_head->start_line, tokens_head->start_column,
                    tokens_head->end_line, tokens_head->end_column);
        } else if ((tokens_head->type == KEYWORD) ||
            (tokens_head->type == ARRAY_KEYWORD)) {
                sn_highlight(SN_HIGH_KEYWORD,
                tokens_head->start_line, tokens_head->start_column,
                tokens_head->end_line, tokens_head->end_column);
        }
        free_head_token(); /* OPEN_PAREN || CLOSE_PAREN || VARIABLE || COMMA */
    }

    #ifdef TOKEN_DEBUG
    if (current_function_args) {
        fprintf(tokenout, "Oops, function %s left over function args?", current_function);
    }
    #endif

    assert(current_function_args == NULL);
    if (!noargs) {
        current_function_args = SN_StrDup(abuff.buf);
    }
    abuff.free(&abuff);

    if (tokens_head->type == OPEN_BRACE) {
        free_head_token(); /* OPEN_BRACE */
        push_brace(FUNCTION_KEYWORD);
    } else {
        free_head_token(); /* SEMICOLON */
        pop_function();    /* undo push_function() for declarations */
        emit_function_declaration();
    }
}

<TOKEN>"TRY_KEYWORD OPEN_BRACE" {
    sn_highlight(SN_HIGH_KEYWORD,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);
    push_brace(TRY_KEYWORD);
    free_head_token(); /* TRY keyword */
    free_head_token(); /* OPEN_BRACE */
}

<TOKEN>"CATCH_KEYWORD OPEN_PAREN"{function-argument-tokens}"CLOSE_PAREN OPEN_BRACE " {
    int parens, noargs;
    LongString abuff;

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "catch block at token %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif

    sn_highlight(SN_HIGH_KEYWORD,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);

    free_head_token(); /* CATCH_KEYWORD */
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
        } else if ((tokens_head->type == DOUBLE_QUOTED_STRING) ||
            (tokens_head->type == SINGLE_QUOTED_STRING)) {
                sn_highlight(SN_HIGH_STRING,
                    tokens_head->start_line, tokens_head->start_column,
                    tokens_head->end_line, tokens_head->end_column);
        } else if (tokens_head->type == SOMEWORD) {
            sn_highlight(SN_HIGH_CLASSDEF,
                tokens_head->start_line, tokens_head->start_column,
                tokens_head->end_line, tokens_head->end_column);
        } /* else { } */

        free_head_token(); /* anything upto and including final CLOSE_PAREN */
    }

    push_brace(CATCH_KEYWORD);
    free_head_token(); /* OPEN_BRACE */
}

<TOKEN>"OPEN_BRACE" {
    push_brace(UNKNOWN_TOKEN);
    free_head_token(); /* OPEN_BRACE */
}

<TOKEN>"CLOSE_BRACE" {
    TokenType braceMatch;

    braceMatch = pop_brace();
    if (braceMatch == FUNCTION_KEYWORD) {
        #ifdef TOKEN_DEBUG
        fprintf(tokenout, "end of function %s at token %d\n",
        current_function, token_index);
        #endif

        current_function_line_end = tokens_head->end_line;
        current_function_column_end = tokens_head->end_column;

        emit_function_declaration();

        pop_function(); /* in case of nested function */
    } else if (braceMatch == CLASS_KEYWORD) {
        #ifdef TOKEN_DEBUG
        fprintf(tokenout, "exit out of class %s\n", current_class);
        #endif
        ckfree((char*) current_class);
        current_class = (char *)NULL;
        if (current_parent) {
            ckfree((char*) current_parent);
            current_parent = (char *)NULL;
        }
    } else {
        /* all other brace types */
        #ifdef TOKEN_DEBUG
        fprintf(tokenout, "close brace (0x%d)\n", braceMatch);
        #endif
    }

    free_head_token(); /* CLOSE_BRACE */
    reset_vis(); /* prep for visibility specs */
}

<TOKEN>("INCLUDE_KEYWORD "{include-string-token}" SEMICOLON")|("INCLUDE_KEYWORD OPEN_PAREN "{include-string-token}" CLOSE_PAREN") {
    int line_start, line_end, column_start, column_end;
    char * filename;

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "include statement begins at token %d\n", token_index);
    #endif

    sn_highlight(SN_HIGH_KEYWORD,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);

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

    sn_highlight(SN_HIGH_STRING,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);

    free_head_token(); /* SINGLE_QUOTED_STRING || DOUBLE_QUOTED_STRING */
    free_head_token(); /* CLOSE_PAREN || SEMICOLON */
}

<TOKEN>("GLOBAL_KEYWORD"{variable-tokens}"SEMICOLON") {
    SearchEntry entry;

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "global statement found at %d\n", token_index);
    #endif

    sn_highlight(SN_HIGH_KEYWORD,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);

    free_head_token(); /* GLOBAL_KEYWORD */

    if (current_function && (global_var_table == NULL)) {
        global_var_table = SearchTableCreate(100, SEARCH_HASH_TABLE, FreeGlobalEntry);
    }

    /* Insert the variable name into the function's global table */

    while (tokens_head->type != SEMICOLON) {
        if (current_function && (tokens_head->type == VARIABLE)) {
            entry.key = tokens_head->strval;
            entry.key_len = -1;

            if (global_var_table->search( &global_var_table, entry ) == NULL) {
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

        free_head_token(); /* VARIABLE || COMMA */
    }

    free_head_token(); /* SEMICOLON */
}

<TOKEN>{class-clause-tokens} {
    int line;
    int ref_from_scope_type;

    /* NOTE: interface declarations are processed in this block also */

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "found class tokens at %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif

    assert(tokens_head->type == CLASS_KEYWORD);
    sn_highlight(SN_HIGH_KEYWORD,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);
    free_head_token(); /* CLASS keyword */

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "class name is \"%s\"\n", tokens_head->strval);
    #endif
    /* first insert the class name */
    result = sn_insert_symbol(SN_CLASS_DEF, NULL, tokens_head->strval,
        sn_current_file(),
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->start_line, tokens_head->end_column,
        recent_vis, NULL, NULL, NULL, NULL,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->start_line, tokens_head->end_column);
    assert(result == 0);

    reset_vis(); /* prep for visibility specs */

    /* now add highlight ability to editor using parser toolbox */
    sn_highlight(SN_HIGH_CLASSDEF,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);

    /* assert(current_class == (char *)NULL); */ /* removed to be more tolerant */

    current_class = SN_StrDup(tokens_head->strval);

    free_head_token(); /* SOMEWORD=CLASSNAME */
    push_brace(CLASS_KEYWORD);

    ref_from_scope_type = from_what_scope();

    while (tokens_head->type != OPEN_BRACE) {
        if (tokens_head->type == SOMEWORD) {
            /* Note: at this point we are dealing with 'interfaces' */
            sn_highlight(SN_HIGH_CLASSDEF,
                tokens_head->start_line, tokens_head->start_column,
                tokens_head->end_line, tokens_head->end_column);

            /* FIXME: insert xref here as SN_IMPLEMENTS or direct inherit like 'extends'? */

            result = sn_insert_xref(SN_REF_TO_CLASS,
                SN_MBR_FUNC_DEF,
                SN_REF_SCOPE_GLOBAL,
                current_class,
                current_refblock,
                NULL,
                "#",
                tokens_head->strval,
                NULL, 
                sn_current_file(),
                tokens_head->start_line,
                SN_REF_PASS);

            assert(result == 0);

        } else if (tokens_head->type == CLASS_EXTENDS_KEYWORD) {
            sn_highlight(SN_HIGH_KEYWORD,
                tokens_head->start_line, tokens_head->start_column,
                tokens_head->end_line, tokens_head->end_column);
            free_head_token(); /* EXTENDS */
            if (tokens_head->type == SOMEWORD) {
                sn_highlight(SN_HIGH_CLASSDEF,
                    tokens_head->start_line, tokens_head->start_column,
                    tokens_head->end_line, tokens_head->end_column);

                #ifdef TOKEN_DEBUG
                fprintf(tokenout, "derived class name is \"%s\" baseclass %s\n", current_class, tokens_head->strval);
                #endif
                result = sn_insert_symbol(SN_CLASS_INHERIT, current_class, tokens_head->strval, 
                    sn_current_file(),
                    tokens_head->start_line, tokens_head->start_column,
                    tokens_head->end_line, tokens_head->end_column,
                    SN_PUBLIC, NULL, NULL, NULL, NULL,
                    tokens_head->start_line, tokens_head->start_column,
                    tokens_head->end_line, tokens_head->end_column);

                assert(result == 0);

                /* WARNING: added hack so xref pane points back to parent class */
                result = sn_insert_xref(SN_REF_TO_CLASS,
                    SN_MBR_FUNC_DEF,
                    SN_REF_SCOPE_GLOBAL,
                    current_class,
                    current_class,
                    NULL,
                    "#",
                    tokens_head->strval,
                    NULL, 
                    sn_current_file(),
                    tokens_head->start_line,
                    SN_REF_PASS);

                assert(result == 0);

                /* Note: single predecessor for PHP classes (but zero or more interfaces) */
                /* assert(current_parent == (char *)NULL); */ /* removed to be more tolerant */
                current_parent = SN_StrDup(tokens_head->strval);
            }
        } else if (tokens_head->type == CLASS_IMPLEMENTS_KEYWORD) {
            sn_highlight(SN_HIGH_KEYWORD,
                tokens_head->start_line, tokens_head->start_column,
                tokens_head->end_line, tokens_head->end_column);
        }
        free_head_token(); /* anything before OPEN_BRACE */
    }
    if (tokens_head->type == OPEN_BRACE)
        free_head_token(); /* OPEN_BRACE */
}


<TOKEN>"NEW_KEYWORD SOMEWORD "("OPEN_PAREN"|"SEMICOLON") {
    /* FIXME: use only first two tokens for pattern 'callfunc(new blah);' */
    int ref_from_scope_type;

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "found class instantiation tokens at %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif

    sn_highlight(SN_HIGH_KEYWORD,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);
    free_head_token(); /* "new" keyword */

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "class instance name is \"%s\"\n", tokens_head->strval);
    #endif

    ref_from_scope_type = from_what_scope();

    /* NOTE: we use '#' as the refclass so the GUI will point to the class def */
    result = sn_insert_xref(SN_REF_TO_CLASS,
        ref_from_scope_type,
        SN_REF_SCOPE_GLOBAL,
        current_class,
        current_refblock,
        NULL,
        "#",
        tokens_head->strval,
        NULL, 
        sn_current_file(),
        tokens_head->start_line,
        SN_REF_PASS);

    assert(result == 0);

    sn_highlight(SN_HIGH_CLASSDEF,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);

    free_head_token(); /* SOMEWORD=class name */
    free_head_token(); /* OPEN_PAREN || SEMICOLON */
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

    ref_from_scope_type = from_what_scope();

    result = sn_insert_xref(SN_REF_TO_FUNCTION,
        ref_from_scope_type,
        SN_REF_SCOPE_GLOBAL,
        current_class,
        current_refblock,
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


<TOKEN>"VARIABLE ASSIGNMENT_OPERATOR" {
    SearchEntry entry;

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "variable assignment at token %d\n", token_index);
    #endif

    if (current_class && !current_function) {
        result = sn_insert_symbol(SN_MBR_VAR_DEF, current_class,
        tokens_head->strval, sn_current_file(),
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->start_line, tokens_head->end_column,
        recent_vis, NULL, NULL, NULL, NULL,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->start_line, tokens_head->end_column);
        assert(result == 0);
    } else if (!current_class && !current_function) {
        /* not a class member var nor a function var, so it is probably
        * an implied global var. add to global list.
        */
        if (global_var_table == NULL) {
            global_var_table = SearchTableCreate(100, SEARCH_HASH_TABLE, FreeGlobalEntry);
        }

        entry.key = tokens_head->strval;
        entry.key_len = -1;

        if (global_var_table->search( &global_var_table, entry ) == NULL) {
            result = sn_insert_symbol(SN_GLOB_VAR_DEF, current_class,
                tokens_head->strval, sn_current_file(),
                tokens_head->start_line, tokens_head->start_column,
                tokens_head->start_line, tokens_head->end_column,
                SN_PUBLIC, NULL, NULL, NULL, NULL,
                tokens_head->start_line, tokens_head->start_column,
                tokens_head->start_line, tokens_head->end_column);
            assert(result == 0);

            entry.data = NULL;
            entry.data_len = 0;
            entry.flag = SEARCH_DUP_KEY; /* add copy of entry.key */
            global_var_table->add( &global_var_table, entry );
        }
    }
    /* now add the fact that we are modifying this var */
    emit_var_access(tokens_head, VAR_WRITE);

    free_head_token(); /* VARIABLE */
    free_head_token(); /* ASSIGNMENT_OPERATOR */
}

<TOKEN>"VARIABLE ASSIGNMENT_OPERATORS" {
    int offset;

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "variable modification assignment at token %d\n", token_index);
    #endif

    emit_var_access(tokens_head, VAR_READWRITE);

    free_head_token(); /* VARIABLE */
    free_head_token(); /* ASSIGNMENT_OPERATORS */
}

<TOKEN>("INCREMENT_OPERATORS VARIABLE"|"VARIABLE INCREMENT_OPERATORS") {
    int offset, pre = 0;
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "variable increment at token %d\n", token_index);
    #endif

    if (tokens_head->type == INCREMENT_OPERATORS) {
        pre = 1;
        free_head_token(); /* INCREMENT_OPERATORS */
    }

    emit_var_access(tokens_head, VAR_READWRITE);

    free_head_token(); /* VARIABLE */
    if (!pre) {
        free_head_token(); /* INCREMENT_OPERATORS */
    }
}

<TOKEN>"CONST_KEYWORD SOMEWORD ASSIGNMENT_OPERATOR" {
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "class const token %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif
    sn_highlight(SN_HIGH_KEYWORD,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);

    free_head_token(); /* const keyword */

    if ( !current_class )
        return;

    result = sn_insert_symbol(SN_ENUM_CONST_DEF,
        current_class,
        tokens_head->strval,
        sn_current_file(),
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column,
        /* attr */ SN_PUBLIC,
        /* ret type */ NULL,
        /* arg_types */ NULL,
        /* arg_names */ NULL,
        /* comment */ NULL,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);

    assert(result == 0);

    free_head_token(); /* class constant name */
    free_head_token(); /* = */
}

<TOKEN>"THIS_VARIABLE CLASS_MEMBER_OPERATOR SOMEWORD OPEN_PAREN" {
    char* fname;
    int line;
    int ref_from_scope_type;

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "found $this class member function call tokens at %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif
    free_head_token(); /* $this */
    free_head_token(); /* -> */

    fname = tokens_head->strval;
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "function name is \"%s\"\n", fname);
    #endif
    line = tokens_head->start_line;

    ref_from_scope_type = from_what_scope();

    result = sn_insert_xref(SN_REF_TO_MBR_FUNC,
        ref_from_scope_type,
        SN_REF_SCOPE_GLOBAL,
        current_class,
        current_refblock,
        NULL,
        current_class,
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

<TOKEN>"THIS_VARIABLE CLASS_MEMBER_OPERATOR SOMEWORD" {
    int ref_from_scope_type;

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "this class variable read at token %d\n", token_index);
    #endif
    free_head_token(); /* $this */
    free_head_token(); /* -> */

    /* Remove to simplify editor, although should be instance var highlighting
    * sn_highlight(SN_HIGH_MBR_VAR,
    *     tokens_head->start_line, tokens_head->start_column,
    *     tokens_head->end_line, tokens_head->end_column);
    */

    ref_from_scope_type = from_what_scope();

    result = sn_insert_xref(SN_REF_TO_MBR_VAR,
        ref_from_scope_type,
        SN_REF_SCOPE_GLOBAL,
        current_class,
        current_refblock,
        NULL,
        current_class,
        tokens_head->strval,
        NULL, 
        sn_current_file(),
        tokens_head->start_line,
        VAR_CLASS_DEREF);

    assert(result == 0);

    free_head_token(); /* SOMEWORD */
}

<TOKEN>"PARENT_VARIABLE CLASS_MEMBER_OPERATOR SOMEWORD OPEN_PAREN" {
    char* fname;
    int line;
    int ref_from_scope_type;

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "found $parent class member function call tokens at %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif
    free_head_token(); /* $parent */
    free_head_token(); /* -> */

    fname = tokens_head->strval;
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "function name is \"%s\"\n", fname);
    #endif
    line = tokens_head->start_line;

    ref_from_scope_type = from_what_scope();

    result = sn_insert_xref(SN_REF_TO_FUNCTION,
        ref_from_scope_type,
        SN_REF_SCOPE_GLOBAL,
        current_class,
        current_refblock,
        NULL,
        current_parent,
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

<TOKEN>"PARENT_VARIABLE CLASS_MEMBER_OPERATOR SOMEWORD" {
    int ref_from_scope_type;

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "parent class variable read at token %d\n", token_index);
    #endif
    free_head_token(); /* $parent */
    free_head_token(); /* -> */

    ref_from_scope_type = from_what_scope();

    result = sn_insert_xref(SN_REF_TO_MBR_VAR,
        ref_from_scope_type,
        SN_REF_SCOPE_GLOBAL,
        current_class,
        current_refblock,
        NULL,
        current_parent,
        tokens_head->strval,
        NULL, 
        sn_current_file(),
        tokens_head->start_line,
        VAR_CLASS_DEREF);

    assert(result == 0);

    /* Remove to simplify editor, although should be instance var highlighting
    * sn_highlight(SN_HIGH_MBR_VAR,
    *     tokens_head->start_line, tokens_head->start_column,
    *     tokens_head->end_line, tokens_head->end_column);
    */

    free_head_token(); /* SOMEWORD */
}

<TOKEN>"VARIABLE CLASS_MEMBER_OPERATOR SOMEWORD OPEN_PAREN" {
    char* fname;
    int line;
    int ref_from_scope_type;

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "found $var class member function call tokens at %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif

    free_head_token(); /* $variable */
    free_head_token(); /* -> */

    fname = tokens_head->strval;
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "function name is \"%s\"\n", fname);
    #endif
    line = tokens_head->start_line;

    ref_from_scope_type = from_what_scope();

    result = sn_insert_xref(SN_REF_TO_FUNCTION,
        ref_from_scope_type,
        SN_REF_SCOPE_GLOBAL,
        current_class,
        current_refblock,
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

<TOKEN>"VARIABLE CLASS_MEMBER_OPERATOR SOMEWORD" {
    int ref_from_scope_type;

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "some class variable read at token %d\n", token_index);
    #endif

    free_head_token(); /* $variable */
    free_head_token(); /* -> */

    ref_from_scope_type = from_what_scope();

    result = sn_insert_xref(SN_REF_TO_MBR_VAR,
        ref_from_scope_type,
        SN_REF_SCOPE_GLOBAL,
        current_class,
        current_refblock,
        NULL,
        NULL, /* CHEAT: should use type of variable but typeless language interferes */
        tokens_head->strval,
        NULL, 
        sn_current_file(),
        tokens_head->start_line,
        VAR_CLASS_DEREF);

    assert(result == 0);

    /* Remove to simplify editor, although should be instance var highlighting
    *    sn_highlight(SN_HIGH_MBR_VAR,
    *        tokens_head->start_line, tokens_head->start_column,
    *        tokens_head->end_line, tokens_head->end_column);
    */

    free_head_token(); /* SOMEWORD */
}


<TOKEN>"CLASS_MEMBER_OPERATOR SOMEWORD OPEN_PAREN" {
    int ref_from_scope_type;

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "found deref class member function call tokens at %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif

    free_head_token(); /* -> */

    ref_from_scope_type = from_what_scope();

    result = sn_insert_xref(SN_REF_TO_MBR_FUNC,
        ref_from_scope_type,
        SN_REF_SCOPE_GLOBAL,
        current_class,
        current_refblock,
        NULL,
        NULL,    /* CHEAT: don't limit to class of var because of typeless language */
        tokens_head->strval,
        NULL, 
        sn_current_file(),
        tokens_head->start_line,
        SN_REF_PASS);

    assert(result == 0);

    sn_highlight(SN_HIGH_FUNCTION,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);

    free_head_token(); /* SOMEWORD */
    free_head_token(); /* OPEN_PAREN */
}

<TOKEN>"CLASS_MEMBER_OPERATOR SOMEWORD" {
    int ref_from_scope_type;

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "class variable read at token %d\n", token_index);
    #endif
    free_head_token(); /* -> */

    ref_from_scope_type = from_what_scope();

    result = sn_insert_xref(SN_REF_TO_MBR_VAR,
        ref_from_scope_type,
        SN_REF_SCOPE_GLOBAL,
        current_class,
        current_refblock,
        NULL,
        NULL,
        tokens_head->strval,
        NULL, 
        sn_current_file(),
        tokens_head->start_line,
        SN_REF_PASS);

    assert(result == 0);

    /* Remove to simplify editor, although should be instance var highlighting
    *    sn_highlight(SN_HIGH_MBR_VAR,
    *        tokens_head->start_line, tokens_head->start_column,
    *        tokens_head->end_line, tokens_head->end_column);
    */

    free_head_token(); /* SOMEWORD = class member */
}

<TOKEN>"ABSTRACT_KEYWORD" {
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "found abstract tokens at %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif

    sn_highlight(SN_HIGH_KEYWORD,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);
    free_head_token(); /* keyword */
    set_vis(ABSTRACT_KEYWORD);
}


<TOKEN>"PUBLIC_KEYWORD" {
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "found visibility public tokens at %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif

    sn_highlight(SN_HIGH_KEYWORD,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);
    free_head_token(); /* keyword */
    set_vis(PUBLIC_KEYWORD);
}

<TOKEN>"PROTECTED_KEYWORD" {
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "found visibility protected tokens at %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif

    sn_highlight(SN_HIGH_KEYWORD,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);
    set_vis(PROTECTED_KEYWORD);
    free_head_token(); /* keyword */
}

<TOKEN>"PRIVATE_KEYWORD" {
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "found visibility private tokens at %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif

    sn_highlight(SN_HIGH_KEYWORD,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);
    set_vis(PRIVATE_KEYWORD);
    free_head_token(); /* keyword */
}

<TOKEN>"STATIC_KEYWORD" {
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "found class static tokens at %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif

    sn_highlight(SN_HIGH_KEYWORD,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);
    set_vis(STATIC_KEYWORD);
    free_head_token(); /* keyword */
}

<TOKEN>"VARIABLE" {
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "variable read at token %d, vis 0x%x\n", token_index, recent_vis);
    #endif

    if (current_class && !current_function) {
        result = sn_insert_symbol(SN_MBR_VAR_DEF, current_class,
            tokens_head->strval, sn_current_file(),
            tokens_head->start_line, tokens_head->start_column,
            tokens_head->start_line, tokens_head->end_column,
            recent_vis, NULL, NULL, NULL, NULL,
            tokens_head->start_line, tokens_head->start_column,
            tokens_head->start_line, tokens_head->end_column);
        assert(result == 0);
    }

    emit_var_access(tokens_head, VAR_READ);

    free_head_token(); /* VARIABLE */
}


<TOKEN>"SOMEWORD CLASS_SCOPE_OPERATOR" {
    /*
    * Note: keep parsing "fuzzy" by stopping at the pattern 'classname::'
    *  and then let other rules take over.
    */
    int ref_from_scope_type;
    char *clsname;

    if (strcmp(tokens_head->strval, "parent") == 0) {
        clsname = current_parent;
    } else if (strcmp(tokens_head->strval, "self") == 0) {
        clsname = current_class;
    } else {
        clsname = tokens_head->strval;
        /* FIXME: is there a highlight for a reference to a class name? */
        sn_highlight(SN_HIGH_CLASSDEF,
            tokens_head->start_line, tokens_head->start_column,
            tokens_head->end_line, tokens_head->end_column);
    }

    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "direct class access at %d\n", token_index);
    fprintf(tokenout, "match text was \"%s\"\n", yytext);
    #endif

    ref_from_scope_type = from_what_scope();

    /* NOTE: we use '#' as the refclass so the GUI will point to the class def */
    result = sn_insert_xref(SN_REF_TO_CLASS,
        ref_from_scope_type,
        SN_REF_SCOPE_GLOBAL,
        current_class,
        current_refblock,
        NULL,
        "#",
        clsname,
        NULL, 
        sn_current_file(),
        tokens_head->start_line,
        SN_REF_READ);

    assert(result == 0);

    free_head_token(); /* REFCLASS */
    free_head_token(); /* :: */
}


<TOKEN>"INCREMENT_OPERATORS VARIABLE OPEN_BRACKET" {
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "increment array variable start token %d\n", token_index);
    #endif

    /* This is the start of a prefix incremented array variable
    *
    *   ++$arr[0]
    */

    if (current_array_bracket_count > 0) {
        /* Nested brackets : $arr1[++$arr2[0]] */
        push_bracket_counter();
    }
    current_array_bracket_count = 1;
    current_array_prefix_incr = 1;

    free_head_token(); /* INCREMENT_OPERATORS */
    current_array_token = pop_head_token(); /* VARIABLE */
    free_head_token(); /* OPEN_BRACKET */
}

<TOKEN>"VARIABLE OPEN_BRACKET" {
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "array variable start token %d\n", token_index);
    #endif

    /* This is the start of an array variable.
    *
    *   $arr[0]
    */

    if (current_array_bracket_count > 0) {
        /* Nested brackets : $arr1[$arr2[0]] */
        push_bracket_counter();
    }
    current_array_bracket_count = 1;
    current_array_token = pop_head_token(); /* VARIABLE */
    free_head_token(); /* OPEN_BRACKET */
}

<TOKEN>"CLOSE_BRACKET OPEN_BRACKET" {
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "array variable array dimension tokens %d\n", token_index);
    #endif

    /* This rule matches the "][" tokens in the following statement:
    *
    * array["one"]["two"] = 1;
    *
    * Note that we don't change the current_array_bracket_count here.
    */

    free_head_token(); /* CLOSE_BRACKET */
    free_head_token(); /* OPEN_BRACKET */
}

<TOKEN>"CLOSE_BRACKET ASSIGNMENT_OPERATOR" {
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "array variable assignment end token %d\n", token_index);
    #endif

    /* This could be the end of an array variable.
    *
    *   $arr["num"] = 2;
    */

    if (current_array_bracket_count > 0) {
        if (--current_array_bracket_count == 0) {
            emit_var_access(current_array_token, VAR_WRITE);
            FreeToken(current_array_token);
            current_array_prefix_incr = 0;

            pop_bracket_counter();

            #ifdef TOKEN_DEBUG
            fprintf(tokenout, "assignment operator end of array variable\n");
            #endif
        }
    }

    free_head_token(); /* CLOSE_BRACKET */
    free_head_token(); /* ASSIGNMENT_OPERATOR */
}

<TOKEN>"CLOSE_BRACKET ASSIGNMENT_OPERATORS" {
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "array variable read/write assignment end token %d\n", token_index);
    #endif

    /* This could be the end of an array variable.
    *
    *   $arr["num"] += 2;
    */

    if (current_array_bracket_count > 0) {
        if (--current_array_bracket_count == 0) {
            emit_var_access(current_array_token, VAR_READWRITE);
            FreeToken(current_array_token);
            current_array_prefix_incr = 0;
            pop_bracket_counter();

            #ifdef TOKEN_DEBUG
            fprintf(tokenout, "assignment operators end of array variable\n");
            #endif
        }
    }

    free_head_token(); /* CLOSE_BRACKET */
    free_head_token(); /* ASSIGNMENT_OPERATORS */
}

<TOKEN>"CLOSE_BRACKET INCREMENT_OPERATORS" {
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "array variable post increment end token %d\n", token_index);
    #endif

    /* This could be the end of an array variable increment
    *
    *   $arr[0]++
    */

    if (current_array_bracket_count > 0) {
        if (--current_array_bracket_count == 0) {
            emit_var_access(current_array_token, VAR_READWRITE);
            current_array_prefix_incr = 0;
            FreeToken(current_array_token);

            pop_bracket_counter();

            #ifdef TOKEN_DEBUG
            fprintf(tokenout, "end of array variable post increment\n");
            #endif
        }
    }

    free_head_token(); /* CLOSE_BRACKET */
    free_head_token(); /* INCREMENT_OPERATORS */
}

<TOKEN>"CLOSE_BRACKET" {
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "array variable read end token %d\n", token_index);
    #endif

    /* This could be the end of an array variable
    *
    *   $arr[0] = 2;
    *
    * Or the end of an array variable that has
    * a prefix increment.
    *
    *   ++$arr[0]
    */

    if (current_array_bracket_count > 0) {
        if (--current_array_bracket_count == 0) {
            if (current_array_prefix_incr) {
                emit_var_access(current_array_token, VAR_READWRITE);
                #ifdef TOKEN_DEBUG
                fprintf(tokenout, "read/write end of array variable\n");
                #endif
            } else {
                emit_var_access(current_array_token, VAR_READ);
                #ifdef TOKEN_DEBUG
                fprintf(tokenout, "read end of array variable\n");
                #endif
            }
            current_array_prefix_incr = 0;
            FreeToken(current_array_token);

            pop_bracket_counter();
        }
    }

    free_head_token(); /* CLOSE_BRACKET */
}

<TOKEN>"VDOUBLE_QUOTED_STRING" {
    #ifdef TOKEN_DEBUG
    fprintf(tokenout, "ate VDOUBLE_QUOTED_STRING token %d", token_index);
    #endif

    sn_highlight(SN_HIGH_STRING,
        tokens_head->start_line, tokens_head->start_column,
        tokens_head->end_line, tokens_head->end_column);

    emit_var_access_for_dqstring(tokens_head);

    free_head_token(); /* VDOUBLE_QUOTED_STRING */
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
        switch (tokens_head->type)
        {
            case DOUBLE_QUOTED_STRING:
            case SINGLE_QUOTED_STRING:
                type = SN_HIGH_STRING;
                break;
            case DEFINE_KEYWORD:
            case CLASS_KEYWORD:
            case NEW_KEYWORD:
            case ARRAY_KEYWORD:
            case TRY_KEYWORD:
            case CATCH_KEYWORD:
            case PUBLIC_KEYWORD:
            case ABSTRACT_KEYWORD:
            case PROTECTED_KEYWORD:
            case PRIVATE_KEYWORD:
            case STATIC_KEYWORD:
            case CONST_KEYWORD:
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

<TOKEN>. {
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
    /* A class closing brace was not found before we hit EOF */
    if (current_class) {
        #ifdef TOKEN_DEBUG
        fprintf(tokenout, "found unfinished class at EOF in %s\n", sn_current_file());
        #endif
        ckfree(current_class);
        current_class = (char *)NULL;
        if (current_parent) {
            ckfree(current_parent);
            current_parent = (char *)NULL;
        }
    }

    assert(!tokens_head); /* all tokens were processed */

    yy_delete_buffer( YY_CURRENT_BUFFER );
    yy_switch_to_buffer( original_buffer );
    enter_html_mode();
    yyterminate();
}

<HTML_MODE,PHP,COMMENT_MODE,DQSTRING,SQSTRING,HDSTRING><<EOF>> {
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
    switch (YY_START)
    {
        case COMMENT_MODE:
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
        case HTML_MODE:
            emit_html();
            break;
    }

    /* If no tokens were generated, then quit now */
    if (tokens_head == NULL) {
        #ifdef TOKEN_DEBUG
        fprintf(tokenout, "no TOKENs generated\n");
        #endif
        enter_html_mode();
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
                tok->start_line, tok->start_column, tok->end_line, tok->end_column);
            fprintf(dump_tokens, "\n");
        }

        fclose(dump_tokens);

        enter_html_mode();
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
            tok->start_line, tok->start_column, tok->end_line, tok->end_column);
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

static char* modestring()
{
    char* mode = "UNKNOWN";

    switch (YY_START)
    {
        case INITIAL:
            mode = "INITIAL";
            break;
        case PHP:
            mode = "PHP";
            break;
        case COMMENT_MODE:
            mode = "COMMENT_MODE";
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
        case HTML_MODE:
            mode = "HTML_MODE";
            break;
    }
    return mode;
}

#if MATCH_DUMP
/* Helper method that will print matches as they are made.
* This method is typically used in the token generation phase.
*/
static void matched_pattern(char * pattern, char * text)
{
    char * mode = modestring();

    fprintf(stderr, "Matched \"%s\", with text \"%s\", in mode \"%s\" (%d.%d)\n",
    pattern, text, mode, sn_line(), sn_column());
}
#endif /* MATCH_DUMP */

static void FreeGlobalEntry(SearchEntry *entry) {}


void FreeToken(Token* tok)
{
    if (tok->strval != NULL) {
        ckfree(tok->strval);
    }
    if (tok->vars != NULL) {
        Token* tmp, *next;
        for (tmp=tok->vars; tmp; ) {
            next = tmp->next;
            FreeToken(tmp);
            tmp = next;
        }
    }
    ckfree((char *) tok);
}

void append_dqstring_var_token(char* var, long start_line, int start_column,
 long end_line, int end_column)
{
    Token* tok;
    tok = (Token*) ckalloc(sizeof(Token));
    tok->type = VARIABLE;
    tok->strval = SN_StrDup(var);

    tok->start_line = start_line;
    tok->start_column = start_column;

    tok->end_line = end_line;
    tok->end_column = end_column;
    tok->vars = NULL;
    tok->next = NULL;

    /* append to var token list */

    if (embedded_dq_string_vars_tail == NULL) {
        embedded_dq_string_vars_head = embedded_dq_string_vars_tail = tok;
    } else {
        embedded_dq_string_vars_tail->next = tok;
        embedded_dq_string_vars_tail = tok;
    }
}

void append_dqstring_token(char* strval, long start_line, int start_column,
 long end_line, int end_column)
{
    if (embedded_dq_string_vars_head != NULL) {
        append_token(VDOUBLE_QUOTED_STRING, strval, start_line,
            start_column, end_line, end_column);
        tokens_tail->vars = embedded_dq_string_vars_head;
        embedded_dq_string_vars_head = NULL;
        embedded_dq_string_vars_tail = NULL;
    } else {
        append_token(DOUBLE_QUOTED_STRING, strval, start_line, start_column,
            end_line, end_column);
    }
}

void append_token(TokenType type, char* strval, long start_line,
 int start_column, long end_line, int end_column)
{
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
    tok->vars = NULL;
    tok->next = NULL;

    /* append to token list */

    if (tokens_tail == NULL) {
        tokens_head = tokens_tail = tok;
    } else {
        tokens_tail->next = tok;
        tokens_tail = tok;
    }
}

Token* pop_head_token()
{
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

void free_head_token()
{
    FreeToken(pop_head_token());
}

/*
* This method is invoked when nested brackets within an
* array name are encountered. For example: $arr1[++$arr2[0]]
*/
void push_bracket_counter()
{
    NestedBracket* nbp = (NestedBracket*) ckalloc(sizeof(NestedBracket));
    nbp->var = current_array_token;
    nbp->count = current_array_bracket_count;
    nbp->prefix_incr = current_array_prefix_incr;

    if (array_nest_head == NULL) {
        array_nest_head = array_nest_tail = nbp;
    } else {
        nbp->next = array_nest_head;
        array_nest_head = nbp;
    }
}

void pop_bracket_counter()
{
    if (array_nest_head != NULL) {
        NestedBracket* nbp = array_nest_head;
        if (array_nest_head == array_nest_tail) {
            array_nest_head = array_nest_tail = (NestedBracket*) NULL;
        } else {
            array_nest_head = array_nest_head->next;
        }
        current_array_token = nbp->var;
        current_array_bracket_count = nbp->count;
        current_array_prefix_incr = nbp->prefix_incr;
        ckfree((char*) nbp);
    }
}

/*
* This method is invoked when nested braces occur.
*/
void push_brace(TokenType braceType)
{
    NestedBrace* nbp = (NestedBrace*) ckalloc(sizeof(NestedBrace));
    if ( !nbp )
        return;
    nbp->btype = braceType;

    if (brace_nest_head == NULL) {
        brace_nest_head = brace_nest_tail = nbp;
    } else {
        nbp->next = brace_nest_head;
        brace_nest_head = nbp;
    }
}

TokenType pop_brace()
{
    TokenType retToken;
    if (brace_nest_head != NULL) {
        NestedBrace* nbp = brace_nest_head;
        if (brace_nest_head == brace_nest_tail) {
            brace_nest_head = brace_nest_tail = (NestedBrace*) NULL;
        } else {
            brace_nest_head = brace_nest_head->next;
        }
        retToken = nbp->btype;
        ckfree((char*) nbp);
        return(retToken);
    }
    return(UNKNOWN_TOKEN);
}

/*
* This method is invoked when a nested function is
* encountered.
*/

void push_function()
{
    NestedFunction* n = (NestedFunction*) ckalloc(sizeof(NestedFunction));

    n->name = current_function;
    n->args = current_function_args;
    n->high_line = current_function_highlight_line;
    n->high_col_start = current_function_highlight_column_start;
    n->high_col_end = current_function_highlight_column_end;
    n->line_start = current_function_line_start;
    n->line_end = current_function_line_end;
    n->col_start = current_function_column_start;
    n->col_end = current_function_column_end;

    if (function_nest_head == NULL) {
        function_nest_head = function_nest_tail = n;
    } else {
        n->next = function_nest_head;
        function_nest_head = n;
    }
}

void pop_function()
{
    if (function_nest_head != NULL) {
        NestedFunction* n = function_nest_head;
        if (function_nest_head == function_nest_tail) {
            function_nest_head = function_nest_tail = (NestedFunction*) NULL;
        } else {
            function_nest_head = function_nest_head->next;
        }

        current_function = n->name;
        current_function_args = n->args;
        current_function_highlight_line = n->high_line;
        current_function_highlight_column_start = n->high_col_start;
        current_function_highlight_column_end = n->high_col_end;
        current_function_line_start = n->line_start;
        current_function_line_end = n->line_end;
        current_function_column_start = n->col_start;
        current_function_column_end = n->col_end;

        ckfree((char*) n);
    }
}

/* Determine the type of location where a cross reference is being initiated from */
int from_what_scope()
{
    /* FIXME: SN_GLOBAL_NAMESPACE not forming a useable xref. See toolbox/snptools.c
    * for the same reason that a reference from the global namespace is a problem.
    * xrefs from within classes and functions, for the most part, works.
    */
    if (current_function == NULL) {
        if (current_class == NULL) {
            /* snptools.c asserts current function = NULL with SN_GLOBAL_NAMESPACE */
            current_refblock = NULL;
            return(SN_GLOBAL_NAMESPACE);
        } else {
            current_refblock = "__construct";
            return(SN_SUBR_DEF);
        }
    } else {
        current_refblock = current_function;
        if (current_class == NULL)
            return(SN_FUNC_DEF);
        else
            return(SN_MBR_FUNC_DEF);
    }
}

/* Note: these strings get put into the token stream used during the
* token parsing portion of the program. Make sure the strings here
* are reflected in the rules above.
*/
char * TokenTypeToString(Token *tok)
{
    switch(tok->type)
    {
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
        case PERIOD:
            return "PERIOD";
        case COLON_CHAR:
            return "COLON_CHAR";
        case ATSIGN_CHAR:
            return "ATSIGN_CHAR";
        case VARIABLE:
            return "VARIABLE";
        case UNI_OPERATORS:
            return "UNI_OPERATORS";
        case ASSIGNMENT_OPERATOR:
            return "ASSIGNMENT_OPERATOR";
        case ASSIGNMENT_OPERATORS:
            return "ASSIGNMENT_OPERATORS";
        case COMPARISON_OPERATORS:
            return "COMPARISON_OPERATORS";
        case INCREMENT_OPERATORS:
            return "INCREMENT_OPERATORS";
        case REFERENCE_OPERATOR:
            return "REFERENCE_OPERATOR";
        case CLASS_SCOPE_OPERATOR:
            return "CLASS_SCOPE_OPERATOR";
        case CLASS_MEMBER_OPERATOR:
            return "CLASS_MEMBER_OPERATOR";
        case MAPPING_OPERATOR:
            return "MAPPING_OPERATOR";
        case THIS_VARIABLE:
            return "THIS_VARIABLE";
        case PARENT_VARIABLE:
            return "PARENT_VARIABLE";
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
        case CLASS_KEYWORD:
            return "CLASS_KEYWORD";
        case CLASS_EXTENDS_KEYWORD:
            return "CLASS_EXTENDS_KEYWORD";
        case CLASS_IMPLEMENTS_KEYWORD:
            return "CLASS_IMPLEMENTS_KEYWORD";
        case VDOUBLE_QUOTED_STRING:
            return "VDOUBLE_QUOTED_STRING";
        case DOUBLE_QUOTED_STRING:
            return "DOUBLE_QUOTED_STRING";
        case SINGLE_QUOTED_STRING:
            return "SINGLE_QUOTED_STRING";
        case NEW_KEYWORD:
            return "NEW_KEYWORD";
        case ARRAY_KEYWORD:
            return "ARRAY_KEYWORD";
        case DEFINE_KEYWORD:
            return "DEFINE_KEYWORD";
        case TRY_KEYWORD:
            return "TRY_KEYWORD";
        case CATCH_KEYWORD:
            return "CATCH_KEYWORD";
        case ABSTRACT_KEYWORD:
            return "ABSTRACT_KEYWORD";
        case PUBLIC_KEYWORD:
            return "PUBLIC_KEYWORD";
        case PROTECTED_KEYWORD:
            return "PROTECTED_KEYWORD";
        case PRIVATE_KEYWORD:
            return "PRIVATE_KEYWORD";
        case STATIC_KEYWORD:
            return "STATIC_KEYWORD";
        case CONST_KEYWORD:
            return "CONST_KEYWORD";
        case UNKNOWN_TOKEN:
            return "UNKNOWN_TOKEN";
        case COMMENT_TOKEN:
            return "COMMENT_TOKEN";
        case HTML_TOKEN:
            return "HTML_TOKEN";
        default:
            return "TOKEN_NOT_MATCHED";
    }
}

/* Called when the closing brace of a function is found
* or when we hit EOF without finding the end of the
* function.
*/
void emit_function_declaration()
{
    if (current_class) {
        #ifdef TOKEN_DEBUG
        fprintf(tokenout, "emit class function definition \"%s\"\n", current_function);
        #endif
        result = sn_insert_symbol(SN_MBR_FUNC_DEF,
            current_class,
            current_function,
            sn_current_file(), 
            current_function_line_start, current_function_column_start,
            current_function_line_end, current_function_column_end,
            recent_vis,
            NULL /* return type */,
            current_function_args /* argument types, FIXME: php not strongly typed, use names*/,
            current_function_args /* argument names */,
            NULL /* comment */,
            current_function_highlight_line,
            current_function_highlight_column_start,
            current_function_highlight_line,
            current_function_highlight_column_end );
        assert(result == 0);
        /* NOTE: both declare and define because PHP is almost an un-typed language */
        result = sn_insert_symbol(SN_MBR_FUNC_DCL,
            current_class,
            current_function,
            sn_current_file(), 
            current_function_line_start, current_function_column_start,
            current_function_line_end, current_function_column_end,
            recent_vis,
            NULL /* return type */,
            current_function_args /* argument types, FIXME: php not strongly typed, use names*/,
            current_function_args /* argument names */,
            NULL /* comment */,
            current_function_highlight_line,
            current_function_highlight_column_start,
            current_function_highlight_line,
            current_function_highlight_column_end );
        assert(result == 0);
    } else {
        #ifdef TOKEN_DEBUG
        fprintf(tokenout, "emit function definition \"%s\"\n", current_function);
        #endif
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
    }


    ckfree(current_function);
    current_function = NULL;
    ckfree(current_function_args);
    current_function_args = NULL;

    if (global_var_table) {
        global_var_table->destroy( &global_var_table );
        global_var_table = NULL;
    }
    reset_vis(); /* prep for visibility specs */
}

void emit_comment()
{
    char* comment = mode_buff.buf;

    #if COMMENT_DUMP
    fprintf(stderr, "emit comment \"%s\"\n", comment);
    #endif

    /* If dumping tokens, emit a special COMMENT token.
    * Otherwise, insert a comment symbol and a highlight.
    */
    if (token_dump_file) {
        append_token(COMMENT_TOKEN, comment,
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
            mode_start_line, mode_start_column - 2,
            sn_line(), sn_column() + 2);
    }

    mode_buff.free(&mode_buff);
}

void emit_dqstring()
{
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

void emit_sqstring()
{
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

void emit_hdstring()
{
    char* hdstring = mode_buff.buf;

    /* Nuke trailing newline in hdstring */
    char * end = hdstring + strlen(hdstring) - 1;
    if (*end == '\n') {
        *end = '\0';
    }

    #if HDSTRING_DUMP
    fprintf(stderr, "creating hdstring token \"%s\"\n", hdstring);
    #endif

    /* sn_column() should be at the ; at the end of a heredoc */

    append_dqstring_token(hdstring,
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


/* Helper method to enter HTML mode.
* Should be called in place of BEGIN(HTML_MODE) */

void enter_html_mode()
{
    if (token_dump_file) {
        LongStringInit(&mode_buff,0);
        mode_start_line = sn_line();
        mode_start_column = sn_column();
    }
    BEGIN(HTML_MODE);
}


void emit_html()
{
    if (token_dump_file) {
        char * html_text = mode_buff.buf;

        #if HTML_DUMP
        fprintf(stderr, "creating html token \"%s\"\n", html_text);
        #endif

        append_token(HTML_TOKEN, html_text,
            mode_start_line,
            mode_start_column,
            sn_line(),
            sn_column());

        mode_buff.free(&mode_buff);
    }
}

/* Write out read xrefs for variables embedded in this
* VDOUBLE_QUOTED_STRING token
*/
void emit_var_access_for_dqstring(Token* tok)
{
    assert(tok->vars);
    for (tok = tok->vars; tok; tok = tok->next) {
        emit_var_access(tok, VAR_READ);
    }
}

/* This method is invoked when a var operation is matched
* in the token stream.
*/
void emit_var_access(Token *tok, VarAccess acc)
{
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

    if (((current_function == NULL) && (current_class == NULL)) ||
        (super_global_var_table->search( &super_global_var_table, entry ) != NULL) ||
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

    ref_from_scope_type = from_what_scope();

    if ((ref_to_symbol_type == SN_REF_TO_GLOB_VAR) ||
        ((ref_to_symbol_type == SN_REF_TO_LOCAL_VAR) &&
        ((int) sn_getopt(SN_OPT_LOCAL_VARS) != 0))) {

        if ((acc == VAR_READ) || (acc == VAR_READWRITE)) {
            /*
            * fprintf(tokenout,
            *     "var readable insert xref \"%s\" from %d to %d class %s func %s rd %d\n",
            *     varname, ref_from_scope_type, ref_to_symbol_scope,
            *     (current_class ? current_class : "NULL"),
            *     (current_function ? current_function : "NULL"), acc);
            */
            result = sn_insert_xref(ref_to_symbol_type,
                ref_from_scope_type,
                ref_to_symbol_scope,
                current_class,
                current_refblock,
                NULL,
                NULL,
                varname,
                "UNDECLARED",
                sn_current_file(),
                line_start,
                SN_REF_READ);

            assert(result == 0);
        }
        /*
        *    fprintf(tokenout, "var not readable skip xref \"%s\" to %d acc %d\n",
        *        varname, ref_to_symbol_scope, acc);
        */

        if ((acc == VAR_WRITE) || (acc == VAR_READWRITE)) {
            /*
            * fprintf(tokenout,
            *     "var writeable insert xref \"%s\" from %d to %d class %s func %s rd %d\n",
            *     varname, ref_from_scope_type, ref_to_symbol_scope,
            *    (current_class ? current_class : "NULL"),
            *    (current_function ? current_function : "NULL"), acc);
            */
            result = sn_insert_xref(ref_to_symbol_type,
                ref_from_scope_type,
                ref_to_symbol_scope,
                current_class,
                current_refblock,
                NULL,
                NULL,
                varname,
                "UNDECLARED",
                sn_current_file(),
                line_start,
                SN_REF_WRITE);

            assert(result == 0);
        }
        /*
        * else
        * fprintf(tokenout, "var not writeable skip xref \"%s\" to %d acc %d\n",
        *    varname, ref_to_symbol_scope, acc);
        */
    } else if (acc == VAR_CLASS_DEREF) {
        /*
        * fprintf(tokenout, "class var insert xref \"%s\" to %d opt %d\n",
        *    varname, ref_to_symbol_scope, (int) sn_getopt(SN_OPT_LOCAL_VARS));
        */
        result = sn_insert_xref(ref_to_symbol_type,
            ref_from_scope_type,
            ref_to_symbol_scope,
            current_class,
            current_refblock,
            NULL,
            NULL,
            varname,
            "UNDECLARED",
            sn_current_file(),
            line_start,
            SN_REF_PASS);

        assert(result == 0);
    }
    /*
    * else
    *    fprintf(tokenout, "local var skip insert xref \"%s\" to %d opt %d\n",
    *        varname, ref_to_symbol_scope, (int) sn_getopt(SN_OPT_LOCAL_VARS));
    */

    if (ref_to_symbol_type == SN_REF_TO_GLOB_VAR) {
        sn_highlight(SN_HIGH_VAR_GLOBAL,
            line_start, column_start,
            line_end, column_end);
    } else {
        #ifdef ALLOW_LOCAL_VAR_HIGHLIGHT
        sn_highlight(SN_HIGH_VAR_LOCAL,
            line_start, column_start,
            line_end, column_end);
        #endif
    }
    reset_vis(); /* prep for visibility specs */
}

int get_num_tokens_matched(char *match)
{
    char *x;
    int tokens;

    assert(*match != '\0'); /* match must have some text */
    assert(*match != ' '); /* match can't start with a space */
    for (x=match, tokens=0; *x; x++) {
        if (*x == ' ') {
            /* We just got to the end of a token */
            assert(*(x-1) != ' '); /* two spaces in a row is not allowed */
            tokens++;
        }
    }
    if (*(x-1) != ' ') {
        tokens++;
    }
    return tokens;
}

void
reset()
{
    assert(!current_function);
    assert(!current_class);
    assert(!current_parent);
    sn_reset_line();
    sn_reset_column();
    sn_reset_encoding();
}

void
reset_vis()
{
    recent_vis = SN_PUBLIC;
    recent_vis_toknum = token_index - 1;
}

void
set_vis(TokenType useTok)
{
    if (useTok == ABSTRACT_KEYWORD) {
        recent_vis = SN_ABSTRACT;
    } else if (useTok == STATIC_KEYWORD) {
        recent_vis |= SN_STATIC;
    } else {
        if (useTok == PUBLIC_KEYWORD) {
            recent_vis &= ~(SN_PROTECTED | SN_PRIVATE);
            recent_vis |= SN_PUBLIC;
        } else if (useTok == PROTECTED_KEYWORD) {
            recent_vis &= ~(SN_PUBLIC | SN_PRIVATE);
            recent_vis |= SN_PROTECTED;
        } else if (useTok == PRIVATE_KEYWORD) {
            recent_vis &= ~(SN_PUBLIC | SN_PROTECTED);
            recent_vis |= SN_PRIVATE;
        }
    }
    recent_vis_toknum = token_index;
}

int
main(int argc, char *argv[])
{
    /* Init the super globals lookup table before starting
    * since it will never change during parsing.
    */

    int i;
    SearchEntry entry;
    char* super_globals[] = {
        "GLOBALS", "_SERVER", "_GET", "_POST", "_COOKIE",
        "_FILES", "_ENV", "_REQUEST", "_SESSION", NULL
    };
    super_global_var_table =
    SearchTableCreate(20, SEARCH_HASH_TABLE, FreeGlobalEntry);

    for (i=0; super_globals[i]; i++) {
        entry.key = super_globals[i];
        entry.key_len = -1;
        entry.flag = SEARCH_DUP_KEY; /* add copy of entry.key */
        super_global_var_table->add( &super_global_var_table, entry );
    }

    return sn_main(argc, argv, group, &yyin, yylex, reset);
}
