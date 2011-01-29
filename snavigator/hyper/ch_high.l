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

%{

  /*
   * ch_high.l
   *
   * Copyright (C) 1998 Cygnus Solutions
   *
   * Description:
   * A GNU flex specification for detecting comments, string literals and
   * keywords in the CCITT High Level Language (CHILL).  Appropriate values
   * are then returned to the `hyper' interpreter for syntax higlighting.
   *
   * Some extensions made by GNU CHILL are observed.  No doubt more will
   * be requested by customers in the future.
   */

#include <ctype.h>
#include "highlight.h"

static long linenumber = 1;
static int  charnumber = 0;
static int lex_buf_size = YY_BUF_SIZE;
static void *user_data1;
static void *user_data2;

extern int tk_text_buffer(char *buf, int maxs, int lexs, void *u_d1, void *u_d2);

#undef yywrap
#define    YY_SKIP_YYWRAP    1
static    int    yywrap();
%}

string-literal  (\"(\"\"|[^\"])*\")|('(''|[^'])*')

identifier      [A-Za-z]([A-Za-z]|[0-9]|_)*

%x COMMENT

%%

{string-literal} {
  paf_high_pos.beg_lineno = paf_high_pos.end_lineno = linenumber;
  paf_high_pos.beg_charno = charnumber;
  charnumber += strlen(yytext);
  paf_high_pos.end_charno = charnumber;

  return PAF_HIGH_STRING;
}

"/*" {
  paf_high_pos.beg_lineno = linenumber;
  paf_high_pos.beg_charno = charnumber;
  charnumber += 2;

  BEGIN(COMMENT);
}

<COMMENT>"*/" {
  paf_high_pos.end_lineno = linenumber;
  charnumber += 2;
  paf_high_pos.end_charno = charnumber;

  BEGIN(INITIAL);

  return PAF_HIGH_COMMENT;
}

<COMMENT>. { charnumber++; }

<COMMENT>\n { linenumber++; charnumber = 0; }

--.*\n {
  paf_high_pos.beg_lineno = paf_high_pos.end_lineno = linenumber;
  paf_high_pos.beg_charno = charnumber;
	    
  charnumber += yyleng;
  paf_high_pos.end_charno = charnumber - 1;
  
  linenumber++;
  charnumber = 0;

  return PAF_HIGH_COMMENT;
}

^[ \t]*#[ \t]*[a-z]+ |
"<>" |
access|ACCESS           |
after|AFTER             |
all|ALL                 |
and|AND                 |
andif|ANDIF             |
array|ARRAY             |
assert|ASSERT           |
association|ASSOCIATION |
at|AT                   |
based|BASED             |
begin|BEGIN             |
bin|BIN                 |
buffer|BUFFER           |
body|BODY               |
bool|BOOL               |
bools|BOOLS             |
by|BY                   |
case|CASE               |
cause|CAUSE             |
char|CHAR               |
chars|CHARS             |
context|CONTEXT         |
continue|CONTINUE       |
cycle|CYCLE             |
dcl|DCL                 |
delay|DELAY             |
do|DO                   |
down|DOWN               |
duration|DURATION       |
dynamic|DYNAMIC         |
else|ELSE               |
elsif|ELSEIF            |
end|END                 |
esac|ESAC               |
event|EVENT             |
ever|EVER               |
exceptions|EXCEPTIONS   |
exit|EXIT               |
fi|FI                   |
float|FLOAT             |
for|FOR                 |
forbid|FORBID           |
general|GENERAL         |
goto|GOTO               |
grant|GRANT             |
if|IF                   |
in|IN                   |
init|INIT               |
inline|INLINE           |
instance|INSTANCE       |
int|INT                 |
inout|INOUT             |
loc|LOC                 |
long|LONG               |
mod|MOD                 |
module|MODULE           |
newmode|NEWMODE         |
nonref|NONREF           |
nopack|NOPACK           |
not|NOT                 |
od|OD                   |
of|OF                   |
on|ON                   |
or|OR                   |
orif|ORIF               |
out|OUT                 |
pack|PACK               |
powerset|POWERSET       |
prefixed|PREFIXED       |
priority|PRIORITY       |
proc|PROC               |
process|PROCESS         |
ptr|PTR                 |
range|RANGE             |
read|READ               |
receive|RECEIVE         |
recursive|RECURSIVE     |
ref|REF                 |
region|REGION           |
rem|REM                 |
remote|REMOTE           |
result|RESULT           |
return|RETURN           |
returns|RETURNS         |
row|ROW                 |
seize|SEIZE             |
send|SEND               |
set|SET                 |
signal|SIGNAL           |
simple|SIMPLE           |
spec|SPEC               |
start|START             |
static|STATIC           |
step|STEP               |
stop|STOP               |
struct|STRUCT           |
syn|SYN                 |
synmode|SYNMODE         |
text|TEXT               |
then|THEN               |
this|THIS               |
time|TIME               |
timeout|TIMEOUT         |
to|TO                   |
ulong|ULONG             |
up|UP                   |
varying|VARYING         |
while|WHILE             |
with|WITH               |
xor|XOR {

  paf_high_pos.beg_lineno = paf_high_pos.end_lineno = linenumber;
  paf_high_pos.beg_charno = charnumber;
  charnumber += yyleng;
  paf_high_pos.end_charno = charnumber;

  return PAF_HIGH_KEYWORD;
}

{identifier} {
  charnumber += yyleng;
}

\r { }

\n+ {
  linenumber += yyleng;
  charnumber = 0;
}

. {
  /* Eat up everything else. */
  charnumber++;
}

%%

static    int
yywrap()
{
  linenumber = 1;
  charnumber = 0;
  return 1;
}

static void
flush_lex_scanner()
{
    YY_FLUSH_BUFFER;
}

void
ch_highlight_init_func(int maxs, int lineno, int charno, void *(*func), void *u_d1, void *u_d2)
{
    lex_buf_size = maxs;
    linenumber = lineno;
    charnumber = charno;
    *func = flush_lex_scanner;
    user_data1 = u_d1;
    user_data2 = u_d2;
}

