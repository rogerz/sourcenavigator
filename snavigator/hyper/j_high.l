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
#include <ctype.h>

#include "highlight.h"

static long lineNumber = 1;
static int    charNumber = 0;
static int lex_buf_size = 0;
static void *user_data1;
static void *user_data2;

static    void    drop_until();

#undef yywrap
#define    YY_SKIP_YYWRAP    1
static    int    yywrap();
%}

%%

"//".*$    {
    paf_high_pos.beg_lineno = lineNumber;
    paf_high_pos.beg_charno = charNumber;
    paf_high_pos.end_lineno = lineNumber + 1;
    paf_high_pos.end_charno = 0;

    charNumber += yyleng;

    return PAF_HIGH_COMMENT;
}

"/*"    {
    int c, prev_c;

    paf_high_pos.beg_lineno = lineNumber;
    paf_high_pos.beg_charno = charNumber;

    charNumber += yyleng;

    for (prev_c = EOF;;)
    {
	charNumber++;
	c = input();
	if (c == EOF)
	{
	    paf_high_pos.end_lineno = lineNumber + 1;
	    paf_high_pos.end_charno = 0;
	    /* printf("COMMENT: eof: %d %d.%d\n",
		c == EOF,lineNumber,charNumber); */
	    return PAF_HIGH_COMMENT;
	}
	else if (c == '\n')
	{
	    lineNumber++;
	    charNumber = 0;
	}
	else if (c == '/' && prev_c == '*')
	{
	    paf_high_pos.end_lineno = lineNumber;
	    paf_high_pos.end_charno = charNumber;
		
	    return PAF_HIGH_COMMENT;
	}
	prev_c = c;
    }
}

\' {
    paf_high_pos.beg_lineno = lineNumber;
    paf_high_pos.beg_charno = charNumber;

    charNumber += yyleng;

    drop_until((int)'\'',(int)'\\');

    paf_high_pos.end_lineno = lineNumber;
    paf_high_pos.end_charno = charNumber;

    return PAF_HIGH_STRING;
}

\" {
    paf_high_pos.beg_lineno = lineNumber;
    paf_high_pos.beg_charno = charNumber;

    charNumber += yyleng;

    drop_until((int)'"',(int)'\\');

    paf_high_pos.end_lineno = lineNumber;
    paf_high_pos.end_charno = charNumber;

    return PAF_HIGH_STRING;
}

abstract |
boolean |
break |
byte |
case |
cast |
catch |
char |
class |
continue |
default |
do |
double |
else |
extends |
false |
final |
finally |
float |
for |
future |
generic |
goto |
if |
implements |
import |
inner |
instanceof |
int |
interface |
long |
native |
new |
null |
operator |
outer |
package |
private |
protected |
public |
rest |
return |
short |
static |
super |
switch |
synchronized |
this |
throw |
throws |
transient |
true |
try |
var |
void |
volatile |
while {
    paf_high_pos.beg_lineno = lineNumber;
    paf_high_pos.beg_charno = charNumber;
    paf_high_pos.end_lineno = lineNumber;
    paf_high_pos.end_charno = charNumber + yyleng;

    charNumber += yyleng;

    return PAF_HIGH_KEYWORD;
}

\n+                   {
    lineNumber += yyleng;
    charNumber = 0;
}

\r+	    {
	    }

[a-zA-Z0-9_]+ |
[ \t]+	|
.	{
    charNumber += yyleng;
}
%%

static    void
drop_until(int term, int esc)
{
    int c, l = 0;

    for (l = -1; ((c = (int)input()) != term || l == esc); l = c, charNumber++)
    {
	switch (c)
	{
	case '\\':
	    c = input();
	    if (c <= 0)
	    {
		return;
		break;
	    }
	    if (c != '\n')
	    {
	        charNumber++;
	        c = -1;
	        break;
	    }
	case '\n':
	    lineNumber++;
	    charNumber = -1;
	    break;

	case '\r':
	    charNumber--;
	    break;

	case EOF:
	    return;
	    break;
	}
    }
    charNumber++;
}

static    int
yywrap()
{
    return 1;
}

static void
flush_lex_scanner()
{
    YY_FLUSH_BUFFER;
}

void
java_highlight_init_func(int maxs, int lineno, int charno, void *(*func), void *u_d1, void *u_d2)
{
    lex_buf_size = maxs;
    lineNumber = lineno;
    charNumber = charno;
    *func = flush_lex_scanner;
    user_data1 = u_d1;
    user_data2 = u_d2;
}

