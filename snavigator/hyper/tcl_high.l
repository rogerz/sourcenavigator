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

static long braceNest       = 0;
static long lineNumber      = 1;
static int lex_buf_size = 0;
static void *user_data1;
static void *user_data2;

static int    charNumber = 0;
static long comment_caller  = 0;

static    void    drop_until();

#undef yywrap
#define    YY_SKIP_YYWRAP    1
static    int    yywrap();
%}

%x COMMENT

%%

^[ \t]*"#"          |
";"[ \t]*"#"        {
    char    *p;

    for (p = &yytext[yyleng - 1]; isspace(*p); p++);
    paf_high_pos.beg_lineno = lineNumber;
    paf_high_pos.beg_charno = charNumber + p - yytext;

    charNumber += yyleng;
    BEGIN(COMMENT);
}

<COMMENT>.*\\[ \t]*\n   {
    lineNumber++;
    charNumber = 0;
}

<COMMENT>\n	{	    /* end of comment */
    lineNumber++;
    charNumber = 0;
    paf_high_pos.end_lineno = lineNumber;
    paf_high_pos.end_charno = charNumber;

    BEGIN(comment_caller);

    return PAF_HIGH_COMMENT;
}

<COMMENT>.*	{	    /* eat up everything */
    charNumber += yyleng;
}

\"                  {
    paf_high_pos.beg_lineno = lineNumber;
    paf_high_pos.beg_charno = charNumber;

    charNumber += yyleng;

    drop_until((int)'"',(int)'\\');

    paf_high_pos.end_lineno = lineNumber;
    paf_high_pos.end_charno = charNumber;

    return PAF_HIGH_STRING;
}

::[ ]* {
    charNumber += yyleng;
}

::(proc|itcl_class|class|virtual|previous|body|configbody|inherit|public|private|protected|common|method|variable|array|break|catch|continue|if|then|else|elseif|exit|for|foreach|while|switch|default|eval|info|return|global|namespace|event|rename|package|source|\$this|\$this[ \t]+delete([ \t]|$)|uplevel|upvar|after|bind|bindtags|button|checkbutton|close|destroy|entry|error|exec|fconfigure|fileevent|focus|frame|grab|grid|label|load|menu|menubutton|message|open|option|pack|place|radiobutton|raise|scale|send|scrollbar|selection|table|text|tkwait|toplevel|trace|update|vwait|wm) {
    paf_high_pos.beg_lineno = lineNumber;
    paf_high_pos.beg_charno = charNumber + 2;
    paf_high_pos.end_lineno = lineNumber;
    paf_high_pos.end_charno = charNumber + yyleng;

    charNumber += yyleng;

    return PAF_HIGH_KEYWORD;
}

proc	|
itcl_class    |
class	|
virtual	|
previous    |
body	|
configbody    |
inherit	|
public	|
private	|
protected    |
common	|
method	|
variable    |
array       |
break	|
catch |
continue |
if |
then |
else        |
elseif      |
exit	|
for         |
foreach     |
while       |
switch      |
default     |
eval        |
info	|
return      |
global      |
namespace    |
event	|
rename	|
package	|
source      |
\$this      |
\$this[ \t]+delete([ \t]|$)  |
uplevel	|
upvar	|
after 	|
bind        |
bindtags    |
button	|
checkbutton    |
close	|
destroy	|
entry	|
error	|
exec	|
fconfigure    |
fileevent    |
focus       |
frame       |
grab        |
grid	|
label       |
load	|
menu        |
menubutton    |
message     |
open	|
option      |
pack        |
place	|
radiobutton    |
raise	|
scale       |
send	|
scrollbar   |
selection   |
table	|
text	|
tkwait	|
toplevel    |
trace	|
update	|
vwait	|
wm	    {
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

\\.	    {    /* eat up \. characters like \\ \" \{*/
    charNumber += yyleng;
}

\[	    {
    charNumber += yyleng;
    /* drop_braces(); */
}

\{                  {
    braceNest++;
    charNumber += yyleng;
}

\}                  {
    charNumber += yyleng;
    if (--braceNest <= 0)
	braceNest = 0;
}

[;]+		    {
    charNumber += yyleng;
}

[a-zA-Z_&0-9@\.\-\+:$()]+ {
    charNumber += yyleng;
}

\r+	    {
	    }

[ \t]+	|
.	{
    charNumber += yyleng;
}
%%

static    void
drop_until(int term, int esc)
{
    int    c;
    int    l = 0;

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
    if (c == term)
	charNumber++;
}

static    int
yywrap()
{
    braceNest = 0;
    comment_caller = 0;
    return 1;
}

static void
flush_lex_scanner()
{
    YY_FLUSH_BUFFER;
}

void
tcl_highlight_init_func(int maxs, int lineno, int charno, void *(*func), void *u_d1, void *u_d2)
{
    lex_buf_size = maxs;
    lineNumber = lineno;
    charNumber = charno;
    *func = flush_lex_scanner;
    user_data1 = u_d1;
    user_data2 = u_d2;
}

