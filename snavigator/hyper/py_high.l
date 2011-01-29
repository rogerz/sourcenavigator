/* -*- c -*- */
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

/*
 * py_high.l
 *
 * Written by Thomas Heller <thomas.heller@ion-tof.com>
 *
 * Description:
 * A lex specification for a scanner to parse python files
 * and highlight the source code.
 */

%{
#include <ctype.h>

#include "highlight.h"

static long lineNumber = 1;
static int charNumber = 0;
static int lex_buf_size = YY_BUF_SIZE;
static void *user_data1;
static void *user_data2;

static void drop_until();
static void parse_triple();

#undef yywrap
#define	YY_SKIP_YYWRAP	1
static	int	yywrap();
%}

%%

"#".*$	{
    paf_high_pos.beg_lineno = lineNumber;
    paf_high_pos.beg_charno = charNumber;
    paf_high_pos.end_lineno = lineNumber + 1;
    paf_high_pos.end_charno = 0;

    charNumber += yyleng;

    return PAF_HIGH_COMMENT;
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

\"\"\" {
    paf_high_pos.beg_lineno = lineNumber;
    paf_high_pos.beg_charno = charNumber;
    charNumber += yyleng;

    parse_triple((int)'"');

    paf_high_pos.end_lineno = lineNumber;
    paf_high_pos.end_charno = charNumber;

    return PAF_HIGH_STRING;
}

\'\'\' {
    paf_high_pos.beg_lineno = lineNumber;
    paf_high_pos.beg_charno = charNumber;
    charNumber += yyleng;

    parse_triple((int)'\'');

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

and |
assert |
break |
class |
continue |
def |
del |
elif |
else |
except |
exec |
finally |
for |
from |
global |
if |
import |
in |
is |
lambda |
not |
or |
pass |
print |
raise |
return |
try |
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

\r+			{
			}

[a-zA-Z0-9_]+ |
[ \t]+		|
.		{
	charNumber += yyleng;
}
%%

static void
parse_triple(int term)
{
    int c;
    int count;
    while (1) {
	c = input();
	charNumber++;
	if (c == term) {
	    ++count;
	    if (count == 3)
		return;
	} else {
	    count = 0;
	    switch (c) {
	    case '\\':
		c = input();
		if (c <= 0)
		    return;
		if (c != '\n') {
		    charNumber++;
		    c = -1;
		} else {
		    ++lineNumber;
		}
		break;

	    case '\n':
		lineNumber++;
		charNumber = -1;
		break;

	    case '\r':
		charNumber--;
		break;

	    case EOF:
		return;
	    }
	}
    }
}

static	void
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
			return;
		    if (c != '\n') {
			charNumber++;
			c = -1;
		    } else {
			++lineNumber;
		    }
		    break;

		case '\n':
		    lineNumber++;
		    charNumber = -1;
		    return;

		case '\r':
		    charNumber--;
		    break;

		case EOF:
		    return;
		}
	}
    charNumber++;
}

static	int
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
py_highlight_init_func(int maxs, int lineno, int charno, void *(*func), void *u_d1, void *u_d2)
{
    lex_buf_size = maxs;
    lineNumber = lineno;
    charNumber = charno;
    *func = flush_lex_scanner;
    user_data1 = u_d1;
    user_data2 = u_d2;
}

