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
   * verilog_high.l
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
#define	YY_SKIP_YYWRAP	1
static	int	yywrap();
%}

/*  string-literal  (\"(\"\"|[^\"])*\")|('(''|[^'])*') */
string-literal  (\"(\"\"|[^\"])*\")

identifier      [A-Za-z]([A-Za-z]|[0-9]|_)*

%x COMMENT
%x LCOMMENT

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

"//" {
  paf_high_pos.beg_lineno = linenumber;
  paf_high_pos.beg_charno = charnumber;
  charnumber += 2;

  BEGIN(LCOMMENT);
}

<LCOMMENT>. { charnumber++; }

<LCOMMENT>\n {
  paf_high_pos.end_lineno = linenumber;
  paf_high_pos.end_charno = charnumber;
  linenumber++; charnumber = 0;

  BEGIN(INITIAL);

  return PAF_HIGH_COMMENT;
}

--.*\n {
  paf_high_pos.beg_lineno = paf_high_pos.end_lineno = linenumber;
  paf_high_pos.beg_charno = charnumber;
		    
  charnumber += yyleng;
  paf_high_pos.end_charno = charnumber - 1;
  
  linenumber++;
  charnumber = 0;

  return PAF_HIGH_COMMENT;
}

   /* ^[ \t]*#[ \t]*[a-z]+ |
     "<>" |  */
end                   |
or                    |
endcase               |
endtable              | 
rtran		      |  
endfunction	      |  
endprimitive	      |  
nor		      |
not                   |
endspecify	      |
output		      |
and		      |
nand		      |
event		      |
assign		      |
deassign	      |
tran		      |
table		      |
endtask		      |
edge		      |
begin		      |
negedge		      |
disable		      |
trior		      |
triand		      |
integer		      |
strong1		      |
rtranif1	      |
join                  |
inout		      |
task		      |
notif1		      |
strong0		      |
`ifdef                |
`else                 |
`endif                |
`include          |
`timescale        |
else		      |
release		      |
reg		      |
notif0		      |
vectored	      |
case		      |
casez		      |
signed		      |
tranif1		      |
scalered              |
trireg		      |
tri		      |
repeat		      |
tranif0		      |
tri1		      |
large		      |
posedge		      |
rnmos		      |
specify		      |
function	      |
tri0		      |
wor		      |
default		      |
for                   |
nmos		      |
module		      |
forever		      |
parameter	      |
casex		      |
wand		      |
primitive	      |
input		      |
supply1		      |
wire		      |
fork		      |
supply0		      |
\$time		      |
\$timeformat		      |
\$dumpfile            |
\$dumpvars            |
\$finish              |
\$strobe              |
bufif1                |
rcmos		      |
highz1		      |
initial		      |
pulldown	      |
bufif0		      |
cmos		      |
highz0		      |
while		      |
wait		      |
weak1		      |
weak0		      |
rpmos		      |
macromodule	      |
endmodule	      |
specparam             |
always		      |
pmos		      |
small		      |
buf		      |
pull1		      |
medium		      |
defparam	      |
pull0		      |
if		      |
localparam	      |
pullup		      |
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

static	int
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
verilog_highlight_init_func(int maxs, int lineno, int charno, void *(*func), void *u_d1, void *u_d2)
{

	lex_buf_size = maxs;
	linenumber = lineno;
	charnumber = charno;
	*func = flush_lex_scanner;
	user_data1 = u_d1;
	user_data2 = u_d2;
}

