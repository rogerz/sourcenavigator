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
 * highlight.h
 *
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * Public information for lexers wanting to highlight text from source files.
 */

#ifndef HIGHLIGHT_H
#define HIGHLIGHT_H

#include "tcl.h"

/* Use a user-defined input function for lex. This extracts text from the
   Tk text widget and translates it to UTF-8 encoding. */
#undef YY_INPUT
#define YY_INPUT(buf,r,ms) \
    (r = lexinput_tktext(buf, ms, lex_buf_size, user_data1, user_data2))

enum
{
    PAF_HIGH_COMMENT = 1,
    PAF_HIGH_KEYWORD,
    PAF_HIGH_STRING
};

struct Paf_High_Position
{
    int beg_lineno;
    int beg_charno;
    int end_lineno;
    int end_charno;
};

extern struct Paf_High_Position paf_high_pos;

extern int lexinput_tktext(char *buf, int maxsize, int lex_buf_size,
	       void * user_data1, void * user_data2);

int Sn_Syntax_Highlight(ClientData clientData, Tcl_Interp *interp,
                         int argc, char **argv);

#endif /* HIGHLIGHT_H */

