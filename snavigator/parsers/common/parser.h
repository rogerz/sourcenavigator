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

#ifndef _PARSER_H_
#define  _PARSER_H_

void Paf_Pipe_Create(char *incl_to_pipe);

int Paf_Pipe_Close();

void put_status_parsing_file(char *fname);

int put_file(char *file_name,char *group, char *highlight_file);

int put_symbol(int type,char *scope,char *sym_name,char *file,
   int start_lineno,int start_colpos,int end_lineno,int end_colpos,
   unsigned long attr,char *ret,char *arg_types,char *args, char *comment,
   int high_start_lineno,int high_start_colpos,int high_end_lineno,
   int high_end_colpos);

int put_comment(char *classn,char *func,char *filename,
   char *comment,int beg_line,int beg_char);

int put_cross_ref(int type,int scope_type,int scope_lev,
   char *fnc_cls,char *fnc,char *fnc_arg_types,char *scope,char *what,
   char *arg_types,char *file,int lineno,int acc);

char *Paf_tempnam(char *dir,char *pref);

#endif /* _PARSER_H_ */

