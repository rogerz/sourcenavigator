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

#include "tcl.h"

/* See sn.h for definitions of PAF_FILE -> PAF_SYMBOL_TYPE_MAX */
static char *SN_symbol_types[] = {
	"f", "t", "cl", "mi", "iv", "e", "con", "ma", "fu", "su",
	"gv", "com", "cov", "in", "fil", "by", "to","md","fd","ec",
	"un","fr","na","ex","lv","vd","iu","rem","cpp","ud",
	"xfi"
};


/*
 * We don't use strdup because it needs to use Tcl's allocator.
 */
char * SN_StrDup (char *str)
{
	char *buf = (char*)ckalloc (strlen(str)+1);
	strcpy (buf, str);
	return buf;
}


/*
 * Get the type string for a given symbol value. The symbol values
 * are defined in sn.h, for example PAF_FILE maps to "f".
 */

char *
SN_GetSymbolType(int type) {
	if ((type < 0) || (type >= sizeof(SN_symbol_types))) {	
	    panic("type %d passed to SN_GetSymbolType is out of range", type);
	}
	return SN_symbol_types[type];
}

