/*

Copyright (c) 2001, Enchanted Systems Limited

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
 * ibrowser.l
 *
 * Copyright (C) 2001-2007, Enchanted Systems Limited
 *
 * Description:
 * Lex input file for the Source-Navigator Intel8085 assembly parser.
 *
 * The target system is a cross-compiler running on VaxVms.  The compiler is
 * case-insensitive (hence setLower() calls and the tortured regExps of key
 * words etc).
 */

%{

#include <ctype.h>
#include <stdio.h>
#include "snptools.h"
#include "lexinput.h"

#undef yywrap
#define YY_SKIP_YYWRAP

#undef YY_INPUT
#define YY_INPUT(buf,r,ms) (r = sn_encoded_input(buf, ms))

static char *p;				/* general purpose pointer */
static char *savep;			/* general purpose pointer */
static char group[] = "8085";

static struct {
    char name[512];
    long line;
    long column;
    int length;
    unsigned char flag;
} last_label = { "", 0, 0, 0, 0 };

static struct {
    char name[512];
    long line;
    long column;
    int length;
    unsigned char flag;
} last_macro = { "", 0, 0, 0, 0 };

void
emit_func_defn()
{
    if ( last_label.flag > 0 )
    {
	sn_insert_symbol( SN_FUNC_DEF	/* symbol type			*/
		, NULL			/* classname			*/
		, last_label.name	/* identifier			*/
		, sn_current_file()	/* file name			*/
		, last_label.line	/* start line number		*/
		, last_label.column	/* start column number		*/
		, sn_line()		/* end line number		*/
		, 0			/* end column number		*/
		, 0			/* attributes			*/
		, NULL			/* return type			*/
		, NULL			/* argument types		*/
		, NULL			/* argument names		*/
		, NULL			/* comment			*/
		, last_label.line	/* highlighting start line	*/
		, last_label.column	/* highlighting start column	*/
		, last_label.line	/* highlighting end line	*/
		, last_label.column + last_label.length
					/* highlighting end column	*/
		);
	last_label.flag = 0;
    }
}

/*
 * Flush any function definitions that might be outstanding (ie. if its
 * label appears _last_ in a file.  When we reach EOF, check to see if its
 * defn needs to be flushed to the database.  Normally the occurence of
 * another label causes the defn to be stored.
 * 
 * Return 1 so flex will keep playing.
 *
 */
int
yywrap()
{
    emit_func_defn();
    return( 1 );
}

void
setLower				/* set character string to lower case		*/
    ( char * p				/* pointer to start of NULL-terminated string	*/
    )
{
    while ( *p )
    {
	if ( isupper( *p ) ) *p = tolower( *p );
	p++;
    }
}

%}

%x MACRO

alphas		[a-zA-Z]
digits		[0-9]
alphanums	({alphas}|{digits})
specials	[?@"."]

ws		[ \t]
string		\'.*\'
filename	({alphanums}|[$-])({alphanums}|[_$-]){0,38}"."({alphanums}|[_$-]){0,39}
symbolname	({alphas}|{specials})({alphanums}|".")*

reg8		([AaBbCcDdEeHhLl])
reg16		([BbDdHh])
sp		([Ss][Pp])
allreg8		({reg8}|[Mm])
allreg16	({reg16}|{sp})

%%

^{ws}*[Ii][Nn][Cc][Ll][Uu][Dd][Ee]{ws}+{filename} {

    /* walk over the indentation (if any) */
    for ( p = yytext; isspace( *p ); sn_advance_column( 1 ), p++ );

    /* walk over the include directive */
    for ( ; !isspace( *p ); sn_advance_column( 1 ), p++ );

    /* walk over the white space between directive and filename */
    for ( ; isspace( *p ); sn_advance_column( 1 ), p++ );

    setLower( p );
    sn_insert_symbol( SN_INCLUDE_DEF	/* symbol type			*/
  		, NULL			/* class name			*/
		, p			/* identifier			*/
		, sn_current_file()	/* file name			*/
		, sn_line()		/* start line number		*/
		, sn_column()		/* start column number		*/
		, sn_line()		/* end line number		*/
		, sn_column() + strlen(p)
				  	/* end column number		*/
		, 0			/* attributes			*/
		, NULL			/* return types			*/
		, NULL			/* argument types		*/
		, NULL			/* argument names		*/
		, NULL			/* comment			*/
		, sn_line()		/* highlighting start line	*/
		, sn_column()		/* highlighting start column	*/
		, sn_line()		/* highlighting end line	*/
		, sn_column() + strlen(p)
				  	/* highlighting end column	*/
		);

    sn_advance_column( yytext + yyleng - p );
}

(([Cc][Aa][Ll][Ll])|([Jj][Mm][Pp])|([CcJj][Nn]?[Cc])|([CcJj][Nn]?[Zz])|([CcJj][Mm])|([CcJj][Pp][EeOo]?)){ws}+{symbolname} {

    /* walk over the jump instruction mnemonic */
    for ( p = yytext; !isspace( *p ); p++ );

    /* walk over the whitespace between the opcode and the operand */
    for ( ; isspace( *p ); p++ );

    setLower( p );
    /* local labels are either '[0-9]+$' or '\?\?[0-9]+' which are
     * not valid {symbolname}s so we only get here if the target
     * of the (call|jmp) is a global symbol
     */
    sn_insert_xref( SN_REF_TO_FUNCTION	/* referenced symbol type	*/
		, SN_FUNC_DEF		/* scope type			*/
		, SN_REF_SCOPE_GLOBAL	/* scope level			*/
		, NULL			/* class name			*/
		, last_label.name	/* calling function name	*/
		, NULL			/* argument types		*/
		, NULL			/* namespace of symbol		*/
		, p			/* referenced symbol		*/
		, NULL			/* referenced argument types	*/
		, sn_current_file()	/* file name			*/
		, sn_line()		/* line number			*/
		, SN_REF_PASS		/* access type			*/
		);
    sn_advance_column( yyleng );
}

^{ws}*{symbolname}{ws}+([Mm][Aa][Cc][Rr][Oo]) {

    /* walk over the indentation (if any) */
    for ( p = yytext; isspace( *p ); sn_advance_column( 1 ), p++ );

    /* remember where macro name starts */
    savep = p;
    last_macro.column = sn_column();

    /* walk over the macro name */
    for ( ; !isspace( *p ); sn_advance_column( 1 ), p++ );
    *p = 0;				/* NULL-terminate macro name	*/

    setLower( savep );
    strcpy( last_macro.name, savep );
    last_macro.line = sn_line();
    last_macro.length = strlen( savep );
    last_macro.flag = 1;

    sn_advance_column( yytext + yyleng - p );
    BEGIN( MACRO );
}

<MACRO>^{ws}*{symbolname}{ws}+([Mm][Aa][Cc][Rr][Oo])	{
    last_macro.flag++;			/* handle nested macros */
    sn_advance_column( yyleng );
}

<MACRO>.		{		/* throw away any macro definition */
      sn_advance_column( yyleng );
}

<MACRO>\n		{		/* throw away any macro definition */
      sn_advance_line();
      sn_reset_column();
}

<MACRO>^{ws}*([Ee][Nn][Dd][Mm]) {

    sn_advance_column( yyleng );
    last_macro.flag--;
    if ( last_macro.flag == 0 )
    {
	sn_insert_symbol( SN_MACRO_DEF	/* symbol type			*/
		, NULL			/* class name			*/
		, last_macro.name	/* identifier			*/
		, sn_current_file()	/* file name			*/
		, last_macro.line	/* start line number		*/
		, last_macro.column	/* start column number		*/
		, sn_line()		/* end line number		*/
		, sn_column()		/* end column number		*/
		, 0			/* attributes			*/
		, NULL			/* return types			*/
		, NULL			/* argument types		*/
		, NULL			/* argument names		*/
		, NULL			/* comment			*/
		, last_macro.line	/* highlighting start line	*/
		, last_macro.column	/* highlighting start column	*/
		, last_macro.line	/* highlighting end line	*/
		, last_macro.column + last_macro.length
					/* highlighting end column	*/
		);

	BEGIN( INITIAL );
    }
}

^{ws}*{symbolname}{ws}+([Ee][Qq][Uu]){ws}+ {

    /* walk over the indentation (if any) */
    for ( p = yytext; isspace( *p ); sn_advance_column( 1 ), p++ );

    /* remember where equality name starts */
    savep = p;

    /* walk over the equality name */
    for ( ; !isspace( *p ); sn_advance_column( 1 ), p++ );
    *p = 0;				/* NULL-terminate equality name	*/

    setLower( savep );
    sn_insert_symbol( SN_CONS_DEF	/* symbol type			*/
	    , NULL			/* class name			*/
	    , savep			/* identifier			*/
	    , sn_current_file()		/* file name			*/
	    , sn_line()			/* start line number		*/
	    , sn_column() - strlen( savep )
					/* start column number		*/
	    , sn_line()			/* end line number		*/
	    , sn_column()		/* end column number		*/
	    , 0				/* attributes			*/
	    , NULL			/* return types			*/
	    , NULL			/* argument types		*/
	    , NULL			/* argument names		*/
	    , NULL			/* comment			*/
	    , sn_line()			/* highlighting start line	*/
	    , sn_column() - strlen( savep )
					/* highlighting start column	*/
	    , sn_line()			/* highlighting end line	*/
	    , sn_column()		/* highlighting end column	*/
	    );

    sn_advance_column( yytext + yyleng - p );
}

^{ws}*({symbolname}":"){ws}*([Dd][BbSsWw]){ws}+	{

    /* walk over the indentation (if any) */
    for ( p = yytext; isspace( *p ); sn_advance_column( 1 ), p++ );

    /* remember where symbol name starts */
    savep = p;

    /* walk over the symbol name */
    for ( ; *p != ':'; sn_advance_column( 1 ), p++ );
    *p = 0;				/* NULL-terminate symbol name	*/

    setLower( savep );
    sn_insert_symbol( SN_GLOB_VAR_DEF	/* symbol type			*/
	    , NULL			/* class name			*/
	    , savep			/* identifier			*/
	    , sn_current_file()		/* file name			*/
	    , sn_line()			/* start line number		*/
	    , sn_column() - strlen( savep )
					/* start column number		*/
	    , sn_line()			/* end line number		*/
	    , sn_column()		/* end column number		*/
	    , 0				/* attributes			*/
	    , NULL			/* return types			*/
	    , NULL			/* argument types		*/
	    , NULL			/* argument names		*/
	    , NULL			/* comment			*/
	    , sn_line()			/* highlighting start line	*/
	    , sn_column() - strlen( savep )
					/* highlighting start column	*/
	    , sn_line()			/* highlighting end line	*/
	    , sn_column()		/* highlighting end column	*/
	    );

    sn_advance_column( yytext + yyleng - p );
}

^{ws}*({symbolname}":") {

    /* walk over the indentation (if any) */
    for ( p = yytext; isspace( *p ); sn_advance_column( 1 ), p++ );

    /* remember where symbol name starts */
    savep = p;

    /* walk over the symbol name */
    for ( ; *p != ':'; sn_advance_column( 1 ), p++ );
    *p = 0;				/* NULL-terminate symbol name	*/

    emit_func_defn();

    setLower( savep );
    strcpy( last_label.name, savep );
    last_label.line = sn_line();
    last_label.column = sn_column() - strlen( savep );
    last_label.length = strlen( savep );
    last_label.flag = 1;

    sn_advance_column( yytext + yyleng - p );
}

(([Aa][CcDdNn])|([Cc][Pp])|([OoXx][Rr])|([Ss][BbUu]))[Ii]{ws}+{symbolname}	{

    /* walk over instruction mnemonic */
    for ( p = yytext; !isspace( *p ); p++ );

    /* walk over the whitespace between the opcode and the operand */
    for ( ; isspace( *p ); p++ );

    setLower( p );
    sn_insert_xref( SN_REF_TO_CONSTANT	/* referenced symbol type	*/
	    , SN_FUNC_DEF		/* scope type			*/
	    , SN_REF_SCOPE_GLOBAL	/* scope level			*/
	    , NULL			/* class name			*/
	    , last_label.name		/* calling function name	*/
	    , NULL			/* argument types		*/
	    , NULL			/* namespace of symbol		*/
	    , p				/* referenced symbol		*/
	    , NULL			/* referenced argument types	*/
	    , sn_current_file()		/* file name			*/
	    , sn_line()			/* line number			*/
	    , SN_REF_READ		/* access type			*/
	    );
    sn_advance_column( yyleng );
}

([Ll]([Dd][Aa])|([Hh][Ll][Dd])){ws}+{symbolname}	{

    /* walk over instruction mnemonic */
    for ( p = yytext; !isspace( *p ); p++ );

    /* walk over the whitespace between the opcode and the operand */
    for ( ; isspace( *p ); p++ );

    setLower( p );
    sn_insert_xref( SN_REF_TO_GLOB_VAR	/* referenced symbol type	*/
	    , SN_FUNC_DEF		/* scope type			*/
	    , SN_REF_SCOPE_GLOBAL	/* scope level			*/
	    , NULL			/* class name			*/
	    , last_label.name		/* calling function name	*/
	    , NULL			/* argument types		*/
	    , NULL			/* namespace of symbol		*/
	    , p				/* referenced symbol		*/
	    , NULL			/* referenced argument types	*/
	    , sn_current_file()		/* file name			*/
	    , sn_line()			/* line number			*/
	    , SN_REF_READ		/* access type			*/
	    );
    sn_advance_column( yyleng );
}

([Ss]([Tt][Aa])|([Hh][Ll][Dd])){ws}+{symbolname}	{

    /* walk over instruction mnemonic */
    for ( p = yytext; !isspace( *p ); p++ );

    /* walk over the whitespace between the opcode and the operand */
    for ( ; isspace( *p ); p++ );

    setLower( p );
    sn_insert_xref( SN_REF_TO_GLOB_VAR	/* referenced symbol type	*/
	    , SN_FUNC_DEF		/* scope type			*/
	    , SN_REF_SCOPE_GLOBAL	/* scope level			*/
	    , NULL			/* class name			*/
	    , last_label.name		/* calling function name	*/
	    , NULL			/* argument types		*/
	    , NULL			/* namespace of symbol		*/
	    , p				/* referenced symbol		*/
	    , NULL			/* referenced argument types	*/
	    , sn_current_file()		/* file name			*/
	    , sn_line()			/* line number			*/
	    , SN_REF_WRITE		/* access type			*/
	    );
    sn_advance_column( yyleng );
}

([Ll][Xx][Ii]){ws}+{allreg16}{ws}*,{ws}*{symbolname}	{

    /* walk over instruction mnemonic and register definition */
    for ( p = yytext; *p != ','; p++ );

    /* walk over the whitespace between the comma and the second operand */
    for ( ; isspace( *p ); p++ );

    setLower( p );
    sn_insert_xref( SN_REF_TO_CONSTANT	/* referenced symbol type	*/
	    , SN_FUNC_DEF		/* scope type			*/
	    , SN_REF_SCOPE_GLOBAL	/* scope level			*/
	    , NULL			/* class name			*/
	    , last_label.name		/* calling function name	*/
	    , NULL			/* argument types		*/
	    , NULL			/* namespace of symbol		*/
	    , p				/* referenced symbol		*/
	    , NULL			/* referenced argument types	*/
	    , sn_current_file()		/* file name			*/
	    , sn_line()			/* line number			*/
	    , SN_REF_READ		/* access type			*/
	    );
    sn_advance_column( yyleng );
}

([Mm][Vv][Ii]){ws}+{allreg8}{ws}*,{ws}*{symbolname}	{

    /* walk over instruction mnemonic and register definition */
    for ( p = yytext; *p != ','; p++ );

    /* walk over the whitespace between the comma and the second operand */
    for ( ; isspace( *p ); p++ );

    setLower( p );
    sn_insert_xref( SN_REF_TO_CONSTANT	/* referenced symbol type	*/
	    , SN_FUNC_DEF		/* scope type			*/
	    , SN_REF_SCOPE_GLOBAL	/* scope level			*/
	    , NULL			/* class name			*/
	    , last_label.name		/* calling function name	*/
	    , NULL			/* argument types		*/
	    , NULL			/* namespace of symbol		*/
	    , p				/* referenced symbol		*/
	    , NULL			/* referenced argument types	*/
	    , sn_current_file()		/* file name			*/
	    , sn_line()			/* line number			*/
	    , SN_REF_READ		/* access type			*/
	    );
    sn_advance_column( yyleng );
}

";".*\n {

    /* walk over any spaces after the leading ';' */
    for ( p = yytext, p++; isspace( *p ); p++ );

    sn_insert_comment( NULL		/* class name			*/
	    , last_label.name		/* function name		*/
	    , sn_current_file()		/* file name			*/
	    , p				/* comment			*/
	    , sn_line()			/* start line of comment	*/
	    , sn_column()		/* start column of comment	*/
	    );

    sn_advance_line();
    sn_reset_column();
}

{string}	{
      sn_advance_column( yyleng );	/* eat string literals */
}

.		{
      sn_advance_column( yyleng );	/* eat intel8085 code text */
}

\n		{
      sn_advance_line();
      sn_reset_column();
}

%%
  
void
reset()
{
      sn_reset_line();
      sn_reset_column();
}

int
main
    ( int argc
    , char *argv[]
    )
{
      return sn_main( argc, argv, group, &yyin, yylex, reset );
}

