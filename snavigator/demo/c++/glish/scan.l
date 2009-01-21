%{
// $Header$

#include <string.h>
#include <iostream.h>
#include <osfcn.h>
#include <ctype.h>

#include "Expr.h"
#include "BuiltIn.h"
#include "Reporter.h"
#include "Sequencer.h"
#include "input.h"
#include "y.tab.h"

extern YYSTYPE yylval;

char* input_file_name;

// Whether the last token might be the end of a statement.
int statement_can_end = 0;

int interactive = 0;
int first_line = 1;

// When we convert a newline to a ';' we don't bump the line count just
// yet, as that'll result in the line count being one too high for the
// statement that ends with that ';'.  So instead we set this flag and
// subsequently bump the line count on the next call.
int bump_line_num = 0;

// A class for holding information on an input file: where we are in
// it (i.e., its flex buffer), its name, and the current line number.
// InputFileInfo objects are created without any parameters; they glean
// all the pertinent information from the globals present in input.h.
// To restore the globals to the state they had when the object was
// created, simply delete it.
class InputFileInfo {
public:
	InputFileInfo();
	~InputFileInfo();

protected:
	YY_BUFFER_STATE buf;
	char* filename;
	int line;
	int save_first_line;
	int save_bump_line_num;
	int save_statement_can_end;
	};

// List of input buffers associated with included files.
declare(PList,InputFileInfo);
PList(InputFileInfo) file_list;

static int scanner_read( char buf[], int max_size );
static void new_file();

#undef YY_INPUT
#define YY_INPUT(buf,result,max_size)	\
	result = scanner_read( buf, max_size );

#define RETURN_CONSTANT(value)					\
	{							\
	statement_can_end = 1;					\
	yylval.expr = new ConstExpr( new Value( value ) );	\
	return TOK_CONSTANT;					\
	}

#define RETURN_LAST_EVENT(type)					\
	{							\
	statement_can_end = 1;					\
	yylval.event_type = type;				\
	return TOK_LAST_EVENT;					\
	}

#define RETURN_COMPOUND(type)					\
	{							\
	yylval.ival = type;					\
	return TOK_ASSIGN;					\
	}

#define RETURN_ACTIVATE(is_activate)				\
	{							\
	statement_can_end = 1;					\
	yylval.ival = is_activate;				\
	return TOK_ACTIVATE;					\
	}

#undef YY_BREAK
// The following makes actions by default fall through to the next
// action.  We are careful then that every action ends in a "return"
// or a break.  The reason for bother with this is so that picky
// compilers don't complain about the zillions of actions that
// terminate with a "return" followed by a "break".
#define YY_BREAK

#define yywrap() 1
%}

%x QUOTE RECORD_FIELD SCAN_EVENT_NAME INCL ATTR ATTR_QUOTE

	static char literal[512];
	static int is_double_quote;


ID	[A-Za-z_][A-Za-z0-9_]*
WS	[ \t]+
OWS	[ \t]*
D	[0-9]

FLOAT	(({D}*"."?{D}+)|({D}+"."?{D}*))(e[-+]?{D}+)?

%%
	// Whether to return a ';' token if a newline is seen.
	int newline_is_semi = statement_can_end;

	if ( bump_line_num )
		++line_num;

	bump_line_num = 0;
	statement_can_end = 0;


[!:;,|&({[+*/%^=-]	return yytext[0];

[)\]]		statement_can_end = 1; return yytext[0];

"}"		{
		if ( newline_is_semi )
			{
			unput( '}' );
			return ';';
			}
		else
			return '}';
		}

"++"|"--"	{
		error->Report( yytext, " is not a valid Glish operator" );
		break;
		}

"."		BEGIN(RECORD_FIELD); return yytext[0];
"->"		BEGIN(SCAN_EVENT_NAME); return TOK_ARROW;
"::"		BEGIN(ATTR); return TOK_ATTR;

"..."		return TOK_ELLIPSIS;

"=="		return TOK_EQ;
"!="		return TOK_NE;
"<="		return TOK_LE;
">="		return TOK_GE;
"<"		return TOK_LT;
">"		return TOK_GT;

"&&"		return TOK_AND_AND;
"||"		return TOK_OR_OR;

":="		RETURN_COMPOUND(0);

"+:="		RETURN_COMPOUND(yytext[0]);
"-:="		RETURN_COMPOUND(yytext[0]);
"*:="		RETURN_COMPOUND(yytext[0]);
"/:="		RETURN_COMPOUND(yytext[0]);
"%:="		RETURN_COMPOUND(yytext[0]);
"^:="		RETURN_COMPOUND(yytext[0]);

"&:="		RETURN_COMPOUND(yytext[0]);
"|:="		RETURN_COMPOUND(yytext[0]);

"&&:="		RETURN_COMPOUND(TOK_AND_AND);
"||:="		RETURN_COMPOUND(TOK_OR_OR);

activate	RETURN_ACTIVATE(1);
await		return TOK_AWAIT;
break		statement_can_end = 1; return TOK_BREAK;
const		return TOK_CONST;
deactivate	RETURN_ACTIVATE(0);
do		return TOK_DO;
else		return TOK_ELSE;
except		return TOK_EXCEPT;
exit		statement_can_end = 1; return TOK_EXIT;
for		return TOK_FOR;
func(tion)?	return TOK_FUNCTION;
if		return TOK_IF;
in		return TOK_IN;
link		return TOK_LINK;
local		return TOK_LOCAL;
next|continue	statement_can_end = 1; return TOK_LOOP;
only		return TOK_ONLY;
print		return TOK_PRINT;
ref		return TOK_REF;
request		return TOK_REQUEST;
return		statement_can_end = 1; return TOK_RETURN;
send		return TOK_SEND;
subseq(uence)?	return TOK_SUBSEQUENCE;
to		return TOK_TO;
unlink		return TOK_UNLINK;
val		return TOK_VAL;
whenever	return TOK_WHENEVER;
while		return TOK_WHILE;

"$agent"	RETURN_LAST_EVENT( EVENT_AGENT );
"$name"		RETURN_LAST_EVENT( EVENT_NAME );
"$value"	RETURN_LAST_EVENT( EVENT_VALUE );

F		RETURN_CONSTANT( glish_false );
T		RETURN_CONSTANT( glish_true );

include		BEGIN(INCL); break;

{ID}		{
		statement_can_end = 1;
		yylval.id = strdup( yytext );
		return TOK_ID;
		}

<RECORD_FIELD,ATTR,SCAN_EVENT_NAME>{ID}	{
		// We use a separate start condition for these names so
		// that they can include reserved words.

		BEGIN(INITIAL);
		statement_can_end = 1;
		yylval.id = strdup( yytext );
		return TOK_ID;
		}

<SCAN_EVENT_NAME>[*\[]	BEGIN(INITIAL); return yytext[0];

{D}+		RETURN_CONSTANT( atoi( yytext ) );

{FLOAT}		{
		RETURN_CONSTANT( atof( yytext ) );
		}

({FLOAT}[-+])?{FLOAT}i {
		RETURN_CONSTANT( atodcpx( yytext ) );
		}

["']		{
		literal[0] = '\0';
		is_double_quote = yytext[0] == '"';
		BEGIN(QUOTE);
		break;
		}

<ATTR>"["	{
		BEGIN(INITIAL);
		return yytext[0];
		}

<ATTR>["']	{
		literal[0] = '\0';
		is_double_quote = yytext[0] == '"';
		BEGIN(ATTR_QUOTE);
		break;
		}

<QUOTE,ATTR_QUOTE>[^'"\n\\]+	strcat( literal, yytext ); break;
<QUOTE,ATTR_QUOTE>"\\n"		strcat( literal, "\n" ); break;
<QUOTE,ATTR_QUOTE>"\\t"		strcat( literal, "\t" ); break;
<QUOTE,ATTR_QUOTE>"\\r"		strcat( literal, "\r" ); break;
<QUOTE,ATTR_QUOTE>"\\f"		strcat( literal, "\f" ); break;
<QUOTE,ATTR_QUOTE>"\\"\n{OWS}	++line_num; break;
<QUOTE,ATTR_QUOTE>"\\".		strcat( literal, &yytext[1] ); break;

<QUOTE>\"	|
<QUOTE>\'	{
		if ( (is_double_quote && yytext[0] == '\'') ||
		     (! is_double_quote && yytext[0] == '"') )
			strcat( literal, &yytext[0] );

		else
			{
			BEGIN(INITIAL);

			if ( is_double_quote )
				{
				statement_can_end = 1;
				yylval.expr = new ConstExpr( split( literal ) );
				return TOK_CONSTANT;
				}

			else
				RETURN_CONSTANT( literal );
			}

		break;
		}

<ATTR_QUOTE>\"	|
<ATTR_QUOTE>\'	{
		if ( (is_double_quote && yytext[0] == '\'') ||
		     (! is_double_quote && yytext[0] == '"') )
			strcat( literal, &yytext[0] );

		else
			{
			BEGIN(INITIAL);
			statement_can_end = 1;
			yylval.id = strdup( literal );
			return TOK_ID;
			}

		break;
		}

<QUOTE,ATTR_QUOTE>\n	{
		error->Report( "unmatched quote (",
				is_double_quote ? "\"" : "'", ")" );
		++line_num;
		BEGIN(INITIAL);
		RETURN_CONSTANT( glish_false );
		}

<INCL>\"[^"]*\"	{
		yytext[yyleng - 1] = '\0';	// nuke trailing quote
		char* filename = &yytext[1];	// skip leading quote
		FILE* file = fopen( filename, "r" );

		if ( ! file )
			error->Report( "can't open include file \"",
					filename, "\"" );

		else
			{
			// Save current file information.
			file_list.append( new InputFileInfo() );
			input_file_name = strdup( filename );
			YY_BUFFER_STATE incl_buf =
				yy_create_buffer( file, YY_BUF_SIZE );
			yy_switch_to_buffer( incl_buf );
			new_file();
			}

		BEGIN(INITIAL);
		break;
		}

<INITIAL,RECORD_FIELD,SCAN_EVENT_NAME,ATTR>#.*		break; // comment
<INITIAL,RECORD_FIELD,SCAN_EVENT_NAME,INCL,ATTR>{WS}+	break; // eat whitespace

<INITIAL,RECORD_FIELD,SCAN_EVENT_NAME,ATTR>\\\n	{
		++line_num;
		first_line = 0;
		break;
		}

<ATTR>\n	{
		// Treat this case as ending the newline, so that
		// "foo := bar::" works as expected.
		BEGIN(INITIAL);
		bump_line_num = 1;
		return ';';
		}

<ATTR>.		{ // Allow "bar:::=" to work.
		unput( yytext[0] );
		statement_can_end = 1;
		BEGIN(INITIAL);
		break;
		}

<*>\n		{
		if ( newline_is_semi )
			{
			bump_line_num = 1;
			return ';';
			}

		++line_num;
		first_line = 0;
		break;
		}

<*>.		{
		error->Report( "unrecognized character '", yytext, "'" );
		statement_can_end = 1;
		BEGIN(INITIAL);
		break;
		}

<<EOF>>		{
		delete input_file_name;		// done with file name

		int nesting = file_list.length();
		if ( nesting > 0 )
			{
			// We're done with this include file, delete it.
			yy_delete_buffer( YY_CURRENT_BUFFER );

			// Pop back to previous file.
			delete file_list.remove_nth( nesting - 1 );
			break;
			}

		else
			yyterminate();
		}
%%


InputFileInfo::InputFileInfo()
	{
	buf = YY_CURRENT_BUFFER;
	filename = input_file_name;
	line = line_num;
	save_first_line = first_line;
	save_bump_line_num = bump_line_num;
	save_statement_can_end = statement_can_end;
	}

InputFileInfo::~InputFileInfo()
	{
	yy_switch_to_buffer( buf );
	input_file_name = filename;
	line_num = line;
	first_line = save_first_line;
	bump_line_num = save_bump_line_num;
	statement_can_end = save_statement_can_end;
	}


// If non-null, we're scanning an array of strings, with our present
// position given by input_offset.
static const char** input_strings;
static int input_offset;

void scan_strings( const char** strings )
	{
	input_strings = strings;
	input_offset = 0;
	}

int scanner_read( char buf[], int max_size )
	{
	if ( input_strings )
		{
		const char* s = input_strings[input_offset];
		if ( ! s )
			{ // All done.
			input_strings = 0;
			return 0;
			}

		char* bufptr = buf;
		while ( max_size > 0 && s )
			{
			int len = strlen( s );
			if ( len >= max_size )
				// Okay, we've read enough.
				return bufptr - buf;

			strcpy( bufptr, s );
			bufptr[len] = '\n';
			++len;	// add in the newline
			bufptr += len;
			max_size -= len;

			// Move on to the next string.
			s = input_strings[++input_offset];
			}

		return bufptr - buf;
		}

	if ( interactive && yyin == stdin )
		{
		const char* prompt = first_line ? "- " : "+ ";
		return interactive_read( yyin, prompt, buf, max_size );
		}
	else
		return read( fileno( yyin ), buf, max_size );
	}

void restart_yylex( FILE* input_file )
	{
	static int first_call = 1;

	if ( yyin && yyin != stdin )
		fclose( yyin );

	yyin = input_file;

	new_file();

	if ( first_call )
		first_call = 0;
	else
		yyrestart( yyin );
	}

void new_file()
	{
	line_num = 1;
	first_line = 1;
	bump_line_num = 0;
	statement_can_end = 0;
	}
