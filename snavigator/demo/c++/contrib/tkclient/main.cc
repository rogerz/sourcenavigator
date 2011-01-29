// $Header$

#include <string.h>
#include <stdlib.h>

#include "TK_Client.h"

TK_Client* tk_client;

void event_callback( GlishEvent* e )
	{
	const char* name = e->name;

	if ( ! strcmp( name, "sync" ) )
		tk_client->Reply( false_value );
	}

void exit_callback()
	{
	delete tk_client;
	exit( 0 );
	}

int main( int argc, char** argv )
	{
	char* display = getenv( "DISPLAY" );
	char* prog_name = argv[0];
	char* name = prog_name;

	tk_client = new TK_Client( argc, argv, name, display,
					event_callback, exit_callback );
	Tcl_Interp* tcl = tk_client->TCL();

	while ( --argc > 0 )
		{
		if ( Tcl_EvalFile( tcl, *++argv ) != TCL_OK )
			{
			fprintf( stderr, "%s: Tcl errors on file %s: %s\n",
				prog_name, argv[0], tcl->result );
			exit( 1 );
			}
		}

	Tk_MainLoop();

	delete tk_client;

	return 0;
	}
