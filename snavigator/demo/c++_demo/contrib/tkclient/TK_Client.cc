// $Header$

#include <stdlib.h>
#include <string.h>

#include "TK_Client.h"

class TK_FileHandler {
public:
	TK_FileHandler( int fd, TK_Client* client );
	~TK_FileHandler();

	TK_Client* MyClient()	{ return client; }
	int FD() const		{ return fd; }

protected:
	static void TK_Callback( ClientData cd, int mask );

	int fd;
	TK_Client* client;
};

TK_Client::TK_Client( int& argc, char** argv, char* name, char* display,
			EventCallback arg_event_callback,
			ExitCallback arg_exit_callback ) : Client(argc, argv)
	{
	event_callback = arg_event_callback;
	exit_callback = arg_exit_callback;

	tcl = Tcl_CreateInterp();
	Tcl_Init( tcl );

        tk = Tk_CreateMainWindow( tcl, display, name, name );
	if ( ! tk || Tk_Init( tcl ) != TCL_OK ||
	     ! Tcl_SetVar( tcl, "tcl_interactive", "0", TCL_GLOBAL_ONLY ) )
		{
		fprintf( stderr, "%s: couldn't open tk window: %s\n",
				name, tcl->result );
		exit( 1 );
		}

	Tcl_CreateCommand( tcl, (char*) "glish_send",
			SendEventCallback, ClientData(this), 0 );

	Tcl_CreateCommand( tcl, (char*) "glish_reply",
			ReplyCallback, ClientData(this), 0 );

	Tcl_CreateCommand( tcl, (char*) "glish_exit",
			TclExitCallback, ClientData(this), 0 );

	fd_set fds;
	FD_ZERO( &fds );
	AddInputMask( &fds );

	for ( int i = 0; i < FD_SETSIZE; ++i )
		if ( FD_ISSET( i, &fds ) )
			AddFile( i );
	}

TK_Client::~TK_Client()
	{
	Tcl_DeleteInterp( tcl );
	}

int TK_Client::TclError( const char* msg )
	{
	if ( strlen( msg ) >= sizeof tcl_buf )
		msg = "<Error message too long>";

	strcpy( tcl_buf, msg );
	tcl->result = tcl_buf;
	return TCL_ERROR;
	}

void TK_Client::FD_Change( int fd, bool add_flag )
	{
	if ( add_flag )
		AddFile( fd );
	else
		Tk_DeleteFileHandler( fd );
	}

void TK_Client::AddFile( int fd )
	{
	// ### memory leak
	TK_FileHandler* memory_leak = new TK_FileHandler( fd, this );
	}

void TK_Client::FileCallback( int fd )
	{
	fd_set fds;
	FD_ZERO( &fds );
	FD_SET( fd, &fds );

	GlishEvent* e = NextEvent( &fds );

	if ( ! e )
		{
		if ( exit_callback )
			exit_callback();
		else
			exit( 0 );
		}

	if ( ! strcmp( e->name, "Tcl" ) )
		{
		char* cmd = e->value->StringVal();

		if ( Tcl_Eval( tcl, cmd ) != TCL_OK )
			Error( "Tcl/TK error: %s", tcl->result );

		delete cmd;
		}

	else if ( ! strcmp( e->name, "TclN" ) )
		{
		char** cmd = e->value->StringPtr();

		for ( int i = 0; i < e->value->Length(); ++i )
			if ( Tcl_Eval( tcl, cmd[i] ) != TCL_OK )
				{
				Error( "Tcl/TK error: %s", tcl->result );
				return;
				}
		}

	else if ( event_callback )
		event_callback( e );

	else
		Unrecognized();
	}

int TK_Client::SendEventCallback( ClientData cd, Tcl_Interp* /* tcl */,
					int argc, char** argv )
	{
	TK_Client* me = (TK_Client*) cd;

	if ( argc != 3 )
		return me->TclError( "usage: glish_send name value" );

	me->PostEvent( argv[1], argv[2] );

	return TCL_OK;
	}

int TK_Client::ReplyCallback( ClientData cd, Tcl_Interp* /* tcl */,
					int argc, char** argv )
	{
	TK_Client* me = (TK_Client*) cd;

	if ( argc != 2 )
		return me->TclError( "usage: glish_reply value" );

	Value reply_val( argv[1] );
	me->Reply( &reply_val );

	return TCL_OK;
	}

int TK_Client::TclExitCallback( ClientData cd, Tcl_Interp* /* tcl */,
					int /* argc */, char** /* argv */ )
	{
	TK_Client* me = (TK_Client*) cd;

	if ( me->exit_callback )
		me->exit_callback();
	else
		exit( 0 );

	return TCL_OK;
	}


TK_FileHandler::TK_FileHandler( int arg_fd, TK_Client* arg_client )
	{
	fd = arg_fd;
	client = arg_client;
	Tk_CreateFileHandler( fd, TK_READABLE, TK_Callback, ClientData(this) );
	}

TK_FileHandler::~TK_FileHandler()
	{
	Tk_DeleteFileHandler( fd );
	}

void TK_FileHandler::TK_Callback( ClientData cd, int /* mask */ )
	{
	TK_FileHandler* fh = (TK_FileHandler*)(cd);
	fh->MyClient()->FileCallback( fh->FD() );
	}
