// $Header$

#include <stdio.h>
#include <string.h>

#include "Glish/Value.h"
#include "Glish/Client.h"

#include "Channel.h"
#include "RemoteExec.h"


RemoteExec::RemoteExec( Channel* arg_daemon_channel,
			const char* arg_executable, const char** argv )
    : Executable( arg_executable )
	{
	daemon_channel = arg_daemon_channel;

	char id_buf[64];
	static int remote_exec_id = 0;

	sprintf( id_buf, "remote task %d", ++remote_exec_id );
	id = strdup( id_buf );

	int argc = 0;
	while ( argv[argc] )
		++argc;

	charptr* client_argv = new charptr[argc + 1];

	client_argv[0] = id;
	for ( int i = 1; i <= argc; ++i )
		client_argv[i] = argv[i-1];

	Value argv_value( client_argv, argc + 1, COPY_ARRAY );
	send_event( daemon_channel->WriteFD(), "client", &argv_value );

	delete client_argv;
	}


RemoteExec::~RemoteExec()
	{
	if ( Active() )
		{
		Value id_value( id );
		send_event( daemon_channel->WriteFD(), "kill", &id_value );
		}

	delete id;
	}


void RemoteExec::Ping()
	{
	if ( Active() )
		{
		Value id_value( id );
		send_event( daemon_channel->WriteFD(), "ping", &id_value );
		}
	}


int RemoteExec::Active()
	{
	if ( has_exited || exec_error )
		return 0;
	else
		return 1;	// ### query agent?
	}
