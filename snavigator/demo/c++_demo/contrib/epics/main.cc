// $Header$

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <osfcn.h>
#include <sys/types.h>

#include "Glish/Client.h"
#include "EpicsChannel.h"

int main( int argc, char** argv )
	{
	Client c( argc, argv );

	if ( argc != 2 )
		{
		fprintf( stderr, "usage: %s channel-name\n", argv[0] );
		return 1;
		}

	EpicsChannel* chan = new EpicsChannel( argv[1], 5.0 );

	if ( ! chan->Connected() )
		{
		c.PostEvent( "connection_failed", new Value( chan->Status() ) );
		return 1;
		}

	fd_set chan_fds = chan->InputSources();
	fd_set fds;

	for ( ; ; )
		{
		fds = chan_fds;
		c.AddInputMask( &fds );

		while ( select( FD_SETSIZE, &fds, 0, 0, 0 ) < 0 )
			{
			if ( errno != EINTR )
				{
				fprintf( stderr, "%s: ", argv[0] );
				perror( "error during select()" );
				return 0;
				}
			}

		for( int i = 0; i < FD_SETSIZE; ++i )
			if ( FD_ISSET( i, &chan_fds ) &&
			     FD_ISSET( i, &fds ) )
				{
				fprintf( stderr, "channel access\n" );
				}

		if ( ! c.HasClientInput( &fds ) )
			continue;

		GlishEvent* e = c.NextEvent( &fds );

		if ( ! e )
			break;

		if ( ! strcmp( e->name, "get" ) )
			{
			Value* v = chan->Get();

			if ( v )
				c.Reply( v );
			else
				c.PostEvent( "error",
					new Value( chan->Status() ) );

			Unref( v );
			}

		else if ( ! strcmp( e->name, "set" ) )
			{
			if ( ! chan->Set( e->value ) )
				c.PostEvent( "bad_set",
					new Value( chan->Status() ) );
			}

		else
			c.Unrecognized();
		}

	delete chan;

	return 0;
	}
