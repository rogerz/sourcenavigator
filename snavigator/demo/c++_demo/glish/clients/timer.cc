// $Header$

#include "system.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <osfcn.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/time.h>

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#include "Glish/Client.h"

#include "Channel.h"


inline int streq( const char* a, const char* b )
	{
	return ! strcmp( a, b );
	}


// given a value like 4.32, returns a timeval struct with, for example,
// the seconds field set to 4 and the microseconds field set to 320000
struct timeval* build_timeout( double timeout_val )
	{
	static struct timeval result;

	if ( timeout_val < 0 )
		return 0;	// invalid time -> block till next, valid time

	result.tv_sec = (int) timeout_val;
	result.tv_usec = (int) ((timeout_val - (int) timeout_val) * 1000000.0);

	return &result;
	}


int main( int argc, char** argv )
	{
	Client c( argc, argv );

	char* prog_name = argv[0];
	++argv, --argc;

	double timeout_val;	// how long we're waiting for

	// by default, first time around we want to block till we get an event
	struct timeval* timeout = 0;

	int one_shot = 0;	// whether we should only fire once per "delay"
	if ( argc > 0 && streq( argv[0], "-oneshot" ) )
		{
		++one_shot;
		++argv, --argc;
		}

	if ( argc > 0 )
		{
		timeout_val = atof( argv[0] );
		timeout = build_timeout( timeout_val );
		}

	fd_set selection_mask;
	FD_ZERO( &selection_mask );

	for ( ; ; )
		{
		c.AddInputMask( &selection_mask );

		int status = select( FD_SETSIZE, &selection_mask, 0, 0,
					timeout );

		if ( status < 0 )
			{
			fprintf( stderr, "%s: ", prog_name );
			perror( "select() returned for unknown reason" );
			exit( 1 );
			}
		
		else if ( status == 0 )
			{ // timeout elapsed
			Value val( timeout_val );
			c.PostEvent( "ready", &val );

			if ( one_shot )
				// don't rearm
				timeout = 0;

			else
				timeout = build_timeout( timeout_val );
			}

		else if ( c.HasClientInput( &selection_mask ) )
			{
			GlishEvent* e = c.NextEvent();

			if ( ! e )
				return 0;

			if ( streq( e->name, "interval" ) )
				{
				timeout_val = e->value->DoubleVal();
				timeout = build_timeout( timeout_val );
				}

			else if ( streq( e->name, "sleep" ) )
				{
				// "sleep" is a request/reply event,
				// mainly for testing purposes.
				int duration = e->value->IntVal();

				sleep( duration );

				c.Reply( e->value );
				}

			else
				c.Unrecognized();
			}

		else
			{
			fprintf( stderr, "%s: bogus select() return\n",
				 prog_name );
			exit( 1 );
			}
		}
	}
