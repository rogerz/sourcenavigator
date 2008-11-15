// $Header$

#include <stream.h>

#include "Glish/Client.h"

#include "Reporter.h"

int main( int argc, char** argv )
	{
	Client c( argc, argv );

	cout << argv[0] << " fired up, arg list is: ";

	for ( int i = 1; i < argc; ++i )
		{
		cout << argv[i];
		if ( i < argc - 1 )
			cout << ", ";
		}

	cout << "\n";

	for ( GlishEvent* e; (e = c.NextEvent()); )
		message->Report( "received event, name = ", e->name,
				 ", value =", e->value );

	return 0;
	}
