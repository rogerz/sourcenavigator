// Sample Glish program - a "plus/minus" server to demonstrate Glish records
//
// This Glish client receives "compute" events that have a record value
// with (at least) "x" and a "y" fields.  It generates an "answer" event
// whose value is a record with "sum" and "difference" fields, representing
// the sum and difference of the "x" and "y" fields.
//
// Compile this program using:
//
//	C++ -I$ISTKPLACE/include -c plus_minus_server.cc
//	C++ -o plus_minus_server plus_minus_server.o -L$ISTKPLACE/lib/$ARCH \
//		-lglish -lsds -lm
//
// where "C++" is the local C++ compiler (e.g., "g++" or "CC").
//
//
// See the file "plus_minus_server.g" for a Glish program for testing this
// client.  Invoking "glish plus_minus_server.g" will run that program.


#include <string.h>

#include "Glish/Client.h"


void do_plus_minus( Client* client, Value* event_val );


main( int argc, char** argv )
	{
	// Create a Glish client object.  We pass "argc" and "argv" to
	// the client so it can figure out whether we were invoked from
	// Glish.  Any Glish-related arguments will be stripped out upon
	// return from the constructor.
	Client c( argc, argv );


	// Enter the standard Glish event loop.  Keep calling the client's
	// NextEvent() method until it returns an nil GlishEvent pointer.
	GlishEvent* e;
	while ( (e = c.NextEvent()) )
		{
		// A GlishEvent object has two fields of interest: the event
		// name and the event value.  First check the name to make
		// sure it's an event that we respond to.

		if ( ! strcmp( e->name, "compute" ) )
			do_plus_minus( &c, e->value );

		else
			// Not an event that we respond to.
			c.PostEvent( "error", "don't understand %s", e->name );
		}
	}


void do_plus_minus( Client* c, Value* val )
	{
	int len_x, len_y;
	const double* x = val->FieldDoublePtr( "x", len_x );
	const double* y = val->FieldDoublePtr( "y", len_y );

	if ( ! x || ! y )
		{
		c->PostEvent( "error",
				"compute record lacks numeric x or y field" );
		return;
		}

	int len = len_x < len_y ? len_x : len_y;

	double* sum_ptr = new double[len];
	double* diff_ptr = new double[len];

	for ( int i = 0; i < len; ++i )
		{
		sum_ptr[i] = x[i] + y[i];
		diff_ptr[i] = x[i] - y[i];
		}

	Value* result = create_record();

	result->SetField( "sum", sum_ptr, len );
	result->SetField( "difference", diff_ptr, len );

	c->PostEvent( "answer", result );

	// We're done with "result".
	Unref( result );
	}
