// Sample Glish program - a "sqrt" server
//
// This Glish client responds to "compute" events by generating an "answer"
// event whose value is the square-root of the value of the "compute" event.
// The "compute" event's value must be of type "double"; the "answer" is
// also of type "double".
//
// Compile this program using:
//
//	C++ -I$ISTKPLACE/include -c sqrt_server.cc
//	C++ -o sqrt_server sqrt_server.o -L$ISTKPLACE/lib/$ARCH \
//		-lglish -lsds -lm
//
// where "C++" is the local C++ compiler (e.g., "g++" or "CC").
//
//
// See the file "sqrt_server.g" for a Glish program for testing this
// client.  Invoking "glish sqrt_server.g" will run that program.


#include <stdio.h>
#include <math.h>
#include <string.h>

#include "Glish/Client.h"


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
			{
			// Okay, access the event's value.  This is
			// a pointer to an object of type "Value".
			Value* val = e->value;

			// Check to make sure the event's value is of
			// type "double".
			if ( val->Type() != TYPE_DOUBLE )
				c.PostEvent( "error", "argument not double" );

			else
				{
				// In general the event value will be an
				// array and not a single element.  Get
				// the number of elements.
				int num_elements = val->Length();

				// Get a pointer to the individual elements.
				double* elements = val->DoublePtr();

				// We're going to create a new Value to
				// send.  We need an array of double's
				// to hold the square-roots.
				double* result = new double[num_elements];

				// Spin through the "compute" event's elements
				// and put the corresponding square-roots
				// into "result".
				for ( int i = 0; i < num_elements; ++i )
					result[i] = sqrt( elements[i] );

				// Now create a new Value corresponding to
				// the result.
				Value* result_val =
					new Value( result, num_elements );

				// Send the result Value as the value of
				// an "answer" event.
				c.PostEvent( "answer", result_val );

				// When done with a Value we need to
				// Unref() it.  Note that we *don't* also
				// do "delete result;" to reclaim the
				// memory used by the "result" array.
				// Unref()'ing the Value will make sure
				// that that memory is reclaimed once
				// there are no more references to the
				// Value.
				Unref( result_val );

				// We also do *not* do "Unref( val );".
				// The event's value will be automatically
				// Unref()'d upon our next call to NextEvent().
				}
			}

		else
			// Not an event that we respond to.
			c.PostEvent( "error", "don't understand %s", e->name );
		}
	}
