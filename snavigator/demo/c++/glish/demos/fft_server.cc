#include <string.h>
#include "Glish/Client.h"

// Computes the FFT of the first ``len" elements of ``in", returning
// the real part in ``real" and the imaginary part in ``imag".
extern void fft( double* in, int len, double* real, double* imag );

main( int argc, char** argv )
	{
	Client c( argc, argv );

	GlishEvent* e;
	while ( (e = c.NextEvent()) )
		{
		if ( ! strcmp( e->name, "fft" ) )
			{ // an ``fft" event
			Value* val = e->value;

			// Make sure the value's type is ``double".
			val->Polymorph( TYPE_DOUBLE );
			int num = val->Length();

			// Get a pointer to the individual elements.
			double* elements = val->DoublePtr();

			// Create arrays for results.
			double* real = new double[num];
			double* imag = new double[num];

			// Compute the FFT.
			fft( elements, num, real, imag );

			// Create a record for returning the
			// two arrays.
			Value* r = create_record();
			r->SetField( "real", real, num );
			r->SetField( "imag", imag, num );

			c.PostEvent( "answer", r );
			Unref( r );
			}
		else
			c.Unrecognized();
		}
	return 0;
	}
