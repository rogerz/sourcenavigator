// $Header$

#include <stream.h>
#include "Glish/Value.h"
#include "Frame.h"
#include "Reporter.h"


Frame::Frame( int frame_size, Value* param_info )
	{
	size = frame_size;
	missing = param_info ? param_info : empty_value();
	values = new Value*[size];

	for ( int i = 0; i < size; ++i )
		values[i] = 0;

	description = "<frame>";
	}


Frame::~Frame()
	{
	Unref( missing );

	for ( int i = 0; i < size; ++i )
		Unref( values[i] );
	delete values;
	}


Value*& Frame::FrameElement( int offset )
	{
	if ( offset < 0 || offset >= size )
		fatal->Report( "bad offset in Frame::FrameElement" );

	return values[offset];
	}
