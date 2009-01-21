// $Header$

#include <stream.h>
#include "Glish/Object.h"


int line_num;

void GlishObject::Describe( ostream& s ) const
	{
	DescribeSelf( s );
	}

void GlishObject::DescribeSelf( ostream& s ) const
	{
	s << description;
	}
