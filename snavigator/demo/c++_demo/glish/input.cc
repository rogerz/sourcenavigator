// $Header$

#include <stream.h>
#include <osfcn.h>

#include "input.h"
#include "Sequencer.h"

int interactive_read( FILE* file, const char prompt[], char buf[],
			int max_size )
	{
#ifndef __GNUC__
        static int did_sync = 0;
        if ( ! did_sync )
		{
		ios::sync_with_stdio();
		did_sync = 1;
		}
#endif

	cout << prompt;
	cout.flush();

	current_sequencer->EventLoop();

	return read( fileno( file ), buf, max_size );
	}
