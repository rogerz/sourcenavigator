// $Header$

#include "system.h"

#include <stdio.h>
#include <string.h>
#include <osfcn.h>
#include <sys/file.h>
#include "Executable.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


extern "C" {
char* getenv( const char* );
char* strdup( const char* );
}


Executable::Executable( const char* arg_executable )
	{
	executable = strdup( arg_executable );
	exec_error = has_exited = 0;
	}


Executable::~Executable()
	{
	delete executable;
	}


char* which_executable( const char* exec_name )
	{
	char* path = getenv( "PATH" );

	if ( ! path || exec_name[0] == '/' || exec_name[0] == '.' )
		{
		if ( access( exec_name, X_OK ) == 0 )
			return strdup( exec_name );

		else
			return 0;
		}

	char directory[1024];

	char* dir_beginning = path;
	char* dir_ending = path;

	while ( *dir_beginning )
		{
		while ( *dir_ending && *dir_ending != ':' )
			++dir_ending;

		int hold_char = *dir_ending;

		if ( hold_char )
			*dir_ending = '\0';

		sprintf( directory, "%s/%s", dir_beginning, exec_name );

		if ( hold_char )
			*(dir_ending++) = hold_char;

		if ( access( directory, X_OK ) == 0 )
			return strdup( directory );

		dir_beginning = dir_ending;
		}

	return 0;
	}
