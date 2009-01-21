// $Header$

#include "system.h"

#include <stream.h>
#include <osfcn.h>
#include <errno.h>
#include <signal.h>
#include <sys/file.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>

#ifdef HAVE_SIGLIB_H
#include <sigLib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif

#include "LocalExec.h"


LocalExec::LocalExec( const char* arg_executable, const char** argv )
    : Executable( arg_executable )
	{
	MakeExecutable( argv );
	}

LocalExec::LocalExec( const char* arg_executable )
    : Executable( arg_executable )
	{
	const char* argv[2];
	argv[0] = arg_executable;
	argv[1] = 0;
	MakeExecutable( argv );
	}


LocalExec::~LocalExec()
	{
	if ( Active() )
		kill( pid, SIGTERM );
	}


void LocalExec::MakeExecutable( const char** argv )
	{
	pid = 0;
	exec_error = 1;
	has_exited = 0;

	if ( access( executable, X_OK ) < 0 )
		return;


	pid = int( vfork() );

	if ( pid == 0 )
		{ // child
		extern char** environ;
#ifndef POSIX
		execve( executable, (char **)argv, environ );
#else
		execve( executable, (char *const*)argv, environ );
#endif

		cerr << "LocalExec::MakeExecutable: couldn't exec ";
		perror( executable );
		_exit( -1 );
		}

	if ( pid > 0 )
		exec_error = 0;
	}


int LocalExec::Active()
	{
	if ( has_exited || exec_error )
		return 0;

	int status;
	int child_id = wait_for_pid( pid, &status, WNOHANG );

	if ( child_id == 0 )
		return 1;

	if ( child_id == pid )
		{
		if ( (status & 0xff) != 0 )
			cerr << "LocalExec::Active: strange child status for "
			     << executable << "\n";
		}

	else if ( errno != ECHILD )
		{
		cerr << "LocalExec::Active: problem getting child status for ";
		perror( executable );
		}

	has_exited = 1;
	return 0;
	}


void LocalExec::Ping()
	{
	if ( kill( pid, SIGIO ) < 0 )
		{
		cerr << "LocalExec::Ping: problem pinging executable ";
		perror( executable );
		}
	}
