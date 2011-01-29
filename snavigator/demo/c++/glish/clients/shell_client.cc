// $Header$

#include "system.h"

#include <stdio.h>
#include <string.h>
#include <osfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/types.h>

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifndef WEXITSTATUS
#define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif

#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif

#include "Glish/Client.h"

#include "Channel.h"


inline int streq( const char* a, const char* b )
	{
	return ! strcmp( a, b );
	}

char* prog_name;
char* child_name;
int pid;
int ping_through;


// Given an argv, forks argv[0] as a new child, returning a Channel
// to the child and modifying pid to the child's PID.
//
// The global prog_name is used for generating error messages.

Channel* CreateChild( char** argv, int& pid );


// Get the next event via Client c and depending on its name do the following:
//
//	EOF		close the output file
//	terminate	kill the child
//	stdin		send it to the child via write_to_child_fd
//
// Any other event name generates a message to stderr and is ignored.
//
// The globals prog_name and child_name are used for generating error
// messages; pid for sending signals to the child; and, if non-zero,
// ping_through indicates that the child should be sent a SIGIO after
// "stdin" data has been written to it

void SendChildInput( Client& c, int write_to_child_fd );


// Exhaust input present on a child's fd; if read goes okay (not EOF),
// send a "stdout" message via Client c and return 0; otherwise, wait for
// child termination and return non-zero and set status to child exit status.
//
// The globals prog_name and child_name are used for generating error
// messages.

int ReceiveChildOutput( Client& c, int read_from_child_fd, int& status );


// Wait for the child to exit and return its exit status.

int await_child_exit();

// Create a master/slave pty pair.  Returns 0 if failure, non-zero on success.
int get_pty_pair( int pty[2] );

// Handler for when the child exits.  All it does is close the writing
// end of the following pipe, which we can subsequently detect via select().
void child_exit_handler();
int child_exit_pipe[2];


int main( int argc, char** argv )
	{
	Client c( argc, argv );

	prog_name = argv[0];
	++argv, --argc;

	ping_through = 0;
	if ( argc > 0 && streq( argv[0], "-ping" ) )
		{
		ping_through = 1;
		++argv, --argc;
		}

	child_name = argv[0];

	(void) install_signal_handler( SIGCHLD, child_exit_handler );

	if ( pipe( child_exit_pipe ) < 0 )
		{
		fprintf( stderr, "%s (%s): ", prog_name, child_name );
		perror( "child-exit pipe failed" );
		return 1;
		}

	Channel* child_channel = CreateChild( argv, pid );

	if ( ! child_channel )
		return 1;

	int child_fd = child_channel->ReadFD();
	int child_exit_fd = child_exit_pipe[0];

	fd_set selection_mask;
	FD_ZERO( &selection_mask );

	for ( ; ; )
		{
		FD_SET( child_fd, &selection_mask );
		FD_SET( child_exit_fd, &selection_mask );
		c.AddInputMask( &selection_mask );

		if ( select( FD_SETSIZE, &selection_mask, 0, 0, 0 ) < 0 )
			{
			if ( errno != EINTR )
				{
				int preserve_errno = errno;

				fprintf( stderr, "%s (%s): ",
					prog_name, child_name );
				errno = preserve_errno;
				perror(
				    "select() returned for unknown reason" );
				}

			continue;
			}

		if ( c.HasClientInput( &selection_mask ) )
			SendChildInput( c, child_channel->WriteFD() );

		if ( FD_ISSET( child_fd, &selection_mask ) )
			{
			int status;
			if ( ReceiveChildOutput( c, child_fd, status ) )
				return status;
			}

		if ( FD_ISSET( child_exit_fd, &selection_mask ) )
			return await_child_exit();
		}
	}


Channel* CreateChild( char** argv, int& pid )
	{
	int to_pipe[2];

	if ( pipe( to_pipe ) < 0 )
		{
		fprintf( stderr, "%s (%s): ", prog_name, argv[0] );
		perror( "couldn't create pipe" );
		return 0;
		}

	// Try to create a pseudo-terminal for the child's standard
	// output so that text it generates will be line-buffered.
	int from_pipe[2];

	int using_pty = 1;
	if ( ! get_pty_pair( from_pipe ) )
		{
		using_pty = 0;

		// Fall back on using a pipe for the output.
		if ( pipe( from_pipe ) < 0 )
			{
			fprintf( stderr, "%s (%s): ", prog_name, argv[0] );
			perror( "couldn't create pipe" );
			return 0;
			}
		}

	int input_fd = from_pipe[0];
	int output_fd = to_pipe[1];

	pid = vfork();

	if ( pid == 0 )
		{ // child
		if ( dup2( to_pipe[0], fileno(stdin) ) < 0 ||
		     dup2( from_pipe[1], fileno(stdout) ) < 0 )
			{
			fprintf( stderr, "%s (%s): ", prog_name, argv[0] );
			perror( "couldn't do dup2()" );
			_exit( -1 );
			}

		close( input_fd );
		close( output_fd );
		close( to_pipe[0] );
		close( from_pipe[1] );

		execvp( argv[0], &argv[0] );

		fprintf( stderr, "%s (child): couldn't exec ", prog_name );
		perror( argv[0] );
		_exit( -1 );
		}

	close( to_pipe[0] );
	close( from_pipe[1] );

	return new Channel( input_fd, output_fd );
	}


void SendChildInput( Client& c, int send_to_child_fd )
	{
	GlishEvent* e = c.NextEvent();

	if ( ! e || streq( e->name, "terminate" ) )
		kill( pid, SIGTERM );

	else if ( streq( e->name, "EOF" ) )
		close( send_to_child_fd );

	else if ( streq( e->name, "stdin" ) )
		{
		char* input_str = e->value->StringVal();

		char buf[8192];
		sprintf( buf, "%s\n", input_str );

		delete input_str;

		if ( write( send_to_child_fd, buf, strlen( buf ) ) < 0 )
			{
			fprintf( stderr, "%s (%s): ", prog_name, child_name );
			perror( "write to child failed" );
			return;
			}

		if ( ping_through )
			kill( pid, SIGIO );
		}

	else
		c.Unrecognized();
	}


int ReceiveChildOutput( Client& c, int read_from_child_fd, int& status )
	{
	// Exhaust child's output, until we come across a read that ends
	// on a line ('\n') boundary.
	char buf[8192];
	char* buf_ptr = buf;

	*buf_ptr = '\0';

	do
		{
		char* line_end = strchr( buf_ptr, '\n' );

		while ( ! line_end )
			{ // Need to fill buffer.
			int num_to_move = buf_ptr - buf;

			for ( int i = 0; i < num_to_move; ++i )
				buf[i] = buf_ptr[i];

			buf_ptr = buf;

			int buf_size = read( read_from_child_fd, buf,
						sizeof( buf ) - 1 );

			// When reading from the pty after the child has
			// executed we can get EIO or EINVAL.
			if ( buf_size < 0 && errno != EIO && errno != EINVAL )
				{
				fprintf( stderr, "%s (%s): ", prog_name,
						child_name );
				perror( "read from child failed" );
				}

			if ( buf_size <= 0 )
				{
				status = await_child_exit();
				return 1;
				}

			// Mark the end of the buffer.
			buf[buf_size] = '\0';

			line_end = strchr( buf_ptr, '\n' );
			}

		// Nuke trailing newline.
		*line_end = '\0';

		// While we're at it, get rid of the \r that stdio
		// includes due to using a pty.
		if ( line_end > buf && line_end[-1] == '\r' )
			line_end[-1] = '\0';

		c.PostEvent( "stdout", buf_ptr );
		buf_ptr = line_end + 1;
		}
	while ( *buf_ptr != '\0' );

	return 0;
	}


int await_child_exit()
	{ // EOF - presumably the child is about to exit.
	int child_status;
	int child_id;

	do
		{
		child_id = wait_for_pid( 0, &child_status, WNOHANG );
		}
	while ( child_id < 0 && errno == EINTR );

	if ( child_id < 0 )
		{
		fprintf( stderr, "%s: problem waiting for child %s",
			 prog_name, child_name );
		perror( " to terminate" );
		}

	return WEXITSTATUS(child_status);
	}


int get_pty_pair( int pty[2] )
	{
	static char pty_name[sizeof( "/dev/ttyp1" )];

	// First find the master.
	int master_fd = -1;

	for ( char p1 = 0; p1 < ('s' - 'p') && master_fd == -1; p1++ )
		for ( char p2 = 0; p2 < 0x10; p2++ )
			{
			sprintf( pty_name, "/dev/pty%c%x", p1 + 'p', p2 );

			if ( (master_fd = open( pty_name, O_RDWR )) >= 0 )
				{
				// Success.
				sprintf( pty_name,
					"/dev/tty%c%x", p1 + 'p', p2 );
				break;
				}
			}

	if ( master_fd < 0 )
		return 0;

	int slave_fd = open( pty_name, O_RDWR );

	if ( slave_fd < 0 )
		{
		close( master_fd );
		return 0;
		}

	pty[0] = master_fd;
	pty[1] = slave_fd;

	return 1;
	}


void child_exit_handler()
	{
	close( child_exit_pipe[1] );
	}
