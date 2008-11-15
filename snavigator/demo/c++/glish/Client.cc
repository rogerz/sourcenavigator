// $Header$

#include "system.h"

#include <stdlib.h>
#include <osfcn.h>
#include <fcntl.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <signal.h>

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

extern "C" {
#include <netinet/in.h>
#ifndef HAVE_STRDUP
char* strdup( const char* );
#endif
}

#include "Sds/sdsgen.h"
#include "Glish/Client.h"

#include "BuiltIn.h"
#include "Reporter.h"
#include "Socket.h"
#include "glish_event.h"
#include "system.h"


typedef RETSIGTYPE (*SigHandler)(int);

static Client* current_client;
static const char* prog_name = "glish-interpreter";

static int did_init = 0;


inline streq( const char* s1, const char* s2 )
	{
	return ! strcmp( s1, s2 );
	}


void Client_signal_handler()
	{
	current_client->HandlePing();
	}


GlishEvent::GlishEvent( const char* arg_name, const Value* arg_value )
	{
	name = arg_name;
	value = (Value*) arg_value;
	flags = 0;
	delete_name = delete_value = 0;
	}

GlishEvent::GlishEvent( const char* arg_name, Value* arg_value )
	{
	name = arg_name;
	value = arg_value;
	flags = 0;
	delete_name = 0;
	delete_value = 1;
	}

GlishEvent::GlishEvent( char* arg_name, Value* arg_value )
	{
	name = arg_name;
	value = arg_value;
	flags = 0;
	delete_name = delete_value = 1;
	}

GlishEvent::~GlishEvent()
	{
	if ( delete_name )
		delete (char*) name;

	if ( delete_value )
		Unref( (Value*) value );
	}


int GlishEvent::IsRequest() const
	{
	return flags & GLISH_REQUEST_EVENT;
	}

int GlishEvent::IsReply() const
	{
	return flags & GLISH_REPLY_EVENT;
	}

void GlishEvent::SetIsRequest()
	{
	flags |= GLISH_REQUEST_EVENT;
	}

void GlishEvent::SetIsReply()
	{
	flags |= GLISH_REPLY_EVENT;
	}


class EventLink {
public:
	EventLink( char* task_id, char* new_name, int initial_activity )
		{
		id = task_id;
		name = new_name;
		fd = -1;
		active = initial_activity;
		path = 0;
		}

	~EventLink()
		{
		delete id;
		delete name;
		delete path;
		}

	const char* ID() const		{ return id; }
	int FD() const			{ return fd; }
	int Active() const		{ return active; }
	const char* Name() const	{ return name; }
	const char* Path() const	{ return path; }

	void SetActive( int activity )	{ active = activity; }
	void SetFD( int new_fd )	{ fd = new_fd; }
	void SetPath( const char* p )	{ path = strdup( p ); }

private:
	char* id;
	char* name;
	int fd;
	int active;
	char* path;	// if nil, indicates this is a non-local socket
	};


Client::Client( int& argc, char** argv )
	{
	char** orig_argv = argv;

	prog_name = argv[0];
	--argc, ++argv;	// remove program name from argument list

	ClientInit();

	int suspend = 0;

	no_glish = 0;
	interpreter_tag = "*no-interpreter*";

	if ( argc < 2 || ! streq( argv[0], "-id" ) )
		{
		// We weren't invoked by the Glish interpreter.  Set things up
		// so that we receive "events" from stdin and send them
		// to stdout.
		have_interpreter_connection = 0;
		client_name = prog_name;

		if ( argc == 0 || (argc >= 1 && !streq( argv[0], "-glish" )) )
			{
			no_glish = 1;
			}

		if ( argc >= 1 && streq( argv[0], "-noglish" ) )
			{
			no_glish = 1;
			--argc, ++argv;
			}

		else
			{
			read_fd = fileno( stdin );
			write_fd = fileno( stdout );

			if ( argc >= 1 && streq( argv[0], "-glish" ) )
				// Eat up -glish argument.
				--argc, ++argv;
			}
		}

	else
		{
		have_interpreter_connection = 1;
		client_name = argv[1];
		argc -= 2, argv += 2;

		if ( argc < 2 ||
		     (! streq( argv[0], "-host" ) &&
		      ! streq( argv[0], "-pipes" )) )
			fatal->Report(
		"invalid Client argument list - missing connection arguments" );

		if ( streq( argv[0], "-host" ) )
			{ // Socket connection.
			char* host = argv[1];
			argc -= 2, argv += 2;

			if ( argc < 2 || ! streq( argv[0], "-port" ) )
				fatal->Report(
			"invalid Client argument list - missing port number" );

			int port = atoi( argv[1] );
			argc -= 2, argv += 2;

			int socket = get_tcp_socket();

			if ( ! remote_connection( socket, host, port ) )
				fatal->Report(
		"could not establish Client connection to interpreter" );

			read_fd = write_fd = socket;
			}

		else
			{ // Pipe connection.
			++argv, --argc;	// skip -pipes argument

			if ( argc < 2 )
				fatal->Report(
			"invalid Client argument list - missing pipe fds" );

			read_fd = atoi( argv[0] );
			write_fd = atoi( argv[1] );

			argc -= 2, argv += 2;
			}

		if ( argc > 1 && streq( argv[0], "-interpreter" ) )
			{
			interpreter_tag = argv[1];
			argc -= 2, argv += 2;
			}

		if ( argc > 0 && streq( argv[0], "-suspend" ) )
			{
			suspend = 1;
			--argc, ++argv;
			}
		}

	if ( suspend )
		{
		int pid = int( getpid() );

		fprintf( stderr, "%s @ %s, pid %d: suspending ...\n",
			prog_name, local_host, pid );

		while ( suspend )
			sleep( 1 );	// allow debugger to attach
		}

	if ( have_interpreter_connection )
		SendEstablishedEvent();

	CreateSignalHandler();

	for ( int i = 1; i <= argc; ++i )
		orig_argv[i] = *(argv++);

	orig_argv[i] = 0;
	argv = orig_argv;

	// Put argv[0] back into the argument count - it was removed
	// near the top of this routine.
	++argc;
	}


Client::Client( int client_read_fd, int client_write_fd, const char* name )
	{
	client_name = prog_name = name;

	ClientInit();

	no_glish = 0;
	have_interpreter_connection = 1;

	read_fd = client_read_fd;
	write_fd = client_write_fd;

	SendEstablishedEvent();

	CreateSignalHandler();
	}

Client::~Client()
	{
	(void) install_signal_handler( SIGIO, former_handler );

	if ( have_interpreter_connection )
		PostEvent( "done", client_name );

	if ( ! no_glish )
		{
		close( read_fd );
		close( write_fd );
		}
	}

GlishEvent* Client::NextEvent()
	{
	if ( no_glish )
		return 0;

	if ( input_links.length() == 0 )
		// Only one input channel, okay to block reading it.
		return GetEvent( read_fd );

	fd_set input_fds;

	FD_ZERO( &input_fds );
	AddInputMask( &input_fds );

	while ( select( FD_SETSIZE, &input_fds, 0, 0, 0 ) < 0 )
		{
		if ( errno != EINTR )
			{
			fprintf( stderr, "%s: ", prog_name );
			perror( "error during select()" );
			return 0;
			}
		}

	return NextEvent( &input_fds );
	}

GlishEvent* Client::NextEvent( fd_set* mask )
	{
	if ( no_glish )
		return 0;

	if ( FD_ISSET( read_fd, mask ) )
		return GetEvent( read_fd );

	loop_over_list( input_links, i )
		{
		int fd = input_links[i];

		if ( FD_ISSET( fd, mask ) )
			return GetEvent( fd );
		}

	PostEvent( "error", "bad call to Client::NextEvent" );

	return 0;
	}

void Client::Unrecognized()
	{
	if ( ! last_event )
		return;

	if ( last_event->name[0] == '*' )
		// Internal event - ignore.
		return;

	PostEvent( "unrecognized", last_event->name );
	}

void Client::Error( const char* msg )
	{
	if ( last_event )
		PostEvent( "error", "bad \"%s\" event: %s",
				last_event->name, msg );
	else
		PostEvent( "error", msg );
	}

void Client::Error( const char* fmt, const char* arg )
	{
	char buf[8192];
	sprintf( buf, fmt, arg );
	Error( buf );
	}

void Client::PostEvent( const char* event_name, const Value* event_value )
	{
	GlishEvent e( event_name, event_value );
	SendEvent( &e );
	}

void Client::PostEvent( const GlishEvent* event )
	{
	SendEvent( event );
	}

void Client::PostEvent( const char* event_name, const char* event_value )
	{
	Value val( event_value );
	PostEvent( event_name, &val );
	}

void Client::PostEvent( const char* event_name, const char* event_fmt,
    const char* event_arg )
	{
	char buf[8192];
	sprintf( buf, event_fmt, event_arg );
	PostEvent( event_name, buf );
	}

void Client::PostEvent( const char* event_name, const char* event_fmt,
    const char* arg1, const char* arg2 )
	{
	char buf[8192];
	sprintf( buf, event_fmt, arg1, arg2 );
	PostEvent( event_name, buf );
	}

void Client::Reply( const Value* event_value )
	{
	if ( ! ReplyPending() )
		error->Report( prog_name,
	" issued Reply without having received a corresponding request" );

	else
		{
		GlishEvent e( (const char*) pending_reply, event_value );
		e.SetIsReply();
		PostEvent( &e );
		delete pending_reply;
		pending_reply = 0;
		}
	}

void Client::PostOpaqueSDS_Event( const char* event_name, int sds )
	{
	GlishEvent e( event_name, (const Value*) 0 );
	SendEvent( &e, sds );
	}

int Client::AddInputMask( fd_set* mask )
	{
	if ( no_glish )
		return 0;

	int num_added = 0;
	if ( ! FD_ISSET( read_fd, mask ) )
		{
		FD_SET( read_fd, mask );
		++num_added;
		}

	// Now add in any fd's due to event links.
	loop_over_list( input_links, i )
		if ( ! FD_ISSET( input_links[i], mask ) )
			{
			FD_SET( input_links[i], mask );
			++num_added;
			}

	return num_added;
	}

int Client::HasClientInput( fd_set* mask )
	{
	if ( no_glish )
		return 0;

	if ( FD_ISSET( read_fd, mask ) )
		return 1;

	loop_over_list( input_links, i )
		{
		if ( FD_ISSET( input_links[i], mask ) )
			return 1;
		}

	return 0;
	}


void Client::ClientInit()
	{
	if ( ! did_init )
		{
		sds_init();
		init_reporters();
		init_values();

		did_init = 1;
		}

	last_event = 0;
	pending_reply = 0;

	local_host = local_host_name();
	}

void Client::CreateSignalHandler()
	{
	current_client = this;
	former_handler = install_signal_handler( SIGIO, Client_signal_handler );
	}

void Client::SendEstablishedEvent()
	{
	Value* r = create_record();

	r->SetField( "name", client_name );
	r->SetField( "protocol", GLISH_CLIENT_PROTO_VERSION );

	PostEvent( "established", r );

	Unref( r );
	}

GlishEvent* Client::GetEvent( int fd )
	{
	Unref( last_event );

	if ( have_interpreter_connection )
		{
		last_event = recv_event( fd );

		if ( ! last_event && fd != read_fd )
			{
			// A link has gone away; but don't return a nil
			// event, that'll cause us to go away too.  Instead,
			// remove this fd from the list of input links and
			// cobble together a dummy event.

			input_links.remove( fd );
			FD_Change( fd, 0 );

			last_event = new GlishEvent( (const char*) "*dummy*",
							error_value() );
			}

		if ( last_event )
			{
			Value* v = last_event->value;

			if ( v->FieldVal( "*reply*", pending_reply ) )
				{ // Request/reply event; unwrap the request.
				Value* request = v->Field( "*request*" );

				if ( ! request )
					fatal->Report(
						"bad request/reply event" );

				Ref( request );
				Unref( v );
				last_event->value = request;
				}
			}
		}

	else
		{
		char buf[512];
		if ( ! fgets( buf, sizeof( buf ), stdin ) )
			return 0;

		// Nuke final newline, if present.
		char* delim = strchr( buf, '\n' );

		if ( delim )
			*delim = '\0';

		// Find separation between event name and value.
		delim = strchr( buf, ' ' );
		Value* result;

		if ( ! delim )
			result = error_value();

		else
			{
			*delim++ = '\0';
			result = new Value( delim );
			}

		last_event = new GlishEvent( strdup( buf ), result );
		}

	if ( last_event )
		{
		const char* name = last_event->name;

		if ( streq( name, "terminate" ) )
			{
			Unref(last_event);
			last_event = 0;
			}

		else if ( name[0] == '*' )
			{
			Value* v = last_event->value;

			if ( streq( name, "*sync*" ) )
				Reply( false_value );

			else if ( streq( name, "*link-sink*" ) )
				BuildLink( v );

			else if ( streq( name, "*rendezvous-orig*" ) )
				RendezvousAsOriginator( v );

			else if ( streq( name, "*rendezvous-resp*" ) )
				RendezvousAsResponder( v );

			else if ( streq( name, "*unlink-sink*" ) )
				UnlinkSink( v );
			}
		}

	return last_event;
	}

void Client::FD_Change( int /* fd */, int /* add_flag */ )
	{
	}

void Client::HandlePing()
	{
	}

void Client::UnlinkSink( Value* v )
	{
	int is_new;
	EventLink* el = AddOutputLink( v, 1, is_new );

	if ( ! el )
		return;

	if ( is_new )
		PostEvent( "error", "no link to %s", el->ID() );

	el->SetActive( 0 );
	}

void Client::BuildLink( Value* v )
	{
	int is_new;
	EventLink* el = AddOutputLink( v, 0, is_new );

	if ( ! el )
		return;

	if ( ! is_new )
		{ // No need to forward, we've already built the link.
		el->SetActive( 1 );
		return;
		}

	char* sink_id;
	if ( ! v->FieldVal( "sink_id", sink_id ) )
		fatal->Report( "bad internal link event" );

	int fd = remote_sinks[sink_id];
	delete sink_id;

	// Minor-league bug here.  fd=0 is legal, though it means that
	// somehow stdin got closed.  But even in that case, we'll just
	// miss an opportunity to reuse the connection, and instead create
	// a new, separate connection.
	if ( fd )
		{ // Already have a connection, piggyback on it.
		el->SetFD( fd );
		el->SetActive( 1 );
		return;
		}

	// We have to create a new AcceptSocket each time, otherwise
	// there's a race in which we send out rendezvous events to
	// B and then to C, but C gets the event first and connects
	// to the accept socket in lieu of B.
	AcceptSocket* accept_socket;

	int is_local;
	if ( ! v->FieldVal( "is_local", is_local ) )
		fatal->Report( "internal link event lacks is_local field" );

	if ( is_local )
		{
		accept_socket = new AcceptSocket( 1 );

		char* path = local_socket_name( accept_socket->FD() );
		el->SetPath( path );
		v->SetField( "path", path );
		}

	else
		{ // Use an ordinary socket.
		accept_socket = new AcceptSocket();

		// Include information in the event about how to reach us.
		v->SetField( "host", local_host );
		v->SetField( "port", accept_socket->Port() );
		}

	// So we can recover this socket when the interpreter reflects
	// this rendezvous event reflected back to us.
	v->SetField( "accept_fd", accept_socket->FD() );

	PostEvent( "*rendezvous*", v );
	}

EventLink* Client::AddOutputLink( Value* v, int want_active, int& is_new )
	{
	char* event_name;
	char* new_name;
	char* task_id;

	if ( ! v->FieldVal( "event", event_name ) ||
	     ! v->FieldVal( "new_name", new_name ) ||
	     ! v->FieldVal( "sink_id", task_id ) )
		fatal->Report( "bad internal link event" );

	event_link_list* l = output_links[event_name];

	if ( ! l )
		{ // Event link doesn't exist yet.
		l = new event_link_list;
		output_links.Insert( event_name, l );
		}

	else
		{ // Look for whether this event link already exists.
		delete event_name;

		loop_over_list( *l, i )
			{
			EventLink* el = (*l)[i];

			if ( el->Active() == want_active &&
			     streq( el->Name(), new_name ) &&
			     streq( el->ID(), task_id ) )
				{ // This one fits the bill.
				delete new_name;
				delete task_id;

				is_new = 0;

				return el;
				}
			}
		}

	EventLink* result = new EventLink( task_id, new_name, want_active );
	l->append( result );

	is_new = 1;

	return result;
	}

void Client::RendezvousAsOriginator( Value* v )
	{
	int is_new;
	EventLink* el = AddOutputLink( v, 0, is_new );

	if ( ! el || is_new )
		fatal->Report( "bad internal link event" );

	int output_fd;

	int accept_fd;
	if ( ! v->FieldVal( "accept_fd", accept_fd ) )
		fatal->Report( "bad internal link event" );

	char* sink_id;
	if ( ! v->FieldVal( "sink_id", sink_id ) )
		fatal->Report( "bad internal link event" );

	int sink_fd = remote_sinks[sink_id];
	if ( sink_fd )
		{
		// Already have a socket connection to this remote sink.
		// Even though BuildLink endeavors to avoid sending out
		// a *rendezvous* if such a connection already exists,
		// it's possible for a second *link-sink* to come in before
		// we've finished processing the first, in which case the
		// second call to BuildLink won't see that we already have
		// a socket connection.  So we check here, too.
		output_fd = sink_fd;
		delete sink_id;

		// If this was a local socket, get rid of the corresponding
		// file.
		char* path;
		if ( v->FieldVal( "path", path ) )
			{
			if ( unlink( path ) < 0 )
				PostEvent( "error",
					"can't unlink local socket sink %s",
						path );

			delete path;
			}
		}

	else
		{ // Create a new socket connection.
		int is_local;
		if ( ! v->FieldVal( "is_local", is_local ) )
			fatal->Report( "bad internal link event" );

		output_fd = is_local ?
			accept_local_connection( accept_fd ) :
			accept_connection( accept_fd );

		if ( output_fd < 0 )
			fatal->Report( "socket accept for link failed" );

		remote_sinks.Insert( sink_id, output_fd );
		}

	close( accept_fd );

	el->SetFD( output_fd );
	el->SetActive( 1 );
	}

void Client::RendezvousAsResponder( Value* v )
	{
	char* source_id;
	if ( ! v->FieldVal( "source_id", source_id ) )
		fatal->Report( "bad internal link event" );

	if ( remote_sources[source_id] )
		{
		// We're all set, we already select on this input source.
		delete source_id;
		return;
		}

	int input_fd;

	int is_local;
	if ( ! v->FieldVal( "is_local", is_local ) )
		fatal->Report( "bad internal link event" );

	if ( is_local )
		{
		input_fd = get_local_socket();

		char* path;
		if ( ! v->FieldVal( "path", path ) )
			fatal->Report( "bad internal link event" );

		if ( ! local_connection( input_fd, path ) )
			fatal->Report( "couldn't connect to local socket",
					path );

		// Now that we've rendezvous'd, remove the Unix-domain
		// socket from the file system namespace.

		if ( unlink( path ) < 0 )
			PostEvent( "error", "can't unlink local socket sink %s",
					path );

		delete path;
		}

	else
		{
		char* host;
		int port;

		if ( ! v->FieldVal( "host", host ) ||
		     ! v->FieldVal( "port", port ) )
			fatal->Report( "bad internal link event" );

		input_fd = get_tcp_socket();

		if ( ! remote_connection( input_fd, host, port ) )
			{
			PostEvent( "error", "can't open socket sink %s", host );
			return;
			}

		delete host;
		}

	input_links.append( input_fd );
	FD_Change( input_fd, 1 );

	remote_sources.Insert( source_id, input_fd );
	}

void Client::SendEvent( const GlishEvent* e, int sds )
	{
	if ( no_glish )
		return;

	const char* name = e->name;
	const Value* value = e->value;

	if ( have_interpreter_connection )
		{
		event_link_list* l = output_links[name];

		if ( l )
			{
			int did_send = 0;

			loop_over_list( *l, i )
				{
				EventLink* el = (*l)[i];

				if ( el->Active() )
					{
					send_event( el->FD(), el->Name(),
							e, sds );
					did_send = 1;
					}
				}

			// If we didn't send any events then it means that
			// all of the links are inactive.  Forward to
			// the interpreter instead.
			if ( ! did_send )
				send_event( write_fd, e->name, e, sds );
			}
		else
			send_event( write_fd, e->name, e, sds );
		}

	else if ( value )
		message->Report( name, " ", value );

	else
		message->Report( name, " <no value>" );
	}

static int read_buffer( int fd, char* buffer, int buf_size )
	{
	int status = read( fd, buffer, buf_size );

	if ( status == 0 )
		return 0;

	else if ( status < 0 )
		{
		if ( errno != EPIPE && errno != ECONNRESET )
			error->Report( prog_name,
					": problem reading buffer, errno = ",
					errno );
		return 0;
		}

	else if ( status < buf_size )
		return read_buffer( fd, buffer + status, buf_size - status );

	return 1;
	}

static int write_buffer( int fd, char* buffer, int buf_size )
	{
	int status = write( fd, buffer, buf_size );

	if ( status < 0 )
		{
		if ( errno != EPIPE && errno != ECONNRESET )
			error->Report( prog_name,
					": problem writing buffer, errno = ",
					errno );
		return 0;
		}

	else if ( status < buf_size )
		return write_buffer( fd, buffer + status, buf_size - status );

	return 1;
	}


GlishEvent* recv_event( int fd )
	{
	struct event_header hdr;

	if ( ! read_buffer( fd, (char*) &hdr, sizeof( hdr ) ) )
		return 0;

	hdr.flags = ntohl( hdr.flags );
	int size = (int) ntohl( hdr.event_length );

	Value* result;

	if ( hdr.flags & GLISH_STRING_EVENT )
		{
		char* value = new char[size+1];
		if ( size > 0 && ! read_buffer( fd, value, size ) )
			{
			error->Report( prog_name,
		": read only partial string event value from socket, errno = ",
					errno );
			delete value;
			return 0;
			}

		else
			{
			value[size] = '\0';
			result = split( value );
			}

		delete value;
		}

	else
		{
		int sds = (int) sds_read_open_fd( fd, size );

		if ( sds <= 0 )
			{
			if ( sds != SDS_FILE_WR )
				error->Report( prog_name,
				": problem reading event, SDS error code = ",
						sds );
			return 0;
			}

		else
			result = read_value_from_SDS( sds,
					int( hdr.flags & GLISH_OPAQUE_EVENT ) );
		}

	GlishEvent* e = new GlishEvent( strdup( hdr.event_name ), result );
	e->SetFlags( int( hdr.flags ) );
	return e;
	}


static int send_event_header( int fd, int flags, int length, const char* name )
	{
	// The following is static so that we don't wind up copying
	// uninitialized memory; makes Purify happy.
	static struct event_header hdr;

	hdr.flags = htonl( (u_long) flags );
	hdr.event_length = htonl( (u_long) length );

	if ( strlen( name ) >= MAX_EVENT_NAME_SIZE )
		warn->Report( prog_name,
				": event name \"", name, "\" truncated to ",
				MAX_EVENT_NAME_SIZE - 1, " characters" );

	strncpy( hdr.event_name, name, MAX_EVENT_NAME_SIZE - 1 );
	hdr.event_name[MAX_EVENT_NAME_SIZE - 1] = '\0';

	if ( ! write_buffer( fd, (char*) &hdr, sizeof( hdr ) ) )
		return 0;

	return 1;
	}

void send_event( int fd, const char* name, const GlishEvent* e, int sds )
	{
	const Value* value = e->value;
	int event_flags = e->Flags();

	if ( value )
		value = value->Deref();

	if ( value && value->Type() == TYPE_STRING && value->Length() == 1 &&
	     ! value->AttributePtr() )
		{
		const_args_list a;
		a.append( value );
		char* string_val = paste( &a );

		int size = strlen( string_val );
		event_flags |= GLISH_STRING_EVENT;

		if ( send_event_header( fd, event_flags, size, name ) )
			(void) write_buffer( fd, string_val, size );

		delete string_val;
		}

	else
		{
		if ( value && value->Type() == TYPE_OPAQUE )
			{
			sds = value->SDS_IndexVal();
			value = 0;
			}

		del_list d;

		if ( sds >= 0 )
			event_flags |= GLISH_OPAQUE_EVENT;

		else
			{
			if ( ! value )
				value = new Value( glish_false );

			sds = (int) sds_new( (char*) "" );

			if ( sds < 0 )
				error->Report( prog_name,
				": problem posting event, SDS error code = ",
						sds );

			value->AddToSds( sds, &d );
			}

		int size = (int) sds_fullsize( sds );

		if ( send_event_header( fd, event_flags, size, name ) )
			{
			if ( sds_write2fd( fd, sds ) != sds &&
			     sds_error != SDS_FILE_WR )
				error->Report( prog_name,
			": problem sending event on socket, SDS error code = ",
						(int) sds_error );
			}

		if ( value )
			{
			sds_destroy( sds );
			sds_discard( sds );

			delete_list( &d );
			}
		}
	}
