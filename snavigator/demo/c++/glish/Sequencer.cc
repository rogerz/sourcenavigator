// $Header$

#include "system.h"

#include <stdlib.h>
#include <string.h>
#include <stream.h>
#include <osfcn.h>
#include <sys/param.h>
#include <sys/types.h>

#ifdef HAVE_SIGPROCMASK
#include <signal.h>
#endif

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif

extern "C" {
char* getenv( const char* );
int isatty( int fd );
int system( const char* string );
}


#include "Reporter.h"
#include "Sequencer.h"
#include "Frame.h"
#include "BuiltIn.h"
#include "Task.h"
#include "input.h"
#include "Channel.h"
#include "Select.h"
#include "Socket.h"
#include "ports.h"
#include "version.h"

#define GLISH_RC_FILE ".glishrc"

// Time to wait until probing a remote daemon, in seconds.
#define PROBE_DELAY 5

// Interval between subsequent probes, in seconds.
#define PROBE_INTERVAL 5


// A Selectee corresponding to input for a Glish client.
class ClientSelectee : public Selectee {
    public:
	ClientSelectee( Sequencer* s, Task* t );
	int NotifyOfSelection();

    protected:
	Sequencer* sequencer;
	Task* task;
	};


// A Selectee used only to intercept the initial "established" event
// generated by a local Glish client.  Because we communicate with these
// clients using pipes instead of sockets, we can't use the AcceptSelectee
// (see below) to recognize when they're connecting.
class LocalClientSelectee : public Selectee {
    public:
	LocalClientSelectee( Sequencer* s, Channel* c );
	int NotifyOfSelection();

    protected:
	Sequencer* sequencer;
	Channel* chan;
	};


// A Selectee corresponding to a new request of a client to connect
// to the sequencer.
class AcceptSelectee : public Selectee {
    public:
	AcceptSelectee( Sequencer* s, Socket* conn_socket );
	int NotifyOfSelection();

    protected:
	Sequencer* sequencer;
	Socket* connection_socket;
	};


// A Selectee corresponding to a Glish script's own client.
class ScriptSelectee : public Selectee {
public:
	ScriptSelectee( Client* client, Agent* agent, int conn_socket );
	int NotifyOfSelection();

protected:
	Client* script_client;
	Agent* script_agent;
	int connection_socket;
	};


// A Selectee for detecting user input.
class UserInputSelectee : public Selectee {
public:
	UserInputSelectee( int user_fd ) : Selectee( user_fd )	{ }

	// Indicate user input available by signalling to end the
	// select.
	int NotifyOfSelection()	{ return 1; }
	};


// A Selectee for detecting Glish daemon activity.
class DaemonSelectee : public Selectee {
public:
	DaemonSelectee( RemoteDaemon* daemon, Selector* sel, Sequencer* s );

	int NotifyOfSelection();

protected:
	RemoteDaemon* daemon;
	Selector* selector;
	Sequencer* sequencer;
	};


// A SelectTimer for handling glishd probes.
class ProbeTimer : public SelectTimer {
public:
	ProbeTimer( PDict(RemoteDaemon)* daemons, Sequencer* s );

protected:
	int DoExpiration();

	PDict(RemoteDaemon)* daemons;
	Sequencer* sequencer;
	};


// A special type of Client used for script clients.  It overrides
// FD_Change() to create or delete ScriptSelectee's as needed.
class ScriptClient : public Client {
public:
	ScriptClient( int& argc, char** argv );

	// Inform the ScriptClient as to which selector and agent
	// it should use for getting and propagating events.
	void SetInterface( Selector* selector, Agent* agent );

protected:
	void FD_Change( int fd, int add_flag );

	Selector* selector;
	Agent* agent;
	};


// A special type of Agent used for script clients; when it receives
// an event, it propagates it via the ScriptClient object.
class ScriptAgent : public Agent {
public:
	ScriptAgent( Sequencer* s, Client* c ) : Agent(s)	{ client = c; }

	Value* SendEvent( const char* event_name, parameter_list* args,
			int /* is_request */, int /* log */ )
		{
		Value* event_val = BuildEventValue( args, 1 );
		client->PostEvent( event_name, event_val );
		Unref( event_val );
		return 0;
		}

protected:
	Client* client;
	};


// A RemoteDaemon keeps track of a glishd running on a remote host.
// This includes the Channel used to communicate with the daemon and
// modifiable state indicating whether we're currently waiting for
// a probe response from the daemon.

// Possible states a daemon can be in.
typedef enum
	{
	DAEMON_OK,	// all is okay
	DAEMON_REPLY_PENDING,	// we're waiting for reply to last probe
	DAEMON_LOST	// we've lost connectivity
	} daemon_states;

class RemoteDaemon {
public:
	RemoteDaemon( const char* daemon_host, Channel* channel )
		{
		host = daemon_host;
		chan = channel;
		SetState( DAEMON_OK );
		}

	const char* Host()		{ return host; }
	Channel* DaemonChannel()	{ return chan; }
	daemon_states State()		{ return state; }
	void SetState( daemon_states s )	{ state = s; }

protected:
	const char* host;
	Channel* chan;
	daemon_states state;
	};


Notification::Notification( Agent* arg_notifier, const char* arg_field,
			    Value* arg_value, Notifiee* arg_notifiee )
	{
	notifier = arg_notifier;
	field = strdup( arg_field );
	value = arg_value;
	notifiee = arg_notifiee;

	Ref( value );
	}

Notification::~Notification()
	{
	delete field;
	Unref( value );
	}

void Notification::Describe( ostream& s ) const
	{
	s << "notification of ";
	notifier->DescribeSelf( s );
	s << "." << field << " (";
	value->DescribeSelf( s );
	s << ") for ";
	notifiee->stmt->DescribeSelf( s );
	}


Sequencer::Sequencer( int& argc, char**& argv )
	{
	init_reporters();
	init_values();

	// Create the global scope.
	scopes.append( new expr_dict );

	create_built_ins( this );

	null_stmt = new NullStmt;

	stmts = null_stmt;
	last_task_id = my_id = 1;

	await_stmt = except_stmt = 0;
	await_only_flag = 0;
	pending_task = 0;

	maximize_num_fds();

	// avoid SIGPIPE's - they can occur if a client's termination
	// is not detected prior to sending an event to the client
#ifdef HAVE_SIGPROCMASK
	sigset_t sig_mask;
	sigemptyset( &sig_mask );
	sigaddset( &sig_mask, SIGPIPE );
	sigprocmask( SIG_BLOCK, &sig_mask, 0 );
#else
	sigblock( sigmask( SIGPIPE ) );
#endif

	connection_socket = new AcceptSocket( 0, INTERPRETER_DEFAULT_PORT );
	mark_close_on_exec( connection_socket->FD() );

	selector = new Selector;
	selector->AddSelectee( new AcceptSelectee( this, connection_socket ) );
	selector->AddTimer( new ProbeTimer( &daemons, this ) );

	connection_host = local_host_name();
	connection_port = new char[32];
	sprintf( connection_port, "%d", connection_socket->Port() );

	static const char tag_fmt[] = "*tag-%s.%d*";
	int n = strlen( tag_fmt ) + strlen( connection_host ) + /* slop */ 32;

	interpreter_tag = new char[n];
	sprintf( interpreter_tag, tag_fmt, connection_host, int( getpid() ) );

	monitor_task = 0;
	last_notification = 0;
	last_whenever_executed = 0;

	num_active_processes = 0;
	verbose = 0;


	// Create the "system" global variable.
	system_agent = new UserAgent( this );
	Value* system_val = system_agent->AgentRecord();

	Expr* system_expr = InstallID( strdup( "system" ), GLOBAL_SCOPE );
	system_expr->Assign( system_val );

	system_val->SetField( "version", new Value( GLISH_VERSION ) );


	// Create "script" global.
	script_client = new ScriptClient( argc, argv );

	Expr* script_expr = InstallID( strdup( "script" ), GLOBAL_SCOPE );

	if ( script_client->HasSequencerConnection() )
		{
		// Set up script agent to deal with incoming and outgoing
		// events.
		ScriptAgent* script_agent =
			new ScriptAgent( this, script_client );
		script_client->SetInterface( selector, script_agent );
		script_expr->Assign( script_agent->AgentRecord() );

		system_val->SetField( "is_script_client",
					new Value( glish_true ) );

		// Include ourselves as an active process; otherwise
		// we'll exit once our child processes are gone.
		++num_active_processes;
		}

	else
		{
		script_expr->Assign( new Value( glish_false ) );
		system_val->SetField( "is_script_client",
					new Value( glish_false ) );
		}

	name = argv[0];

	for ( ++argv, --argc; argc > 0; ++argv, --argc )
		{
		if ( ! strcmp( argv[0], "-v" ) )
			++verbose;

		else if ( strchr( argv[0], '=' ) )
			putenv( argv[0] );

		else
			break;
		}

	MakeEnvGlobal();
	BuildSuspendList();

	char* monitor_client_name = getenv( "glish_monitor" );
	if ( monitor_client_name )
		ActivateMonitor( monitor_client_name );

	Parse( glish_init );

	const char* glish_rc;
	if ( (glish_rc = getenv( "GLISHRC" )) )
		Parse( glish_rc );

	else
		{
		FILE* glish_rc_file = fopen( GLISH_RC_FILE, "r" );
		const char* home;

		if ( glish_rc_file )
			{
			fclose( glish_rc_file );
			Parse( GLISH_RC_FILE );
			}

		else if ( (home = getenv( "HOME" )) )
			{
			char glish_rc_filename[256];
			sprintf( glish_rc_filename, "%s/%s",
					home, GLISH_RC_FILE );

			if ( (glish_rc_file = fopen( glish_rc_filename, "r")) )
				{
				fclose( glish_rc_file );
				Parse( glish_rc_filename );
				}
			}
		}

	int do_interactive = 1;

	if ( argc > 0 && strcmp( argv[0], "--" ) )
		{ // We have a file to parse.
		Parse( argv[0] );
		do_interactive = 0;
		++argv, --argc;
		}

	MakeArgvGlobal( argv, argc );

	if ( do_interactive )
		Parse( stdin );
	}


Sequencer::~Sequencer()
	{
	delete script_client;
	delete selector;
	delete connection_socket;
	delete connection_port;
	delete interpreter_tag;
	}


void Sequencer::AddBuiltIn( BuiltIn* built_in )
	{
	Expr* id = InstallID( strdup( built_in->Name() ), GLOBAL_SCOPE );
	id->Assign( new Value( built_in ) );
	}


void Sequencer::QueueNotification( Notification* n )
	{
	if ( verbose > 1 )
		message->Report( "queueing", n );

	notification_queue.EnQueue( n );
	}


void Sequencer::PushScope()
	{
	scopes.append( new expr_dict );
	}

int Sequencer::PopScope()
	{
	int top_scope_pos = scopes.length() - 1;

	if ( top_scope_pos < 0 )
		fatal->Report( "scope underflow in Sequencer::PopScope" );

	expr_dict* top_scope = scopes[top_scope_pos];
	int frame_size = top_scope->Length();

	scopes.remove( top_scope );
	delete top_scope;

	return frame_size;
	}


Expr* Sequencer::InstallID( char* id, scope_type scope )
	{
	int scope_index;

	if ( scope == LOCAL_SCOPE )
		scope_index = scopes.length() - 1;
	else
		scope_index = 0;

	int frame_offset = scopes[scope_index]->Length();
	Expr* result = new VarExpr( id, scope, frame_offset, this );

	scopes[scope_index]->Insert( id, result );

	if ( scope == GLOBAL_SCOPE )
		global_frame.append( 0 );

	return result;
	}

Expr* Sequencer::LookupID( char* id, scope_type scope, int do_install )
	{
	int scope_index;

	if ( scope == LOCAL_SCOPE )
		scope_index = scopes.length() - 1;
	else
		scope_index = 0;

	Expr* result = (*scopes[scope_index])[id];

	if ( ! result && do_install )
		{
		if ( scope == LOCAL_SCOPE )
			return LookupID( id, GLOBAL_SCOPE );
		else
			return InstallID( id, GLOBAL_SCOPE );
		}

	delete id;
	return result;
	}


void Sequencer::PushFrame( Frame* new_frame )
	{
	local_frames.append( new_frame );
	}

Frame* Sequencer::PopFrame()
	{
	int top_frame = local_frames.length() - 1;
	if ( top_frame < 0 )
		fatal->Report(
			"local frame stack underflow in Sequencer::PopFrame" );

	return local_frames.remove_nth( top_frame );
	}

Frame* Sequencer::CurrentFrame()
	{
	int top_frame = local_frames.length() - 1;
	if ( top_frame < 0 )
		return 0;

	return local_frames[top_frame];
	}


Value* Sequencer::FrameElement( scope_type scope, int frame_offset )
	{
	if ( scope == LOCAL_SCOPE )
		{
		int top_frame = local_frames.length() - 1;
		if ( top_frame < 0 )
			fatal->Report(
	    "local frame requested but none exist in Sequencer::FrameElement" );

		return local_frames[top_frame]->FrameElement( frame_offset );
		}

	else
		{
		if ( frame_offset < 0 || frame_offset >= global_frame.length() )
			fatal->Report(
			"bad global frame offset in Sequencer::FrameElement" );
		return global_frame[frame_offset];
		}
	}

void Sequencer::SetFrameElement( scope_type scope, int frame_offset,
					Value* value )
	{
	Value* prev_value;

	if ( scope == LOCAL_SCOPE )
		{
		int top_frame = local_frames.length() - 1;
		if ( top_frame < 0 )
			fatal->Report(
	"local frame requested but none exist in Sequencer::SetFrameElement" );

		Value*& frame_value =
			local_frames[top_frame]->FrameElement( frame_offset );
		prev_value = frame_value;
		frame_value = value;
		}

	else
		{
		if ( frame_offset < 0 || frame_offset >= global_frame.length() )
			fatal->Report(
		"bad global frame offset in Sequencer::SetFrameElement" );
		prev_value = global_frame.replace( frame_offset, value );
		}

	Unref( prev_value );
	}


char* Sequencer::RegisterTask( Task* new_task )
	{
	char buf[128];
	sprintf( buf, "task%d", ++last_task_id );

	char* new_ID = strdup( buf );

	ids_to_tasks.Insert( new_ID, new_task );

	return new_ID;
	}

void Sequencer::DeleteTask( Task* task )
	{
	(void) ids_to_tasks.Remove( task->TaskID() );
	}


void Sequencer::AddStmt( Stmt* addl_stmt )
	{
	stmts = merge_stmts( stmts, addl_stmt );
	}


int Sequencer::RegisterStmt( Stmt* stmt )
	{
	registered_stmts.append( stmt );
	return registered_stmts.length();
	}

Stmt* Sequencer::LookupStmt( int index )
	{
	if ( index <= 0 || index > registered_stmts.length() )
		return 0;

	return registered_stmts[index - 1];
	}

Channel* Sequencer::GetHostDaemon( const char* host )
	{
	if ( ! host ||
	     ! strcmp( host, ConnectionHost() ) ||
	     ! strcmp( host, "localhost" ) )
		// request is for local host
		return 0;

	RemoteDaemon* d = daemons[host];
	if ( ! d )
		d = CreateDaemon( host );

	return d->DaemonChannel();
	}


void Sequencer::Exec()
	{
	if ( interactive )
		return;

	if ( error->Count() > 0 )
		{
		message->Report( "execution aborted" );
		return;
		}

	stmt_flow_type flow;
	Unref( stmts->Exec( 0, flow ) );

	EventLoop();
	}


void Sequencer::Await( Stmt* arg_await_stmt, int only_flag,
			Stmt* arg_except_stmt )
	{
	Stmt* hold_await_stmt = await_stmt;
	int hold_only_flag = await_only_flag;
	Stmt* hold_except_stmt = except_stmt;

	await_stmt = arg_await_stmt;
	await_only_flag = only_flag;
	except_stmt = arg_except_stmt;

	EventLoop();

	await_stmt = hold_await_stmt;
	await_only_flag = only_flag;
	except_stmt = hold_except_stmt;
	}


Value* Sequencer::AwaitReply( Task* task, const char* event_name,
				const char* reply_name )
	{
	GlishEvent* reply = recv_event( task->GetChannel()->ReadFD() );
	Value* result = 0;

	if ( ! reply )
		{
		warn->Report( task, " terminated without replying to ",
				event_name, " request" );
		result = error_value();
		}

	else if ( ! strcmp( reply->name, reply_name ) )
		{
		result = reply->value;
		Ref( result );
		}

	else
		{
		warn->Report( "expected reply from ", task, " to ",
				event_name, " request, instead got \"",
				reply->name, "\"" );

		Ref( reply );	// So NewEvent doesn't throw it away.
		NewEvent( task, reply );

		result = reply->value;
		Ref( result );	// So following Unref doesn't discard value.
		}

	Unref( reply );

	return result;
	}


Channel* Sequencer::AddLocalClient( int read_fd, int write_fd )
	{
	Channel* c = new Channel( read_fd, write_fd );
	Selectee* s = new LocalClientSelectee( this, c );

	selector->AddSelectee( s );

	return c;
	}


Channel* Sequencer::WaitForTaskConnection( Task* task )
	{
	Task* t;
	Channel* chan;

	// Need to loop because perhaps we'll receive connections
	// from tasks other than the one we're waiting for.
	do
		{
		int new_conn = accept_connection( connection_socket->FD() );
		mark_close_on_exec( new_conn );

		chan = new Channel( new_conn, new_conn );
		t = NewConnection( chan );
		}
	while ( t && t != task );

	if ( t )
		return chan;
	else
		return 0;
	}

Task* Sequencer::NewConnection( Channel* connection_channel )
	{
	GlishEvent* establish_event =
		recv_event( connection_channel->ReadFD() );

	// It's possible there's already a Selectee for this channel,
	// due to using a LocalClientSelectee.  If so, remove it, so
	// it doesn't trigger additional activity.
	RemoveSelectee( connection_channel );

	if ( ! establish_event )
		{
		error->Report( "new connection immediately broken" );
		return 0;
		}

	Value* v = establish_event->value;
	char* task_id;
	int protocol;

	if ( v->Type() == TYPE_STRING )
		{
		task_id = v->StringVal();
		protocol = 1;
		}

	else if ( ! v->FieldVal( "name", task_id ) ||
		  ! v->FieldVal( "protocol", protocol ) )
		{
		error->Report( "bad connection establishment" );
		return 0;
		}

	// ### Should check for protocol compatibility here.

	Task* task = ids_to_tasks[task_id];

	if ( ! task )
		{
		error->Report( "connection received from non-existent task ",
				task_id );
		Unref( establish_event );
		return 0;
		}

	else
		{
		task->SetProtocol( protocol );
		AssociateTaskWithChannel( task, connection_channel );
		NewEvent( task, establish_event );
		}

	delete task_id;

	return task;
	}


void Sequencer::AssociateTaskWithChannel( Task* task, Channel* chan )
	{
	task->SetChannel( chan, selector );
	task->SetActive();

	selector->AddSelectee( new ClientSelectee( this, task ) );

	// empty out buffer so subsequent select()'s will work
	if ( chan->DataInBuffer() )
		EmptyTaskChannel( task );
	}

void Sequencer::RemoveSelectee( Channel* chan )
	{
	if ( selector->FindSelectee( chan->ReadFD() ) )
		selector->DeleteSelectee( chan->ReadFD() );
	}


int Sequencer::NewEvent( Task* task, GlishEvent* event )
	{
	if ( ! event )
		{ // task termination
		task->CloseChannel();

		if ( ! task->Active() )
			return 0;

		// Abnormal termination - no "done" message first.
		event = new GlishEvent( (const char*) "fail",
					new Value( task->AgentID() ) );
		}

	const char* event_name = event->name;
	Value* value = event->value;

	if ( verbose > 0 )
		message->Report( name, ": received event ",
				 task->Name(), ".", event_name, " ", value );

	if ( monitor_task && task != monitor_task )
		LogEvent( task->TaskID(), task->Name(), event_name, value, 1 );

	// If true, generate message if no interest in event.
	int complain_if_no_interest = 0;

	if ( ! strcmp( event_name, "established" ) )
		{
		// We already did the SetActive() when the channel
		// was established.
		}

	else if ( ! strcmp( event_name, "done" ) )
		task->SetDone();

	else if ( ! strcmp( event_name, "fail" ) )
		{
		task->SetDone();
		complain_if_no_interest = 1;
		}

	else if ( ! strcmp( event_name, "*rendezvous*" ) )
		Rendezvous( event_name, value );

	else if ( ! strcmp( event_name, "*forward*" ) )
		ForwardEvent( event_name, value );

	else
		complain_if_no_interest = 1;

	int ignore_event = 0;
	int await_finished = 0;

	if ( await_stmt )
		{
		await_finished =
			task->HasRegisteredInterest( await_stmt, event_name );

		if ( ! await_finished && await_only_flag &&
		     ! task->HasRegisteredInterest( except_stmt, event_name ) )
			ignore_event = 1;
		}

	if ( ignore_event )
		warn->Report( "event ", task->Name(), ".", event_name,
			      " ignored due to \"await\"" );

	else
		{
		// We're going to want to keep the event value as a field
		// in the task's AgentRecord.
		Ref( value );

		int was_interest = task->CreateEvent( event_name, value );

		if ( ! was_interest && complain_if_no_interest )
			warn->Report( "event ", task->Name(), ".", event_name,
					" (", value, ") dropped" );

		RunQueue();	// process effects of CreateEvent()
		}

	Unref( event );

	if ( await_finished )
		{
		pending_task = task;

		// Make sure the pending task isn't delete'd before
		// we can exhaust its pending input.

		Ref( pending_task );

		return 1;
		}

	else
		return 0;
	}


void Sequencer::NewClientStarted()
	{
	++num_active_processes;
	}


int Sequencer::ShouldSuspend( const char* task_var_ID )
	{
	if ( task_var_ID )
		return suspend_list[task_var_ID];
	else
		// This is an anonymous client - don't suspend.
		return 0;
	}

int Sequencer::EmptyTaskChannel( Task* task, int force_read )
	{
	int status = 0;

	if ( task->Active() )
		{
		Channel* c = task->GetChannel();
		ChanState old_state = c->ChannelState();

		c->ChannelState() = CHAN_IN_USE;

		if ( force_read )
			status = NewEvent( task, recv_event( c->ReadFD() ) );

		while ( status == 0 &&
			c->ChannelState() == CHAN_IN_USE &&
			c->DataInBuffer() )
			{
			status = NewEvent( task, recv_event( c->ReadFD() ) );
			}

		if ( c->ChannelState() == CHAN_INVALID )
			{ // This happens iff the given task has exited
			selector->DeleteSelectee( c->ReadFD() );
			delete c;

			while ( reap_terminated_process() )
				;

			--num_active_processes;
			}

		else
			c->ChannelState() = old_state;
		}

	return status;
	}


void Sequencer::MakeEnvGlobal()
	{
	Value* env_value = create_record();

	extern char** environ;
	for ( char** env_ptr = environ; *env_ptr; ++env_ptr )
		{
		char* delim = strchr( *env_ptr, '=' );

		if ( delim )
			{
			*delim = '\0';
			env_value->AssignRecordElement( *env_ptr,
						    new Value( delim + 1 ) );
			*delim = '=';
			}
		else
			env_value->AssignRecordElement( *env_ptr,
						new Value( glish_false ) );
		}

	Expr* env_expr = LookupID( strdup( "environ" ), GLOBAL_SCOPE );
	env_expr->Assign( env_value );
	}


void Sequencer::MakeArgvGlobal( char** argv, int argc )
	{
	// If there's an initial "--" argument, remove it, it's a vestige
	// from when "--" was needed to separate script files from their
	// arguments.
	if ( argc > 0 && ! strcmp( argv[0], "--" ) )
		++argv, --argc;

	Value* argv_value = new Value( (charptr*) argv, argc, COPY_ARRAY );
	Expr* argv_expr = LookupID( strdup( "argv" ), GLOBAL_SCOPE );
	argv_expr->Assign( argv_value );
	}


void Sequencer::BuildSuspendList()
	{
	char* suspend_env_list = getenv( "suspend" );

	if ( ! suspend_env_list )
		return;

	char* suspendee = strtok( suspend_env_list, " " );

	while ( suspendee )
		{
		suspend_list.Insert( suspendee, 1 );
		suspendee = strtok( 0, " " );
		}
	}


void Sequencer::Parse( FILE* file, const char* filename )
	{
	restart_yylex( file );

	yyin = file;
	current_sequencer = this;
	line_num = 1;
	input_file_name = filename ? strdup( filename ) : 0;

	if ( yyin && isatty( fileno( yyin ) ) )
		{
		message->Report( "Glish version ", GLISH_VERSION, "." );

		// We're about to enter the "interactive" loop, so
		// first execute any statements we've seen so far due
		// to .glishrc files.
		Exec();

		// And add a special Selectee for detecting user input.
		selector->AddSelectee( new UserInputSelectee( fileno( yyin ) ) );
		interactive = 1;
		}
	else
		interactive = 0;

	if ( yyparse() )
		error->Report( "syntax errors parsing input" );

	// Don't need to delete input_file_name, yylex() already did
	// that on <<EOF>>.
	input_file_name = 0;

	line_num = 0;
	}


void Sequencer::Parse( const char file[] )
	{
	FILE* f = fopen( file, "r" );

	if ( ! f )
		error->Report( "can't open file \"", file, "\"" );
	else
		Parse( f, file );
	}

void Sequencer::Parse( const char* strings[] )
	{
	scan_strings( strings );
	Parse( 0, "glish internal initialization" );
	}


RemoteDaemon* Sequencer::CreateDaemon( const char* host )
	{
	RemoteDaemon* rd = OpenDaemonConnection( host );

	if ( rd )
		// We're all done, the daemon was already running.
		return rd;

	// Have to start up the daemon.
	message->Report( "activating Glish daemon on ", host );

	char daemon_cmd[1024];
	sprintf( daemon_cmd, "%s %s -n glishd &", RSH, host );
	system( daemon_cmd );

	rd = OpenDaemonConnection( host );
	while ( ! rd )
		{
		message->Report( "waiting for daemon ..." );
		sleep( 1 );
		rd = OpenDaemonConnection( host );
		}

	return rd;
	}

RemoteDaemon* Sequencer::OpenDaemonConnection( const char* host )
	{
	int daemon_socket = get_tcp_socket();

	if ( remote_connection( daemon_socket, host, DAEMON_PORT ) )
		{ // Connected.
		mark_close_on_exec( daemon_socket );

		Channel* daemon_channel =
			new Channel( daemon_socket, daemon_socket );

		RemoteDaemon* r = new RemoteDaemon( host, daemon_channel );
		daemons.Insert( strdup( host ), r );

		// Read and discard daemon's "establish" event.
		GlishEvent* e = recv_event( daemon_channel->ReadFD() );
		Unref( e );

		// Tell the daemon which directory we want to work out of.
		char work_dir[MAXPATHLEN];

		if ( ! getcwd( work_dir, sizeof( work_dir ) ) )
			fatal->Report( "problems getting cwd:", work_dir );

		Value work_dir_value( work_dir );
		send_event( daemon_channel->WriteFD(), "setwd",
				&work_dir_value );

		selector->AddSelectee(
			new DaemonSelectee( r, selector, this ) );

		return r;
		}

	else
		{
		close( daemon_socket );
		return 0;
		}
	}


void Sequencer::ActivateMonitor( char* monitor_client_name )
	{
	TaskAttr* monitor_attrs =
		new TaskAttr( "*monitor*", "localhost", 0, 0, 0, 0 );

	const_args_list monitor_args;
	monitor_args.append( new Value( monitor_client_name ) );

	monitor_task = new ClientTask( &monitor_args, monitor_attrs, this );

	if ( monitor_task->TaskError() )
		{
		Unref( monitor_task );
		monitor_task = 0;
		}
	}


void Sequencer::LogEvent( const char* gid, const char* id,
			const char* event_name, const Value* event_value,
			int is_inbound )
	{
	if ( ! monitor_task )
		return;

	Value gid_value( gid );
	Value id_value( id );
	Value name_value( event_name );

	parameter_list args;

	ConstExpr gid_expr( &gid_value );
	ConstExpr id_expr( &id_value );
	ConstExpr name_expr( &name_value );
	ConstExpr value_expr( event_value );

	Parameter gid_param( "glish_id", VAL_VAL, &gid_expr, 0 );
	Parameter id_param( "id", VAL_VAL, &id_expr, 0 );
	Parameter name_param( "name", VAL_VAL, &name_expr, 0 );
	Parameter value_param( "value", VAL_VAL, &value_expr, 0 );

	args.insert( &name_param );
	args.insert( &id_param );
	args.insert( &gid_param );
	args.insert( &value_param );

	const char* monitor_event_name = is_inbound ? "event_in" : "event_out";
	monitor_task->SendEvent( monitor_event_name, &args, 0, 0 );
	}

void Sequencer::LogEvent( const char* gid, const char* id, const GlishEvent* e,
			int is_inbound )
	{
	LogEvent( gid, id, e->name, e->value, is_inbound );
	}


void Sequencer::SystemEvent( const char* name, const Value* val )
	{
	system_agent->SendSingleValueEvent( name, val, 1 );
	}


void Sequencer::Rendezvous( const char* event_name, Value* value )
	{
	char* source_id;
	char* sink_id;

	if ( ! value->FieldVal( "source_id", source_id ) ||
	     ! value->FieldVal( "sink_id", sink_id ) )
		fatal->Report( "bad internal", event_name, "event" );

	Task* src = ids_to_tasks[source_id];
	Task* snk = ids_to_tasks[sink_id];

	if ( ! src || ! snk )
		fatal->Report( "no such source or sink ID in internal",
				event_name, "event:", source_id, sink_id );

	// By sending out these two events immediately, before any other
	// *rendezvous* events can arise, we impose a serial ordering on
	// all rendezvous.  This avoids deadlock.
	//
	// Actually, now that we always use sockets (even locally), the
	// following isn't necessary, since connecting to a socket won't
	// block (unless the "listen" queue for the remote socket is
	// full).  We could just pass along the *rendezvous-resp* event
	// to the sink and be done with it.  But we retain the protocol
	// because it was a lot of work getting it right and we don't want
	// to have to figure it out again if for some reason we don't
	// always use sockets.
	src->SendSingleValueEvent( "*rendezvous-orig*", value, 1 );
	snk->SendSingleValueEvent( "*rendezvous-resp*", value, 1 );

	delete source_id;
	delete sink_id;
	}


void Sequencer::ForwardEvent( const char* event_name, Value* value )
	{
	char* receipient_id;
	char* new_event_name;

	if ( ! value->FieldVal( "receipient", receipient_id ) ||
	     ! value->FieldVal( "event", new_event_name ) )
		fatal->Report( "bad internal event \"", event_name, "\"" );

	Task* task = ids_to_tasks[receipient_id];

	if ( ! task )
		fatal->Report( "no such receipient ID in ", event_name,
				"internal event:", receipient_id );

	task->SendSingleValueEvent( new_event_name, value, 1 );

	delete receipient_id;
	delete new_event_name;
	}


void Sequencer::EventLoop()
	{
	RunQueue();

	if ( pending_task )
		{
		EmptyTaskChannel( pending_task );

		// We Ref()'d the pending_task when assigning it, to make
		// sure it didn't go away due to the effects of RunQueue().

		Unref( pending_task );

		pending_task = 0;
		}

	while ( ActiveClients() && ! selector->DoSelection() )
		RunQueue();
	}


void Sequencer::RunQueue()
	{
	Notification* n;

	while ( (n = notification_queue.DeQueue()) )
		{
		if ( verbose > 1 )
			message->Report( "doing", n );

		if ( n->notifiee->frame )
			PushFrame( n->notifiee->frame );

		Value* notifier_val = n->notifier->AgentRecord();

		if ( notifier_val->Type() == TYPE_RECORD &&
		     notifier_val->HasRecordElement( n->field ) != n->value )
			// Need to assign the event value.
			notifier_val->AssignRecordElement( n->field, n->value );

		// There are a bunch of Ref's and Unref's here because the
		// Notify() call below can lead to a recursive call to this
		// routine (due to an "await" statement), so 'n' might
		// otherwise be deleted underneath our feet.
		Unref( last_notification );
		last_notification = n;

		Ref( n );
		n->notifiee->stmt->Notify( n->notifier );

		if ( n->notifiee->frame )
			(void) PopFrame();
		Unref( n );
		}
	}


ClientSelectee::ClientSelectee( Sequencer* s, Task* t )
    : Selectee( t->GetChannel()->ReadFD() )
	{
	sequencer = s;
	task = t;
	}

int ClientSelectee::NotifyOfSelection()
	{
	return sequencer->EmptyTaskChannel( task, 1 );
	}


LocalClientSelectee::LocalClientSelectee( Sequencer* s, Channel* c )
    : Selectee( c->ReadFD() )
	{
	sequencer = s;
	chan = c;
	}

int LocalClientSelectee::NotifyOfSelection()
	{
	(void) sequencer->NewConnection( chan );
	return 0;
	}


AcceptSelectee::AcceptSelectee( Sequencer* s, Socket* conn_socket )
    : Selectee( conn_socket->FD() )
	{
	sequencer = s;
	connection_socket = conn_socket;
	}

int AcceptSelectee::NotifyOfSelection()
	{
	int new_conn;

	if ( connection_socket->IsLocal() )
		new_conn = accept_local_connection( connection_socket->FD() );
	else
		new_conn = accept_connection( connection_socket->FD() );

	mark_close_on_exec( new_conn );

	(void) sequencer->NewConnection( new Channel( new_conn, new_conn ) );

	return 0;
	}


ScriptSelectee::ScriptSelectee( Client* client, Agent* agent, int conn_socket )
    : Selectee( conn_socket )
	{
	script_client = client;
	script_agent = agent;
	connection_socket = conn_socket;
	}

int ScriptSelectee::NotifyOfSelection()
	{
	fd_set fd_mask;

	FD_ZERO( &fd_mask );
	FD_SET( connection_socket, &fd_mask );

	GlishEvent* e = script_client->NextEvent( &fd_mask );

	if ( ! e )
		{
		delete script_client;
		exit( 0 );
		}

	// Ref() the value, since CreateEvent is going to Unref() it, and the
	// script_client is also going to Unref() it via Unref()'ing the
	// whole GlishEvent.
	Ref( e->value );

	script_agent->CreateEvent( e->name, e->value );

	return 0;
	}


DaemonSelectee::DaemonSelectee( RemoteDaemon* arg_daemon, Selector* sel,
				Sequencer* s )
: Selectee( arg_daemon->DaemonChannel()->ReadFD() )
	{
	daemon = arg_daemon;
	selector = sel;
	sequencer = s;
	}

int DaemonSelectee::NotifyOfSelection()
	{
	int fd = daemon->DaemonChannel()->ReadFD();
	GlishEvent* e = recv_event( fd );

	const char* message_name = 0;

	if ( e )
		{
		if ( ! strcmp( e->name, "probe-reply" ) )
			{
			if ( daemon->State() == DAEMON_LOST )
				{
				message->Report( "connectivity to daemon @ ",
						daemon->Host(), " restored" );
				message_name = "connection_restored";
				}

			daemon->SetState( DAEMON_OK );
			}

		else
			{
			error->Report(
				"received unsolicited message from daemon @ ",
					daemon->Host() );
			}

		Unref( e );
		}

	else
		{
		error->Report( "Glish daemon @ ", daemon->Host(),
				" terminated" );
		selector->DeleteSelectee( fd );
		message_name = "daemon_terminated";
		}

	if ( message_name )
		{
		Value message_val( daemon->Host() );
		sequencer->SystemEvent( message_name, &message_val );
		}

	return 0;
	}


ProbeTimer::ProbeTimer( PDict(RemoteDaemon)* arg_daemons, Sequencer* s )
: SelectTimer( PROBE_DELAY, 0, PROBE_INTERVAL, 0 )
	{
	daemons = arg_daemons;
	sequencer = s;
	}

int ProbeTimer::DoExpiration()
	{
	IterCookie* c = daemons->InitForIteration();

	RemoteDaemon* r;
	const char* key;
	while ( (r = daemons->NextEntry( key, c )) )
		{
		if ( r->State() == DAEMON_REPLY_PENDING )
			{ // Oops.  Haven't gotten a reply from our last probe.
			warn->Report( "connection to Glish daemon @ ", key,
					" lost" );
			r->SetState( DAEMON_LOST );

			Value message_val( r->Host() );
			sequencer->SystemEvent( "connection_lost",
						&message_val );
			}

		// Probe the daemon, regardless of its state.
		send_event( r->DaemonChannel()->WriteFD(), "probe",
				false_value );

		if ( r->State() == DAEMON_OK )
			r->SetState( DAEMON_REPLY_PENDING );
		}

	return 1;
	}


ScriptClient::ScriptClient( int& argc, char** argv ) : Client( argc, argv )
	{
	selector = 0;
	agent = 0;
	}

void ScriptClient::SetInterface( Selector* s, Agent* a )
	{
	selector = s;
	agent = a;
	selector->AddSelectee( new ScriptSelectee( this, agent, read_fd ) );
	}

void ScriptClient::FD_Change( int fd, int add_flag )
	{
	if ( ! agent )
		return;

	if ( add_flag )
		selector->AddSelectee( new ScriptSelectee( this, agent, fd ) );
	else
		selector->DeleteSelectee( fd );
	}
