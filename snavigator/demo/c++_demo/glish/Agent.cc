// $Header$

#include <stream.h>
#include <stdio.h>
#include <string.h>

#include "Agent.h"
#include "Frame.h"
#include "Glish/Value.h"
#include "Reporter.h"
#include "Sequencer.h"


#define INTERESTED_IN_ALL "*"


agent_list agents;


Notifiee::Notifiee( Stmt* arg_stmt, Frame* arg_frame )
	{
	stmt = arg_stmt;
	frame = arg_frame;

	Ref( stmt );

	if ( frame )
		Ref( frame );
	}

Notifiee::~Notifiee()
	{
	Unref( stmt );
	Unref( frame );
	}

Agent::Agent( Sequencer* s )
	{
	sequencer = s;
	agent_ID = 0;
	agent_value = 0;
	string_copies = 0;

	agent_value = new Value( create_record_dict(), this );

	agents.append( this );
	}

Agent::~Agent()
	{
	IterCookie* c = interested_parties.InitForIteration();

	notification_list* list;
	const char* key;
	while ( (list = interested_parties.NextEntry( key, c )) )
		{
		loop_over_list( *list, i )
			delete (*list)[i];
		}


	if ( string_copies )
		{
		loop_over_list( *string_copies, i )
			{
			char* str = (char*) (*string_copies)[i];
			delete str;
			}

		delete string_copies;
		}

	(void) agents.remove( this );
	}

void Agent::SendSingleValueEvent( const char* event_name, const Value* value,
					int log )
	{
	ConstExpr c( value );	// ### make sure ConstExpr doesn't nuke
	Parameter p( "event_in", VAL_VAL, &c, 0 );
	parameter_list plist;
	plist.append( &p );

	SendEvent( event_name, &plist, 0, log );
	}

int Agent::CreateEvent( const char* event_name, Value* event_value )
	{
	if ( ! agent_value )
		fatal->Report(
			"no event agent value in Agent::CreateEvent" );

	return NotifyInterestedParties( event_name, event_value );
	}

void Agent::RegisterInterest( Notifiee* notifiee, const char* field,
					int is_copy )
	{
	if ( ! field )
		field = INTERESTED_IN_ALL;

	notification_list* interest_list = interested_parties[field];

	if ( ! interest_list )
		{
		interest_list = new notification_list;
		interested_parties.Insert( field, interest_list );
		}

	interest_list->append( notifiee );

	if ( is_copy )
		{
		// We need to remember the field since we're responsible
		// for garbage-collecting it when we're destructed.

		if ( ! string_copies )
			string_copies = new string_list;

		string_copies->append( field );
		}
	}

int Agent::HasRegisteredInterest( Stmt* stmt, const char* field )
	{
	notification_list* interest = interested_parties[field];

	if ( interest && SearchNotificationList( interest, stmt ) )
		return 1;

	interest = interested_parties[INTERESTED_IN_ALL];

	if ( interest && SearchNotificationList( interest, stmt ) )
		return 1;

	return 0;
	}

Value* Agent::AssociatedStatements()
	{
	int num_stmts = 0;

	IterCookie* c = interested_parties.InitForIteration();
	const char* key;
	notification_list* interest;

	while ( (interest = interested_parties.NextEntry( key, c )) )
		num_stmts += interest->length();

	char** event = new char*[num_stmts];
	int* stmt = new int[num_stmts];
	int count = 0;

	c = interested_parties.InitForIteration();
	while ( (interest = interested_parties.NextEntry( key, c )) )
		{
		loop_over_list( *interest, j )
			{
			event[count] = strdup( key );
			stmt[count] = (*interest)[j]->stmt->Index();
			++count;
			}
		}

	if ( count != num_stmts )
		fatal->Report(
		"internal inconsistency in Agent::AssociatedStatements" );

	Value* r = create_record();
	r->SetField( "event", (charptr*) event, num_stmts );
	r->SetField( "stmt", stmt, num_stmts );

	return r;
	}

Task* Agent::AgentTask()
	{
	return 0;
	}

void Agent::DescribeSelf( ostream& s ) const
	{
	if ( agent_ID )
		s << agent_ID;
	else
		s << "<agent>";
	}

Value* Agent::BuildEventValue( parameter_list* args, int use_refs )
	{
	if ( args->length() == 0 )
		return error_value();

	if ( args->length() == 1 )
		{
		Expr* arg_expr = (*args)[0]->Arg();

		return use_refs ? arg_expr->RefEval( VAL_CONST ) :
					arg_expr->CopyEval();
		}

	// Build up a record.
	Value* event_val = create_record();

	loop_over_list( *args, i )
		{
		Parameter* p = (*args)[i];
		const char* index;
		char buf[64];
		
		if ( p->Name() )
			index = p->Name();
		else
			{
			sprintf( buf, "arg%d", i + 1 );
			index = buf;
			}

		Expr* arg_expr = p->Arg();
		Value* arg_val = use_refs ?
			arg_expr->RefEval( VAL_CONST ) : arg_expr->CopyEval();

		event_val->AssignRecordElement( index, arg_val );

		if ( use_refs )
			Unref( arg_val );
		}

	return event_val;
	}

int Agent::NotifyInterestedParties( const char* field, Value* value )
	{
	notification_list* interested = interested_parties[field];
	int there_is_interest = 0;

	if ( interested )
		{
		loop_over_list( *interested, i )
			{
			// We ignore DoNotification's return value, for now;
			// we consider that the Notifiee exists, even if not
			// active, sufficient to consider that there was
			// interest in this event.
			(void) DoNotification( (*interested)[i], field, value );
			}

		there_is_interest = 1;
		}

	interested = interested_parties[INTERESTED_IN_ALL];

	if ( interested )
		{
		loop_over_list( *interested, i )
			(void) DoNotification( (*interested)[i], field, value );

		there_is_interest = 1;
		}

	if ( ! there_is_interest && agent_value->Type() == TYPE_RECORD )
		{
		// We have to assign the corresponding field in the agent
		// record right here, ourselves, since the sequencer isn't
		// going to do it for us in Sequencer::RunQueue.
		agent_value->AssignRecordElement( field, value );
		}

	Unref( value );

	return there_is_interest;
	}

int Agent::DoNotification( Notifiee* n, const char* field, Value* value )
	{
	Stmt* s = n->stmt;

	if ( s->IsActiveFor( this, field, value ) )
		{
		Notification* note = new Notification( this, field, value, n );

		sequencer->QueueNotification( note );

		return 1;
		}

	else
		return 0;
	}

int Agent::SearchNotificationList( notification_list* list, Stmt* stmt )
	{
	loop_over_list( *list, i )
		if ( (*list)[i]->stmt == stmt )
			return 1;

	return 0;
	}


Value* UserAgent::SendEvent( const char* event_name, parameter_list* args,
				int /* is_request */, int log )
	{
	Value* event_val = BuildEventValue( args, 0 );

	if ( log )
		sequencer->LogEvent( "<agent>", "<agent>",
					event_name, event_val, 0 );

	CreateEvent( event_name, event_val );

	return 0;
	}
