// $Header$

#include <stream.h>
#include <string.h>

#include "Glish/Value.h"

#include "Expr.h"
#include "Func.h"
#include "Agent.h"
#include "Reporter.h"


EventDesignator::EventDesignator( Expr* arg_agent, Expr* arg_event_name )
	{
	agent = arg_agent;
	event_name_expr = arg_event_name;
	event_name_str = 0;
	event_agent_ref = 0;
	}

EventDesignator::EventDesignator( Expr* arg_agent, const char* arg_event_name )
	{
	agent = arg_agent;
	event_name_expr = 0;
	event_name_str = arg_event_name;
	event_agent_ref = 0;
	}


Agent* EventDesignator::EventAgent( value_type val_type )
	{
	event_agent_ref = agent->RefEval( val_type );
	Value* event_agent_val = event_agent_ref->Deref();

	if ( ! event_agent_val->IsAgentRecord() )
		{
		EventAgentDone();
		return 0;
		}

	return event_agent_val->AgentVal();
	}

void EventDesignator::EventAgentDone()
	{
	Unref( event_agent_ref );
	}

Value* EventDesignator::SendEvent( parameter_list* arguments, int is_request )
	{
	name_list* nl = EventNames();

	if ( ! nl )
		{
		error->Report( "->* illegal for sending an event" );
		return is_request ? error_value() : 0;
		}

	Agent* a = EventAgent( VAL_REF );
	Value* result = 0;

	if ( a )
		{
		if ( nl->length() > 1 )
			error->Report( this,
					"must designate exactly one event" );

		result = a->SendEvent( (*nl)[0], arguments, is_request, 1 );
		}

	else
		error->Report( EventAgentExpr(), "is not an agent" );

	delete_name_list( nl );

	EventAgentDone();

	return result;
	}

void EventDesignator::Register( Notifiee* notifiee )
	{
	name_list* nl = EventNames();

	Agent* a = EventAgent( VAL_CONST );

	if ( a )
		{
		if ( ! nl )
			// Register for all events.
			a->RegisterInterest( notifiee );

		else
			loop_over_list( *nl, i )
				a->RegisterInterest( notifiee, (*nl)[i], 1 );

		// We don't delete the elements of nl because they've
		// been given over to the agent.

		delete nl;
		}

	else
		{
		error->Report( EventAgentExpr(), "is not an agent" );

		delete_name_list( nl );
		}

	EventAgentDone();
	}

name_list* EventDesignator::EventNames()
	{
	name_list* result = new name_list;

	if ( event_name_str )
		{
		result->append( strdup( event_name_str ) );
		return result;
		}

	if ( ! event_name_expr )
		return 0;

	const Value* index_val = event_name_expr->ReadOnlyEval();

	if ( index_val->Type() == TYPE_STRING )
		{
		int n = index_val->Length();
		const char** s = index_val->StringPtr();
		for ( int i = 0; i < n; ++i )
			result->append( strdup( s[i] ) );
		}

	else
		error->Report( this, "does not have a string-valued index" );

	event_name_expr->ReadOnlyDone( index_val );

	return result;
	}

void EventDesignator::DescribeSelf( ostream& s ) const
	{
	EventAgentExpr()->DescribeSelf( s );
	s << "->";

	if ( event_name_expr )
		{
		s << "[";
		event_name_expr->Describe( s );
		s << "]";
		}

	else if ( event_name_str )
		s << "." << event_name_str;

	else
		s << "*";
	}


void delete_name_list( name_list* nl )
	{
	if ( nl )
		{
		loop_over_list( *nl, i )
			delete (*nl)[i];

		delete nl;
		}
	}


void describe_event_list( const event_list* list, ostream& s )
	{
	if ( list )
		loop_over_list( *list, i )
			{
			if ( i > 0 )
				s << ", ";
			(*list)[i]->Describe( s );
			}
	}
