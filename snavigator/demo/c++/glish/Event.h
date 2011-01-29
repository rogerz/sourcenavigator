// $Header$

#ifndef event_h
#define event_h

#include "Glish/List.h"

class EventDesignator;

declare(PList,EventDesignator);
typedef PList(EventDesignator) event_list;

class Expr;
class Notifiee;
class Agent;


class EventDesignator : public GlishObject {
public:
	EventDesignator( Expr* agent, Expr* event_name );
	EventDesignator( Expr* agent, const char* event_name );

	// Send out an event with the given value.  The event designator
	// called already knows what the event name is.  If is_request is
	// true than this is a request/response event, and the value of
	// the response is returned; otherwise the function returns nil.
	Value* SendEvent( ParameterPList* arguments, int is_request );

	// Used to register a "notifiee" (i.e., an event statement plus
	// an associated Frame) as wanting to be notified of occurrences
	// of the event corresponding to this event designator.
	void Register( Notifiee* notifiee );

	// Evaluates and returns the event's agent.  Returns nil if
	// the agent expression does not evaluate to an agent value.
	// EventAgentDone() must be called when done with the agent
	// value (it should not be called if a nil value was returned).
	//
	// The val_type argument indicates whether the agent is going
	// to be used for modification (VAL_REF) or not (VAL_CONST).
	Agent* EventAgent( value_type val_type );
	void EventAgentDone();

	// Returns the event agent expression, primarily for use
	// in error messages.
	Expr* EventAgentExpr() const	{ return agent; }

	// Evaluates the event name(s) and returns a list of them.
	// The contents of the list should be deleted when done, as
	// should the list itself.
	//
	// If this event designator is for "any" event then a nil
	// list is returned.
	name_list* EventNames();

	void DescribeSelf( ostream& s ) const;

protected:
	Expr* agent;
	Expr* event_name_expr;
	const char* event_name_str;
	Value* event_agent_ref;
	};

extern void delete_name_list( name_list* nl );

extern void describe_event_list( const event_list* list, ostream& s );

#endif /* event_h */
