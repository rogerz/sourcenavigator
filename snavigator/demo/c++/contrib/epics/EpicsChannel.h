// $Header$

#ifndef epicsclient_h
#define epicsclient_h

extern "C" {
#include <caerr.h>
#include <cadef.h>
}

#include "Glish/Client.h"

class EpicsChannel {
public:
	EpicsChannel( const char* name, double timeout );

	virtual ~EpicsChannel();

	int Connected() const	{ return connected; }

	// Return native EPICS type, or DBF_NO_ACCESS if not connected.
	chtype NativeType() const;

	// Return corresponding Glish type, or TYPE_ERROR if not connected.
	glish_type Type() const;

	// Number of elements, or 0 if not connected.
	int Length() const;

	const char* Name() const	{ return channel_name; }

	// Most recent error status.
	int Status() const		{ return status; }

	enum channel_state State() const;

	Value* Get();	// get current value, or nil pointer if error.

	// Change the channel's value.  Returns non-zero on success.
	// new_value will be polymorphed to correspond to Channel's
	// native type.
	int Set( Value* new_value );

	fd_set InputSources() const;

protected:
	virtual void EventHandler();

	static void CA_event_handler( struct event_handler_args args );

	double timeout;
	char* channel_name;
	chid channel_id;
	int connected;
	int status;
};

#endif	/* epicsclient_h */
