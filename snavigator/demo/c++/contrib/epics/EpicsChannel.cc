// $Header$

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "EpicsChannel.h"

extern "C" {
#include "iocmsg.h"
#include "iocinf.h"
}


EpicsChannel::EpicsChannel( const char* name, double arg_timeout )
	{
	channel_name = strdup( name );
	timeout = arg_timeout;

	status = ca_search( channel_name, &channel_id );

	if ( status == ECA_NORMAL &&
	     (status = ca_pend_io( timeout )) == ECA_NORMAL &&
	     (status = ca_add_event( NativeType(), channel_id, CA_event_handler,
					(void*) this, 0 )) == ECA_NORMAL )
		connected = 1;
	else
		connected = 0;
	}

EpicsChannel::~EpicsChannel()
	{
	delete channel_name;
	}

chtype EpicsChannel::NativeType() const
	{
	return connected ? ca_field_type(channel_id) : DBF_NO_ACCESS;
	}

glish_type EpicsChannel::Type() const
	{
	switch ( NativeType() )
		{
		case DBF_STRING:
			return TYPE_STRING;

#if 0
		case DBF_ENUM:
			return TYPE_BYTE;

		case DBF_SHORT:
			return TYPE_SHORT;
#endif

		case DBF_INT:
		case DBF_LONG:
			return TYPE_INT;

		case DBF_FLOAT:
			return TYPE_FLOAT;

		case DBF_DOUBLE:
			return TYPE_DOUBLE;

		default:
			return TYPE_ERROR;
		}
	}

int EpicsChannel::Length() const
	{
	return connected ? int(ca_element_count(channel_id)) : 0;
	}

enum channel_state EpicsChannel::State() const
	{
	return ca_state(channel_id);
	}

Value* EpicsChannel::Get()
	{
	status = ca_pend_io( timeout );

	if ( status != ECA_NORMAL )
		return 0;

	switch ( Type() )
		{
#define FETCH(tag, type)						\
	case tag:							\
		{							\
		type* pvalue = new type[Length()];			\
		status = ca_array_get( NativeType(), Length(),		\
				   channel_id, pvalue );		\
		if ( status == ECA_NORMAL &&				\
		     (status = ca_pend_io( timeout )) == ECA_NORMAL )	\
			return new Value( pvalue, Length() );		\
		else							\
			{						\
			delete pvalue;					\
			return 0;					\
			}						\
		}

		FETCH(TYPE_INT, int);
		FETCH(TYPE_FLOAT, float);
		FETCH(TYPE_DOUBLE, double);

		//### probably wrong.
		FETCH(TYPE_STRING, char*);

		default:
			return 0;
		}
	}

int EpicsChannel::Set( Value* new_value )
	{
	new_value->Polymorph( Type() );

	if ( new_value->Length() > Length() )
		return 0;

	void* pvalue;

	switch ( new_value->Type() )
		{
		case TYPE_INT:
			pvalue = new_value->IntPtr();
			break;

		case TYPE_FLOAT:
			pvalue = new_value->FloatPtr();
			break;

		case TYPE_DOUBLE:
			pvalue = new_value->DoublePtr();
			break;

		case TYPE_STRING:
			//### almost surely wrong.
			pvalue = new_value->StringPtr();
			break;

		default:
			return 0;
		}

	status = ca_array_put( NativeType(), Length(), channel_id, pvalue );

	if ( status == ECA_NORMAL )
		status = ca_pend_io( timeout );

	return status == ECA_NORMAL;
	}

fd_set EpicsChannel::InputSources() const
	{
	return ca_static->ca_readch;
	}

void EpicsChannel::EventHandler()
	{
	}

void EpicsChannel::CA_event_handler( struct event_handler_args args )
	{
	fprintf( stderr, "handler hit: type=%d count=%d\n",
		args.type, args.count );

	((EpicsChannel*) args.usr)->EventHandler();
	}
