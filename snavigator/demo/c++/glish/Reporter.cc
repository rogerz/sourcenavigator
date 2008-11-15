// $Header$

#include <stdlib.h>
#include <stream.h>
#include <string.h>

#include "Reporter.h"
#include "input.h"


class WarningReporter : public Reporter {
    public:
	WarningReporter() : Reporter( cerr )	{ }
	virtual void Prolog();
	};


class ErrorReporter : public Reporter {
    public:
	ErrorReporter() : Reporter( cerr )	{ }
	virtual void Prolog();
	};


class FatalReporter : public Reporter {
    public:
	FatalReporter() : Reporter( cerr )	{ }
	virtual void Prolog();
	virtual void Epilog();
	};


class MessageReporter : public Reporter {
    public:
	MessageReporter() : Reporter( cout )	{ }
	virtual void Prolog();
	};


RMessage EndMessage( "" );

// Due to bugs in gcc-2.1, we don't initialize these here but rather
// have Sequencer::Sequencer do it.

Reporter* warn;
Reporter* error;
Reporter* fatal;
Reporter* message;


RMessage::RMessage( const GlishObject* message_object )
	{
	object = message_object;
	str = 0;
	}

RMessage::RMessage( const char* message_string )
	{
	str = message_string;
	object = 0;
	}

RMessage::RMessage( int message_int )
	{
	str = 0;
	object = 0;
	int_val = message_int;
	}

char RMessage::Write( ostream& s, int leading_space, int trailing_space ) const
	{
	if ( object )
		{
		if ( leading_space )
			s << " ";

		object->Describe( s );

		if ( trailing_space )
			s << " ";

		return '\0';
		}

	else if ( str )
		{
		if ( str[0] )
			{
			s << str;
			return str[strlen( str ) - 1];
			}

		else
			return '\0';
		}

	else
		{
		if ( leading_space )
			s << " ";

		s << int_val;

		if ( trailing_space )
			s << " ";

		return '\0';
		}
	}

char RMessage::FirstChar() const
	{
	if ( object )
		return '\0';

	else if ( str )
		return str[0];

	else
		return '0';
	}


Reporter::Reporter( ostream& reporter_stream ) : stream( reporter_stream )
	{
	count = 0;
	}

void Reporter::Report( const RMessage& m0, const RMessage& m1,
			const RMessage& m2, const RMessage& m3,
			const RMessage& m4, const RMessage& m5,
			const RMessage& m6, const RMessage& m7,
			const RMessage& m8, const RMessage& m9,
			const RMessage& m10 )
	{
	const int max_messages = 50;
	const RMessage* messages[max_messages];

	messages[0] = &m0;
	messages[1] = &m1;
	messages[2] = &m2;
	messages[3] = &m3;
	messages[4] = &m4;
	messages[5] = &m5;
	messages[6] = &m6;
	messages[7] = &m7;
	messages[8] = &m8;
	messages[9] = &m9;
	messages[10] = &m10;
	messages[11] = &EndMessage;

	Prolog();

	const char* suppress_following_blank = " ([{#@$%-`'\"";
	const char* suppress_preceding_blank = " )]},:;.'\"";

	int leading_space = 0;
	int trailing_space;

	for ( int i = 0; i < max_messages; ++i )
		{
		char c;

		if ( messages[i] == &EndMessage )
			break;

		if ( i == max_messages - 1 )
			trailing_space = 0;

		else
			{
			char c = messages[i+1]->FirstChar();
			if ( c && strchr( suppress_preceding_blank, c ) )
				trailing_space = 0;
			else
				trailing_space = 1;
			}

		c = messages[i]->Write( stream, leading_space, trailing_space );

		leading_space = ! (c && strchr( suppress_following_blank, c ));
		}

	Epilog();

	++count;
	}

void Reporter::Prolog()
	{
	if ( ! interactive )
		{
		if ( input_file_name )
			stream << "\"" << input_file_name << "\", ";

		if ( line_num > 0 )
			stream << "line " << line_num << ": ";
		}
	}

void Reporter::Epilog()
	{
	stream << "\n";
	}


void WarningReporter::Prolog()
	{
	Reporter::Prolog();
	stream << "warning, ";
	}


void ErrorReporter::Prolog()
	{
	Reporter::Prolog();
	stream << "error, ";
	}


void FatalReporter::Prolog()
	{
	Reporter::Prolog();
	stream << "fatal internal error, ";
	}

void FatalReporter::Epilog()
	{
	Reporter::Epilog();
	exit( 1 );
	}


void MessageReporter::Prolog()
	{
	}


void init_reporters()
	{
	warn = new WarningReporter;
	error = new ErrorReporter;
	fatal = new FatalReporter;
	message = new MessageReporter;
	}
