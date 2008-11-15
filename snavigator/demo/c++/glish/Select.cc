// $Header$

#include "system.h"

#include <stdio.h>
#include <osfcn.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/resource.h>

#ifdef HAVE_X11_FD_H
#include <X11/fd.h>
#endif

#ifdef SOLARIS
#include <sys/time.h>
#include <string.h>
extern "C" int gettimeofday( struct timeval *, struct timezone * );
#endif

#include "Select.h"


void gripe( const char* msg );


// Increment a timeval by the given amount.
void increment_time( struct timeval& t, const struct timeval& incr )
	{
	t.tv_sec += incr.tv_sec;
	t.tv_usec += incr.tv_usec;

	while ( t.tv_usec >= 1000000 )
		{
		++t.tv_sec;
		t.tv_usec -= 1000000;
		}
	}

// Decrement a timeval by the given amount.
void decrement_time( struct timeval& t, const struct timeval& decr )
	{
	t.tv_sec -= decr.tv_sec;
	t.tv_usec -= decr.tv_usec;

	while ( t.tv_usec < 0 )
		{
		--t.tv_sec;
		t.tv_usec += 1000000;
		}
	}

// Returns true if t1 is chronologically less than t2, false otherwise.
int time_less_than( const struct timeval& t1, const struct timeval& t2 )
	{
	return t1.tv_sec < t2.tv_sec ||
		(t1.tv_sec == t2.tv_sec && t1.tv_usec < t2.tv_usec);
	}


Selectee::~Selectee()
	{
	}


SelectTimer::SelectTimer( struct timeval* delta, struct timeval* interval )
	{
	Init( delta, interval );
	}

SelectTimer::SelectTimer( long delta_sec, long delta_usec,
			long interval_sec, long interval_usec )
	{
	struct timeval delta;
	delta.tv_sec = delta_sec;
	delta.tv_usec = delta_usec;

	struct timeval interval;
	interval.tv_sec = interval_sec;
	interval.tv_usec = interval_usec;

	Init( &delta, &interval );
	}

SelectTimer::~SelectTimer()
	{
	}


void SelectTimer::Init( struct timeval* delta, struct timeval* interval )
	{
	if ( gettimeofday( &exp_t, (struct timezone *) 0 ) < 0 )
		gripe( "gettimeofday failed" );

	increment_time( exp_t, *delta );

	if ( interval )
		interval_t = *interval;
	else
		interval_t.tv_sec = interval_t.tv_usec = 0;
	}

int SelectTimer::Expired()
	{
	if ( DoExpiration() &&
	     (interval_t.tv_sec > 0 || interval_t.tv_usec > 0) )
		{
		increment_time( exp_t, interval_t );
		return 1;
		}

	return 0;
	}

int SelectTimer::DoExpiration()
	{
	return 1;
	}


Selector::Selector()
	{
#ifdef HAVE_SETRLIMIT
	struct rlimit rl;
	if ( getrlimit( RLIMIT_NOFILE, &rl ) < 0 )
		gripe( "getrlimit() failed" );

	max_num_fds = int( rl.rlim_max );
#else
	max_num_fds = 32;
#endif

	selectees = new Selectee* [max_num_fds];

	for ( int i = 0; i < max_num_fds; ++i )
		selectees[i] = 0;

	current_selectee = 0;
	nuke_current_selectee = 0;

	fdset = new fd_set;
	FD_ZERO( fdset );
	}

Selector::~Selector()
	{
	for ( int i = 0; i < max_num_fds; ++i )
		delete selectees[i];

	delete selectees;
	delete fdset;
	}

void Selector::AddSelectee( Selectee* s )
	{
	selectees[s->FD()] = s;
	FD_SET( s->FD(), fdset );
	}

void Selector::DeleteSelectee( int selectee_fd )
	{
	if ( ! FD_ISSET( selectee_fd, fdset ) )
		gripe( "non-existent selectee in RemoveSelectee()" );

	Selectee* s = selectees[selectee_fd];

	if ( s == current_selectee )
		// Don't delete it right now, while it's in use, just
		// flag that we should do so when we're done with it.
		nuke_current_selectee = 1;

	else
		delete s;

	selectees[selectee_fd] = 0;
	FD_CLR( selectee_fd, fdset );
	}

Selectee* Selector::FindSelectee( int selectee_fd ) const
	{
	if ( ! FD_ISSET( selectee_fd, fdset ) )
		return 0;

	return selectees[selectee_fd];
	}

void Selector::AddTimer( SelectTimer* t )
	{
	timers.append( t );
	}

int Selector::DoSelection()
	{
	struct timeval* timeout = 0;
	struct timeval timeout_buf;
	struct timeval min_t;

	if ( timers.length() > 0 )
		{
		int have_min = 0;

		for ( int i = 0; i < timers.length(); ++i )
			{
			struct timeval timer_t = timers[i]->ExpirationTime();

			if ( ! have_min )
				{
				min_t = timer_t;
				have_min = 1;
				}

			else
				{
				if ( time_less_than( timer_t, min_t ) )
					min_t = timer_t;
				}
			}

		if ( ! have_min )
			gripe( "internal consistency problem" );

		struct timeval t;
		if ( gettimeofday( &t, (struct timezone *) 0 ) < 0 )
			gripe( "gettimeofday failed" );

		timeout = &timeout_buf;
		*timeout = min_t;

		// Convert the timeout to a delta from the current time.

		if ( time_less_than( *timeout, t ) )
			{ // Don't decrement the timeout, it'll go negative.
			timeout->tv_sec = timeout->tv_usec = 0;
			}

		else
			decrement_time( *timeout, t );
		}

	fd_set read_mask = *fdset;
	int status;

	if ( (status =
	      select( FD_SETSIZE, &read_mask, (fd_set *) 0, (fd_set *) 0,
		      timeout )) < 0 )
		{
		if ( errno != EINTR )
			gripe( "error in DoSelection()" );

		return 0;
		}

	if ( status == 0 )
		{ // Timeout expired.  Assume current time is min_t.
		for ( int i = 0; i < timers.length(); ++i )
			{
			struct timeval timer_t = timers[i]->ExpirationTime();
			if ( ! time_less_than( min_t, timer_t ) )
				// timer_t <= min_t
				if ( ! timers[i]->Expired() )
					{
					// Timer is now inactive.
					timers.remove_nth( i );
					--i;	// because loop's about to ++
					}
			}
		}

	for ( int i = 0; status > 0 && i < max_num_fds; ++i )
		{
		if ( FD_ISSET( i, &read_mask ) )
			{
			current_selectee = selectees[i];
			nuke_current_selectee = 0;

			int selectee_value =
				current_selectee->NotifyOfSelection();

			if ( nuke_current_selectee )
				delete current_selectee;

			current_selectee = 0;

			if ( selectee_value )
				return selectee_value;

			--status;
			}
		}

	if ( status != 0 )
		gripe( "inconsistency in DoSelection()" );

	return 0;
	}

void gripe( const char* msg )
	{
	fprintf( stderr, "Selector/Selectee/SelectTimer error: %s\n", msg );
	perror( "perror value" );
	exit( 1 );
	}
