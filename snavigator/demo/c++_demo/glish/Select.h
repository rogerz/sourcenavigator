// $Header$

#ifndef select_h
#define select_h

#include "system.h"

#include <sys/time.h>
#include "Glish/List.h"

#if HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

class SelectTimer;

declare(PList,SelectTimer);
typedef PList(SelectTimer) timer_list;


struct fd_set;

class Selectee {
public:
	Selectee( int selectee_fd )	{ fd = selectee_fd; }
	virtual ~Selectee();

	int FD()	{ return fd; }

	// returns non-zero if the selection should stop, zero otherwise
	virtual int NotifyOfSelection()	{ return 0; }

protected:
	int fd;
	};


class SelectTimer {
public:
	// Creates a timer that expires "delta" seconds from now.
	// If "interval" is non-zero then after expiring the timer
	// will reset to expire after that many more seconds.
	SelectTimer( struct timeval* delta, struct timeval* interval = 0 );
	SelectTimer( long delta_sec, long delta_usec,
			long interval_sec = 0, long interval_usec = 0 );

	virtual ~SelectTimer();

	// Returns the timer's absolute expiration time.
	struct timeval ExpirationTime()			{ return exp_t; }

protected:
	friend class Selector;

	void Init( struct timeval* delta, struct timeval* interval );

	// Called by a Selector to indicate that the timer has expired.
	// Returns non-zero if the timer has reactivated itself, zero
	// if it is now inactive.
	int Expired();

	// Called to do whatever work is associated with the timer expiring.
	// Returns non-zero if the timer should reactive (ignored if the
	// interval value is itself zero), zero if the timer should become
	// inactive.
	virtual int DoExpiration();

	struct timeval exp_t;
	struct timeval interval_t;
	};

class Selector {
public:
	Selector();
	~Selector();

	void AddSelectee( Selectee* s );
	void DeleteSelectee( int selectee_fd );

	// Returns the Selectee associated with the given fd, or, if
	// none, returns 0.
	Selectee* FindSelectee( int selectee_fd ) const;

	void AddTimer( SelectTimer* t );

	// If selection stops early due to non-zero return from Selectee's
	// NotifyOfSelection(), returns that non-zero value.  Otherwise
	// returns 0.
	int DoSelection();

protected:
	int max_num_fds;
	Selectee** selectees;	// array indexed by fd

	Selectee* current_selectee;	// current selectee being notified

	// If true, delete selectee when notification done.
	int nuke_current_selectee;

	struct fd_set* fdset;
	timer_list timers;
	};

#endif	/* select_h */
