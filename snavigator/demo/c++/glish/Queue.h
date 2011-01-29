// $Header$

#ifndef queue_h
#define queue_h

#include <generic.h>

class BaseQueue;

#define Queue(type) name2(type,Queue)
#define PQueue(type) name2(type,PQueue)

class QueueElement {
    protected:
    friend BaseQueue;
	QueueElement( void* element )
		{ elem = element; next = 0; }
	QueueElement* next;
	void* elem;
	};

class BaseQueue {
    public:
	BaseQueue();
	void EnQueue( void* element );
	void* DeQueue();

    protected:
	QueueElement* head;
	QueueElement* tail;
	};

#define Queuedeclare(type)						\
	class Queue(type) : public BaseQueue {				\
	    public:							\
		void EnQueue( type element )				\
			{ BaseQueue::EnQueue( (void*) element ); }	\
		type DeQueue()						\
			{ return (type) BaseQueue::DeQueue(); }		\
		}

#define PQueuedeclare(type)						\
	class PQueue(type) : public BaseQueue {				\
	    public:							\
		void EnQueue( type* element )				\
			{ BaseQueue::EnQueue( (void*) element ); }	\
		type* DeQueue()						\
			{ return (type*) BaseQueue::DeQueue(); }	\
		}

#endif	/* queue_h */
