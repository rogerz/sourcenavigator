// $Header$

#include "Queue.h"


BaseQueue::BaseQueue()
	{
	head = tail = 0;
	}

void BaseQueue::EnQueue( void* element )
	{
	QueueElement* qe = new QueueElement( element );

	if ( ! head )
		head = tail = qe;

	else
		{
		tail->next = qe;
		tail = qe;
		}
	}

void* BaseQueue::DeQueue()
	{
	if ( ! head )
		return 0;

	QueueElement* qe = head;
	head = head->next;

	if ( qe == tail )
		tail = 0;

	void* result = qe->elem;
	delete qe;

	return result;
	}
