// $Header$

#include "system.h"

#include <stdlib.h>
#include <stream.h>

#include "Glish/List.h"


// Print message on stderr and exit.
void default_error_handler(char* s)
	{
	cerr << s << "\n";
	exit(1);
	}

PFC BaseList::set_error_handler(PFC handler)
	{
	PFC old = error_handler;

	if ( handler == 0 )
		error_handler = default_error_handler;
	else
		error_handler = handler;

	return old;
	}

BaseList::BaseList(int size, PFC handler)
	{
	const int DEFAULT_CHUNK_SIZE = 10;

	if ( size <= 0 )
		chunk_size = DEFAULT_CHUNK_SIZE;

	if ( size < 0 )
		{
		num_entries = max_entries = 0;
		entry = 0;
		}
	else
		{
		num_entries = 0;
		if ( (entry = new ent[chunk_size]) )
			max_entries = chunk_size;
		else
			max_entries = 0;
		}

	if ( handler == 0 )
		error_handler = default_error_handler;
	else
		error_handler = handler;
	}


BaseList::BaseList(BaseList& b)
	{
	max_entries = b.max_entries;
	chunk_size = b.chunk_size;
	num_entries = b.num_entries;
	error_handler = b.error_handler;

	if ( max_entries )
		entry = new ent[max_entries];
	else
		entry = 0;

	for ( int i = 0; i < num_entries; i++ )
		entry[i] = b.entry[i];
	}

void BaseList::operator=(BaseList& b)
	{
	if ( this == &b )
		return;	// i.e., this already equals itself

	delete entry;

	max_entries = b.max_entries;
	chunk_size = b.chunk_size;
	num_entries = b.num_entries;
	error_handler = b.error_handler;

	if ( max_entries )
		entry = new ent[max_entries];
	else
		entry = 0;

	for ( int i = 0; i < num_entries; i++ )
		entry[i] = b.entry[i];
	}

void BaseList::insert(ent a)
	{
	if ( num_entries == max_entries )
		{
		resize(max_entries+chunk_size);	// make more room
		chunk_size *= 2;
		}

	for ( int i = num_entries; i > 0; i-- )	
		entry[i] = entry[i-1];	// move all pointers up one

	num_entries++;
	entry[0] = a;
	}

ent BaseList::remove(ent a)
	{
	for ( int i = 0; i < num_entries && a != entry[i]; i++ )
		;

	return remove_nth(i);
	}

ent BaseList::remove_nth(int n)
	{
	if ( n < 0 || n >= num_entries )
		return 0;

	ent old_ent = entry[n];
	--num_entries;

	for ( ; n < num_entries; n++ )
		entry[n] = entry[n+1];

	entry[n] = 0;	// for debugging
	return old_ent;
	}

void BaseList::append(ent a)
	{
	if ( num_entries == max_entries )
		{
		resize(max_entries+chunk_size);	// make more room
		chunk_size *= 2;
		}

	entry[num_entries++] = a;
	}

// Get and remove from the end of the list.
ent BaseList::get()
	{
	if ( num_entries == 0 )
		{
		error_handler("get from empty BaseList");
		return 0;
		}

	return entry[--num_entries];
	}


void BaseList::clear()
	{
	delete entry;
	entry = 0;
	num_entries = max_entries = 0;
	}

ent BaseList::operator[](int i) const
	{
	if ( i < 0 || i > num_entries-1 )
		{
		return 0;
		}
	else
		return entry[i];
	}

ent BaseList::replace(int ent_index,ent new_ent)
	{
	if ( ent_index < 0 || ent_index > num_entries-1 )
		{
		return 0;
		}
	else
		{
		ent old_ent = entry[ent_index];
		entry[ent_index] = new_ent;
		return old_ent;
		}
	}

int BaseList::resize(int new_size)
	{
	if ( new_size < num_entries )
		new_size = num_entries;	// do not lose any entries

	if ( new_size != num_entries )
		{
		entry = (ent*) realloc_memory( (void*) entry,
					sizeof( ent ) * new_size );
		if ( entry == 0 )
			{
			}

		if ( new_size > max_entries )
			max_entries = new_size;
		}

	return max_entries;
	}

ent BaseList::is_member(ent e) const
	{
	for ( int i = 0; i < length() && e != entry[i]; i++ )
		;

	return (i == length()) ? 0 : e;
	}
