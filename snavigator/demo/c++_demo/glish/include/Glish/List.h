// $Header$

#ifndef list_h
#define list_h

// BaseList.h --
//	Interface for class BaseList, current implementation is as an
//	array of ent's.  This implementation was chosen to optimize
//	getting to the ent's rather than inserting and deleting.
//	Also pairs of append's and get's act like push's and pop's
//	and are very efficient.  The only really expensive operations
//	are inserting (but not appending), which requires pushing every
//	element up, and resizing the list, which involves getting new space
//	and moving the data.  Resizing occurs automatically when inserting
//	more elements than the list can currently hold.  Automatic
//	resizing is done one "chunk_size" of elements at a time and
//	always increases the size of the list.  Resizing to zero
//	(or to less than the current value of num_entries)
//	will decrease the size of the list to the current number of
//	elements.  Resize returns the new max_entries.
//
//	Entries must be either a pointer to the data or nonzero data with
//	sizeof(data) <= sizeof(void*).

#include <generic.h>
#include <stdarg.h>

typedef void* ent;
typedef void (*PFC)(char*);	// mostly for error handling
     
class BaseList {
    public:
	~BaseList()		{ clear(); }

	void clear();		// remove all entries
	int length() const	{ return num_entries; }
	int resize(int = 0);	// 0 => size to fit current number of entries

    protected:
	BaseList(int = 0, PFC = 0);
	BaseList(BaseList&);

	void insert(ent);	// add at head of list
	void append(ent);	// add to end of list
	ent remove(ent);	// delete entry from list
	ent remove_nth(int);	// delete nth entry from list
	ent get();		// return and remove ent at end of list

	// Return 0 if ent is not in the list, ent otherwise.
	ent is_member(ent) const;

	PFC set_error_handler(PFC = 0);	//reset to default handler if null arg
	ent replace(int, ent);	// replace entry #i with a new value

	// return nth ent of list (do not remove).
	ent operator[](int) const;

	void operator=(BaseList&);

	ent* entry;
	int chunk_size;		// increase size by this amount when necessary
	int max_entries;
	int num_entries;
	PFC error_handler;

	friend class BaseListIterator;
	};

class BaseListIterator {
    public:
	BaseListIterator(BaseList& l)	{ lp = &l; pos = 0; }

	void reset()			{ pos = 0; }
	ent operator()()	
		{
		return (pos < lp->num_entries) ? lp->entry[pos++] : 0;
		}

    protected:
	BaseList* lp;
	int pos;
	};


// List.h -- interface for class List
//	Use:	to get a list of pointers to class foo you should:
//		1) typedef foo* Pfoo; (the macros don't like explicit pointers)
//		2) declare(List,Pfoo); (declare an interest in lists of Pfoo's)
//		3) variables are declared like:
//				List(Pfoo) bar;	(bar is of type list of Pfoo's)
//				ListIterator(Pfoo) next(bar);

// For lists of "type".

// InterViews 3.0 defines List ...
#undef List
#define List(type)			name2(type,List)
#define ListIterator(type)	name2(type,ListIterator)

// For lists of pointers to "type"
#define PList(type)			name2(type,PList)
#define PListIterator(type)	name2(type,PListIterator)

#define Listdeclare(type)						\
struct List(type) : BaseList						\
	{								\
	List(type)(type ...);						\
	List(type)(PFC eh =0) : BaseList(0,eh) {}			\
	List(type)(int sz, PFC eh =0) : BaseList(sz,eh) {}		\
	List(type)(List(type)& l) : BaseList((BaseList&)l) {}		\
									\
	void operator=(List(type)& l)					\
		{ BaseList::operator=((BaseList&)l); }			\
	void insert(type a)	{ BaseList::insert(ent(a)); }		\
	void append(type a)	{ BaseList::append(ent(a)); }		\
	type remove(type a)						\
			{ return type(BaseList::remove(ent(a))); }	\
	type remove_nth(int n)	{ return type(BaseList::remove_nth(n)); }\
	type get()		{ return type(BaseList::get()); }	\
	type replace(int i, type new_type)				\
		{ return type(BaseList::replace(i,ent(new_type))); }	\
	type is_member(type e)						\
		{ return type(BaseList::is_member(ent(e))); }		\
	PFC set_error_handler(PFC eh =0)				\
		{ return BaseList::set_error_handler(eh); }		\
									\
	type operator[](int i) const					\
		{ return type(BaseList::operator[](i)); }		\
	};								\
									\
struct ListIterator(type) : BaseListIterator				\
	{								\
	ListIterator(type)(List(type)& l)				\
		: BaseListIterator((BaseList&)l) {}			\
	void reset()		{ BaseListIterator::reset(); }		\
	type operator()()						\
		{ return type(BaseListIterator::operator()()); }	\
	}

#define Listimplement(type)						\
List(type)::List(type)(type e1 ...) : BaseList()			\
	{								\
	append(e1);							\
	va_list ap;							\
	va_start(ap,e1);						\
	for ( type e = va_arg(ap,type); e != 0; e = va_arg(ap,type) )	\
		append(e);						\
	resize();							\
	}

#define PListdeclare(type)						\
struct PList(type) : BaseList						\
	{								\
	PList(type)(type* ...);						\
	PList(type)(PFC eh =0) : BaseList(0,eh) {}			\
	PList(type)(int sz, PFC eh =0) : BaseList(sz,eh) {}		\
	PList(type)(PList(type)& l) : BaseList((BaseList&)l) {}		\
									\
	void operator=(PList(type)& l)					\
		{ BaseList::operator=((BaseList&)l); }			\
	void insert(type* a)	{ BaseList::insert(ent(a)); }		\
	void append(type* a)	{ BaseList::append(ent(a)); }		\
	type* remove(type* a)						\
		{ return (type*)BaseList::remove(ent(a)); }		\
	type* remove_nth(int n)	{ return (type*)(BaseList::remove_nth(n)); }\
	type* get()		{ return (type*)BaseList::get(); }	\
	type* operator[](int i) const					\
		{ return (type*)(BaseList::operator[](i)); }		\
	type* replace(int i, type* new_type)				\
		{ return (type*)BaseList::replace(i,ent(new_type)); }	\
	type* is_member(type* e)					\
		{ return (type*)BaseList::is_member(ent(e)); }		\
	PFC set_error_handler(PFC eh =0)				\
		{ return BaseList::set_error_handler(eh); }		\
	};								\
									\
struct PListIterator(type) : BaseListIterator				\
	{								\
	PListIterator(type)(PList(type)& l)				\
		: BaseListIterator((BaseList&)l) {}			\
	void reset()		{ BaseListIterator::reset(); }		\
	type* operator()()						\
		{ return (type*)(BaseListIterator::operator()()); }	\
	}

#define PListimplement(type)						\
PList(type)::PList(type)(type* ep1 ...) : BaseList()			\
	{								\
	append(ep1);							\
	va_list ap;							\
	va_start(ap,ep1);						\
	for ( type* ep = va_arg(ap,type*); ep != 0;			\
	      ep = va_arg(ap,type*) )					\
		append(ep);						\
	resize();							\
	}


// Popular type of list: list of strings.
declare(PList,char);
typedef PList(char) name_list;

#endif /* list_h */
