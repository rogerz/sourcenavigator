// $Header$

#ifndef object_h
#define object_h


// GlishObject is the root of the class hierarchy.  GlishObjects know how to
// describe themselves.

// Line number to associate with newly created objects..
extern int line_num;

class ostream;

class GlishObject {
    public:
	GlishObject()		{ line = line_num; ref_count = 1; }
	virtual ~GlishObject()	{ }

	int Line()		{ return line; }

	// Return the ref count so other classes can do intelligent copying.
	int RefCount() const	{ return ref_count; }

	// Generate a long description of the object to the
	// given stream.  This typically includes descriptions of
	// subobjects as well as this object.
	virtual void Describe( ostream& ) const;

	// Generate a short description of the object to the
	// given stream.
	virtual void DescribeSelf( ostream& ) const;

	// Non-virtual, non-const versions of Describe() and DescribeSelf().
	// We add it here so that if when deriving a subclass of GlishObject we
	// forget the "const" declaration on the Describe/DescribeSelf
	// member functions, we'll hopefully get a warning message that
	// we're shadowing a non-virtual function.
	void Describe( ostream& stream )
		{ ((const GlishObject*) this)->Describe( stream ); }
	void DescribeSelf( ostream& stream )
		{ ((const GlishObject*) this)->DescribeSelf( stream ); }

    protected:
	friend inline void Ref( GlishObject* object );
	friend inline void Unref( GlishObject* object );

	const char* description;
	int line;
	int ref_count;
	};


inline void Ref( GlishObject* object )
	{
	++object->ref_count;
	}

inline void Unref( GlishObject* object )
	{
	if ( object && --object->ref_count == 0 )
		delete object;
	}

#endif	/* object_h */
