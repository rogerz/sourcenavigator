// $Header$

#ifndef value_h
#define value_h

#include "Glish/Dict.h"
#include "Glish/glish.h"
#include "Glish/GlishType.h"
#include "Glish/Object.h"
#include "Glish/Complex.h"

// Different types of values: constant references, references, and
// ordinary (non-indirect) values.
typedef enum { VAL_CONST, VAL_REF, VAL_VAL } value_type;

// Different types of storage for an array used to construct a Value.
typedef enum {
	COPY_ARRAY,		// copy the array
	TAKE_OVER_ARRAY,	// use the array, delete it when done with it
	PRESERVE_ARRAY		// use the array, don't delete it or grow it
	} array_storage_type;


class Value;
class Agent;
class Func;
class ArithExpr;
class RelExpr;
struct complex;
struct dcomplex;
struct record_header;	// Needed when dealing with SDS; see AddToSds()

typedef const char* charptr;
typedef Func* funcptr;
typedef Agent* agentptr;

declare(PList,Value);
typedef PList(Value) value_list;

declare(PDict,Value);
typedef PDict(Value)* recordptr;

typedef recordptr attributeptr;

typedef const Value* const_value;
declare(List,const_value);
typedef List(const_value) const_value_list;

// Classes for subvector references.
class VecRef;
#define SubVecRef(type) name2(type,SubVecRef)
class SubVecRef(glish_bool);
class SubVecRef(byte);
class SubVecRef(short);
class SubVecRef(int);
class SubVecRef(float);
class SubVecRef(double);
class SubVecRef(complex);
class SubVecRef(dcomplex);
class SubVecRef(charptr);

typedef SubVecRef(glish_bool) glish_boolref;
typedef SubVecRef(byte) byteref;
typedef SubVecRef(short) shortref;
typedef SubVecRef(int) intref;
typedef SubVecRef(float) floatref;
typedef SubVecRef(double) doubleref;
typedef SubVecRef(complex) complexref;
typedef SubVecRef(dcomplex) dcomplexref;
typedef SubVecRef(charptr) charptrref;


// Class used to differentiate integers that are SDS indices from
// just plain integers.
class SDS_Index {
public:
	SDS_Index( int ind )	{ index = ind; }
	int Index() const	{ return index; }

protected:
	int index;
	};


// Used to create lists of objects or dynamic memory that should be freed.
class DelObj;
declare(PList,DelObj);
typedef PList(DelObj) del_list;


#define copy_array(src,dest,len,type) \
	memcpy( (void*) dest, (void*) src, sizeof(type) * len )

#define copy_values(src,type) \
	copy_array( src, (void *) new type[len], length, type )

extern Value* copy_value( const Value* value );

extern const Value* false_value;
extern Value* empty_value();
extern Value* error_value();

extern Value* create_record();
extern recordptr create_record_dict();
void delete_record( recordptr r );


// The number of Value objects created and deleted.  Useful for tracking
// down inefficiencies and leaks.
extern int num_Values_created;
extern int num_Values_deleted;


class Value : public GlishObject {
public:
	Value( glish_bool value );
	Value( byte value );
	Value( short value );
	Value( int value );
	Value( float value );
	Value( double value );
	Value( complex value );
	Value( dcomplex value );
	Value( const char* value );
	Value( funcptr value );

	Value( agentptr value );

	Value( recordptr value, Agent* agent = 0 );

	Value( SDS_Index& sds_index );

	// Reference constructor.
	Value( Value* ref_value, value_type val_type );

	// Subref constructor.
	Value( Value* ref_value, int index[], int num_elements,
		value_type val_type );

	Value( glish_bool value[], int num_elements,
		array_storage_type storage = TAKE_OVER_ARRAY );
	Value( byte value[], int num_elements,
		array_storage_type storage = TAKE_OVER_ARRAY );
	Value( short value[], int num_elements,
		array_storage_type storage = TAKE_OVER_ARRAY );
	Value( int value[], int num_elements,
		array_storage_type storage = TAKE_OVER_ARRAY );
	Value( float value[], int num_elements,
		array_storage_type storage = TAKE_OVER_ARRAY );
	Value( double value[], int num_elements,
		array_storage_type storage = TAKE_OVER_ARRAY );
	Value( complex value[], int num_elements,
		array_storage_type storage = TAKE_OVER_ARRAY );
	Value( dcomplex value[], int num_elements,
		array_storage_type storage = TAKE_OVER_ARRAY );
	Value( charptr value[], int num_elements,
		array_storage_type storage = TAKE_OVER_ARRAY );
	Value( funcptr value[], int num_elements,
		array_storage_type storage = TAKE_OVER_ARRAY );

	Value( glish_boolref& value_ref );
	Value( byteref& value_ref );
	Value( shortref& value_ref );
	Value( intref& value_ref );
	Value( floatref& value_ref );
	Value( doubleref& value_ref );
	Value( complexref& value_ref );
	Value( dcomplexref& value_ref );
	Value( charptrref& value_ref );


	// Discard present value and instead take new_value.
	void TakeValue( Value* new_value );

	~Value();


	// A value manager is an object that is Unref()'d upon destruction
	// of a Value in lieu of deleting the Value's "values" member
	// variable.  Presumably the value manager knows something about
	// "values" and when deleted performs some sort of cleanup on it
	// (perhaps by deleting it, perhaps by changing its reference count,
	// perhaps by leaving it alone, etc.).
	void SetValueManager( GlishObject* manager )
		{ value_manager = manager; }


	glish_type Type() const			{ return type; }
	unsigned int Length() const
		{
		if ( Type() == TYPE_RECORD )
			return RecordPtr()->Length();
		else if ( IsRef() )
			return Deref()->Length();
		else
			return length;
		}

	// True if the value is a reference.
	int IsRef() const
		{ return type == TYPE_REF || type == TYPE_CONST; }

	// True if the value is a constant reference.
	int IsConst() const
		{ return type == TYPE_CONST; }

	// True if the value is a sub-vector reference.
	int IsVecRef() const
		{ return type == TYPE_SUBVEC_REF || type == TYPE_SUBVEC_CONST; }

	// True if the value makes sense as a numeric type (i.e.,
	// bool, integer, or floating-point).
	int IsNumeric() const;

	// True if the value is a record corresponding to a agent.
	int IsAgentRecord() const;

	// Returns the "n"'th element coereced to the corresponding type.
	glish_bool BoolVal( int n = 1 ) const;
	byte ByteVal( int n = 1 ) const;
	short ShortVal( int n = 1 ) const;
	int IntVal( int n = 1 ) const;
	float FloatVal( int n = 1 ) const;
	double DoubleVal( int n = 1 ) const;
	complex ComplexVal( int n = 1 ) const;
	dcomplex DcomplexVal( int n = 1 ) const;

	// Returns the entire value converted to a single string, with
	// "sep" used to separate array elements.  If "use_attr" is
	// true, then the value's attributes are used for determining
	// its shape (as a n-D array).
	//
	// Returns a new string, which should be delete'd when done with.
	char* StringVal( char sep = ' ', int use_attr = 0 ) const;

	// Returns the agent or function corresponding to the Value.
	Agent* AgentVal() const;
	funcptr FuncVal() const;

	// Returns the Value's SDS index, if TYPE_OPAQUE.  Returns
	// SDS_NO_SUCH_SDS if the value is not TYPE_OPAQUE.
	int SDS_IndexVal() const;


	// The following accessors return pointers to the underlying value
	// array.  The "const" versions complain with a fatal error if the
	// value is not the given type.  The non-const versions first
	// Polymorph() the values to the given type.  If called for a
	// subref, retrieves the complete underlying value, not the
	// just selected subelements.  (See the XXXRef() functions below.)
	glish_bool* BoolPtr() const;
	byte* BytePtr() const;
	short* ShortPtr() const;
	int* IntPtr() const;
	float* FloatPtr() const;
	double* DoublePtr() const;
	complex* ComplexPtr() const;
	dcomplex* DcomplexPtr() const;
	charptr* StringPtr() const;
	funcptr* FuncPtr() const;
	agentptr* AgentPtr() const;
	recordptr RecordPtr() const;

	glish_bool* BoolPtr();
	byte* BytePtr();
	short* ShortPtr();
	int* IntPtr();
	float* FloatPtr();
	double* DoublePtr();
	complex* ComplexPtr();
	dcomplex* DcomplexPtr();
	charptr* StringPtr();
	funcptr* FuncPtr();
	agentptr* AgentPtr();
	recordptr RecordPtr();

	Value* RefPtr() const		{ return (Value*) values; }

	// The following accessors are for accessing sub-array references.
	// They complain with a fatal error if the value is not a sub-array
	// reference.  Otherwise they return a reference to the underlying
	// *sub*elements. The "const" versions complain with a fatal error
	// if the value is not the given type.  The non-const versions 
	// first Polymorph() the values to the given type.
	glish_boolref& BoolRef() const;
	byteref& ByteRef() const;
	shortref& ShortRef() const;
	intref& IntRef() const;
	floatref& FloatRef() const;
	doubleref& DoubleRef() const;
	complexref& ComplexRef() const;
	dcomplexref& DcomplexRef() const;
	charptrref& StringRef() const;

	glish_boolref& BoolRef();
	byteref& ByteRef();
	shortref& ShortRef();
	intref& IntRef();
	floatref& FloatRef();
	doubleref& DoubleRef();
	complexref& ComplexRef();
	dcomplexref& DcomplexRef();
	charptrref& StringRef();

	VecRef* VecRefPtr() const	{ return (VecRef*) values; }

	// Follow the reference chain of a non-constant or constant value
	// until finding its non-reference base value.
	Value* Deref();
	const Value* Deref() const;

	Value* VecRefDeref();
	const Value* VecRefDeref() const;

	// Return a copy of the Value's contents coerced to an array
	// of the given type.  If the Value has only one element then
	// "size" copies of that element are returned (this is used
	// for promoting scalars to arrays in operations that mix
	// the two).  Otherwise, the first "size" elements are coerced
	// and returned.
	//
	// If the value cannot be coerced to the given type then a nil
	// pointer is returned.
	glish_bool* CoerceToBoolArray( int& is_copy, int size,
		glish_bool* result = 0 ) const;
	byte* CoerceToByteArray( int& is_copy, int size,
			byte* result = 0 ) const;
	short* CoerceToShortArray( int& is_copy, int size,
			short* result = 0 ) const;
	int* CoerceToIntArray( int& is_copy, int size,
			int* result = 0 ) const;
	float* CoerceToFloatArray( int& is_copy, int size,
			float* result = 0 ) const;
	double* CoerceToDoubleArray( int& is_copy, int size,
			double* result = 0 ) const;
	complex* CoerceToComplexArray( int& is_copy, int size,
			complex* result = 0 ) const;
	dcomplex* CoerceToDcomplexArray( int& is_copy, int size,
			dcomplex* result = 0 ) const;
	charptr* CoerceToStringArray( int& is_copy, int size,
			charptr* result = 0 ) const;

	// These coercions are very limited: they essentially either
	// return the corresponding xxxPtr() (if the sizes match,
	// no "result" is prespecified, and the Value is already
	// the given type) or generate a fatal error.
	funcptr* CoerceToFuncArray( int& is_copy, int size,
			funcptr* result = 0 ) const;

	// The following both return a new value.
	Value* operator[]( const Value* index ) const;
	Value* operator[]( const_value_list *index ) const;

	// Pick distinct elements from an array.
	Value* Pick( const Value* index ) const;

	// Return a reference to distinct elements from an array.
	Value* PickRef( const Value* index );

	// Assign to distinct array elements.
	void PickAssign( const Value* index, Value *value );

	// Return a true sub-array reference.
	Value* SubRef( const Value* index );
	Value* SubRef( const_value_list *args_val );

	Value* RecordRef( const Value* index ) const;	// returns a new Value

	// Returns an (unmodifiable) existing Value, or false_value if the
	// given field does not exist.
	const Value* ExistingRecordElement( const Value* index ) const;
	const Value* ExistingRecordElement( const char field[] ) const;

	// Returns a modifiable existing Value.  If the given field does
	// not exist, it is added, with an initial value of F.
	Value* GetOrCreateRecordElement( const Value* index );
	Value* GetOrCreateRecordElement( const char field[] );

	// Returns the given record element if it exists, 0 otherwise.
	// (The value must already have been tested to determine that it's
	// a record.)
	Value* HasRecordElement( const char field[] ) const;

	// Returns a modifiable existing Value, or if no field exists
	// with the given name, returns 0.
	Value* Field( const Value* index );
	Value* Field( const char field[] );

	// Returns the given field, polymorphed to the given type.
	Value* Field( const char field[], glish_type t );

	// Returns a modifiable existing Value of the nth field of a record,
	// with the first field being numbered 1.  Returns 0 if the field
	// does not exist (n is out of range) or the Value is not a record.
	Value* NthField( int n );
	const Value* NthField( int n ) const;

	// Returns a non-modifiable pointer to the nth field's name.
	// Returns 0 in the same cases as NthField does.
	const char* NthFieldName( int n ) const;

	// Returns a copy of a unique field name (one not already present)
	// for the given record, or 0 if the Value is not a record.
	//
	// The name has an embedded '*' to avoid collision with user-chosen
	// names.
	char* NewFieldName();

	// Returns a pointer to the underlying values of the given field,
	// polymorphed to the indicated type.  The length of the array is
	// returned in "len".  A nil pointer is returned the Value is not
	// a record or if it doesn't contain the given field.
	glish_bool* FieldBoolPtr( const char field[], int& len );
	byte* FieldBytePtr( const char field[], int& len );
	short* FieldShortPtr( const char field[], int& len );
	int* FieldIntPtr( const char field[], int& len );
	float* FieldFloatPtr( const char field[], int& len );
	double* FieldDoublePtr( const char field[], int& len );
	complex* FieldComplexPtr( const char field[], int& len );
	dcomplex* FieldDcomplexPtr( const char field[], int& len );
	charptr* FieldStringPtr( const char field[], int& len );

	// Looks for a field with the given name.  If present, returns true,
	// and in the second argument the scalar value corresponding to that
	// field polymorphed to the appropriate type.  If not present, returns
	// false.  The optional third argument specifies which element of a
	// multi-element value to return (not applicable when returning a
	// string).
	int FieldVal( const char field[], glish_bool& val, int n = 1 );
	int FieldVal( const char field[], byte& val, int n = 1 );
	int FieldVal( const char field[], short& val, int n = 1 );
	int FieldVal( const char field[], int& val, int n = 1 );
	int FieldVal( const char field[], float& val, int n = 1 );
	int FieldVal( const char field[], double& val, int n = 1 );
	int FieldVal( const char field[], complex& val, int n = 1 );
	int FieldVal( const char field[], dcomplex& val, int n = 1 );

	// Returns a new string in "val".
	int FieldVal( const char field[], char*& val );


	// The following SetField member functions take a field name and
	// arguments for creating a numeric or string Value.  The target
	// Value must be a record or a fatal error is generated.  A new
	// Value is constructing given the arguments and assigned to the
	// given field.

	void SetField( const char field[], glish_bool value );
	void SetField( const char field[], byte value );
	void SetField( const char field[], short value );
	void SetField( const char field[], int value );
	void SetField( const char field[], float value );
	void SetField( const char field[], double value );
	void SetField( const char field[], complex value );
	void SetField( const char field[], dcomplex value );
	void SetField( const char field[], const char* value );

	void SetField( const char field[], glish_bool value[], int num_elements,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetField( const char field[], byte value[], int num_elements,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetField( const char field[], short value[], int num_elements,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetField( const char field[], int value[], int num_elements,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetField( const char field[], float value[], int num_elements,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetField( const char field[], double value[], int num_elements,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetField( const char field[], complex value[], int num_elements,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetField( const char field[], dcomplex value[], int num_elements,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetField( const char field[], charptr value[], int num_elements,
			array_storage_type storage = TAKE_OVER_ARRAY );

	void SetField( const char field[], Value* value )
		{ AssignRecordElement( field, value ); }


	// General assignment of "this[index] = value", where "this" might
	// be a record or an array type.  Second form is for n-D arrays.
	void AssignElements( const Value* index, Value* value );
	void AssignElements( const_value_list* index, Value* value );

	// Assigns the elements of the value parameter to the corresponding 
	// elements of this.
	void AssignElements( Value* value );

	// Assigns a single record element to the given value.  Note
	// that the value may or may not wind up being copied (depending
	// on whether the record element is a reference or not).  The
	// caller should "Unref( xxx )" after the call, where "xxx"
	// is either "value" or some larger value of which "value" is
	// an element.  If AssignRecordElement needs the value to stick
	// around, it will have bumped its reference count.
	void AssignRecordElement( const char* index, Value* value );

	void Negate();	// value <- -value
	void Not();	// value <- ! value

	void ByteOpCompute( const Value* value, int lhs_len, ArithExpr* expr );
	void ShortOpCompute( const Value* value, int lhs_len, ArithExpr* expr );
	void IntOpCompute( const Value* value, int lhs_len, ArithExpr* expr );
	void FloatOpCompute( const Value* value, int lhs_len, ArithExpr* expr );
	void DoubleOpCompute( const Value* value, int lhs_len,
				ArithExpr* expr );
	void ComplexOpCompute( const Value* value, int lhs_len,
				ArithExpr* expr );
	void DcomplexOpCompute( const Value* value, int lhs_len,
				ArithExpr* expr );


	// Add the Value to the sds designated by "sds" using the given
	// name.  "dlist" is a del_list (PList of DelObj) that is used to
	// record any objects or dynamic memory required by AddToSds in order
	// to construct the SDS.  Once done with the SDS (and the SDS has
	// been destroyed), "delete_list( dlist )" should be called to
	// reclaim the memory that AddToSds needed.
	//
	// The "rh" argument is a pointer to the SDS record header
	// describing the record we're inside.  If this Value object is
	// not part of some larger record, then in the AddToSds call "rh"
	// will be nil.  But if we're part of a record, then "rh" will
	// give us a pointer to the SDS data structure corresponding to
	// that record.
	//
	// Additionally, "level" indicates how deep we are into a
	// record.  A level of 0 indicates we're at the top-level; 1
	// indicates we're dealing with a subrecord, 2 a subsubrecord,
	// etc.  We need to know this information because we do different
	// things for the cases level=0, level=1, and level=n for n > 1.
	void AddToSds( int sds, del_list* dlist, const char* name = 0,
			struct record_header* rh = 0, int level = 0 ) const;


	// Change from present type to given type.
	void Polymorph( glish_type new_type );
	void VecRefPolymorph( glish_type new_type );

	// Retrieve the non-modifiable set of attributes, possibly nil.
	const attributeptr AttributePtr() const
		{
		return attributes ? attributes->RecordPtr() : 0;
		}

	// Retrieve a modifiable, non-nil set of attributes.
	attributeptr ModAttributePtr()
		{
		InitAttributes();
		return attributes->RecordPtr();
		}

	// Retrieve a copy of a (possibly nil) attribute set.
	Value* CopyAttributePtr() const
		{
		return attributes ? copy_value( attributes ) : 0;
		}

	// Returns an (unmodifiable) existing Value, or false_value if the
	// given attribute does not exist.
	const Value* ExistingAttribute( const Value* index ) const
		{
		return attributes ?
			attributes->ExistingRecordElement( index ) :
			false_value;
		}
	const Value* ExistingAttribute( const char attribute[] ) const
		{
		return attributes ?
			attributes->ExistingRecordElement( attribute ) :
			false_value;
		}

	// Returns the given attribute if it exists, 0 otherwise.
	const Value* HasAttribute( const char attribute[] ) const
		{
                return attributes ?
			attributes->HasRecordElement( attribute ) : 0;
		}

	// Returns a new Value with the selected attributes.
	Value* AttributeRef( const Value* index ) const
		{
		return attributes ? attributes->RecordRef( index ) :
			new Value( glish_false );
		}

	// Returns a modifiable existing Value.  If the given field does
	// not exist, it is added, with an initial value of F.
	Value* GetOrCreateAttribute( const Value* index )
		{
		InitAttributes();
		return attributes->GetOrCreateRecordElement( index );
		}
	Value* GetOrCreateAttribute( const char field[] )
		{
		InitAttributes();
		return attributes->GetOrCreateRecordElement( field );
		}

	// Perform an assignment on an attribute value.
	void AssignAttribute( const char* index, Value* value )
		{
		InitAttributes();
		attributes->AssignRecordElement( index, value );
		}

	// Deletes a particular attribute. If the attribute does
	// not exist, NO action is performed.
	void DeleteAttribute( const Value* index );
	void DeleteAttribute( const char field[] );

	// Take new attributes from the given value.
	void CopyAttributes( const Value* value )
		{
		DeleteAttributes();
		attributes = value->CopyAttributePtr();
		}

	// Sets all of a Value's attributes (can be nil).
	void AssignAttributes( Value* a )
		{
		DeleteAttributes();
		attributes = a;
		}

	// Takes the attributes from a value without modifying them.
	Value* TakeAttributes()
		{
		Value* a = attributes;
		attributes = 0;
		return a;
		}

	void DescribeSelf( ostream& s ) const;

	// Provide the rudiments of copy on write... i.e. it copies
	// when necessary.
	//
	// Public to allow users to implement their own copy on write
	// until copy on write is a formal part of Glish.
	Value* CopyUnref()
		{
		if ( RefCount() == 1 )
			return this;
		else
			{
			Unref( this );	// Safe!
			Value* copy = copy_value( this );
			return copy;
			}
		}

protected:
	void SetValue( glish_bool array[], int len,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetValue( byte array[], int len,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetValue( short array[], int len,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetValue( int array[], int len,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetValue( float array[], int len,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetValue( double array[], int len,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetValue( complex array[], int len,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetValue( dcomplex array[], int len,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetValue( const char* array[], int len,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetValue( agentptr array[], int len,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetValue( funcptr array[], int len,
			array_storage_type storage = TAKE_OVER_ARRAY );
	void SetValue( recordptr value, Agent* agent );
	void SetValue( SDS_Index& array );

	void SetValue( glish_boolref& value_ref );
	void SetValue( byteref& value_ref );
	void SetValue( shortref& value_ref );
	void SetValue( intref& value_ref );
	void SetValue( floatref& value_ref );
	void SetValue( doubleref& value_ref );
	void SetValue( complexref& value_ref );
	void SetValue( dcomplexref& value_ref );
	void SetValue( charptrref& value_ref );

	void SetValue( Value *ref_value, int index[], int num_elements, 
			value_type val_type );

	virtual void SetType( glish_type new_type );

	void DeleteValue();
	void DeleteAttributes();

	void InitValue();
	void InitRecord( recordptr r, Agent* agent = 0 );

	void InitAttributes()
		{
		if ( ! attributes )
			attributes = create_record();
		}

	// Given an array index value, returns an array of integers
	// listing those elements indicated by the index.  Returns
	// nil if the index was invalid.  "num_indices" is updated
	// with the number of indices returned; "indices_are_copy"
	// indicates that the indices should be deleted once done
	// with using them.
	int* GenerateIndices( const Value* index, int& num_indices,
				int& indices_are_copy,
				int check_size = 1 ) const;

	// Return a new value holding the specified subelement(s).
	Value* ArrayRef( int* indices, int num_indices ) const;

	// Return a new value holding a reference the specified subelement(s).
	Value* TrueArrayRef( int* indices, int num_indices ) const;

	// Returns a slice of a record at the given indices.
	Value* RecordSlice( int* indices, int num_indices ) const;

	// Assign the specified subelements to copies of the corresponding
	// values.
	void AssignRecordElements( const Value* index, Value* value );
	void AssignRecordSlice( Value* value, int* indices, int num_indices );
	void AssignArrayElements( Value* value, int* indices,
					int num_indices );

	// Assigns the elements from record parameter to the corresponding 
	// elements in this object.
	void AssignRecordElements( Value* value );

	// Copies the elements from the value parameter. It assumes
	// that the sizes are compatible, and generates a warning,
	// and copies a portion otherwise.
	void AssignArrayElements( Value* value );

	// Does the actual work of assigning a list of array elements,
	// once type-checking has been done.
	void AssignArrayElements( int* indices, int num_indices,
				Value* value, int rhs_len );

	// Searches a list of indices to find the largest and returns
	// it in "max_index".  If an invalid (< 1) index is found, a
	// error is generated and false is returned; otherwise true
	// is returned.
	int IndexRange( int* indices, int num_indices, int& max_index ) const;

	char* RecordStringVal() const; // returns a new string

	// Increase array size from present value to given size.
	// If successful, true is returned.  If this can't be done for
	// our present type, an error message is generated and false is return.
	int Grow( unsigned int new_size );

	glish_type type;

	unsigned int length;
	unsigned int max_size;
	void* values;
	array_storage_type storage;
	GlishObject* value_manager;
	Value* attributes;
	};

// We couldn't do this earlier, because some of the inline definitions
// for the VecRef class depend on inline Value functions (and vice versa).

#include "VecRef.h"


extern Value* bool_rel_op_compute( const Value* lhs, const Value* rhs,
				int lhs_len, RelExpr* expr );
extern Value* byte_rel_op_compute( const Value* lhs, const Value* rhs,
				int lhs_len, RelExpr* expr );
extern Value* short_rel_op_compute( const Value* lhs, const Value* rhs,
				int lhs_len, RelExpr* expr );
extern Value* int_rel_op_compute( const Value* lhs, const Value* rhs,
				int lhs_len, RelExpr* expr );
extern Value* float_rel_op_compute( const Value* lhs, const Value* rhs,
				int lhs_len, RelExpr* expr );
extern Value* double_rel_op_compute( const Value* lhs, const Value* rhs,
				int lhs_len, RelExpr* expr );
extern Value* complex_rel_op_compute( const Value* lhs, const Value* rhs,
				int lhs_len, RelExpr* expr );
extern Value* dcomplex_rel_op_compute( const Value* lhs, const Value* rhs,
				int lhs_len, RelExpr* expr );
extern Value* string_rel_op_compute( const Value* lhs, const Value* rhs,
				int lhs_len, RelExpr* expr );

extern Value* read_value_from_SDS( int sds, int is_opaque_sds = 0 );

extern int compatible_types( const Value* v1, const Value* v2,
				glish_type& max_type );

extern void init_values();

extern void delete_list( del_list* dlist );

// The following convert a string to integer/double/dcomplex.  They
// set successful to return true if the conversion was successful,
// false if the text does not describe a valid integer/double/dcomplex.
extern int text_to_integer( const char text[], int& successful );
extern double text_to_double( const char text[], int& successful );
extern dcomplex text_to_dcomplex( const char text[], int& successful );

#endif /* value_h */
