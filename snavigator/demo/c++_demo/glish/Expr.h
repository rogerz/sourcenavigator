// $Header$

#ifndef expr_h
#define expr_h

#include "Glish/Dict.h"
#include "Glish/Value.h"

class Stmt;
class Expr;
class EventDesignator;
class Sequencer;
class ParameterPList;

declare(PList,Expr);
declare(PDict,Expr);

typedef PList(Expr) expr_list;


// Different scopes to use when resolving identifiers; used by the VarExpr
// and Sequencer classes.
typedef enum { LOCAL_SCOPE, GLOBAL_SCOPE } scope_type;

// Different types of expression evaluation: evaluate and return a
// modifiable copy of the result; evaluate and return a read-only
// version of the result (which will subsequently be released using
// Expr::ReadOnlyDone); or evaluate for side effects only, and return
// nothing.
typedef enum { EVAL_COPY, EVAL_READ_ONLY, EVAL_SIDE_EFFECTS } eval_type;


class Expr : public GlishObject {
    public:
	Expr( const char* desc )
		{ description = desc; }

	// Returns a copy of the present value of the event expression.
	// The caller is responsible for deleting the copy when done
	// using it.
	Value* CopyEval()
		{ return Eval( EVAL_COPY ); }

	// Returns a read-only copy (i.e., the original) of the present
	// value of the event expression.  The caller is responsible for
	// later calling ReadOnlyDone() when the copy is no longer needed.
	const Value* ReadOnlyEval()
		{ return Eval( EVAL_READ_ONLY ); }

	// Declares that the previously returned ReadOnlyEval() value
	// is no longer needed.
	void ReadOnlyDone( const Value* returned_value )
		{ Unref( (Value*) returned_value ); }


	// Returns the present value of the event expression.  If
	// "modifiable" is true then a modifiable version of the value is
	// returned; otherwise, a read-only copy.
	virtual Value* Eval( eval_type etype ) = 0;


	// Evaluates the Expr just for side-effects.
	virtual void SideEffectsEval();


	// Returns a reference to the value of the event expression.
	// If val_type is VAL_REF then a "ref" reference is returned,
	// otherwise a "const" reference.
	//
	// The reference should be Unref()'d once done using it.
	virtual Value* RefEval( value_type val_type );


	// Assigns a new value to the variable (LHS) corresponding
	// to this event expression, if appropriate.  The passed
	// value becomes the property of the Expr, which must
	// subsequently take care of garbage collecting it as necessary
	// (in particular, next time a value is Assign()'d, the value
	// should be deleted).
	//
	// Note that new_value can be nil (providing that index is nil,
	// too), in which case the old value
	// is deleted and the value set to nil.  Used for things like
	// formal parameters where it's desirable to free up the memory
	// used by their values as soon as the function call is complete,
	// rather than waiting for the next call to the function (and
	// subsequent assignment to the formal parameters).
	virtual void Assign( Value* new_value );

	// Returns true if, when evaluated as a statement, this expression's
	// value should be "invisible" - i.e., the statement's value is "no
	// value" (false).
	virtual int Invisible() const;

    protected:
	// Return either a copy of the given value, or a reference to
	// it, depending on etype.  If etype is EVAL_SIDE_EFFECTS, a
	// warning is generated and 0 returned.
	Value* CopyOrRefValue( const Value* value, eval_type etype );
	};


class VarExpr : public Expr {
    public:
	VarExpr( char* var_id, scope_type scope, int frame_offset,
			Sequencer* sequencer );

	~VarExpr();

	Value* Eval( eval_type etype );
	Value* RefEval( value_type val_type );

	void Assign( Value* new_value );

	const char* VarID()	{ return id; }

    protected:
	char* id;
	scope_type scope;
	int frame_offset;
	Sequencer* sequencer;
	};


class ValExpr : public Expr {
    public:
	ValExpr( Value *v ) : Expr("value"), val(v) { Ref(val); }

	~ValExpr() { Unref(val); }

	Value* Eval( eval_type etype );
	Value* RefEval( value_type val_type );

    protected:
	Value *val;
	};

class ConstExpr : public Expr {
    public:
	ConstExpr( const Value* const_value );

	Value* Eval( eval_type etype );
	void DescribeSelf( ostream& s ) const;

    protected:
	const Value* const_value;
	};


class UnaryExpr : public Expr {
    public:
	UnaryExpr( Expr* operand, const char* desc );

	Value* Eval( eval_type etype ) = 0;
	void Describe( ostream& s ) const;

    protected:
	Expr* op;
	};


class BinaryExpr : public Expr {
    public:
	BinaryExpr( Expr* op1, Expr* op2, const char* desc );

	Value* Eval( eval_type etype ) = 0;
	void Describe( ostream& s ) const;

    protected:
	Expr* left;
	Expr* right;
	};



class NegExpr : public UnaryExpr {
    public:
	NegExpr( Expr* operand );

	Value* Eval( eval_type etype );
	};


class NotExpr : public UnaryExpr {
    public:
	NotExpr( Expr* operand );

	Value* Eval( eval_type etype );
	};


class AssignExpr : public BinaryExpr {
    public:
	AssignExpr( Expr* op1, Expr* op2 );

	Value* Eval( eval_type etype );
	void SideEffectsEval();
	int Invisible() const;
	};


class OrExpr : public BinaryExpr {
    public:
	OrExpr( Expr* op1, Expr* op2 );

	Value* Eval( eval_type etype );
	};


class AndExpr : public BinaryExpr {
    public:
	AndExpr( Expr* op1, Expr* op2 );

	Value* Eval( eval_type etype );
	};


class ConstructExpr : public Expr {
    public:
	ConstructExpr( ParameterPList* args );

	Value* Eval( eval_type etype );
	void Describe( ostream& s ) const;

    protected:
	Value* BuildArray();
	Value* BuildRecord();

	int TypeCheck( const Value* values[], int num_values,
			glish_type& max_type );
	int MaxNumeric( const Value* values[], int num_values,
				glish_type& max_type );
	int AllEquivalent( const Value* values[], int num_values,
				glish_type& max_type );

	Value* ConstructArray( const Value* values[], int num_values,
				int total_length, glish_type max_type );

	int is_array_constructor;
	ParameterPList* args;
	};


class ArrayRefExpr : public UnaryExpr {
    public:
	ArrayRefExpr( Expr* op1, expr_list* a );

	Value* Eval( eval_type etype );
	Value* RefEval( value_type val_type );

	void Assign( Value* new_value );

	void Describe( ostream& s ) const;

    protected:
	Value *CallFunc(Func *fv, eval_type etype, ParameterPList *);
	expr_list* args;
	};


class RecordRefExpr : public UnaryExpr {
    public:
	RecordRefExpr( Expr* op, char* record_field );

	Value* Eval( eval_type etype );
	Value* RefEval( value_type val_type );

	void Assign( Value* new_value );

	void Describe( ostream& s ) const;

    protected:
	char* field;
	};


class AttributeRefExpr : public BinaryExpr {
    public:
	AttributeRefExpr( Expr* op1 );
	AttributeRefExpr( Expr* op1, Expr* op2 );
	AttributeRefExpr( Expr* op, char* attribute );

	Value* Eval( eval_type etype );
	Value* RefEval( value_type val_type );

	void Assign( Value* new_value );

	void Describe( ostream& s ) const;

    protected:
	char* field;
	};


class RefExpr : public UnaryExpr {
    public:
	RefExpr( Expr* op, value_type type );

	Value* Eval( eval_type etype );
	void Assign( Value* new_value );

	void Describe( ostream& s ) const;

    protected:
	value_type type;
	};


class RangeExpr : public BinaryExpr {
    public:
	RangeExpr( Expr* op1, Expr* op2 );

	Value* Eval( eval_type etype );
	};


class CallExpr : public UnaryExpr {
    public:
	CallExpr( Expr* func, ParameterPList* args );

	Value* Eval( eval_type etype );
	void SideEffectsEval();

	void Describe( ostream& s ) const;

    protected:
	ParameterPList* args;
	};


class SendEventExpr : public Expr {
    public:
	SendEventExpr( EventDesignator* sender, ParameterPList* args,
			int is_request_reply );

	Value* Eval( eval_type etype );
	void SideEffectsEval();

	void Describe( ostream& s ) const;

    protected:
	EventDesignator* sender;
	ParameterPList* args;
	int is_request_reply;
	};


typedef enum { EVENT_AGENT, EVENT_NAME, EVENT_VALUE } last_event_type;

class LastEventExpr : public Expr {
    public:
	LastEventExpr( Sequencer* sequencer, last_event_type type );

	Value* Eval( eval_type etype );
	Value* RefEval( value_type val_type );
	void Describe( ostream& s ) const;

    protected:
	Sequencer* sequencer;
	last_event_type type;
	};


extern void describe_expr_list( const expr_list* list, ostream& s );


#endif /* expr_h */
