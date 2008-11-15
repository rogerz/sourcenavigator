// $Header$

#ifndef func_h
#define func_h

#include "Expr.h"


class Sequencer;
class Stmt;


typedef Value* value_ptr;
declare(List,value_ptr);
typedef List(value_ptr) args_list;

class Parameter;
declare(PList,Parameter);
typedef PList(Parameter) parameter_list;


class Func : public GlishObject {
    public:
	virtual Value* Call( parameter_list* args, eval_type etype ) = 0;

	int Mark() const	{ return mark; }
	void Mark( int m )	{ mark = m; }

    protected:
	int mark;
	};


class Parameter : public GlishObject {
    public:
	Parameter( const char* name, value_type parm_type, Expr* arg,
			int is_ellipsis = 0, Expr* default_value = 0,
			int is_empty = 0 );

	const char* Name() const		{ return name; }
	value_type ParamType() const		{ return parm_type; }
	Expr* Arg() const			{ return arg; }
	int IsEllipsis() const			{ return is_ellipsis; }
	int IsEmpty() const			{ return is_empty; }
	Expr* DefaultValue() const		{ return default_value; }

	// Number of values represented by "..." argument.
	int NumEllipsisVals() const;

	// Returns the nth value from a "..." argument; the first such
	// value is indexed using n=0.
	const Value* NthEllipsisVal( int n ) const;

	void Describe( ostream& s ) const;

    protected:
	const char* name;
	value_type parm_type;
	Expr* arg;
	int is_ellipsis;
	int is_empty;
	Expr* default_value;
	};


class FormalParameter : public Parameter {
    public:
	FormalParameter( const char* name, value_type parm_type, Expr* arg,
			int is_ellipsis = 0, Expr* default_value = 0 );

	void Describe( ostream& s ) const;
	};

class ActualParameter : public Parameter {
    public:
	ActualParameter( const char* name, value_type parm_type, Expr* arg,
			int is_ellipsis = 0, Expr* default_value = 0,
			int is_empty = 0 )
		: Parameter( name, parm_type, arg, is_ellipsis,
				default_value, is_empty )
		{
		}

	// A missing parameter.
	ActualParameter() : Parameter( 0, VAL_VAL, 0, 0, 0, 1 )
		{
		}
	};


class UserFunc : public Func {
    public:
	UserFunc( parameter_list* formals, Stmt* body, int size,
			Sequencer* sequencer, Expr* subsequence_expr );

	Value* Call( parameter_list* args, eval_type etype );
	Value* DoCall( args_list* args_vals, eval_type etype, Value* missing );

	void Describe( ostream& s ) const;

    protected:
	Value* EvalParam( Parameter* p, Expr* actual );

	// Decode an actual "..." argument.
	void AddEllipsisArgs( args_list* args_vals, Parameter* actual_ellipsis,
				int& num_args, int num_formals,
				Value* formal_ellipsis_value, int& do_call );

	// Add to a formal "..." parameter.
	void AddEllipsisValue( Value* ellipsis_value, Expr* arg );

	void ArgOverFlow( Expr* arg, int num_args, int num_formals,
				Value* ellipsis_value, int& do_call );

	parameter_list* formals;
	Stmt* body;
	int frame_size;
	Sequencer* sequencer;
	Expr* subsequence_expr;
	int valid;
	int has_ellipsis;
	int ellipsis_position;
	};


extern void describe_parameter_list( parameter_list* params, ostream& s );

#endif /* func_h */
