// $Header$

#include "system.h"

#include <string.h>
#include <stream.h>
#include <math.h>
#include <stdlib.h>

#include "Agent.h"
#include "Func.h"
#include "Stmt.h"
#include "Frame.h"
#include "Sequencer.h"
#include "Reporter.h"


Parameter::Parameter( const char* arg_name, value_type arg_parm_type,
			Expr* arg_arg, int arg_is_ellipsis,
			Expr* arg_default_value, int arg_is_empty )
	{
	name = arg_name;
	parm_type = arg_parm_type;
	arg = arg_arg;
	is_ellipsis = arg_is_ellipsis;
	default_value = arg_default_value;
	is_empty = arg_is_empty;
	}

int Parameter::NumEllipsisVals() const
	{
	if ( ! is_ellipsis )
		fatal->Report(
			"Parameter::NumEllipsisVals called but not ellipsis" );

	const Value* values = Arg()->ReadOnlyEval();

	int n = values->RecordPtr()->Length();

	Arg()->ReadOnlyDone( values );

	return n;
	}

const Value* Parameter::NthEllipsisVal( int n ) const
	{
	if ( ! is_ellipsis )
		fatal->Report(
			"Parameter::NthEllipsisVal called but not ellipsis" );

	const Value* values = Arg()->ReadOnlyEval();

	if ( n < 0 || n >= values->RecordPtr()->Length() )
		fatal->Report( "bad value of n in Parameter::NthEllipsisVal" );

	char field_name[256];
	sprintf( field_name, "%d", n );

	const Value* result = values->ExistingRecordElement( field_name );

	Arg()->ReadOnlyDone( values );

	return result;
	}

void Parameter::Describe( ostream& s ) const
	{
	if ( name )
		s << name;
	else
		arg->Describe( s );

	if ( default_value )
		{
		s << " = ";
		default_value->Describe( s );
		}
	}


FormalParameter::FormalParameter( const char* name, value_type parm_type,
			Expr* arg, int is_ellipsis, Expr* default_value )
    : Parameter(name, parm_type, arg, is_ellipsis, default_value, 0)
	{
	}

void FormalParameter::Describe( ostream& s ) const
	{
	if ( parm_type == VAL_CONST )
		s << "const";
	else if ( parm_type == VAL_REF )
		s << "ref";
	else
		s << "val";

	s << " ";

	Parameter::Describe( s );
	}


UserFunc::UserFunc( parameter_list* arg_formals, Stmt* arg_body, int arg_size,
			Sequencer* arg_sequencer, Expr* arg_subsequence_expr )
	{
	formals = arg_formals;
	body = arg_body;
	frame_size = arg_size;
	sequencer = arg_sequencer;
	subsequence_expr = arg_subsequence_expr;

	valid = 1;

	has_ellipsis = 0;
	loop_over_list( *formals, i )
		if ( (*formals)[i]->IsEllipsis() )
			{
			if ( has_ellipsis )
				{
				error->Report(
			"\"...\" appears more than once in parameter list" );
				valid = 0;
				break;
				}

			has_ellipsis = 1;
			ellipsis_position = i;
			}
	}

Value* UserFunc::Call( parameter_list* args, eval_type etype )
	{
	if ( ! valid )
		return error_value();

	args_list args_vals;
	int do_call = 1;
	Parameter* f;

	int num_args = 0;
	int num_supplied_args = args->length();
	int num_formals;

	Value* ellipsis_value;
	if ( has_ellipsis )
		{
		ellipsis_value = create_record();
		num_formals = ellipsis_position;
		}

	else
		{
		ellipsis_value = 0;
		num_formals = formals->length();
		}

	int missing_size = num_supplied_args + formals->length();
	int missing_len = 0;
	glish_bool* missing =
		(glish_bool*) alloc_memory( missing_size * sizeof(glish_bool) );
	if ( ! missing )
		fatal->Report( "out of memory in UserFunc::Call" );

// Macro to note which arguments were missing and which were present.  Use
// is: spin through the arguments sequentially and call the macro with either
// "true" or "false" to indicate that the next argument was missing/present.
#define ADD_MISSING_INFO(value)						\
	{								\
	if ( missing_len >= missing_size )				\
		{							\
		missing_size *= 2;					\
		missing = (glish_bool*) realloc_memory( (void*) missing,\
				missing_size * sizeof(glish_bool) );	\
		if ( ! missing )					\
			fatal->Report( "out of memory in UserFunc::Call" );\
		}							\
	missing[missing_len++] = value;					\
	}

	// Match until a named argument is encountered.
	loop_over_list( *args, i )
		{
		Parameter* arg = (*args)[i];

		if ( arg->Name() )
			break;

		if ( arg->IsEllipsis() )
			{
			AddEllipsisArgs( &args_vals, arg, num_args, num_formals,
					ellipsis_value, do_call );
			for ( int j = 0; j < arg->NumEllipsisVals(); ++j )
				ADD_MISSING_INFO(glish_false)
			}

		else
			{
			if ( num_args >= num_formals )
				{
				Parameter* p = (*args)[i];
				if ( p->IsEmpty() )
					{
					f = (*formals)[ellipsis_position];
					Expr* dflt = f ? f->DefaultValue() : 0;
					if ( dflt )
						{
						ArgOverFlow( dflt, num_args,
							num_formals,
							ellipsis_value,
							do_call );
						ADD_MISSING_INFO(glish_true)
						}
					else
						{
						error->Report(
						"Missing parameter ", i+1,
						", no default available" );
						do_call = 0;
						}
					}
				else
					{
					ArgOverFlow( p->Arg(), num_args,
							num_formals,
							ellipsis_value,
							do_call );
					ADD_MISSING_INFO(glish_false)
					}
				}

			else
				{
				f = (*formals)[num_args];
				Parameter* p = (*args)[i];
				if ( p->IsEmpty() )
					{
					Expr* dflt = f ? f->DefaultValue() : 0;
					if ( dflt )
						{
						args_vals.append(
							EvalParam( f, dflt ) );
						ADD_MISSING_INFO(glish_true)
						}
					else
					  	{
						error->Report(
						"Missing parameter ", i+1, 
						", no default available." );
						do_call = 0;
						}
					}
				else
					{
					args_vals.append( EvalParam( f,
							(*args)[i]->Arg() ) );
					ADD_MISSING_INFO(glish_false)
					}

				++num_args;
				}
			}
		}

	int first_named_arg = i;

	// Check the named arguments to see if they're valid.
	for ( int named_arg = first_named_arg; named_arg < num_supplied_args;
	      ++named_arg )
		{
		const char* arg_name = (*args)[named_arg]->Name();

		if ( ! arg_name )
			{
			if ( do_call )
				{
				error->Report( "unnamed arg (position",
						named_arg,
					") given after named arg in call to",
						this );
				do_call = 0;
				}
			}

		else
			{
			loop_over_list( *formals, j )
				{
				const char* formal_name = (*formals)[j]->Name();

				if ( formal_name &&
				     ! strcmp( formal_name, arg_name ) )
					break;
				}

			if ( j >= formals->length() )
				{
				error->Report( "named arg \"", arg_name,
				    "\" does not match any formal in call to",
						this );
				do_call = 0;
				}

			else if ( j < args_vals.length() )
				{
				error->Report( "formal \"", arg_name,
		    "\" matched by both positional and named arg in call to",
						this );
				do_call = 0;
				}
			}
		}

	if ( do_call )
		{
		// Fill in remaining formals, looking for matching named
		// arguments or else using the formal's default value (if
		// any).  Also put the ellipsis value (if any) into args_vals.

		// Be sure to match formals beyond the ellipsis, too.
		num_formals = formals->length();

		for ( ; num_args < num_formals; ++num_args )
			{
			f = (*formals)[num_args];

			if ( f->IsEllipsis() )
				{
				args_vals.append( ellipsis_value );
				continue;
				}

			const char* formal_name = f->Name();

			for ( int match = first_named_arg;
			      match < num_supplied_args; ++match )
				{
				const char* actual_name =
					(*args)[match]->Name();
				if ( ! strcmp( formal_name, actual_name ) )
					break;
				}

			if ( match < num_supplied_args )
				{
				args_vals.append( EvalParam( f,
						(*args)[match]->Arg() ) );
				ADD_MISSING_INFO(glish_false)
				}
			else
				{
				Expr* default_value = f->DefaultValue();

				if ( ! default_value )
					{
					error->Report( "parameter \"",
							f->Name(),
							"\" missing in call to",
							this );
					do_call = 0;
					}

				else
					{
					args_vals.append( EvalParam( f,
							default_value ) );
					ADD_MISSING_INFO(glish_true)
					}
				}
			}
		}

	Value* result;

	if ( do_call )
		{
		Value* missing_val = missing_len > 0 ?
			new Value( missing, missing_len, PRESERVE_ARRAY ) : 0;
		result = DoCall( &args_vals, etype, missing_val );
		// No need to Unref() missing_val, Sequencer::PopFrame did
		// that for us.
		}

	else
		{
		loop_over_list( args_vals, k )
			Unref( args_vals[k] );

		result = error_value();
		}

	free_memory( (void*) missing );

	return result;
	}

Value* UserFunc::DoCall( args_list* args_vals, eval_type etype, Value* missing )
	{
	Frame* call_frame = new Frame( frame_size, missing );
	sequencer->PushFrame( call_frame );

	if ( subsequence_expr )
		{
		UserAgent* self = new UserAgent( sequencer );
		subsequence_expr->Assign( new Value( self->AgentRecord(),
							VAL_REF ) );
		}

	loop_over_list( (*formals), i )
		(*formals)[i]->Arg()->Assign( (*args_vals)[i] );

	int value_needed = etype != EVAL_SIDE_EFFECTS;
	stmt_flow_type flow;
	Value* result = body->Exec( value_needed, flow );

	if ( subsequence_expr )
		{
		if ( result &&
		     (result->Type() != TYPE_BOOL || result->BoolVal()) )
			{
			warn->Report( "value (", result,
			") returned from subsequence replaced by ref self" );
			Unref( result );
			}

		result = subsequence_expr->RefEval( VAL_REF );

		if ( etype == EVAL_SIDE_EFFECTS )
			warn->Report( "agent returned by subsequence ignored" );
		}

	if ( sequencer->PopFrame() != call_frame )
		fatal->Report( "frame inconsistency in UserFunc::DoCall" );

	Unref( call_frame );

	return result;
	}


void UserFunc::Describe( ostream& s ) const
	{
	s << "function (";
	describe_parameter_list( formals, s );
	s << ") ";
	body->Describe( s );
	}


Value* UserFunc::EvalParam( Parameter* p, Expr* actual )
	{
	value_type param_type = p->ParamType();

	if ( param_type == VAL_VAL )
		return actual->CopyEval();
	else
		return actual->RefEval( param_type );
	}


void UserFunc::AddEllipsisArgs( args_list* args_vals,
				Parameter* actual_ellipsis, int& num_args,
				int num_formals, Value* formal_ellipsis_value,
				int& do_call )
	{
	int len = actual_ellipsis->NumEllipsisVals();

	for ( int i = 0; i < len; ++i )
		{
		const Value* val;

		if ( num_args >= num_formals )
			{
			if ( ! formal_ellipsis_value )
				{
				error->Report( "too many arguments (> ",
						num_formals,
						") supplied in call to", this );
				do_call = 0;
				return;
				}

			char field_name[256];
			sprintf( field_name, "%d",
				formal_ellipsis_value->RecordPtr()->Length() );

			val = actual_ellipsis->NthEllipsisVal( i );

			Value* val_ref = new Value( (Value*) val, VAL_CONST );
			formal_ellipsis_value->AssignRecordElement( field_name,
								val_ref );

			// See comment in Value.h regarding AssignRecordElement.
			Unref( val_ref );

			continue;
			}

		Parameter* f = (*formals)[num_args];

		if ( f->ParamType() == VAL_REF )
			{
			error->Report(
				"\"...\" is a \"const\" reference, formal",
					f->Name(), " is \"ref\"" );
			do_call = 0;
			return;
			}

		val = actual_ellipsis->NthEllipsisVal( i );

		if ( f->ParamType() == VAL_VAL )
			args_vals->append( copy_value( val ) );

		else
			{
			Value* val_ref = new Value( (Value*) val, VAL_CONST );
			args_vals->append( val_ref );
			}

		++num_args;
		}
	}


void UserFunc::AddEllipsisValue( Value* ellipsis_value, Expr* arg )
	{
	char field_name[256];
	sprintf( field_name, "%d", ellipsis_value->RecordPtr()->Length() );

	Value* val = arg->RefEval( VAL_CONST );

	ellipsis_value->AssignRecordElement( field_name, val );

	Unref( val );	// see comment in Value.h regarding AssignRecordElement
	}


void UserFunc::ArgOverFlow( Expr* arg, int num_args, int num_formals,
				Value* ellipsis_value, int& do_call )
	{
	if ( ellipsis_value )
		AddEllipsisValue( ellipsis_value, arg );

	else
		{
		if ( num_args == num_formals + 1 )
			{
			error->Report( "too many arguments (> ",
				num_formals, ") supplied in call to",
				this );
			}

		do_call = 0;
		}
	}


void describe_parameter_list( parameter_list* params, ostream& s )
	{
	loop_over_list( *params, i )
		{
		if ( i > 0 )
			s << ", ";

		(*params)[i]->Describe( s );
		}
	}
