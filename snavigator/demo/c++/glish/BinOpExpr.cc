// $Header$

#include <string.h>
#include <math.h>
#include "Reporter.h"
#include "BinOpExpr.h"
#include "Glish/Value.h"
#include "Glish/Complex.h"


BinOpExpr::BinOpExpr( binop bin_op, Expr* op1, Expr* op2, const char* desc )
    : BinaryExpr(op1, op2, desc)
	{
	op = bin_op;
	}

int BinOpExpr::TypeCheck( const Value* lhs, const Value* rhs,
				int& element_by_element ) const
	{
	element_by_element = 1;

	if ( ! lhs->IsNumeric() || ! rhs->IsNumeric() )
		{
		error->Report( "non-numeric operand in expression:", this );
		return 0;
		}

	else
		return 1;
	}

glish_type BinOpExpr::OperandsType( const Value* lhs, const Value* rhs ) const
	{
	glish_type t1 = lhs->Type();
	glish_type t2 = rhs->Type();

	if ( t1 == TYPE_DCOMPLEX || t2 == TYPE_DCOMPLEX )
		return TYPE_DCOMPLEX;

	else if ( t1 == TYPE_COMPLEX || t2 == TYPE_COMPLEX )
		{
		if ( t1 == TYPE_DOUBLE || t2 == TYPE_DOUBLE )
			return TYPE_DCOMPLEX;
		else
			return TYPE_COMPLEX;
		}

	else if ( t1 == TYPE_DOUBLE || t2 == TYPE_DOUBLE )
		return TYPE_DOUBLE;

	else if ( t1 == TYPE_FLOAT || t2 == TYPE_FLOAT )
		return TYPE_FLOAT;

	else if ( t1 == TYPE_INT || t2 == TYPE_INT )
		return TYPE_INT;

	else if ( t1 == TYPE_SHORT || t2 == TYPE_SHORT )
		return TYPE_SHORT;

	else if ( t1 == TYPE_BYTE || t2 == TYPE_BYTE )
		return TYPE_BYTE;

	else if ( t1 == TYPE_BOOL || t2 == TYPE_BOOL )
		// Promote bool's to int's.
		return TYPE_INT;

	else if ( t1 == TYPE_STRING || t2 == TYPE_STRING )
		return TYPE_STRING;

	else
		// Hmmmm, not a numeric type.  Just pick one of the two.
		return t1;
	}

int BinOpExpr::Compute( const Value* lhs, const Value* rhs, int& lhs_len )
    const
	{
	int lhs_scalar = lhs->Length() == 1;
	int rhs_scalar = rhs->Length() == 1;

	if ( lhs_scalar && rhs_scalar )
		lhs_scalar = 0;	// Treat lhs as 1-element array

	lhs_len = lhs->Length();
	int rhs_len = rhs->Length();

	if ( lhs_len != rhs_len && ! lhs_scalar && ! rhs_scalar )
		{
		error->Report( "different-length operands in expression (",
				lhs_len, " vs. ", rhs_len, "):\n\t",
				this );
		return 0;
		}

	if ( lhs_scalar )
		// We need to replicate the left-hand-side so that it's
		// the same length as the right-hand-side.
		lhs_len = rhs_len;

	return 1;
	}


Value* ArithExpr::Eval( eval_type /* etype */ )
	{
	Value* result = left->CopyEval();
	const Value* rhs = right->ReadOnlyEval();

	int lhs_len;
	int element_by_element;
	if ( TypeCheck( result, rhs, element_by_element ) &&
	     (! element_by_element ||
	      // Stupid cfront requires the BinOpExpr:: here ...
	      BinOpExpr::Compute( result, rhs, lhs_len )) )
		result = OpCompute( result, rhs, lhs_len );

	else
		{
		Unref( result );
		result = error_value();
		}

	right->ReadOnlyDone( rhs );

	return result;
	}

Value* ArithExpr::OpCompute( Value* lhs, const Value* rhs, int lhs_len )
	{
	switch ( OperandsType( lhs, rhs ) )
		{
		case TYPE_BYTE:
			lhs->ByteOpCompute( rhs, lhs_len, this );
			break;

		case TYPE_SHORT:
			lhs->ShortOpCompute( rhs, lhs_len, this );
			break;

		case TYPE_INT:
			lhs->IntOpCompute( rhs, lhs_len, this );
			break;

		case TYPE_FLOAT:
			lhs->FloatOpCompute( rhs, lhs_len, this );
			break;

		case TYPE_DOUBLE:
			lhs->DoubleOpCompute( rhs, lhs_len, this );
			break;

		case TYPE_COMPLEX:
			lhs->ComplexOpCompute( rhs, lhs_len, this );
			break;

		case TYPE_DCOMPLEX:
			lhs->DcomplexOpCompute( rhs, lhs_len, this );
			break;

		default:
			fatal->Report(
				"bad operands type in ArithExpr::OpCompute()" );
		}

	return lhs;
	}


#define COMPUTE_OP(name,op,type)					\
void name::Compute( type lhs[], type rhs[], int lhs_len, int rhs_incr )	\
	{								\
	for ( int i = 0, j = 0; i < lhs_len; ++i, j += rhs_incr )	\
		lhs[i] op rhs[j];				\
	}

#define COMPLEX_COMPUTE_OP(name,op,type)				\
void name::Compute( type lhs[], type rhs[], int lhs_len, int rhs_incr )	\
	{								\
	for ( int i = 0, j = 0; i < lhs_len; ++i, j += rhs_incr )	\
		{							\
		lhs[i].r op rhs[j].r;					\
		lhs[i].i op rhs[j].i;					\
		}							\
	}

#define COMPLEX_COMPUTE_MUL_OP(type)				\
void MultiplyExpr::Compute( type lhs[], type rhs[], int lhs_len, int rhs_incr )\
	{								\
	for ( int i = 0, j = 0; i < lhs_len; ++i, j += rhs_incr )	\
		lhs[i] = mul( lhs[i], rhs[j] );				\
	}

#define COMPLEX_COMPUTE_DIV_OP(type)				\
void DivideExpr::Compute( type lhs[], type rhs[], int lhs_len, int rhs_incr ) \
	{								\
	for ( int i = 0, j = 0; i < lhs_len; ++i, j += rhs_incr )	\
		lhs[i] = div( lhs[i], rhs[j] );				\
	}

#define DEFINE_ARITH_EXPR(name, op)	\
COMPUTE_OP(name,op,byte)		\
COMPUTE_OP(name,op,short)		\
COMPUTE_OP(name,op,int)			\
COMPUTE_OP(name,op,float)		\
COMPUTE_OP(name,op,double)		\

#define DEFINE_SIMPLE_ARITH_EXPR(name, op)	\
DEFINE_ARITH_EXPR(name,op)			\
COMPLEX_COMPUTE_OP(name,op,complex)		\
COMPLEX_COMPUTE_OP(name,op,dcomplex)		\

DEFINE_SIMPLE_ARITH_EXPR(AddExpr,+=)
DEFINE_SIMPLE_ARITH_EXPR(SubtractExpr,-=)

DEFINE_ARITH_EXPR(MultiplyExpr,*=)
COMPLEX_COMPUTE_MUL_OP(complex)
COMPLEX_COMPUTE_MUL_OP(dcomplex)

DEFINE_ARITH_EXPR(DivideExpr,/=)
COMPLEX_COMPUTE_DIV_OP(complex)
COMPLEX_COMPUTE_DIV_OP(dcomplex)


glish_type DivideExpr::OperandsType( const Value* lhs, const Value* rhs ) const
	{
	glish_type ltype = lhs->Type();
	glish_type rtype = rhs->Type();

	if ( ltype == TYPE_DCOMPLEX || rtype == TYPE_DCOMPLEX )
		return TYPE_DCOMPLEX;

	else if ( ltype == TYPE_COMPLEX || rtype == TYPE_COMPLEX )
		return TYPE_COMPLEX;

	else
		return TYPE_DOUBLE;
	}


glish_type ModuloExpr::OperandsType( const Value* /* lhs */,
					const Value* /* rhs */ ) const
	{
	return TYPE_INT;
	}

void ModuloExpr::Compute( byte lhs[], byte rhs[], int lhs_len, int rhs_incr )
	{
	for ( int i = 0, j = 0; i < lhs_len; ++i, j += rhs_incr )
		lhs[i] %= rhs[j];
	}

void ModuloExpr::Compute( short lhs[], short rhs[], int lhs_len, int rhs_incr )
	{
	for ( int i = 0, j = 0; i < lhs_len; ++i, j += rhs_incr )
		lhs[i] %= rhs[j];
	}

void ModuloExpr::Compute( int lhs[], int rhs[], int lhs_len, int rhs_incr )
	{
	for ( int i = 0, j = 0; i < lhs_len; ++i, j += rhs_incr )
		lhs[i] %= rhs[j];
	}

void ModuloExpr::Compute( float*, float*, int, int )
	{
	fatal->Report( "ModuloExpr::Compute() called with float operands" );
	}

void ModuloExpr::Compute( double*, double*, int, int )
	{
	fatal->Report( "ModuloExpr::Compute() called with double operands" );
	}

void ModuloExpr::Compute( complex*, complex*, int, int )
	{
	fatal->Report( "ModuloExpr::Compute() called with complex operands" );
	}

void ModuloExpr::Compute( dcomplex*, dcomplex*, int, int )
	{
	fatal->Report( "ModuloExpr::Compute() called with dcomplex operands" );
	}


glish_type PowerExpr::OperandsType( const Value* lhs, const Value* rhs ) const
	{
	glish_type t1 = lhs->Type();
	glish_type t2 = rhs->Type();

	if ( t1 == TYPE_DCOMPLEX || t2 == TYPE_DCOMPLEX )
		return TYPE_DCOMPLEX;

	else if ( t1 == TYPE_COMPLEX || t2 == TYPE_COMPLEX )
		return TYPE_DCOMPLEX;

	else return TYPE_DOUBLE;
	}

void PowerExpr::Compute( byte*, byte*, int, int )
	{
	fatal->Report( "PowerExpr::Compute() called with byte operands" );
	}

void PowerExpr::Compute( short*, short*, int, int )
	{
	fatal->Report( "PowerExpr::Compute() called with short operands" );
	}

void PowerExpr::Compute( int*, int*, int, int )
	{
	fatal->Report( "PowerExpr::Compute() called with integer operands" );
	}

void PowerExpr::Compute( float*, float*, int, int )
	{
	fatal->Report( "PowerExpr::Compute() called with float operands" );
	}

void PowerExpr::Compute( double lhs[], double rhs[], int lhs_len, int rhs_incr )
	{
	for ( int i = 0, j = 0; i < lhs_len; ++i, j += rhs_incr )
		lhs[i] = pow( lhs[i], rhs[j] );
	}

void PowerExpr::Compute( complex*, complex*, int, int )
	{
	fatal->Report( "PowerExpr::Compute() called with complex operands" );
	}

void PowerExpr::Compute( dcomplex lhs[], dcomplex rhs[],
			int lhs_len, int rhs_incr )
	{
	for ( int i = 0, j = 0; i < lhs_len; ++i, j += rhs_incr )
		lhs[i] = pow( lhs[i], rhs[j] );
	}



Value* RelExpr::Eval( eval_type /* etype */ )
	{
	const Value* lhs = left->ReadOnlyEval();
	const Value* rhs = right->ReadOnlyEval();

	Value* result;
	int lhs_len;
	int element_by_element;
	if ( TypeCheck( lhs, rhs, element_by_element ) &&
	     (! element_by_element ||
	      BinOpExpr::Compute( lhs, rhs, lhs_len )) )
		result = OpCompute( lhs, rhs, lhs_len );
	else
		result = error_value();

	left->ReadOnlyDone( lhs );
	right->ReadOnlyDone( rhs );

	return result;
	}

int RelExpr::TypeCheck( const Value* lhs, const Value* rhs,
				int& element_by_element ) const
	{
	element_by_element = 1;

	if ( lhs->Type() == TYPE_STRING && rhs->Type() == TYPE_STRING )
		return 1;

	else if ( lhs->IsNumeric() || rhs->IsNumeric() )
		return BinOpExpr::TypeCheck( lhs, rhs, element_by_element );

	else
		{
		// Equality comparisons are allowed between all types,
		// but not other operations.
		if ( op == OP_EQ || op == OP_NE )
			{
			element_by_element = 0;
			return 1;
			}

		else
			return 0;
		}
	}

glish_type RelExpr::OperandsType( const Value* lhs, const Value* rhs ) const
	{
	glish_type t1 = lhs->Type();
	glish_type t2 = rhs->Type();

	if ( t1 == TYPE_STRING && t2 == TYPE_STRING )
		return TYPE_STRING;

	else if ( (op == OP_AND || op == OP_OR) &&
		  (t1 == TYPE_BOOL && t2 == TYPE_BOOL) )
		return TYPE_BOOL;

	else
		return BinOpExpr::OperandsType( lhs, rhs );
	}

Value* RelExpr::OpCompute( const Value* lhs, const Value* rhs, int lhs_len )
	{
	Value* result;

	switch ( OperandsType( lhs, rhs ) )
		{
		case TYPE_BOOL:
			result = bool_rel_op_compute( lhs, rhs, lhs_len, this );
			break;

		case TYPE_BYTE:
			result = byte_rel_op_compute( lhs, rhs, lhs_len, this );
			break;

		case TYPE_SHORT:
			result = short_rel_op_compute( lhs, rhs, lhs_len, this );
			break;

		case TYPE_INT:
			result = int_rel_op_compute( lhs, rhs, lhs_len, this );
			break;

		case TYPE_FLOAT:
			result = float_rel_op_compute( lhs, rhs,
							lhs_len, this );
			break;

		case TYPE_DOUBLE:
			result = double_rel_op_compute( lhs, rhs,
							lhs_len, this );
			break;

		case TYPE_COMPLEX:
			result = complex_rel_op_compute( lhs, rhs,
							lhs_len, this );
			break;

		case TYPE_DCOMPLEX:
			result = dcomplex_rel_op_compute( lhs, rhs,
							lhs_len, this );
			break;

		case TYPE_STRING:
			result = string_rel_op_compute( lhs, rhs,
							lhs_len, this );
			break;

		case TYPE_AGENT:
		case TYPE_FUNC:
		case TYPE_RECORD:
		case TYPE_OPAQUE:
			if ( op == OP_EQ )
				return new Value( glish_bool( lhs == rhs ) );

			else if ( op == OP_NE )
				return new Value( glish_bool( lhs != rhs ) );

			else
				fatal->Report(
				"bad operands type in RelExpr::OpCompute()" );
			break;

		default:
			fatal->Report(
				"bad operands type in RelExpr::OpCompute()" );
		}

	return result;
	}

#define COMPUTE_BOOL_REL_RESULT(name,op,type)				\
void name::Compute( type lhs[], type rhs[], glish_bool result[],	\
    int lhs_len, int rhs_incr )						\
	{								\
	for ( int i = 0, j = 0; i < lhs_len; ++i, j += rhs_incr )	\
		result[i] = glish_bool( int( lhs[i] ) op int( rhs[j] ) );\
	}

#define COMPUTE_NUMERIC_REL_RESULT(name,op,type)			\
void name::Compute( type lhs[], type rhs[], glish_bool result[],	\
    int lhs_len, int rhs_incr )						\
	{								\
	for ( int i = 0, j = 0; i < lhs_len; ++i, j += rhs_incr )	\
		result[i] = glish_bool( lhs[i] op rhs[j] );		\
	}

#define COMPUTE_STRING_REL_RESULT(name,op,type)				\
void name::Compute( type lhs[], type rhs[], glish_bool result[],	\
    int lhs_len, int rhs_incr )						\
	{								\
	for ( int i = 0, j = 0; i < lhs_len; ++i, j += rhs_incr )	\
		result[i] = glish_bool( strcmp( lhs[i], rhs[j] ) op 0 );\
	}

#define DEFINE_REL_EXPR(name, op)					\
COMPUTE_BOOL_REL_RESULT(name,op,glish_bool)					\
COMPUTE_NUMERIC_REL_RESULT(name,op,byte)				\
COMPUTE_NUMERIC_REL_RESULT(name,op,short)				\
COMPUTE_NUMERIC_REL_RESULT(name,op,int)					\
COMPUTE_NUMERIC_REL_RESULT(name,op,float)				\
COMPUTE_NUMERIC_REL_RESULT(name,op,double)				\
COMPUTE_NUMERIC_REL_RESULT(name,op,complex)				\
COMPUTE_NUMERIC_REL_RESULT(name,op,dcomplex)				\
COMPUTE_STRING_REL_RESULT(name,op,charptr)

DEFINE_REL_EXPR(EQ_Expr, ==)
DEFINE_REL_EXPR(NE_Expr, !=)
DEFINE_REL_EXPR(LE_Expr, <=)
DEFINE_REL_EXPR(GE_Expr, >=)
DEFINE_REL_EXPR(LT_Expr, <)
DEFINE_REL_EXPR(GT_Expr, >)


#define DEFINE_LOG_EXPR_COMPUTE(type, typename)				\
void LogExpr::Compute( type*, type*, glish_bool*, int, int )		\
	{								\
	fatal->Report( "LogExpr::Compute called with", typename, "operands" );\
	}
DEFINE_LOG_EXPR_COMPUTE(glish_bool, "boolean")
DEFINE_LOG_EXPR_COMPUTE(byte, "byte")
DEFINE_LOG_EXPR_COMPUTE(short, "short")
DEFINE_LOG_EXPR_COMPUTE(int, "integer")
DEFINE_LOG_EXPR_COMPUTE(float, "float")
DEFINE_LOG_EXPR_COMPUTE(double, "double")
DEFINE_LOG_EXPR_COMPUTE(complex, "complex")
DEFINE_LOG_EXPR_COMPUTE(dcomplex, "dcomplex")
DEFINE_LOG_EXPR_COMPUTE(charptr, "string")

int LogExpr::TypeCheck( const Value* lhs, const Value* rhs,
				int& element_by_element ) const
	{
	element_by_element = 1;

	if ( lhs->Type() == TYPE_BOOL && rhs->Type() == TYPE_BOOL )
		return 1;
	else
		{
		error->Report( "non-boolean operands to", this );
		return 0;
		}
	}

glish_type LogExpr::OperandsType( const Value*, const Value* ) const
	{
	return TYPE_BOOL;
	}

COMPUTE_BOOL_REL_RESULT(LogAndExpr,&,glish_bool)
COMPUTE_BOOL_REL_RESULT(LogOrExpr,|,glish_bool)
