// $Header$


// Definitions for arithmetic and relational Expr classes

#ifndef binopexpr_h
#define binopexpr_h

#include "Expr.h"


// Arithmetic operations supported on values.
typedef enum {
	OP_ADD, OP_SUBTRACT, OP_MULTIPLY, OP_DIVIDE, OP_MODULO, OP_POWER,
	OP_AND, OP_OR,
	OP_EQ, OP_NE, OP_LE, OP_GE, OP_LT, OP_GT
	} binop;


// An BinOpExpr is a binary expression which performs some arithmetic
// or relational operation.

class BinOpExpr : public BinaryExpr {
    public:
	BinOpExpr( binop op, Expr* op1, Expr* op2, const char* desc );

	Value* Eval( eval_type etype ) = 0;

    protected:
	// Returns true if the expression's operands type-check, false
	// (and generates an error message) otherwise.  The default TypeCheck
	// implements arithmetic type-checking (operands must be numeric).
	//
	// The third argument is set to true if this expression operates
	// element-by-element on arrays, false otherwise.
	virtual int TypeCheck( const Value* lhs, const Value* rhs,
				int& element_by_element ) const;

	// What type the BinOpExpr's operands should be promoted to.  The
	// default OperandsType implements numeric promotion (higher operand
	// type is used, where the hierarchy is double, float, int, and
	// bool is promoted to int).
	virtual glish_type OperandsType( const Value* lhs, const Value* rhs )
			const;

	// Called after type-checking is done.  Checks array lengths
	// for compatibility and promotes scalars to arrays as necessary.
	// Returns in lhs_len the array length at which lhs should be
	// used (if lhs is a scalar and rhs is an array then this will be
	// the length of rhs, otherwise the length of lhs).  Returns true
	// all checking was okay, false otherwise (in which case lhs_len
	// may not have been set).
	int Compute( const Value* lhs, const Value* rhs, int& lhs_len ) const;

	binop op;
	};


// A BinOp expression that performs an arithmetic operation: i.e., one
// in which the operands are of numeric type and the result is the same
// type as the operands.
class ArithExpr : public BinOpExpr {
    public:
	ArithExpr( binop op, Expr* op1, Expr* op2,
			const char* desc ) : BinOpExpr(op, op1, op2, desc)
		{ }

	Value* Eval( eval_type etype );

	virtual void Compute( byte lhs[], byte rhs[],
				int lhs_len, int rhs_incr ) = 0;
	virtual void Compute( short lhs[], short rhs[],
				int lhs_len, int rhs_incr ) = 0;
	virtual void Compute( int lhs[], int rhs[],
				int lhs_len, int rhs_incr ) = 0;
	virtual void Compute( float lhs[], float rhs[],
				int lhs_len, int rhs_incr ) = 0;
	virtual void Compute( double lhs[], double rhs[],
				int lhs_len, int rhs_incr ) = 0;
	virtual void Compute( complex lhs[], complex rhs[],
				int lhs_len, int rhs_incr ) = 0;
	virtual void Compute( dcomplex lhs[], dcomplex rhs[],
				int lhs_len, int rhs_incr ) = 0;

    protected:
	Value* OpCompute( Value* lhs, const Value* rhs, int lhs_len );
	};

#define DECLARE_ARITH_EXPR(name, op, op_name, overloads)		\
class name : public ArithExpr {						\
    public:								\
	name( Expr* op1, Expr* op2 )					\
		: ArithExpr(op, op1, op2, op_name)	{ }		\
	overloads							\
	void Compute( byte lhs[], byte rhs[], int lhs_len, int rhs_incr );\
	void Compute( short lhs[], short rhs[], int lhs_len, int rhs_incr );\
	void Compute( int lhs[], int rhs[], int lhs_len, int rhs_incr );\
	void Compute( float lhs[], float rhs[], int lhs_len, int rhs_incr );\
	void Compute( double lhs[], double rhs[], int lhs_len, int rhs_incr );\
	void Compute( complex lhs[], complex rhs[], int lhs_len,	\
			int rhs_incr );					\
	void Compute( dcomplex lhs[], dcomplex rhs[], int lhs_len,	\
			int rhs_incr );					\
	};

DECLARE_ARITH_EXPR(AddExpr, OP_ADD, "+",)
DECLARE_ARITH_EXPR(SubtractExpr, OP_SUBTRACT, "-",)
DECLARE_ARITH_EXPR(MultiplyExpr, OP_MULTIPLY, "*",)
DECLARE_ARITH_EXPR(DivideExpr, OP_DIVIDE, "/",
	glish_type OperandsType( const Value* lhs, const Value* rhs ) const;)
DECLARE_ARITH_EXPR(ModuloExpr, OP_MODULO, "/",
	glish_type OperandsType( const Value* lhs, const Value* rhs ) const;)
DECLARE_ARITH_EXPR(PowerExpr, OP_POWER, "^",
	glish_type OperandsType( const Value* lhs, const Value* rhs ) const;)


// A BinOpExpr that performs a relational operation; i.e., an operation with
// a boolean result value.
class RelExpr : public BinOpExpr {
    public:
	RelExpr( binop op, Expr* op1, Expr* op2, const char* desc )
			: BinOpExpr(op, op1, op2, desc) { }

	Value* Eval( eval_type etype );

	virtual void Compute( glish_bool lhs[], glish_bool rhs[],
				glish_bool result[],
				int lhs_len, int rhs_incr ) = 0;
	virtual void Compute( byte lhs[], byte rhs[], glish_bool result[],
				int lhs_len, int rhs_incr ) = 0;
	virtual void Compute( short lhs[], short rhs[], glish_bool result[],
				int lhs_len, int rhs_incr ) = 0;
	virtual void Compute( int lhs[], int rhs[], glish_bool result[],
				int lhs_len, int rhs_incr ) = 0;
	virtual void Compute( float lhs[], float rhs[], glish_bool result[],
				int lhs_len, int rhs_incr ) = 0;
	virtual void Compute( double lhs[], double rhs[], glish_bool result[],
				int lhs_len, int rhs_incr ) = 0;
	virtual void Compute( complex lhs[], complex rhs[], glish_bool result[],
				int lhs_len, int rhs_incr ) = 0;
	virtual void Compute( dcomplex lhs[], dcomplex rhs[],
				glish_bool result[],
				int lhs_len, int rhs_incr ) = 0;
	virtual void Compute( charptr lhs[], charptr rhs[], glish_bool result[],
				int lhs_len, int rhs_incr ) = 0;

    protected:
	int TypeCheck( const Value* lhs, const Value* rhs,
			int& element_by_element ) const;
	glish_type OperandsType( const Value* lhs, const Value* rhs ) const;
	Value* OpCompute( const Value* lhs, const Value* rhs, int lhs_len );
	};


#define DECLARE_REL_EXPR(name, op, op_name)				\
class name : public RelExpr {						\
    public:								\
	name( Expr* op1, Expr* op2 )					\
		: RelExpr(op, op1, op2, op_name)	{ }		\
	void Compute( glish_bool lhs[], glish_bool rhs[],		\
			glish_bool result[],				\
			int lhs_len, int rhs_incr );			\
	void Compute( byte lhs[], byte rhs[], glish_bool result[],	\
			int lhs_len, int rhs_incr );			\
	void Compute( short lhs[], short rhs[], glish_bool result[],	\
			int lhs_len, int rhs_incr );			\
	void Compute( int lhs[], int rhs[], glish_bool result[],	\
			int lhs_len, int rhs_incr );			\
	void Compute( float lhs[], float rhs[], glish_bool result[],	\
			int lhs_len, int rhs_incr );			\
	void Compute( double lhs[], double rhs[], glish_bool result[],	\
			int lhs_len, int rhs_incr );			\
	void Compute( complex lhs[], complex rhs[], glish_bool result[],\
			int lhs_len, int rhs_incr );			\
	void Compute( dcomplex lhs[], dcomplex rhs[], glish_bool result[],\
			int lhs_len, int rhs_incr );			\
	void Compute( charptr lhs[], charptr rhs[], glish_bool result[],\
			int lhs_len, int rhs_incr );			\
	};

DECLARE_REL_EXPR(EQ_Expr, OP_EQ, "==")
DECLARE_REL_EXPR(NE_Expr, OP_NE, "!=")
DECLARE_REL_EXPR(LE_Expr, OP_LE, "<=")
DECLARE_REL_EXPR(GE_Expr, OP_GE, ">=")
DECLARE_REL_EXPR(LT_Expr, OP_LT, "<")
DECLARE_REL_EXPR(GT_Expr, OP_GT, ">")


// A RelExpr that performs a logical operation; i.e., boolean operands with
// a boolean result value.
class LogExpr : public RelExpr {
    public:
	LogExpr( binop op, Expr* op1, Expr* op2, const char* desc )
			: RelExpr(op, op1, op2, desc)	{ }


	void Compute( glish_bool lhs[], glish_bool rhs[], glish_bool result[],
			int lhs_len, int rhs_incr );
	void Compute( byte lhs[], byte rhs[], glish_bool result[],
			int lhs_len, int rhs_incr );
	void Compute( short lhs[], short rhs[], glish_bool result[],
			int lhs_len, int rhs_incr );
	void Compute( int lhs[], int rhs[], glish_bool result[],
			int lhs_len, int rhs_incr );
	void Compute( float lhs[], float rhs[], glish_bool result[],
			int lhs_len, int rhs_incr );
	void Compute( double lhs[], double rhs[], glish_bool result[],
			int lhs_len, int rhs_incr );
	void Compute( complex lhs[], complex rhs[], glish_bool result[],
			int lhs_len, int rhs_incr );
	void Compute( dcomplex lhs[], dcomplex rhs[], glish_bool result[],
			int lhs_len, int rhs_incr );
	void Compute( charptr lhs[], charptr rhs[], glish_bool result[],
			int lhs_len, int rhs_incr );

    protected:
	int TypeCheck( const Value* lhs, const Value* rhs,
			int& element_by_element ) const;
	glish_type OperandsType( const Value* lhs, const Value* rhs ) const;
	};


#define DECLARE_LOG_EXPR(name, op, op_name)				\
class name : public LogExpr {						\
    public:								\
	name( Expr* op1, Expr* op2 )					\
		: LogExpr(op, op1, op2, op_name)	{ }		\
									\
	void Compute( glish_bool lhs[], glish_bool rhs[],		\
			glish_bool result[], int lhs_len, int rhs_incr );\
									\
	void Compute( int lhs[], int rhs[], glish_bool result[],	\
			int lhs_len, int rhs_incr )			\
		{ LogExpr::Compute(lhs,rhs,result,lhs_len,rhs_incr); }	\
	void Compute( float lhs[], float rhs[], glish_bool result[],	\
			int lhs_len, int rhs_incr )			\
		{ LogExpr::Compute(lhs,rhs,result,lhs_len,rhs_incr); }	\
	void Compute( double lhs[], double rhs[], glish_bool result[],	\
			int lhs_len, int rhs_incr )			\
		{ LogExpr::Compute(lhs,rhs,result,lhs_len,rhs_incr); }	\
	void Compute( complex lhs[], complex rhs[], glish_bool result[],\
			int lhs_len, int rhs_incr )			\
		{ LogExpr::Compute(lhs,rhs,result,lhs_len,rhs_incr); }	\
	void Compute( dcomplex lhs[], dcomplex rhs[], glish_bool result[],\
			int lhs_len, int rhs_incr )			\
		{ LogExpr::Compute(lhs,rhs,result,lhs_len,rhs_incr); }	\
	void Compute( charptr lhs[], charptr rhs[], glish_bool result[],\
			int lhs_len, int rhs_incr )			\
		{ LogExpr::Compute(lhs,rhs,result,lhs_len,rhs_incr); }	\
	void Compute( byte lhs[], byte rhs[], glish_bool result[],	\
			int lhs_len, int rhs_incr )			\
		{ LogExpr::Compute(lhs,rhs,result,lhs_len,rhs_incr); }	\
	void Compute( short lhs[], short rhs[], glish_bool result[],	\
			int lhs_len, int rhs_incr )			\
		{ LogExpr::Compute(lhs,rhs,result,lhs_len,rhs_incr); }	\
	};

DECLARE_LOG_EXPR(LogAndExpr, OP_AND, "&")
DECLARE_LOG_EXPR(LogOrExpr, OP_OR, "|")

#endif /* binopexpr_h */
