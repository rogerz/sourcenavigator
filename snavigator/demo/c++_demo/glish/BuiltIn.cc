// $Header$

#include <string.h>
#include <stream.h>
#include <stdlib.h>
#include <math.h>

// For MAXINT, MAXFLOAT, HUGE.
#include <values.h>

#include "Sds/sdsgen.h"
#include "glish_event.h"
#include "BuiltIn.h"
#include "Reporter.h"
#include "Task.h"
#include "Sequencer.h"
#include "Frame.h"

#if !defined(HUGE) /* this because it's not defined in the vxworks includes */
#define HUGE (infinity())
#define MAXINT 0x7fffffff

// Half-assed guess.
#define MAXFLOAT 1e38
#endif


Value* BuiltIn::Call( parameter_list* args, eval_type etype )
	{
	if ( num_args != NUM_ARGS_VARIES )
		{
		int num_args_present = 0;

		loop_over_list( *args, i )
			{
			if ( (*args)[i]->IsEllipsis() )
				num_args_present +=
					(*args)[i]->NumEllipsisVals();
			else
				++num_args_present;
			}

		if ( num_args_present != num_args )
			{
			error->Report( this, " takes", num_args, " argument",
					num_args == 1 ? ";" : "s;",
					num_args_present, " given" );

			return error_value();
			}
		}

	loop_over_list( *args, j )
		{
		Parameter* arg = (*args)[j];
		if ( ! arg->Arg() )
			{
			error->Report( "missing parameter invalid for", this );
			return error_value();
			}
		if ( arg->Name() )
			{
			error->Report( this,
					" does not have a parameter named \"",
					arg->Name(), "\"" );
			return error_value();
			}
		}

	const_args_list* args_vals = new const_args_list;

	int do_call = 1;

	loop_over_list( *args, i )
		{
		Parameter* arg = (*args)[i];
		const Value* arg_val;

		if ( arg->IsEllipsis() )
			{
			int len = arg->NumEllipsisVals();

			for ( int j = 0; j < len; ++j )
				{
				arg_val = arg->NthEllipsisVal( j );
				if ( do_deref )
					arg_val = arg_val->Deref();

				args_vals->append( arg_val );
				}
			}

		else
			{
			arg_val = arg->Arg()->ReadOnlyEval();
			if ( do_deref )
				arg_val = arg_val->Deref();

			args_vals->append( arg_val );
			}
		}

	Value* result;

	if ( do_call )
		{
		if ( etype == EVAL_SIDE_EFFECTS )
			{
			int side_effects_okay = 0;
			DoSideEffectsCall( args_vals, side_effects_okay );

			if ( ! side_effects_okay )
				warn->Report( "function return value ignored:",
						this );

			result = 0;
			}

		else
			result = DoCall( args_vals );
		}
	else
		result = error_value();

	loop_over_list( *args, k )
		{
		if ( ! (*args)[k]->IsEllipsis() )
			(*args)[k]->Arg()->ReadOnlyDone( (*args_vals)[k] );
		}

	delete args_vals;

	return result;
	}

void BuiltIn::DoSideEffectsCall( const_args_list* args_vals,
				int& side_effects_okay )
	{
	side_effects_okay = side_effects_call_okay;
	Unref( DoCall( args_vals ) );
	}

void BuiltIn::DescribeSelf( ostream& s ) const
	{
	s << description << "()";
	}

int BuiltIn::AllNumeric( const_args_list* args_vals, glish_type& max_type,
	int strings_okay )
	{
	max_type = TYPE_STRING;

	loop_over_list( *args_vals, i )
		{
		const Value* arg = (*args_vals)[i];

		if ( arg->IsNumeric() )
			{
			max_type = max_numeric_type( max_type, arg->Type() );
			continue;
			}

		if ( strings_okay && arg->Type() == TYPE_STRING )
			continue;

		error->Report( "argument #", i + 1, "to", this,
			"is not numeric", strings_okay ? " or a string" : "" );
		return 0;
		}

	return 1;
	}


Value* OneValueArgBuiltIn::DoCall( const_args_list* args_val )
	{
	return (*func)( (*args_val)[0] );
	}


Value* NumericVectorBuiltIn::DoCall( const_args_list* args_val )
	{
	const Value* arg = (*args_val)[0];
	Value* result;

	if ( ! arg->IsNumeric() )
		{
		error->Report( this, " requires a numeric argument" );
		return error_value();
		}

	int len = arg->Length();
	glish_type type = arg->Type();

#define NUMERIC_BUILTIN_ACTION(type,accessor,fn)	\
	{						\
	int is_copy;					\
	type* args_vec = arg->accessor( is_copy, len );	\
	type* stor = new type[len];			\
							\
	for ( int i = 0; i < len; ++i )			\
		stor[i] = (*fn)( args_vec[i] );		\
							\
	if ( is_copy )					\
		delete args_vec;			\
							\
	result = new Value( stor, len );		\
	result->CopyAttributes( arg );			\
	}

	if ( type == TYPE_COMPLEX || type == TYPE_DCOMPLEX )
		NUMERIC_BUILTIN_ACTION(dcomplex,CoerceToDcomplexArray,cfunc)
	else
		NUMERIC_BUILTIN_ACTION(double,CoerceToDoubleArray,func)

	return result;
	}

Value* RealBuiltIn::DoCall( const_args_list* args_val )
	{
	const Value* v = (*args_val)[0];

	if ( ! v->IsNumeric() )
		{
		error->Report( this, " requires a numeric argument" );
		return error_value();
		}

	Value* result;

#define RE_IM_BUILTIN_ACTION(tag,type,subtype,accessor,elem)	\
	case tag:						\
		{						\
		int is_copy;					\
		int len = v->Length();				\
		subtype* stor = new subtype[len];		\
		type* from = v->accessor( is_copy, len );	\
		for ( int i = 0; i < len; i++ )			\
			stor[i] = from[i] elem;			\
		if ( is_copy )					\
			delete from;				\
		result = new Value( stor, len );		\
		result->CopyAttributes( v );			\
		}						\
		break;

	switch ( v->Type() )
		{
RE_IM_BUILTIN_ACTION(TYPE_COMPLEX,complex,float,CoerceToComplexArray,.r)
RE_IM_BUILTIN_ACTION(TYPE_DCOMPLEX,dcomplex,double,CoerceToDcomplexArray,.r)

		default:
			result = copy_value(v);
		}

	return result;
	}

Value* ImagBuiltIn::DoCall( const_args_list* args_val )
	{
	const Value* v = (*args_val)[0];

	if ( ! v->IsNumeric() )
		{
		error->Report( this, " requires a numeric argument" );
		return error_value();
		}

	Value* result;

	switch ( v->Type() )
		{
RE_IM_BUILTIN_ACTION(TYPE_COMPLEX,complex,float,CoerceToComplexArray,.i)
RE_IM_BUILTIN_ACTION(TYPE_DCOMPLEX,dcomplex,double,CoerceToDcomplexArray,.i)

		default:
			result = new Value( 0.0 );
		}

	return result;
	}

Value* ComplexBuiltIn::DoCall( const_args_list* args_val )
	{
	int len = args_val->length();
	Value* result;

	if ( len < 1 || len > 2 )
		{
		error->Report( this, " takes 1 or 2 arguments" );
		return error_value();
		}

	if ( len == 2 )
		{
		const Value* rv = (*args_val)[0];
		const Value* iv = (*args_val)[1];

		if ( ! rv->IsNumeric() || ! iv->IsNumeric() )
			{
			error->Report( this,
				" requires one or two numeric arguments" );
			return error_value();
			}

		int rlen = rv->Length();
		int ilen = iv->Length();

		int rscalar = rlen == 1;
		int iscalar = ilen == 1;

		if ( rlen != ilen && ! rscalar && ! iscalar )
			{
			error->Report(
				"different-length operands in expression (",
					rlen, " vs. ", ilen, "):\n\t",
					this );
			return error_value();
			}

		glish_type maxt = max_numeric_type( rv->Type(), iv->Type() );

#define COMPLEXBUILTIN_TWOPARM_ACTION(tag,type,rettype,accessor,coerce)	\
	case tag:							\
		{							\
		int r_is_copy;						\
		int i_is_copy;						\
		int maxlen = rlen > ilen ? rlen : ilen;			\
		rettype* r = rv->accessor( r_is_copy, rlen );		\
		rettype* i = iv->accessor( i_is_copy, ilen );		\
		type* stor = new type[maxlen];				\
		for ( int cnt = 0; cnt < maxlen; ++cnt )		\
			{						\
			stor[cnt].r = coerce( r[rscalar ? 0 : cnt] );	\
			stor[cnt].i = coerce( i[iscalar ? 0 : cnt] );	\
			}						\
		if ( r_is_copy )					\
			delete r;					\
		if ( i_is_copy )					\
			delete i;					\
		result = new Value( stor, maxlen );			\
		}							\
		break;

		switch ( maxt )
			{
COMPLEXBUILTIN_TWOPARM_ACTION(TYPE_BOOL,complex,glish_bool,
	CoerceToBoolArray,float)
COMPLEXBUILTIN_TWOPARM_ACTION(TYPE_BYTE,complex,byte,CoerceToByteArray,float)
COMPLEXBUILTIN_TWOPARM_ACTION(TYPE_SHORT,complex,short,CoerceToShortArray,float)
COMPLEXBUILTIN_TWOPARM_ACTION(TYPE_INT,complex,int,CoerceToIntArray,float)
COMPLEXBUILTIN_TWOPARM_ACTION(TYPE_FLOAT,complex,float,CoerceToFloatArray,)
COMPLEXBUILTIN_TWOPARM_ACTION(TYPE_DOUBLE,dcomplex,double,CoerceToDoubleArray,)

			case TYPE_COMPLEX:
			case TYPE_DCOMPLEX:
				if ( rv->Type() == TYPE_COMPLEX ||
				     rv->Type() == TYPE_DCOMPLEX )
					result = copy_value( rv );
				else
					result = copy_value( iv );
				break;

			default:
				result = error_value();
			}
		}

	else
		{
		const Value* v = (*args_val)[0];

		if ( ! v->IsNumeric() )
			{
			error->Report( this,
				" requires one or two numeric arguments" );
			return error_value();
			}

#define COMPLEXBUILTIN_ONEPARM_ACTION(tag,type,rettype,accessor,coerce)	\
	case tag:						\
		{						\
		int is_copy;					\
		int vlen = v->Length();				\
		rettype* vp = v->accessor( is_copy, vlen );	\
		type* stor = new type[vlen];			\
		for ( int cnt = 0; cnt < vlen; ++cnt )		\
			{					\
			stor[cnt].r = coerce( vp[cnt] );	\
			stor[cnt].i = 0;			\
			}					\
		if ( is_copy )					\
			delete vp;				\
		result = new Value( stor, vlen );			\
		}						\
		break;

		switch ( v->Type() )
			{
COMPLEXBUILTIN_ONEPARM_ACTION(TYPE_BOOL,complex,glish_bool,
	CoerceToBoolArray,float)
COMPLEXBUILTIN_ONEPARM_ACTION(TYPE_BYTE,complex,byte,CoerceToByteArray,float)
COMPLEXBUILTIN_ONEPARM_ACTION(TYPE_SHORT,complex,short,CoerceToShortArray,float)
COMPLEXBUILTIN_ONEPARM_ACTION(TYPE_INT,complex,int,CoerceToIntArray,float)
COMPLEXBUILTIN_ONEPARM_ACTION(TYPE_FLOAT,complex,float,CoerceToFloatArray,)
COMPLEXBUILTIN_ONEPARM_ACTION(TYPE_DOUBLE,dcomplex,double,CoerceToDoubleArray,)

			case TYPE_COMPLEX:
			case TYPE_DCOMPLEX:
				result = copy_value( v );
				break;

			default:
				result = error_value();
			}
		}

	return result;
	}

Value* SumBuiltIn::DoCall( const_args_list* args_val )
	{
	glish_type max_type;
	Value* result;

	if ( ! AllNumeric( args_val, max_type ) )
		return error_value();

#define SUM_BUILTIN_ACTION(type,accessor)			\
	{							\
	type sum = 0.0;						\
	loop_over_list( *args_val, i )				\
		{						\
		const Value* val = (*args_val)[i];		\
		int len = val->Length();			\
		int is_copy;					\
		type* val_array = val->accessor(is_copy,len);	\
		for ( int j = 0; j < len; ++j )			\
			sum += val_array[j];			\
		if ( is_copy )					\
			delete val_array;			\
		}						\
	result = new Value( sum );				\
	}

	if ( max_type == TYPE_COMPLEX || max_type == TYPE_DCOMPLEX )
		SUM_BUILTIN_ACTION(dcomplex,CoerceToDcomplexArray)
	else
		SUM_BUILTIN_ACTION(double,CoerceToDoubleArray)

	return result;
	}

Value* ProdBuiltIn::DoCall( const_args_list* args_val )
	{
	glish_type max_type;
	Value* result;

	if ( ! AllNumeric( args_val, max_type ) )
		return error_value();

	switch ( max_type )
		{
#define PRODBUILTIN_ACTION(type,accessor)				\
		{							\
		type prod = 1.0;					\
		loop_over_list( *args_val, i )				\
			{						\
			const Value* val = (*args_val)[i];		\
			int len = val->Length();			\
			int is_copy;					\
			type* val_array = val->accessor(is_copy, len);	\
			for ( int j = 0; j < len; ++j )			\
				prod *= val_array[j];			\
			if ( is_copy )					\
				delete val_array;			\
			}						\
		result = new Value( prod );				\
		break;							\
		}

		case TYPE_COMPLEX:
		case TYPE_DCOMPLEX:
			PRODBUILTIN_ACTION(dcomplex,CoerceToDcomplexArray)

		case TYPE_BOOL:
		case TYPE_BYTE:
		case TYPE_SHORT:
		case TYPE_INT:
		case TYPE_FLOAT:
		case TYPE_DOUBLE:
			PRODBUILTIN_ACTION(double,CoerceToDoubleArray)

		default:
			error->Report( "bad type in ProdBuiltIn::DoCall()" );
			return 0;
		}

	return result;
	}

Value* LengthBuiltIn::DoCall( const_args_list* args_val )
	{
	int num = args_val->length();

	if ( num > 1 )
		{
		int* len = new int[args_val->length()];
		loop_over_list( *args_val, i )
			len[i] = (*args_val)[i]->Length();
		return new Value( len, num );
		}

	else if ( num == 1 )
		return new Value( int( (*args_val)[0]->Length() ) );

	else
		return empty_value();
	}

Value* RangeBuiltIn::DoCall( const_args_list* args_val )
	{
	glish_type max_type;
	Value* result;

	if ( ! AllNumeric( args_val, max_type ) )
		return error_value();

#define RANGEBUILTIN_ACTION(tag,type,accessor,max)			\
	case tag:							\
		{							\
		type min_val = (type) max;				\
		type max_val = (type) -max;				\
									\
		loop_over_list( *args_val, i )				\
			{						\
			const Value* val = (*args_val)[i];		\
			int len = val->Length();			\
			int is_copy;					\
									\
			type* val_array = val->accessor( is_copy, len );\
									\
			for ( int j = 0; j < len; ++j )			\
				{					\
				if ( val_array[j] < min_val )		\
					min_val = val_array[j];		\
									\
				if ( val_array[j] > max_val )		\
					max_val = val_array[j];		\
				}					\
									\
			if ( is_copy )					\
				delete val_array;			\
			}						\
		type* range = new type[2];				\
		range[0] = min_val;					\
		range[1] = max_val;					\
									\
		result = new Value( range, 2 );				\
		}							\
			break;

	switch ( max_type )
		{
RANGEBUILTIN_ACTION(TYPE_DCOMPLEX,dcomplex,CoerceToDcomplexArray,HUGE)
RANGEBUILTIN_ACTION(TYPE_COMPLEX,complex,CoerceToComplexArray,MAXFLOAT)
RANGEBUILTIN_ACTION(TYPE_DOUBLE,double,CoerceToDoubleArray,HUGE)
RANGEBUILTIN_ACTION(TYPE_FLOAT,float,CoerceToFloatArray,MAXFLOAT)
		case TYPE_BOOL:
		case TYPE_BYTE:
		case TYPE_SHORT:
RANGEBUILTIN_ACTION(TYPE_INT,int,CoerceToIntArray,MAXINT)
		default:
			result = error_value();
		}

	return result;
	}

Value* SeqBuiltIn::DoCall( const_args_list* args_val )
	{
	int len = args_val->length();

	if ( len == 0 || len > 3 )
		{
		error->Report( this, " takes from one to three arguments" );
		return error_value();
		}

	double starting_point = 1.0;
	double stopping_point;
	double stride = 1.0;

	const Value* arg;

	if ( len == 1 )
		{
		arg = (*args_val)[0];

		if ( arg->Length() != 1 )
			stopping_point = double( arg->Length() );
		else
			stopping_point = double( arg->IntVal() );
		}

	else
		{
		starting_point = (*args_val)[0]->DoubleVal();
		stopping_point = (*args_val)[1]->DoubleVal();

		if ( len == 3 )
			stride = (*args_val)[2]->DoubleVal();

		else if ( starting_point > stopping_point )
			stride = -1;
		}

	if ( stride == 0 )
		{
		error->Report( "in call to ", this, ", stride = 0" );
		return error_value();
		}

	if ( (starting_point < stopping_point && stride < 0) ||
	     (starting_point > stopping_point && stride > 0) )
		{
		error->Report( "in call to ", this,
				", stride has incorrect sign" );
		return error_value();
		}

	double range = stopping_point - starting_point;
	int num_vals = int( range / stride ) + 1;

	if ( num_vals > 1e6 )
		{
		error->Report( "ridiculously large sequence in call to ",
				this );
		return error_value();
		}

	double* result = new double[num_vals];

	double val = starting_point;
	for ( int i = 0; i < num_vals; ++i )
		{
		result[i] = val;
		val += stride;
		}

	Value* result_val = new Value( result, num_vals );

	if ( starting_point == double( int( starting_point ) ) &&
	     stopping_point == double( int( stopping_point ) ) &&
	     stride == double( int( stride ) )  )
		result_val->Polymorph( TYPE_INT );

	return result_val;
	}

Value* RepBuiltIn::DoCall( const_args_list* args_val )
	{
	const Value* element = (*args_val)[0];
	const Value* times = (*args_val)[1];

	if ( ! times->IsNumeric() )
		{
		error->Report( "non-numeric parameters invalid for", this );
		return error_value();
		}

	if ( times->Length() != 1 && times->Length() != element->Length() )
		{
		error->Report( this,
				": parameter vectors have unequal lengths" );
		return error_value();
		}

	int times_is_copy;
	int times_len = times->Length();
	int* times_vec = times->CoerceToIntArray( times_is_copy, times_len );

	for ( int x = 0; x < times_len; ++x )
		if ( times_vec[x] < 0 )
			{
			error->Report( "invalid replication parameter, 2nd (",
					times_vec[x], "), in ", this );
			if ( times_is_copy )
				delete times_vec;
			return error_value();
			}

	Value* ret = 0;
	if ( times_len > 1 )
		{
		// Here we know that BOTH the length of the element and the
		// length of the multiplier are greater than zero.
		int off = 0;
		int veclen = 0;

		for ( int i = 0; i < times_len; ++i )
			veclen += times_vec[i];

		switch ( element->Type() )
			{
#define REPBUILTIN_ACTION_A(tag,type,accessor,copy_func)			\
			case tag:					\
				{					\
				type* vec = new type[veclen];		\
				type* elm = element->accessor;			\
				for ( i=0; i < times_len; ++i )		\
					for ( int j=0; j < times_vec[i]; ++j )\
					  vec[off++] = copy_func( elm[i] );	\
				ret = new Value( vec, veclen );		\
				}					\
				break;

		REPBUILTIN_ACTION_A(TYPE_BOOL,glish_bool,BoolPtr(),)
		REPBUILTIN_ACTION_A(TYPE_BYTE,byte,BytePtr(),)
		REPBUILTIN_ACTION_A(TYPE_SHORT,short,ShortPtr(),)
		REPBUILTIN_ACTION_A(TYPE_INT,int,IntPtr(),)
		REPBUILTIN_ACTION_A(TYPE_FLOAT,float,FloatPtr(),)
		REPBUILTIN_ACTION_A(TYPE_DOUBLE,double,DoublePtr(),)
		REPBUILTIN_ACTION_A(TYPE_COMPLEX,complex,ComplexPtr(),)
		REPBUILTIN_ACTION_A(TYPE_DCOMPLEX,dcomplex,DcomplexPtr(),)
		REPBUILTIN_ACTION_A(TYPE_STRING,charptr,StringPtr(),strdup)

			default:
				error->Report(
					"bad type in RepBuiltIn::DoCall()" );
			}
		}
	else
		{
		int len = times_vec[0];

		if ( element->Length() == 1 )
			{
			switch ( element->Type() )
				{
#define REPBUILTIN_ACTION_B(tag,type,accessor,copy_func,CLEANUP_VAL)	\
				case tag:				\
					{				\
					type val = element->accessor();	\
					type *vec = new type[len];	\
					for (int i = 0; i < len; i++)	\
						vec[i] = copy_func(val);\
					ret = new Value( vec, len );	\
					CLEANUP_VAL			\
					}				\
					break;

			REPBUILTIN_ACTION_B(TYPE_BOOL,glish_bool,BoolVal,,)
			REPBUILTIN_ACTION_B(TYPE_BYTE,byte,ByteVal,,)
			REPBUILTIN_ACTION_B(TYPE_SHORT,short,ShortVal,,)
			REPBUILTIN_ACTION_B(TYPE_INT,int,IntVal,,)
			REPBUILTIN_ACTION_B(TYPE_FLOAT,float,FloatVal,,)
			REPBUILTIN_ACTION_B(TYPE_DOUBLE,double,DoubleVal,,)
			REPBUILTIN_ACTION_B(TYPE_COMPLEX,complex,ComplexVal,,)
			REPBUILTIN_ACTION_B(TYPE_DCOMPLEX,dcomplex,DcomplexVal,,)
			REPBUILTIN_ACTION_B(TYPE_STRING,charptr,StringVal,strdup,delete (char *)val;)

				default:
					error->Report(
					"bad type in RepBuiltIn::DoCall()" );
				}
			}
		else
			{
			int off = 0;
			int repl = times_vec[0];
			int e_len = element->Length();
			int veclen = e_len * repl;

			switch ( element->Type() )
				{
#define REPBUILTIN_ACTION_C(tag,type,accessor,copy_func)		\
			case tag:					\
				{					\
				type* val = element->accessor;		\
				type* vec = new type[veclen];		\
				for ( int j = 0; j < repl; ++j )	\
					for ( int i = 0; i < e_len; ++i )\
						vec[off++] =  copy_func(val[i]);\
				ret = new Value( vec, veclen );		\
				}					\
				break;

	REPBUILTIN_ACTION_C(TYPE_BOOL,glish_bool,BoolPtr(),)
	REPBUILTIN_ACTION_C(TYPE_BYTE,byte,BytePtr(),)
	REPBUILTIN_ACTION_C(TYPE_SHORT,short,ShortPtr(),)
	REPBUILTIN_ACTION_C(TYPE_INT,int,IntPtr(),)
	REPBUILTIN_ACTION_C(TYPE_FLOAT,float,FloatPtr(),)
	REPBUILTIN_ACTION_C(TYPE_DOUBLE,double,DoublePtr(),)
	REPBUILTIN_ACTION_C(TYPE_COMPLEX,complex,ComplexPtr(),)
	REPBUILTIN_ACTION_C(TYPE_DCOMPLEX,dcomplex,DcomplexPtr(),)
	REPBUILTIN_ACTION_C(TYPE_STRING,charptr,StringPtr(),strdup)

				default:
					error->Report(
					"bad type in RepBuiltIn::DoCall()" );
				}
			}
		}

	if ( times_is_copy )
		delete times_vec;

	return ret ? ret : error_value();
	}

Value* NumArgsBuiltIn::DoCall( const_args_list* args_val )
	{
	return new Value( args_val->length() );
	}

Value* NthArgBuiltIn::DoCall( const_args_list* args_val )
	{
	int len = args_val->length();

	if ( len <= 0 )
		{
		error->Report( "first argument missing in call to", this );
		return error_value();
		}

	int n = (*args_val)[0]->IntVal();

	if ( n < 0 || n >= len )
		{
		error->Report( "first argument (=", n, ") to", this,
				" out of range: ", len - 1,
				"additional arguments supplied" );
		return error_value();
		}

	return copy_value( (*args_val)[n] );
	}

Value* MissingBuiltIn::DoCall( const_args_list* /* args_val */ )
	{
	Frame* cur = sequencer->CurrentFrame();
	if ( ! cur )
		return empty_value();

	return copy_value( cur->Missing() );
	}


Value* PasteBuiltIn::DoCall( const_args_list* args_val )
	{
	if ( args_val->length() == 0 )
		{
		error->Report( "paste() invoked with no arguments" );
		return error_value();
		}

	// First argument gives separator string.
	char* separator = (*args_val)[0]->StringVal();

	charptr* string_vals = new charptr[args_val->length()];

	int len = 1;	// Room for end-of-string.
	int sep_len = strlen( separator );

	for ( int i = 1; i < args_val->length(); ++i )
		{
		string_vals[i] = (*args_val)[i]->StringVal( ' ', 1 );
		len += strlen( string_vals[i] ) + sep_len;
		}

	char* paste_val = new char[len];
	paste_val[0] = '\0';

	for ( int j = 1; j < i; ++j )
		{
		strcat( paste_val, string_vals[j] );

		if ( j < i - 1 )
			strcat( paste_val, separator );

		delete (char*) string_vals[j];
		}

	delete string_vals;
	delete separator;

	charptr* result = new charptr[1];
	result[0] = paste_val;

	return new Value( result, 1 );
	}

Value* SplitBuiltIn::DoCall( const_args_list* args_val )
	{
	int len = args_val->length();

	if ( len < 1 || len > 2 )
		{
		error->Report( this, " takes 1 or 2 arguments" );
		return error_value();
		}

	char* source = (*args_val)[0]->StringVal();

	char* split_chars = " \t\n";
	if ( len == 2 )
		split_chars = (*args_val)[1]->StringVal();

	Value* result = split( source, split_chars );

	delete source;
	if ( len == 2 )
		delete split_chars;

	return result;
	}


Value* ReadValueBuiltIn::DoCall( const_args_list* args_val )
	{
	char* file_name = (*args_val)[0]->StringVal();

	int sds = (int) sds_access( file_name, SDS_FILE, SDS_READ );

	Value* result;

	if ( sds < 0 )
		{
		error->Report( "could not read value from \"", file_name,
				"\"" );
		result = error_value();
		}

	else
		result = read_value_from_SDS( sds );

	delete file_name;

	return result;
	}


Value* WriteValueBuiltIn::DoCall( const_args_list* args_val )
	{
	char* file_name = (*args_val)[1]->StringVal();
	const Value* v = (*args_val)[0];

	int result = 1;

	if ( v->Type() == TYPE_OPAQUE )
		{
		int sds = v->SDS_IndexVal();

		if ( sds_ass( sds, file_name, SDS_FILE ) != sds )
			{
			error->Report( "could not save opaque value to \"",
					file_name, "\"" );
			result = 0;
			}
		}

	else
		{
		int sds = (int) sds_new( (char*) "" );

		if ( sds < 0 )
			{
			error->Report( "problem saving value to \"", file_name,
					"\", SDS error code = ", sds );
			result = 0;
			}

		else
			{
			del_list d;

			(*args_val)[0]->AddToSds( sds, &d );

			if ( sds_ass( sds, file_name, SDS_FILE ) != sds )
				{
				error->Report( "could not save value to \"",
						file_name, "\"" );
				result = 0;
				}

			sds_destroy( sds );

			delete_list( &d );
			}
		}

	delete file_name;

	return new Value( result );
	}


Value* WheneverStmtsBuiltIn::DoCall( const_args_list* args_val )
	{
	Agent* agent = (*args_val)[0]->AgentVal();

	if ( ! agent )
		return error_value();

	else
		return agent->AssociatedStatements();
	}


Value* ActiveAgentsBuiltIn::DoCall( const_args_list* /* args_val */ )
	{
	Value* r = create_record();

	loop_over_list( agents, i )
		{
		Value* a = agents[i]->AgentRecord();
		r->SetField( r->NewFieldName(), new Value( a, VAL_REF ) );
		}

	return r;
	}


Value* CreateAgentBuiltIn::DoCall( const_args_list* /* args_val */ )
	{
	Agent* user_agent = new UserAgent( sequencer );
	return user_agent->AgentRecord();
	}


Value* CurrentWheneverBuiltIn::DoCall( const_args_list* /* args_val */ )
	{
	Notification* n = sequencer->LastNotification();

	if ( ! n )
		{
		error->Report( "no active whenever, in call to", this );
		return new Value( 0 );
		}

	return new Value( n->notifiee->stmt->Index() );
	}

Value* LastWheneverExecutedBuiltIn::DoCall( const_args_list* /* args_val */ )
	{
	Stmt* s = sequencer->LastWheneverExecuted();

	if ( ! s )
		{
		error->Report( "no whenever's executed, in call to", this );
		return new Value( 0 );
		}

	return new Value( s->Index() );
	}


#define DEFINE_AS_XXX_BUILT_IN(name,type,tag,stringcvt,coercer,text,zero) \
Value* name( const Value* arg )						\
	{								\
	int len = arg->Length();					\
									\
	if ( arg->Type() == TYPE_STRING )				\
		{							\
		const charptr* strings = arg->StringPtr();		\
		type* result = new type[len];				\
									\
		for ( int i = 0; i < len; ++i )				\
			result[i] = stringcvt( strings[i] );		\
									\
		return new Value( result, len );			\
		}							\
									\
	if ( ! arg->IsNumeric() )					\
		{							\
		error->Report( "non-numeric argument to ", text );	\
		return new Value( type(zero) );				\
		}							\
									\
	if ( arg->Type() == tag )					\
		return copy_value( arg );				\
									\
	int is_copy;							\
	type* result = arg->coercer( is_copy, len );			\
									\
	Value* ret = new Value( result, len );				\
	ret->CopyAttributes( arg );					\
	return ret;							\
	}

glish_bool string_to_bool( const char* string )
	{
	int successful;
	double d = text_to_double( string, successful );
	if ( successful )
		return glish_bool( int( d ) );
	else
		return glish_false;
	}

DEFINE_AS_XXX_BUILT_IN(as_boolean_built_in, glish_bool, TYPE_BOOL,
	string_to_bool, CoerceToBoolArray, "as_boolean", glish_false)

DEFINE_AS_XXX_BUILT_IN(as_short_built_in, short, TYPE_SHORT, atoi,
	CoerceToShortArray, "as_short", 0)

DEFINE_AS_XXX_BUILT_IN(as_integer_built_in, int, TYPE_INT, atoi,
	CoerceToIntArray, "as_integer", 0)

DEFINE_AS_XXX_BUILT_IN(as_float_built_in, float, TYPE_FLOAT, atof,
	CoerceToFloatArray, "as_float", 0.0)

DEFINE_AS_XXX_BUILT_IN(as_double_built_in, double, TYPE_DOUBLE, atof,
	CoerceToDoubleArray, "as_double", 0.0)

DEFINE_AS_XXX_BUILT_IN(as_complex_built_in, complex, TYPE_COMPLEX, atocpx,
	CoerceToComplexArray, "as_complex", complex(0.0, 0.0))

DEFINE_AS_XXX_BUILT_IN(as_dcomplex_built_in, dcomplex, TYPE_DCOMPLEX, atodcpx,
	CoerceToDcomplexArray, "as_dcomplex", dcomplex(0.0, 0.0))

Value* as_byte_built_in( const Value* arg )
	{
	if ( arg->Type() == TYPE_STRING )
		{
		char* arg_str = arg->StringVal();
		int len = strlen( arg_str );
		byte* result = new byte[len];

		for ( int i = 0; i < len; ++i )
			result[i] = byte(arg_str[i]);

		delete arg_str;

		return new Value( result, len );
		}

	int len = arg->Length();
	if ( ! arg->IsNumeric() )
		{
		error->Report( "non-numeric argument to ", "byte" );
		return new Value( byte(0) );
		}

	if ( arg->Type() == TYPE_BYTE )
		return copy_value( arg );

	int is_copy;
	byte* result = arg->CoerceToByteArray( is_copy, len );

	return new Value( result, len );
	}


Value* as_string_built_in( const Value* arg )
	{
	if ( arg->Type() == TYPE_STRING )
		return copy_value( arg );

	if ( ! arg->IsNumeric() )
		{
		error->Report( "non-numeric argument to as_string()" );
		return new Value( "" );
		}

	int len = arg->Length();

	if ( arg->Type() == TYPE_BYTE )
		{
		byte* vals = arg->BytePtr();
		char* s = new char[len+1];

		for ( int i = 0; i < len; ++i )
			s[i] = char(vals[i]);

		s[i] = '\0';

		Value* result = new Value( s );
		delete s;

		return result;
		}

	charptr* result = new charptr[len];
	int i;
	char buf[256];

	switch ( arg->Type() )
		{
		case TYPE_BOOL:
			{
			glish_bool* vals = arg->BoolPtr();
			for ( i = 0; i < len; ++i )
				result[i] = strdup( vals[i] ? "T" : "F" );
			}
			break;

#define COMMA_SEPARATED_SERIES(x,y) x,y
#define COERCE_XXX_TO_STRING(tag,type,accessor,format,rest)		\
	case tag:							\
		{							\
		type* vals = arg->accessor();				\
		for ( i = 0; i < len; ++i )				\
			{						\
			sprintf( buf, format, vals[i] rest );		\
			result[i] = strdup( buf );			\
			}						\
		}							\
		break;

		COERCE_XXX_TO_STRING(TYPE_SHORT,short,ShortPtr,"%d",)
		COERCE_XXX_TO_STRING(TYPE_INT,int,IntPtr,"%d",)
		COERCE_XXX_TO_STRING(TYPE_FLOAT,float,FloatPtr,"%.6g",)
		COERCE_XXX_TO_STRING(TYPE_DOUBLE,double,DoublePtr,"%.12g",)
		COERCE_XXX_TO_STRING(TYPE_COMPLEX,complex,ComplexPtr,(vals[i].i>=0.0?"%.6g+%.6g":"%.6g%.6g"),COMMA_SEPARATED_SERIES(.r,vals[i].i))
		COERCE_XXX_TO_STRING(TYPE_DCOMPLEX,dcomplex,DcomplexPtr,(vals[i].i>=0.0?"%.12g+%.12g":"%.12g%.12g"),COMMA_SEPARATED_SERIES(.r,vals[i].i))

		default:
			fatal->Report( "bad type tag in as_string()" );
		}

	return new Value( result, len );
	}


Value* type_name_built_in( const Value* arg )
	{
	glish_type t = arg->Type();

	if ( t == TYPE_REF || t == TYPE_CONST )
		{
		Value* deref_val = type_name_built_in( arg->RefPtr() );
		char* deref_name = deref_val->StringVal();

		char buf[512];

		sprintf( buf, "%s %s", t == TYPE_REF ? "ref" : "const",
			deref_name );

		delete deref_name;
		Unref( deref_val );

		return new Value( buf );
		}

	if ( arg->IsVecRef() )
		t = arg->VecRefDeref()->Type();

	return new Value( type_names[t] );
	}

Value* length_built_in( const Value* arg )
	{
	return new Value( int( arg->Length() ) );
	}

Value* field_names_built_in( const Value* arg )
	{
	if ( arg->Type() != TYPE_RECORD )
		{
		error->Report( "argument to field_names is not a record" );
		return error_value();
		}

	recordptr record_dict = arg->RecordPtr();
	IterCookie* c = record_dict->InitForIteration();

	charptr* names = new charptr[record_dict->Length()];
	const char* key;

	for ( int i = 0; record_dict->NextEntry( key, c ); ++i )
		names[i] = strdup( key );

	return new Value( names, i );
	}


char* paste( parameter_list* args )
	{
	PasteBuiltIn paste;

	// Create another parameter list with the separator at the
	// beginning.
	parameter_list args2;
	Value sep( " " );
	ConstExpr sep_expr( &sep );
	Parameter sep_parm( 0, VAL_CONST, &sep_expr );

	args2.append( &sep_parm );

	loop_over_list( *args, i )
		args2.append( (*args)[i] );

	Value* args_value = paste.Call( &args2, EVAL_COPY );

	// ### could save on some string copies here by returning the
	// value instead, and using StringPtr() instead of StringVal()
	// to get its string value.
	char* result = args_value->StringVal();
	Unref( args_value );

	return result;
	}


char* paste( const_args_list* args )
	{
	PasteBuiltIn paste;

	// Create another args list with the separator at the beginning.
	const_args_list args2;
	Value sep( " " );
	args2.append( &sep );

	loop_over_list( *args, i )
		args2.append( (*args)[i] );

	Value* args_value = paste.DoCall( &args2 );
	char* result = args_value->StringVal();
	Unref( args_value );

	return result;
	}


Value* split( char* source, char* split_chars )
	{
	// First see how many pieces the split will result in.
	int num_pieces = 0;
	char* source_copy = strdup( source );
	charptr next_string = strtok( source_copy, split_chars );
	while ( next_string )
		{
		++num_pieces;
		next_string = strtok( 0, split_chars );
		}
	delete source_copy;

	charptr* strings = new charptr[num_pieces];
	charptr* sptr = strings;
	next_string = strtok( source, split_chars );
	while ( next_string )
		{
		*(sptr++) = strdup( next_string );
		next_string = strtok( 0, split_chars );
		}

	return new Value( strings, num_pieces );
	}


static void add_one_arg_built_in( Sequencer* s, value_func_1_value_arg func,
					const char* name, int do_deref = 1 )
	{
	BuiltIn* b = new OneValueArgBuiltIn( func, name );
	b->SetDeref( do_deref );
	s->AddBuiltIn( b );
	}

//
//### Dummy complex functions
//	These should be supplied later, probably from the `fn' library
//	on `netlib'.
//
dcomplex asin( const dcomplex )
	{
	error->Report( "Sorry, complex arcsine not yet implemented" );
	return dcomplex( 0, 0 );
	}
dcomplex acos( const dcomplex )
	{
	error->Report( "Sorry, complex arccosine not yet implemented" );
	return dcomplex( 0, 0 );
	}
dcomplex atan( const dcomplex )
	{
	error->Report( "Sorry, complex arctangent not yet implemented" );
	return dcomplex( 0, 0 );
	}

void create_built_ins( Sequencer* s )
	{
	add_one_arg_built_in( s, as_boolean_built_in, "as_boolean" );
	add_one_arg_built_in( s, as_byte_built_in, "as_byte" );
	add_one_arg_built_in( s, as_short_built_in, "as_short" );
	add_one_arg_built_in( s, as_integer_built_in, "as_integer" );
	add_one_arg_built_in( s, as_float_built_in, "as_float" );
	add_one_arg_built_in( s, as_double_built_in, "as_double" );
	add_one_arg_built_in( s, as_complex_built_in, "as_complex" );
	add_one_arg_built_in( s, as_dcomplex_built_in, "as_dcomplex" );
	add_one_arg_built_in( s, as_string_built_in, "as_string" );

	add_one_arg_built_in( s, type_name_built_in, "type_name", 0 );
	add_one_arg_built_in( s, field_names_built_in, "field_names" );

	s->AddBuiltIn( new NumericVectorBuiltIn( sqrt, sqrt, "sqrt" ) );
	s->AddBuiltIn( new NumericVectorBuiltIn( exp, exp, "exp" ) );
	s->AddBuiltIn( new NumericVectorBuiltIn( log, log, "log" ) );
	s->AddBuiltIn( new NumericVectorBuiltIn( sin, sin, "sin" ) );
	s->AddBuiltIn( new NumericVectorBuiltIn( cos, cos, "cos" ) );
	s->AddBuiltIn( new NumericVectorBuiltIn( tan, tan, "tan" ) );
	s->AddBuiltIn( new NumericVectorBuiltIn( asin, asin, "asin" ) );
	s->AddBuiltIn( new NumericVectorBuiltIn( acos, acos, "acos" ) );
	s->AddBuiltIn( new NumericVectorBuiltIn( atan, atan, "atan" ) );

	s->AddBuiltIn( new RealBuiltIn );
	s->AddBuiltIn( new ImagBuiltIn );
	s->AddBuiltIn( new ComplexBuiltIn );

	s->AddBuiltIn( new SumBuiltIn );
	s->AddBuiltIn( new ProdBuiltIn );
	s->AddBuiltIn( new LengthBuiltIn );
	s->AddBuiltIn( new RangeBuiltIn );
	s->AddBuiltIn( new SeqBuiltIn );
	s->AddBuiltIn( new RepBuiltIn );
	s->AddBuiltIn( new NumArgsBuiltIn );
	s->AddBuiltIn( new NthArgBuiltIn );
	s->AddBuiltIn( new MissingBuiltIn( s ) );

	s->AddBuiltIn( new PasteBuiltIn );
	s->AddBuiltIn( new SplitBuiltIn );

	s->AddBuiltIn( new ReadValueBuiltIn );
	s->AddBuiltIn( new WriteValueBuiltIn );

	s->AddBuiltIn( new WheneverStmtsBuiltIn );

	s->AddBuiltIn( new ActiveAgentsBuiltIn );

	s->AddBuiltIn( new CreateAgentBuiltIn( s ) );
	s->AddBuiltIn( new CreateTaskBuiltIn( s ) );

	s->AddBuiltIn( new LastWheneverExecutedBuiltIn( s ) );
	s->AddBuiltIn( new CurrentWheneverBuiltIn( s ) );

	sds_init();
	}
