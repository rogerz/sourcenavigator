// $Header$

#ifndef complex_h
#define complex_h

#include <iostream.h>

//
// Complex types
//

struct complex;
struct dcomplex;

struct complex {
	complex() {}
	complex( float rv, float iv ) : r(rv), i(iv) {}
	complex( float rv ) : r(rv), i(float(0)) {}
	complex( const complex& cv ) : r(cv.r), i(cv.i) {}
	complex( const dcomplex& cv );

	float r;
	float i;
};

struct dcomplex {
	dcomplex() {}
	dcomplex( double rv, double iv ) : r(rv), i(iv) {}
	dcomplex( double rv ) : r(rv), i(double(0)) {}
	dcomplex( const complex& cv ) : r(cv.r), i(cv.i) {}
	dcomplex( const dcomplex& cv ) : r(cv.r), i(cv.i) {}

	double r;
	double i;
};

inline complex::complex( const dcomplex& cv )
	{
	r = cv.r;
	i = cv.i;
	}


inline float norm( const complex x )
	{
	return x.r * x.r + x.i * x.i;
	}

inline double norm( const dcomplex x )
	{
	return x.r * x.r + x.i * x.i;
	}

#define COMPLEX_CPX_BINOP(type,lhs_type,rhs_type,cast,op) 		\
inline type operator op( const lhs_type x, const rhs_type y ) 		\
	{								\
	return type((cast) x.r op (cast) y.r, (cast) x.r op (cast) y.r);\
	}

#define COMPLEX_CPX_ASGNOP(cpx_type,blt_type,cast,op)			\
inline void operator op( cpx_type &x, const blt_type y ) 		\
	{								\
	x.r op (cast) y.r;						\
	x.i op (cast) y.i;						\
	}

#define COMPLEX_BLT_BINOP(type,cpx_type,blt_type,cast,op)		\
inline type operator op( const cpx_type x, const blt_type y ) 		\
	{								\
	return type((cast) x.r op (cast) y, (cast) x.i);		\
	}								\
									\
inline type operator op( const blt_type y, const cpx_type x )		\
	{								\
	return type((cast) y op (cast) x.r, (cast) x.i);		\
	}

#define COMPLEX_BLT_ASGNOP( cpx_type,blt_type,cast,op )			\
inline void operator op (cpx_type &x, const blt_type y)			\
	{								\
	x.r op (cast) y;						\
	}								\
									\
inline void operator op( blt_type &y, const cpx_type x )		\
	{								\
	y op (blt_type) x.r;						\
	}

#define COMPLEX_OP(op)							\
COMPLEX_CPX_BINOP(complex,complex,complex,float,op)			\
COMPLEX_CPX_BINOP(dcomplex,dcomplex,complex,double,op)			\
COMPLEX_CPX_BINOP(dcomplex,complex,dcomplex,double,op)			\
COMPLEX_CPX_BINOP(dcomplex,dcomplex,dcomplex,double,op)			\
COMPLEX_CPX_ASGNOP(complex,complex,float,op##=)				\
COMPLEX_CPX_ASGNOP(dcomplex,complex,double,op##=)			\
COMPLEX_CPX_ASGNOP(complex,dcomplex,float,op##=)			\
COMPLEX_CPX_ASGNOP(dcomplex,dcomplex,double,op##=)			\
COMPLEX_BLT_BINOP(complex,complex,float,float,op)			\
COMPLEX_BLT_BINOP(dcomplex,complex,double,float,op)			\
COMPLEX_BLT_BINOP(dcomplex,dcomplex,float,double,op)			\
COMPLEX_BLT_BINOP(dcomplex,dcomplex,double,double,op)			\
COMPLEX_BLT_ASGNOP(complex,float,float,op##=)				\
COMPLEX_BLT_ASGNOP(complex,double,float,op##=)				\
COMPLEX_BLT_ASGNOP(dcomplex,float,double,op##=)				\
COMPLEX_BLT_ASGNOP(dcomplex,double,double,op##=)

// COMPLEX_OP(+)
// COMPLEX_OP(-)

#define COMPLEX_CPX_ASSIGN(lhs_type,rhs_type,cast)		\
inline lhs_type operator=(lhs_type &x, const rhs_type y)	\
	{							\
	x.r = (cast) y.r;					\
	x.i = (cast) y.i;					\
	return x;						\
	}

#define COMPLEX_BLT_ASSIGN(lhs_type,rhs_type,cast)		\
inline lhs_type operator=(lhs_type &x, const rhs_type y)	\
	{							\
	x.r = (cast) y;						\
	x.i = (cast) 0;						\
	return x;						\
	}

#define BLT_COMPLEX_ASSIGN(lhs_type,rhs_type,cast)		\
inline lhs_type operator=(lhs_type &x, const rhs_type y)	\
	{							\
	x = (cast) y.r;						\
	return x;						\
	}
//
// COMPLEX_CPX_ASSIGN(complex,complex,float)
// COMPLEX_CPX_ASSIGN(complex,dcomplex,float)
// COMPLEX_CPX_ASSIGN(dcomplex,complex,double)
// COMPLEX_CPX_ASSIGN(dcomplex,dcomplex,double)
// COMPLEX_BLT_ASSIGN(complex,float,float)
// COMPLEX_BLT_ASSIGN(complex,double,float)
// COMPLEX_BLT_ASSIGN(dcomplex,float,double)
// COMPLEX_BLT_ASSIGN(dcomplex,double,double)
// BLT_COMPLEX_ASSIGN(float,complex,float)
// BLT_COMPLEX_ASSIGN(float,dcomplex,float)
// BLT_COMPLEX_ASSIGN(double,complex,double)
// BLT_COMPLEX_ASSIGN(double,dcomplex,double)
//

COMPLEX_CPX_ASGNOP(dcomplex,dcomplex,double,+=)
COMPLEX_CPX_ASGNOP(dcomplex,dcomplex,double,*=)


//
// Defined to be consistent with S
//
#define COMPLEX_LOGOP(type,op)					\
inline int operator op (type x, type y)				\
	{							\
	return ( x.r == y.r ? x.i op y.i : x.r op y.r );	\
	}

#define COMPLEX_LOGOP_SET(type)	\
COMPLEX_LOGOP(type,>)		\
COMPLEX_LOGOP(type,>=)		\
COMPLEX_LOGOP(type,<)		\
COMPLEX_LOGOP(type,<=)		\
COMPLEX_LOGOP(type,==)		\
COMPLEX_LOGOP(type,!=)

COMPLEX_LOGOP_SET(complex)
COMPLEX_LOGOP_SET(dcomplex)

inline complex mul( const complex x, const complex y )
	{
	return complex( x.r*y.r - x.i*y.i, x.r*y.i + x.i*y.r );
	}

inline dcomplex mul( const dcomplex x, const dcomplex y )
	{
	return dcomplex( x.r*y.r - x.i*y.i, x.r*y.i + x.i*y.r );
	}

extern complex atocpx(const char text[]);
extern dcomplex atodcpx(const char text[]);
extern dcomplex mul(const dcomplex x, const dcomplex y );
extern complex div(const complex divd, const complex dsor );
extern dcomplex exp(const dcomplex v);
extern dcomplex log(const dcomplex v);
extern dcomplex sin(const dcomplex v);
extern dcomplex cos(const dcomplex v);
extern dcomplex sqrt(const dcomplex v);
extern dcomplex pow(const dcomplex x, const dcomplex y);

inline dcomplex tan(const dcomplex v) {return div(sin(v),cos(v));}

inline ostream &operator<<(ostream &ios, complex x) {
  ios << x.r << (x.i>=0?"+":"") << x.i << "i";
  return ios;
}

inline ostream &operator<<(ostream &ios, dcomplex x) {
  ios << x.r << (x.i>=0?"+":"") << x.i << "i";
  return ios;
}

#endif
