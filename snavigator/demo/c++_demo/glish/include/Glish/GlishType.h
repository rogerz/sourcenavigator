/* $Header$ */

#ifndef glishtype_h
#define glishtype_h


typedef enum {
	/* If you change the order here or add new types, be sure to
	 * update the definition of type_names[] in Value.cc.
	 *
	 * If adding numeric types update the "max_numeric_type" function
	 * definition in Value.cc.
	 */
	TYPE_ERROR,
	TYPE_REF, TYPE_CONST,
	TYPE_SUBVEC_REF, TYPE_SUBVEC_CONST,
	TYPE_BOOL, TYPE_BYTE, TYPE_SHORT, TYPE_INT, TYPE_FLOAT, TYPE_DOUBLE,
	TYPE_STRING,
	TYPE_AGENT,
	TYPE_FUNC,
	TYPE_RECORD,
	TYPE_COMPLEX,
	TYPE_DCOMPLEX,
	TYPE_OPAQUE
#define NUM_GLISH_TYPES (((int) TYPE_OPAQUE) + 1)
	} glish_type;

/* Given two types, returns the "maximum" one, that is, which of the two
 * the other should be promoted to.
 */
extern glish_type max_numeric_type( glish_type t1, glish_type t2 );

extern const char* type_names[NUM_GLISH_TYPES];

#endif /* glishtype_h */
