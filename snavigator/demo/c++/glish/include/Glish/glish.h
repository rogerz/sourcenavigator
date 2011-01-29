// $Header$

#ifndef glish_h
#define glish_h

typedef enum { glish_false, glish_true } glish_bool;

typedef const char* string;
typedef unsigned char byte;

#define loop_over_list(list, iterator)	\
	for ( int iterator = 0; iterator < (list).length(); ++iterator )

typedef void (*glish_signal_handler)();

#endif
