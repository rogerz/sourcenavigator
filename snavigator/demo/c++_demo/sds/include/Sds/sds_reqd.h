#ifndef sds_reqd_h
#define sds_reqd_h 1

#include "Sds/sdsgen.h"

struct type_list sval_tl[] = 
   { 1, SDS_INTERNAL_POINTER,
		 1, SDS_LONG,
		 1, SDS_LONG,
		 1, SDS_LONG,
		 1, SDS_INTERNAL_POINTER,

  void *address;
  int nelems;
  int current;
  int inuse;
  char *name;


typedef struct {
  sval *svals;
  int nelems;
  char *name;
  } sobject;


#endif
