/* $Header$ */

#ifndef sds_record_h
#define sds_record_h

#ifndef vms
#include "Sds/sdsgen.h"
#else
#include "sdsgen.h"
#endif

#define START_RECORD   SDS_INDLIST
#define END_P_RECORD     SDS_RETLIST
#define END_RECORD   SDS_ENDLIST

struct hier_list {
  struct hier_list *next;
	struct record_entry *start;
	};

struct record_header {
	struct record_entry *recent_head;
	struct record_entry *recent_tail;
	struct hier_list *hier_head;
	int record_depth;
	int unmatched_endmarks;
	int nrecs;
	int stopped;
	};

#endif
