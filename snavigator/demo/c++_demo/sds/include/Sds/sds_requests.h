#ifndef sds_requests_h
#define sds_requests_h 1

#include "Sds/sds_list.h"

#define REG_NONE 0
#define REG_LIKE 1
#define REG_EXACT 2
#define REG_MINMAX 3

typedef struct {
	struct node_control *n;
	struct list_control *r;
	} field;
typedef field * field_handle;

typedef struct {
  void *address;    /* data address */
	void **returnadd; /* user pointer to set to data address */
  int   nelems;     /* Number of elements of data *parent* */
  int   arraysize;  /* array size of data */
  int   current;    /* current index of parent array */
  int   inuse;      /* Can't remember */
  char *name;       /* pointer to name of data */
	int   size;       /* size in bytes of one element of data */
	int   jump;       /* jump size - ie size of parent structure */
	double min;       /* limits to use in condition */
	double max;
	char *compstring; /* comparision string to be used in condition */
	int comptype;     /* type of condition */
	int elemcod;      /* SDS code of data type */
	field f;          /* field structure; contains list and node pointers */
  } sval;


field_handle sds_register(sds_handle,char *,char *,void **);
field_handle sds_like(field_handle, char *);
field_handle sds_match(field_handle, char *);
field_handle sds_limits(field_handle, double, double);
field_handle get_list(char *);
field_handle sds_remove_condition(field_handle);
int          sds_reset_list(field_handle);
int          sds_get_next(field_handle);

sval * sds_request_info(field_handle);
char * fortstring(char *,int);

#endif
