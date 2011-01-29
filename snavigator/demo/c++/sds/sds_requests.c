#include "Sds/sdsgen.h"
#include "Sds/sds_requests.h"
#include <stdlib.h>
#include <string.h>


field_handle
sds_register(sds,object,fieldname, pointer)
sds_handle sds;
char *object;
char *fieldname;
void **pointer;
{
  struct sds_odesc *thing;
  field_handle fh;
  sval *sv;
  int regid;
  struct list_control *reglist;
  char *temp;

  if (*fieldname == 0)
    temp = object;
  else
  {
    temp = malloc(strlen(object) + strlen(fieldname) + 2);
    sprintf(temp,"%s.%s",object,fieldname);
  }

  if (!(fh = get_list(object)))
  {
    char *tname = malloc(strlen(object) + 1);
    strcpy(tname,object);
    regid = sds_new_list(object, 1);
    reglist = goodlist(regid);
    sds_add(sds_reglist(),tname,(void *)reglist,0,0,0);
  }
  else
    reglist = fh->r;
  if (!sds_find_thing(sds,temp, &thing))
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Register object");
    if (temp != object)
      free (temp);
    return (field_handle)0;
  }
  if (temp != object)
    free (temp);
  sv = (sval *)calloc(1,sizeof(sval));
  sv->address = thing->address;
  sv->returnadd = pointer;
  sv->nelems = thing[-1].nelems;
  sv->arraysize = thing[0].nelems;
  if (sv->nelems == 0) /* if the thing's at object level */
    sv->nelems = 1;
  sv->current = 0;
  sv->name = thing->name;
  sv->jump = thing[-1].size;
  sv->elemcod = thing->elemcod;
  if (sv->elemcod == SDS_STRING || sv->elemcod == SDS_FSTRING)
    sv->size = thing[0].nelems;
  else
    sv->size = thing[0].size;
  sv->f.r = reglist;

  sv->f.n = sds_add(reglist,thing->name,sv,1,sizeof(sval),SDS_INDLIST);

  return &(sv->f);
}

field_handle
sds_like(rfield, string)
field_handle rfield;
char *string;
{
  sval *sv;

  if (rfield == 0 || rfield->r == 0 || rfield->n == 0)
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Object condition");
    return (field_handle)0;
  }
  sv = (sval *)rfield->n->data;
  if ((sv->elemcod != SDS_STRING) &&
      (sv->elemcod != SDS_FSTRING))
  {
    sds_push_error(SDS_WRONG_TYPE,SDS_ERROR,"Wrong type comparison");
    return (field_handle)0;
  }
  sv->comptype = REG_LIKE;
  sv->compstring = string;
  return rfield;
}

field_handle
sds_match(rfield, string)
field_handle rfield;
char *string;
{
  sval *sv;

  if (rfield == 0 || rfield->r == 0 || rfield->n == 0)
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Object condition");
    return (field_handle)0;
  }
  sv = (sval *)rfield->n->data;
  if ((sv->elemcod != SDS_STRING) &&
      (sv->elemcod != SDS_FSTRING))
  {
    sds_push_error(SDS_WRONG_TYPE,SDS_ERROR,"Wrong type comparison");
    return (field_handle)0;
  }
  sv->comptype = REG_EXACT;
  sv->compstring = string;
  return rfield;
}

field_handle
sds_limits(rfield, min,max)
field_handle rfield;
double min,max;
{
  sval *sv;

  if (rfield == 0 || rfield->r == 0 || rfield->n == 0)
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Setting limits");
    return (field_handle)0;
  }
  sv = (sval *)rfield->n->data;
  if ((sv->elemcod != SDS_FLOAT) &&
      (sv->elemcod != SDS_DOUBLE) &&
      (sv->elemcod != SDS_INT) &&
      (sv->elemcod != SDS_SHORT) &&
      (sv->elemcod != SDS_CHAR))
  {
    sds_push_error(SDS_WRONG_TYPE,SDS_ERROR,"Wrong type comparison");
    return (field_handle)0;
  }
  sv->comptype = REG_MINMAX;
  sv->min = min;
  sv->max = max;
  return rfield;
}

int
sds_reset_list(rfield)
field_handle rfield;
{
  struct list_control *reglist = rfield->r;
  if (!reglist)
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"trying to load pointers");
    return 0;
  }
  return sds_gobefore(reglist,0);
}

int
sds_get_next(rfield)
field_handle rfield;
{
  sval *sv;
  int goodline = 1;
  int stilldata = 1;
  struct node_control *n;
  char *genptr;
  struct list_control *reglist = rfield->r;
  char *temp = 0;

  if (!reglist)
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"trying to load pointers");
    return -1;
  }

  while (stilldata)
  {
    n = 0;
    goodline = 1;
    while ((n = sds_next(reglist,n)) != 0)
    {
      sv = (sval *)n->data;
      if (sv->current == sv->nelems)
        return -1;
      genptr = (char *)sv->address + (sv->current * sv->jump);
      switch(sv->comptype)
      {
        case REG_LIKE:
        temp = calloc(sv->size + 1, 1);
        strncpy(temp,genptr,sv->size);
        if (!strstr(temp,sv->compstring))
          goodline = 0;
        free(temp);
        break;
        case REG_EXACT:
        if (strcmp(genptr,sv->compstring))
        goodline = 0;
        break;
        case REG_MINMAX:
          switch (sv->elemcod)
          {
          case SDS_FLOAT:
            if (((double)*(float *)genptr < sv->min) || 
                ((double)*(float *)genptr > sv->max))
            goodline = 0;
          break;
          case SDS_DOUBLE:
            if ((*(double *)genptr < sv->min) || 
                (*(double *)genptr > sv->max))
            goodline = 0;
          break;
          case SDS_LONG:
            if (((double)*(long *)genptr < sv->min) || 
                ((double)*(long *)genptr > sv->max))
            goodline = 0;
          break;
          case SDS_SHORT:
            if (((double)*(short *)genptr < sv->min) || 
                ((double)*(short *)genptr > sv->max))
            goodline = 0;
          break;
          case SDS_CHAR:
            if (((double)*(char *)genptr < sv->min) || 
                ((double)*(char *)genptr > sv->max))
            goodline = 0;
          break;
          }
        break;
        case REG_NONE:
        default:
        break;
      }
    }
    n = 0;
    while ((n = sds_next(reglist,n)) != 0)
    {
			long tempad;
      sv = (sval *)n->data;
      tempad = (long)sv->address + (sv->current * sv->jump);
      *(sv->returnadd) = (char *)tempad;
      sv->current++;
    }
    if (goodline)
      return sv->current - 1;
  }
  sds_push_error(SDS_END_RES_STACK,SDS_WARNING,"All rows scanned");
  return -1;
}

field_handle
get_list(object)
char *object;
{
  struct node_control *n = 0;
  while ((n = sds_next(sds_reglist(),n)) != 0)
    if (!strcmp(n->name,object))
    {
      struct list_control *reglist = (struct list_control *)n->data;
      sval *sv = (sval *)reglist->head->data;
      return &sv->f;
    }
  return 0;
}

sval *
sds_request_info(rfield)
field_handle rfield;
{
  return (sval *)rfield->n->data;
}

char *
fortstring(string,size)
char *string;
int size;
{
  static char buf[128];
  size = size>=127?127:size;
  strncpy(buf,string,size);
  return buf;
}


field_handle
sds_remove_condition(rfield)
field_handle rfield;
{
  sval *sv;

  if (rfield == 0 || rfield->r == 0 || rfield->n == 0)
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Object condition");
    return (field_handle)0;
  }
  sv = (sval *)rfield->n->data;
  sv->comptype = REG_NONE;
  sv->compstring = 0;
  return rfield;
}
