
/* $Header$ */


#if defined(vms)
#include "sdsgen.h"
#include "sds_externs.h"
#else
#if ! defined(__GCC_2__)
#include <stddef.h>
#endif
#include "Sds/sdsgen.h"
#include "Sds/sds_externs.h"
#endif
#if !defined(VXWORKS)
#include <memory.h>
#include <malloc.h>
#endif

#include <stdlib.h>
#include <string.h>

/* forward declarations */
char sds_get_align(sds_handle, sds_code);
int  put_in_order(sds_record_handle *, struct record_entry *);

sds_record_handle *
sds_begin_record(name)
char *name;
{
  struct record_header *rh = 
      (struct record_header *)calloc(1,sizeof(struct record_header));
  rh->recent_head = 
      (struct record_entry *)calloc(1, sizeof(struct record_entry));
  rh->recent_tail = 
  rh->recent_head->next = 
      (struct record_entry *)calloc(1,sizeof(struct record_entry));
  rh->recent_head->next->next = NULL;
  rh->record_depth = 0;

  rh->recent_head->elemtype = START_RECORD;
  if (name != NULL && strlen(name) != 0)
  {
    rh->recent_head->name = malloc(strlen(name) + 1);
    strcpy(rh->recent_head->name,name);
  }

  rh->hier_head = (struct hier_list *)calloc(1,sizeof(struct hier_list));
  rh->hier_head->next = NULL;
  rh->hier_head->start = rh->recent_head;

  rh->unmatched_endmarks = 0;
  rh->nrecs = 0;
  rh->stopped = 0;

  return rh;
}

sds_handle
sds_end_and_declare(rh, sds)
sds_record_handle *rh;
sds_handle sds;
{
  struct type_list *tl;
  struct hier_list *hprev,*h = rh->hier_head;
  struct record_entry *r;
  int tl_count;
  int depth;
  sds_handle object;
  char buffer[4096];

  if (h == NULL)
    {
        sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Record handle is null");
        return 0L;
    }
  if (rh->unmatched_endmarks != 0)
    {
        sds_push_error(SDS_NOT_DEFINABLE,SDS_ERROR,"Record def incomplete");
        return 0L;
    }

  if (!rh->stopped)
  {
    sds_end_sub_record(rh);
    rh->stopped = 1;
  }

  tl = (struct type_list *)sds_malloc((rh->nrecs + 2) * sizeof(struct type_list));
  while (h != NULL)
  {
    r = h->start;
    buffer[0] = 0;
    tl_count = 0;
    r = r->next;
    depth = r->record_depth;
    while(r->elemtype != END_RECORD)
    {
      if (r->record_depth == depth && r->elemtype != END_P_RECORD)
      {
        tl[tl_count].nelems = r->number;
        tl[tl_count++].elemcod = r->elemtype;
        if (r->name != NULL)
        {
          strcat(buffer,r->name);
          strcat(buffer,",");
        }
      }
      r = r->next;
    }
    r->elemtype = END_P_RECORD;
    buffer[strlen(buffer)] = 0;
    tl[tl_count++].elemcod = SDS_RETLIST;
    tl[tl_count++].elemcod = SDS_ENDLIST;
    h->start->elemtype = sds_define_structure(sds,tl,buffer);
    h->start->number = 1;
    hprev = h;
    h = h->next;
  }
  object = sds_declare_structure(sds,rh,
                  rh->recent_head->name,1,hprev->start->elemtype);
  sds_set_object_type(sds,object,SDS_RECORDS);
  free(tl);
  return 1;
}

void
sds_begin_sub_record(rh, name)
sds_record_handle *rh;
char *name;
{
  struct record_entry *r = rh->recent_tail;
  r->record_depth =  rh->record_depth++;
  r->elemtype = START_RECORD;
  if (name != NULL && strlen(name) != 0)
  {
    r->name = (char *)malloc(strlen(name) + 1);
    strcpy(r->name,name);
  }
  rh->recent_tail = 
  r->next = (struct record_entry *)calloc(1,sizeof(struct record_entry));
  r->next->next = NULL;

  rh->unmatched_endmarks++;
  rh->nrecs++;
  put_in_order(rh, r);
}

void
sds_end_sub_record(rh)
sds_record_handle *rh;
{
  struct record_entry *r = rh->recent_tail;
  r->record_depth =  rh->record_depth--;
  r->elemtype = END_RECORD;
  rh->recent_tail = 
  r->next = (struct record_entry *)calloc(1,sizeof(struct record_entry));
  r->next->next = NULL;
  rh->unmatched_endmarks--;
}

void
sds_record_entry(rh,type,number,pointer, name)
sds_record_handle *rh;
sds_code type;
sds_code number;
void *pointer;
char *name;
{
  struct record_entry *r = rh->recent_tail;
  r->record_depth =  rh->record_depth;
  r->elemtype = type;
  r->number = number;
  r->pointer = pointer;
  if (name != NULL && strlen(name) != 0)
  {
    r->name = (char *)malloc(strlen(name) + 1);
    strcpy(r->name,name);
  }
  else
    r->name = NULL;
  rh->recent_tail = 
  r->next = (struct record_entry *)calloc(1,sizeof(struct record_entry));
  r->next->next = NULL;
  rh->nrecs++;
}

void 
sds_destroy_record_def(rh, object_destroy) /* flag = 0 or 1 */
sds_record_handle *rh;
int object_destroy;
{
  struct record_entry *next, *r = rh->recent_head;
  struct hier_list *hnext,*h = rh->hier_head;

  while(h != NULL)
  {
    hnext = h->next;
    free(h);
    h = hnext;
  }
  while (r != NULL)
  {
    next = r->next;
    if (r->name != NULL)
      free(r->name);
    if (object_destroy && r->pointer != NULL)
      free(r->pointer);
    free(r);
    r = next;
  }
  free(rh);
}

void
sds_print_record_def(rh)
sds_record_handle *rh;
{

  int i = 0;
  struct record_entry *next, *r = rh->recent_head;
  struct hier_list *hnext,*h = rh->hier_head;

  printf("head %lx, tail %lx, depth %d\n",
              (unsigned long)rh->recent_head,
              (unsigned long)rh->recent_tail, (unsigned)rh->record_depth);

  if (rh->unmatched_endmarks == 0)
    printf("Record markers match\n");
  else
    printf("%d unmatched record markers\n", rh->unmatched_endmarks);

  while ((next = r->next) != NULL)
  {
    printf("%d %s,0x%lx of type 0x%lx at 0x%lx; depth %d\n",
        i++,
        r->name,
        (unsigned long)r->number,
        (unsigned long)r->elemtype,
        (unsigned long)r->pointer,
        r->record_depth);
    r = next;
  }
  while((hnext = h->next) != NULL)
  {
    printf("record %s level %d\n",h->start->name,h->start->record_depth);
    h = hnext;
  }
  printf("record %s level %d\n",h->start->name,h->start->record_depth);

}

sds_handle
sds_write_records(sds_handle sds, int fd, sds_record_handle *rh,
                      char *buffer,int size , int *count)
{
  int memcount = 0, padno, esize;
  struct record_entry *next, *r = rh->recent_head;
  static int padbytes[2] = { 0,0 };
  unsigned char align;
  sds_handle obsize = sds_element_size(sds,r->elemtype,&align);


  while ((next = r->next) != NULL)
  {
    if (r->elemtype & SDS_INDLIST || r->elemtype < (long)NTYPES)
    {
      align = sds_get_align(sds, r->elemtype);
      if ((padno = align_delta(memcount, align)) != 0)
      {
        memcount += padno;
        sds_buffered(fd,(char *)padbytes, padno,buffer,size,count);
      }
    }
    if (r->elemtype < (long)NTYPES) /* It's a primitive */
    {
      esize = r->number * (int)(sds_psize(r->elemtype) & 0xff);
      memcount += esize;
       sds_buffered(fd,r->pointer, esize,buffer,size,count);
    }
    r = next;
  }
  /* Finally pad the back end of the object if necessary */
  if ((padno = obsize - memcount) > 0)
  {
    memcount += padno;
     sds_buffered(fd,(char *)padbytes, padno,buffer,size,count);
  }
  sds_flush(fd,buffer,count);
  return memcount;
}

int
put_in_order(rh, r)
sds_record_handle *rh;
struct record_entry *r;
{
  struct hier_list *hprev = NULL,*hnew,*h = rh->hier_head;

  if (h->next != NULL)
  {
    while (h->next->start->record_depth > r->record_depth)
    {
      hprev = h;
      h = h->next;
    }
  }
  hnew = (struct hier_list *)calloc(1,sizeof(struct hier_list));
  hnew->start = r;
  hnew->next = h;
  if (hprev != NULL)
    hprev->next = hnew;
  else
    rh->hier_head = hnew;
  return 0;
}

char 
sds_get_align(sds, elemtype)
sds_code elemtype;
sds_handle sds;
{
  if (elemtype < (long)NTYPES) /* It's a primitive */
     return sds_palign(elemtype);
  else
  {
    struct type_list *tl = sds_tlist(sds);
    tl = &tl[(elemtype & ~SDS_INDLIST) + 1];
    return (tl->elemcod & 0xff);
  }
}

int
sds_buffered(int fd,char *data, int datasize,char *buffer,int size,int *count)
{
  int retno = 0;

  if (((datasize + *count) >= size) && (*count > 0))
        {
  /* Flush what is already there */
    sds_flush(fd, buffer, count);
                *count = 0;
        }
  if (datasize >= size)
  /* Write it out directly - buffered stuff already flush above */
    retno += sds_write(fd, data, datasize);
  else
  { /* We can buffer it until next time */
    memcpy((buffer + *count),data, datasize);
    *count += datasize;
    retno += datasize;
  }
  return retno;
}

int
sds_flush(int fd,char *buffer,int *count)
{
  if (*count > 0)
  {
    int r =  sds_write(fd, buffer, *count);
    *count = 0;
    return r;
  }
  return 0;
}

int
sds_copy_records(sds, cptr, rh)
sds_handle sds;
char *cptr;
sds_record_handle *rh;
{
  int memcount = 0, padno, esize;
  struct record_entry *next, *r = rh->recent_head;
  unsigned char align;
  sds_handle obsize = sds_element_size(sds,r->elemtype,&align);

  while ((next = r->next) != NULL)
  {
    if (r->elemtype & SDS_INDLIST || r->elemtype < (long)NTYPES)
    {
      align = sds_get_align(sds, r->elemtype);
      if ((padno = align_delta(memcount, align)) != 0)
      {
        memcount += padno;
        cptr += padno;
      }
    }
    if (r->elemtype < (long)NTYPES) /* It's a primitive */
    {
      esize = r->number * (int)(sds_psize(r->elemtype) & 0xff);
      memcount += esize;
      memcpy(cptr,r->pointer,esize);
      cptr += esize;
    }
    r = next;
  }
  /* Finally pad the back end of the object if necessary */
  if ((padno = obsize - memcount) > 0)
  {
    memcount += padno;
    cptr += padno;
  }
  return memcount;
}
