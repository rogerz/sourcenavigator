
/* $Header$ */

/**************************************************************************
 *                 ****** ISTK Release 1.2 *****                          *
 *                                                                        *
 *                                                                        *
 * This code has been produced by numerous authors at the CERN centre for *
 * high energy physics, Geneve, Switzerland, at the SSC laboratory in     *
 * Dallas, Texas, USA and at the Lawrence Berekeley Laboratory in         *
 * California, USA.                                                       *
 * The latter two institutions perform work under US Government contract. *
 * The intent of the work is to provide useful code for people who need   *
 * it, with an emphasis on free and collaborative exchange of ideas,      *
 * techniques and implementations.                                        *
 * Please read the disclaimer and copyright notices contained in the ISTK *
 * distribution and in distributed applications.                          *
 *                                                                        *
 **************************************************************************/


/* Reference release  Aug 10 1991 - C G Saltmarsh */
/* Has the basics used at CDG & SSC 1988-1991, plus vxworks
   support
*/

#include <string.h>
#include <stdlib.h>

#ifndef vms
#include <unistd.h>
#ifndef VXWORKS
#include <memory.h>
#endif
#endif

#if defined(vms)
#include "sdsgen.h"
#include "sds_externs.h"
#else
#include "Sds/sdsgen.h"
#include "Sds/sds_externs.h"
#endif

#define    STACK_INC  16

#define ALL_ELEMENTS 1
#define ONE_ELEMENT 2


void           sds_prev_object(rescon *,int);
void           sds_next_object(rescon * );
sds_handle     sds_vdescribe(rescon *,struct sds_header *,struct direc *,
                             char *, char *,struct type_list *,sds_code,
                             struct sds_odesc **);
sds_handle    sds_vresolve(rescon *,struct sds_header *,struct direc *,
                           char *, char *, struct type_list *,sds_code,
                           struct sds_odesc **, int);
int           sds_nextname(char *,char *,char **);
sds_handle    sds_res_init(rescon *,struct sds_header *, struct direc *,
                             char *,char *, struct type_list *,sds_code,int);
void          sds_fix_align(struct sds_header *, rescon *);
unsigned long sds_rsize(sds_handle, sds_code, unsigned char *);

/***********************************************************************/

static char    rnull = (char)0;

/***********************************************************************/

void
sds_cleanc(rescon *c)
{
  if (c->stack_size && c->start_stack)
    free((char *)c->start_stack);
  c->stack_size = 0;
  c->addr_inc = 0;
  c->thiso = 0;
  c->start_stack = 0;
  c->cette = 0;
  c->parent = 0;
  c->tptr = 0;
  c->nheap = 0;
  c->base_address = 0;
  c->firstpass = 1;
  c->done = 0;
}

/***********************************************************************/
int
sds_describe(sds,object_index,thing_list)
sds_handle  sds;
sds_code  object_index;
struct   sds_odesc  **thing_list;
/***********************************************************************/
{

  struct direc     *dptr = sds_direc(sds);
  char             *heap = sds_heap(sds);
  char             *name = sds_obind2name(sds,0);
  struct type_list *tptr = sds_tlist(sds);
  struct sds_header *h   = sds_head(sds);
  rescon *c = sds_src(sds);
  if (!c) return -1;

  return (int)sds_vdescribe(c,h,dptr,heap,name,tptr,object_index,thing_list);
}
/***********************************************************************/
sds_handle
sds_vdescribe(c,h,dptr,heap,name,tptr,object,thing_list)
rescon               *c;
struct sds_header    *h;
sds_code              object;
struct   sds_odesc  **thing_list;
struct direc         *dptr;
char                 *heap;
char                 *name;
struct type_list     *tptr;
/***********************************************************************/
{

  int          ind;
  char        *temp_addr;

  if (c->firstpass)
  {
    if ((!sds_res_init(c,h,dptr,heap,name,tptr,object,SDS_OBJECT)) &&
        (sds_last_warning() != SDS_ZERO_LENGTH)) 
    {
      return -1L;
    }
    c->base_address = c->cette->address;
  }

  while (1) 
  {
    while ((c->tptr[c->cette->ind].elemcod & SDS_RETLIST)) 
    {
      sds_prev_object(c,ALL_ELEMENTS);
      if (c->cette == c->start_stack) 
      {
        c->firstpass = 1;
        sds_push_error(SDS_END_RES_STACK,SDS_RETURN,"Resolution calls");
        return -1L;
      }
    }
    if (c->firstpass) 
    {
      c->firstpass = 0;
    } 
    else 
    {
      c->cette->elemcod = c->tptr[c->cette->ind].elemcod;
      c->cette->nelems = c->tptr[c->cette->ind].nelems;
    }

    ind = c->cette->elemcod & ~SDS_CODE_MASK;
    if (c->cette->elemcod & SDS_INDLIST)
    {
      if (c->tptr[ind].elemcod & SDS_LENLIST) 
      {
        c->cette->namelist = &rnull;
        c->cette->maxn = c->tptr[ind].nelems >> 16;
        if (c->cette->maxn)
          c->cette->namelist =
                 (c->nheap + (c->tptr[ind].nelems & 0xffff));
        ind++;
      }
      if (c->tptr[ind].elemcod & SDS_SIZE_ALIGN) 
      {
        c->cette->size = c->tptr[ind].nelems;
        c->cette->align = (char)(c->tptr[ind].elemcod & 0xff);
        ind++;
      }
      c->cette->nnames =  (c->parent->maxn >= c->cette->nelems)?
         (unsigned long)c->cette->nelems:
          1;

      c->parent->maxn -= c->cette->nnames;
      c->cette->name = c->parent->namelist;
      if (c->cette->nnames != (unsigned long)0)
        c->parent->namelist =
             sds_jstr(c->parent->namelist,c->cette->nnames);
      c->cette->address += c->addr_inc;
      c->cette->address += align_delta(c->cette->address - c->base_address,
                                      c->cette->align);
      temp_addr = c->cette->address;
      c->cette->ind++;

      sds_next_object(c);
      c->cette->address = temp_addr;
      c->addr_inc = 0;
      c->cette->ind = ind;
      c->cette->nelems = c->tptr[ind].nelems;
      c->cette->elemcod = c->tptr[ind].elemcod;
      *thing_list = c->start_stack;
      return(c->thiso-1);
    }
    else 
    {
      ind = c->cette->ind; 
      c->cette->nelems = c->tptr[ind].nelems;
      if (((c->cette->elemcod = c->tptr[ind].elemcod) >= SDS_RETLIST)) 
      {
      }
      else 
      {
        int bitf = 0;
        c->cette->nnames =  (c->parent->maxn >= (unsigned long)1)?
                           (unsigned long)1:
                           (unsigned long)0;
        c->parent->maxn -= c->cette->nnames;
        c->cette->name = c->parent->namelist;
        if (c->cette->nnames != (unsigned long)0)
           c->parent->namelist =
                 sds_jstr(c->parent->namelist,c->cette->nnames);

        c->cette->size = (long)(sds_psize(c->cette->elemcod) & 0xff);
        c->cette->align = c->sds_al[c->cette->elemcod] & 0xff;
				c->cette->nbits = 0;
        if ((c->cette->elemcod == SDS_CHAR_BITFIELD) ||
            (c->cette->elemcod == SDS_SHORT_BITFIELD) ||
            (c->cette->elemcod == SDS_LONG_BITFIELD) ||
            (c->cette->elemcod == SDS_DOUBLE_LONG_BITFIELD))
        {
          bitf =  (c->cette->nelems & SDS_LAST_BITFIELD)?1:2;

					c->cette->nbits = (char )(c->cette->nelems & SDS_BITFIELD_NMASK);
					c->cette->startbit = (char )(c->cette->nelems >> 16);
					if (!(c->cette->nelems & SDS_LAST_BITFIELD))
						c->cette->size = 0;
					c->cette->nelems = 1;
        }

        c->cette->address += c->addr_inc;
        c->cette->address += 
          align_delta(c->cette->address - c->base_address,c->cette->align);

        *thing_list = c->start_stack;

        /* Don't increment address if we're in the middle of a bitfield */
        c->addr_inc = (bitf == 2)?0:c->cette->size * (c->cette->nelems);

        c->cette->ind += 1;
        return(c->thiso);
      }
    }
  }
}

/***********************************************************************/
char  *
sds_jstr(str,n)
char  *str;
unsigned long   n;
/***********************************************************************/
{

  unsigned long  j;
  if (n == (unsigned long)0) return(str);
  for (j=0;j<n;j++)
    str = strchr(str,0) + 1;
  return(str);
}
/***********************************************************************/
void
sds_prev_object(rescon *c,int flag)
/***********************************************************************/
{
  int number_to_inc = 1;

  c->cette--;
  c->parent--;
  c->thiso--;

  if (flag == ALL_ELEMENTS)
    number_to_inc = c->cette->nelems;

  if (!(c->cette->elemcod & SDS_INDLIST))
    c->cette->elemcod = c->tptr[c->cette->ind].elemcod;

  if (c->cette->elemcod & SDS_RETLIST)
  {
    c->addr_inc = 0;
  }
  else if (c->cette != c->start_stack) 
  {
    c->addr_inc = c->cette->size * number_to_inc;
    c->addr_inc += align_delta(c->addr_inc,c->cette->align);
  }
}
/***********************************************************************/
void
sds_next_object(rescon *c)
/***********************************************************************/
{
  if (c->stack_size == 0) 
  {
    c->thiso = 0;
    c->stack_size = STACK_INC;
    c->cette = c->start_stack = 
      (struct sds_odesc *)sds_calloc(c->stack_size , sizeof(struct sds_odesc)); 
  } 
  else 
  {
    c->thiso++;
    if (c->thiso == c->stack_size) 
    {
      c->stack_size += STACK_INC;
      c->start_stack = 
        (struct sds_odesc *)sds_realloc((char *)c->start_stack,
          (unsigned)c->stack_size*sizeof(struct sds_odesc));
    }
  }
  c->cette = &c->start_stack[c->thiso];
  if (c->thiso)
  {
    c->parent = &c->start_stack[c->thiso-1];
  } 
  else 
  {
    c->parent = c->cette;
  }
}
/***********************************************************************/
sds_handle
sds_find_thing(sds,name,thing)
sds_handle  sds;
char  *name;
struct  sds_odesc  **thing;
/***********************************************************************/
{

  struct  sds_odesc  *thinglist;
  int nocomp = 1,level = 0,dlev,firstpass = 1;
  long  object = 0,nomen,addr_inc;
  char  *thingname,lname[128],*cptr = name;
  rescon *c = sds_src(sds);
  if (!c) return 0;
  lname[0] = 0;

  sds_cleanup(sds);
  while (sds_nextname(name,lname,&cptr)) 
  {
    level++;
    if (firstpass) 
    {
      firstpass = 0;
      if (!(object = sds_name2ind(sds,lname))) 
      {
        sds_push_error(SDS_NO_SUCH_OBJ,SDS_WARNING,"Searching for object");
        return (sds_handle)0;
      }  
    }
    while ((dlev = sds_describe(sds,object,&thinglist)) == level) 
    {
      nocomp = 1;
      thingname = thinglist[dlev].name;
      for (nomen = (long)0;nomen < thinglist[dlev].nnames;nomen++) 
      {
        if (!(nocomp = strcmp(lname,thingname))) 
        {
          addr_inc = nomen*thinglist[dlev].size;
          addr_inc += align_delta(addr_inc,thinglist[dlev].align);
          thinglist[dlev].address += addr_inc;
          if (thinglist[dlev].elemcod &
            SDS_INDLIST)
            thinglist[dlev+1].address += addr_inc;
          thinglist[dlev].nelems -= nomen;
          break;
        }
        else 
          thingname = strchr(thingname,0) + 1;
      }
      *thing = &thinglist[dlev];
      if (!nocomp) 
        break;
    }  
    if (nocomp)
    {
      sds_cleanup(sds);
      sds_push_error(SDS_NO_SUCH_OBJ,SDS_WARNING,"Searching for object");
      return (sds_handle)0;
    }
  }
  return(object);
}
/***********************************************************************/
int
sds_nextname(char *name,char *buffer,char **cptr)
/***********************************************************************/
{
  char *bptr = *cptr;

  if (*cptr == 0 || name == 0) 
    return 0;

  if ((*cptr = (char *)strchr(*cptr,'.')) != NULL) 
  {
    strncpy(buffer,bptr,(*cptr-bptr));
    buffer[*cptr-bptr] = 0;
    bptr = ++(*cptr);
  }
  else  
  {
    strcpy(buffer,bptr);
    *cptr = 0;
  }
  return 1;
}
/***********************************************************************/
int
sds_make_name(name,thing_list,start_level,end_level)
char  *name;
struct  sds_odesc  *thing_list;
int  start_level,end_level;
/***********************************************************************/
{
  int  lev;

  if (start_level < 0) start_level = 0;
  if (end_level < 0) end_level = 0;
  if (start_level > end_level) start_level = end_level;
  *name = 0;
  for (lev=start_level;lev <= end_level;lev++) 
  {
    if ((thing_list[lev].name != NULL) &&
        (*thing_list[lev].name != 0)) 
        {
      strcat(name,thing_list[lev].name);
      strcat(name,".");
    }
  }
  *(strchr(name,0)-1) = 0;
  return((int)thing_list[lev].nnames);
}
/***********************************************************************/
sds_handle
sds_res_init(c,h,dptr,heap,name,tptr,object,flag)
rescon            *c;
struct sds_header *h;
int                flag;
sds_code           object;
struct direc      *dptr;
char              *heap;
char              *name;
struct type_list  *tptr;
/***********************************************************************/
  {

  int  retval = 1;

  if (dptr == DNULL)
    {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Initialise resolution calls");
    return (sds_handle)0;
    }
  sds_cleanc(c);
  c->addr_inc = 0;
  c->nheap = heap;
  c->tdum[1].elemcod = SDS_RETLIST;
  sds_fix_align(h,c);

  sds_next_object(c);
  c->cette->nelems = (unsigned long)0;
  c->cette->address   = (char *)0;
  c->cette->name   = name;
  c->cette->namelist   = &rnull;

  if (flag == SDS_OBJECT) 
    {
    c->cette->nnames   = 
    c->cette->maxn   = dptr[object].obj_name >> 16;
    if (c->cette->maxn)
      c->cette->namelist = (c->nheap + (dptr[object].obj_name & 0xffff));
    }

  sds_next_object(c);

  if (flag == SDS_OBJECT) 
    {
    c->cette->elemcod   = dptr[object].elemcod;
    c->cette->address   = (char*) sds_getp(dptr,object);
    c->cette->align     = dptr[object].align_type;
    if ((c->cette->nelems = dptr[object].nelems) == 0L)
      {
      retval = 0;
      sds_push_error(SDS_ZERO_LENGTH,SDS_WARNING,"Initialise resolution calls");
      }
    c->cette->size   = dptr[object].elemsz;
    }
  else 
    {
    c->cette->elemcod  = object;
    c->cette->address   = (char *)0;
    c->cette->align     = (char)0;
    c->cette->nelems    = (unsigned long)1;
    }
  c->tdum[0].elemcod  =  c->cette->elemcod;
  c->tdum[0].nelems  =  c->cette->nelems;

  if ((c->cette->elemcod & SDS_INDLIST)) 
    {
    c->tptr = tptr;
    c->cette->ind = 
      (unsigned long)tlist_size(c->tptr)/sizeof(struct type_list); 
    c->cette->ind -= 3;
    }  
  else 
    {
    c->tptr = c->tdum;
    c->cette->ind  = (unsigned long)0;
    }
  c->parent->ind = c->cette->ind;

  return(retval);
  }
/***********************************************************************/
int
sds_resolve(sds,object_index,thing_list,flag)
sds_handle  sds;
sds_code  object_index,flag;
struct   sds_odesc  **thing_list;
/***********************************************************************/
{
  struct direc     *dptr = sds_direc(sds);
  char             *heap = sds_heap(sds);
  char             *name = sds_obind2name(sds,0);
  struct type_list *tptr = sds_tlist(sds);
        struct sds_header *h   = sds_head(sds);
  rescon *c = sds_src(sds);
        if (!c) return -1;

  return (int)sds_vresolve(c,h,dptr,heap,name,tptr,object_index,thing_list,flag);
}
/***********************************************************************/
sds_handle
sds_vresolve(c,h,dptr,heap,name,tptr,object,thing_list,flag)
rescon              *c;
struct sds_header   *h;
int                  flag;
sds_code             object;
struct   sds_odesc **thing_list;
struct direc        *dptr;
char                *heap;
char                *name;
struct type_list    *tptr;
/***********************************************************************/
{

  int  ind = 0;
  char *temp_addr;

  if (c->firstpass)
  { 
    if (c->done || !sds_res_init(c,h,dptr,heap,name,tptr,object,flag))
    {
      c->done = 0;
      return -1L;
    }
    c->firstpass = 0;
    c->base_address = c->cette->address;
    if ((flag & SDS_OBJECT)  && (dptr[object].structype == SDS_RECORDS))
      c->r = ((sds_record_handle *)sds_getp(dptr,object))->recent_head;
    else
      c->r = 0;
  }

  while (1) 
  {
    while ((c->tptr[c->cette->ind].elemcod & SDS_RETLIST)) 
    {
      sds_prev_object(c,ONE_ELEMENT);
      if (c->cette == c->start_stack) 
      {
        c->firstpass = 1;
        c->done = 1;
        sds_push_error(SDS_END_RES_STACK,SDS_RETURN,"Resolution calls");
        return -1L;
      }
    }
    ind = c->cette->elemcod & ~SDS_CODE_MASK;
    if (c->cette->elemcod & SDS_INDLIST)
    {
      if (c->tptr[ind].elemcod & SDS_LENLIST) 
      {
        ind++;
      }
      if (c->tptr[ind].elemcod & SDS_SIZE_ALIGN) 
      {
        c->cette->size = c->tptr[ind].nelems;
        c->cette->align = (char)(c->tptr[ind].elemcod & 0xff);
        ind++;
      }
      c->cette->address += c->addr_inc;
      c->cette->address += align_delta(c->cette->address - c->base_address,
                                      c->cette->align);
      temp_addr = c->cette->address;
      if ((c->cette->nelems == 0) || ((--(c->cette->nelems)) == 0)) 
      {
        c->cette->ind += 1;
        c->cette->nelems = c->tptr[c->cette->ind & ~SDS_CODE_MASK].nelems;
        c->cette->elemcod = c->tptr[c->cette->ind & ~SDS_CODE_MASK].elemcod;
      }
      sds_next_object(c);
      c->cette->address = temp_addr;
      c->addr_inc = 0;
      c->cette->ind = ind;
      c->cette->nelems = c->tptr[ind].nelems;
      c->cette->elemcod = c->tptr[ind].elemcod;
    } 
    else 
    {
      ind = c->cette->ind;
      c->cette->nelems = c->tptr[ind].nelems;
      if (((c->cette->elemcod = c->tptr[ind].elemcod) >= SDS_RETLIST)) 
      {
      } 
      else 
      {
        int bitf = 0;
        c->cette->size = (long)(sds_psize(c->cette->elemcod) & 0xff);
        c->cette->align = c->sds_al[c->cette->elemcod] & 0xff;
				c->cette->nbits = 0;
        if ((c->cette->elemcod == SDS_CHAR_BITFIELD) ||
            (c->cette->elemcod == SDS_SHORT_BITFIELD) ||
            (c->cette->elemcod == SDS_LONG_BITFIELD) ||
            (c->cette->elemcod == SDS_DOUBLE_LONG_BITFIELD))
        {
          bitf =  ((c->cette->nelems & SDS_LAST_BITFIELD))?1:2;

					c->cette->nbits = (char )(c->cette->nelems & SDS_BITFIELD_NMASK);
					c->cette->startbit = (char )(c->cette->nelems >> 16);
					if (!(c->cette->nelems & SDS_LAST_BITFIELD))
						c->cette->size = 0;
					c->cette->nelems = 1;
        }

        c->cette->address += c->addr_inc;
        c->cette->address += 
         align_delta(c->cette->address - c->base_address,c->cette->align);

        *thing_list = c->start_stack;

        /* Don't increment address if we're in the middle of a bitfield */
        c->addr_inc = (bitf == 2)?0:c->cette->size * (c->cette->nelems);

        c->cette->ind += 1;
        if (c->r)
        {
          while (c->r->elemtype & SDS_CODE_MASK)
            c->r = c->r->next;
          c->cette->address = c->r->pointer;
          c->r = c->r->next;
        }
        return(c->thiso);
      }
    }
  }
}
/***********************************************************************/
unsigned long
sds_tlsize(sds,code,alignment_type)
sds_handle  sds;
long  code;
char  *alignment_type;
/*  This figures out the size of an object described by ty_list
 *  The size returned is that of the object in the ORIGINAL system -
 *  While this is going on, I also work out
 *  what sort of datum the thing is. This is needed for
 *  sparchitecture as alignment depends on data type    */
/***********************************************************************/
{
  unsigned long  laddr = 0,lsize = 0,thing_size = (unsigned long)0;
  struct  sds_odesc  *thing;
  int  thiso;

  *alignment_type = (char)0;
  sds_cleanup(sds);
  while ((thiso = sds_resolve(sds,code,&thing,SDS_CODE)) >= 0) 
  {
    lsize = thing[thiso].nelems * thing[thiso].size;
    laddr = (unsigned long)thing[thiso].address;
    *alignment_type = 
      (thing[thiso].align > *alignment_type)?
      thing[thiso].align:*alignment_type;
  }
  thing_size = lsize + (unsigned long)laddr; 
  thing_size += align_delta(thing_size,*alignment_type);
  return(thing_size);
}
/***********************************************************************/
unsigned long
sds_rsize(sds,code,alignment_type)
sds_handle  sds;
sds_code  code;
unsigned char  *alignment_type;
/***********************************************************************/
{
  struct  type_list  *tptr = sds_tlist(sds);
  unsigned long  size = (unsigned long)0;
  unsigned long  ind = code & ~SDS_CODE_MASK;
  int  i;

  for (i=0;i<2;i++,ind++)
    if (tptr[ind].elemcod & SDS_SIZE_ALIGN) 
    {
      size = tptr[ind].nelems;
      *alignment_type = (char)(tptr[ind].elemcod & 0xff);
    }
  return(size);
}
    
/***********************************************************************/
sds_handle
sds_sizeof_object(sds, object_index)
sds_handle    sds;
sds_code    object_index;
/***********************************************************************/
{
    unsigned char junk;
    struct direc *dptr = sds_direc(sds);
    sds_handle    size;

    size = sds_element_size(sds, dptr[object_index].elemcod, &junk);
    return size * dptr[object_index].nelems;
}
/***********************************************************************/
sds_handle
sds_element_size(sds,code,alignment_type)
sds_handle  sds;
sds_code    code;
unsigned char  *alignment_type;
/***********************************************************************/
/*  How big is a thing - a primitive or a reference to a tlist     */
/*  If the size is < 0, this signals an unknown thing.    */
/*  Size 0 is 'undetermined' (eg SDS)        */
{
  rescon *c = sds_src(sds);

  if (!c) return 0;

  if (code == SDS_SDS) 
    return 0L;
  else if (code & SDS_INDLIST)
    return (long)sds_rsize(sds,code,alignment_type);
  else 
  {
                long size;
    sds_fix_align(sds_head(sds),c);
    *alignment_type = c->sds_al[code];
    if (!(size = (long)sds_psize(code)))
      sds_push_error(SDS_UNDEFINED_TYPE,SDS_ERROR,"Get object size");
    return size;
  }
}
/***********************************************************************/
void
sds_fix_align(struct sds_header *header, rescon *c)
/***********************************************************************/
{
  int    arch = SDS_ARC;

  if (!header)
    c->sds_al = sds_arc_aligns(arch);
  else 
  {
    int state = (!sds_header_ok(header))?sds_last_warning():0;
    arch = (state == SDS_SWAPPED_BYTES)?
      (header->magic_number & 0x00ff0000) >> 16:
      (header->magic_number & 0x0000ff00) >> 8;
    c->sds_al = sds_arc_aligns(arch-1);
  }
}
/***********************************************************************/
sds_handle
object_size(dptr,tptr,heap)
struct direc     *dptr;
struct type_list  *tptr;
char        *heap;
/***********************************************************************/
{
  long  code = dptr->elemcod;
  long size;
  rescon *c = sds_new_rescon();

  if (code & SDS_INDLIST)
  {
    size = object_tlsize(c,dptr,heap,"",tptr);
  }
  else
  {
    sds_fix_align(0,c);
    dptr->align_type = c->sds_al[code];
    if (!(size = (long)sds_psize(code)))
      sds_push_error(SDS_UNDEFINED_TYPE,SDS_ERROR,"Get object size");
  }
  sds_delete_rescon(c);
  return size;
}
/***********************************************************************/
sds_handle
object_tlsize(c,dptr,heap,name,tptr)
rescon             *c;
struct direc       *dptr;
char               *heap;
char               *name;
struct type_list   *tptr;
/***********************************************************************/
{
  unsigned long  laddr = 0,lsize = 0,thing_size = (unsigned long)0;
  struct  sds_odesc  *thing;
  int  thiso;
  long  code = dptr->elemcod;

  while ((thiso = sds_vresolve(c,0,dptr,heap,name,tptr,code,&thing,SDS_CODE)) >= 0)
  {
    lsize = thing[thiso].nelems * thing[thiso].size;
    laddr = (unsigned long)thing[thiso].address;
    dptr->align_type =
      (thing[thiso].align > (char)dptr->align_type)?
      thing[thiso].align:dptr->align_type;
  }
  thing_size = lsize + (unsigned long)laddr;
  thing_size += align_delta(thing_size,dptr->align_type);
  return(thing_size);
}
