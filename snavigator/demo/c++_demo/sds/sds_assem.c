
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
#include <fcntl.h>
#ifndef VXWORKS
#include <values.h>
#else
#define BITSPERBYTE 8
#define BITS(type)  (BITSPERBYTE * (int)sizeof(type))
#endif

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

#ifdef MEMMAP
#include <sys/mman.h>
#endif

#define  SDS_INC_DIREC 32
#define  SDS_INC_SUBS  32
#define  SDS_INC_HEAP  256
#define  SDS_FIXED     -1


extern char  *strncpy();

#if !defined(hpux) && !defined(VXWORKS) && !defined(mips)
#if !defined(vms)
#if ! defined (__GCC_2__)
#if defined(__GNUC__)
#if __GNUC__ != 2
extern char  *memcpy();
#endif
#endif
#endif
#endif
#endif

/*  Forward declarations  */
#ifndef VXWORKS
off_t           lseek();
#endif

extern void           sds_delete_scp(struct sds_control_p *);
extern sds_handle     next_sds();

void                  sds_fixbits(struct type_list *);
sds_handle            sds_prepads(struct direc *, sds_handle);
char                **sds_saverestore(sds_handle, struct direc *, int);
sds_handle            sds_type_duplicate_def(sds_handle, sds_handle, sds_code);
sds_handle            sds_start_heap(sds_handle);
sds_handle            sds_inc_heap(sds_handle, int);
long                  tlist_len(struct  type_list  *);
struct direc        * sds_inc_direc_size(sds_handle);
sds_handle            sds_pname(sds_handle,sds_code,char *);

/*********************************************************************/
sds_handle
sds_dup(old_sds,name)
sds_handle  old_sds;
char *name;
/*********************************************************************/
{
  sds_handle  new_sds;
  sds_code  object,typecode;
  struct  direc  *dptr,*old_dptr;
  struct sds_control_p *scp;

  if (!sds_initialised())
  {
    fprintf(stderr,"Sds must be initialised with sds_init()");
    exit(1);
  }

  if ((old_dptr = sds_direc(old_sds)) == DNULL)
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Dataset to be duplicated");
    return 0L;
  }

  if ((new_sds = good_sds(name)))
  {
    sds_push_error(SDS_DEJA_LA,SDS_WARNING,"Trying to make duplicate");
    return 0L;
  }

  if ((new_sds = sds_new(name)) == 0)
  {
    sds_push_error(SDS_NO_SPC,SDS_WARNING,"Trying to make duplicate");
    return 0L;
  }


  scp = sds_control(new_sds);
  scp->dup_size = (sds_handle *)sds_malloc(old_dptr[0].nelems * sizeof(long));


  for (object = 1;object < old_dptr[0].nelems; object++)
  {
    scp->dup_size[object] = old_dptr[object].nelems;
    sds_cleanup(old_sds);
    if ((typecode = old_dptr[object].elemcod) & SDS_INDLIST)
      typecode = sds_duplicate_def(old_sds,new_sds,object);

    sds_declare_structure(new_sds, 
      sds_obind2ptr(old_sds,object),
      sds_obind2name(old_sds,object),
      old_dptr[object].nelems,
      typecode);
   dptr = sds_direc(new_sds);
   dptr[object].structype = old_dptr[object].structype;  
   if (old_dptr[object].illoca == SDS_DISJOINT_OBJECT)
     dptr[object].illoca = SDS_DISJOINT_OBJECT;  
  }
  return new_sds;
}
/*********************************************************************/
sds_handle
sds_duplicate_def(source_sds,new_sds,object)
sds_handle  source_sds,new_sds;
sds_code  object;
/*********************************************************************/
{
  struct   direc    *dptr = sds_direc(source_sds);
  sds_code           typecode =  dptr[object].elemcod;

  return sds_type_duplicate_def(source_sds,new_sds,typecode);
}
/*********************************************************************/
void
sds_put_name(sds,object,name)
sds_handle  sds;
sds_code  object;
char  *name;
/*********************************************************************/
{
  char  *oname = sds_obind2name(sds,object);
  int len = strlen(oname);
  strncpy(oname,name,len);
}
/*********************************************************************/
sds_handle
sds_pname(sds,object,name)
sds_handle sds;
sds_code  object;
char  *name;
/*********************************************************************/
{
  struct  direc*  dptr = sds_direc(sds);
  dptr[object].obj_name = sds_add_to_heap(sds,name,',');
  return dptr[object].obj_name;
}
/*********************************************************************/
char  *
sds_obind2name(sds,object)
sds_handle sds;
sds_code object;
/*********************************************************************/
{
  return(sds_oname(sds,object,SDS_GEN_NAME));
}
/*********************************************************************/
sds_handle
sds_named_elements(sds,object)
sds_handle  sds;
sds_code  object;
/************************************************************************/
{
  struct  direc  *dptr = sds_direc(sds);

  if (dptr == DNULL)
  {
     sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"");
     return 0L;
  }
  return((sds_handle)(dptr[object].obj_name >> 16));
}
  
/*********************************************************************/
char  *
sds_oname(sds,object,elem)
sds_handle  sds;
sds_code  elem,object;
/*********************************************************************/
{
  struct  direc  *dptr = sds_direc(sds);
  char    *cptr;

  elem++;
  if (dptr == DNULL) 
  {
    sds_push_error(SDS_NO_SUCH_SDS, SDS_ERROR," Finding object name");
    return (char *)0;
  }
  if (elem > (dptr[object].obj_name >> 16)) 
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Finding object name");
    return (char *)0;
  }
  cptr = (sds_heap(sds) + (dptr[object].obj_name & 0xffff));
  cptr = sds_jstr(cptr,elem);
  return(cptr);
}
/*********************************************************************/
sds_handle
sds_new(name)
char *name;
/*********************************************************************/
{

  struct direc *ddptr;
  sds_handle  sds;
  struct sds_control_p *scp;
  sds_handle  heap_place;

  if (!sds_initialised())
  {
    fprintf(stderr,"Sds must be initialised with sds_init()");
    exit(1);
  }
  if ((sds = good_sds(name)))
  {
    sds_push_error(SDS_DEJA_LA,SDS_WARNING,"Trying to make new dataset");
    return sds;
  }
  if (!(sds = next_sds())) return 0L;

  scp = sds_control(sds);

/*  Start-up space for the directory      */
  ddptr = scp->dptr = 
    (struct direc *)sds_calloc((unsigned int)SDS_INC_DIREC,
          (unsigned int)sizeof(struct direc));
  if (ddptr == 0 )
  {
     sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Trying to make new dataset");
     return 0L;
  }
  scp->allofl = SDS_DPTR_ALLOC;
  scp->direc_size = SDS_INC_DIREC;

/*  Initialise direc[0]: the directory entry    */
  ddptr[0].nelems = (unsigned long)1;
  ddptr[0].elemsz = sizeof(struct direc);
  ddptr[0].align_type = sds_palign(SDS_DIRECTORY_STRUCTURE);
  ddptr[0].offst = SDS_NOT_ASSEMBLED;
  ddptr[0].structype = (short)0;
  ddptr[0].illoca = (unsigned char)0;

  scp->tlist = TNULL;
  scp->shead = HNULL;

  heap_place = sds_add_to_heap(sds,name,0);
  if (heap_place < 0)
  {
    ddptr[0].obj_name = (sds_code)heap_place;
    return 0L;
  }

/*  And timestamp it for the hell of it      */
  sds_tstamp(sds,SDS_TIMESTAMP_ALL);
  return(sds);
}
/*********************************************************************/
sds_handle
sds_access(source_name,source_type,mode)
char *source_name;
sds_code source_type,mode;
/*   get access to an (alleged?) SDS outside process memory.
 Unless  this involves attaching to shared memory, this means I load
 into process memory.
*/
/*********************************************************************/
{
  struct direc *dptr = 0;
  sds_handle    sds = SDS_NO_SUCH_SDS;
  int           obj;
  struct sds_control_p *scp;
  int success = 0;
  char *temprpt = malloc(strlen(source_name) + 32);
  sprintf(temprpt,"Dataset %s", source_name);

  if (!sds_initialised())
  {
    fprintf(stderr,"Sds must be initialised with sds_init()");
    exit(1);
  }
  if (source_type & SDS_PROC_MEM)
  {
    free(temprpt);
    return good_sds(source_name);
  }

  if ((source_type & SDS_FILE) || (source_type == SDS_SEQUENTIAL))
  {
    if ((source_type & SDS_DIREC_ONLY) || (source_type == SDS_SEQUENTIAL))
    {
      int fd;
      sds_handle  orig_state,sdsn;
      struct  sds_header head;
      sdsn = sds = next_sds();
      if ((fd = sds_open_file(source_name, O_RDONLY | O_BINARY)) >= 0)
      {
        if ((sds = sds_cload_direc(sds,fd,&orig_state, &head)) != 0) 
        {
          dptr = sds_direc(sds);
          success = 1;
          if (source_type == SDS_SEQUENTIAL)
            dptr[0].offst = SDS_NOT_ASSEMBLED;
        }
                                else
                                {
          if (sds_last_return() == SDS_GOOD_FORMAT)
                                        {
            if ((sds = sds_load_direc(sdsn,fd, &head)))
                                                {
              dptr = sds_direc(sds);
              success = 1;
              if (source_type == SDS_SEQUENTIAL)
                dptr[0].offst = SDS_NOT_ASSEMBLED;
                                                }
                                        }
                                }
        sds_close_fd(fd);
      }
    }
    else 
    {
      if ((sds = sds_load_conv(source_name)))
      {
        dptr = sds_direc(sds);
        success = 1;
      }
    }
  }
#ifdef SHMEM
  else if (source_type & SDS_SHARED_MEM) 
  {
    if (shm_q(source_name)) 
    {                        /* it exists! */
      if ((sds = next_sds()) < 0)  
        return(sds);
      if (mode & SDS_READ)
        dptr = (struct direc *)shm_attr(source_name);
      else  
        dptr = (struct direc *)shm_attw(source_name);
      if (dptr != 0)
      {
        success = 1;
        set_sys_vars(sds,dptr);
      }
    }
    else 
      sds_push_error(SDS_NO_SUCH_SDS, SDS_ERROR, "Attach to shared mem");
  }
#endif
  else 
    sds_push_error(SDS_NO_SUCH_SDS, SDS_ERROR, "undefined sds source type");

  if (!success)
  {
    sds_push_error(sds_last_error(), SDS_ERROR, temprpt);
    free(temprpt);
    return 0L;
  }
  free(temprpt);
  scp = sds_control(sds);
  scp->source = source_type;
  scp->load_name = malloc(strlen(source_name) + 1);
  strcpy(scp->load_name, source_name);
  scp->is_proto = 0;

  /* clean up in case allocation flags have been left in file (not nice)
  */
  if (!(source_type & SDS_SHARED_MEM))
    for (obj = 0;obj < dptr[0].nelems; obj++)
      dptr[obj].illoca &= 
         ~(SDS_WAS_ALLOCATED | SDS_EXTERNAL_OBJECT | SDS_REALLOC);
  return(sds);
}

/*********************************************************************/
void
set_sys_vars(sds,dptr)
sds_handle  sds;
struct direc  *dptr;
/*********************************************************************/
{
  struct sds_control_p *scp = sds_control(sds);

  scp->dptr = dptr;
  scp->direc_size = SDS_FIXED;
  scp->shead = (struct sds_header *)((char *)dptr - dptr[0].offst);
  scp->tlist = scp->shead->list_size?
      (struct type_list *)((char *)scp->shead + BASE_OFFSET):TNULL;
  scp->heap = (char *)scp->shead 
      + (int)scp->shead->list_size
      + BASE_OFFSET;
  scp->current_heap = scp->heap + scp->shead->heap_size;
}

/*********************************************************************/
sds_handle
sds_start_heap(sds)
sds_handle  sds;
/*********************************************************************/
{
  struct sds_control_p *scp = sds_control(sds);

  if (scp->heap != NULL) return 0L;

  scp->current_heap = scp->heap = (char*) sds_malloc(SDS_INC_HEAP);

  scp->heap_size = SDS_INC_HEAP;
  scp->allofl |= SDS_HEAP_ALLOC;
  return 1L;
}
/*********************************************************************/
sds_handle
sds_inc_heap(sds,count)
sds_handle sds;
int count;
/*********************************************************************/
{

  struct sds_control_p *scp = sds_control(sds);
  int  delta = scp->current_heap - scp->heap;

  scp->heap_size += count * SDS_INC_HEAP;

  scp->current_heap = scp->heap = 
  sds_realloc(scp->heap,(unsigned)scp->heap_size);

  scp->current_heap += delta;
  return 1L;
}
/*********************************************************************/
sds_off_t
sds_add_to_heap(sds_handle sds,char *buffer,char delim)
/*********************************************************************/
{

  struct sds_control_p *scp = sds_control(sds);
  long  temp,count;
  int  incr,space_left,size = (int)strlen(buffer) + 1;

  if (!scp->heap) 
    if (!sds_start_heap(sds))
      return -1L;

  space_left = scp->heap_size - (scp->current_heap - scp->heap);

  if ((incr  = (size - space_left)) > 0) 
  {
    incr = 1 + incr/SDS_INC_HEAP;
    if (!(sds_inc_heap(sds,incr)))
      return -1L;
  }

  count = sds_namelist(scp->current_heap,buffer,delim);

  temp = (scp->current_heap - scp->heap) + (count << 16);

  scp->current_heap += size;

  return temp;
}
/*********************************************************************/
struct direc *
sds_inc_direc_size(sds)
sds_handle  sds;
/*********************************************************************/
{
  struct sds_control_p *scp = sds_control(sds);
  scp->direc_size += SDS_INC_DIREC;
  scp->dptr = (struct direc *)sds_realloc((char *)scp->dptr,
      (unsigned int)(scp->direc_size*sizeof(struct direc)));
  return scp->dptr;
}
  
/*********************************************************************/
long
tlist_len(tyl)
struct  type_list  *tyl;
/*********************************************************************/
{
  long  len = (long)0;
  if (tyl == TNULL) return(len);
  while ((tyl++)->elemcod != SDS_ENDLIST) len++;
  return(++len);
}
/*********************************************************************/
sds_handle 
sds_define_structure(sds,ty_list,names)
sds_handle  sds;
struct type_list *ty_list;
char  *names;
/*********************************************************************/
{
  long new_size = 0,old_size = 0;
  struct type_list *size_align,tlist_head;
  struct type_list *new_list,*ttemp = ty_list,*oldlist;
  struct sds_control_p *scp = sds_control(sds);
  int  i;
  char  align;

/* clear up any bitfield definitions passed in.... */
  sds_fixbits(ty_list);

/*  How big is the new one? (NB: delimited by SDS_ENDLIST, not SDS_RETLIST */
  new_size = tlist_len(ty_list) + (long)2L;
/*  (A size/namelist pair will be added:, and the size/alignment type
  pair:hence the '+2' */

/*      And the old one?                                                */
  if ((oldlist = scp->tlist) != NULL)
    old_size = tlist_len(oldlist) - (long)1L;
/*  Here I subtract 1 to throw away the SDS_ENDLIST which marks the
  end of the type_list buffer
 */

/*  This is the first tlist entry: giving number of elements in the
  object and, if provided, a pointer to and length of its namelist
 */
  tlist_head.elemcod = SDS_LENLIST;
  if (names != (char *)0) 
    tlist_head.nelems = sds_add_to_heap(sds,names,',');
  else
    tlist_head.nelems = (unsigned long)0;

  if (old_size)/* There is an existing list, so get more space....  */
  {
     scp->tlist = new_list = (struct type_list *)sds_realloc(scp->tlist,
        (unsigned)(new_size+old_size) * sizeof(struct type_list));
  }
  else 
  { /*  No old list, simple malloc needed...        */

    scp->tlist = new_list = 
       (struct type_list *)sds_malloc((unsigned long)new_size * 
          (unsigned long)sizeof(struct type_list));
    scp->allofl |= SDS_TLIST_ALLOC;
  }

/*  ...and put the new list on the end          */
  new_list += old_size;
  new_list->nelems = tlist_head.nelems;
  new_list->elemcod = tlist_head.elemcod;
  new_list++;

/*  the next slot is for the size/align pair, calculated soon  */
  size_align = new_list++;

  for (i=0;i<new_size - 2;i++) 
  {
    new_list->nelems = ttemp->nelems;
    new_list->elemcod = ttemp->elemcod; 
    if ((new_list->elemcod & SDS_LOCALLIST)) 
    {
      new_list->elemcod &= ~SDS_LOCALLIST;
      new_list->elemcod |= SDS_INDLIST;
      new_list->elemcod += old_size;
    }
    new_list++;
    ttemp++;
  }
  old_size |= SDS_INDLIST;

  size_align->elemcod = SDS_SIZE_ALIGN;
  size_align->nelems = sds_tlsize(sds,old_size,&align);
  size_align->elemcod |= (long)(align & 0xff);

/*  Return the pointer with its indirection flag      */
  return(old_size);
}  
/*********************************************************************/
sds_handle
sds_resize_object(sds,object,new_size)
sds_handle  sds;
sds_code object;
sds_code new_size;
/*********************************************************************/
{

  struct direc   *dptr = sds_direc(sds);
  sds_handle ret = 1L;

  if ( dptr[0].offst != SDS_NOT_ASSEMBLED)
  {
     sds_push_error(SDS_NOT_ASSEMBL,SDS_WARNING,"Resize attempted, unassembled dataset");
      ret = 0L;
  }
  else if (dptr[object].nelems != new_size)
  {
      dptr[object].nelems = new_size;
      dptr[object].illoca |= SDS_REALLOC;
  }
  else 
  {
    sds_push_error(SDS_CANNOT_RESIZE,SDS_WARNING,"Null resize attempted");
    ret = 0L;
  }
  return ret;
}
/*********************************************************************/
void
sds_destroy(sds)
sds_handle sds;
/*********************************************************************/
{
  sds_handle     object;
  struct direc   *dptr = sds_direc(sds);

  if (dptr == DNULL)
    return;

  if (sds_source(sds) == SDS_SHARED_MEM)
  {
#ifdef SHMEM
      sds_discard(sds);
#endif
  } 
  else if (sds_source(sds) == SDS_FILE)
  {
    sds_discard(sds);
  } 
  else 
  {
    for (object = 1;object < dptr[0].nelems;object++)
    {
      if (dptr[object].illoca & SDS_WAS_ALLOCATED)
        free(sds_obind2ptr(sds,object));
      else if (dptr[(int)object].structype == SDS_RECORDS)
        sds_destroy_record_def(
                 (struct record_header *)sds_obind2ptr(sds,object),0);
    }
    sds_discard(sds);
  }
}
/*********************************************************************/
void
sds_discard(sds)
sds_handle  sds;
/*********************************************************************/
{ 
  int      object_count;
  char  ***elstart;
  int    **varcount;
  char    *temp;

  struct sds_control_p *scp = sds_control(sds);
  struct direc         *dptr = sds_direc(sds);

#ifdef MEMMAP
  int disob,object;
#endif

  if (dptr == DNULL)
    return;

  temp = (char *)sds_head(sds);
  object_count = dptr[0].nelems;
/* Clean up the three malloc regions and the serial stream and 
   flag everything NULL
*/
  if (scp->source == SDS_SHARED_MEM)
  {
#ifdef SHMEM
    shm_quit(dptr);
#endif
  }
#ifdef MEMMAP
  else
  if (scp->source == SDS_MAPPED_MEM)
  {
    disob = 0;
    /* find out if an object has been searately mapped:
       if not, the whole file must have been */
    for (object=1; object<object_count;object++)
      if (dptr[object].illoca == SDS_DISJOINT_OBJECT)
        disob = 1;
    if (disob == 0 || dptr[0].illoca == SDS_DISJOINT_OBJECT)
      munmap((char *)sds_head(sds), sds_fullsize(sds));
    else
    {
      for (object=1; object<object_count;object++)
        if (dptr[object].illoca == SDS_DISJOINT_OBJECT)
          munmap((char *)sds_obind2ptr(sds,object),
                      dptr[object].nelems * dptr[object].elemsz);
    }
  } 
#endif
  else if (scp->source == SDS_FILE)
  {
    if ((scp->heap != NULL) && (scp->allofl & SDS_HEAP_ALLOC)) 
    {
      scp->allofl &= ~SDS_HEAP_ALLOC;
      free(scp->heap);
    }
    free(temp);
  }
  else 
  {
    if ((dptr  != DNULL) && (scp->allofl & SDS_DPTR_ALLOC)) 
    {
      scp->allofl &= ~SDS_DPTR_ALLOC;
      free((char *)dptr);
    }
    if ((scp->tlist != TNULL) && (scp->allofl & SDS_TLIST_ALLOC)) 
    {
      scp->allofl &= ~SDS_TLIST_ALLOC;
      free((char *)scp->tlist);
    }
    if ((scp->heap != NULL) && (scp->allofl & SDS_HEAP_ALLOC)) 
    {
      scp->allofl &= ~SDS_HEAP_ALLOC;
      free(scp->heap);
    }
    if ((scp->shead != NULL) && (scp->allofl & SDS_HEAD_ALLOC)) 
    {
      scp->allofl &= ~SDS_HEAD_ALLOC;
      free(scp->shead);
    }
    if ((scp->dup_size != NULL) && (scp->allofl & SDS_DUP_ALLOC)) 
    {
      scp->allofl &= ~SDS_DUP_ALLOC;
      free(scp->dup_size);
    }
  }
  elstart = scp->element_start;
  varcount =  scp->varel_count;
  if (elstart != NULL)
  {
    int i;
    for (i=1;i<object_count;i++)
    {
      if (dptr[i].illoca != SDS_DISJOINT_OBJECT)
      {
        if (elstart[i] != NULL)
          free(elstart[i]);
        if (varcount[i] != NULL)
          free((char *)varcount[i]);
      }
    }
    free(elstart);
    free((char *)varcount);
  }
  scp->shead = NULL;
  scp->dup_size = NULL;
  scp->dptr = NULL;
  scp->stream = SDS_FILE_OP;
  scp->tlist = NULL;
  scp->source = 0;
  scp->heap = NULL;
  if (scp->src) sds_delete_rescon(scp->src);
  if (scp->load_name) free(scp->load_name);
  if (scp->target_name) free(scp->target_name);
  if (scp->file_offsets) free(scp->file_offsets);
        sds_delete_scp(scp);
}
/*********************************************************************/
sds_handle
sds_declare_structure(sds,obj_ptr,name,number,code)
sds_handle sds;
void  *obj_ptr;
char  *name;
sds_code number;
sds_code code;
/*********************************************************************/
{
  struct direc *ddptr;
  sds_code      object;
  sds_handle    tsz;
  unsigned char atype;
  struct sds_control_p *scp;
  short structype = 0;

  if (code & SDS_RECLIST)
  {
     code &= ~SDS_RECLIST;
     structype = SDS_RECORDS;
  }
  if (!(scp = sds_control(sds)))
  {
    sds_push_error(SDS_NO_SUCH_SDS, SDS_ERROR, "Adding object to non-existant dataset");
    return 0L;
  }
  if (obj_ptr == SDS_ALLOCATE && scp->target_name)
  {
    sds_push_error(SDS_FILE_OP,SDS_ERROR,"Cannot allocate for targeted file");
    return 0L;
  }
  
/*  If I can't find this thing's size, it is a. not a known
  primitive and b. not a described complex object. So bug off.
*/
  if (code & SDS_INDLIST)
    tsz = sds_tlsize(sds,code,(char *)&atype);
  else
    tsz = sds_element_size(sds,code,&atype);

  if (tsz == 0)
  {
    sds_push_error(SDS_ZERO_LENGTH, SDS_ERROR, "Adding incomplete object");
    return 0L;
  }

/*  Found it. Add one to directory list....      */
  ddptr = scp->dptr;
  object = (int)ddptr[0].nelems++;

/*  May have to get more memory for the directory structure  */
  if (object == scp->direc_size) 
    ddptr = sds_inc_direc_size(sds);

/*  Fill in the definition of this object      */
  ddptr[object].offst = (sds_off_t)obj_ptr;
  ddptr[object].elemcod = code;
  ddptr[object].elemsz = tsz;
  ddptr[object].nelems = number;
  ddptr[object].wtime = 0;
  ddptr[object].illoca = 0;
  ddptr[object].align_type = atype;
  ddptr[object].structype = structype;

  if ((!sds_pname(sds,(long)object,name)))
    sds_push_error(SDS_NO_MEM, SDS_WARNING, "While adding object");

/* Write to disk if requested */
  if (scp->target_name != NULL)
  {
    long padbytes[2];
    char b[4096];
    int c = 0,s = 4096;

    off_t cur = lseek(scp->tstream, 0L, SEEK_CUR);
    int pads = align_delta((sds_off_t)cur ,ddptr[(int)object].align_type);
    padbytes[0] = padbytes[1] = 0L;
    if (pads && sds_buffered(scp->tstream, (char *)padbytes, pads, b,s,&c) != pads)
    {
      sds_push_error(SDS_FILE_WR, SDS_ERROR, "Adding data object to file");
      return 0L;
    }
    if (!sds_write_object(scp->tstream,sds,object,obj_ptr,b,s,&c))
    {
      sds_push_error(SDS_FILE_WR,SDS_ERROR, "Adding data object to file");
      return 0L;
    }
    sds_flush(scp->tstream, b,&c);
  }
/*  And return its object number        */
  return(object);
}
/*********************************************************************/
char **
sds_saverestore(sds,dptr, flag)
sds_handle sds;
struct direc *dptr;
int flag;
/*********************************************************************/
{
  int           i;
  long          junk = 0;
  struct sds_saverestore *sr = sds_saver();

/*  I'm going to put the real object addresses in this pointer list        */
  if (flag == SDS_SAVE_STATE)
  {
    sr->opointer = (char **)sds_malloc((unsigned int)dptr[0].nelems *
            (unsigned int)sizeof(char *));
    if (sr->opointer == 0 )
    {
      sds_push_error(SDS_NO_MEM, SDS_ERROR,"calloc failure, sds_make");
      return (char **)0;
    }
  
  /*  Get the addresses from the approved routine ...  */
  /*  Save the directory state in case it's NOT_ASSEMBLED */
    sr->sds_state = (char *)dptr[0].offst;
    for (i=0;i < dptr[0].nelems;i++) 
      sr->opointer[i] = (char*) sds_getp(dptr,i);
  
    if (sr->sds_state == (char *)SDS_NOT_ASSEMBLED)
      sr->status = offil(sds, 0, 0, &junk);

    return sr->opointer;
  }
  else
  {
/* leave things as before in case dataset expansion is required */
  
    if (sr->sds_state == (char *)SDS_NOT_ASSEMBLED) 
    {
      dptr[0].offst = (sds_off_t)sr->sds_state;
      for (i=1;i < dptr[0].nelems;i++)
        dptr[i].offst = (sds_off_t)sr->opointer[i];
    }
    free((char *)sr->opointer);
    return NULL;
  }
}

/*********************************************************************/
sds_handle
sds_ass(sds,name,flg)
sds_handle sds;
sds_code flg;
char  *name;
/*  'assemble' means assemble the bits of an SDS into a coherent
  thing. That means the target place for assembly must be specified.
  In the simplest case - assemble in process memory - the only 
   action will be to allocate memory for any objects which don't
  yet have any - no new SDS is created. For assembly to *shared memory*
  a contiguous memory chunk has to be filled, so a new SDS is made.
  Assembling to file or streaming to a network do generate a new
  SDS but it is NOT accessible to the generating process without
  more work (eg sds_load) so no new SDS index is returned
*/
/*********************************************************************/
{
  struct direc   *dptr;
  char           *cptr;
  sds_handle     sds_return = sds;
  unsigned long  i;
  int            tw,ow,fd;
  char         **object_pointer;
  struct sds_control_p *scp;
  long           padbytes[2];

  char b[4096];
  int c = 0,s = 4096;
  
#ifdef SHMEM
  sds_handle    cp_size;
  sds_off_t    new_base,new_offset;
  struct direc *new_dptr;
#endif

  padbytes[0] = padbytes[1] = 0L;
  
  if (!(scp = sds_control(sds)))
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Assembling non-existant dataset");
    return 0L;
  }
  dptr = sds_direc(sds);

  if ((scp->target_name))
  {
    char buf[1024];
    char *ch;
    int nb,pads;
    char *tname = sds_malloc(strlen(scp->target_name) + 5);
    if ((ch = strrchr(scp->target_name,'.')) != (char *)0)
      *ch = (char)0;
    strcpy(tname,scp->target_name);
    strcat(tname,".tmp");
    if (scp->tstream != SDS_FILE_OP)
      sds_close_fd(scp->tstream);
    if ((scp->tstream = sds_open_file(scp->target_name, O_RDONLY)) == -1)
    {
      sds_push_error(SDS_FILE_OP,SDS_ERROR, "Could not open data file");
      sds_return =  0L;
    }
    else if ((fd = sds_open_file(tname,O_RDWR)) >= 0) 
    {
      object_pointer = sds_saverestore(sds,dptr,SDS_SAVE_STATE);
      if ((tw = sds_write_header(fd,sds,b,s,&c)) < 0)
      {
        sds_push_error(SDS_FILE_WR,SDS_ERROR,"Failure writing header to file");
        sds_return = 0L;
      }
      else if ((pads = sds_prepads(&dptr[0], tw)) > 0)
      {
        if (sds_buffered(fd, (char *)padbytes, pads,b,s,&c) != pads)
        {
          sds_push_error(SDS_FILE_WR,SDS_ERROR,"Failure writing pads to file");
          sds_return = 0L;
        }
        else 
          tw += pads;
      }
      if ((ow = sds_write_object(fd,sds,0,(char *)scp->dptr,b,s,&c)) < 0)
      {
        sds_push_error(SDS_FILE_WR,SDS_ERROR,
                  "Failure writing directory to file");
        sds_return = 0L;
      }
      tw += ow;
      if (lseek(scp->tstream,0,SEEK_SET) == -1)
      {
        sds_push_error(SDS_FILE_OP,SDS_ERROR,"lseek on data target");
        sds_return = 0L;
      }
      else
      {  
        while((nb = read(scp->tstream, buf, 1024)) > 0)
          sds_buffered(fd,buf, nb,b,s,&c);
        if (nb == -1)
        {
          sds_push_error(SDS_FILE_RD,SDS_ERROR,"read on data target");
          sds_return = 0L;
        }
        sds_flush(scp->tstream,b,&c);
        sds_close_fd(scp->tstream);
        scp->tstream = (int)NULL;
      }
      sds_close_fd(fd);
    }
    else 
    {
      sds_push_error(SDS_FILE_OP,SDS_ERROR, "Could not open file");
      sds_return = 0L;
    }
    if (sds_return)
    {
#if defined(__MSDOS__)
       unlink(scp->target_name);
#endif
       rename(tname,scp->target_name);
    }
    free(tname);
    sds_saverestore(sds,dptr,SDS_RESTORE_STATE);
  } 
  else if ((flg & SDS_FILE))  /* no new sds created */
  {
    object_pointer = sds_saverestore(sds,dptr,SDS_SAVE_STATE);
    if ((fd = sds_open_file(name,(int)flg)) >= 0) 
    {
      if (!to_file(sds,fd,object_pointer)) 
      {
         sds_push_error(SDS_FILE_WR,SDS_ERROR,
                          "Failure writing dataset to file");
         sds_return = 0L;
      }
      sds_close_fd(fd);
    }
    else 
    {
      sds_push_error(SDS_FILE_OP,SDS_ERROR, "Could not open file");
      sds_return = 0L;
    }
    sds_saverestore(sds,dptr,SDS_RESTORE_STATE);
  } 
  else if (flg & SDS_SYBASE)  /* no new sds created */
  {
#ifndef  SDSDB
    sds_push_error(SDS_FILE_OP,SDS_ERROR,"Database operations not supported:");
    return 0L;
#else
    object_pointer = sds_saverestore(sds,dptr,SDS_SAVE_STATE);
    sds_return = sds_db_make(sds,name,name,flg,object_pointer);
    sds_saverestore(sds,dptr,SDS_RESTORE_STATE);
#endif
  }
#ifdef SHMEM
  else if (flg & SDS_SHARED_MEM)  /* a new sds is created */
  {
    if ((sds_return = next_sds()) >= 0)
    {
      object_pointer = sds_saverestore(sds,dptr,SDS_SAVE_STATE);

/*  Allocate the memory          */
      if ((cptr = (char *)shm_make(name,
                   sds_fullsize(sds),0666)) == (char *)0) 
      {
        sds_push_error(SDS_NO_MEM,SDS_ERROR, "Shared memory failure");
        return 0L;
      }

/*  Make and load the sds header,tlist and heap    */
      cptr += sds_mem_header(sds,cptr);
      cptr += align_delta((sds_off_t)cptr,dptr[0].align_type);

/*  How big is the directory?        */
      cp_size = (sds_handle)dptr[0].nelems*dptr[0].elemsz;

/*  Copy the directory over          */
      memcpy(cptr,(char *)dptr,cp_size);

      new_dptr = (struct direc *)cptr;
      new_base = (sds_off_t)cptr;

      set_sys_vars(sds_return,(struct direc *)cptr);

      cptr += cp_size;

/*  And all the objects          */

      for (i=1;i < dptr[0].nelems;i++) 
      {
        new_dptr[i].illoca = SDS_EXTERNAL_OBJECT;
        cptr += align_delta((sds_off_t)cptr,dptr[i].align_type);
        new_offset = new_dptr[0].offst + (sds_off_t)cptr - new_base;
        if (new_offset != new_dptr[i].offst ) 
          new_dptr[i].offst = (sds_off_t)new_offset;
        cp_size = (sds_handle)dptr[i].nelems;

        if (dptr[i].illoca & SDS_REALLOC)
          if (scp->dup_size[i] < dptr[i].nelems)
            cp_size = scp->dup_size[i];

        cp_size *= dptr[i].elemsz;
        if (new_dptr[i].structype & SDS_RECORDS)
        {
          cptr += sds_copy_records(sds_return,cptr,
                     (sds_record_handle *)object_pointer[i]);
          new_dptr[i].structype &= ~SDS_RECORDS;
        }
        else 
        {
          if (object_pointer[i] != SDS_ALLOCATE)
            memcpy(cptr, object_pointer[i], cp_size);
          cptr += (int)dptr[i].nelems*dptr[i].elemsz;
        }
      }
      sds_saverestore(sds,dptr,SDS_RESTORE_STATE);
    }
  }
#endif
  else if (flg & SDS_PROC_MEM)  /* Just do necessary memory allocation */
  {
    for (i=1;i < dptr[0].nelems;i++) 
    {
      if ((dptr[i].offst == (sds_code)SDS_ALLOCATE))
      {
        dptr[i].offst = (sds_off_t)sds_calloc(dptr[i].nelems,dptr[i].elemsz);
        dptr[i].illoca |= SDS_WAS_ALLOCATED;
      }
      else if (dptr[i].illoca & SDS_REALLOC)
      {
      /* Can't: have to copy over?
        if (dptr[i].illoca & SDS_WAS_ALLOCATED)
          free((char *)dptr[i].offst;
      */
        dptr[i].offst = (sds_off_t)sds_calloc(dptr[i].nelems,dptr[i].elemsz);
        dptr[i].illoca |= SDS_WAS_ALLOCATED;
        dptr[i].illoca &= ~SDS_REALLOC;
      }
      else
      {
        dptr[i].illoca |= SDS_EXTERNAL_OBJECT;
      }
    }
    scp->is_proto = 0;
  }
  else /* this is an unknown target */ 
  {
    sds_return = 0L;
    sds_push_error(SDS_TRANSFER_UNDEF,SDS_WARNING,
                     "Do not understand this assembly target");
  }
  return sds_return;
}
/*********************************************************************/
sds_handle
to_file(sds,fd,object_pointer)
sds_handle sds;
int fd;
char **object_pointer;
/*********************************************************************/
{
  struct direc *dptr;
  unsigned long  i;
  sds_handle total_write = 0;
  sds_handle pads, ob_write;
  long       padbytes[2];

  char b[4096];
  int c= 0,s= 4096;

  padbytes[0] = padbytes[1] = 0L;

  if ((dptr = sds_direc(sds)) == DNULL) 
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Writing non-existant dataset to file");
    return 0L;
  }

  if ((total_write = sds_write_header(fd,sds,b,s,&c)) < 0)
  {
    sds_push_error(SDS_FILE_WR,SDS_ERROR,"Writing header failed");
    return 0L;
  }

  for (i=0;i < dptr[0].nelems;i++)
  {
     if (dptr[i].illoca != SDS_DISJOINT_OBJECT)
     {
       if ((pads = sds_prepads(&dptr[i], total_write)) > 0)
       {
         if (sds_buffered(fd, (char *)padbytes, pads,b,s,&c) != pads)
         {
           sds_push_error(SDS_FILE_WR,SDS_ERROR,"Writing pads failed");
           return 0L;
         }
         else 
           total_write += pads;
       }
       if ((ob_write = 
          sds_write_object(fd,sds,i,object_pointer[i],b,s,&c)) < 0)
       {
         sds_push_error(SDS_FILE_WR,SDS_ERROR,"Writing object");
         return 0L;
       }
       else 
         total_write += ob_write;
    }
  }
  sds_flush(fd,b,&c);
  return 1L;;
}
/*********************************************************************/
sds_handle
sds_prepads(dptr, total_write)
struct direc *dptr;
sds_handle total_write;
/*********************************************************************/
{
  return (sds_handle)align_delta((sds_off_t)total_write,dptr->align_type); 
}
/*********************************************************************/
sds_handle
sds_mem_header(sds,cptr)
sds_handle  sds;
char  *cptr;
/*********************************************************************/
{
  struct sds_header *sdsh = (struct sds_header *)cptr;
  sdsh->magic_number = (long)SDS_MAGIC;
  sdsh->version = SDS_VERSION;
  sdsh->controlbits |= SDS_BIGADDR?SDS_FROM_BIGADDR:0;
  sdsh->list_size = (short)tlist_size(sds_tlist(sds));
  sdsh->heap_size = (short)sds_heap_size(sds);
  sdsh->heap_size += (short)align_delta((sds_off_t)sdsh->heap_size,4);
  cptr += sizeof(struct sds_header);

  if (sdsh->list_size) /* there is a tlist  */
  {
    memcpy(cptr,(char *)sds_tlist(sds),(int)sdsh->list_size);
    cptr += sdsh->list_size;
  }
  memcpy(cptr,sds_heap(sds),(int)sdsh->heap_size);

  return((int)sdsh->list_size +(int)sdsh->heap_size + sizeof(struct sds_header));
}
/*********************************************************************/
sds_handle
sds_write_header(int fd,sds_handle sds,char *b, int s, int *c)
/*********************************************************************/
{
  struct sds_header sdsh;
  int ret_size = 0L;
  sdsh.magic_number = (long)SDS_MAGIC;
  sdsh.version = SDS_VERSION;
  sdsh.controlbits = SDS_BIGADDR?SDS_FROM_BIGADDR:0;
  sdsh.list_size = (short)tlist_size(sds_tlist(sds));
  sdsh.heap_size = (short)sds_heap_size(sds);

  ret_size += sds_buffered(fd,(char *)&sdsh,sizeof(struct sds_header),b,s,c);
  if (sdsh.list_size)
    ret_size += sds_buffered(fd,(char *)sds_tlist(sds),(int)sdsh.list_size,b,s,c);
  ret_size += sds_buffered(fd,sds_heap(sds),(int)sdsh.heap_size,b,s,c);
  return ret_size;
}
/*********************************************************************/
sds_handle
sds_write_object(int fd,sds_handle sds,sds_code i,void *pointer,
                       char *b,int s, int *c)
/*********************************************************************/
{
  unsigned long  pad_size = 0;
  unsigned long  realloc_size = 0;
  unsigned long  total_size = 0;
  unsigned long  temp_size = 0;
  struct direc   *dptr = sds_direc(sds);
  unsigned char    pattern = 0xff;

  total_size = dptr[i].nelems*dptr[i].elemsz;
  realloc_size = total_size;

  if (i == 0) 
  {
/* Directory: do not copy over RECORD flags, and mark disjoint objects as
   unknown address */
    int obcount;
    pointer = sds_malloc(total_size);
    memcpy(pointer, (char *)dptr, total_size);
    dptr = (struct direc *)pointer;
    for (obcount=0;obcount<dptr[0].nelems;obcount++)
    {
      if (dptr[obcount].structype == SDS_RECORDS)
        dptr[obcount].structype = SDS_NORMAL_OBJECT;
      if (dptr[obcount].illoca == SDS_DISJOINT_OBJECT)
        dptr[obcount].offst = SDS_IMPOSSIBLE_ADDRESS;
    }
  }
  else if (dptr[i].structype == SDS_RECORDS)
  {
    return sds_write_records(sds, fd, (sds_record_handle *)pointer,b,s,c);
  }

  if ((i != 0) && (dptr[i].illoca & SDS_REALLOC) && sds_dup_size(sds))
  {
    temp_size = sds_dup_size(sds)[i] * dptr[i].elemsz;
    if (temp_size < total_size)
    {
      realloc_size = temp_size;
      pad_size = total_size - realloc_size;
      pattern = 0x00;
    }
  }

  if (pointer == SDS_ALLOCATE) 
  {
      pad_size = total_size;
      realloc_size = 0;
  }

/*  If this thing is unallocated, I'll write ff bytes to
  indicate that there is undefined junk in it.
*/
  if (realloc_size != 0)
  {
    if (sds_buffered(fd,pointer,(int)realloc_size,b,s,c) !=
                      (int)realloc_size) 
    {
      sds_push_error(SDS_FILE_WR,SDS_ERROR,"Writing object data");
      return 0L;
    }
  }
  if (pad_size != 0)
  {
    if (sds_write_pattern(fd,pad_size,pattern,b,s,c) < 0) 
    {
      sds_push_error(SDS_FILE_WR,SDS_ERROR,"Writing null pattern to object");
      return 0L;
    }
  }
  if (i == 0)
    free(pointer);
  return total_size;
}

/*********************************************************************/
sds_handle
sds_load_direc(sds_handle sds,int fd, struct sds_header *h)
/*********************************************************************/
{
  struct direc dtemp,*dptr, *dtp = &dtemp;
  char  *list_ptr,*cptr;
  int  ierr,ndirecs,lhsize;
  int  o,ar;
  struct sds_header header;
  struct sds_control_p *scp = sds_control(sds);

/*  Read in the header ;     */
  if (h) /* Header has already been read in 
            ...probably as test for conversion */
  {
    memcpy(&header, h, sizeof(struct sds_header));
  }
  else if  ((ierr = sds_read_header(sds,fd,&header)) == 0) 
  {
    scp->stream = SDS_FILE_OP;
    sds_push_error(ierr,SDS_ERROR,"Directory load");
    return 0;
  }

  lhsize = (int)header.heap_size + (int)header.list_size;
  lhsize += align_delta(lhsize + BASE_OFFSET,sds_palign(SDS_DIRECTORY_STRUCTURE));

  list_ptr = (char*) sds_malloc((unsigned)lhsize);

  if (sds_read(fd,lhsize,list_ptr) != lhsize)
  {
    sds_push_error(SDS_FILE_NOP,SDS_ERROR,"Unexpected end-of-dataset");
    return 0;
  }

/*  Find size of directory        */
  if (sds_read(fd,sizeof(struct direc),(char *)dtp) != 
                      sizeof(struct direc))
  {
    sds_push_error(SDS_FILE_NOP,SDS_ERROR,"Unexpected end-of-dataset");
    return 0;
  }
  ndirecs = (int)dtemp.nelems;

/*  throw away previous directory      */
  if (scp->dptr != DNULL) 
  {
    free((char *)scp->dptr);
    scp->dptr = DNULL;
  }

/*  New directory,tlist heap  and header go here    */
  ar = ((ndirecs*sizeof(struct direc)) +
      (unsigned int)BASE_OFFSET +  
      (unsigned int)header.heap_size +
      (unsigned int)header.list_size);
  ar += align_delta(ar,sds_palign(SDS_DIRECTORY_STRUCTURE));
  cptr = (char*) sds_malloc(ar);

  scp->allofl = SDS_HEAD_ALLOC;


/*  Copy in header.....        */
  memcpy(cptr,(char *)&header,BASE_OFFSET);

  cptr += BASE_OFFSET;

/*  Copy in tlist and heap        */
  memcpy(cptr,list_ptr,lhsize);

  cptr += lhsize;

  dptr = (struct direc *)cptr;

/*  Copy in top directory.....      */
  memcpy(cptr,(char *)&dtemp,sizeof(struct direc));

  set_sys_vars(sds,(struct direc *)cptr);

  cptr += sizeof(struct direc);

/*  Read in rest of directory.....      */
  if (sds_read(fd,sizeof(struct direc)*(ndirecs-1),cptr) !=
                      sizeof(struct direc)*(ndirecs-1))
  {
    sds_push_error(SDS_FILE_NOP,SDS_ERROR,"Unexpected end-of-dataset");
    return 0;
  }

/*  reclaim some space.        */
  free(list_ptr);

/* Save offsets and mark object addresses impossible */
  scp->file_offsets = (sds_handle *)malloc(dptr[0].nelems * sizeof(long));
  for (o = 1; o< dptr[0].nelems; o++)
  {
    scp->file_offsets[o] =  (long)dptr[(int)o].offst;
    dptr[(int)o].offst = SDS_IMPOSSIBLE_ADDRESS;
  }

  return sds;
}

/*********************************************************************/
sds_handle
sds_read_header(sds_handle sds,int fd,struct sds_header *header)
/*********************************************************************/
{
  if (sds_read(fd,sizeof(struct sds_header),(char *)header)
               != sizeof(struct sds_header))
  {
    sds_push_error(SDS_FILE_RD,SDS_ERROR,"Header load");
    return 0L;
  }
  if (header->version != SDS_VERSION ) 
  {
    sds_push_error(SDS_BAD_VERSION,SDS_WARNING,"Header load");
    return 0L;
  }
  return 1L;
}
/*********************************************************************/
int
sds_open_file(name,flg)
int  flg;
char  *name;
/*  Here be System Dragons eg VMS's plethora of file type flags   */
/*********************************************************************/
{
  int  fflag = O_RDWR,fd;
  char errstring[128];
  errstring[0] = (char )0;

  if (flg != O_RDONLY)
    fflag |= (flg & (int)SDS_APPEND)?O_APPEND:O_CREAT | O_TRUNC;
  else
    fflag = flg;

#ifdef vms
  if (!(fflag & O_APPEND)) 
  {
    if ((fd = creat(name,0666,"ctx=nocvt","ctx = bin","mrs = 2048","rfm=udf")) < 0) {
       strcpy(errstring,"Cannot create file ");
    }
  }
  else
#endif

  if ((fd = open(name,fflag,0666)) == -1) 
  {
    strcpy(errstring,"Cannot open file ");
  }
  if (*errstring != (char)0)
  {
    strcat(errstring, name);
    sds_push_error(SDS_FILE_OP,SDS_ERROR,errstring);
    return -1;
  }

#ifdef VXWORKS
  lseek(fd,0,SEEK_END);  /* Go to end of file. */
#endif

  return(fd);
}

/*********************************************************************/
sds_handle
sds_write_pattern(int fd,unsigned long size,char pattern,char *b,int s, int *c)
/*  Write crud to unallocated object going to eg file, although
  I'm not really sure why anyone would do it. Still, it's possible
  so one must do something rational-ish
*/
/*********************************************************************/
{
#define SDS_JUNK_BUFFER  256
  char  buffer[SDS_JUNK_BUFFER];
  int  i,mod = (int)size/SDS_JUNK_BUFFER;
  int  frac = (int)size%SDS_JUNK_BUFFER;
  memset(buffer,pattern,SDS_JUNK_BUFFER);
  if (mod > 0)
    for (i=0;i<mod;i++)
    {
      if (sds_buffered(fd,buffer,SDS_JUNK_BUFFER,b,s,c) < 0) 
      {
        sds_push_error(SDS_FILE_WR,SDS_ERROR,"Writing spacers to file");
        return 0;
      }
    }
  if (sds_buffered(fd,buffer,frac,b,s,c) <0) 
  {
    sds_push_error(SDS_FILE_WR,SDS_ERROR,"Writing spacers to file");
    return 0;
  }
  return 1;
}
/*********************************************************************/
sds_handle
sds_which(dptr)
struct direc *dptr;
/*   Given a directory pointer , which sds does it refer to ?     */
/*********************************************************************/
{
  if (dptr != DNULL) 
  {
    sds_handle i;
    for (i=1L;i<=sds_max();i++)
      if (dptr == sds_direc(i))
        return(i);
  }
  return 0L;
}

/*********************************************************************/
float
sds_version(sds)
sds_handle sds;
/*********************************************************************/
{
  struct  sds_header   *head = sds_head(sds);
  float version;
  short ver;

  if (head == HNULL)
    ver = SDS_VERSION;
  else
    ver = head->version;

  version = (float)ver;

  return version;
}
/* Additions to support SDS to file descriptors. */

/***********************************************************************/
sds_handle
sds_write2fd(fd,sds)
int fd;
sds_handle sds;
/***********************************************************************/
{
  struct direc *dptr;
  sds_handle    sds_return = sds;
  char        **object_pointer;
  
  if ((dptr = sds_direc(sds)) == DNULL)
  {
    sds_push_error(SDS_NO_SUCH_SDS, SDS_ERROR, "Write to file desc");
    return 0L;
  }

  object_pointer = sds_saverestore(sds,dptr,SDS_SAVE_STATE);
  if (!to_file(sds,fd,object_pointer)) 
  {
    sds_push_error(SDS_FILE_WR, SDS_ERROR, "Could not send data to fd");
    sds_return = 0L;
  }
  sds_saverestore(sds,dptr,SDS_RESTORE_STATE);

  return(sds_return);
}

void
sds_settype(s,t,type, size)
struct type_list *s,*t;
sds_code type;
int size;
{
  unsigned short a = 0xff00;
  int nb,sz = 0;
  t++;
  while (s != t)
  {
    nb = s->nelems & SDS_BITFIELD_NMASK;
    if (*(char *)&a)                   /* Big endian or..... */
    {
      sz += nb;
      s->nelems = ((size - sz) << 16) + nb; 
    }
    else                               /* ....little endian! */
    {
      s->nelems = (sz << 16) + nb; 
      sz += nb;
    }
    s->elemcod = type;
    s++;
  }
  t--;
  t->nelems |= SDS_LAST_BITFIELD;
}

void 
sds_fixbits(struct type_list *tl)
{
  struct type_list *startbits;
  int bitson = 0, bitsize = 0;
  while (tl->elemcod != SDS_RETLIST)
  {
    if (tl->elemcod == SDS_BITFIELD)
    {
      if (!bitson)
      {
        bitson = 1;
        startbits = tl;
      }
      if ((bitson && tl[1].elemcod != SDS_BITFIELD) ||
          (bitsize > BITS(int)))
      {
        bitson = 0;
        if ((bitsize + tl->nelems) <= BITS(char))
        {
          tl->nelems += bitsize << 16;
          bitsize = 0;
          sds_settype(startbits,tl,SDS_CHAR_BITFIELD, 8);
        }
        else if ((bitsize + tl->nelems) <= BITS(short))
        {
          tl->nelems += bitsize << 16;
          bitsize = 0;
          sds_settype(startbits,tl,SDS_SHORT_BITFIELD, 16);
        }
        else if ((bitsize + tl->nelems) <= BITS(int))
        {
          tl->nelems += bitsize << 16;
          bitsize = 0;
          sds_settype(startbits,tl,SDS_LONG_BITFIELD, 32);
        }
        else
        {
          sds_settype(startbits,tl,SDS_LONG_BITFIELD,32);
          startbits = &tl[-1];
          bitsize = tl[-1].nelems & SDS_BITFIELD_NMASK;
          tl->nelems += bitsize  << 16;
          bitsize += tl[0].nelems & SDS_BITFIELD_NMASK;
          bitson = 1;
        }
      }
      else
      {
        tl->nelems += bitsize << 16;
        bitsize += tl->nelems & SDS_BITFIELD_NMASK;
      }
    }
    tl++;
  }
}

sds_handle
sds_pmem_attach(char *mem)
{
  sds_handle sds;
  struct direc *dptr;
  struct sds_header *h = (struct sds_header *)mem;

  if ((sds = next_sds()) < 0)
    return sds;
  dptr = (struct direc *)(mem + BASE_OFFSET + h->heap_size + h->list_size);
  set_sys_vars(sds, dptr);
  return sds;
}
