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

#ifdef HPUX
#include <ctype.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <math.h>

#ifndef vms
#include <unistd.h>
#endif

#if defined(vms)

#include "sdsgen.h"
#include "sds_externs.h"
#else
#include "Sds/sdsgen.h"
#include "Sds/sds_externs.h"
#endif

struct  dshort 
  {
  char  one;
  char  two;
  };
struct  dlong 
  {
  char  one;
  char  two;
  char  three;
  char  four;
};
struct  ddouble 
  {
  char  one;
  char  two;
  char  three;
  char  four;
  char  five;
  char  six;
  char  seven;
  char  eight;
  };

#if !defined(hpux) && !defined(VXWORKS) && !defined(mips)
#if !defined(vms)
#if !defined(__GCC_2__)
#if defined(__GNUC__)
#if __GNUC__ != 2
extern char  *memcpy();
#endif
#endif
#endif
#endif
#endif

/********* forward declarations ****************************************/
void   rswabw(char *, char *, int);
void   rswab(char *, char *, int);
void   rswabd(char *, char *, int);
void   sds_fix_tlist(struct sds_header *, struct type_list *, int);
void   sds_fix_header(struct sds_header *, int);
void   sds_fix_direc(struct direc *, int, int, int);
int    sds_copen(char *);

extern char  *load_sds_file(int, unsigned long);
extern void   r_float(char *, char *, int);
extern void   r_double(char *, char *, int);

sds_handle sds_lofd(sds_handle,int,unsigned long,struct sds_header *);

/***********************************************************************/
/* Load and convert from filename */
sds_handle
sds_load_conv(char *filename)
/***********************************************************************/
{
  int fd;
  sds_handle sds;
  if ((fd = open(filename,O_RDONLY,0644)) < 0) 
  {
    sds_push_error(SDS_FILE_OP,SDS_ERROR,"Foreign arch directory load");
    return 0L;
  }
  sds = sds_read_open_fd(fd, ffsiz(fd));
  sds_close_fd(fd);
  return sds;
}

/***********************************************************************/
/* Read from file descriptor, and convert if necessary */
/* A size of 0 means unknown - but if the size IS known, one
   can often get performance advantage, so we will use it if
   possible.
   If buffer is non-zero, some at least of the incoming data 
   has been loaded already.
 */
sds_handle 
sds_read_open_fd(int fd, int size)
/***********************************************************************/
{

  sds_handle  orig_state;
  sds_handle  new_sds = 0,old_sds = 0;
  int         i,old,new;
  long        lhsize, asize;
  long        new_type;
  long        del = 0;
  long        element_end_addr;
  char        junk[32], *j = junk;
  char       *cptr;

  struct sds_header     head;
  struct direc         *dptr;
  struct direc         *ndptr;
  struct sds_odesc     *othing,*nthing;
  struct sds_control_p *scp;
  struct type_list     *ntlist;
  struct sds_header    *nheader,*header;

  old_sds = next_sds();

/* Check to see if conversion is necessary */
  if (!(new_sds = sds_cload_direc(old_sds,fd,&orig_state,&head))) 
  {
    if (sds_last_return() == SDS_GOOD_FORMAT)
    /* In this case, cload_direc has loaded the header before 
       it found that conversion is not necessary
     */
    {
      if (!(new_sds = sds_lofd(old_sds, fd, size, &head)))
        sds_destroy(old_sds);
      return new_sds;
    }
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Foreign arch directory load");
    sds_destroy(old_sds);
    return 0L;
  }

  header = sds_head(old_sds);
  dptr = sds_direc(old_sds);

  /*   Duplicate this header for the new, native arch, dataset */
  /*   sds_cload_direc() has done a job of loading the direcs
       correctly, and adjusting offsets therein. We now prepare to
       read actual data objects.
   */

  lhsize = BASE_OFFSET + header->heap_size + header->list_size;
  lhsize += align_delta(lhsize,sds_palign(SDS_DIRECTORY_STRUCTURE));
  asize = lhsize + dptr[0].nelems * sizeof(struct direc);

  cptr = sds_malloc(asize);
  memcpy(cptr,(char *)header,asize);
  nheader = (struct sds_header *)cptr;

/* Some adaptions of this data to indicate native architecture */
  nheader->magic_number = SDS_MAGIC;

  ndptr = (struct direc *)(cptr + lhsize);
  ndptr[0].offst = (sds_off_t)ndptr - (long)nheader;

  new_sds = next_sds();
  scp = sds_control(new_sds);

  scp->allofl = SDS_HEAD_ALLOC;
  set_sys_vars(new_sds,ndptr);
  ntlist = sds_tlist(new_sds);
  ndptr = sds_direc(new_sds);

/* Check if the request has been to truncate 64-bit ints or put them as doubles:
   if so, convert() will throw away the extra incoming high-end int
   fields or do the conversion, and maybe check for overflow 
 */
  if (!SDS_BIGADDR && (sds_query_truncate_b() || sds_query_float_b()))
  {
    long l;
    sds_code code = sds_query_truncate_b()?SDS_LONG:SDS_DOUBLE;
    for (l=0;l<(int)nheader->list_size/sizeof(struct type_list);l++)
      if (ntlist[l].elemcod == SDS_DOUBLE_LONG)
        ntlist[l].elemcod = code;
    for (l=0;l<ndptr[0].nelems;l++)
      if (ndptr[l].elemcod == SDS_DOUBLE_LONG)
        ndptr[l].elemcod = code;
  }

  fix_sizes_and_aligns(new_sds);

  ndptr[0].offst = SDS_NOT_ASSEMBLED;
  for (i=1;i<dptr[0].nelems;i++) 
  {
    ndptr[i].offst = (long)SDS_ALLOCATE;
    ndptr[i].illoca = (char)0;
    dptr[i].illoca = (char)0;
  }

  new_sds = sds_ass(new_sds,"temp",SDS_PROC_MEM);

/* element_end_addr is there in case there is padding before the 
   next object. sds_resolve reports actuall addresses (although 
   in this case they are used as offsets, as the data is not yet
   in process memory)
 */
  element_end_addr = (long)dptr + dptr[0].nelems * sizeof(struct direc);

/*  The complete descriptions - header, tlist and
  directories - are now an accurate description
  of both input and output sds's. It remains to convert the 
  objects, which are pointed to by the object_pointer
  list and described by the direc's
 */

   for (i=1;i<ndptr[0].nelems;i++) 
   {
    sds_cleanup(new_sds);
    sds_cleanup(old_sds);
    while((old = sds_resolve(old_sds,i,&othing,SDS_OBJECT)) >= 0) 
    {
      new = sds_resolve(new_sds,i,&nthing,SDS_OBJECT);

      /* Extra pads bfore the next object? */
      if ((del = (long)(othing[old].address - element_end_addr)) > 0)
        if (sds_read(fd,del,j) != del)
        {
          sds_discard(new_sds);
          sds_discard(old_sds);
          return 0;
        }
      new_type = convert(old_sds,fd,&othing[old],
          &nthing[new],
          (int)orig_state);
      if (ndptr[i].elemcod & SDS_INDLIST)
        ntlist[nthing[new].ind-1].elemcod = new_type;
      else
        ndptr[i].elemcod = new_type;
      element_end_addr = (long)othing[old].address +
          othing[old].nelems * othing[old].size;
    }
    new = sds_resolve(new_sds,i,&nthing,SDS_OBJECT);
  }
  sds_destroy(old_sds);
  return(new_sds);
}

/***********************************************************************/
void
fix_sizes_and_aligns(sds)
sds_handle  sds;
/***********************************************************************/
{
  struct  direc *dptr = sds_direc(sds);
  struct  type_list  *ttptr = sds_tlist(sds);
  struct  sds_header  *head = sds_head(sds);
  char  align;
  unsigned  long  ind,i;
  unsigned long ntp = (unsigned long)head->list_size/sizeof(struct type_list);

  if (ntp) 
  {
    for (i=0;i<ntp;i++,ttptr++) 
    {
      if (ttptr->elemcod & SDS_SIZE_ALIGN) 
      {
        ind = (i+1) | SDS_INDLIST;
        ttptr->nelems = 
          sds_tlsize(sds,ind,&align);
        ttptr->elemcod = SDS_SIZE_ALIGN | (long)(align & 0xff);
      }
    }
  }
  for (i=0;i<dptr[0].nelems;i++)
    dptr[i].elemsz = sds_element_size(sds, dptr[i].elemcod,
        &dptr[i].align_type);
}

/***********************************************************************/
sds_code
convert(sds_handle sds,int fd,struct sds_odesc *old,
               struct sds_odesc *new,int bs_flag)
/***********************************************************************/
{

  long  ret_type = old->elemcod;
  int   this_read, size;

  char buffer[8192];

  size = (int)old->size * old->nelems;
  while (size && (this_read = sds_read(fd,size,buffer)))
  {
    size -= this_read;

    switch((int)old->elemcod) 
    {
#ifdef  IEEEFP
      case SDS_IFLOAT:
        if(bs_flag == SDS_SWAPPED_BYTES) 
          rswabw(buffer,new->address,old->size*old->nelems);
        else 
          memcpy(new->address,buffer,old->size*old->nelems);
      break;

      case SDS_VFLOAT:
        ret_type = SDS_IFLOAT;
#endif
#ifdef  VAXFP
      case SDS_VFLOAT:
        if(bs_flag == SDS_SWAPPED_BYTES) 
          rswabw(buffer,new->address,old->size*old->nelems);
        else 
          memcpy(new->address,buffer,old->size*old->nelems);
      break;

      case SDS_IFLOAT:
        ret_type = SDS_VFLOAT;
#endif
        if(bs_flag != SDS_SWAPPED_BYTES) 
          rswabw(buffer,buffer,old->size*old->nelems);
        r_float(buffer,new->address,old->size*old->nelems);
      break;

#ifdef  IEEEFP
      case SDS_ICOMPLEX:
        if(bs_flag == SDS_SWAPPED_BYTES) 
          rswabw(buffer,new->address,old->size*old->nelems);
        else 
          memcpy(new->address,buffer,old->size*old->nelems);

        break;
      case SDS_VCOMPLEX:
        ret_type = SDS_ICOMPLEX;
#endif
#ifdef  VAXFP
      case SDS_VCOMPLEX:
        if(bs_flag == SDS_SWAPPED_BYTES) 
          rswabw(buffer,new->address,old->size*old->nelems);
        else 
          memcpy(new->address,buffer,old->size*old->nelems);
  
        break;
      case SDS_ICOMPLEX:
        ret_type = SDS_VCOMPLEX;
#endif
        if(bs_flag != SDS_SWAPPED_BYTES) 
          rswabw(buffer,buffer,old->size*old->nelems);
        r_float(buffer,new->address,old->size*old->nelems);

        break;
#ifdef  IEEEFP
      case SDS_IDOUBLE:
        if(bs_flag == SDS_SWAPPED_BYTES) 
          rswabd(buffer,new->address,old->size*old->nelems);
        else 
          memcpy(new->address,buffer,old->size*old->nelems);

        break;
      case SDS_DVDOUBLE:
      case SDS_GVDOUBLE:
        ret_type = SDS_IDOUBLE;
#endif
#ifdef  VAXFP
      case SDS_DVDOUBLE:
      case SDS_GVDOUBLE:
        if(bs_flag == SDS_SWAPPED_BYTES) 
          rswabd(buffer,new->address,old->size*old->nelems);
        else 
          memcpy(new->address,buffer,old->size*old->nelems);

        break;
      case SDS_IDOUBLE:
        ret_type = SDS_DVDOUBLE;
#endif
        if(bs_flag != SDS_SWAPPED_BYTES) 
          rswabd(buffer,new->address,old->size*old->nelems);

        r_double(buffer,new->address,old->size*old->nelems);

    break;

#ifdef  IEEEFP
      case SDS_IDOUBLE_COMPLEX:
        if(bs_flag == SDS_SWAPPED_BYTES) 
          rswabd(buffer,new->address,old->size*old->nelems);
        else 
          memcpy(new->address,buffer,old->size*old->nelems);

        break;
      case SDS_DVDOUBLE_COMPLEX:
      case SDS_GVDOUBLE_COMPLEX:
        ret_type = SDS_IDOUBLE;
#endif
#ifdef  VAXFP
      case SDS_DVDOUBLE_COMPLEX:
      case SDS_GVDOUBLE_COMPLEX:
        if(bs_flag == SDS_SWAPPED_BYTES) 
          rswabd(buffer,new->address,old->size*old->nelems);
        else 
          memcpy(new->address,buffer,old->size*old->nelems);
  
        break;
      case SDS_IDOUBLE_COMPLEX:
        ret_type = SDS_DVDOUBLE;
#endif
        if(bs_flag != SDS_SWAPPED_BYTES) 
          rswabd(buffer,buffer,old->size*old->nelems);
  
        r_double(buffer,new->address,old->size*old->nelems);
      break;
      case SDS_DOUBLE_LONG:
      if (!SDS_BIGADDR && !sds_query_pack_b())
      {
        if (sds_query_float_b())
        {
          long l;
          if (bs_flag == SDS_SWAPPED_BYTES)
            rswabd(buffer,buffer,old->size*old->nelems);
          for (l=0;l<old->nelems;l++)
           *(double *)(new->address + l * 8) = 
                  (double)*(int *)(buffer + 4 + l * 8) +
                  (double)*(int *)(buffer + l *8) * pow(2.0,32.0); 
           ret_type = SDS_DOUBLE;
        }
        else if (sds_query_truncate_b())
        {
          long l;
          if (bs_flag == SDS_SWAPPED_BYTES)
            rswabd(buffer,buffer,old->size*old->nelems);
          for (l=0;l<old->nelems;l++)
          {
            memcpy((new->address + l * 4),(buffer + 4 + l * 8), 4);
            if (sds_query_trap_b() && *(int *)(buffer + l * 8))
              sds_bigint_trap(buffer + l * 8);
          }
           ret_type = SDS_LONG;
        }
        else
        {
          if(bs_flag == SDS_SWAPPED_BYTES)
            rswabd(buffer,new->address,old->size*old->nelems);
          else
            memcpy(new->address,buffer,old->size*old->nelems);
        }
      }
      else
      {
        if(bs_flag == SDS_SWAPPED_BYTES)
          rswabd(buffer,new->address,old->size*old->nelems);
        else
          memcpy(new->address,buffer,old->size*old->nelems);
      }
  
        break;
      case SDS_POINTER:
        printf("Problem in conversion: pointer found!\n");
      break;
      case SDS_PADB:
        printf("Problem n conversion: pad byte found!\n");
      break;
      default:
      if(bs_flag != SDS_SWAPPED_BYTES ||
            old->elemcod == SDS_STRING ||
            old->elemcod == SDS_FSTRING ||
            old->elemcod == SDS_BYTE ||
            old->elemcod == SDS_UNS_BYTE ||
            old->elemcod == SDS_CHAR_BITFIELD ||
            old->elemcod == SDS_LOGICAL_1) 
        memcpy(new->address,buffer,old->size*old->nelems);
      else 
      {
        if (new->size == 2)
          rswab(buffer,new->address,old->size*old->nelems);
        else if (new->size == 4)
          rswabw(buffer,new->address,old->size*old->nelems);
        else if (new->size == 8)
          rswabd(buffer,new->address,old->size*old->nelems);
        else
        {
          memcpy(new->address,buffer,old->size*old->nelems);
          printf("Problem n conversion: unknown type found!\n");
        }
      break;
      }
    }
  }
  if (size)
    sds_push_error(SDS_FILE_RD,SDS_ERROR,
           "Reading for conversion:not enough bytes!");
  return(ret_type);
}

/***********************************************************************/
void
rswab(buffer,bto,number)
int  number;
char  *buffer, *bto;
/***********************************************************************/
{
  int  i;
  struct  dshort  *to = (struct dshort *)bto;
  struct  dshort  *from = (struct dshort *)buffer;
  char  temp;


  for (i=0;i<number;i += 2,to++,from++) 
  {
    temp = from->one;
     to->one = from->two;
     to->two = temp;
  }
}

/***********************************************************************/
void
rswabw(buffer,bto,number)
int  number;
char  *buffer, *bto;
/***********************************************************************/
{
  int  i;
  struct  dlong  *from = (struct dlong *)buffer;
  struct  dlong  *to = (struct dlong *)bto;
  char  temp;

  for (i=0;i<number;i += 4,to++,from++) 
  {
    temp = from->four;
     to->four = from->one;
     to->one = temp;
    temp = from->two;
     to->two = from->three;
     to->three = temp;
   }
}

/***********************************************************************/
void
rswabd(buffer,bto,number)
int  number;
char *buffer, *bto;
/***********************************************************************/
{
  int  i;
  struct  ddouble  *from = (struct ddouble *)buffer;
  struct  ddouble  *to = (struct ddouble *)bto;
  char  temp;


  for (i=0;i<number;i += 8,to++,from++) 
  {
    temp = from->one;
     to->one = from->eight;
     to->eight = temp;
    temp = from->two;
     to->two = from->seven;
     to->seven = temp;
    temp = from->three;
     to->three = from->six;
     to->six = temp;
    temp = from->four;
     to->four = from->five;
     to->five = temp;
   }
}

/***********************************************************************/
void
sds_fix_header(header,orig_state)
struct  sds_header  *header;
int  orig_state;
/***********************************************************************/
{
  char  *cptr = (char *)header;

  if (orig_state == SDS_SWAPPED_BYTES) 
  {
    rswabw(cptr,cptr,4);
    cptr += 4;
    if (*(cptr + 6) > (char)2) /* New version coding - 2 bytes */
      rswab(cptr,cptr,4);
    else
      rswabw(cptr,cptr,4);
    cptr += 4;
    rswab(cptr,cptr,4);
  }
}

/***********************************************************************/
void
sds_fix_tlist(header,tlist,orig_state)
struct  sds_header  *header;
struct  type_list  *tlist;
int  orig_state;
/***********************************************************************/
{
  if (orig_state == SDS_SWAPPED_BYTES)
    rswabw((char *)tlist,(char *)tlist,(int)header->list_size);
}

/***********************************************************************/
void
sds_fix_direc(struct  direc  *dptr,int number,int orig_state,int size)
/***********************************************************************/
{
  int  i;
  char  *fromptr,*toptr,*start;
  int delta = size - sizeof(struct direc);
  int from_bigger = (delta > 0)?1:0;
  int testint = 1, goodbytes = 1;
  char *t = (char *)&testint;

  /* Find out if the I am big or little endian: goodbytes is true if
     I'm big endian.
   */
  if (*t != 0)
    goodbytes = 0;

  fromptr = (char *)dptr;
  start = toptr = delta?calloc(number,size):fromptr;

  for (i=0;i<number;i++) 
  {
    if (orig_state == SDS_SWAPPED_BYTES)
    {
      char *temp = fromptr;
      if (from_bigger)
      {
        rswabd(fromptr,fromptr,8);
        fromptr += 8;
      }
      else
      {
        rswabw(fromptr,fromptr,4);
        fromptr += 4;
      }
      rswabw(fromptr,fromptr,16);
      fromptr += 16;
      rswab(fromptr,fromptr,2);
      fromptr += 4;
      rswabw(fromptr,fromptr,4);
      fromptr = temp;
    }
    if (delta)
    {
      if (from_bigger)
      {
        memcpy(toptr, fromptr + goodbytes * 4, 4);
        memcpy(toptr + 4, fromptr + 8,sizeof(struct direc) - 4);
      }
      else
      {
        memcpy(toptr + goodbytes * 4, fromptr, 4);
        memcpy(toptr + 8, fromptr + 4,sizeof(struct direc) - 8);
      }
    }
    toptr += sizeof(struct direc);
    fromptr += size;
  }
  if (delta)
  {
    memcpy((char *)dptr,start,number * sizeof(struct direc));
    free(start);
  }
}

/*********************************************************************/
sds_handle
sds_cload_direc(sds_handle sds, int fd,sds_handle *state ,struct sds_header *header)
/*********************************************************************/
{
  struct direc *dtp;
  char  *cptr;
  struct  type_list  *tl;
  struct sds_header *nhead;
  struct  direc  *dptr;
  int  ndirecs,lhsize;
  char    old_rbyte;
  char    *temp_malloc;
  int arc;
  struct sds_control_p *scp;
  int incoming_direc_size = sizeof(struct direc);
  long l, total_direc_delta;
  long MoreInFile = 0, MoreInMemory = 0;

  if (fd < 0)
  {
    sds_push_error(SDS_FILE_NOP,SDS_ERROR, "File for conversion not open");
    return 0L;
  }

/*  Read in the header ;     */
  if  (sds_read(fd,BASE_OFFSET,(char *)header) != BASE_OFFSET)
  {
    sds_push_error(SDS_FILE_RD,SDS_ERROR, "Reading for conversion failed");
    return 0L;
  }

  if (sds_header_ok(header))
  {
    sds_push_error(SDS_GOOD_FORMAT,SDS_WARNING, "Conversion not necessary");
    return 0;
  }
  *state = sds_last_warning();

  if ((header->magic_number  & 0xff00ffff) == SDS_MAGIC_BYTESWAP)
    *state = SDS_SWAPPED_BYTES;

  sds_fix_header(header,*state);

/* Find out what sort of architecture : save this in the control block
   later */
  arc = (int)((header->magic_number & 0x0000ff00) >> 8) - 1;
  if (arc > NARCS)
  {
    sds_push_error(SDS_FILE_RD,SDS_ERROR,
                    "Unknown architecture received for conversion");
    return 0L;
  }

  lhsize = (int)header->heap_size + (int)header->list_size + 
            align_delta((sds_off_t)header->heap_size,4);
  lhsize += align_delta((sds_off_t)lhsize, sds_palign(SDS_DIRECTORY_STRUCTURE));

/* If I'm not 8-byte addressing, but incoming data is, the direc stucture
   is bigger incoming than here, and vice versa. This is only true
   with version 3 and up...before that, I had a different version coding
   and had not implemented 8-byte addresses
 */
  if (!SDS_BIGADDR &&
      header->version > (short)2 &&
      header->controlbits & SDS_FROM_BIGADDR)
  {
    incoming_direc_size += 4;
    MoreInFile = align_delta((sds_off_t)lhsize + BASE_OFFSET,8);
  }
  if (SDS_BIGADDR && 
      ((header->version <= (short)2) || !(header->controlbits & SDS_FROM_BIGADDR)))
  {
    incoming_direc_size -= 4;
    MoreInMemory = align_delta((sds_off_t)lhsize + BASE_OFFSET,8);
  }

  temp_malloc = sds_malloc((unsigned)lhsize+ MoreInFile);
  old_rbyte = sds_arc_rbyte(arc);

  tl = (struct  type_list *)temp_malloc;
  if (sds_read(fd,lhsize + MoreInFile,temp_malloc) != 
                     lhsize + MoreInFile)
  {
    sds_push_error(SDS_FILE_RD,SDS_ERROR, "Unexpected end-of-dataset");
    sds_discard(sds);
    return 0;
  }

  sds_fix_tlist(header,tl,*state);


/*  Find size of directory        */
  dtp = (struct direc *)sds_malloc(incoming_direc_size);
  if (sds_read(fd,incoming_direc_size,(char *)dtp) 
                                 != incoming_direc_size)
  {
    sds_push_error(SDS_FILE_RD,SDS_ERROR, "Unexpected end-of-dataset");
    sds_discard(sds);
    return 0;
  }
  sds_fix_direc(dtp,1,*state,incoming_direc_size);
  ndirecs = (int)dtp->nelems;

/* Just in case I'm loading from a different directory structure size */
  dtp->align_type = sds_palign(SDS_DIRECTORY_STRUCTURE);
  dtp->elemsz = sds_psize(SDS_DIRECTORY_STRUCTURE);

/*  New directory,tlist heap  and header go here    */
  cptr = sds_malloc((unsigned int)(ndirecs*sizeof(struct direc)) +
      (unsigned int)BASE_OFFSET +  
      (unsigned int)lhsize );

/*  Copy in header.....        */
  memcpy(cptr,(char *)header,BASE_OFFSET);
  nhead = (struct sds_header *)cptr;

  cptr += BASE_OFFSET;

/*  Copy in tlist and heap        */
  memcpy(cptr,tl,lhsize);
  free(temp_malloc);

  cptr += lhsize + MoreInMemory;

/*  Copy in top directory.....      */
  memcpy(cptr,(char *)dtp,sizeof(struct direc));
        free((char *)dtp);
  dptr = (struct direc *)cptr;

  cptr += sizeof(struct direc);
  temp_malloc = sds_malloc((ndirecs-1) * incoming_direc_size);

/*  Read in rest of directory.....      */
  if (sds_read(fd,incoming_direc_size*(ndirecs-1),temp_malloc) != 
                               incoming_direc_size*(ndirecs-1))
  {
    sds_push_error(SDS_FILE_RD,SDS_ERROR, "Unexpected end-of-dataset");
    sds_discard(sds);
    return 0;
  }
  sds_fix_direc((struct direc *)temp_malloc,ndirecs-1,*state,incoming_direc_size);

  memcpy(cptr,temp_malloc,sizeof(struct direc)*(ndirecs-1)); 
  free(temp_malloc);

/* Now, I've resized the direc structures to be locally OK - if there are any
   offst values greater than 32 bits something is drastically cock-eyed. However
   the internal offsets of other direcs, are wrong
 */
  dptr[0].offst = (sds_off_t )dptr - (sds_off_t)nhead;
  total_direc_delta = (incoming_direc_size - sizeof(struct direc)) * dptr[0].nelems;
  for (l=1;l<dptr[0].nelems;l++)
    dptr[l].offst -= total_direc_delta;
  scp = sds_control(sds);
  set_sys_vars(sds,dptr);
  scp->allofl |= SDS_HEAD_ALLOC;
  scp->genarc = arc;

  return(sds);
}


/*********************************************************************/
sds_handle
sds_lofd(sds_handle sds,int fd, unsigned long nbytes, struct sds_header *header )
/*********************************************************************/
/* Here I have loaded the header - but I cannot assume a seekable device;
   so I will read in what remains from the fd (nbytes is the COMPLETE
   size, including the header) and copy the header info.
   The name of the game is minimise your system calls.
   If size is 0, however, it mean I have to work out the size.
 */
{
  char *sdptr, *sdp;             /* Points to beginning of allocated memory */
  struct sds_header *sdsh;       /* ...so does this */
  struct direc *dptr;
  sds_handle obj;
  struct sds_control_p *scp;
  long already_read,del;
  long remains;
  sds_off_t lhsize;

  scp = sds_control(sds);

  lhsize = BASE_OFFSET + 
           (sds_off_t)header->list_size + (sds_off_t)header->heap_size;
  lhsize += align_delta(lhsize,sds_palign(SDS_DIRECTORY_STRUCTURE));

  if (!nbytes)
  {
    remains = lhsize + sizeof(struct direc) - BASE_OFFSET;
    sdptr = sds_malloc(lhsize + sizeof(struct direc));
  }
  else
  {
    remains = nbytes - BASE_OFFSET;
    sdptr = sds_malloc(nbytes);
  }

  sdp = sdptr + BASE_OFFSET;

  if (sds_read(fd,remains,sdp) != remains)
  {
     free(sdptr);
     return 0L;
  }
  memcpy(sdptr, (char *)header, BASE_OFFSET);
  sdsh = (struct sds_header*)sdptr;
  dptr = (struct direc *)(sdptr + lhsize);

  if (!nbytes)
  {
    already_read = lhsize + sizeof(struct direc);
    remains = (dptr[0].nelems-1) * sizeof(struct direc);
    sdptr = sds_realloc(sdptr,already_read + remains);
    dptr = (struct direc *)(sdptr + lhsize);
    sdp = sdptr + already_read;

    /* Do this now to get trailing pads, if they exist, in del */
    already_read += ((dptr[0].nelems-1) * sizeof(struct direc));
    del = align_delta((sds_off_t)already_read, sds_palign(dptr[1].align_type));
    remains += del;
    already_read += del;

    if (sds_read(fd,remains,sdp) != remains)
    {
       free(sdptr);
       return 0L;
    }
    nbytes = sds_sz(dptr);
    sdptr = sds_realloc(sdptr,nbytes);
    dptr = (struct direc *)(sdptr + lhsize);
    sdp = sdptr + already_read;
    sds_read(fd,nbytes - already_read,sdp);
  }

  set_sys_vars(sds,dptr);
  scp->source = SDS_FILE;
  for (obj = 0;obj < dptr[0].nelems; obj++)
  /*
  This is gross. Must get illoca sorted properly.
  Just 0R'ing it takes out DISJOINT_OBJETC
    dptr[obj].illoca = 0;
    */
    dptr[obj].illoca &=
                   ~(SDS_WAS_ALLOCATED | SDS_EXTERNAL_OBJECT | SDS_REALLOC);

  return(sds);
}
