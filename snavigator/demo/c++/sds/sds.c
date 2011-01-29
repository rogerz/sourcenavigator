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


/*s**********************************************************************
 *                                                                      *
 *              Copyright (C)                                           *
 *     Epsilon Eridani 3 Megasoft Corporation, 1986                     *
 *                                                                      *
 *      This library contains Proprietary Information of Frogsoft       *
 *      Corporation and should not be treated as Confidential.          *
 *                                                                      *
 *      Authors: WFH, CGS, Verklim Fartislart                           *
 *                                                                      */
/*e**********************************************************************/

/* Reference release  Aug 10 1991 - C G Saltmarsh */
/* Has the basics used at CDG & SSC 1988-1991, plus vxworks
   support
*/


#include <stdlib.h>
#include <string.h>
#include <fcntl.h>

#ifndef vms
#include <unistd.h>
#endif

#ifndef vms
#include "Sds/sdsgen.h"
#include "Sds/sds_externs.h"
#else
#include "sdsgen.h"
#include "sds_externs.h"
#endif

extern  char      *sds_align;

/***** forward declarations ******/
#if defined(__STDC__)
void pindent(int);
int  sds_praw_directory(sds_handle);
void sds_printohead(sds_handle,sds_handle, struct sds_odesc *,int);
#else
void pindent();
int  sds_praw_directory();
void sds_printohead();
#endif
    
char      *ctime();

/***********************************************************************/
void
nexpand(fullname,name)
/*s**********************************************************************
 *                                                                      *
 *      Expands input name to full pathname for standard sd directory   *
 *      if first character = '@', will take name literally              *
 *                                                                      *
 *              Author: CGS, WFH                                        *
 *                                                                      */
/*e**********************************************************************/
char    *fullname;
char    *name;
/***********************************************************************/
{
  char *temp;
  char *getenv();
#ifdef VXWORKS
  char *user;
#endif
     
  if (*name == '@' ) 
  {
    name++;
    strcpy(fullname,name);
  }
  else 
  {
#ifdef VXWORKS
    remCurIdGet(user, NULL);
    strcpy(fullname,"/home/");
    strcat(fullname,user);
#else
    temp = getenv("HOME");
    strcpy(fullname,temp);
#endif
    strcat(fullname,"/shardat/");
    strcat(fullname,name);
  }  
}
     
/***********************************************************************/
sds_handle
sds_obind2code(sds, object_index)
sds_handle object_index;
sds_handle  sds;
/***********************************************************************/
{
  struct direc    *dptr = sds_direc(sds);
  if (dptr == DNULL) 
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Getting object code");
    return 0L;
  }
  if (object_index < 1 || object_index > dptr[0].nelems)
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Getting object code");
    return 0L;
  }
  return(dptr[object_index].elemcod);
}
/***********************************************************************/
void    *
sds_obind2ptr(sds,object)
/*s**********************************************************************
 *                                                                      
 *      1. Return pointer to object "ielem" in  data area         
 *         with sds handle sds
 *      2. Return 0 if it is out-of-bounds or no entry ...              
 *                                                                      
 *      Author: CGS                                                     
 *                                                                      */
/*e**********************************************************************/
sds_code object;
sds_handle  sds;
/***********************************************************************/
{
  struct direc    *dptr = sds_direc(sds);

  if (dptr == DNULL) 
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Getting object pointer");
    return (void *)0;
  }
  return(sds_getp(dptr,object));
}

/***********************************************************************/
void    *
sds_getp(dptr,ielem)
sds_handle    ielem;
struct  direc*  dptr;
/***********************************************************************/
{

  unsigned long    jelem;
  char    *cptr;

  if (ielem == (unsigned long)0) 
    return((void *)dptr);

  jelem = dptr[0].nelems;
  if ( ielem > jelem ) 
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Getting object pointer");
    return (void *)0;
  }
  if (dptr[ielem].nelems == 0 || dptr[ielem].offst == SDS_IMPOSSIBLE_ADDRESS)
    cptr = NULL;
  else if (dptr[0].offst == SDS_NOT_ASSEMBLED)
    cptr = (char *)dptr[ielem].offst;
 else
    cptr = (sds_addr)dptr + (long)dptr[ielem].offst - (long)dptr[0].offst;
  return((void*) cptr);
}
/***********************************************************************/
sds_handle
sds_get_checked(sds,object_name,type_code)
sds_handle sds;
char *object_name;
sds_code type_code;
/***********************************************************************/
{
  sds_handle ind;
  struct direc    *dptr = sds_direc(sds);

  if (dptr == DNULL) 
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Checking object");
    return 0L;
  }

  if (!(ind = sds_name2ind(sds,object_name)))
    return 0L;

  if (dptr[ind].elemcod != type_code)
  {
    sds_push_error(SDS_WRONG_TYPE,SDS_ERROR,"Checking object");
    return 0L;
  }
  return(ind);
}
/***********************************************************************/
sds_handle 
sds_name2ind(sds,name)
sds_handle  sds;
char  *name;
/***********************************************************************/
{
  sds_handle nobj;
  sds_code j;
  struct direc    *dptr = sds_direc(sds);

  if (dptr == DNULL) 
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Getting object index");
    return 0L;
  }

  nobj = dptr[0].nelems;
  for (j=1;j<nobj;j++) 
    if (!strcmp(sds_obind2name(sds,j),name))
      return(j);

  sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Getting object index");
  return 0L;
}
/***********************************************************************/
sds_handle 
sds_like2ind(sds,name,start)
sds_handle  sds,start;
char  *name;
/***********************************************************************/
{
  sds_handle nobj;
  sds_code j;
  struct direc    *dptr = sds_direc(sds);

  if (dptr == DNULL) 
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Getting object index");
    return 0L;
  }

  nobj = dptr[0].nelems;
  for (j=start;j<nobj;j++) 
    if (strstr(sds_obind2name(sds,j),name) != 0)
      return(j);

  sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Getting object index");
  return 0L;
}
/***********************************************************************/
sds_handle 
sds_array_size(sds,object)
sds_handle sds;
sds_handle object;
/***********************************************************************/
{

  struct direc    *dptr = sds_direc(sds);

  if (dptr == DNULL) 
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Getting array size");
    return SDS_NO_SUCH_SDS;
  }

  if (object < dptr[0].nelems && object >= 0)
    return(dptr[object].nelems); 
  else
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Getting array size");
    return SDS_NO_SUCH_OBJ;
  }
}
/***********************************************************************/
void  *
sds_obname2ptr(sds,name)
sds_handle  sds;
char  *name;
/***********************************************************************/
{

  sds_handle object_number;
  struct direc    *dptr = sds_direc(sds);

  if (dptr == DNULL) 
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Getting object pointer");
    return (void *)0;
  }

  if ((object_number = sds_name2ind(sds,name))) 
    return(sds_getp(dptr,object_number)); 
  else
    return (void *)0; 
}

/*s************************************************** 
* 
*       Stick timestamp on object obj within directory 
* 
*       The directory timestamp - dptr[0].wtime - 
*       holds Unix time (seconds since Jan 1, 1970)
*       while individual objects hold delta time 
*       since directory time (in millisecinds? - to 
*       be determined. As of now, no time info is there.) 
*****************************************************/
/***********************************************************************/
sds_handle
sds_tstamp(sds,obj) 
sds_handle    sds; 
sds_handle obj;
/***********************************************************************/
{
  struct direc *dptr; 
  if ((dptr = sds_direc(sds)) == DNULL)
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Timestamping");
    return 0L;
  }
  if ((obj == SDS_TIMESTAMP_ALL) || (obj == (unsigned long)0)) 
  {
    dptr[0].wtime = (unsigned long)time(0); 
    if (obj != (unsigned long)0) 
    {
      unsigned long     i; 
      for (i=1;i<dptr[0].nelems;i++)
        dptr[i].wtime = i;
    } 
  } 
  else 
  {
    dptr[obj].wtime = obj; 
  }
  return 1L; 
}
/***********************************************************************/
long *
sds_get_tstamp(sds,obj)
sds_handle sds;
sds_handle obj;
/***********************************************************************/
{
  struct direc *dptr; 
  if ((dptr = sds_direc(sds)) == DNULL)
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Getting timestamp");
    return (long *)0;
  }
  if (obj < 0 || obj > dptr[0].nelems)
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Getting timestamp");
    return (long *)0;
  }
  return (long *)&dptr[obj].wtime;
}


/***********************************************************************/
char  *
load_sds_file(sds_handle sds,int fd,unsigned long nbytes)
/***********************************************************************/
{
  char    *sdptr;

  sdptr = sds_malloc((unsigned int)nbytes);

	if (sds_read(fd,(int)nbytes,sdptr) != nbytes)
  {
    free(sdptr);
    sds_push_error(SDS_FILE_RD,SDS_ERROR,"Loading from file desc");
    return (char *)0;
  }
  else 
    return sdptr;
}

/*********************************************************************/
sds_handle
sds_header_ok(sdsh)
struct sds_header *sdsh;
/*********************************************************************/
{
  if (sdsh->magic_number != SDS_MAGIC) 
  {
    if ((sdsh->magic_number & 0xffff00ff) == SDS_BASE_MAGIC)
    {
      sds_push_error(SDS_WRONG_PADS,SDS_WARNING,"Check SDS type");
      if ((SDS_ARC == SDS_SPARC) && (sdsh->controlbits & SDS_IS_RISCY))
				return 1;
      return 0L;
    }
    else
    {
      if ((sdsh->magic_number  & 0xff00ffffL) == SDS_MAGIC_BYTESWAP)
        sds_push_error(SDS_SWAPPED_BYTES,SDS_WARNING,"Check SDS type");
      else
        sds_push_error(SDS_NOT_SDS,SDS_ERROR,"Check SDS type");
    return 0L;
    }
  }
  if (sdsh->version > SDS_VERSION ) 
  {
    sds_push_error(SDS_BAD_VERSION,SDS_WARNING,"Check SDS type");
    return 0L;
  }
  return 1L; 
}
/*********************************************************************/
unsigned long
tlist_size(tlist)
struct type_list *tlist;
/*********************************************************************/
{
  
  unsigned long list_size = (unsigned long)0;
  if (tlist == TNULL) return(list_size);
  while((tlist++)->elemcod != SDS_ENDLIST) 
    list_size++;
  list_size++;
  return list_size*(unsigned long)sizeof(struct type_list);
}
/*********************************************************************/
struct direc*
header_to_direc(sds_header)
struct sds_header* sds_header;
/*********************************************************************/
{
  char *cptr = (char *)sds_header;
  return((struct direc *)(cptr + (int)sds_header->list_size + 
    (int)sds_header->heap_size + BASE_OFFSET));
}
/***********************************************************************/
int
sds_praw_directory(sds)
sds_handle  sds;
/***********************************************************************/
{
  struct  sds_odesc  *thing;
  int  indent,ts = -1,hs;
  struct direc *dptr = sds_direc(sds);
  unsigned long  i,nobj = dptr[0].nelems;
  char  *cptr,*type_ptr;
  struct  type_list  *tptr;


  hs = (int)sds_heap_size(sds);
  if (sds_head(sds) != NULL) 
  {
    ts = (int)(sds_head(sds)->list_size);
    printf("\
      magic 0x%lx\n\
      version 0x%lx\n\
      tlist size %d bytes\n\
      heap size %d bytes\n",
      (unsigned long)sds_head(sds)->magic_number,
      (unsigned long)sds_head(sds)->version,
      ts,
      hs);
  }
  else
    printf("Not assembled: no header\n");
  tptr = sds_tlist(sds);
  if ((tptr != TNULL) && (ts != 0))
  {
    int i = 0;
    while (tptr->elemcod != SDS_ENDLIST) 
    {
      printf("%x: %x code %x\n",
                   (unsigned)i,
                   (unsigned)tptr->nelems,
                   (unsigned)tptr->elemcod);
      tptr++;
      i++;
    }
  }
  else
    printf("NULL tlist\n");
  printf("***********************\n");
  cptr = sds_heap(sds);
  if (cptr != NULL)
    sds_printit(SDS_BYTE,(long)hs,cptr,0,0);
  else
    printf("NULL heap\n");
  printf("***********************\n");
  for (i=0;i<nobj;i++) 
  {
    printf("****************\n\
      name offset 0x%lx\n\
      data offset 0x%lx \n\
      timestamp 0x%lx \n\
      code 0x%lx \n\
      number of elements %ld \n\
      size of element %ld \n\
      reallocation flag %lx \n\
      structure type %lx \n\
      align %d\n",
        (unsigned long)dptr[i].obj_name,
        (unsigned long)dptr[i].offst,
        (unsigned long)dptr[i].wtime,
        (unsigned long)dptr[i].elemcod,
        (long)dptr[i].nelems,
        (long)dptr[i].elemsz,
        (unsigned long)dptr[i].illoca & 0xff,
        (unsigned long)dptr[i].structype,
        (int)dptr[i].align_type & 0xff);
    if (SDS_INDLIST & dptr[i].elemcod) 
    {
      sds_cleanup(sds);
      while ((indent = sds_describe(sds,i,&thing)) >= 0) 
      {
        if (thing[indent].elemcod & SDS_INDLIST)
          type_ptr = sds_typename(0);
        else
          type_ptr = sds_typename(thing[indent].elemcod);
        pindent(indent);
        printf("%s %ld (%lx %lx %lx)\n",
          thing[indent].name,
          (long)thing[indent].nelems,  
          (unsigned long)thing[indent].address,  
          (unsigned long)thing[indent].size,
          (unsigned long)(thing[indent].align & 0xff));
      }
    }
  }
  return(1);
}

/***********************************************************************/
sds_handle 
sds_list(sds,object,flag)
sds_handle sds;
sds_code   object,flag;
/***********************************************************************/
{

  int                 indent;
  struct  sds_odesc  *thing = 0;
  static  int         depth = -1;
  sds_code            i,nobj;
  char                inbuf[80],opbuff[128];
  char               *cptr;
  struct direc       *dptr = sds_direc(sds);

  depth++;
  *inbuf = (char)0;
  for (i=0;i<depth;i++) strcat(inbuf,"   ");

  if (object >= dptr[0].nelems) 
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_WARNING,"Listing");
    return 0L;
  }

  if (object == (unsigned long)0) 
  {
  /*  Print out the directory description  */
    if (flag & SDS_LIST_RAW) 
    {
      sds_praw_directory(sds);
      return 1L;
    }

    nobj = dptr[0].nelems;
    sprintf(opbuff," dataset '%s' %ld user objects created %s",
      sds_obind2name(sds,(sds_code)0),
      dptr[0].nelems-(unsigned long)1,  
      ctime((time_t *)&dptr[0].wtime));
    printf("%s",inbuf);
    for (i=0;i<(int)strlen(opbuff);i++) printf("-");
    printf("\n");
    printf("%s%s%s",inbuf,opbuff,inbuf);
    printf("- Sds version %3f ",sds_version(sds));
    for (i=0;i<(int)strlen(opbuff)- 19;i++) printf("-");
    printf("\n");

    for (i=1;i<nobj;i++) 
    {
    if ((SDS_INDLIST & dptr[i].elemcod) || (dptr[i].elemcod < NTYPES)) 
      sds_printohead(sds,i,thing,depth);
    else 
      if (dptr[i].elemcod == SDS_SDS) 
        cptr = (char*) sds_getp(dptr,i);
    else 
      printf("%s%ld :'%s' %ld structure(s) \n",
        inbuf, i,
        sds_obind2name(sds,i),
        dptr[i].nelems);
    }
  }
  else 
  {
  /*  Print the data object    */
  cptr = (char*) sds_getp(dptr,object);
  if (cptr == NULL || flag & SDS_HEADER_ONLY)
  {
    if (cptr == NULL && !(flag & SDS_HEADER_ONLY))
      printf("Object template only - no data\n");
    sds_printohead(sds,object,thing,depth);
  }
  else if (flag & SDS_LIST_RAW) 
  {
    sds_printit(SDS_BYTE,dptr[object].nelems*dptr[object].elemsz,cptr,0,0);
  }
    
  else 
    {
      sds_cleanup(sds);
      while ((indent = 
        sds_resolve(sds,object,&thing,SDS_OBJECT)) >= 0) 
        {
        if (flag & SDS_LIST_HEADERS)
          printf("%s %ld %s\n",
            thing[indent].name,
            thing[indent].nelems,
            sds_typename((int)thing[indent].elemcod));
        sds_printit(thing[indent].elemcod,
          thing[indent].nelems,
          thing[indent].address,
					thing[indent].nbits,
					thing[indent].startbit);
      }
    }
  }
  depth--;
  return 1L;
}
void
sds_printohead(sds,object_index,thing, depth)
sds_handle          sds,object_index;
struct  sds_odesc  *thing;
int                 depth;
{
  int    indent,ik,nelems;
  char  *nptr,*type_ptr;

  sds_cleanup(sds);
  while ((indent = sds_describe(sds,object_index,&thing)) >= 0) 
  {
    nelems = thing[indent].nelems;
    if (thing[indent].elemcod & SDS_INDLIST)
      type_ptr = sds_typename(0);
    else
      type_ptr = sds_typename((int)thing[indent].elemcod);
    pindent(indent+depth);
    nptr = thing[indent].name;
    printf("%s %s", nptr, type_ptr);
    if ((thing[indent].nbits))
      printf("(%d):%d\n",thing[indent].nbits,thing[indent].startbit);
    else if (nelems != (unsigned long)1)
    {
      printf("[%d]\n",nelems);  
      if (thing[indent].nnames == nelems) 
      {
        for (ik=1;ik<thing[indent].nnames;ik++) 
        {
          nptr += (int)strlen(nptr) +1;
          pindent(indent+depth);
          printf("%s\n",nptr);
        }
      }
    }
    else
      printf("\n");
  }
}
EXTERN void       sds_printit(sds_code ,sds_code ,char *,char,char);
/***********************************************************************/
void
sds_printit(sds_code type_code,sds_code total_size,
								char *object_pointer,char fb,char bs)
/***********************************************************************/
{

  sds_code    i,k;
  int         kmax,cline;
  char        *bptr;
  long        *iiptr;
  sds_handle  *liptr;
  float       *fptr;
  double      *ffptr;
  short       *sptr;

  switch((int)type_code) 
  {
    case  SDS_WORD :
    case  SDS_UNS_WORD :
        cline = 16;
        sptr = (short *)object_pointer;
        for (i = (unsigned long)0;i<total_size;i+=(unsigned long)cline)
        {
          kmax = (total_size-i)>cline?cline:(int)(total_size-i);
          for (k=0;k<kmax;k++) 
            printf("%d ",(int)*(sptr+k));
          sptr+=kmax;
          printf("\n");
        }
        break;
    case  SDS_POINTER:
    case    SDS_LONG  :
    case    SDS_UNS_LONG  :
        cline = 16;
        liptr = (sds_handle *)object_pointer;
        for (i = 0;i<total_size;i+=cline)
        {
          kmax = (total_size-i)>cline?cline:(total_size-i);
          for (k=0;k<kmax;k++) 
            printf("%ld ",*(liptr+k));
          liptr+=kmax;
          printf("\n");
        }
        break;
    case    SDS_DOUBLE_LONG  :
        cline = 16;
        iiptr = (long *)object_pointer;
        for (i = 0;i<total_size;i+=cline)
        {
          kmax = (total_size-i)>cline?cline:(total_size-i);
          for (k=0;k<kmax;k++) 
#if (BIGADDR == 1)
            printf("%ld ",*(iiptr+k));
          iiptr+=kmax;
#else
            printf("[%ld]%ld ",*(iiptr+2*k),*(iiptr+2*k+1));
          iiptr+=2*kmax;
#endif
          printf("\n");
        }
        break;
    case    SDS_TIME  :
        iiptr = (long *)object_pointer;
        for (i = 0;i<total_size;i++)
        {
          printf("%x :",(unsigned)*iiptr++);
          printf(" %x",(unsigned)*iiptr++);
          printf("\n");
        }
        break;
    case    SDS_FLOAT :
        cline = 8;
        fptr = (float *)object_pointer;
        for (i = 0;i<total_size;i+=cline)
        {
          kmax = (total_size-i)>cline?cline:(total_size-i);
          for (k=0;k<kmax;k++) 
            printf("%e ",*(fptr+k));
          fptr+=kmax;
          printf("\n");
        }
        break;
    case    SDS_DOUBLE:
        cline = 8;
        ffptr = (double *)object_pointer;
        for (i = 0;i<total_size;i+=cline)
        {
          kmax = (total_size-i)>cline?cline:(total_size-i);
          for (k=0;k<kmax;k++) 
            printf("%e ",*(ffptr+k));
          ffptr+=kmax;
          printf("\n");
        }
        break;
    case   SDS_PADB  :
        printf("%ld pad bytes\n",(long)total_size);
        break;
    case  SDS_BYTE  :
        cline = 16;
        bptr = (char *)object_pointer;
        for (i = 0;i<total_size;i+=cline)
        {
          kmax = (total_size-i)>cline?cline:(total_size-i);
          printf("\n%5x| ",(unsigned int)i);
          for (k=0;k<kmax;k++) 
            printf("%2x ",((unsigned int)*(bptr+k) & 0xff));
          for (k=0;k<cline-kmax;k++)
            printf("   ");
          printf("|");
          for (k=0;k<kmax;k++) 
            if (isprint((int)*(bptr+k))) 
                printf("%c",*(bptr+k));
            else printf(".");
          bptr+=kmax;
        }
        printf("\n");
        break;
    case  SDS_STRING  :
    case  SDS_FSTRING  :
        kmax = total_size;
        for (k=0;k<kmax;k++) 
          if (isprint((int)*(object_pointer+k))) 
              printf("%c",*(object_pointer+k));
          else printf(".");
        printf("\n");
        break;
    case  SDS_SDS:
        break;
    case  SDS_LONG_BITFIELD  :
        liptr = (sds_handle *)object_pointer;
        printf("0x%lx\n", ((*liptr >> bs) & ((1 << fb) - 1)));
        break;
    case  SDS_SHORT_BITFIELD  :
        sptr = (short *)object_pointer;
        printf("0x%x\n", ((*sptr >> bs) & ((1 << fb) - 1)));
        break;
    case  SDS_CHAR_BITFIELD  :
        bptr = (char *)object_pointer;
        printf("0x%x\n", ((*bptr >> bs) & ((1 << fb) - 1)));
        break;
    case    SDS_BITFIELD  :
        break;
    default       :
      printf("element type %lx unprintable\n",type_code);
      break;
  }
}
/***********************************************************************/
sds_code
align_delta(sds_off_t address,char alignment_size)
/***********************************************************************/
{
  long  i,j;
  long ad = address;
  
  if (!alignment_size)
    return(0);
  j = (long)(alignment_size & 0xff);
  i=ad%j;
  i = (i == 0)?0:j-i;
  return (sds_code)i;
}
/***********************************************************************/
void
pindent(n)
int  n;
/***********************************************************************/
{ int  i; for (i=0;i<n;i++) printf("   "); }

/***********************************************************************/
sds_handle
sds_set_object_type(sds, object,structure_flag)
sds_handle sds,object;
int structure_flag;
/***********************************************************************/
{
  struct direc   *dptr = sds_direc(sds);
  if (dptr == NULL)
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Set object type");
    return 0L;
  }
  if ((object < 0) || (object >= dptr[0].nelems))
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Set object type");
    return 0L;
  }
  dptr[(int)object].structype = structure_flag;
  return sds;
}

/***********************************************************************/
sds_handle 
sds_get_object_type(sds,object)
sds_handle sds,object;
/***********************************************************************/
{
  struct direc   *dptr = sds_direc(sds);
  if (dptr == DNULL)
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Get object type");
    return 0L;
  }
  if ((object < 0) ||
      (object >= dptr[0].nelems))
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Get object type");
    return 0L;
  }
  return dptr[(int)object].structype;
}

/***********************************************************************/
sds_handle 
sds_set_object_location(sds, object,location_flag)
sds_handle sds,object;
int location_flag;
/***********************************************************************/
{
  struct direc   *dptr = sds_direc(sds);
  if (dptr == DNULL)
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Set object location");
    return 0L;
  }
  if ((object < 1) || (object >= dptr[0].nelems))
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Set object location");
    return 0L;
  }
  dptr[(int)object].illoca = location_flag;
  return 1L;
}

/***********************************************************************/
sds_handle
sds_mark_disjoint(sds,object)
sds_handle sds;
sds_handle object;
/***********************************************************************/
{ return sds_set_object_location(sds,object,SDS_DISJOINT_OBJECT); }
