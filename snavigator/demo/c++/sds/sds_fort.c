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


/**** FORTRAN ACCESS CALLS *******************************************/
/*    All are functions returning a 4-byte integer : ie i*4
 *   basic rules for error returns are:
 *
 *  1 (ONE), the thing Bertrand Russell was worried about, is
 *    *** GOOD ***
 *  Even (bit 1 zero) is bad, usually fatal unless recoverable file
 *  name problem.
 *  
 *  SDS stands for 'standard dataset'.
 *
 *  It should be noted that all of this is conceptually MUCH
 *  CLEANER if a proper programming language such as C, Pascal,
 *  Modula whatever, is used.
 *  
 *  Most calls are simple interfaces to underlying C-routines.
 *  VMS-Fortran names are SDS_F**** where the C name is sds_***.
 *  F77 names are a mess.
 *  
 *  STATUS RETURNS:
 *  
 *  1  Everthing is wonderful.
 *  2   SDS_NO_SUCH_SDS            No such SDS
 *  4   SDS_NO_SPC                 No space for SDS directory 
 *                                         (current limit is 16)
 *  6   SDS_FILE_OP                Cannot open file
 *  8   SDS_FILE_WR                Cannot write to file
 *  10  SDS_NO_SUCH_OBJ            No such object in SDS
 *  12  SDS_FILE_RD                Cannot read file
 *  14  SDS_NOT_SDS                File is not an SDS
 *  16  SDS_VERSION                File is old SDS version
 *  18  SDS_FILE_NOP               No file open
 *  20  SDS_SWAPPED_BYTES          Sds with bytes the wrong way round
 *  22  SDS_NOT_ASS                SDS not assembled
 *  24  SDS_NOT_INITIALISED        Guess
 *  26  SDS_UNDEFINED_TYPE         Object type is not defined.
 *  28  SDS_NOT_DEFINABLE          You can't redefine an dataset you
 *                                 haven't started yourself.
 *  30  SDS_DEJA_LA                The named dataset already exists.
 *  32  SDS_TRANSFER_UNDEF         Transfer type unknown (eg SDS_TAPEFILE
 *                                           is not known on this version).
 *  34  SDS_WRONG_TYPE             Element type not as requested.
 *  36  SDS_WRONG_PADS             Wrong padding type.
 *  38  SDS_NO_MEM                 Not enough memory.
 *  40  SDS_NO_DB_PROC             No db - process assigned.
 *  42  SDS_DB_ACCESS              Database access error.
 *  44  SDS_NOT_COMPLEX_OBJECT     Not a complex object.
 *  46  SDS_WRONG_RES_LIST         Mixed up resolution lists
 *  48  SDS_ZERO_LENGTH            Structure is defined,but no data allocated
 *  
 *********************************************************************/

#include <stdlib.h>
#include <string.h>

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

char  cstring[256];
extern int sds_error;

/********** forward declarations **************/

#ifndef vms

#if defined(__STDC__)

int  getc_from_f(char *,char *,int*);
int  getf_from_c(char *,char *,int*);
void unzterm(char *,int);
void zterm(char *);

#else /* not STDC */

int  getc_from_f();
int  getf_from_c();
void unzterm();
void zterm();

#endif /* not STDC */

#else /* in vms */
/*  Zap out the sybase access routines  */
int sds_dbrow_ins(){};
int sds_dbtab_make(){};
#endif

#ifdef vms
struct vms_fstring {
  short len;
  short code;
  char   *string;
  };
#endif

/*********************************************************************/
sds_handle
sds_read_object(sds,obj,pointer,start,max)
sds_handle  sds;
sds_code obj;
void  *pointer;
long  start;
unsigned long  max;
/*********************************************************************/
{
  unsigned long  i,copy_size,offset;
  unsigned long  nelem;
  struct  sds_header  *head;
  struct direc *dptr = sds_direc(sds);
  int  rbytes;
	int fd = sds_stream(sds);

  if (fd < 0)
  {
    sds_push_error(SDS_FILE_NOP,SDS_ERROR,"Object load");
    return 0L;
  }
  if (dptr == DNULL)
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Object load");
    return 0L;
  }
  if (obj >= dptr[0].nelems || start >= dptr[obj].nelems)
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Object load");
    return 0L;
  }

  head = sds_head(sds);
  offset = BASE_OFFSET + head->list_size + head->heap_size;
  for (i=0;i<obj;i++) 
  {
    offset += dptr[i].nelems*dptr[i].elemsz;
    offset += align_delta((int)offset,dptr[i].align_type);
  }
  offset += align_delta((int)offset,dptr[i].align_type);
  offset += start * dptr[obj].elemsz;

/* Don't try to copy more than max objects (else potential bang)   */
  if (max > (dptr[obj].nelems - start)) max = dptr[obj].nelems - start;
  dptr[obj].offst = (long)pointer;

/*  In bytes.....            */
  copy_size = max*dptr[obj].elemsz;

  if (lseek(fd,offset,L_SET) < 0) 
  {
    sds_push_error(SDS_FILE_OP,SDS_ERROR,"Lseek failure:");
    return 0L;
  }
  rbytes = read(fd, pointer , copy_size);
  if (rbytes != copy_size)
    nelem = SDS_FILE_RD;
  else
    nelem = rbytes/dptr[obj].elemsz;
  dptr[obj].offst = (long)pointer;

  return(nelem);
}
/*********************************************************************/
/*  SDS_FINIT
 *  initialise sds . MUST be called before using sds services
 *
 *  
 *********************************************************************/
#ifdef vms
/**************** VMS_FORTRAN ****************************************/
int
sds_finit()
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sdinit_()
#else
sds_finit_()
#endif
#endif
{
  sds_init();
  return(1);
}
/*********************************************************************/
/*  SDS_DEATH
 *
 *  Print system error and exit.
 *      SDS_DEATH("from boredom")
 *
 *  
 *********************************************************************/
#ifdef vms
/**************** VMS_FORTRAN ****************************************/
int
sds_death(name)
char *name;
{
  int  length;
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sddie_(name,length)
#else
sds_death_(name,length)
#endif
char *name;
int  length;
{
#endif
  getc_from_f(cstring,name,&length);  
  sds_perror(cstring);
  exit((char)sds_error);
  return 1;
}
/*********************************************************************/
/*  SDS_FSTART
 *  start a new SDS of name 'name' or returns sds index
 *  of existing sds.
 *
 *      ISTATUS = SDS_FSTART("fred",sds)
 *
 *  Builds a new sds directory called "fred": the
 *  program may now fill this SDS with calls to 
 *  SDS_DECLARE.
 *  
 *  
 *********************************************************************/
#ifdef vms
/**************** VMS_FORTRAN ****************************************/
int
sds_fstart(name,sds)
char *name;
int  *sds;
{
  int  length;
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sdstrt_(name,sds,length)
#else
sds_fstart_(name,sds,length)
#endif
char *name;
int  length,*sds;
{
#endif
  getc_from_f(cstring,name,&length);  
  *sds = sds_new(cstring);
  if (*sds == 0) 
  {
    if (sds_error == SDS_DEJA_LA)
      *sds = good_sds(cstring);
    return(-2*sds_error);
  }
  return 1;
}
#ifdef vms
/**************** VMS_FORTRAN ****************************************/
int
sds_perr(name)
char *name;
{
  int  length;
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sdperr_(name,length)
#else
sds_perr_(name, length)
#endif
char *name;
int length;
{
#endif
  getc_from_f(cstring,name,&length);  
  sds_perror(cstring);
  return 1;
}
/*********************************************************************/
/*  SDS_FTSTAMP
 *  Put a timestamp on a dataset (OBJ_IND = 0)
 *  on a single object (OBJ_IND = object number)
 *  or on dataset and all objects (OBJ_IND = SDS_TIMESTAMP_ALL)
 *
 *      ISTATUS = SDS_FTSTAMP(sds,OBJ_IND)
 *
 *  
 *********************************************************************/
#ifdef vms
/**************** VMS_FORTRAN ****************************************/
int
sds_ftstamp(sds,obj_ind)
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sdstmp_(sds,obj_ind)
#else
sds_ftstamp_(sds,obj_ind)
#endif
#endif
int  *sds,*obj_ind;
{ 
  int ret = sds_tstamp(*sds,*obj_ind);
  if (ret == 0) return(-2*sds_error);
  else return(1);
}
/*********************************************************************/
/*  SDS_FEND
 *  Delete directory of SDS sds
 *
 *      ISTATUS = SDS_FEND(4)
 *
 *  Deletes sds 4
 *  
 *  
 *********************************************************************/
#ifdef vms
int
sds_fend(sds)
#else
int
#if defined(F77)
sdend_(sds)
#else
sds_fend_(sds)
#endif
#endif
int  *sds;
{ 
  sds_discard(*sds);
  return(1);
}
/*********************************************************************/
/*  SDS_FDECLARE
 *  Declare an object to be in an sds
 *
 *  integer*2  IARR(100)
 *  integer*4  OBJ_IND
 *     c  .......
 *      ISTATUS = SDS_FDECLARE(1,IARR,"MYARRAY",100,SDS_WORD,OBJ_IND)
 *
 *  Declares first 100 elements of 2-byte integer array IARR to be part of
 *  SDS 1; its name in that SDS will be MYARRAY.
 *  The object index is returned in OBJ_IND.
 *
 *  Memory for an object may be allocated at runtime by the underlying
 *  software. Unfortunatly, there is no (standard) way to tell Fortran
 *  where such an object is, so if you want runtime memory allocation
 *  either wait until a solution is cobbled up or, better still, use
 *  C. 
 *  
 *    
 *    
 *********************************************************************/
#ifdef vms
/**************** VMS_FORTRAN ****************************************/
int
sds_fdeclare(sds,obj_ptr,name,nelems,elemcod,obj_index)
long  *sds,*elemcod,*nelems,*obj_index;
char  *obj_ptr,*name;
{
  int  length,leno;
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sddecl_(sds,obj_ptr,name,nelems,elemcod,obj_index,leno,length)
#else
sds_fdeclare_(sds,obj_ptr,name,nelems,elemcod,obj_index,leno,length)
#endif
int  *sds,*elemcod,*nelems,length,leno,*obj_index;
char  *obj_ptr,*name;
{
#endif
  char  *pass_ptr = obj_ptr;
  long  ec = *elemcod;

  if (ec == SDS_FSTRING) {
#ifdef vms
    struct vms_fstring *obj = (struct vms_fstring*)pass_ptr;
    leno = (int)obj->len;
    pass_ptr = obj->string;
#endif
    getc_from_f(&cstring[0],name,&length);  
  }
  else
    getc_from_f(&cstring[0],name,&leno);  

  *obj_index = 
    sds_declare_structure(*sds,pass_ptr,cstring,*nelems,ec);
  if (*obj_index == 0) return(-2*(sds_error));
  else return(1);
}
/*********************************************************************/
/*  SDS_TWOD_FDECLARE 
 *  Declare an 2-d array to be in an sds
 *
 *  integer*2  IARR(100,20)
 *  integer*4  OBJ_IND
 *     c  .......
 *      ISTATUS = SDS_TWOD_FDECLARE(1,IARR,"MYARRAY",100,20,SDS_WORD,OBJ_IND)
 *
 *  The object index is returned in OBJ_IND.
 *
 *    
 *********************************************************************/
#ifdef vms
/**************** VMS_FORTRAN ****************************************/
int
sds_twod_fdeclare(sds,obj_ptr,name,n1,n2,elemcod,obj_index)
long  *sds,*elemcod,*n1,*n2,*obj_index;
char  *obj_ptr,*name;
{
  int  length,leno;
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sdtwod_(sds,obj_ptr,name,n1,n2,elemcod,obj_index,length,leno)
#else
sds_twod_fdeclare_(sds,obj_ptr,name,n1,n2,elemcod,obj_index,length,leno)
#endif
int  *sds,*elemcod,*n1,*n2,length,leno,*obj_index;
char  *obj_ptr,*name;
{
#endif
  char  *pass_ptr = obj_ptr;
  long  ec = *elemcod;

  if (ec == SDS_FSTRING) {
#ifdef vms
    struct vms_fstring *obj = (struct vms_fstring*)pass_ptr;
    leno = (int)obj->len;
    pass_ptr = obj->string;
#endif
    getc_from_f(&cstring[0],name,&length);  
  }
  else
    getc_from_f(&cstring[0],name,&leno);  

  *obj_index = sds_twod_declare(*sds,pass_ptr,cstring,*n2,*n1,ec);
  if (*obj_index == 0) return(-2*(sds_error));
  else return(1);
}
/*********************************************************************/
/*  SDS_THREED_FDECLARE 
 *  Declare an 3-d array to be in an sds
 *
 *  integer*2  IARR(100,20,10)
 *  integer*4  OBJ_IND
 *     c  .......
 *      ISTATUS = SDS_THREED_FDECLARE(1,IARR,"MYARRAY",100,20,10,SDS_WORD,OBJ_IND)
 *
 *  The object index is returned in OBJ_IND.
 *
 *    
 *********************************************************************/
/**************** VMS_FORTRAN ****************************************/
#ifdef vms
int
sds_threed_fdeclare(sds,obj_ptr,name,n1,n2,n3,elemcod,obj_index)
long  *sds,*elemcod,*n1,*n2,*n3,*obj_index;
char  *obj_ptr,*name;
{
  int  length,leno;
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sdthrd_(sds,obj_ptr,name,n1,n2,n3,elemcod,obj_index,length,leno)
#else
sds_threed_fdeclare_(sds,obj_ptr,name,n1,n2,n3,elemcod,obj_index,length,leno)
#endif
int  *sds,*elemcod,*n1,*n2,*n3,length,leno,*obj_index;
char  *obj_ptr,*name;
{
#endif
  char  *pass_ptr = obj_ptr;
  long  ec = *elemcod;

  if (ec == SDS_FSTRING) {
#ifdef vms
    struct vms_fstring *obj = (struct vms_fstring*)pass_ptr;
    leno = (int)obj->len;
    pass_ptr = obj->string;
#endif
    getc_from_f(&cstring[0],name,&length);  
  }
  else
    getc_from_f(&cstring[0],name,&leno);  

  *obj_index = sds_threed_declare(*sds,pass_ptr,cstring,*n3,*n2,*n1,ec);
  if (*obj_index == 0) return(-2*(sds_error));
  else return(1);
}
/*********************************************************************/
/*  SDS_FDUPLICATE 
 *  Duplicate an existing SDS
 *
 *      ISTATUS = SDS_FDUPLICATE(sds,"myfile",NEW_SDS)
 *
 *  The new sds index is returned in NEW_SDS
 *  
 *  
 *********************************************************************/
/**************** VMS_FORTRAN ****************************************/
#ifdef vms
int
sds_fduplicate(sds,filename,new_sds)
int *sds,*new_sds;
char  *filename;
{
  int  length;
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sddup_(sds,filename,new_sds,length)
#else
sds_fduplicate_(sds,filename,new_sds,length)
#endif
int *sds,length,*new_sds;
char  *filename;
{
#endif

  getc_from_f(cstring,filename,&length);
  *new_sds = sds_dup(*sds,cstring);
  if (*new_sds == 0) return(-2*(sds_error));
  else return(1);
}
/*********************************************************************/
/*      SDS_FMAKE
*      Assemble an SDS
*
*      ISTATUS = SDS_FMAKE(sds,"myfile",TYPE_PARAMETER,NEW_SDS)
*
*      NEW_SDS may be different from SDS if a new sds is made (eg when
*      assembled to shared memory)
*
*      where TYPE_PARAMETER is one of
*              SDS_NEW_FILE
*              SDS_APP_FILE
*              SDS_PROC_MEM
*              SDS_SHARED_MEM
*              SDS_SYBASE
*
*
*********************************************************************/
/**************** VMS_FORTRAN ****************************************/
#ifdef vms
int
sds_fmake(sds,filename,type,new_sds)
int *sds,*type,*new_sds;
char  *filename;
{
  int  length;
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sdmake_(sds,filename,type,new_sds,length)
#else
sds_fmake_(sds,filename,type,new_sds,length)
#endif
int *sds,length,*type,*new_sds;
char  *filename;
{
#endif

  getc_from_f(cstring,filename,&length);
  *new_sds = sds_ass((int)*sds,cstring,*type);
  if (*new_sds == 0) return(-2*(sds_error));
  else return(1);
}
/*********************************************************************/
/* SDNORD
 *   Named object read.
 *   Reads a named sds object into a piece of Fortran memory
 * Bound checking is not done: caveat programmer!
 * Better still, don't use Fortram
 */
/**************** VMS_FORTRAN ****************************************/
#ifdef vms
int
sds_snord(sds,obj_ptr,object,fd)
int  *sds,*fd;
char  *obj_ptr,*object;
{
  int oblen;
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sdnord_(sds,obj_ptr,object,oblen,fd)
#else
sds_sdnord_(sds,obj_ptr,object,oblen,fd)
#endif
int  *sds,oblen,*fd;
char  *obj_ptr,*object;
{
#endif
  char  obname[128];
  int max, obindex, number;
  struct direc *dptr = sds_direc_ptr(*sds);
 
  getc_from_f(obname,object,&oblen);
  obindex = sds_name2ind(*sds, obname);
  max = sds_array_size(*sds, obindex);
  number = sds_read_object(*sds,obindex,obj_ptr,0,max,fd);
  if (number == 0) 
    return(-2*sds_error);
  else 
  {
    dptr[obindex].offst = (long)obj_ptr;
    return(1);
  }
}

/*********************************************************************/
/*  SDS_FREADS 
 *
 *  Loads a data object into previously defined character string
 *  spcae. This is only necessary for Vax fortran character
 *  data: it is here for Sun as well for source code 
 *  compatablity. I'll try to find a better way....
 *
 */
/*********************************************************************/
/**************** VMS_FORTRAN ****************************************/
#ifdef vms
int
sds_freads(sds,object,obj_ptr,start,max,number)
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sdsrd_(sds,object,obj_ptr,start,max,number)
#else
sds_freads_(sds,object,obj_ptr,start,max,number)
#endif
#endif
int  *sds,*object,*max,*number,*start;
char  *obj_ptr;
{
  char  *pass_ptr = obj_ptr;
  long  cstart = *start - 1;
  struct direc *dptr = sds_direc_ptr(*sds);

#ifdef vms
  struct vms_fstring *obj = (struct vms_fstring*)obj_ptr;
  pass_ptr = obj->string;
#endif
  *number = sds_read_object(*sds,*object,pass_ptr,cstart,*max);
  if (*number == 0) 
    return(-2*sds_error);
  else 
  {
    dptr[*object].offst = (long)pass_ptr;
    return(1);
  }
}
/*********************************************************************/
/*  SDS_FREAD0 
 *
 *  Loads a data object into previously defined space
 */
/*********************************************************************/
/**************** VMS_FORTRAN ****************************************/
#ifdef vms
int
sds_freado(sds,object,obj_ptr,start,max,number)
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sdread_(sds,object,obj_ptr,start,max,number)
#else
sds_freado_(sds,object,obj_ptr,start,max,number)
#endif
#endif
int  *sds,*object,*max,*number,*start;
char  *obj_ptr;
{
  char  *pass_ptr = obj_ptr;
  long  cstart = *start - 1;
  struct direc *dptr = sds_direc_ptr(*sds);

  *number = sds_read_object(*sds,*object,pass_ptr,cstart,*max);
  if (*number == 0) 
    return(-2*(sds_error));
  else 
  {
    dptr[*object].offst = (long)obj_ptr;
    return(1);
  }
}
/*********************************************************************/
/*  SDS_FLOAD 
 *
 *  Loads an existing SDS to process memory
 *
 *      ISTATUS = SDS_FLOAD("myfile_name",SOURCE_TYPE,ACCESS_MODE,sds)
 *
 *  Where SOURCE_TYPE is one of
 *    SDS_FILE
 *    SDS_DIREC_ONLY
 *    SDS_SHARED_MEM
 *  And ACCESS_MODE is one of
 *    SDS_READ
 *    SDS_WRITE
 *
 *  The resulting SDS index is return in sds
 *  
 *  
 *********************************************************************/
#ifdef vms
/**************** VMS_FORTRAN ****************************************/
int
sds_fload(name,type,mode,sds)
char  *name;
int  *type,*mode,*sds;
{
  int  length;
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sdload_(name,type,mode,sds,length)
#else
sds_fload_(name,type,mode,sds,length)
#endif
char  *name;
int  length,*type,*mode,*sds;
{
#endif
  getc_from_f(cstring,name,&length);  
  *sds = sds_access(cstring,*type,*mode);
  if (*sds == 0) return(-2*(sds_error));
  else return(1);
}
/*********************************************************************/
/*  SDS_STREAM_CLOSE 
 *  close an input stream (use after SDS_FLOAD with
 *  DIRECTORY_ONLY switch on
 *********************************************************************/
#ifdef vms
/**************** VMS_FORTRAN ****************************************/
int
sds_stream_close(sds)
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sdsclo_(sds)
#else
sds_stream_close_(sds)
#endif
#endif
sds_handle  *sds;
{
  int fd = sds_stream(*sds);
  *sds = sds_ass(*sds,sds_obind2name(*sds,0),SDS_PROC_MEM);
  close(fd);
  return(1);
}
/*********************************************************************/
/*  SDS_FWHAT 
 *  
 *  Returns description of object 'obj' in sds 'sds'
 *  number of elements,name and type of element (elemcod) are filled.
 *
 *  INTEGER*4 IA,IB,IC
 *  CHARACTER*20 NAME
 *  .
 *  .
 *      ISTATUS = SDS_FWHAT(1,1,IA,IB,IC,NAME)
 *
 *  
 *  
 *********************************************************************/
#ifdef vms
/**************** VMS_FORTRAN ****************************************/
int
sds_fwhat(sds,obj,nelems,elemcod,name)
int  *sds,*obj,*nelems,*elemcod;
char   *name;
{
  int  length;
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sdwhat_(sds,obj,nelems,elemcod,name,length)
#else
sds_fwhat_(sds,obj,nelems,elemcod,name,length)
#endif
int  *sds,*obj,*nelems,*elemcod,length;
char   *name;
{
#endif
  struct direc *dptr;

  if ((dptr = sds_direc_ptr(*sds)) == DNULL) 
    return(-2*SDS_NO_SUCH_SDS);
  if (*obj > dptr[0].nelems) return(-2*SDS_NO_SUCH_OBJ);
  *nelems = dptr[*obj].nelems;
  *elemcod = dptr[*obj].elemcod;
  getf_from_c(sds_obind2name(*sds,*obj),name,&length);
  return(1);
}

/*********************************************************************/
/*  SDS_FPRINT 
 *  
 *  lists to standard out object 'obj' of SDS 'sds'
 *
 *
 *      ISTATUS = SDS_FPRINT(1,0)
 *
 *  Lists directory of SDS 1
 *
 *  
 *  
 *********************************************************************/
#ifdef vms
/**************** VMS_FORTRAN ****************************************/
int
sds_fprint(sds,obj)
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sdprin_(sds,obj)
#else
sds_fprint_(sds,obj)
#endif
#endif
int *sds,*obj;
{
  sds_list(*sds,*obj,SDS_LIST_FORMATTED);
  return(1);
}
/*********************************************************************/
/*  SDS_FINDO 
 *  find object name 'name' in SDS 'sds'
 *
 *      ISTATUS = SDS_FINDO(1,"blech",obj_ind)
 *
 *  Searches for object "blech" in SDS 1
 *  
 *  Returns object index in obj_ind
 *  
 *********************************************************************/
#ifdef vms
/**************** VMS_FORTRAN ****************************************/
int
sds_findo(sds,name,object)
int *sds,*object;
char *name;
{
  int  length;
#else
/*************** SUN-XTENDED_FORT ******************************************/
int
#if defined(F77)
sdfind_(sds,name,object,length)
#else
sds_findo_(sds,name,object,length)
#endif
int *sds,length,*object;
char *name;
{
#endif
  getc_from_f(cstring,name,&length);
  *object = sds_name2ind(*sds,cstring); 
  if (*object == 0) return(-2*(sds_error));
  else return(1);
}
/*********************************************************************/
void
unzterm(buffer,length)
char *buffer;
int  length;
/*********************************************************************/
{
  int  i;
  for (i=0;i<length;i++,buffer++)
    if (*buffer == 0) *buffer = ' ';
}
/*********************************************************************/
void
zterm(cbuffer)
char *cbuffer;
/*********************************************************************/
{
  while(*(--cbuffer) == ' ');
  cbuffer++;
  if ((int)*cbuffer) *cbuffer = (char)0;
}
#ifdef vms
/*********************************************************************/
int
getc_from_f(cbuffer,fpointer,len)
char  *cbuffer;
struct vms_fstring *fpointer;
int  *len;
/*********************************************************************/
{
  int  i;
  char  *fptr = fpointer->string;
  for(i=0;i<(int)fpointer->len;i++,*cbuffer++ = *fptr++);
  zterm(cbuffer);
}
#else

/*********************************************************************/
int
getc_from_f(cbuffer,fpointer,len)
char  *cbuffer,*fpointer;
int  *len;
/*********************************************************************/
{
  int i;
  for(i=0;i<*len;i++,*cbuffer++ = *fpointer++);
  zterm(cbuffer);
  return 0;
}
#endif
#ifdef vms
/*********************************************************************/
int
getf_from_c(cbuffer,fpointer,len)
char  *cbuffer;
struct vms_fstring *fpointer;
int  *len;
/*********************************************************************/
{
  int i;
  char  *fptr,*tptr;
  tptr = fpointer->string;
  fptr = cbuffer;
  for(i=0;i<(int)fpointer->len && *fptr != (char)0;i++,*tptr++ = *fptr++);
  for(;i<(int)fpointer->len;i++,*tptr++ = ' ');
  return 1;
}
#else

/*********************************************************************/
int
getf_from_c(cbuffer,fpointer,len)
char  *cbuffer,*fpointer;
int  *len;
/*********************************************************************/
{
  int i;
  char  *fptr,*tptr;
  tptr = fpointer;
  fptr = cbuffer;
  for(i=0;i<*len && *fptr != (char)0;i++,*tptr++ = *fptr++);
  for(;i<*len;i++,*tptr++ = ' ');
  return 1;
}
#endif
