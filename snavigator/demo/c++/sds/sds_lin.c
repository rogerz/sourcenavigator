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

static  sds_handle  flat_sds = -1,flat_obj = -1;
static  int  template_count;
static  int  bumpy_struct_size;
static char  *bump_buffer = (char *)0;

static struct template {
  sds_code        elemcod;
  char           *address;
  sds_code        size;
  char           *target;
  } template[64];

/********** forward declarations ***********/
void       flat_cleanup(void);
int        make_template(sds_handle,sds_handle);

/*********************************************************************/
sds_handle
sds_flat_setup(sds,object,subelem,type,pointer)
sds_handle  sds;
sds_code    object,*type;
int         subelem;
void        *pointer;
/*********************************************************************/
{
  struct  direc  *dptr = sds_direc(sds);

  if (dptr == DNULL)
		{
				sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Flattened data structure");
				return 0L;
		}
  if (flat_sds == -1) 
  {
    if (!make_template(sds,object)) 
    {
      flat_cleanup();
						sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Flattened data structure");
						return 0L;
    }
    flat_sds = sds;
    flat_obj = object;
  }
  if (flat_sds != sds) 
  {
    flat_cleanup();
				sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Flattened data structure");
				return 0L;
  }
  if ((subelem > template_count) || (subelem < 0)) 
  {
		  sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Flattened data structure");
				return 0L;
  }

  template[subelem].target = (char*) pointer;
  *type = (int)template[subelem].elemcod;
  return 1L;
}
/*********************************************************************/
void
flat_cleanup()
/*********************************************************************/
{
  flat_sds = -1;
  if (bump_buffer != (char *)0)
    free(bump_buffer);
  bump_buffer = (char *)0;
}
/*********************************************************************/
sds_handle 
sds_flat_read(sds_handle sds,sds_code object,sds_code max, int fd)
/*********************************************************************/
{
  unsigned long  rec,i,j,offset;
  struct  sds_header  *head;
  struct direc *dptr = sds_direc(sds);

  if (fd < 0)
  {
    sds_push_error(SDS_FILE_NOP,SDS_ERROR,"Reading flat data");
    return 0L;
  }

  if (dptr == DNULL)
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Reading flat data");
    return 0L;
  }
  
  if (object != flat_obj)
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Reading flat data");
    return 0L;
  }

  head = sds_head(sds);
  offset = BASE_OFFSET + head->list_size + head->heap_size;
  for (i=0;i<object;i++) 
  {
    offset += dptr[i].nelems*dptr[i].elemsz;
    offset += align_delta((int)offset,sds_rbyte());
  }

/* Don't try to copy more than max objects (else potential bang)   */
  if (max > dptr[object].nelems) max = dptr[object].nelems;

  if (lseek(fd,offset,L_SET) < 0) 
  {
    sds_push_error(SDS_FILE_OP,SDS_ERROR,"Lseek failure:");
    return 0L;
  }
  for (rec = 0;rec<max;rec++) 
  {
    if ((sds_read(fd,bumpy_struct_size,bump_buffer)) < 0)
    {
      sds_push_error(SDS_FILE_RD,SDS_ERROR,"Reading flat data");
      return 0L;
    }
    for (j=0;j<template_count;j++) 
      if (template[j].target  != (char *)0) 
      {
        memcpy(template[j].target,
          template[j].address,
          template[j].size);
        template[j].target += template[j].size;
      }
  }
  return(rec);
}
/*********************************************************************/
int
make_template(sds,obj)
sds_handle  sds,obj;
/*********************************************************************/
{

  int  level,ok = 1,start = 0;
  struct  sds_odesc  *thing;
  char  *base_addr = 0;

  template_count = 0;
  while ((level = sds_describe(sds,obj,&thing)) >= 0) 
  {
    if (!(thing[level].elemcod & SDS_INDLIST) && !start) 
    {
      start = level;
      bumpy_struct_size = thing[level-1].size;
      bump_buffer = sds_malloc(bumpy_struct_size);
      base_addr = thing[level].address;
    }
    if (start == level) 
    {
      if ((thing[level].elemcod & SDS_INDLIST)) 
        ok = 0;
      template[template_count].elemcod = thing[level].elemcod;
      template[template_count].address =
        thing[level].address -
        base_addr +
        bump_buffer;
      template[template_count].target = (char *)0;
      template[template_count].size  = (int)thing[level].size;
      template_count++;
    }
  }    
  return(ok);
}
