/* $Header$ */



#if ! defined(vms) && ! defined (__GCC_2__)
#include <stddef.h>
#endif
#ifndef VXWORKS
#include <memory.h>
#include <malloc.h>
#endif


#include "Sds/sdsgen.h"
#include "Sds/sds_externs.h"

/*********************************************************************/
sds_handle
sds_fullsize(sds)
sds_handle sds;
/*********************************************************************/
{
  struct direc *dptr;

  if ((dptr = sds_direc(sds)) == DNULL)
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"From dataset size");
    return 0L;
  }
  else
    return sds_sz(dptr);
}
/***********************************************************************/
unsigned long
sds_data_sz(dptr)
struct direc *dptr;
/***********************************************************************/
{
  unsigned long size = sds_sz(dptr);
  if (size > (unsigned long)0) 
    size -= (long)dptr[1].offst;
  return(size);
}

/*********************************************************************/
unsigned long
sds_sz(dptr)
struct direc *dptr;
/*********************************************************************/
{
  int size, i;
	sds_handle sds = sds_which(dptr);

  if (!sds)
	  size = (long)dptr[0].offst;
  else
    size =  (unsigned long )(tlist_size(sds_tlist(sds)) + sds_heap_size(sds));
  size += BASE_OFFSET;

  for ( i = 0; i<dptr[0].nelems ; i++ )
  {
    size += align_delta((int)size,dptr[i].align_type);
    size += dptr[i].nelems*dptr[i].elemsz;
  }
  return size;
}
