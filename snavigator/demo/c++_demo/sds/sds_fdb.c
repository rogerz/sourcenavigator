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

#include <stdlib.h>
#include <string.h>

#ifndef vms
#include <unistd.h>
#endif

#ifndef VXWORKS
#include <memory.h>
#endif

#if defined(vms)
#include "sdsgen.h"
#include "sds_externs.h"
#else
#include "Sds/sdsgen.h"
#include "Sds/sds_externs.h"
#endif

static	int	db_object[MAX_SDS];
static	long	db_next_row[MAX_SDS];

/************************************************************************/
sds_handle
sds_dbtab_make(nrows,basename,tlist,select_list)
int	nrows;
char	*select_list,*basename;
struct type_list	*tlist;
/************************************************************************/
{
	sds_handle	sds, obj;
	unsigned long	tlist_index;

	if (!sds_initialised())
		sds_init();

	sds = sds_new(basename);
  if (!sds)
	{
		sds_perror("Making table sds:");
		exit(1);
	}

	tlist_index = sds_define_structure(sds,tlist,select_list);

	obj = sds_declare_structure(sds,
			SDS_ALLOCATE,
			basename,
			nrows,
			tlist_index);
  if (!obj)
	{
		sds_perror("Making table sds:");
		exit(1);
	}

	db_object[sds] = obj;
	db_next_row[sds] = 0;

	if (!(sds = sds_ass(sds,basename,SDS_PROC_MEM)))
	{
		sds_perror("Making table sds:");
		exit(1);
	}
	return(sds);
}
/************************************************************************/
sds_handle
sds_dbrow_ins(sds,pointer)
sds_handle	sds;
char	*pointer;
/************************************************************************/
{

	long	next_row = db_next_row[sds];
	int	object = db_object[sds];
	char	*sptr;
	struct direc	*dptr = sds_direc(sds);

	if (next_row > dptr[object].nelems)
	{
			sds_push_error(SDS_NO_MEM, SDS_ERROR,"No space for all these rows");
			return 0L;
	}

	sptr = (char *)sds_getp(dptr,(long)object) +
										next_row * dptr[object].elemsz;

	memcpy(sptr,pointer,(int)dptr[object].elemsz);
	db_next_row[sds]++;
	return 1L;
}
