
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


#if defined(vms)
#include "sdsgen.h"
#include "sds_externs.h"
#else
#include "Sds/sdsgen.h"
#include "Sds/sds_externs.h"
#endif

/*  Here is stored code to allow Fortran to load some
  special types - multi-dimensioned arrays - into
  SDS.
*/

static struct type_list twod_type_list[] =
  {{-1,-1},
   {0,SDS_RETLIST},
   {0,SDS_ENDLIST}};

/*********************************************************************/
sds_handle
sds_twod_declare(sds_index,obj_ptr,name,n1,n2,code)
sds_handle  sds_index;
void  *obj_ptr;
char  *name;
sds_code  code,n1,n2;
/*********************************************************************/
{
  int err;
  twod_type_list[0].nelems = (unsigned long)n2;
  twod_type_list[0].elemcod = (unsigned long)code;
  err = sds_declare_structure(sds_index,obj_ptr,name,n1,
    sds_define_structure(sds_index,twod_type_list, "dim1,dim2"));
  return err?err:sds_error;
}
/*********************************************************************/
sds_handle
sds_threed_declare(sds_index,obj_ptr,name,n1,n2,n3,code)
sds_handle  sds_index;
void  *obj_ptr;
char  *name;
sds_code  code,n1,n2,n3;
/*********************************************************************/
{

  unsigned long  two_code;
	int err;

  twod_type_list[0].nelems = (unsigned long)n3;
  twod_type_list[0].elemcod = (unsigned long)code;
  two_code = sds_define_structure(sds_index,twod_type_list, "dim1,dim2");
  twod_type_list[0].nelems = (unsigned long)n2;
  twod_type_list[0].elemcod = (unsigned long)two_code;

  err = sds_declare_structure(sds_index,obj_ptr,name,n1,
    sds_define_structure(sds_index,twod_type_list, "dim2,dim3"));
  return err?err:sds_error;
}

