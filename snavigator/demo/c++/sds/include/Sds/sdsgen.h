#ifndef ISTKsdsgen_h
#define ISTKsdsgen_h

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


#if defined(psos)
#include <prepc.h>
#else
#include <stdio.h>     
#include <ctype.h>     
#include <errno.h>
#endif /* psos */


/* On most systems we can get the headers required by SDS from the same
 * place.  Here we test for those systems and if present we set SDS_STD_HEADERS
 * and are done till below, where we test for SDS_STD_HEADERS.
 */
#ifndef vms
#if defined(NeXT) || defined(hpux) || defined(__hpux__) || \
    defined(sun) || defined(__sun__) || defined(__DGUX__) || \
    defined(masscomp) || defined(mips) || defined(__mips__) || \
    defined (__i486__) || defined(__i386__) || defined(eunice) || \
    defined (__alpha__)
#define SDS_STD_HEADERS
#endif
#endif


#ifdef vms
#include <types.h>
#include <stat.h>
#include <file.h>
#include <time.h>
#define SDS_VMS_FILENAMES
#endif /* vms */

#ifdef mv147
#ifdef VXWORKS
#include <vxWorks.h>
#define SDS_STD_HEADERS
#include <string.h>
#include <in.h>
#include <remLib.h>
#define strchr index
#endif /* VXWORKS */

#ifdef psos
#include <stdio.h>
#include <types.h>
#include <psos.h>
#include <in.h>
#include <sds_mdep.h> /* start of SDS-specific include files */
#include <direc.h>
#include <sds.h>
#include <sds_errors.h>
#include <sds_record.h>
#include <ctype.h>
#endif /* psos */
#elif defined(VXWORKS)
#include <vxWorks.h>
#include <string.h>
#include <sys/stat.h>
#include <in.h>
#include <remLib.h>
#define strchr index
#endif /* mv147 */


#ifdef atari
#include <types.h>
#include <stat.h>
#include <time.h>
#define O_TRUNC 0x10
#define O_APPEND 0x08
#define O_RDONLY 0
#define O_WRONLY 0x01
#define O_RDWR   0x02
#define O_CREAT  0x04
#define SDS_MSDOS_FILENAMES
#define EINVAL   123
typedef  long  off_t;
#endif /* atari */


#ifdef SDS_STD_HEADERS
#include <sys/types.h>
#if defined(__hpux__)
#include <sys/uio.h>
#endif
#include <sys/file.h>
#include <sys/stat.h>
#include <time.h>
#include <fcntl.h>
#endif /* SDS_STD_HEADERS */


#if !defined(SDS_MSDOS_FILENAMES) && !defined(SDS_VMS_FILENAMES)
#define O_BINARY 0 /* just a dummy for normal machines */
#include "Sds/sds_mdep.h"
#include "Sds/direc.h"
#include "Sds/sds.h"
#include "Sds/sds_errors.h"
#include "Sds/sds_record.h"
#endif

#ifdef SDS_MSDOS_FILENAMES /* MSDOS filenames */
#include "..\include\Sds\sds_mdep.h"
#include "..\include\Sds\direc.h"
#include "..\include\Sds\sds.h"
#include "..\include\Sds\sds_errors.h"
#include "..\include\Sds\sds_record.h"
#endif /* SDS_MSDOS_FILENAMES */

#ifdef SDS_VMS_FILENAMES 
#include "sds_mdep.h"
#include "direc.h"
#include "sds.h"
#include "sds_errors.h"
#include "sds_record.h"
#endif /* SDS_VMS_FILENAMES */

struct sds_sys_control {
  int re_init;
	int fd_buffer;
  int maxsds;
  int maxbufsize;
  int controlbits;
  struct sds_error_control *sec;
  struct sds_control_p **sds_cp;
  struct sds_saverestore sr;
};


extern int sds_error;


#endif /* ISTKsdsgen_h */
