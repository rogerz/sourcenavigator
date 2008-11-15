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

#if defined(hpux)
#include <string.h>
#else
#ifndef VXWORKS
#include <memory.h>
#endif
#endif

#if defined(psos)
#include <prepc.h>
#else
#include <stdio.h>
#endif

#if defined(vms)
#include "sdsgen.h"
#else
#include "Sds/sdsgen.h"
#endif

/* What IEEE double precision floating point looks like on a Snu */
struct  ieee_double {
  unsigned int  sign      : 1;
  unsigned int  exp       : 11;
  unsigned int  mantissa1 : 20;
  unsigned int  mantissa2 : 32;
};

/* Vax double precision floating point,on Sun*/
struct  vax_double {
  unsigned int  exp1      : 1;
  unsigned int  mantissa1 : 7;
  unsigned int  sign      : 1;
  unsigned int  exp2      : 7;
  unsigned int  mantissa3 : 8;
  unsigned int  mantissa2 : 8;
  unsigned int  mantissa4 : 8;
  unsigned int  mantissa5 : 8;
  unsigned int  mantissa6 : 8;
  unsigned int  mantissa7 : 8;
};
/* What IEEE single precision floating point looks like on a Snu */
struct  ieee_single {
  unsigned int  sign    : 1;
  unsigned int  exp     : 8;
  unsigned int  mantissa: 23;
};

/* Vax single precision floating point as seen on Snu */
struct  vax_single {
  unsigned int  exp1      : 1;
  unsigned int  mantissa1 : 7;
  unsigned int  sign      : 1;
  unsigned int  exp2      : 7;
  unsigned int  mantissa3 : 8;
  unsigned int  mantissa2 : 8;
};

#define VAX_DBL_BIAS  0x81
#define IEEE_DBL_BIAS  0x3ff
#define MASK(nbits)  ((1 << nbits) - 1)
#define VAX_SNG_BIAS  0x81
#define IEEE_SNG_BIAS  0x7f

static struct sgl_limits {
  struct vax_single s;
  struct ieee_single ieee;
} sgl_limits[2] = {
  {{ 0x1, 0x7f, 0x0, 0x7f, 0xff, 0xff },  /* Max Vax */
  { 0x0, 0xff, 0x0 }},    /* Max IEEE */
  {{ 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 },  /* Min Vax */
  { 0x0, 0x0, 0x0 }}    /* Min IEEE */
};

static struct dbl_limits {
  struct  vax_double d;
  struct  ieee_double ieee;
} dbl_limits[2] = {
  {{ 0x1, 0x7f, 0x0, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff },  /* Max Vax */
  { 0x0, 0x7ff, 0x0, 0x0 }},      /* Max IEEE */
  {{ 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0},    /* Min Vax */
  { 0x0, 0x0, 0x0, 0x0 }}        /* Min IEEE */
};


/***********************************************************************/
void
r_double(old,new,number)
char  *old,*new;
int  number;
/***********************************************************************/
{
  struct ieee_double  *id = (struct ieee_double *)new;
  struct  vax_double  *vd = (struct vax_double *)old;
  register struct dbl_limits *lim;
  int i,n;

  for (n=0;n<number;n += sizeof(double),vd++,id++) {
    for (i = 0, lim = dbl_limits;
      i < sizeof(dbl_limits)/sizeof(struct dbl_limits);
      i++, lim++) {
      if ((vd->mantissa7 == lim->d.mantissa7) &&
        (vd->mantissa6 == lim->d.mantissa6) &&
        (vd->mantissa5 == lim->d.mantissa5) &&
        (vd->mantissa4 == lim->d.mantissa4) &&
        (vd->mantissa3 == lim->d.mantissa3) &&
        (vd->mantissa2 == lim->d.mantissa2) &&
        (vd->mantissa1 == lim->d.mantissa1) &&
        (vd->exp1 == lim->d.exp1) &&
        (vd->exp2 == lim->d.exp2)) {
        memcpy((char *)id,(char *)&lim->ieee,8);
        goto movedb;
      }
    }
    id->exp = (vd->exp1 | (vd->exp2 << 1)) -
        VAX_DBL_BIAS + IEEE_DBL_BIAS;
    id->mantissa1 = (vd->mantissa1 << 13) |
        (vd->mantissa2 << 5) |
        (vd->mantissa3 >> 3);
    id->mantissa2 = ((vd->mantissa3 & MASK(3)) << 29) |
        (vd->mantissa4 << 21) |
        (vd->mantissa5 << 13) |
        (vd->mantissa6 << 8) |
        ((vd->mantissa7 >> 3) & MASK(5));
    movedb:
      id->sign = vd->sign;
  }
  return ;
}

/***********************************************************************/
void
r_float(old,new,number)
char  *old,*new;
int  number;
/***********************************************************************/
{
  struct ieee_single *is = (struct ieee_single *)new;
  struct vax_single *vs = (struct vax_single *)old;
  struct sgl_limits *lim;
  int i,n;

  for (n=0; n<number;n += sizeof(float),vs++,is++) {
    for (i = 0, lim = sgl_limits;
      i < sizeof(sgl_limits)/sizeof(struct sgl_limits);
      i++, lim++) {
      if ((vs->mantissa2 == lim->s.mantissa2) &&
        (vs->exp1 == lim->s.exp1) &&
        (vs->exp2 == lim->s.exp2) &&
        (vs->mantissa1 == lim->s.mantissa1)) {
        memcpy((char *)is,(char *)&lim->ieee,4);
        goto movesn;
      }
    }
    is->exp = (vs->exp1 | (vs->exp2 << 1)) -
        VAX_SNG_BIAS + IEEE_SNG_BIAS;
    is->mantissa = (vs->mantissa1 << 16) |
        (vs->mantissa2 << 8) |
        vs->mantissa3;
  movesn:
    is->sign = vs->sign;
  }
  return ;
}
