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


#include <stdio.h>
#ifndef vms
#include "Sds/sdsgen.h"
#else
#include "sdsgen.h"
#endif

/* What vaxx double precision floating point looks like on a vaxx */
struct  vaxx_double {
  unsigned int  mantissa1 : 7;
  unsigned int  exp1      : 1;
  unsigned int  exp2      : 7;
  unsigned int  sign      : 1;
  unsigned int  mantissa3 : 8;
  unsigned int  mantissa2 : 8;
  unsigned int  mantissa5 : 8;
  unsigned int  mantissa4 : 8;
  unsigned int  mantissa7 : 8;
  unsigned int  mantissa6 : 8;
};

/* ieee double precision floating point,on vaxx*/
struct  ieee_double {
  unsigned int  exp2      : 7;
  unsigned int  sign      : 1;
  unsigned int  mantissa1 : 4;
  unsigned int  exp1      : 4;
  unsigned int  mantissa2 : 8;
  unsigned int  mantissa3 : 8;
  unsigned int  mantissa4 : 8;
  unsigned int  mantissa5 : 8;
  unsigned int  mantissa6 : 8;
  unsigned int  mantissa7 : 8;
};
/* What vaxx single precision floating point looks like on a vaxx */
struct  vaxx_single {
  unsigned int  mantissa1: 7;
  unsigned int  exp1     : 1;
  unsigned int  exp2     : 7;
  unsigned int  sign    : 1;
  unsigned int  mantissa2: 8;
  unsigned int  mantissa3: 8;
};

/* ieee single precision floating point as seen on vaxx */
struct  ieee_single {
  unsigned int  exp2      : 7;
  unsigned int  sign      : 1;
  unsigned int  mantissa1 : 7;
  unsigned int  exp1      : 1;
  unsigned int  mantissa3 : 8;
  unsigned int  mantissa2 : 8;
};

#define VAXX_DBL_BIAS  0x81
#define IEEE_DBL_BIAS  0x3ff
#define MASK(nbits)  ((1 << nbits) - 1)
#define VAXX_SNG_BIAS  0x81
#define IEEE_SNG_BIAS  0x7f

static struct sgl_limits {
  struct ieee_single s;
  struct vaxx_single vaxx;
} sgl_limits[2] = {
  {{ 0x7f, 0x0, 0x0, 0x1, 0x00, 0x00 },  /* Max ieee */
  { 0x7f, 0x1, 0x7f, 0x0, 0xff, 0xff}},    /* Max vaxx */
  {{ 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 },  /* Min ieee */
  { 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 }}    /* Min vaxx */
};

static struct dbl_limits {
  struct  ieee_double d;
  struct  vaxx_double vaxx;
} dbl_limits[2] = {
  {{ 0x7f, 0x0, 0x0, 0xf, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 },  /* Max ieee */
  { 0x7f, 0x1, 0x7f, 0x0, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff }},      /* Max vaxx */
  {{ 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0},    /* Min ieee */
  { 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 }}        /* Min vaxx */
};


/***********************************************************************/
void
r_double(old,new,number)
char  *old,*new;
int  number;
/***********************************************************************/
{
  struct ieee_double  *id = (struct ieee_double *)old;
  struct  vaxx_double  *vd = (struct vaxx_double *)new;
  register struct dbl_limits *lim;
  int i,n,exp;
  char  *cptr;

  for (n=0;n<number;n += sizeof(double),vd++,id++) 
  {
    for (i = 0, lim = dbl_limits;
      i < sizeof(dbl_limits)/sizeof(struct dbl_limits);
      i++, lim++) 
      {
      if ((id->mantissa6 == lim->d.mantissa6) &&
        (id->mantissa7 == lim->d.mantissa7) &&
        (id->mantissa5 == lim->d.mantissa5) &&
        (id->mantissa4 == lim->d.mantissa4) &&
        (id->mantissa3 == lim->d.mantissa3) &&
        (id->mantissa2 == lim->d.mantissa2) &&
        (id->mantissa1 == lim->d.mantissa1) &&
        (id->exp1 == lim->d.exp1) &&
        (id->exp2 == lim->d.exp2)) 
        {
        memcpy((char *)vd,(char *)&lim->vaxx,8);
        goto movedb;
      }
    }
    exp = (id->exp1 | (id->exp2 << 4)) +
        VAXX_DBL_BIAS - IEEE_DBL_BIAS;
    vd->exp1 = exp & 0x1;
    vd->exp2 = (exp & 0xfe) >> 1;

    vd->mantissa1 =((0xf & id->mantissa1) << 3) +
          ((0xe0 & id->mantissa2) >> 5);
    vd->mantissa2 =((0x1f & id->mantissa2) << 3) +
          ((0xe0 & id->mantissa3) >> 5);
    vd->mantissa3 =((0x1f & id->mantissa3) << 3) +
          ((0xe0 & id->mantissa4) >> 5);
    vd->mantissa4 =((0x1f & id->mantissa4) << 3) +
          ((0xe0 & id->mantissa5) >> 5);
    vd->mantissa5 =((0x1f & id->mantissa5) << 3) +
          ((0xe0 & id->mantissa6) >> 5);
    vd->mantissa6 =((0x1f & id->mantissa6) << 3) +
          ((0xe0 & id->mantissa7) >> 5);
    vd->mantissa7 = (0x1f & id->mantissa7) << 3;

    movedb:
      vd->sign = id->sign;
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
  struct ieee_single *is = (struct ieee_single *)old;
  struct vaxx_single *vs = (struct vaxx_single *)new;
  struct sgl_limits *lim;
  int i,n,exp;
  char  *cptr;

  for (n=0; n<number;n += sizeof(float),vs++,is++) 
  {
    for (i = 0, lim = sgl_limits;
      i < sizeof(sgl_limits)/sizeof(struct sgl_limits);
      i++, lim++) 
      {
      if ((is->mantissa2 == lim->s.mantissa2) &&
        (is->mantissa3 == lim->s.mantissa3) &&
        (is->exp1 == lim->s.exp1) &&
        (is->exp2 == lim->s.exp2) &&
        (is->mantissa1 == lim->s.mantissa1)) 
        {
        memcpy((char *)vs,(char *)&lim->vaxx,4);
        goto movesn;
      }
    }
    exp = (is->exp1 | (is->exp2 << 1)) +
        VAXX_SNG_BIAS - IEEE_SNG_BIAS;
    vs->exp1 = exp & 0x1;
    vs->exp2 = (exp & 0xfe) >> 1;
    vs->mantissa1 = is->mantissa1;
    vs->mantissa2 = is->mantissa2;
    vs->mantissa3 = is->mantissa3;
  movesn:
    vs->sign = is->sign;
  }
  return ;
}
