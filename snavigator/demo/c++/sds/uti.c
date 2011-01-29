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

/************************************************************************
 *                           
 *      Copyright (C) Frogsoft Corporation, 1986      
 *                           
 *     This library contains Proprietary Information of Frogsoft   
 *   Corporation and should not be treated as Confidential.      
 *                           
 *                general  utility  library                    
 *                           
 *                authors:  probably many                               
 *       
 ************************************************************************/

#include <stdlib.h>
#include <string.h>

#ifndef vms
#include <unistd.h>
#endif

#if !defined(__MSDOS__)
#include <limits.h>
#ifndef VXWORKS
#include <malloc.h>
#include <memory.h>
#endif
#if defined(vms)
#include "sdsgen.h"
#include "sds_externs.h"
#else
#include "Sds/sdsgen.h"
#include "Sds/sds_externs.h"
#endif
#else /* ms dos filenames */
#include <mem.h>
#include <limits.h>
#include <alloc.h>
#include <io.h>
#include "Sds\sdsgen.h"
#include "Sds\sds_exte.h"
#endif

/********* forward declarations ************/
void inttoa(int, char *);
void reverse(char *);
void top_win();
void bot_win();
int  htoi(char *);
     
#if !defined(__hpux__) && !defined(hpux)
/************************************************************************
 * hp ux has its own ltoa, which does the right thing. The gnu
 * loader has problems with multiple definitions.
 ************************************************************************/
void
ltoa(long n,char s[])
/************************************************************************
 * converts an integer n into ascii string s                  
 * Author:  Kernighan and Ritchie 
 ************************************************************************/
{
  int i;
  long sign;
  if((sign = n) < 0)
     n = -n;
  i = 0;
  do   
  {
    s[i++] = n % 10L + '0';
  }  while ((n /= 10L) > 0);
  if(sign<0L)   
		s[i++] = '-';
  s[i] = '\0';
  reverse(s);
}
#endif /* not hpux */

void
inttoa(int n,char s[])
/************************************************************************
 * converts an integer n into ascii string s                  *
 * Author:  Kernighan and Ritchie               */
/************************************************************************/
{
  int i, sign;
  if((sign = n) < 0)
     n = -n;
  i = 0;
  do   
  {
    s[i++] = n % 10 + '0';
  }  while ((n /= 10) > 0);
  if(sign<0)  s[i++] = '-';
  s[i] = '\0';
  reverse(s);
}
void
reverse(char s[])
/************************************************************************
 *     reverses the ascii string s                            *
 *     Author:  Kernighan and Ritchie                         */
/************************************************************************/
{
  int c,i,j;
     
  for (i = 0, j = (int)strlen(s) -1 ; i<j; i++, j--) 
  {
     c = s[i];
     s[i] = s[j];
     s[j] = c;
  }
}
/************************************************************************/
off_t ffsiz(int fildes)
/************************************************************************/
{
   struct   stat   buf;
   int   i;
     
   i = fstat(fildes,&buf);
   if (i != 0 ) 
		 return(-1);
   return buf.st_size;
}
     
/************************************************************************/
off_t fsiz(path)
char   *path;
/************************************************************************/
{
   struct   stat   buf;
   int   i;
     
   i = stat(path,&buf);
   if (i != 0 ) 
		 return(-1);
   return(buf.st_size);
}
int
htoi(char *string)
{
 
   int    i;
   sscanf(string,"%x",&i);
   return i;
}
/************************************************************************/
int
sds_namelist(char *to,char *from,char delim)
/************************************************************************/
{
  int  count = 1;
  char  *f = from,*t = to;

  if ((*t = *f) == (char)0)
    return 0;
  while (*f != (char)0) 
  {
    *t = *f;
    if (*f == delim) 
    {
      *t = (char)0;
      count++;
    }
    f++;
    t++;
  }
  *t = (char)0;
  return count;
}
int
get_size(char *input)
{

  int size,i=0;

  if (input[(int)strlen(input)-1] != 'k') 
    size = atoi(input);
  else 
  {
    input[(int)strlen(input)-1] = (char)0;
    size = atoi(input) << 10;
  }
  if (size > 0 ) 
    while ((1 << i++) < size);
      size = 1 << --i;
  return size;
}

int
r_get_size(char *input)
{
  int size;

  if (input[(int)strlen(input)-1] != 'k') 
    size = atoi(input);
  else 
  {
    input[(int)strlen(input)-1] = (char)0;
    size = atoi(input) << 10;
  }
  return(size);
}

/***********************************************************************/


void *
sds_calloc(nelem,elsize)
sds_code nelem,elsize;
{
  void *retval;


  if (!(retval = (void *)calloc((unsigned)nelem,(unsigned)elsize)))
  {
    perror("Memory allocation failure in sds_calloc()");
    exit(1);
  }
  return retval;
}

void *
sds_malloc(size)
sds_code size;
{
  void *retval;
  if (!(retval = (void *)malloc((unsigned)size)))
  {
    perror("Memory allocation failure in sds_malloc()");
    exit(1);
  }
  return retval;
}

void *
sds_realloc(block,size)
void *block;
sds_code size;
{
  void *retval;
  if (!(retval = (void *)realloc(block, (unsigned)size)))
  {
    perror("Memory allocation failure in sds_realloc()");
    exit(1);
  }
  return retval;
}

