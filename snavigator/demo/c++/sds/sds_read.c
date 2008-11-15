/* $Header$*/

#include "Sds/sdsgen.h"

#include <unistd.h>
#include <stdlib.h>

#if !defined(VXWORKS)
#if !defined(__MSDOS__) 
#include <malloc.h>
#include <memory.h>
#else
#include <alloc.h>
#include <mem.h>
#endif
#endif

#if defined(__SUN4)
extern void bcopy(char *, char *,int);
#define memmove(a,b,n) bcopy(b,a,n)
#endif

int
sds_close_fd(int fd)
{
  if (fd) close(fd);
  return 1;
}

int 
sds_read(int fd, int requested, char *buffer)
{
  return read(fd,buffer,requested);
}
/***********************************************************************/
int
sds_write(int fd,char *ptr,int nbytes)
/***********************************************************************/
{
  int nleft, nwritten;

  nleft = nbytes;
  while (nleft != 0) 
  {
    nwritten = write(fd,ptr,nleft);
#ifdef VXWORKS
    if (nwritten < 0) 
      return nwritten;    /* error */
#else
    if (nwritten <= 0) 
      return nwritten;    /* error */
#endif
    nleft -= nwritten;
    ptr   += nwritten;
  }
  return(nbytes);
}
