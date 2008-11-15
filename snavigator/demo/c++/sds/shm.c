/* $Header$ */

#if defined(vms)
#include "sdsgen.h"
#include "sds_externs.h"
#else
#include "Sds/sdsgen.h"
#include "Sds/sds_externs.h"
#endif

#if defined(SHMEM)

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


/*s**********************************************************************
 *                                                                      *
 *              Copyright (C) Frogsoft Corporation, 1986                *
 *                                                                      *
 *      This library contains Proprietary Information of Frogsoft       *
 *      Corporation and should not be treated as Confidential.          *
 *                                                                      *
 *                                                                      *
 *                                                                      *
 *      Authors: WFH, CGS                                               *
 *                                                                      *
 *                                                                      *
 *                                                                      */
/*e**********************************************************************/

#include <stdio.h>     
#include <ctype.h>     
#include <stdlib.h>

#ifndef vms
#include <unistd.h>
#include <memory.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <string.h>
#if defined(__hpux__)
#include <sys/sysmacros.h>
#endif
#endif

#if defined(__alpha__)
extern key_t ftok(char *,char);
extern int shmget(key_t, u_int, u_int);
#endif

#if defined(__DGUX__) || defined(sgi) || defined(__hpux__)
extern void     *shmat();
#else
extern char     *shmat();
#endif

#ifdef lynx68k
#define SHM_DEF_KEY_SIZE 1
#else
#define SHM_DEF_KEY_SIZE 0
#endif

key_t getkey();

/******* forward declarations *****/
char      *shm_attach(key_t, int);
void       shm_dt(struct direc *);
int        shm_destroy(char *);

int
shm_lock(char *name, int lock)
/******************************************************
 *  [Un]Lock a dataset in memory. Must have su effective ID
 *
 ******************************************************/
{
#if !defined(__alpha__) /* SHM_LOCK not defined for alpha, apparently */
  if (lock != SHM_LOCK && lock != SHM_UNLOCK)
    return 0;
  if (shm_q(name)) /*it exists */
  { 
    key_t             mykey;
    int               shmid;
    char              fullname[256];
    struct  shmid_ds  shmidstruc;
    struct  shmid_ds  *buf = &shmidstruc;

    nexpand(fullname,name);
    mykey = getkey(fullname);
    shmid = shmget(mykey,SHM_DEF_KEY_SIZE,0);
    if (shmctl(shmid,lock,buf) != 0) 
      return 0;
    return 1;
  }
#endif
  return 0;
}
     
char *
shm_make(char *name,int size,int mode)
/*s**********************************************************************
 *                                                                      *
 *      Creates a data segment of 'name' (with expansion)               *
 *      size 'size' (same units as sizeof)                              *
 *      mode 'mode' (umask is cleared before creation)                  *
 *                                                                      *
 *      Author: CGS, WFH                                                *
 *                                                                      */
/*e**********************************************************************/
{
  struct  shmid_ds  nop;
  char             *sdptr;
  key_t             key;
  char              fullname[128];
  int               sid;
  int               cmask,flags;
  int               user_id, grp_id;
  struct  stat      statbuf;

  user_id = getuid();
  grp_id  = getgid();
     
  nop.shm_perm.mode = 0666;
  nop.shm_perm.uid = (ushort)user_id;
  nop.shm_perm.gid = (ushort)grp_id;
   
  cmask = umask(0);
  nexpand(fullname,name);
  if (stat(fullname,&statbuf) == -1) 
  {
    char temprpt[128];
    sprintf(temprpt,"Check existance of key file for shared memory:%s",fullname);
    sds_push_error(SDS_FILE_OP,SDS_ERROR,temprpt);
    return (char *)0;
  }

 if (shm_q(name) && shm_sz(name) == size)
 {
    key = getkey(fullname);
    sid = shmget(key,SHM_DEF_KEY_SIZE,0666);
 }
  else 
 {
    if (shm_q(name))
      shm_destroy(name);
    key = getkey(fullname);
    flags = IPC_CREAT | 0666;
    if ((sid = shmget(key,size,flags)) == -1) 
    {
      char temprpt[128];
      sprintf(temprpt,"shmget() failed, key was %d", (int)key);
      sds_push_error(SDS_FILE_OP,SDS_ERROR,temprpt);
      return (char *)0;
    }
    shmctl(sid,IPC_SET,&nop);
  }

  sdptr = shm_attach(sid,0);

  cmask = umask(cmask);
  return(sdptr);
}
struct direc    *
shm_attr(char *name)
/*s**********************************************************************
 *                                                                      *
 *      1. Attach shared data with name "/user/.../shardat/name" and    *
 *         return pointer to directory.                                 *
 *                                                                      *
 *      Author: CGS, WFH                                                *
 *                                                                      */
/*e**********************************************************************/
{
  char              *sdptr;
  key_t              key;
  int                sid;
  char              fullname[80];
     
  nexpand(fullname,name);
  key = getkey(fullname);
  sid = shmget(key,SHM_DEF_KEY_SIZE,0666);
  sdptr = shm_attach(sid,SHM_RDONLY);

  if (sdptr == SNULL) return(DNULL);

  if (!sds_header_ok((struct sds_header *)sdptr)) 
  {
    shmdt(sdptr);
    return (struct direc *)0;
  }
  return(header_to_direc((struct sds_header *)sdptr));
}
     
struct direc    *
shm_attw(char *name)
/*s**********************************************************************
 *                                                                      *
 *      1. Attach shared data with name "/user/.../shardat/name" and    *
 *         return pointer to it.                                        *
 *                                                                      *
 *      Author: CGS, WFH                                                *
 *                                                                      */
/*e**********************************************************************/
{
  char  *sdptr;
  key_t   key;
  int     sid;
  char    fullname[80];
     
  nexpand(fullname,name);
  key = getkey(fullname);
  sid = shmget(key,SHM_DEF_KEY_SIZE,0666);
  sdptr = shm_attach(sid,0);

  if (sdptr == SNULL) 
  {
    return (struct direc *)0;
  }
  if (!sds_header_ok((struct sds_header *)sdptr)) 
  {
    shmdt(sdptr);
    return (struct direc *)0;
  }
  return(header_to_direc((struct sds_header *)sdptr));
}
     
int
shm_quit(struct direc *dptr)
/*s**********************************************************************
 *                                                                      *
 *      Author: CGS, WFH                                                *
 *                                                                      */
/*e**********************************************************************/
{
  char *sdptr = (char *)dptr - (long)dptr[0].offst;
  return(shmdt(sdptr));
}

int
shm_q(char *name)
/*s**********************************************************************
 *                                                                      *
 *      Query the existence of a shared data segment                    *
 *              calling sequence:       i = shm_q(name)                 *
 *                                      char    *name (name of segment) *
 *              return values:          i = 0  segment does not exist   *
 *                                      i = 1  segment does exist       *
 *                                                                      *
 *      Author: WFH                                                     *
 *      last modification:      23.02.1987      14:36                   *
 *                                                                      */
/*e**********************************************************************/
{
        key_t   key;
        char    fullname[80];
     
        nexpand(fullname,name);
        key = getkey(fullname);
         return(shmget(key,0,0666)<0?0:1);
}

void
shm_dt(struct direc *dptr)
{
  char *sdptr = (char *)dptr;
  sdptr -= (long)dptr[0].offst;
  shmdt(sdptr);
}

sds_handle
sds_file2shm(char *fname,char *ffname)
/*s**********************************************************************
 *                                                                      *
 *   Copies file "fname" to a shared data segment "fname"               *
 *   The shared data segment is created                                 *
 *                                                                      *
 *      Author: WFH,CS,RJ}      09.05.1987      16:51                   *
 *   Last modification:   21.07.1987   10:22                            *
 *                                                                      */
/*e**********************************************************************/
{
  char            *sdptr;
  int    nbytes;
  char    fullname[256];
  key_t   key;
  int     fd;
  int   sid, shmflg;
  off_t   ffsiz();
	sds_handle sds = next_sds();
     
  nexpand(fullname,fname);
  key = getkey(fullname);
  if ((fd = open(ffname,O_RDONLY, 0664)) == -1)
    return 0;

  nbytes = ffsiz(fd) + BASE_OFFSET;
/*      make table                                      */
  shmflg = 0666 | IPC_CREAT;
  sid = shmget(key,(int)nbytes,shmflg);
  if ((sdptr = shm_attach(sid,0)) == SNULL) 
    return 0;

  sds_read(fd,nbytes,sdptr);
  sds_close_fd(fd);
  shmdt(sdptr);
	return sds;
}

int
shm_sz(char *fname)
/*********************************************************************
*                                                                    *
*   returns size of a shared data segment fname                      *
*                                                                    *
*   Author:   CGS,WFH      15.06.1987                                *
*                                                                    *
***************************************************/
{
  struct   direc   *dptr;
  struct sds_header *h;
  long               size;
  char              *sdptr;
  key_t              key;
  int                i, sid;
  char               fullname[80];
     
  nexpand(fullname, fname);
  key = getkey(fullname);
  sid = shmget(key,SHM_DEF_KEY_SIZE,0666);
  sdptr = shm_attach(sid,SHM_RDONLY);

  if (sdptr == SNULL) 
    return 0;

  if (!sds_header_ok((struct sds_header *)sdptr)) 
  {
    shmdt(sdptr);
    return 0;
  }
  dptr = header_to_direc((struct sds_header *)sdptr);
     
  h = (struct sds_header *)sdptr;
  size = h->heap_size + h->list_size;
  size += BASE_OFFSET;

  for ( i = 0; i<dptr[0].nelems ; i++ )
  {
    size += align_delta(size,dptr[i].align_type);
    size += dptr[i].nelems*dptr[i].elemsz;
  }
  shm_dt(dptr);

  return size; 
}
int
shm_destroy(char *name)
/******************************************************
 *  Destroy  dataset <name> if it exists
 *
 ******************************************************/
{
     
  if (shm_q(name)) /*it exists : destroy it */
  { 
    key_t             mykey;
    int               shmid,i;
    char              fullname[128];
    struct  shmid_ds  shmidstruc;
    struct  shmid_ds  *buf = &shmidstruc;

    nexpand(fullname,name);
    mykey = getkey(fullname);
    shmid = shmget(mykey,SHM_DEF_KEY_SIZE,0);
    if ((i = shmctl(shmid,IPC_RMID,buf)) != 0) 
      return 0;
  }
  return 1;
}

/*************************************************************************/
char  *
shm_attach(key_t sid, int flag)
/*************************************************************************/
{
  char      *shm_ptr;

  if ((shm_ptr = (char *)shmat(sid,(char *)0,flag)) == (char *)-1) 
    return 0;
  return shm_ptr;
}

key_t
getkey(char *fullname)
{
  key_t key;
  key = ftok(fullname,'c');
  if (key == -1) 
  {
    char temprpt[128];
    sprintf(temprpt,"Check existance of key file for shared memory:%s",
              fullname);
    sds_push_error(SDS_FILE_OP,SDS_ERROR,temprpt);
    return (key_t)0;
  }
  return key;
}
#else /* no shm - put in stubs */

#include <errno.h>
#include <stdio.h>

/******* forward declarations *****/
int        shm_back(char *,char *);
void       shm_dt(struct direc *);
int        shm_destroy(char *);
int        shm_die(void);

/* I have to put dummy shm routines in */
extern  int errno;
shmdt(void)
{ 
 errno = EINVAL;
 return 0;
}
int
shm_destroy(char *name)
{
 shm_die();
}
int
shm_q(char *name)
{
 return 0;
}

char *shm_make(void)
{
 shm_die();
}
int
shm_quit(struct direc *dptr)
{
 shm_die();
}

struct direc *shm_attr(char *name)
{
 shm_die();
}

struct direc *shm_attw(char *name)
{
 shm_die();
}
int
shm_unfile(void)
{
 shm_die();
}
int
shm_die(void)
{
 fprintf(stderr,"no shared mem on this machine yet\n");
 exit(1);
}
#endif
