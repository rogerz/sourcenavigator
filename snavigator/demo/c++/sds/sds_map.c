/* $Header$ */

#if defined(vms)
#include "sdsgen.h"
#else
#include "Sds/sdsgen.h"
#endif

extern int sds_error;

#ifndef MEMMAP /* mmap() not available for this architecture */

sds_handle
sds_map(source_name, access)
char *source_name;
sds_code access;
{
  fprintf(stderr, "mmap mapping routines not available for this machine\n");
  exit(1);
}

sds_handle
sds_adaptive_map(source_name, number, access, datafilenames, offsets, sizes)
char *source_name;
int number;
sds_code access;
char **datafilenames;
off_t *offsets;
off_t *sizes;
{
  fprintf(stderr, "mmap mapping routines not available for this machine\n");
  exit(1);
}

#else /* mmap() is available */

#include <string.h>
#include <stdlib.h>
#include <memory.h>

#ifndef vms
#include <unistd.h>
#endif

#include <malloc.h>
#include <sys/mman.h>

#include "Sds/sds_externs.h"

/*    forward declarations */
char * sds_mappit( char *,int,off_t *,off_t);

sds_handle
sds_map(source_name, access)
char *source_name; /* filename of source dataset */
sds_code access; /* SDS_READ,SDS_WRITE */
{
  struct sds_header *sdsh;
  char              *dir;
  char              *dataddress;
  sds_handle         sds;
  caddr_t            sds_header;
  off_t              sds_size;
   struct direc *dptr;
   struct sds_control_p *scp;

  sds = next_sds(); /* Any sds space left? */
  if (!(sds = next_sds()))
    return 0L;
  scp = sds_control(sds);

  if ((dataddress = sds_mappit(source_name, access, &sds_size, 0)) == NULL)
    return 0L;
  dptr = sds_direc(sds);
  dptr[0].illoca = SDS_DISJOINT_OBJECT;
  sds_header = (caddr_t)dataddress;

  sdsh = (struct sds_header *)sds_header;
  /* Check to see if the thing actually is an SDS for this architecture. If
  it is an SDS but for some other architecture, straight mapping will not
  help...we have to convert; so that is left to higher-level code invoking
  a conversion load to memory, for instance.
   */
  if (sdsh->magic_number != SDS_MAGIC)
  {
    if ((sdsh->magic_number & 0xffff00ff) == SDS_BASE_MAGIC)
      sds_error = SDS_WRONG_PADS;
    else if ((sdsh->magic_number & 0xff00ffff) == SDS_MAGIC_BYTESWAP)
      sds_error = SDS_SWAPPED_BYTES;
    else
      sds_error = SDS_NOT_SDS;
    munmap(sds_header, sds_size);
    sds_push_error(sds_error,SDS_WARNING, "Mapping dataset");
    return 0L;
  }

  /* OK, the object is the right sort. Find where the directory is and
  register with the SDS system variables.
   */
  dir = (char *)sdsh + BASE_OFFSET +
                        (int)sdsh->list_size + 
                        (int)sdsh->heap_size;

  set_sys_vars(sds,(struct direc *)dir);
  scp->source = SDS_MAPPED_MEM;

  return sds;
}

sds_handle
sds_adaptive_map(source_name, number, access, datafilenames, offset, size)
char         *source_name;
int           number;
sds_code      access;
char        **datafilenames;
off_t        *offset;
off_t        *size;
{
  char **dataddress;
  sds_handle    status;
  int           i;
  struct direc *dptr;
  sds_handle    sds = sds_access(source_name, SDS_FILE, access);
  int           NElems;
  int           allocated = 0;
  off_t        *sizes;
  off_t        *offsets;
  char         *filename;
  struct sds_control_p *scp;

  if (sds <= 0)
    return 0L;
  dptr = sds_direc(sds);
  NElems = dptr[0].nelems;

  dataddress = (char **)sds_malloc(NElems * sizeof(char *));
  if (number != NElems)
  {
    allocated = 1;
    sizes = (off_t *)sds_malloc(NElems * sizeof(int));
    offsets = (off_t *)sds_malloc(NElems * sizeof(int));
    memcpy(offsets,offset,number * sizeof(char *));
    memcpy(sizes,size,number * sizeof(int));
  }
  else
  {
    sizes = size;
    offsets = offset;
  }
 
  /* ...but the disjoint flag may not be there for older headers:
     in that case, all objects are mapped from one file 
   */
  if (number == 1 && datafilenames != NULL &&
   !strcmp(datafilenames[0], "Internal"))
  {
    /* we try to find that mapping file from a descritive object inside
    this header..
    */
    int size, date;
    if  ((filename = sds_searchmapfile(sds, &size, &date)) == NULL)
      return 0L;
    if (sds_mapfilecheck(filename, size, date) < 0)
      return 0L;
    dataddress[1] = sds_mappit(filename, access, &sizes[1],offsets[1]);
  }
  else if (number == 1 && datafilenames != NULL &&  datafilenames[0] != NULL)
  {
     dataddress[1] = sds_mappit(datafilenames[0], access,
                          &sizes[1],offsets[1]);
  }
  else
  {
    if (sizes[0] == 0)
    {
      sizes[0] = fsiz(source_name);
    }

    for (i=1;i<dptr[0].nelems;i++)
    {
      dataddress[i] = 0;
      if (i <= number && datafilenames[i] != NULL)
      {
        if ((dataddress[i] = sds_mappit(datafilenames[i],
                          access, &sizes[i],offsets[i])) == NULL)
        {
          sds_destroy(sds);
          if (allocated)
            free(sizes);
          free(dataddress);
          return 0L;
         }
         else
         {
           dptr[i].illoca = SDS_DISJOINT_OBJECT;
         }
       }
    }
  }
/* Now fill all the offsets, and scan variable size data if necessary */
  status = offil(sds, dataddress, NElems, sizes);
  scp = sds_control(sds);
  scp->source = SDS_MAPPED_MEM;

  if (allocated)
    free(sizes);
  free(dataddress);
  return sds;
}

char *
sds_mappit(source_name, access, size, offset)
char *source_name;
int access;
off_t *size,offset;
{
  char              *status;
  int                prot;
  int                share    = MAP_SHARED;
  int                fd;

    /* fsiz() does an fstat on the file & returns size
       in bytes, or -1 if the file does not exist. It
       does not check to see if the file IS an SDS   
     */
  if (*size == 0) /* I will try to find out from the file itself */
  {
    if ((*size = fsiz(source_name)) == -1) 
    {
      sds_push_error(SDS_NO_SUCH_SDS, SDS_ERROR," In mappit");
      return NULL;
    }
  }

  if (access == SDS_WRITE)
  {
    access = O_RDWR;
    prot = PROT_READ|PROT_WRITE;
  }
  else
  {
    access = O_RDONLY;
    prot = PROT_READ;
  }

  if ((fd = open(source_name, access, 0666)) == -1)
  {
    if (access == O_RDWR)
      fprintf (stderr,"open()'ing %s for read/write in sds_map(): ",
                      source_name);
    else
      fprintf (stderr,"open()'ing %s for read only in sds_map(): ",
                      source_name);
    perror(0);
    exit(1);
  }

  /*  Map the file (it exists and is accesible) into memory. Allow the
  system to choose where (returned in status); tell it how big, and the
  protection/access flags. 
   */
  if ((status = mmap (0, *size, prot, share, fd, offset)) == (caddr_t)-1 )
  {
    perror("mmap()'ing in sds_map()");
    exit(1);
  }
  close(fd);
  return status;
}

char *
sds_searchmapfile(sds, size, mod_date)
sds_handle sds;
int *size,*mod_date;
{
  sds_handle obind;
  int level;
  char *filename = NULL;
 struct sds_odesc *things;

  if (!(obind = sds_like2ind(sds,"DisjointFile", 1)))
 {
   sds_push_error(SDS_NO_MAP_FILE, SDS_WARNING,"Header contains no disjoint file info");
   return NULL;
 }
  while ((level = sds_describe(sds,obind,&things)) >= 0)
  {
    if (!strcmp(things[level].name, "filename"))
      filename = things[level].address;
    if (!strcmp(things[level].name, "mod_date"))
      *mod_date = *(int *)things[level].address;
    if (!strcmp(things[level].name, "size"))
      *size = *(int *)things[level].address;
  }
  return filename;
}

sds_handle
sds_mapfilecheck(filename, size, date) 
char *filename;
int size, date;
{
  struct stat databuf;
  char temprpt[128];
  *temprpt = (char )0;


  if (stat(filename, &databuf) == -1)
  {
    sds_push_error(SDS_FILE_RD,SDS_ERROR,"Map scan");
    return 0L;
  }
  if (databuf.st_mtime != date)
    sprintf(temprpt,"Mapped file %s has been modified since scan\n", filename);
  if (databuf.st_size != size)
    printf(temprpt,"Mapped file %s has changed size since scan\n", filename);
  if (*temprpt)
  {
    sds_push_error(SDS_FILE_RD,SDS_WARNING,temprpt);
    return 0L;
  }
  return 1L;
}

#endif  /* I do have mmap() available */
