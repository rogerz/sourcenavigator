
#include "Sds/sdsgen.h"
#if defined(__MSDOS__)
#include <alloc.h>
#include <io.h>
#include <string.h>
#else
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#endif

#ifdef VXWORKS
#include <sys/fcntlcom.h>
#endif

int
sds_target_file(sds, filename)
sds_handle sds;
char *filename;
{
  struct sds_control_p *scp = sds_control(sds);
  int fd;

  if (scp->dptr == NULL)
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"Converting sds to targeted file");
    return 0;
  }
  if (scp->dptr[0].nelems > 1L)
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Converting sds to targeted file");
    return 0;
  }
  if ((fd = sds_open_file(filename, O_RDWR | O_CREAT)) == -1)
  {
    sds_push_error(SDS_FILE_OP,SDS_ERROR,"Converting sds to targeted file");
    return 0;
  }
  
  scp->tstream = fd;
  scp->target_name = (char *)malloc(strlen(filename) + 1);
  strcpy(scp->target_name, filename);
  return 1;
}

int
sds_load_object(sds,object,pointer)
sds_handle sds,object;
char *pointer;
{
  struct sds_control_p *scp = sds_control(sds);
  int fd = sds_open_file(scp->load_name,O_RDONLY);
  off_t off = scp->file_offsets[object];
  if (fd == -1)
  {
    sds_push_error(SDS_FILE_OP,SDS_ERROR,"Trying to read single object");
    return 0;
  }
  if (lseek(fd,off,SEEK_SET) == -1L)
  {
    sds_push_error(SDS_FILE_OP,SDS_ERROR,"Trying to lseek single object");
    close(fd);
    return 0;
  }
  read(fd,pointer,scp->dptr[object].nelems * scp->dptr[object].elemsz);
  close(fd);
  return 1;
}

