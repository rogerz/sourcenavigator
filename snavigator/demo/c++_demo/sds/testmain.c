
#include <Sds/sdsgen.h>

#include <stdio.h>

void
cprint(char *buf, int n)
{
  int i;
  for (i= 0;i < n; i++)
   putchar(*buf++);
  putchar('\n');
}

int
main()
{
  char buffer[0x4000];
  sds_handle sds;
  int size = 12,i = 0,j = 0;

  int fd = open("crap",O_RDONLY,0666);

  sds_init();
  sds_output_errors(SDS_WARNING);
  sds_output_proginfo(1);

  sds = sds_new("Blarf");

  /* sds_setmaxbuf(0x1000); */
  sds_clear_errors();

  cprint(j += sds_read(fd,size,0),size);
  cprint(j += sds_read(fd,size,0),size);
  cprint(j += sds_read(fd,size,0),size);
  size = 0x2000;
  cprint(j += sds_read(fd,size,buffer),40);
  size = 0x20;
  while (!sds_last_warning())
  {
    sds_read(fd,size,0);
	j += sds_returned( fd);
    i++;
  }
  printf("%d loops, last %d total %d\n",i,sds_returned(sds, fd),j);
  sds_dump_fdc(fd);
  sds_close_fd( fd);

  return 0;
}
