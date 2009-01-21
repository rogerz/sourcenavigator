#include <stdio.h>

int
main()
{
  char b[1024];
	b[0] = 0;
	while(fgets(b, 1024, stdin))
	{
		printf("testing> %s\n",b);
		fflush(stdout);
	}
}
