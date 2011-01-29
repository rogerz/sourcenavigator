#ifndef _SN_COMPAT_H
#define _SN_COMPAT_H

#ifndef MAXPATHLEN
# define MAXPATHLEN 256
#endif

#ifndef MAXHOSTNAMELEN
# define MAXHOSTNAMELEN 256
#endif

#ifndef HAVE_PID_T
  typedef int pid_t;
#endif

#ifndef F_OK
# define F_OK 0
#endif

#if defined(__MINGW32__)
# include <winsock.h> /* Defines u_int and friends under Win32. */
#endif

#ifndef HAVE_UCHAR_T
typedef unsigned char	u_char;		/* 4.[34]BSD names. */
typedef unsigned int	u_int;
typedef unsigned long	u_long;
typedef unsigned short	u_short;
#endif

#ifndef O_NDELAY
# define O_NDELAY 0
/*# define O_NONBLOCK 0*/
#endif

#endif /* _SN_COMPAT_H */
