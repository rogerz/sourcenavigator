/*
 This is a minimal uio.h intended for SN's win32 build unter MinGW.
 No warranty, use at your own risk.
*/

#ifndef __SYS_UIO_H
#define __SYS_UIO_H
struct iovec {
	void *iov_base;
	unsigned long iov_len;
};
#endif

