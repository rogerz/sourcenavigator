/* $Header$ */

#ifndef glish_event_h
#define glish_event_h


/* Glish inter-client protocol version. */
#define GLISH_CLIENT_PROTO_VERSION 3


/* Maximum size of an event name, including the final '\0'. */
#define MAX_EVENT_NAME_SIZE 32


struct event_header
	{
#define GLISH_REQUEST_EVENT 0x1
#define GLISH_REPLY_EVENT 0x2
#define GLISH_STRING_EVENT 0x4
#define GLISH_OPAQUE_EVENT 0x8
	u_long flags;

	u_long event_length;

	char event_name[MAX_EVENT_NAME_SIZE];
	};

#endif /* glish_event_h */
