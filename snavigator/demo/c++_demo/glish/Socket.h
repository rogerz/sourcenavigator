// $Header$

#ifndef Socket_h
#define Socket_h

class Socket {
    public:
	// The first parameter, if true, specifies that the socket
	// will only be used locally (same host).  The second, if
	// present, means "the socket's already been created, here's
	// its fd".
	Socket( int is_local, int socket_fd = -1 );
	~Socket();

	int FD()	{ return fd; }
	int Port()	{ return port; }
	int IsLocal()	{ return is_local; }

    protected:
	void Gripe( char* msg );

	int fd;
	int port;
	int is_local;
	};

class AcceptSocket : public Socket {
    public:
	// The first parameter, if true, specifies that the socket
	// will only be used locally (same host).  The second gives
	// the port number at which to begin searching for a free
	// port.  If the third parameter is false, then the second
	// parameter is *not* a hint, but a requirement; if the
	// particular port is not available, then a subsequent call
	// to Port() will return 0 (and the AcceptSocket should be
	// deleted).
	AcceptSocket( int is_local = 0, int port_hint = 3000,
			int is_a_hint = 1 );

	Socket* Accept();
	};

#endif	/* Socket_h */
