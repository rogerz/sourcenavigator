// $Header$

#ifndef remoteexec_h
#define remoteexec_h

#include "Executable.h"

class Channel;

class RemoteExec : public Executable {
    public:
	RemoteExec( Channel* daemon_channel,
		    const char* arg_executable, const char** argv );
	~RemoteExec();

	int Active();
	void Ping();

    protected:
	char* id;
	Channel* daemon_channel;
	};

#endif	/* remoteexec_h */
