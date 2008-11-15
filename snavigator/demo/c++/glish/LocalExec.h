// $Header$

#ifndef localexec_h
#define localexec_h

#include "Executable.h"


class LocalExec : public Executable {
    public:
	LocalExec( const char* arg_executable, const char** argv );
	LocalExec( const char* arg_executable );
	~LocalExec();

	int Active();
	void Ping();

	int PID()	{ return pid; }

    protected:
	void MakeExecutable( const char** argv );

	int pid;
	};

#endif	/* localexec_h */
