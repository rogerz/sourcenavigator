// $Header$

#ifndef executable_h
#define executable_h


// Searches PATH for the given executable; returns a malloc()'d copy
// of the path to the executable, which the caller should delete when
// done with.
char* which_executable( const char* exec_name );


class Executable {
    public:
	Executable( const char* arg_executable );
	virtual ~Executable();

	int ExecError()	{ return exec_error; }

	// true if the executable is still "out there"
	virtual int Active() = 0;
	virtual void Ping() = 0;

    protected:
	char* executable;
	int exec_error;
	int has_exited;
	};

#endif	/* executable_h */
