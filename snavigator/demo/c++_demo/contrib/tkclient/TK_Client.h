// $Header$

#ifndef tkclient_h
#define tkclient_h

extern "C" {
#include <tk.h>
}

#include "Glish/Client.h"

class TK_FileHandler;

typedef void (*ExitCallback)();
typedef void (*EventCallback)( GlishEvent* e );

class TK_Client : public Client {
public:
	TK_Client( int& argc, char** argv, char* name, char* display,
			EventCallback event_callback = 0,
			ExitCallback exit_callback = 0 );

	~TK_Client();

	Tcl_Interp* TCL()			{ return tcl; }
	Tk_Window TK()				{ return tk; }

	int TclError( const char* msg );

protected:
	friend TK_FileHandler;

	void FD_Change( int fd, bool add_flag );
	void AddFile( int fd );
	void FileCallback( int fd );

	static
	int SendEventCallback( ClientData cd, Tcl_Interp* tcl,
				int argc, char** argv );

	static
	int ReplyCallback( ClientData cd, Tcl_Interp* tcl,
				int argc, char** argv );

	static
	int TclExitCallback( ClientData cd, Tcl_Interp* tcl,
				int argc, char** argv );

	Tcl_Interp* tcl;
	Tk_Window tk;
	char tcl_buf[1024];

	EventCallback event_callback;
	ExitCallback exit_callback;
};

#endif	/* tkclient_h */
