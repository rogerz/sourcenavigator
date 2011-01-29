/* 
 * dllEntryPoint.c --
 *
 *	This file implements the Dll entry point as needed by Windows.
 */

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
/* CYGNUS LOCAL */
#include <tcl.h>

/*
 * The following declaration is for the VC++ DLL entry point.
 */

BOOL APIENTRY		DllMain _ANSI_ARGS_((HINSTANCE hInst,
			    DWORD reason, LPVOID reserved));
/* END CYGNUS LOCAL */

/*
#if defined(_MSC_VER)
#   define DllEntryPoint DllMain
#endif
*/

#ifdef __CYGWIN32__

/* cygwin32 requires an impure pointer variable, which must be
   explicitly initialized when the DLL starts up.  */
struct _reent *_impure_ptr;
extern struct _reent *_imp__reent_data;
#endif /* __CYGWIN32__ */

/*
 *----------------------------------------------------------------------
 *
 * DllEntryPoint --
 *
 *	This wrapper function is used by Borland to invoke the
 *	initialization code for Tk.  It simply calls the DllMain
 *	routine.
 *
 * Results:
 *	See DllMain.
 *
 * Side effects:
 *	See DllMain.
 *
 *----------------------------------------------------------------------
 */

BOOL APIENTRY
DllEntryPoint(hInst, reason, reserved)
    HINSTANCE hInst;		/* Library instance handle. */
    DWORD reason;		/* Reason this function is being called. */
    LPVOID reserved;		/* Not used. */
{
    return DllMain(hInst, reason, reserved);
}

/*
 *----------------------------------------------------------------------
 *
 * DllMain --
 *
 *	DLL entry point.
 *
 * Results:
 *	TRUE on sucess, FALSE on failure.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

BOOL APIENTRY
DllMain(hInstance, reason, reserved)
    HINSTANCE hInstance;
    DWORD reason;
    LPVOID reserved;
{
/* CYGNUS LOCAL */
#ifdef __CYGWIN32__
    /* CYGNUS LOCAL */
    /* cygwin32 requires the impure data pointer to be initialized
       when the DLL starts up.  */
    _impure_ptr = _imp__reent_data;
    /* END CYGNUS LOCAL */
#endif
    
    return(TRUE);
}
