#include <tk.h>
#include <tix.h>

int
My_AddTwoCmd(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    int num;
    char buf[30];

    if (argc != 2) {
	return Tix_ArgcError(interp, 1, argv, 1, "integer");
    }

    if (Tcl_GetInt(interp, argv[1], &num) != TCL_OK) {
	return TCL_ERROR;
    }

    sprintf(buf, "%d", num+2);
    Tcl_AppendResult(interp, buf, NULL);
    return TCL_OK;
}

int
My_SubTwoCmd(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    int num;
    char buf[30];

    if (argc != 2) {
	return Tix_ArgcError(interp, 1, argv, 1, "integer");
    }

    if (Tcl_GetInt(interp, argv[1], &num) != TCL_OK) {
	return TCL_ERROR;
    }

    sprintf(buf, "%d", num-2);
    Tcl_AppendResult(interp, buf, NULL);
    return TCL_OK;
}
