/*

Copyright (c) 2000, Red Hat, Inc.

This file is part of Source-Navigator.

Source-Navigator is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2, or (at your option)
any later version.

Source-Navigator is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along
with Source-Navigator; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA.



*/

#include <ctype.h>
#include "tclInt.h"

/*
 *----------------------------------------------------------------------
 *
 * sn_compare
 *
 *	This function compares two strings as if they were being used in
 *	an index or card catalog.  The case of alphabetic characters is
 *	ignored, except to break ties.  Thus "B" comes before "b" but
 *	after "a".  Also, integers embedded in the strings compare in
 *	numerical order.  In other words, "x10y" comes after "x9y", not
 *      before it as it would when using strcmp().
 *
 * Results:
 *      A negative result means that the first element comes before the
 *      second, and a positive result means that the second element
 *      should come first.  A result of zero means the two elements
 *      are equal and it doesn't matter which comes first.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
sn_compare(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
  int dummy;
  char *left, *right;
  unsigned char leftCh, rightCh;
  
  if (objc != 3)
    {
      Tcl_WrongNumArgs(interp, 1, objv, "string1 string2");
      return TCL_ERROR;
    }
  
  left = Tcl_GetStringFromObj(objv[1], &dummy);
  right = Tcl_GetStringFromObj(objv[2], &dummy);
  
  /*
   * If the tokens are different, then comparing them directly gives
   * the correct result.
   */
  
  if (*left == '~') {
    left++;
  }
  
  if (*right == '~') {
    right++;
  }
  
  for ( ; (*left != '\0') && (*right != '\0'); left++, right++) {
    if (*left == *right) {
      continue;
    }
    
    leftCh = toupper(*left);
    rightCh = toupper(*right);
    
    if (leftCh < rightCh) {
      Tcl_SetIntObj(Tcl_GetObjResult(interp), -1);
      return TCL_OK;
    } else if (rightCh < leftCh) {
      Tcl_SetIntObj(Tcl_GetObjResult(interp), 1);
      return TCL_OK;
    } else {
      /* `A' is less than `a'. */
      if (isupper(*left) && islower(*right)) {
	Tcl_SetIntObj(Tcl_GetObjResult(interp), -1);
	return TCL_OK;
      }
      /* `a' is greater than `A'. */
      if (islower(*left) && isupper(*right)) {
	Tcl_SetIntObj(Tcl_GetObjResult(interp), 1);
	return TCL_OK;
      }
    }
  }
  
  if (*left == '\0') {
    if (*right == '\0') {
      Tcl_SetIntObj(Tcl_GetObjResult(interp), 0);
    } else {
      Tcl_SetIntObj(Tcl_GetObjResult(interp), -1);
    }
  } else {
    Tcl_SetIntObj(Tcl_GetObjResult(interp), 1);
  }

  return TCL_OK;
}

