/*======================================================================
 *                          Copyright (c) 1994
 *                       The Ohio State University
 *               Computer & Information Science Department
 *         Interactive Instructional Computing Facilities (IICF)
 * ======================================================================
 * Permission to use, copy, modify, and  distribute this software and its
 * documentation  for any  purpose  and  without  fee is hereby  granted,
 * provided that the above copyright notice appear in all copies and that
 * both that   the copyright notice  and  warranty  disclaimer appear  in
 * supporting  documentation,   and  that  the  names of   The Ohio State
 * University and  Interactive Instructional Computing Facilities as well
 * as any of   their entities not be  used  in advertising  or  publicity
 * pertaining to distribution of  the software without  specific, written
 * prior permission.
 * 
 * The Ohio State University disclaims all warranties with regard to this
 * software, including   all implied  warranties of   merchantability and
 * fitness.  In no event  shall The Ohio  State University be  liable for
 * any  special,   indirect  or consequential    damages or  any  damages
 * whatsoever resulting from  of use, data or  profits, whether in an
 * action of  contract, negligence or other  tortuous action, arising out
 * of or in connection with the use or performance of this software.
 * =====================================================================
 */
#ifndef TKTREETABLEH
#define TKTREETABLEH

#include <tk.h>
#include <tcl.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#ifdef __cplusplus
extern "C" {
#endif

/* tkWinDefault.h defines this, but tkUnixDefault.h doesn't.  */
#ifndef CTL_FONT
#define CTL_FONT "Helvetica -12"
#endif

/*
 * TableItem defaults
 */

#define NO_TABLEITEM_LINEWIDTH          "-1"
#define DEF_TABLEITEM_LINEWIDTH         "1"
#define DEF_TABLEITEM_TEXT              ""
#define DEF_TABLEITEM_DATA              (char *) NULL /*""*/
#define DEF_TABLEITEM_BITMAP            (char *) NULL
#define DEF_TABLEITEM_IMAGE             (char *) NULL
#define DEF_TABLEITEM_IDLECOMMAND       (char *) NULL
#define DEF_HIDDEN_COMMAND              ""
#define DEF_TABLEITEM_FONT              ""
#define DEF_TABLEITEM_TEXTFOREGROUND    ""
#define DEF_TABLEITEM_BITMAPFOREGROUND  ""
#define DEF_TABLEITEM_BITMAPBACKGROUND  NORMAL_BG
#define DEF_TABLEITEM_BITMAPSELECTFOREGROUND ""
#define DEF_TABLEITEM_BITMAPSELECTBACKGROUND ""
#define DEF_TABLEITEM_LINEFOREGROUND    ""
#define DEF_TABLEITEM_INDENT            "0"
#define DEF_TABLEITEM_PARENT            "-1"
#define DEF_TABLEITEM_SUCCNUM           "0"
#define DEF_TABLEITEM_SEENNUM           "0"
#define DEF_TABLEITEM_TABFREESPACE      "6"
#define DEF_TABLEITEM_FLAGS             "0"

/*
 * TreeTable defaults
 */

#define NO_TREETABLE_LINEWIDTH          "-1"
#define DEF_TREETABLE_LINEWIDTH         "1"
#define DEF_TREETABLE_BG_COLOR          NORMAL_BG
#define DEF_TREETABLE_BG_MONO           WHITE
#define DEF_TREETABLE_BORDER_WIDTH      "2"
#define DEF_TREETABLE_HIGHLIGHT_WIDTH   "2"
#define DEF_TREETABLE_CURSOR            ""
#define DEF_TREETABLE_EXPORT_SELECTION  "0"
#define DEF_TREETABLE_TEXTFG            BLACK
#define DEF_TREETABLE_BITMAPFG          BLACK
#define DEF_TREETABLE_BITMAPBG          NORMAL_BG
#define DEF_TREETABLE_LINEFG            BLACK
#define DEF_TREETABLE_SPLITLINEFG       "#c0c0c0"
#define DEF_TREETABLE_FONT              CTL_FONT
#define DEF_TREETABLE_FG                BLACK
#define DEF_TREETABLE_GEOMETRY          "20x10"
#define DEF_TREETABLE_WIDTH             "20"
#define DEF_TREETABLE_HEIGHT            "10"
#define DEF_TREETABLE_RELIEF            "flat"
#define DEF_TREETABLE_SELECT_COLOR      SELECT_BG
#define DEF_TREETABLE_SELECT_MONO       BLACK
#define DEF_TREETABLE_INDENTWIDTH       "10"
#define DEF_TREETABLE_BITMAPSPACE       "7"
#define DEF_TREETABLE_SELECT_BD         "1"
#ifdef _TKWINDEFAULT
#define DEF_TREETABLE_SELECT_FG_COLOR   SELECT_FG
#else
#define DEF_TREETABLE_SELECT_FG_COLOR   BLACK
#endif /* _TKWINDEFAULT */
#define DEF_TREETABLE_SELECT_FG_MONO    WHITE
#define DEF_TREETABLE_BITMAPSELECTBACKGROUND ""
#define DEF_TREETABLE_BITMAPSELECTFOREGROUND ""

#define DEF_TREETABLE_SET_GRID          "0"
#define DEF_TREETABLE_BESTFIT           "0"
#define DEF_TREETABLE_AUTOFIT           "0"
#define DEF_TREETABLE_SPLITLINES        "1"
#define DEF_TREETABLE_TRUNCATE          "1"
#define DEF_TREETABLE_TAKE_FOCUS        (char *) NULL
#define DEF_TREETABLE_SCROLL_COMMAND    ""
#define DEF_TREETABLE_TABS              (char *) NULL
#define DEF_TREETABLE_JUSTIFY           (char *) NULL
#define DEF_TREETABLE_DEFTABS           (char *) NULL
#define DEF_TREETABLE_SELECTMODE	"browse"
#define DEF_TREETABLE_FILL_SELECTION    "0"
#define DEF_TREETABLE_SORTED_INSERTION  "0"
#define DEF_TREETABLE_SORT_NOCASE       "0"
#define DEF_TREETABLE_SORT_COLNUM       "-1"
#define DEF_TREETABLE_USE_ACCELERATOR   "0"
#if _WINDOWS
#define DEF_TREETABLE_WINDOWSMODE       "1"
#else
#define DEF_TREETABLE_WINDOWSMODE       "0"
#endif
#define DEF_TREETABLE_TRUNCATEMETHODE   "auto"
#define DEF_TREETABLE_UNKNOWN_FLAG      "0"

#define ITEM_HIDDEN_SUBTREE		0x0001
#define ITEM_SELECTED			0x0002

#define MULTI				0
#define SINGLE				1

#define USE_PARENT_CACHE
#define USE_CACHE
#define MAX_CACHED			5

#undef USE_BITMAP_COLORS
#define USE_PATHFINDER
#define PATHFINDER_STEP 1024

/*
 * Flag bits for TreeTable
 * REDRAW_PENDING :            Non-zero means a DoWhenIdle handler 
 *                             has already been queued to redraw
 *                             this window.
 * UPDATE_V_SCROLLBAR :        Non-zero means verticle scrollbar needs
 *                             to be updated.
 * UPDATE_H_SCROLLBAR:         Non-zero means horizontal scrollbar needs
 *                             to be updated.
 * GOT_FOCUS:                  Non-zero means this widget currently
 *                             has the input focus.
 */

#define REDRAW_PENDING        1
#define UPDATE_V_SCROLLBAR    2
#define UPDATE_H_SCROLLBAR    4
#define GOT_FOCUS             8

/*
 * Function declaration
 */

int create_treetable_command _ANSI_ARGS_((Tcl_Interp *interp));

#ifdef __cplusplus
}
#endif

#endif
