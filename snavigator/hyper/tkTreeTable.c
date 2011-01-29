/*
 * tkTreeTable.c 
 *
 * =====================================================================
 *                          Copyright (c) 1997
 *                       Cygnus Multix Software GmbH
 *                        Autor: Khamis Abuelkomboz
 *
 * This tool is modified to mirror a realy full tree. The procedures are
 * modified for quickly viewing, selecting, scrolling etc.
 * This tool is faster than the standard list of Tcl/Tk and is still
 * fast, when more items added to the tree table (100 000 and more),
 * when at least not a linear list is mirrored (without using accelerator)
 * and the cache is used.
 * Supported Feautures:
 * Selection:
 *     Fully compatible to the listbox widget, with "see", "activate", ...
 * Movement:
 *     Fully compatible to the listbox.
 * Toggle: 
 *     A sub tree can be toggled to view/hide it.
 * Images: -image
 *     This is modified to support images too.
 * Tab positions:
 *     With the flag "-tabs" or "-columns" you can add tab stops to
 *     place text on this tab stops. You must give the tab size and
 *     not the x-position like text widget. I think it's better so.
 * Search:
 *     compatible to the text widget and supports extra flag "-begins" to
 *     search only a number of characters.
 * Sort:
 *     This is a usefull method to resort the tree. you can give the
 *     column number to sort items on this. The good thing here, that
 *     every sub tree is sorted as local list and the tree will be not
 *     damaged after sorting, only indeces are changed.
 * Fullname:
 *     with the method 'fullname' you can get the full tree name from
 *     the item back to it's latest root.
 * Truncate:
 *     With this flag you can let the widget cut the rest of string
 *     in a tab stop, if the string is too long for it.
 * Fit States:
 *     -bestfit:
 *         Change tab stop sizes so that all texts fit in there tab stops.
 *     -autofit
 *         Change tab stop sizes so that the current displayed texts fit
 *         in there tab stops.
 * Toggle viewing a column:
 *     column toggle/view/hide column#:
 *         it's possible to hide a column in the tab stop list.
 *         when a cloumn is hidden, there is no text displayed
 *         on it's position and the tab stop size is changed to the value
 *         in 'tabsMinSpace'
 * Accelerator (Pathfinder):
 * Now the treetable is perfect in it's performance. It uses an accelerator
 * to speed up the display and finding an item, especialy when the treetable
 * contains some thouthends or some hundert thouthend of entries. It will
 * still be fast (too fast) like a russan atom roket.
 *
 * The treetable as 'a tree' is fast without this accelerator. But when we
 * use this treetable as list, the entries are localized by linear searching,
 * this is exactly the problem because the display and selection manipulating
 * (without accelerator) will be too slow for alot of entries.
 * That's meen this accelerator works only for the items on the first level
 * (root).
 *
 * This accelerator is disabled by default and must be enabled with
 * '-accelerator 1'
 *
 * It is different as the built in cache. The used cache is only the latest
 * handled items in the treetable.
 *
 * Color managment:
 *     Supports a wide level of selection/fore-, background and bitmap colors.
 *
 * Speed of Treetable:
 * n     = number of all items in the tree
 * depth = depth of the longest sub tree
 * nr    = number of root items (without parents)
 * ni    = max. number items of sons in a sub tree (only one level)
 * ns    = max. number of all items in a sub tree
 *
 * TreeTable without CACHE and without Accel.    Regular linear list
 *          One Item        All items:           One Item:  All items
 * Search:  O(nr + ni)      O(n * (nr + ni))     O(n)       O(n^2)
 * Insert:  O(nr + ni)      O(n * (nr + ni))     O(n)       O(n^2)
 * Delete:  O(nr + ni + ns) O(n * (nr + ni))     O(n)       O(n^2)
    0-end:  -               O(n)                 -          O(n)
 * Display: O(nr + ni)      O(n)                 O(n)       O(n)
 * Select:  O(nr + ni)      O(n)                 O(n)       O(n)
 * Toggle:  O(nr + ni)      -                    -          -   
 * Sort:    -               O(n^2)               -          -
 *
 * Speed of Treetable with accelerator:
 * a = accelerator step
 * Search/Insert/Delete/Display/Select/Toggle: O(nr / a + ni)
 *
 * As example a = 1000, nr = 300,000, ni = 0 (a list):
 * O(300,000/1000 + 0) = 300
 *
 **************************************************************************/

#include <config.h>

#include "tclInt.h"
#include "tkInt.h"

#include <ctype.h>

#ifndef HAVE_STRNCASECMP_DECL
extern int strncasecmp ();
#endif

/*#include "tkConfig.h"*/
#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "default.h"
#include "tkTreeTable.h"

#ifndef IS_ROOT
#if _WINDOWS
#define IS_ROOT(c) ((c) == '/' || (c) == '\\')
#else
#define IS_ROOT(c) ((c) == '/')
#endif
#endif

#define SPACE_PROBLEM

#define CONFIG_OPTIONS_ONLY	0x4000

#define ITEM_HIDDEN     -1
#define ITEM_VIEWED		-2
#define ITEM_UNKNOWN    -3
#define ITEM_TEXT       -4
#define ITEM_NOT_FOUND  -5

#define ACTIVE_LINE_HEIGHT		   1

enum {
	TRUNCATE_AUTO,
	TRUNCATE_PATH,
	TRUNCATE_NORMAL
};

#define Min(a,b) (a<b ? a : b)
#define Max(a,b) (a>b ? a : b)

/*
 * search for a char. in a string and return it's pointer, otherwise NULL
 */
#define NextChar(q, p, c, idx) \
		q = p; idx = 0; \
		while (*q != c && *q) \
			q++, idx++; \
		if (*q == 0) \
			q = NULL

#ifdef USE_CACHE
#define RET_CACHED_INDEX(itemPtr) \
{ \
	int i = 0; \
	while (i<MAX_CACHED) \
	{ \
		if (tablePtr->cachedItem[i] == itemPtr) \
		{ \
			return tablePtr->cachedPos[i]; \
		} \
		i++; \
    } \
}
	
#define VIEW_CACHE() \
{ \
	int i = 0; \
	while (i<MAX_CACHED) \
	{ \
		fprintf (stderr, "cache[%i]=(%i, %s)\n", i, \
				 tablePtr->cachedPos[i], \
				 tablePtr->cachedItem[i] ? tablePtr->cachedItem[i]->text : "NULL"); \
				 i++; \
	} \
}
	
#define SET_CACHED(item, pos) \
{ \
	if (pos >= 0) \
	{ \
		if ( \
		 tablePtr->cachedItem [0] != item && \
	     tablePtr->cachedItem [1] != item && \
	     tablePtr->cachedItem [2] != item && \
	     tablePtr->cachedItem [3] != item && \
	     tablePtr->cachedItem [4] != item && \
	     tablePtr->cachedItem [5] != item && \
	     tablePtr->cachedItem [6] != item && \
	     tablePtr->cachedItem [7] != item \
	     ) \
		{ \
			tablePtr->cachedPos [tablePtr->cacheIndex] = pos; \
			tablePtr->cachedItem[tablePtr->cacheIndex] = item; \
			tablePtr->cacheIndex = (tablePtr->cacheIndex+1) % MAX_CACHED; \
		} \
	} \
}

#define RET_CACHED_ITEM(index) \
{ \
    int i = 0; \
	while (i<MAX_CACHED) \
	{ \
		if (tablePtr->cachedItem[i] != NULL && tablePtr->cachedPos[i] == index) \
		{ \
			return tablePtr->cachedItem[i]; \
		} \
		i++; \
	} \
}

#define GET_CACHED_ITEM(item, index) \
{ \
    int ii=0; \
    while (ii<MAX_CACHED) \
	{ \
		if (tablePtr->cachedItem[ii] != NULL && tablePtr->cachedPos[ii] == index) \
		{ \
			item = tablePtr->cachedItem[ii]; \
		} \
		ii++; \
	} \
}

#define FREE_CACHE() \
{ \
    int i=0; \
    while (i<MAX_CACHED) \
	{ \
		tablePtr->cachedItem[i] = NULL; \
		tablePtr->cachedPos [i] = -1; \
		i++; \
	} \
    tablePtr->cacheIndex = 0; \
}

#define FREE_RONG_CACHE(index) \
{ \
    int i=0; \
    while (i<MAX_CACHED) \
	{ \
		if (tablePtr->cachedPos [i] >= index) \
		{ \
			tablePtr->cachedItem[i] = NULL; \
			tablePtr->cachedPos [i] = -1; \
		} \
		i++; \
	} \
}
#else
#define RET_CACHED_INDEX(itemPtr)
#define SET_CACHED(itemPtr, pos)
#define RET_CACHED_ITEM(index)
#define GET_CACHED_ITEM(itemPtr, index)
#define FREE_RONG_CACHE(index)
#define FREE_CACHE()
#endif

#ifdef USE_PARENT_CACHE
#define FREE_PARENT_CACHE() \
    tablePtr->cachedParent = -5; \
    tablePtr->cachedParentPtr = NULL
#define FREE_RONG_PARENT_CACHE(num) \
    if (tablePtr->cachedParent >= num) \
	{\
		tablePtr->cachedParent = -5; \
		tablePtr->cachedParentPtr = NULL; \
	}
#define GET_CACHED_PARENT(tablePtr, index) \
    ((tablePtr->cachedParentPtr != NULL && tablePtr->cachedParent == index) \
	? tablePtr->cachedParentPtr \
	: TreeTableFindItem (tablePtr, index))
#define SET_PARENT_CACHE(Ptr, num) \
	    tablePtr->cachedParentPtr = Ptr, \
	    tablePtr->cachedParent    = num
#else
#define GET_CACHED_PARENT(tablePtr, index)
#define FREE_PARENT_CACHE()
#define FREE_RONG_PARENT_CACHE(num)
#define SET_PARENT_CACHE(Ptr, num)
#endif


#define TREETABLE_WIDTH(tablePtr)  (Tk_Width(tablePtr->tkwin) \
			- 2*(tablePtr->inset+tablePtr->selBorderWidth))
#define TREETABLE_NUM_LINES(tablePtr) \
		((Tk_Height(tablePtr->tkwin)-2*(tablePtr->borderWidth))) \
    		/ tablePtr->lineHeight;

/*
 * Treetable item coordinates
 */
#define ITEM_X(Ptr) (tablePtr->inset \
                     - tablePtr->xOffset \
                     + Ptr->indent*tablePtr->indentWidth)
/* the index in the following command is the virtual position and not the
 * realy position
 */
#define ITEM_Y(Ptr, vindex) (((vindex - tablePtr->topIndex) * tablePtr->lineHeight) \
				+ tablePtr->inset \
				+ (tablePtr->lineHeight-Ptr->bitmapHeight)/2)

#define ITEM_HEIGHT()          tablePtr->lineHeight
#define ITEM_BITMAP_WIDTH(Ptr) (Ptr->bitmapWidth + tablePtr->bitmapSpace)
#define ITEM_TEXT_WIDTH(Ptr)   tk_TextWidth (((Ptr->fontPtr != NULL) \
					? Ptr->fontPtr : tablePtr->defFontPtr, \
					Ptr->text, Ptr->textLength)
#define ITEM_WIDTH(Ptr)        Ptr->lineWidth

#define ITEM_TEXT_INDENT(itemPtr) (tablePtr->inset \
					+ itemPtr->bitmapWidth \
					+ tablePtr->bitmapSpace \
					+ itemPtr->indent*tablePtr->indentWidth)
#define ITEM_TEXT_X(itemPtr)	(ITEM_TEXT_INDENT(itemPtr)-tablePtr->xOffset)

#ifdef USE_PATHFINDER
#define FIND_IN_PATHFINDER(pos, retPtr, retPos) \
    if (tablePtr->pathFinder) \
	{ \
    	PathFinder_t *pf; \
		for (pf=tablePtr->pathFinder->next; pf; pf=pf->next) \
		{ \
			if (pf->itemPos < pos) \
			{ \
				retPos = pf->itemPos; \
				retPtr = pf->itemPtr; \
			} \
		} \
	}
#define DELETE_FROM_PATHFINDER(pos) \
    if (tablePtr->pathFinder) \
	{ \
		PathFinder_t *pf, *pf1=tablePtr->pathFinder; \
		for (pf=pf1->next; pf; pf=pf->next) \
		{ \
		  if (pf->itemPos >= pos) \
		{ \
			  pf1->next = NULL; \
			  for (pf1=pf; pf1; ) \
			{ \
				  pf = pf1; \
				  pf1=pf1->next; \
				  ckfree ((char*)pf); \
			} \
			  break; \
		} \
		  pf1 = pf; \
		} \
	}
#else
#define FIND_IN_PATHFINDER(pos, retPtr, retPos)
#define DELETE_FROM_PATHFINDER(pos)
#endif

#define TreeTableRedrawRange(tablePtr) \
    if ((tablePtr->tkwin != NULL) && Tk_IsMapped(tablePtr->tkwin) \
		&& ! (tablePtr->flags & REDRAW_PENDING)) \
	{ \
		Tk_DoWhenIdle(DisplayTreeTable, (ClientData) tablePtr); \
		tablePtr->flags |= REDRAW_PENDING; \
	}
    
#define Have_PlusMinus(tablePtr) (tablePtr->plusImage != NULL && tablePtr->minusImage != NULL)
#define Have_Unknown(itemPtr) (itemPtr->unknownFlag)

/*
 * This is the data kept for each line in the treetable.
 */
typedef struct TableItem
{
	short bitmapHeight;		/* Height of the bitmap */
	short bitmapWidth;		/* Width of the bitmap */
	
	short indent;			/* Indentation level for the line */
	short lineWidth;		/* Width of the line joining this item to
							 * it's ancestor, measured in pixels. */
	short flags;
	short fontHeight;		/* Height of the font -- includes ascent
							 * and descent */
	
	Pixmap bitmap;		/* Bitmap to display for the line */
	
	int textLength;		/* number of chars in text label */
	int parent;			/* This is the index for the parent 
						 * of this node. */
	int succNum;		/* Number of all sons, in sub trees too */
	int seenNum;		/* seen item number of sub tree */
	
	int unknownFlag; /* Marks that the item COULD have sons in the future */
	
	GC textGC;			/* Graphics context to use for drawing the
						 * text to the screen */
	GC bitmapGC;		/* Graphics context to use for draing the
						 * bitmap to the screen */
	
	char *text;			/* Pointer to the text label string.  If 
						 * NULL, then there is no label for
						 * the line */
	char *data;			/* Pointer to client data to store
						 * private informations, like flags */
	
	Tk_Image image;		/* we can load images */
	char *imageString;		/* image name */
	
	Tk_Font fontPtr;		/* Font pointer to use for text */
	XColor *textFgColor;	/* Color to write text in */
	
#ifdef USE_BITMAP_COLORS
	GC bitmapSelectGC;           /* GC to use for bitmap when item selected */
	GC lineGC;			 /* Graphics context for the line */
	XColor *bitmapFgColor;	 /* foreground color for bitmap */
	XColor *bitmapBgColor;	 /* background color for the bitmap */
	XColor *bitmapSelectFgColor; /* Bitmap Fg color when item selected */
	XColor *bitmapSelectBgColor; /* Bitmap Bg color when item is selected */ 
	XColor *lineFgColor;	 /* line color */
#endif
	
	struct TableItem *succPtr;	 /* Sons of the tree */
	struct TableItem *parentPtr; /* Pointer to the TableItem which is our
								  * ancestor, or NULL if we don't have a
								  * parent. */
	struct TableItem *nextPtr;	 /* Pointer to the next item in the table */
} TableItem;

#ifdef USE_PATHFINDER
/*
 * This pathfinder is used to accelerate the display routine,
 * exactly when a single list has too many entries (> 20t).
 * The Pathfinder saves 'stop stations' every a number of
 * items in the root list (every 1t as example).
 * So, when an item is searched, numbered 25t) we will find
 * an previous item on the position 20t (in 20 Steps ansted of 20t steps)
 * and we continue normal searching in the left 500 entries, this costs
 * only time of 20+500 = 520 ansted of 20t.
 *
 * The path is built by the treetable routines, as example
 * display procedure, insertion, deleting ans so on.
 * The Path is usually refreshed by deleting or adding an item.
 */
typedef struct PathFinder_t
{
    TableItem* itemPtr;
	int itemPos;
    int seenPos;
    struct PathFinder_t * next;
} PathFinder_t;
#endif

/*
 * Tk has problem with alot of references to the same file
 * and it take a long time to delete the references of the
 * image (exponential)
 *
 * Here is the solution for this problem, every accessed
 * image is added to the tree.
 */
typedef struct ImageList_t {
    char *name;		/* Image name */
    Tk_Image image;		/* Image object */
    int width, height;
    struct ImageList_t *next;
} ImageList_t;

/*
 * Data structure used for each widget of type TreeTable in
 * the interpreter.  (This a bastardization of tkListbox.c)
 */
typedef struct
{
    Tk_Window tkwin;            /* Window that embodies the TreeTable. NULL
								 * means that the window has been destroyed
								 * but the data structures haven't yet been
								 * cleaned up. */
	Display *display;           /* Display containing the widget.  Used, among
								 * other things, so that resources can be freed
								 * even after tkwin has gone away. */
    Tcl_Interp *interp;         /* Interpreter associated with this widget */
    Tcl_Command widgetCmd;	/* Token for listbox's widget command. */
    int numItems;               /* Size of table for the widget */
    int numViewed;              /* number of viewed items, not hidden sub trees */
    TableItem *itemPtr;         /* Pointer to table items -- NULL if there 
                                 * are none. */
    int UseAccelerator;		/* use pathfinder */
#ifdef USE_PATHFINDER
    PathFinder_t *pathFinder;	/* To find elements in the middle of the
								 * item list of the treetable, when we
								 * have alot of items (more than 5000) as
								 * root elements
								 * This is requiered for fast displaying the
								 * list.
								 */
#endif

    ImageList_t *Images;	/* list of all used images */
    int ImagesCount;
    
#ifdef USE_CACHE
    TableItem *cachedItem[MAX_CACHED];	/* Heuristec cache */
    int cachedPos[MAX_CACHED];
    int cacheIndex;			/* points to latest cache position */
#endif
#ifdef USE_PARENT_CACHE
    int cachedParent;
    TableItem *cachedParentPtr;
#endif
    
    /*
     * Information used when displaying the widget :
     */
    
    Tk_3DBorder normalBorder;   /* Used for drawing border around the whole
								 * window, plus used for background */
    int borderWidth;            /* Width of 3-D border around window */
    int relief;                 /* 3-D effect : TK_RELIEF_RAISED, etc... */
    int highlightWidth;         /* Width in pixels of highlight to draw
								 * around widget when it has the focus.
								 * <= 0 means don't draw a highlight */
    XColor *highlightBgColor; 
	/* Color for drawing traversal highlight
	 * area when highlight is off. */
    XColor *highlightColor;     /* Color for drawing traversal highlight. */
    int inset;                  /* Total width of all borders, including
								 * traversal highlight and 3-D border.
								 * Indicates how much interior stuff must
								 * be offset from outside edges to leave
								 * room for borders. */
    Tk_Font defFontPtr;		/* Information about text font, or NULL. */
    int defFontHeight;          /* Height of the default font -- includes
								 * ascent and descent */
    Tk_3DBorder selBorder;      /* Borders and backgrounds for selected
								 * items. */
    int selBorderWidth;         /* Width of border around selection */
    int defLineWidth;           /* Default width of child-parent lines */
    XColor *defTextFgColor;     /* Default foreground color for text */
    XColor *defBitmapFgColor;   /* Default foreground for bitmaps */
    XColor *defBitmapBgColor;   /* Default background for bitmaps */
    XColor *defBitmapSelectFgColor;
    XColor *defBitmapSelectBgColor;
    XColor *defLineFgColor;     /* Default foreground color for lines */
    XColor *selFgColorPtr;      /* Foreground color for select items */
    GC selectGC;                /* For drawing selected items */
    GC defTextGC;               /* Default graphics context for text */
    GC defBitmapGC;             /* Default graphics context for bitmaps */
    GC defBitmapSelectGC;       /* Default graphics context for bitmaps when
								 * selection is active for the line */
    GC defLineGC;               /* Default graphics context for lines */
    char *geometry;             /* Desired geometry for window. Malloc'ed */
    int width, height;          /* another possibility to configure treetable size */
    int lineHeight;             /* Number of pixels allocated for each line 
								 * in the display (tallest out of fonts
								 * or bitmaps */
    int bitmapSpace;            /* Width of empty space between the bitmap
								 * of a line and the text associated with it,
								 * measured in pixels. */
    int indentWidth;            /* Indentation level expressed as number 
								 * of pixels from the left side of the
								 * widget. */
    int topIndex;               /* Index of topmost item in the window */
    int numLines;               /* Number of lines(items) that fit
								 * in window at one time */
    int setGrid;                /* Non-zero means pass gridding information
								 * to window manager */
    
    /*
     * Image to display when sub tree is hidden
     */
    Tk_Image plusImage;
    char *plusImageString;
    int plusWidth;
    int plusHeight;
    
    /*
     * Image to display when sub tree is explored
     */
    Tk_Image minusImage;
    char *minusImageString;
    int minusWidth;
    int minusHeight;
    
    /*
     * Image to display when the state is unknown
     */
    Tk_Image unknownImage;
    char *unknownImageString;
    int unknownWidth;
    int unknownHeight;
    
    Pixmap hiddenBitmap;	/* Bitmap to display when sub tree is hidden */
    Tk_Image hiddenImage;	/* Image to display when sub tree is hidden */
    char *hiddenImageString;
    int hiddenWidth;		/* Bitmap to display when sub tree is hidden */
    int hiddenHeight;		/* Bitmap to display when sub tree is hidden */
#if 0
    char * hiddencommand;	/* command to execute when a sub tree is toggled */
#endif
    char * idlecommand;	/* command to execute */
	
    /*
     * Information to support horizontal scrolling 
     */
    int maxWidth;               /* Width (in pixels) of widest line in the
								 * widget. */
    int xScrollUnit;            /* Number of pixels in one "unit" for
								 * horizontal scrolling (window scrolls
								 * horizontally in increments of this size).
								 * This is an average character size. */
    int xOffset;                /* The left edge of each line in the treetable
								 * is offset to the left by this many
								 * pixels (0 means no offset, positive 
								 * means there is an offset). */
	
    /*
     * Information for scanning:
     */
	
    int scanMarkX;              /* X-position at which scan started (e.g.
                                 * button was pressed here). */
    int scanMarkY;              /* Y-position at which scan started (e.g.
                                 * button was pressed here). */
    int scanMarkXOffset;        /* Value of "xOffset" field when scan
                                 * started. */
    int scanMarkYIndex;         /* Index of line that was at top of window
                                 * when scan started. */
	
	
    
    /*
     * Information about what's selected, if any
     */
    TableItem *lastSelected;
    int numSelected;            /* selected number of items */
    int selectFirst;            /* Index of first selected item (-1 means
								 * nothing selected). */
    int selectAnchor;           /* Line to which the selection is anchored */
    int exportSelection;        /* Non-zero means tie internal value
								 * to X selection. */
    int fillSelection;          /* Non-zero means the selection box will
								 * expand to max. width of window and not
								 * only text region */
    int active;                 /* Index of "active" element (the one that
								 * has been selected by keyboard traversal)
								 * -1 means none. */
    int   selectMode;           /* MULTI or SINGLE */
    char* selectModeStr;        /* multi or single */
	
    /*
     * we support tabs too
     */
    int * defTabs;			/* converted defualt tab stop list */
    char *defTabsList;		/* Tcl List with defualt tab stops */
    
    char *tabsList;		/* Tcl List with tab stops */
    int * tabs;			/* converted tab list */
    int tabsNum;		/* number of tabs */
    int tabsMinSpace;		/* Min. Space between tow tab stops */
    int *tabsHidden;		/* tabs that are hidden */
    
    /*
     * We support right-justify too
     */
    char *justify;
    int *tabsJustify;
    
    /*
     * Miscellaneous information ;
     */
    Cursor cursor;              /* X cursor for the window, or None. */
    char *takeFocus;            /* Value of -takefocus option;  not used in
								 * the C code, but used by keyboard traversal
								 * scripts.  Malloc'ed, but may be NULL */
    char *yScrollCmd;           /* Command prefix for communicating with
								 * vertical scrollbar.  NULL means no
								 * command to issue.  Malloc'ed. */
    char *xScrollCmd;           /* Command prefix for communicating with
								 * horizontal scrollbar.  NULL means no
								 * command to issue.  Malloc'ed */
    char *ResizeCmd;            /* Command prefix for communicating with
                                 * the column headers (tabs)
                                 */

    int flags;                  /* Various flag bits:  see below for
								 * definitions. */
    int sortedInsertion;        /* if this flag is seted, the items will be inserted
								 * on the correct place */
    int sortNoCase;		/* to ignore upper and lower case by comparing
						 * to place items correctly */
    int sortColumn;		/* insert items sorted using the strin in the
						 * tab stop of the given number, default "-1" */
    int pressX, pressY;
    
    int BestFit;		/* Best fit for tab stops */
    int AutoFit;		/* Change Tab Stop Size of the current
						 * displayed items so that we have best
						 * fit, this flag overrites 'BestFit' */
    int Truncate;	/* Truncate the rest of string */
    int SplitLines;
    
    XColor *defSplitLineFgColor; /* Default foreground color for split lines */
    GC defSplitLineGC;           /* Default graphics context for split lines */
    
    int nativeWindowsMode;       /* Convert "/" to "\\" on windows */
    int TruncateMethode;          /* How to truncate column strings */
    char *TruncateMethodeStr;
    int oldHeight;              /* Height before last resize. */
    int oldWidth;               /* Width before last resize. */
} TreeTable;

static int  TreeTableCmd _ANSI_ARGS_((ClientData clientData,
									  Tcl_Interp *interp,
									  int argc, Tcl_Obj*argv[]));
static int  TreeTableWidgetCmd _ANSI_ARGS_((ClientData clientData,
											Tcl_Interp *interp,
											int argc, Tcl_Obj *argv[]));
static void DestroyTreeTable _ANSI_ARGS_((char *memPtr));
static int  ConfigureTreeTable _ANSI_ARGS_((TreeTable *tablePtr,
											int argc, Tcl_Obj *objv[],
											int flags));
static void DisplayTreeTable _ANSI_ARGS_((ClientData clientData));
static int  TreeTableFindIndex _ANSI_ARGS_((TreeTable *tablePtr,
											TableItem *itemPtr));
static TableItem * TreeTableFindItem _ANSI_ARGS_((TreeTable *tablePtr, int index));
static int  ConfigureTreeTableItem _ANSI_ARGS_((TreeTable *tablePtr,
												TableItem *itemPtr,
												int argc, char **argv,
												int flags,
												int mode));
static TableItem* TreeTableInsertItem _ANSI_ARGS_((TreeTable *tablePtr,
												   int index,
												   TableItem* prevPtr,
												   int argc,
												   char **argv));
static int  TreeTableInsertItems (TreeTable* tablePtr,
								  int index, Tcl_Obj *itemlist,
								  int argc, Tcl_Obj *argv[]);
static void TreeTableDeleteRange _ANSI_ARGS_((TreeTable *tablePtr,
											  int start, int end));
static int  TreeTableRemoveItem _ANSI_ARGS_((TreeTable *tablePtr,
											 TableItem *itemPtr,
											 int lineNum,
											 int* width,
											 int original_item,
											 int children));
static void TreeTableFreeItem _ANSI_ARGS_((TreeTable *tablePtr,
										   TableItem *itemPtr));
static void TreeTableEventProc _ANSI_ARGS_((ClientData clientData,
											XEvent *eventPtr));
static int  GetTreeTableIndex _ANSI_ARGS_((TreeTable *tablePtr,
										   char *string, int endAfter,
										   int *indexPtr));
static void ChangeTreeTableView _ANSI_ARGS_((TreeTable *tablePtr,
											 int index, int redraw));
static void ChangeTreeTableOffset _ANSI_ARGS_((TreeTable *tablePtr,
											   int offset));
static void TreeTableScanTo _ANSI_ARGS_((TreeTable *tablePtr,
										 int x, int y));
static int  NearestTreeTableItem _ANSI_ARGS_((TreeTable *tablePtr,
											  int y));
static void TreeTableComputeLineHeight _ANSI_ARGS_((TreeTable *tablePtr, TableItem*thisPtr, int cnt));
static void TreeTableComputeWidths _ANSI_ARGS_((TreeTable *tablePtr,
												TableItem *itemPtr,
												int checkAll));

static int  TreeTableFetchSelection _ANSI_ARGS_((ClientData clientData,
												 int offset, char *buffer,
												 int maxBytes));
static void TreeTableLostSelection _ANSI_ARGS_((ClientData clientData));
static void TreeTableUpdateVScrollbar (TreeTable *tablePtr);
static void TreeTableUpdateHScrollbar (TreeTable *tablePtr);

static int  TreeTableIndex (TableItem *itemPtr, int index, int pos, TableItem **itemRet);
static int  TreeTableInsertListItem (TreeTable* tablePtr,
									 TableItem*newPtr,
									 TableItem*prevPtr,
									 int index, int *newIndex);
static int  TreeTableFindIndex_x (TableItem *itemPtr, TableItem *srcPtr, int index, int* pos);
static int  TreeTableCountNotHidden (TreeTable*tablePtr, int flag);
static int  TreeTableLostSel (TreeTable* tablePtr, int first, int last);
static int  TreeTableLostSel_x (register TreeTable *tablePtr, TableItem* itemPtr,
								int *pos, int first, int last);
static int  TreeTableRealyIndex (register TreeTable *tablePtr, int index);
static int  DisplayRecursive (register TreeTable *tablePtr,
							  register TableItem * itemPPtr,
							  register int* pos,
							  register int* realpos,
							  register int limit,
							  register Pixmap pixmap,
							  int calc);
static int TreeTableCountNotHidden_x (TableItem*itemPtr);
static void TreeTableImageProc(ClientData clientData,
							   int x, int y,
							   int width, int height,
							   int imgWidth, int imgHeight);
static void TreeTableGetItems (TreeTable *tablePtr, int from, int to);
static int TreeTableSearchCmd (TreeTable *tablePtr,  int argc,
							   char **argv);
static int TreeTableFindNotHiddenItem_x (TableItem *itemPtr,
										 int index, int pos,
										 TableItem **itemRet, int next);
static int TreeTableViewedIndex (register TreeTable *tablePtr, int index);
static int TreeTableSort(TreeTable *tablePtr, int argc, char **argv);
static int TreeTableSelectFromTo (TreeTable*tablePtr, TableItem *itemPtr,
								  int pos,
								  int start, int end);
static int TreeTableGetBBox (TreeTable *tablePtr, int index, int *x, int *y, int *w, int *h);
static TableItem * TreeTableFindNotHiddenItem (TreeTable *tablePtr, int index, int next);
static TableItem * TreeTableSortFindPrev(TableItem *listPtr, TableItem *itemPtr,
										 int col, int noCase, int *index);
static void ComputeTabStops (TreeTable*tablePtr, TableItem *itemPtr);

static int TreeTableToggle     (TreeTable* tablePtr, int xx, int yy, int testonly);
static int TreeTableToggleSubTree (TreeTable* tablePtr, TableItem* itemPtr, int* realpos, int* pos, int xx, int yy, int testonly);
static int TreeTableToggleItem (TreeTable* tablePtr, TableItem *itemPtr, int hide);
static void TreeTableToggleIt   (TreeTable* tablePtr, TableItem*Ptr);

void ViewArgs (char *reason, int argc, char *argv[], int mode);

#undef TEST_TREE
#ifdef TEST_TREE
#ifdef USE_CACHE
void ViewCache(TreeTable*tablePtr);
#endif
#endif

static int InsertedNewPosition = 0;

/*
 * Information used for argv parsing:
 */
static Tk_ConfigSpec itemConfigSpecs[] =
{
{TK_CONFIG_PIXELS, "-linewidth", "lineWidth", "LineWidth",
	 NO_TABLEITEM_LINEWIDTH, Tk_Offset(TableItem, lineWidth), 0},
	
{TK_CONFIG_STRING, "-text", "text", "Text",
	 DEF_TABLEITEM_TEXT, Tk_Offset(TableItem, text), 
	 TK_CONFIG_NULL_OK},
{TK_CONFIG_BITMAP, "-bitmap", "bitmap", "Bitmap",
	 DEF_TABLEITEM_BITMAP, Tk_Offset(TableItem, bitmap), 
	 TK_CONFIG_NULL_OK},
	
    /* image */
{TK_CONFIG_STRING, "-image", (char *) NULL, (char *) NULL,
	 DEF_TABLEITEM_IMAGE, Tk_Offset(TableItem, imageString),
	 TK_CONFIG_NULL_OK},
	
	
{TK_CONFIG_FONT, "-font", "font", "Font",
	 DEF_TABLEITEM_FONT, Tk_Offset(TableItem, fontPtr), 
	 TK_CONFIG_NULL_OK},
{TK_CONFIG_COLOR, "-textforeground", "textForeground", "Background",
	 DEF_TABLEITEM_TEXTFOREGROUND, Tk_Offset(TableItem, textFgColor), 
	 TK_CONFIG_NULL_OK},
	
#if USE_BITMAP_COLORS
{TK_CONFIG_COLOR, "-bitmapforeground", "bitmapForeground", "Background",
	 DEF_TABLEITEM_BITMAPFOREGROUND, Tk_Offset(TableItem, bitmapFgColor), 
	 TK_CONFIG_NULL_OK},
{TK_CONFIG_COLOR, "-bitmapbackground", "bitmapBackground", "Background",
	 DEF_TABLEITEM_BITMAPBACKGROUND, Tk_Offset(TableItem, bitmapBgColor), 
	 TK_CONFIG_NULL_OK},
{TK_CONFIG_COLOR, "-bitmapselectfg", "bitmapSelectFg", "Background",
	 DEF_TABLEITEM_BITMAPSELECTFOREGROUND, 
	 Tk_Offset(TableItem, bitmapSelectFgColor), TK_CONFIG_NULL_OK},
{TK_CONFIG_COLOR, "-bitmapselectbg", "bitmapSelectBg", "Background",
	 DEF_TABLEITEM_BITMAPSELECTBACKGROUND,
	 Tk_Offset(TableItem, bitmapSelectBgColor), TK_CONFIG_NULL_OK},
{TK_CONFIG_COLOR, "-lineforeground", "lineForeground", "Background",
	 DEF_TABLEITEM_LINEFOREGROUND, Tk_Offset(TableItem, lineFgColor), 
	 TK_CONFIG_NULL_OK},
#endif
{TK_CONFIG_INT, "-indent", "indent", "Indent",
	 DEF_TABLEITEM_INDENT, Tk_Offset(TableItem, indent), 0},
	
{TK_CONFIG_INT, "-parent", "parent", "Parent",
	 DEF_TABLEITEM_PARENT, Tk_Offset(TableItem, parent), 0},
	
{TK_CONFIG_INT, "-succnum", "succnum", "SuccNum",
	 DEF_TABLEITEM_SUCCNUM, Tk_Offset(TableItem, succNum), 0},
{TK_CONFIG_SYNONYM, "-children", "succnum", (char *) NULL,
	 (char *) NULL, 0, 0},
    
{TK_CONFIG_INT, "-seennum", "seennum", "SeenNum",
	 DEF_TABLEITEM_SEENNUM, Tk_Offset(TableItem, seenNum), 0},
    /*
     * Client data
     */
{TK_CONFIG_STRING, "-data", "data", "data",
	 DEF_TABLEITEM_DATA, Tk_Offset(TableItem, data),
	 TK_CONFIG_NULL_OK},
	
{TK_CONFIG_BOOLEAN, "-unknown", "image",
				"Image",
				DEF_TREETABLE_UNKNOWN_FLAG,
				Tk_Offset(TableItem, unknownFlag),
				0},
#if 0
{TK_CONFIG_INT, "-flags", "flags", "Flags",
	 DEF_TABLEITEM_FLAGS, Tk_Offset(TableItem, flags), 0},
#endif
	
{TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
	 (char *) NULL, 0, 0}
};
	
static Tk_ConfigSpec configSpecs[] =
{
	{TK_CONFIG_BORDER, "-background", "background", "Background",
				DEF_TREETABLE_BG_COLOR,
				Tk_Offset(TreeTable, normalBorder),
				TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_BORDER, "-background", "background", "Background",
				DEF_TREETABLE_BG_MONO,
				Tk_Offset(TreeTable, normalBorder),
				TK_CONFIG_MONO_ONLY},
	{TK_CONFIG_SYNONYM, "-bg", "background", (char *) NULL,
				(char *) NULL, 0, 0},
	  
	{TK_CONFIG_COLOR, "-bitmapforeground", "bitmapForeground",
				"Foreground",
				DEF_TREETABLE_BITMAPFG,
				Tk_Offset(TreeTable, defBitmapFgColor),
				0},
	{TK_CONFIG_COLOR, "-bitmapbackground", "bitmapBackground",
				"Foreground",
				DEF_TREETABLE_BITMAPBG,
				Tk_Offset(TreeTable, defBitmapBgColor),
				0},
{TK_CONFIG_COLOR, "-bitmapselectfg", "bitmapSelectFg", "Foreground",
				DEF_TREETABLE_BITMAPSELECTFOREGROUND, 
       				Tk_Offset(TreeTable, defBitmapSelectFgColor),
				TK_CONFIG_NULL_OK},
{TK_CONFIG_COLOR, "-bitmapselectbg", "bitmapSelectBg", "Background",
				DEF_TREETABLE_BITMAPSELECTBACKGROUND,
				Tk_Offset(TreeTable, defBitmapSelectBgColor),
				TK_CONFIG_NULL_OK},
{TK_CONFIG_PIXELS, "-bitmapspace", "bitmapSpace", "BitmapSpace",
				DEF_TREETABLE_BITMAPSPACE,
				Tk_Offset(TreeTable, bitmapSpace),
				0},

{TK_CONFIG_PIXELS, "-borderwidth", "borderWidth", "BorderWidth",
				DEF_TREETABLE_BORDER_WIDTH,
				Tk_Offset(TreeTable, borderWidth), 
				0},
{TK_CONFIG_SYNONYM, "-bw", "borderWidth", (char *) NULL,
				(char *) NULL, 0, 0},
       
{TK_CONFIG_ACTIVE_CURSOR, "-cursor", "cursor", "Cursor",
				DEF_TREETABLE_CURSOR,
				Tk_Offset(TreeTable, cursor),
				TK_CONFIG_NULL_OK},
       
{TK_CONFIG_BOOLEAN, "-exportselection", "exportSelection",
				"ExportSelection",
				DEF_TREETABLE_EXPORT_SELECTION,
				Tk_Offset(TreeTable, exportSelection),
				0},
       
{TK_CONFIG_SYNONYM, "-fg", "foreground", (char *) NULL,
				(char *) NULL, 0, 0},
{TK_CONFIG_BOOLEAN, "-fillselection", "fillSelection",
				"FillSelection",
				DEF_TREETABLE_FILL_SELECTION,
				Tk_Offset(TreeTable, fillSelection),
				0},
				
{TK_CONFIG_BOOLEAN, "-sortedinsertion", "sortedInsertion",
				"SortedInsertion",
				DEF_TREETABLE_SORTED_INSERTION,
				Tk_Offset(TreeTable, sortedInsertion),
				0},
{TK_CONFIG_BOOLEAN, "-sortnocase", "sortNoCase", "SortNoCase",
				DEF_TREETABLE_SORT_NOCASE,
				Tk_Offset(TreeTable, sortNoCase),
				0},
{TK_CONFIG_SYNONYM, "-nocase", "sortNoCase", (char *) NULL,
				(char *) NULL, 0, 0},
{TK_CONFIG_BOOLEAN, "-sortcolumn", "sortColumn", "SortColumn",
				DEF_TREETABLE_SORT_NOCASE,
				Tk_Offset(TreeTable, sortColumn),
				0},
{TK_CONFIG_SYNONYM, "-col", "sortColumn", (char *) NULL,
				(char *) NULL, 0, 0},
				
{TK_CONFIG_FONT, "-font", "font", "Font",
				DEF_TREETABLE_FONT,
				Tk_Offset(TreeTable, defFontPtr),
				0},
{TK_CONFIG_STRING, "-geometry", "geometry", "Geometry",
				DEF_TREETABLE_GEOMETRY,
				Tk_Offset(TreeTable, geometry),
				0},
{TK_CONFIG_INT, "-width", "width", "Width",
				DEF_TREETABLE_WIDTH,
				Tk_Offset(TreeTable, width), 0},
{TK_CONFIG_INT, "-height", "height", "Height",
				DEF_TREETABLE_HEIGHT,
				Tk_Offset(TreeTable, height), 0},
       
    /*
     * hidden bitmap
     */
{TK_CONFIG_BITMAP, "-hiddenbitmap", "hiddenbitmap", "HiddenBitmap",
				DEF_TABLEITEM_BITMAP,
				Tk_Offset(TreeTable, hiddenBitmap), 
				TK_CONFIG_NULL_OK},
       
    /* plus image */
{TK_CONFIG_STRING, "-plusimage", (char *) NULL, (char *) NULL,
				DEF_TABLEITEM_IMAGE,
				Tk_Offset(TreeTable, plusImageString),
				TK_CONFIG_NULL_OK},

    /* minus image */
{TK_CONFIG_STRING, "-minusimage", (char *) NULL, (char *) NULL,
				DEF_TABLEITEM_IMAGE,
				Tk_Offset(TreeTable, minusImageString),
				TK_CONFIG_NULL_OK},

    /* unknown image */
{TK_CONFIG_STRING, "-unknownimage", (char *) NULL, (char *) NULL,
				DEF_TABLEITEM_IMAGE,
				Tk_Offset(TreeTable, unknownImageString),
				TK_CONFIG_NULL_OK},

    /* hidden image */
{TK_CONFIG_STRING, "-hiddenimage", (char *) NULL, (char *) NULL,
				DEF_TABLEITEM_IMAGE,
				Tk_Offset(TreeTable, hiddenImageString),
				TK_CONFIG_NULL_OK},

{TK_CONFIG_STRING, "-idlecommand", "idleCommand", "IdleCommand",
				DEF_TABLEITEM_IDLECOMMAND,
				Tk_Offset(TreeTable, idlecommand),
				TK_CONFIG_NULL_OK},
				
#if 0
{TK_CONFIG_STRING, "-hiddencommand", "hiddencommand", "HiddenCommand",
				DEF_HIDDEN_COMMAND,
				Tk_Offset(TreeTable, hiddencommand),
				TK_CONFIG_NULL_OK},
#endif
       
{TK_CONFIG_COLOR, "-highlightbackground", "highlightBackground",
				"HighlightBackground", NORMAL_BG,
				Tk_Offset(TreeTable, highlightBgColor),
				0},
{TK_CONFIG_COLOR, "-highlightColor", "highlightcolor",
				"HighlightColor", BLACK, 
				Tk_Offset(TreeTable, highlightColor),
				0},
{TK_CONFIG_PIXELS, "-highlightwidth", "highlightWidth", "HighlightWidth",
				DEF_TREETABLE_HIGHLIGHT_WIDTH,
				Tk_Offset(TreeTable, highlightWidth),
				0},
{TK_CONFIG_PIXELS, "-highlightthickness", "highlightThickness", "HighlightThickness",
				DEF_TREETABLE_HIGHLIGHT_WIDTH,
				Tk_Offset(TreeTable, highlightWidth),
				0},
       
{TK_CONFIG_PIXELS, "-indentwidth", "indentWidth", "BorderWidth",
				DEF_TREETABLE_INDENTWIDTH,
				Tk_Offset(TreeTable, indentWidth),
				0},
{TK_CONFIG_COLOR, "-lineforeground", "lineForeground",
				"Foreground",
				DEF_TREETABLE_LINEFG,
				Tk_Offset(TreeTable, defLineFgColor),
				0},
{TK_CONFIG_PIXELS, "-linewidth", "lineWidth", "LineWidth",
				NO_TREETABLE_LINEWIDTH,
				Tk_Offset(TreeTable, defLineWidth),
				0},
       
{TK_CONFIG_RELIEF, "-relief", "relief", "Relief",
				DEF_TREETABLE_RELIEF,
				Tk_Offset(TreeTable, relief),
				0},
	/* Color for multi colors */
{TK_CONFIG_BORDER, "-selectbackground", "selectBackground",
				"Foreground",
				DEF_TREETABLE_SELECT_COLOR, 
				Tk_Offset(TreeTable, selBorder),
				TK_CONFIG_COLOR_ONLY},
	/* color for black/white option */
{TK_CONFIG_BORDER, "-selectbackground", "selectBackground",
				"Foreground", DEF_TREETABLE_SELECT_MONO,
				Tk_Offset(TreeTable, selBorder),
				TK_CONFIG_MONO_ONLY},
{TK_CONFIG_PIXELS, "-selectborderwidth", "selectBorderWidth",
				"BorderWidth",
				DEF_TREETABLE_SELECT_BD, 
				Tk_Offset(TreeTable, selBorderWidth),
				0},
				
	/* Color for multi colors */
{TK_CONFIG_COLOR, "-selectforeground", "selectForeground",
				"Background",
				DEF_TREETABLE_SELECT_FG_COLOR,
				Tk_Offset(TreeTable, selFgColorPtr),
				TK_CONFIG_COLOR_ONLY},
	/* color for black/white option */
{TK_CONFIG_COLOR, "-selectforeground", "selectForeground",
				"Background",
				DEF_TREETABLE_SELECT_FG_MONO,
				Tk_Offset(TreeTable, selFgColorPtr),
				TK_CONFIG_MONO_ONLY},
{TK_CONFIG_BOOLEAN, "-setgrid", "setGrid", "SetGrid",
				DEF_TREETABLE_SET_GRID,
				Tk_Offset(TreeTable, setGrid),
				0},
{TK_CONFIG_STRING, "-selectmode", "selectMode", "SelectMode",
				DEF_TREETABLE_SELECTMODE,
				Tk_Offset(TreeTable, selectMode), 
				TK_CONFIG_NULL_OK},
    
{TK_CONFIG_STRING, "-tabs", "tabs", "Tabs",
				DEF_TREETABLE_TABS,
				Tk_Offset(TreeTable, tabsList), 
				TK_CONFIG_NULL_OK},
{TK_CONFIG_STRING, "-deftabs", "defTabs", "DefTabs",
				DEF_TREETABLE_DEFTABS,
				Tk_Offset(TreeTable, defTabsList), 
				TK_CONFIG_NULL_OK},
{TK_CONFIG_STRING, "-justify", "justify", "Justify",
				DEF_TREETABLE_JUSTIFY,
				Tk_Offset(TreeTable, justify), 
				TK_CONFIG_NULL_OK},
{TK_CONFIG_BOOLEAN, "-bestfit", "bestFit", "BestFit",
				DEF_TREETABLE_BESTFIT,
				Tk_Offset(TreeTable, BestFit), 
				0},
{TK_CONFIG_BOOLEAN, "-autofit", "autoFit", "AutoFit",
				DEF_TREETABLE_AUTOFIT,
				Tk_Offset(TreeTable, AutoFit), 
				0},
{TK_CONFIG_BOOLEAN, "-truncate", "truncate", "Truncate",
				DEF_TREETABLE_SPLITLINES,
				Tk_Offset(TreeTable, Truncate), 
				0},
{TK_CONFIG_COLOR, "-splitlineforeground", "splitLineForeground",
				"Foreground",
				DEF_TREETABLE_SPLITLINEFG,
				Tk_Offset(TreeTable, defSplitLineFgColor),
				0},
{TK_CONFIG_BOOLEAN, "-splitlines", "splitLines", "SplitLines",
				DEF_TREETABLE_TRUNCATE,
				Tk_Offset(TreeTable, SplitLines), 
				0},
{TK_CONFIG_STRING, "-takefocus", "takeFocus", "TakeFocus",
				DEF_TREETABLE_TAKE_FOCUS,
				Tk_Offset(TreeTable, takeFocus),
				TK_CONFIG_NULL_OK},
{TK_CONFIG_COLOR, "-textforeground", "textForeground",
				"Foreground",
				DEF_TREETABLE_TEXTFG, 
				Tk_Offset(TreeTable, defTextFgColor),
				0},
{TK_CONFIG_STRING, "-xscrollcommand", "xScrollCommand", "ScrollCommand",
				DEF_TREETABLE_SCROLL_COMMAND,
				Tk_Offset(TreeTable, xScrollCmd),
				TK_CONFIG_NULL_OK},
{TK_CONFIG_STRING, "-yscrollcommand", "yScrollCommand", "ScrollCommand",
				DEF_TREETABLE_SCROLL_COMMAND,
				Tk_Offset(TreeTable, yScrollCmd),
				TK_CONFIG_NULL_OK},
 {TK_CONFIG_STRING, "-resizecommand", "resizeCommand", "ResizeCommand",
				DEF_TREETABLE_SCROLL_COMMAND,
				Tk_Offset(TreeTable, ResizeCmd),
				TK_CONFIG_NULL_OK},      
{TK_CONFIG_PIXELS, "-tabfreespace",
    				"tabFreeSpace", "TabFreeSpace",
				DEF_TABLEITEM_TABFREESPACE, Tk_Offset(TreeTable, tabsMinSpace), 0},
    
{TK_CONFIG_BOOLEAN, "-accelerator", "accelerator", "Accelerator",
				DEF_TREETABLE_USE_ACCELERATOR,
				Tk_Offset(TreeTable, UseAccelerator), 
				0},
    
{TK_CONFIG_BOOLEAN, "-windowsmode", "windowsMode", "WindowsMode",
				DEF_TREETABLE_WINDOWSMODE,
				Tk_Offset(TreeTable, nativeWindowsMode), 
				0},

{TK_CONFIG_STRING, "-truncatemethode", "truncateMethode", "TruncateMethode",
				DEF_TREETABLE_TRUNCATEMETHODE,
				Tk_Offset(TreeTable, TruncateMethodeStr), 
				0},
				
#ifdef USE_PATHFINDER
#endif

{TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
       (char *) NULL, 0, 0}
};

/******************************************************************************/
#define O_STR(i)			Tcl_GetStringFromObj(objv[i], NULL)
#define ONAME_STR(obj, i)	Tcl_GetStringFromObj(obj, [i], NULL)

static char ** Construct_Argv (int argc, Tcl_Obj**objv)
{
	char **argv;
	int i;
	argv = (char**)ckalloc(sizeof(char**)*argc);
	for (i=0; i<argc; i++)
	{
		argv[i] = O_STR(i);
	}
	return argv;
}
#define Free_Argv(x) ckfree((char *)x)
/******************************************************************************/


static void
TreeTableDeletedProc(clientData)
    ClientData clientData;      /* Pointer to widget record for widget. */
{
	register TreeTable *tablePtr = (TreeTable *) clientData;
    Tk_Window tkwin = tablePtr->tkwin;
	
    /*
     * This procedure could be invoked either because the window was
     * destroyed and the command was then deleted (in which case tkwin
     * is NULL) or because the command was deleted, and then this procedure
     * destroys the widget.
     */
	
    if (tkwin != NULL)
	{
		if (tablePtr->setGrid)
		{
			Tk_UnsetGrid(tablePtr->tkwin);
		}
		tablePtr->tkwin = NULL;
		Tk_DestroyWindow(tkwin);
	}
}
	
/*
 *-------------------------------------------------------------------
 *
 * TreeTableCmd --
 *
 *      This procedure is invoked to process the "treetable" Tcl
 *      command.  See the user documentation for details on what
 *      it does.
 *
 * Results:
 *      A standard Tcl result.
 *
 * Side Effects:
 *      See the user documentation
 *
 *-------------------------------------------------------------------
 TreeTableCmd
 */
	
static int
TreeTableCmd(ClientData clientData, /* Main window associated with interpreter */
	     Tcl_Interp *interp,    /* Current interpreter */
	     int		argc,       /* number of arguments */
	     Tcl_Obj	*objv[])     /* argument strings */
{
	register TreeTable *tablePtr;
	Tk_Window nnew;
	Tk_Window tkwin = (Tk_Window) clientData;
	
    if (argc < 2)
    {
		Tcl_WrongNumArgs (interp, 1, objv, "pathName ?options?");
		return TCL_ERROR;
	}
    
    nnew = Tk_CreateWindowFromPath(interp, tkwin, O_STR(1), (char *) NULL);
    if (nnew == (Tk_Window) NULL)
    {
		return TCL_ERROR;
	}
    
    /*
     * Initialize the fields of the structure that won't be initialized
     * by ConfigureTreeTable, or that ConfigureTreeTable requires to be
     * initialized already (e.g. resource pointers).
     */
    tablePtr = (TreeTable *) ckalloc(sizeof(TreeTable));
    memset (tablePtr, 0, sizeof (TreeTable));
    if (tablePtr==NULL)
	{
		Tcl_SetResult (interp, "can't allocate memory for treetable\n", TCL_STATIC);
    	return TCL_ERROR;
	}
    tablePtr->tkwin = nnew;
    tablePtr->display = Tk_Display(nnew);
    tablePtr->interp = interp;
    
    tablePtr->relief = TK_RELIEF_RAISED;
    tablePtr->defLineWidth = -1;
    tablePtr->maxWidth = 10;
    tablePtr->selectFirst = -1;
    tablePtr->active=-1;
    tablePtr->TruncateMethodeStr = NULL;
    tablePtr->TruncateMethode = TRUNCATE_AUTO;
    tablePtr->oldHeight = 0;
    tablePtr->oldWidth = 0;

    FREE_CACHE();
    FREE_PARENT_CACHE();
    
#if 0
	Tk_CreateEventHandler(tablePtr->tkwin, 
						  ButtonPressMask,
						  TreeTablePressProc, (ClientData) tablePtr);
    Tk_CreateEventHandler(tablePtr->tkwin, 
						  ButtonReleaseMask,
						  TreeTableReleaseProc, (ClientData) tablePtr);
#endif
	
    Tk_SetClass(tablePtr->tkwin, "TreeTable");
    Tk_CreateEventHandler(tablePtr->tkwin, 
						  ExposureMask|StructureNotifyMask|FocusChangeMask,
						  TreeTableEventProc, (ClientData) tablePtr);
    Tk_CreateSelHandler(tablePtr->tkwin, XA_PRIMARY, XA_STRING, 
						TreeTableFetchSelection, (ClientData) tablePtr, 
						XA_STRING);
    tablePtr->widgetCmd = Tcl_CreateObjCommand(interp, Tk_PathName(tablePtr->tkwin),
											(Tcl_ObjCmdProc *)TreeTableWidgetCmd,
											(ClientData) tablePtr,
											(Tcl_CmdDeleteProc *)TreeTableDeletedProc);
    if (ConfigureTreeTable(tablePtr, argc-2, objv+2, 0) != TCL_OK)
	{
		Tk_DestroyWindow(tablePtr->tkwin);
		return TCL_ERROR;
	}
	interp->result = Tk_PathName(tablePtr->tkwin);
	return TCL_OK;
}

/* This is how the treetable widget is created.  This returns a
   standard Tcl result.  */

int
create_treetable_command (Tcl_Interp *interp)
{
	if (Tcl_CreateObjCommand (interp, "treetable",
						(Tcl_ObjCmdProc *)TreeTableCmd,
						(ClientData)Tk_MainWindow (interp),
						NULL) == NULL)
	{
		return TCL_ERROR;
	}
	return TCL_OK;
}

static int
TreeTableBuildSelection (Tcl_Interp *interp, TableItem*itemPtr, int pos)
{
	TableItem* Ptr;
    int i = pos;
    char index[20];
    for (Ptr=itemPtr; Ptr != NULL; Ptr=Ptr->nextPtr)
	{
		if (Ptr->flags&ITEM_SELECTED)
		{
			sprintf (index, "%d", i);
			Tcl_AppendElement(interp, index);
		}
		i++;
		if (Ptr->succPtr != NULL)
		{
			i = TreeTableBuildSelection (interp, Ptr->succPtr, i);
		}
	}
    return i;
}

/*
 *--------------------------------------------------------------
 *
 * TreeTableWidgetCmd --
 *
 *	This procedure is invoked to process the Tcl command
 *	that corresponds to a widget managed by this module.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */
static int
TreeTableWidgetCmd(
	ClientData clientData,		/* Information about the treetable
								 * widget */
	Tcl_Interp *interp,			/* Current interpreter */
	int argc,					/* Number of arguments. */
	Tcl_Obj *objv[])                       /* Argument strings */
{
    register TreeTable *tablePtr = (TreeTable *) clientData;
    register TableItem *itemPtr, *Ptr;
    register char c, *cmd, *cmd2 = NULL;
    char **strv;
    register int result = TCL_OK;
    register int length;
    int index;
    char tmp [256];
	Tcl_Obj *errm;
	
	errm = Tcl_NewObj();
	
    Tcl_Preserve((ClientData) tablePtr);
    
    if (argc < 2)
	{
		Tcl_AppendStringsToObj (errm, "option ?arg arg ...?", (char *) NULL);
		Tcl_WrongNumArgs(interp, 1, objv, Tcl_GetStringFromObj(errm, NULL));
		goto error;
	}
	
	cmd = Tcl_GetStringFromObj(objv[1], NULL);
    c = cmd[0];
    length = strlen(cmd);
    if ((c == 'a') && (strncmp(cmd, "activate", length) == 0))
	{
    	int next = 1;
		if (argc < 3 || argc > 4)
		{
			Tcl_WrongNumArgs(interp, 2, objv, "index ?next|prev|1|-1?");
			goto error;
		}
		if (GetTreeTableIndex(tablePtr, O_STR(2), 0, &index) != TCL_OK)
		{
			goto error;
		}
		if (argc == 4 && Tcl_GetInt(interp, O_STR(3), &next) != TCL_OK)
		{
			goto error;
		}
		
		/* an item in a hidden sub tree can't be activeted */
		Ptr = TreeTableFindNotHiddenItem (tablePtr, index, next);
		if (Ptr == NULL)
		{
			Ptr = TreeTableFindItem (tablePtr, index);
			if (Ptr) for (Ptr=Ptr->parentPtr; Ptr; Ptr=Ptr->parentPtr)
			{
				if (Ptr->flags & ITEM_HIDDEN_SUBTREE)
				{
					index = TreeTableFindIndex (tablePtr, Ptr);
					if (Ptr->nextPtr != NULL)
					  index ++;
					break;
				}
			}
		}
		else
		{
			index = TreeTableFindIndex (tablePtr, Ptr);
		}
		tablePtr->active = index;
		TreeTableRedrawRange(tablePtr);
	}
    else if ((c == 'c') && (strncmp(cmd, "cget", length) == 0) && (length >= 2))
	{
    	char *p;
		int i;
		int tw;
		
		if (argc != 3)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "option");
			goto error;
		}
		
		/* the tab stops are changed
		 * immediatly and we must reset the tab stop
		 * string before returning the values
		 */
		if (tablePtr->tabsList != NULL)
		{
			if (tablePtr->tabsList != NULL)
			{
				ckfree ((char*)tablePtr->tabsList);
			}
			tablePtr->tabsList = (char*)ckalloc (sizeof (char) * tablePtr->tabsNum * 32);
			p = tablePtr->tabsList;
			p[0] = 0;
			for (i=0; i<tablePtr->tabsNum; i++)
			{
				if (tablePtr->tabsHidden[i])
				{
					if (i==0 && tablePtr->itemPtr!=NULL)
					{
						tw = Max (0, ITEM_TEXT_X(tablePtr->itemPtr));
					}
					else
					{
#ifdef SPACE_PROBLEM
						tw = 0;
#else
						tw = tablePtr->tabsMinSpace;
#endif
					}
				}
				else
				{
					tw = tablePtr->tabs[i];
				}
				sprintf (p+strlen(p), *p ? " %i" : "%i", tw);
			}
		}
		
		result = Tk_ConfigureValue(interp, tablePtr->tkwin, configSpecs,
								   (char *) tablePtr, O_STR(2), 0);
	}
	else if ((c == 'c') && (strncmp(cmd, "configure", length) == 0)
			 && (length > 2))
	{
		
		if (argc == 2)
		{
			result = Tk_ConfigureInfo(interp, tablePtr->tkwin, configSpecs,
									  (char *) tablePtr, (char *) NULL, 0);
		}
		else if (argc == 3)
		{
			  result = Tk_ConfigureInfo(interp, tablePtr->tkwin, configSpecs,
										(char *) tablePtr, O_STR(2), 0);
		}
		else
		{
			result = ConfigureTreeTable(tablePtr, argc-2, objv+2,
										TK_CONFIG_ARGV_ONLY);
		}
	}
	else if ((c == 'c') && (strncmp(cmd, "curselection", length) == 0)
			 && length >= 2)
	{
		if (argc != 2)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "");
			goto error;
		}
		
		/* build and return selection list */
		TreeTableBuildSelection (interp, tablePtr->itemPtr, 0);
		
	}
	else if ((c == 'r') && (strncmp(cmd, "remove", length) == 0))
	{
		int i, lineWidth=0;
		int deleteChildren = 0, cnt = 0;
		TableItem*Ptr;
		
		if (argc < 3 || argc > 4)
		{
		  RemoveError:
			Tcl_WrongNumArgs (interp, 2, objv, "?children? index");
			goto error;
		}
		if (GetTreeTableIndex(tablePtr, O_STR(argc-1), 0, &i) != TCL_OK)
		{
			goto error;
		}
		
		/* verify if delete children only */
		if (argc == 4)
		{
			if (strncmp(O_STR(2), "children", strlen (O_STR(2))) == 0)
			{
				deleteChildren = 1;
			}
			else
			{
				goto RemoveError;
			}
		}
		itemPtr = TreeTableFindItem(tablePtr, i);
		if (itemPtr == (TableItem *) NULL)
		{
			Tcl_AppendResult (interp, "Error obtaining item of index : ",
							 O_STR(argc-1), (char *) NULL);
			goto error;
		}
		if (!deleteChildren)
		{
			TreeTableRemoveItem(tablePtr, itemPtr, i, &lineWidth, 1, 0);
			cnt = 1;
		}
		/* delete all children, but don't delete the item */
		else
		{
			int start = i+1, end = i;
			for (Ptr=itemPtr->succPtr; Ptr; Ptr=Ptr->nextPtr)
			{
				end += 1 + Ptr->succNum;
			}
			if (end >= start)
			{
				TreeTableDeleteRange(tablePtr, start, end);
				cnt = end - start + 1;
			}
		}
		
		sprintf (tmp, "%i", cnt);
		Tcl_ResetResult (interp);
		Tcl_AppendResult (interp, tmp, NULL);
	}
	else if ((c == 'd') && (strncmp(cmd, "delete", length) == 0))
	{
		int start,end;
		
		if (argc < 3 || argc > 4)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "start ?end?");
			goto error;
		}
		if (GetTreeTableIndex(tablePtr, O_STR(2), 0, &start) != TCL_OK)
		{
			Tcl_AppendResult (interp, "Bad starting index.", (char *) NULL);
			goto error;
		}
		if (argc > 3)
		{
			if (GetTreeTableIndex(tablePtr, O_STR(3), 0, &end) != TCL_OK)
			{
				Tcl_AppendResult (interp, "Bad ending index.", (char *) NULL);
				goto error;
			}
		}
		else
		{
			end = start;
		}
		if (end < start)
		{
			end = start;
		}
		TreeTableDeleteRange(tablePtr, start, end);
	}
    else if ((c == 'g') && (strncmp(cmd, "get", length) == 0))
	{
		int to;
		
		if (argc < 3 || argc > 4)
		{
			get_invalid_args:
			Tcl_WrongNumArgs (interp, 2, objv, "index ?to?");
			goto error;
		}
		if (GetTreeTableIndex(tablePtr, O_STR(2), 0, &index) != TCL_OK)
		{
			goto get_invalid_args;
		}
		to = index;
		if (argc == 4 && GetTreeTableIndex(tablePtr, O_STR(3), 0, &to) != TCL_OK)
		{
			goto get_invalid_args;
		}
		if (index < 0)
		{
			index = 0;
		}
		if (index >= tablePtr->numItems)
		{
			index = tablePtr->numItems-1;
		}
		/* find item */
		if (argc == 3)
		{
			itemPtr = TreeTableFindItem (tablePtr, index);
			if (itemPtr != NULL)
			{
				interp->result = itemPtr->text; /*CHG*/
			}
		}
		/* find a couple of items */
		else
		{
			TreeTableGetItems (tablePtr, index, to);
		}
	}
    else if ((c == 'i') && (strncmp(cmd, "insert", length) == 0))
	{
    	TableItem* newPtr;
		if (argc < 3)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "index ?list <list>? ?option1 option2 ...?");
			goto error;
		}
		if (GetTreeTableIndex(tablePtr, O_STR(2), 1, &index) != TCL_OK)
		{
			goto error;
		}
		
		/* add a copple of items with same options */
		if (strcmp (O_STR(3), "list") == 0)
		{
			int ret =  TreeTableInsertItems (tablePtr, index, objv[4], argc-5, objv+5);
			if (ret != TCL_OK)
				goto error;
			else
				goto done;
		}
		InsertedNewPosition = index;
		
		/*
		 * Add only one item
		 */
		strv = Construct_Argv(argc-3, objv+3);
		newPtr = TreeTableInsertItem(tablePtr, index, NULL, argc-3, strv);
		Free_Argv(strv);
		
		if (newPtr == NULL)
		{
			Tcl_AppendResult (interp, "can not add item", NULL);
			goto error;
		}
		sprintf (tmp, "%i", InsertedNewPosition);
		Tcl_ResetResult (interp);
		Tcl_AppendResult (interp, tmp, (char*) NULL);
	}
    else if ((c == 'd') && (strncmp(cmd, "display", length) == 0))
	{
		if (argc != 2)
		{
			Tcl_WrongNumArgs (interp, 1, objv, "display");
			goto error;
		}
		DisplayTreeTable ((ClientData)tablePtr);
	}
	else if ((c == 'i') && (strncmp(cmd, "itemconfigure", length) == 0))
	{
		if (argc < 3)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "index ?option val ...?");
			goto error;
		}
		if (GetTreeTableIndex (tablePtr, O_STR(2), 0, &index) != TCL_OK)
		{
			goto error;
		}
		itemPtr = (TableItem *) TreeTableFindItem(tablePtr, index);
		if (itemPtr == (TableItem *) NULL)
		{
			char s[20];
			
			sprintf(s, "%d", index);
			Tcl_AppendResult(interp, "Unable to locate item with index: \"",
							 s, "\"", (char *) NULL);
			goto error;
		}
		
		/* set realy parent */
		itemPtr->parent = TreeTableFindIndex (tablePtr, itemPtr->parentPtr);
		
		if (argc == 3)
		{
			result = Tk_ConfigureInfo(interp, tablePtr->tkwin, itemConfigSpecs,
									  (char *)itemPtr, (char *) NULL, 0);
		}
		else if  (argc == 4)
		{
			result = Tk_ConfigureInfo(interp,
								tablePtr->tkwin, itemConfigSpecs,
								(char *) itemPtr, O_STR(3), 0);
		}
		else
		{
			strv = Construct_Argv(argc-3, objv+3);
			result = ConfigureTreeTableItem(tablePtr, itemPtr, argc-3,
											strv, TK_CONFIG_ARGV_ONLY, 0);
			Free_Argv (strv);
		}
	}
    else if ((c == 'i') && (strncmp(cmd, "itemcget", length) == 0))
	{
		if (argc != 4)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "index ?option?");
			goto error;
		}
		if (GetTreeTableIndex(tablePtr, O_STR(2), 0, &index) != TCL_OK)
		{
			goto error;
		}
		itemPtr = (TableItem *) TreeTableFindItem(tablePtr, index);
		if (itemPtr == (TableItem *) NULL)
		{
			char s[20];
			
			sprintf(s, "%d", index);
			Tcl_AppendResult(interp, "Unable to locate item with index: \"",
							 s, "\"", (char *) NULL);
			goto error;
		}
		
		/* set realy parent */
		itemPtr->parent = TreeTableFindIndex (tablePtr, itemPtr->parentPtr);
		result = Tk_ConfigureValue (interp, tablePtr->tkwin, itemConfigSpecs,
									(char *) itemPtr, O_STR(3), 0);
	}
    else if ((c == 'n') && (strncmp(cmd, "nearest", length) == 0))
	{
		int y;
		
		if (argc != 3)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "y");
			goto error;
		}
		if (Tcl_GetInt(interp, O_STR(2), &y) != TCL_OK)
		{
			goto error;
		}
		index = NearestTreeTableItem(tablePtr, y);
		sprintf(interp->result, "%d", index);
	} else if ((c == 'i') && (strncmp(cmd, "index", length) == 0)) {
		if (argc != 3)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "index");
			goto error;
		}
		if (GetTreeTableIndex (tablePtr, O_STR(2), 0, &index) != TCL_OK)
		{
			goto error;
		}
		sprintf(interp->result, "%d", index);
	}
	else if ((c == 'p') && (length >= 2) && (strncmp(cmd, "parent", length) == 0))
	{
		int parent;
		
		if (argc != 3)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "index");
			goto error;
		}
		if (GetTreeTableIndex(tablePtr, O_STR(2), 0, &index)
			!= TCL_OK)
		{
			goto error;
		}
		itemPtr = (TableItem *) TreeTableFindItem(tablePtr, index);
		if (itemPtr == (TableItem *) NULL)
		{
			Tcl_AppendResult (interp, "invalid treetable index: \"",
							 O_STR(2), "\"", (char *) NULL);
			goto error;
		}
		if (itemPtr->parentPtr == (TableItem *) NULL)
		{
			interp->result[0] = (char) NULL;
		}
		else
		{
			parent = TreeTableFindIndex(tablePtr, itemPtr->parentPtr);
			if (parent == -1)
			{
				Tcl_AppendResult(interp, "invalid treetable index: \"",
								 O_STR(2), "\"", (char *) NULL);
				goto error;
			}
			sprintf(interp->result, "%d", parent);
		}
	}
	else if ((c == 's') && (length >= 2) 
			 && (strncmp(cmd, "scan", length) == 0))
	{
		int x,y;
		
		if (argc != 5)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "mark|dragto x y");
			goto error;
		}
		if ((Tcl_GetInt(interp, O_STR(3), &x) != TCL_OK) ||
			(Tcl_GetInt(interp, O_STR(4), &y) != TCL_OK))
		{
			goto error;
		}
		if (strncmp(O_STR(2), "mark", strlen(O_STR(2))) == 0)
		{
			tablePtr->scanMarkX = x;
			tablePtr->scanMarkY = y;
			tablePtr->scanMarkXOffset = tablePtr->xOffset;
			tablePtr->scanMarkYIndex = tablePtr->topIndex;
		}
		else if (strncmp(O_STR(2), "dragto", strlen(O_STR(2))) == 0)
		{
			TreeTableScanTo(tablePtr, x, y);
		}
		else
		{
			Tcl_WrongNumArgs (interp, 2, objv, "mark|dragto");
			goto error;
		}
	}
    else if ((c == 's') && (length >= 2) && (strncmp(cmd, "selection", length) == 0))
	{
		if (argc < 3)
		{
			goto selection_invalid_args;
		}
		
		cmd2 = O_STR(2);
		length = strlen(cmd2);
		c = cmd2[0];
		
		/* Only require information */
		if (argc == 4 && (c == 'i') && (strncmp(cmd2, "includes", length) == 0))
		{
			if (GetTreeTableIndex(tablePtr, O_STR(3), 0, &index) != TCL_OK)
			{
				goto selection_invalid_args;
			}
			itemPtr = TreeTableFindItem (tablePtr, index);
			if (itemPtr != NULL && (itemPtr->flags&ITEM_SELECTED))
			{
				sprintf (interp->result, "1");
			}
			else
			{
				sprintf (interp->result, "0");
			}
			
			goto done;
		}
		else if (argc >= 4 && c == 'c' && (strncmp(cmd2, "clear", length) == 0))
		{
			int last;
			if (argc > 5)
			{
				goto selection_invalid_args;
			}
			if (GetTreeTableIndex (tablePtr, O_STR(3), 0, &index) != TCL_OK)
			{
				goto error;
			}
			last = index;
			if (argc == 5 && GetTreeTableIndex (tablePtr, O_STR(4), 0, &last) != TCL_OK)
			{
				goto error;
			}
			if (TreeTableLostSel (tablePtr, Min (index, last), Max (index, last)))
			{
				TreeTableRedrawRange(tablePtr);
			}
			goto done;
		}
		if (argc > 5)
		{
			goto selection_invalid_args;
		}
		
		if ((c == 'a') && (strncmp(cmd2, "anchor", length) == 0))
		{
			if (argc != 4 || GetTreeTableIndex(tablePtr, O_STR(3), 0, &index) 
				!= TCL_OK)
			{
				goto selection_invalid_args;
			}
			tablePtr->selectAnchor = index;
		}
		else if ((c == 's') && (strncmp(cmd2, "set", length) == 0))
		{
			int to;
			if (GetTreeTableIndex(tablePtr, O_STR(3), 0, &index) != TCL_OK)
			{
				goto selection_invalid_args;
			} 
			to = index;
			if (argc == 5 &&
				GetTreeTableIndex(tablePtr, O_STR(4), 0, &to) != TCL_OK)
			{
				goto selection_invalid_args;
			}
			TreeTableSelectFromTo
			  (tablePtr, tablePtr->itemPtr, 0, Min (index, to), Max (index, to));
			TreeTableRedrawRange(tablePtr);
		}
		else
		{
		  selection_invalid_args:
			Tcl_WrongNumArgs (interp, 2, objv, "anchor index, clear index ?last?, includes index, set index ?last?");
			goto error;
		}
	}
    else if ((c == 's') && (length > 2)
			 && (strncmp(cmd, "size", length) == 0))
	{
		sprintf(interp->result, "%d", tablePtr->numItems);
	}
    else if ((c == 'x') && (strncmp(cmd, "xview", length) == 0))
	{
		if (argc > 2)
			cmd2 = O_STR(2);
		if (argc == 2)
		{
			if (tablePtr->maxWidth == 0)
			{
				interp->result = "0 1";
			}
			else
			{
				double fraction,fraction2;
				double windowWidth;
				
				windowWidth = (double) TREETABLE_WIDTH(tablePtr);
				
				fraction = tablePtr->xOffset/((double) tablePtr->maxWidth);
				fraction2 = (tablePtr->xOffset + windowWidth)
				  /((double) tablePtr->maxWidth);
				
				if (fraction2 > 1.0)
				{
					fraction2 = 1.0;
				}
				sprintf(interp->result, "%g %g", fraction, fraction2);
			}
			goto done;
		}
		else if (argc == 3)
		{
			if (Tcl_GetInt (interp, cmd2, &index) != TCL_OK)
			{
				goto xview_invalid_num;
			}
			ChangeTreeTableOffset(tablePtr, index*tablePtr->xScrollUnit);
			goto done;
		}
		else if (argc == 4 && cmd2[0] == 's' && strcmp (cmd2, "see") == 0)
		{
			if (Tcl_GetInt (interp, O_STR(3), &index) != TCL_OK)
			{
				goto xview_invalid_num;
			}
			index = index * tablePtr->xScrollUnit - Tk_Width(tablePtr->tkwin) / 2;
			ChangeTreeTableOffset(tablePtr, index);
			goto done;
		}
		else
		{
			int number, units = 1, ret;
			double pos;
			
			strv = Construct_Argv (argc, objv);
			ret = Tk_GetScrollInfo(interp, argc, strv, &pos, &number);
			Free_Argv(strv);
			
			switch (ret)
			{
			  case TK_SCROLL_MOVETO:
				pos = pos * (double) tablePtr->maxWidth + 0.5;
				index = (int) pos;
				break;
			  case TK_SCROLL_PAGES:
				units = Tk_Width(tablePtr->tkwin) / tablePtr->xScrollUnit;
				index = tablePtr->xOffset + number * units * tablePtr->xScrollUnit;
				break;
			  case TK_SCROLL_UNITS:
				index = tablePtr->xOffset + number * units * tablePtr->xScrollUnit;
				break;
			  case TK_SCROLL_ERROR:
				goto error;
				break;
			}
			if (index < 0) index = 0;
			if (index > tablePtr->maxWidth) index = tablePtr->maxWidth;
			ChangeTreeTableOffset (tablePtr, index);
			goto done;
		}
	  xview_invalid_num:
		Tcl_AppendResult(interp, "xview: invalid number", (char *) NULL);
		goto error;
	}
    else if ((c == 'y') && (strncmp(cmd, "yview", length) == 0))
	{
		if (argc > 2)
			cmd2 = O_STR(2);
		if (argc == 2)
		{
			if (tablePtr->numItems == 0)
			{
				interp->result = "0 1";
			}
			else
			{
				double count;
				double fraction, fraction2;
				
				count = (double)TreeTableCountNotHidden (tablePtr, 0);
				fraction = tablePtr->topIndex/count;
				fraction2 = (tablePtr->topIndex + tablePtr->numLines) /
				  ((double) count);
				if (fraction2 > 1.0)
				{
					fraction2 = 1.0;
				}
				sprintf(interp->result, "%g %g", fraction, fraction2);
			}
			goto done;
		}
		else if (argc == 3)
		{
			if (GetTreeTableIndex(tablePtr, cmd2, 0, &index)
				!= TCL_OK)
			{
				goto yview_invalid_num;
			}
			ChangeTreeTableView(tablePtr, index, 0);
			goto done;
		}
		else if (argc == 4 && cmd2[0] == 's' && strcmp (cmd2, "see") == 0)
		{
			if (GetTreeTableIndex(tablePtr, O_STR(3), 0, &index) != TCL_OK)
			{
				goto yview_invalid_num;
			}
			if (index < 0)
			{
				index = 0;
			}
			else if (index >= tablePtr->numItems)
			{
				index = tablePtr->numItems-1;
			}
			tablePtr->active = index;
			
			/* convert realy index to index relative to viewed items */
			index = TreeTableViewedIndex (tablePtr, index);
			
			if (index < 0)
			{
				index = 0;
			}
			else if (index >= TreeTableCountNotHidden (tablePtr, 0))
			{
				index = TreeTableCountNotHidden (tablePtr, 0) - 1;
			}
			index -= tablePtr->numLines/2;
			ChangeTreeTableView(tablePtr, index, 1);
			goto done;
		}
		else
		{
			int number, ret;
			double pos;
			strv = Construct_Argv(argc, objv);
			ret = Tk_GetScrollInfo(interp, argc, strv, &pos, &number);
			Free_Argv(strv);
			
			switch (ret)
			{
			  case TK_SCROLL_MOVETO:
				/*pos = pos * (double) tablePtr->maxWidth + 0.5;*/
				pos = pos * (double) tablePtr->numViewed + 0.5;
				index = (int) pos;
				break;
			  case TK_SCROLL_PAGES:
				index = tablePtr->topIndex + number * tablePtr->numLines;
				break;
			  case TK_SCROLL_UNITS:
				index = tablePtr->topIndex + number;
				break;
			  case TK_SCROLL_ERROR:
				goto error;
				break;
			}
			ChangeTreeTableView (tablePtr, index, 1);
			goto done;
		}
	    
	  yview_invalid_num:
		Tcl_AppendResult(interp, "yview: invalid number", (char *) NULL);
		goto error;
	}
    else if (argc >= 3 && (c == 's') && strncmp (cmd, "see", length) == 0)
	{
    	int i = 2, top = 0;
		
		/* see the item on the top of the widget */
    	if (strcmp (O_STR(2), "-top") == 0)
		{
			top = 1;
			i = 3;
		}
		if (GetTreeTableIndex(tablePtr, O_STR(i), 0, &index) != TCL_OK)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "?-top? index");
			goto error;
		}
		
		/* convert realy index to index relative to viewed items */
		index = TreeTableViewedIndex (tablePtr, index);
		
		if (index < tablePtr->topIndex || top)
		{
			ChangeTreeTableView(tablePtr, index, 0);
		}
		else if (index > tablePtr->topIndex+tablePtr->numLines-1)
		{
			ChangeTreeTableView(tablePtr, index-tablePtr->numLines+1, 0);
		}
	}
    else if ((c == 'i') && (strncmp(cmd, "identify", length) == 0))
	{
		int x, y, ret;
		char*p;
		
		if (argc != 4)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "x y");
			goto error;
		} 
		if (Tcl_GetInt (interp, O_STR(2), &x) != TCL_OK)
		{
			goto error;
		}
		if (Tcl_GetInt (interp, O_STR(3), &y) != TCL_OK)
		{
			goto error;
		}
		
		ret = TreeTableToggle (tablePtr, x, y, 1);
		switch (ret)
		{
		  case ITEM_VIEWED:
			p = "view";
			break;
		  case ITEM_HIDDEN:
			p = "hide";
			break;
		  case ITEM_TEXT:
			p = "text";
			break;
		  case ITEM_UNKNOWN:
			p = "noroot";
			break;
		  case ITEM_NOT_FOUND:
			p = "";
			break;
		  default:
			p = "";
			break;
		}
		strcpy (interp->result, p);
	}
    else if ((c == 't') && (strncmp(cmd, "toggle", length) == 0))
	{
		int hide = 0, real_index=0;
		
		if (argc < 3 || argc > 5)
		{
		  ToggleError:
			Tcl_WrongNumArgs (interp, 2, objv, "index ?hide|view? ?virtual|real?");
			goto error;
		}
		if (GetTreeTableIndex(tablePtr, O_STR(2), 0, &index) != TCL_OK)
		{
			goto error;
		}
		if (argc == 5) /* verify if we want to toggle a virtual/real position*/
		{
			if (O_STR(4)[0] == 'v')
				real_index = 0;
			else if (O_STR(4)[0] == 'r')
				real_index = 1;
			else
				goto ToggleError;
		}
		if (real_index)
		{
			itemPtr = TreeTableFindItem (tablePtr, index);
		}
		else
		{
			itemPtr = TreeTableFindNotHiddenItem (tablePtr, index, 0);
		}
		if (itemPtr == NULL)
		{
			goto error;
		}
		if (argc >= 4)
		{
			if (O_STR(3)[0] == 'v')
				hide = 0;
			else if (O_STR(3)[0] == 'h')
				hide = 1;
			else
				goto ToggleError;
		}
		else 
		{
			hide = (itemPtr->flags&ITEM_HIDDEN_SUBTREE) ? 0 : 1;
		}
		DELETE_FROM_PATHFINDER(index);
		TreeTableToggleItem (tablePtr, itemPtr, hide);
	}
    else if ((c == 'f') && (strncmp(cmd, "fullname", length) == 0))
	{
		int len, alen;
		char *cchar = " ";
		
		if (argc < 3 || argc > 4)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "index ?join string?");
			goto error;
		}
		if (GetTreeTableIndex(tablePtr, O_STR(2), 0, &index) != TCL_OK)
		{
			goto error;
		}
		if (argc == 4)
		{
			cchar = O_STR(3);
		}
		if (index < 0) 
		{
			index = 0;
		}
		if (index >= tablePtr->numItems)
		{
			index = tablePtr->numItems-1;
		}
		/* find item */
		itemPtr = TreeTableFindItem (tablePtr, index);
		if (itemPtr != NULL)
		{
			char *buf;
			/** calculate length of used buffer */
			alen = strlen (cchar);
			len = strlen(itemPtr->text) + alen + 1;
			for (Ptr=itemPtr->parentPtr; Ptr != NULL; Ptr=Ptr->parentPtr)
			{
				len += Ptr->textLength + alen;
			}
			buf = (char*) ckalloc (len);
			if (buf == NULL)
			{
				goto error;
			}
			/* build full path name and join it with join string */
			strcpy (buf, itemPtr->text);
			for (Ptr=itemPtr->parentPtr; Ptr != NULL; Ptr=Ptr->parentPtr)
			{
				if (*buf)
				{
					memmove (buf + alen, buf, strlen (buf) + 1);
					strncpy (buf, cchar, alen);
				}
				memmove(buf + Ptr->textLength, buf, strlen (buf) + 1);
				strncpy (buf, Ptr->text, Ptr->textLength);
			}
			
			/* We can use TCL_DYNAMIC, because we used ckalloc */
			Tcl_SetResult(interp, buf, TCL_DYNAMIC);
		}
	}
    else if (c == 's' && strncmp (cmd, "search", length) == 0)
	{
		int ret;
		
		strv = Construct_Argv (argc, objv);
		ret = TreeTableSearchCmd(tablePtr, argc, strv);
		Free_Argv (strv);
		
		if (ret != TCL_OK)
			goto error;
	}
    else if ((c == 's') && (length > 5)
			 && (strncmp(cmd, "seenall", length) == 0))
	{
		sprintf(tmp, "%d", tablePtr->numViewed);
		Tcl_SetResult(interp, tmp, TCL_VOLATILE);
	}
    else if ((c == 'd') && (strncmp(cmd, "data", length) == 0))
	{
		if (argc != 3)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "index");
			goto error;
		}
		if (Tcl_GetInt(interp, O_STR(2), &index) != TCL_OK)
		{
			goto error;
		}
		itemPtr = TreeTableFindItem(tablePtr, index);
		if (itemPtr == NULL)
		{
			Tcl_AppendResult(interp, "wrong item #", (char *) NULL);
			goto error;
		}
		Tcl_SetResult (interp,
					   (itemPtr->data) ? itemPtr->data : "",
					   TCL_STATIC);
	}
    else if ((c == 's') && (strncmp(cmd, "sort", length) == 0))
	{
		int ret;
		
		strv = Construct_Argv (argc-2, objv+2);
		ret = TreeTableSort (tablePtr, argc-2, strv);
		Free_Argv (strv);
		
		if (ret != TCL_OK)
			goto error;
	}
    else if ((c == 'x') && (strncmp(cmd, "xoffset", length) == 0))
	{
		sprintf(interp->result, "%d", tablePtr->xOffset);
	}
    else if ((c == 'b') && (strncmp(cmd, "bbox", length) == 0))
	{
		int x, y, x2, y2;
		if (argc != 3)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "index");
			goto error;
		}
		if (GetTreeTableIndex(tablePtr, O_STR(2), 0, &index) != TCL_OK)
		{
			goto error;
		}
		if (TreeTableGetBBox (tablePtr, index, &x, &y, &x2, &y2) != TCL_OK)
		{
			goto error;
		}
		sprintf (tmp, "%i %i %i %i", x, y, x2, y2);
		Tcl_ResetResult (interp);
		Tcl_AppendResult (interp, tmp, NULL);
	}
    else if ((c == 'c') && (strncmp(cmd, "column", length) == 0))
	{
		int num, hide;
		if (argc != 4)
		{
		  column_error:
			Tcl_WrongNumArgs (interp, 2, objv, "hide|view|toggle column#");
			goto error;
		}
		if (Tcl_GetInt(interp, O_STR(3), &num) != TCL_OK)
		{
			goto column_error;
		}
		if (tablePtr->tabs == NULL)
		{
			Tcl_AppendResult(interp,
							 "wrong usage of \"",
							 O_STR(0),
							 "\": no tab stops availiable",
							 (char*) NULL);
			goto error;
		}
		if (num < 0)
		{
			num = 0;
		}
		if (num > tablePtr->tabsNum)
		{
			num = tablePtr->tabsNum;
		}
		if (strcmp (O_STR(2), "hide") == 0)
		{
			hide = 1;
		}
		else if (strcmp (O_STR(2), "view") == 0)
		{
			hide = 0;
		}
		else if (strcmp (O_STR(2), "toggle") == 0)
		{
			if (tablePtr->tabsHidden[num])
			{
				hide = 0;
			}
			else
			{
				hide = 1;
			}
		}
		else
		{
			goto column_error;
		}
		tablePtr->tabsHidden[num] = hide;
		
		TreeTableRedrawRange (tablePtr);
	}
    /*
     * Calculate how many parents does an item have
     */
    else if ((c == 'l') && (strncmp(cmd, "levels", length) == 0))
	{
    	int level = 0;
		if (argc != 3)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "index");
			goto error;
		}
		if (GetTreeTableIndex(tablePtr, O_STR(2), 0, &index) != TCL_OK)
		{
			goto error;
		}
		
		itemPtr = TreeTableFindItem (tablePtr, index);
		if (itemPtr != NULL)
		{
			while (itemPtr->parentPtr != NULL)
			{
				level++;
				itemPtr = itemPtr->parentPtr;
			}
		}
		
		sprintf (tmp, "%i", level);
		Tcl_ResetResult (interp);
		Tcl_AppendResult (interp, tmp, NULL);
	}
    else if ((c == 'c') && (strncmp(cmd, "children", length) == 0))
	{
		if (argc != 3)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "index");
			goto error;
		}
		if (GetTreeTableIndex(tablePtr, O_STR(2), 0, &index) != TCL_OK)
		{
			goto error;
		}
		
		itemPtr = TreeTableFindItem (tablePtr, index);
		if (itemPtr == NULL)
		{
			goto error;
		}
		Tcl_ResetResult (interp);
		for (index += 1, Ptr=itemPtr->succPtr; Ptr; Ptr=Ptr->nextPtr)
		{
			sprintf (tmp, "%i", index);
			Tcl_AppendElement (interp, tmp);
			index += 1 + Ptr->succNum;
		}
	}
    else if ((c == 'r') &&
			 (strncmp(cmd, "root", length) == 0 ||
			  strncmp(cmd, "rootname", length) == 0))
	{
		if (argc != 3)
		{
			Tcl_WrongNumArgs (interp, 2, objv, "index");
			goto error;
		}
		if (GetTreeTableIndex(tablePtr, O_STR(2), 0, &index) != TCL_OK)
		{
			goto error;
		}
		itemPtr = TreeTableFindItem (tablePtr, index);
		if (itemPtr == NULL)
		{
			goto error;
		}
		/* get base root of item */
		for (Ptr=itemPtr; Ptr->parentPtr; Ptr=Ptr->parentPtr)
		{
		}
		if (strcmp(cmd, "root") != 0) /* return root name */
		{
			Tcl_SetResult (interp, Ptr->text, TCL_STATIC);
		}
		else /* return root index */
		{
			sprintf (tmp, "%i", TreeTableFindIndex (tablePtr, Ptr));
			Tcl_SetResult (interp, tmp, TCL_VOLATILE);
		}
	}
#if defined (TEST_TREE) && defined (USE_CACHE)
    else if ((c == 't') && (strncmp(cmd, "test", length) == 0))
	{
		ViewCache (tablePtr);
	}
#endif
    else
	{
		Tcl_AppendResult(interp, "bad option \"", cmd,
						 "\": must be "
						 "activate, "
						 "bbox, "
						 "cget, "
						 "configure, "
						 "curselection, "
						 "delete, "
						 "fullname, "
						 "get, "
						 "hidecolumn, "
						 "identify, "
						 "index, "
						 "insert, "
						 "itemcget, "
						 "levels, "
						 "nearest, "
						 "remove, "
						 "root, "
						 "rootname, "
						 "scan, "
						 "search, "
						 "see, "
						 "seenall, "
						 "select, "
						 "selection, "
						 "size, "
						 "sort, "
						 "toggle, "
						 "xview, "
						 "or "
						 "yview", (char *) NULL);
		goto error;
	}
	
	/*
	  if (tabsAccess)
	{
	  fprintf (stderr, "\ttabs now <%s>\n", tablePtr->tabsList?tablePtr->tabsList:"");
	}
	  */
	
  done:
    Tcl_Release((ClientData) tablePtr);
    
#if 0
	if (cmd[0] != 'i' || strcmp (cmd, "insert") != 0)
	{
		fprintf (stderr, ">%i\n", clock());
	}
#endif
	
    return result;
    
  error:
	Tcl_DecrRefCount(errm);
    Tcl_Release((ClientData) tablePtr);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyTreeTable --
 *
 *	This procedure is invoked by Tcl_EventuallyFree or Tcl_Release
 *	to clean up the internal structure of a treetable at a safe time
 *	(when no-one is using it anymore).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Everything associated with the treetable is freed up.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyTreeTable(memPtr)
  char *memPtr;       /* Info about widget. */
{
	register TreeTable *tablePtr = (TreeTable *) memPtr;
    ImageList_t *img, *nextImg;
    
    /*
     * Free up all of the list elements.
     */
    TreeTableDeleteRange (tablePtr, 0, tablePtr->numItems-1);
	
    /*
     * Free up all the stuff that requires special handling,
     * then let Tk_FreeOptions handle all the standard option-related
     * stuff.
     */
	
    if (tablePtr->defTextGC != None)
	{
		Tk_FreeGC(tablePtr->display, tablePtr->defTextGC);
	}
    if (tablePtr->defBitmapGC != None)
	{
		Tk_FreeGC(tablePtr->display, tablePtr->defBitmapGC);
	}
    if (tablePtr->defBitmapSelectGC != None)
	{
		Tk_FreeGC(tablePtr->display, tablePtr->defBitmapSelectGC);
	}
    if (tablePtr->defLineGC != None)
	{
		Tk_FreeGC(tablePtr->display, tablePtr->defLineGC);
	}
    if (tablePtr->selectGC != None)
	{
		Tk_FreeGC(tablePtr->display, tablePtr->selectGC);
	}
    /* this function doesn't delete the image icons */
    Tk_FreeOptions(configSpecs, (char *) tablePtr, tablePtr->display, 0);
    
    /* also free image if availiable */
    if (tablePtr->hiddenImage != NULL)
	{
		Tk_FreeImage (tablePtr->hiddenImage);
	}
    /* free tab stops, if availiable */
    if (tablePtr->tabs != NULL)
	{
		ckfree ((char *)tablePtr->tabs);
	}
    if (tablePtr->defTabs != NULL)
	{
		ckfree ((char *)tablePtr->defTabs);
	}
    if (tablePtr->tabsJustify != NULL)
	{
		ckfree ((char *)tablePtr->tabsJustify);
	}
    if (tablePtr->idlecommand != NULL)
	{
		ckfree ((char *)tablePtr->idlecommand);
	}
    if (tablePtr->tabsHidden != NULL)
	{
		ckfree ((char *)tablePtr->tabsHidden);
	}
    
    /* free declared images */
    for (img=tablePtr->Images; img; )
	{
    	nextImg=img->next;
		Tk_FreeImage (img->image);
		ckfree ((char*)img->name);
		ckfree ((char*)img);
		img = nextImg;
	}
    
    ckfree ((char *) tablePtr);
}


/*
 *----------------------------------------------------------------------
 *
 * ConfigureTreeTable --
 *
 *      This procedure is called to process an argv/argc list, plus
 *      the Tk option database, in order to configure (or reconfigure)
 *      a treetable widget.
 *
 * Results:
 *      The return value is a standard Tcl result.  If TCL_ERROR is
 *      returned, then interp->result contains an error message.
 *
 * Side effects:
 *      Configuration information, such as colors, border width,
 *      etc. get set for tablePtr;  old resources get freed,
 *      if there were any.
 *
 *----------------------------------------------------------------------
 */

static int
ConfigureTreeTable(register TreeTable *tablePtr, /* Information about widget; may
						  * or may not already have values 
						  * for some fields. */
		   int argc,    /* Number of valid entries in argv */
		   Tcl_Obj *objv[], /* Arguments */
		   int flags)   /* Flags to pass to Tk_ConfigureWidget */
{
    XGCValues gcValues;
    GC new;
    int i, width, height, oldExport;
    Tk_FontMetrics metrics;
    int pixelWidth, pixelHeight;
    Tk_Font oldfont;
    Pixmap oldhiddenBitmap;
    static char **argv = NULL;
    char
	  *oldhiddenImageString,
	  *oldplusImageString,
	  *oldminusImageString,
	  *oldunknownImageString,
	  *oldTabsList,
	  *oldJustify,
	  *oldDefTabsList,
	  *oldSelectModeStr;
    int oldBestFit;
    int compute_width = 0;
#ifdef USE_PATHFINDER
    int oldUseAccel = tablePtr->UseAccelerator;
#endif
    int oldWidth, oldHeight;
    char * oldTruncateMethodeStr;
	
    oldExport		= tablePtr->exportSelection;
    oldfont		= tablePtr->defFontPtr;
    oldhiddenBitmap	= tablePtr->hiddenBitmap;
    oldhiddenImageString= tablePtr->hiddenImageString;
    oldplusImageString  = tablePtr->plusImageString;
    oldminusImageString = tablePtr->minusImageString;
    oldunknownImageString= tablePtr->unknownImageString;
    oldTabsList		= tablePtr->tabsList;
    oldJustify          = tablePtr->justify;
    oldDefTabsList	= tablePtr->defTabsList;
    oldSelectModeStr    = tablePtr->selectModeStr;
    oldBestFit          = tablePtr->BestFit;
    oldHeight           = tablePtr->height;
    oldWidth            = tablePtr->width;
    oldTruncateMethodeStr = tablePtr->TruncateMethodeStr;
	
	/*
	 * translate the list to a string list
	 */
	if (argv != NULL)
	{
		Free_Argv(argv);
	}
	argv = Construct_Argv (argc, objv);
	
    if (Tk_ConfigureWidget(tablePtr->interp, tablePtr->tkwin, configSpecs, 
						   argc, argv, (char *) tablePtr, flags) != TCL_OK)
	{
		return TCL_ERROR;
	}
	
    /*
     * Geometry for the plus image
     */
    if (tablePtr->plusImageString != NULL && oldplusImageString != tablePtr->plusImageString)
	{
    	Tk_Image image;
		image = Tk_GetImage (tablePtr->interp, tablePtr->tkwin,
							 tablePtr->plusImageString,
							 TreeTableImageProc, (ClientData) NULL);
		if (image == NULL)
		{
			Tcl_AppendResult(tablePtr->interp, "bad image name \"", tablePtr->plusImageString,
							 "\"", (char *) NULL);
			return TCL_ERROR;
		}
		if (tablePtr->plusImage != NULL)
		{
			Tk_FreeImage (tablePtr->plusImage);
		}
		tablePtr->plusImage = image;
		Tk_SizeOfImage (tablePtr->plusImage,
						&tablePtr->plusWidth,
						&tablePtr->plusHeight);
		compute_width = 1;
	}
	
    /*
     * Geometry for the minus image
     */
    if (tablePtr->minusImageString != NULL && oldminusImageString != tablePtr->minusImageString)
	{
    	Tk_Image image;
		image = Tk_GetImage (tablePtr->interp, tablePtr->tkwin,
							 tablePtr->minusImageString,
							 TreeTableImageProc, (ClientData) NULL);
		if (image == NULL)
		{
			Tcl_AppendResult(tablePtr->interp, "bad image name \"", tablePtr->minusImageString,
							 "\"", (char *) NULL);
			return TCL_ERROR;
		}
		if (tablePtr->minusImage != NULL)
		{
			Tk_FreeImage (tablePtr->minusImage);
		}
		tablePtr->minusImage = image;
		Tk_SizeOfImage (tablePtr->minusImage,
						&tablePtr->minusWidth,
						&tablePtr->minusHeight);
		compute_width = 1;
	}
	
    /*
     * Geometry for the unknown image
     */
    if (tablePtr->unknownImageString != NULL && oldunknownImageString != tablePtr->unknownImageString)
	{
    	Tk_Image image;
		image = Tk_GetImage (tablePtr->interp, tablePtr->tkwin,
							 tablePtr->unknownImageString,
							 TreeTableImageProc, (ClientData) NULL);
		if (image == NULL)
		{
			Tcl_AppendResult(tablePtr->interp, "bad image name \"", tablePtr->unknownImageString,
							 "\"", (char *) NULL);
			return TCL_ERROR;
		}
		if (tablePtr->unknownImage != NULL)
		{
			Tk_FreeImage (tablePtr->unknownImage);
		}
		tablePtr->unknownImage = image;
		Tk_SizeOfImage (tablePtr->unknownImage,
						&tablePtr->unknownWidth,
						&tablePtr->unknownHeight);
		compute_width = 1;
	}
	
    /* Geometry for the hidden bitmap/image **/
    if (tablePtr->hiddenImageString != NULL && oldhiddenImageString != tablePtr->hiddenImageString)
	{
    	Tk_Image image;
		image = Tk_GetImage (tablePtr->interp, tablePtr->tkwin,
							 tablePtr->hiddenImageString,
							 TreeTableImageProc, (ClientData) NULL);
		if (image == NULL)
		{
			Tcl_AppendResult(tablePtr->interp, "bad image name \"", tablePtr->hiddenImageString,
							 "\"", (char *) NULL);
			return TCL_ERROR;
		}
		if (tablePtr->hiddenImage != NULL)
		{
			Tk_FreeImage (tablePtr->hiddenImage);
		}
		tablePtr->hiddenImage = image;
		Tk_SizeOfImage (tablePtr->hiddenImage,
						&tablePtr->hiddenWidth,
						&tablePtr->hiddenHeight);
		compute_width = 1;
	}
    else if (oldhiddenBitmap != tablePtr->hiddenBitmap)
	{
		Tk_SizeOfBitmap(tablePtr->display, tablePtr->hiddenBitmap,
						&tablePtr->hiddenWidth, &tablePtr->hiddenHeight);
		compute_width = 1;
	}
	
    /*
     * Some options need special processing, such as parsing the
     * geometry and setting the background from a 3-D border.
     */
    Tk_SetBackgroundFromBorder(tablePtr->tkwin, tablePtr->normalBorder);
	
    if (tablePtr->highlightWidth < 0)
	{
		tablePtr->highlightWidth = 0;
	}
    tablePtr->inset = tablePtr->highlightWidth + tablePtr->borderWidth;
    gcValues.foreground = tablePtr->defTextFgColor->pixel;
    gcValues.font = Tk_FontId (tablePtr->defFontPtr);
    gcValues.graphics_exposures = False;
    new = Tk_GetGC(tablePtr->tkwin, GCForeground|GCFont|GCGraphicsExposures,
				   &gcValues);
    if (tablePtr->defTextGC != None)
	{
		Tk_FreeGC(tablePtr->display, tablePtr->defTextGC);
	}
    tablePtr->defTextGC = new;
    gcValues.foreground = tablePtr->selFgColorPtr->pixel;
    gcValues.background = Tk_3DBorderColor(tablePtr->selBorder)->pixel;
    gcValues.font = Tk_FontId (tablePtr->defFontPtr);
    
    new = Tk_GetGC(tablePtr->tkwin, GCForeground|GCBackground|GCFont,
				   &gcValues);
    if (tablePtr->selectGC != None)
	{
		Tk_FreeGC(tablePtr->display, tablePtr->selectGC);
	}
    tablePtr->selectGC = new;
    
    gcValues.foreground = tablePtr->defBitmapFgColor->pixel;
    gcValues.background = tablePtr->defBitmapBgColor->pixel;
    gcValues.graphics_exposures = False;
    if (tablePtr->defBitmapGC != None)
	{
		Tk_FreeGC(tablePtr->display, tablePtr->defBitmapGC);
	}
    tablePtr->defBitmapGC = Tk_GetGC(tablePtr->tkwin, 
									 GCForeground|GCBackground|
									 GCGraphicsExposures, &gcValues);
	
    if ((tablePtr->defBitmapSelectFgColor != None) ||
		(tablePtr->defBitmapSelectBgColor != None))
	{
		if (tablePtr->defBitmapSelectFgColor != None) {
		  gcValues.foreground = tablePtr->defBitmapSelectFgColor->pixel;
		} else {
		  gcValues.foreground = tablePtr->selFgColorPtr->pixel;
		}
		if (tablePtr->defBitmapSelectBgColor != None) {
		  gcValues.background = tablePtr->defBitmapSelectBgColor->pixel;
		} else {
		  gcValues.background = Tk_3DBorderColor(tablePtr->selBorder)->pixel;
		}
		gcValues.graphics_exposures = False;
		if (tablePtr->defBitmapSelectGC != None) {
		  Tk_FreeGC(tablePtr->display, tablePtr->defBitmapSelectGC);
		}
		tablePtr->defBitmapSelectGC = Tk_GetGC(tablePtr->tkwin,
											   GCForeground|GCBackground|
											   GCGraphicsExposures, &gcValues);
	}
    if (tablePtr->defLineWidth == atoi(NO_TREETABLE_LINEWIDTH))
	{
		tablePtr->defLineWidth = atoi(DEF_TREETABLE_LINEWIDTH);
	}
    gcValues.foreground = tablePtr->defLineFgColor->pixel;
    gcValues.graphics_exposures = False;
    gcValues.line_width = tablePtr->defLineWidth;
    if (tablePtr->defLineGC != None)
	{
		Tk_FreeGC(tablePtr->display, tablePtr->defLineGC);
	}
    tablePtr->defLineGC = Tk_GetGC(tablePtr->tkwin,
								   GCForeground|GCGraphicsExposures|GCLineWidth,
								   &gcValues);
    
    /* Split line color */
    gcValues.foreground = tablePtr->defSplitLineFgColor->pixel;
    if (tablePtr->defSplitLineGC != None)
	{
		Tk_FreeGC(tablePtr->display, tablePtr->defSplitLineGC);
	}
    tablePtr->defSplitLineGC = Tk_GetGC(tablePtr->tkwin,
										GCForeground|GCGraphicsExposures|GCLineWidth,
										&gcValues);
	
    /*
     * Claim the selection if we've suddenly started exporting it.
     */
    if (tablePtr->exportSelection && (!oldExport) && (tablePtr->selectFirst != -1))
	{
		Tk_OwnSelection(tablePtr->tkwin, XA_PRIMARY, TreeTableLostSelection,
						(ClientData) tablePtr);
	}
    
    /*
     * Register the desired geometry for the window, and arrange for
     * the window to be redisplayed.
     */
    if ((sscanf(tablePtr->geometry, "%dx%d", &width, &height) != 2) ||
		(width <= 0) || (height <= 0))
	{
		Tcl_AppendResult(tablePtr->interp, "bad geometry \"", tablePtr->geometry,
						 "\", should be \"widthxheight\"", (char *) NULL);
		return TCL_ERROR;
	}
    
    /*
     * width or height is modified
     */
    if (oldHeight != tablePtr->height)
	{
    	height = tablePtr->height;
	}
    if (oldWidth != tablePtr->width)
	{
    	width = tablePtr->width;
	}
    if ((oldWidth != tablePtr->width) || (oldHeight != tablePtr->height))
	{
		ckfree ((char*)tablePtr->geometry);
		tablePtr->geometry = (char*) ckalloc (32);
		sprintf (tablePtr->geometry, "%dx%d", width, height);
	}
    tablePtr->height = height;
    tablePtr->width = width;
    
    if (oldfont != tablePtr->defFontPtr)
	{
        Tk_GetFontMetrics (tablePtr->defFontPtr, &metrics);
		
		tablePtr->defFontHeight = metrics.linespace;
		TreeTableComputeLineHeight(tablePtr, NULL, 0);
		if (metrics.linespace + 1 + 2 * tablePtr->selBorderWidth
			> tablePtr->lineHeight)
		{
			tablePtr->lineHeight = (metrics.linespace + 1
									+ 2 * tablePtr->selBorderWidth);
		}
		tablePtr->flags |= UPDATE_H_SCROLLBAR;
		compute_width = 1;
	}
    
    /*
     * actual tab stop list
     */
    if (oldTabsList != tablePtr->tabsList && tablePtr->tabsList != NULL)
	{
		char **tabs, *p;
		int num, oldnum, *oldtabs;
		if (Tcl_SplitList (tablePtr->interp, tablePtr->tabsList, &num, &tabs) != TCL_OK)
		  return TCL_ERROR;
		if (num == 0)
		{
			ckfree ((char*)tabs);
			return TCL_ERROR;
		}
		oldtabs = tablePtr->tabs;
		oldnum  = tablePtr->tabsNum;
		
		tablePtr->tabs = (int*) ckalloc (sizeof (int*) * num);
		if (tablePtr->tabs == NULL)
		{
			ckfree ((char*)tabs);
			return TCL_ERROR;
		}
		tablePtr->tabsNum = num;
		for (i=0, p=tabs[i]; i<num; i++, p=tabs[i])
		{
			/* don't change value of a column if it usually hidden */
			if (tablePtr->tabsHidden == NULL || !tablePtr->tabsHidden[i])
			{
#ifdef SPACE_PROBLEM
				tablePtr->tabs[i] = Max (0, atoi (p));
#else
				tablePtr->tabs[i] = Max (tablePtr->tabsMinSpace, atoi (p));
#endif
			}
			else if (oldtabs != NULL && i < oldnum) /* calculated value */
			{
				tablePtr->tabs[i] = oldtabs[i];
			}
			else if (tablePtr->defTabs != NULL)
			{
				tablePtr->tabs[i] = tablePtr->defTabs [i];
			}
			else
			{
#ifdef SPACE_PROBLEM
				tablePtr->tabs[i] = 0;
#else
				tablePtr->tabs[i] = tablePtr->tabsMinSpace;
#endif
			}
		}
		if (oldtabs != NULL)
		{
			ckfree ((char*)oldtabs);
		}
    	/*
		 * save tab stops as default. this is usefull by bestfit and
		 * autofit
		 */
		if (tablePtr->defTabs == NULL)
		{
			tablePtr->defTabs = (int*)ckalloc (sizeof (int*) * num);
			if (tablePtr->defTabs == NULL)
			{
				ckfree ((char*)tabs);
				return TCL_ERROR;
			}
			for (i=0, p=tabs[i]; i<num; i++, p=tabs[i])
			{
				tablePtr->defTabs[i] = tablePtr->tabs[i];
			}
		}
		ckfree ((char*)tabs);
		
	}
    
    /*
     * actual default tab stop list
     */
    if (oldDefTabsList != tablePtr->defTabsList && tablePtr->defTabsList != NULL)
	{
		char **tabs, *p;
		int num;
		if (Tcl_SplitList (tablePtr->interp, tablePtr->defTabsList, &num, &tabs) != TCL_OK)
		  return TCL_ERROR;
		if (num == 0)
		{
			ckfree ((char*)tabs);
			return TCL_ERROR;
		}
		if (tablePtr->defTabs != NULL)
		{
			ckfree ((char*)tablePtr->defTabs);
		}
		
		tablePtr->defTabs = (int*)ckalloc (sizeof (int*) * num);
		if (tablePtr->defTabs == NULL)
		{
			ckfree ((char*)tabs);
			return TCL_ERROR;
		}
		for (i=0, p=tabs[i]; i<num; i++, p=tabs[i])
		{
			tablePtr->defTabs[i] = Max (0, atoi (p));
		}
    	/*
		 * if tab stop list isn't given set this tab stop to it
		 */
		if (tablePtr->tabs == NULL)
		{
			tablePtr->tabs = (int*)ckalloc (sizeof (int*) * num);
			if (tablePtr->tabs == NULL)
			{
				ckfree ((char*)tabs);
				return TCL_ERROR;
			}
		}
		/*
		 * the tab stop number must be equevalent
		 * take minimun, else we get a 'core dump'
		 */
		if (tablePtr->tabsNum != num)
		{
			tablePtr->tabsNum = Min (tablePtr->tabsNum, num);
		}
		/* set the tabs and deftabs as equevalent for now */
		for (i=0, p=tabs[i]; i<num; i++, p=tabs[i])
		{
			tablePtr->tabs[i] = tablePtr->defTabs[i];
		}
		ckfree ((char*)tabs);
		
	}
    
    /*
     * if justify is changed
     */
    if (oldJustify != tablePtr->justify)
	{
		char **jj, *p;
		int num;
		if (Tcl_SplitList (tablePtr->interp, tablePtr->justify, &num, &jj) != TCL_OK)
		  return TCL_ERROR;
		if (num != tablePtr->tabsNum)
		{
			ckfree ((char*)jj);
			return TCL_ERROR;
		}
    	if (tablePtr->tabsJustify)
		{
			ckfree ((char*)tablePtr->tabsJustify);
		}
		tablePtr->tabsJustify = (int*)ckalloc (sizeof (int*) * tablePtr->tabsNum);
		for (i=0, p=jj[i]; i<num; i++, p=jj[i])
		{
			if (Tcl_GetInt(tablePtr->interp, jj[i], &tablePtr->tabsJustify[i]) != TCL_OK)
			{
				return TCL_ERROR;
			}
		}
		ckfree ((char*)jj);
	}
    
    /* allocate memory for tabs state (hidden/viewed) */
    if (tablePtr->tabsHidden == NULL && tablePtr->tabs && tablePtr->tabsNum > 0)
	{
		tablePtr->tabsHidden = (int*)ckalloc (sizeof (int*) * tablePtr->tabsNum);
	    memset (tablePtr->tabsHidden, 0, sizeof (int*) * tablePtr->tabsNum);
	}
    
    if (oldSelectModeStr != tablePtr->selectModeStr && tablePtr->selectModeStr != NULL)
	{
		tablePtr->selectMode =
		  (strcmp (tablePtr->selectModeStr, "single") == 0 ||
		   strcmp (tablePtr->selectModeStr, "browse") == 0
		   ? SINGLE
		   : MULTI);
	}
    
    /* only on this position we can look for bestfit */
    if (oldBestFit != tablePtr->BestFit && tablePtr->tabs && tablePtr->BestFit)
	{
		TreeTableComputeWidths (tablePtr, NULL, 3);
	}
    
#ifdef USE_PATHFINDER
    if (oldUseAccel != tablePtr->UseAccelerator)
	{
		if (tablePtr->UseAccelerator)
		{
			if (tablePtr->pathFinder == NULL)
			{
				tablePtr->pathFinder = (PathFinder_t*)ckalloc (sizeof (PathFinder_t));
				memset (tablePtr->pathFinder, 0, sizeof (PathFinder_t));
			}
		}
		else
		{
			/* delete path finder */
			PathFinder_t *pf, *pf1;
			for (pf1=tablePtr->pathFinder; pf1; )
			{
				pf = pf1;
				pf1 = pf1->next;
				ckfree ((char*)pf);
			}
			tablePtr->pathFinder = NULL;
		}
	}
#endif
	
	/*
	 * verify witch truncating methode we should use
	 */
	if (strcmp (tablePtr->TruncateMethodeStr, "path") == 0)
	{
		tablePtr->TruncateMethode = TRUNCATE_PATH;
	}
	else if (strcmp (tablePtr->TruncateMethodeStr, "auto") == 0)
	{
		tablePtr->TruncateMethode = TRUNCATE_AUTO;
	}
	else
	{
		tablePtr->TruncateMethode = TRUNCATE_NORMAL;
	}
	
    tablePtr->numLines = TREETABLE_NUM_LINES(tablePtr);
	
    if (tablePtr->numLines <0)
	{
		tablePtr->numLines = 0;
	}
    
    /* compute width newely if needed, else it take long time */
    if (compute_width)
	{
		TreeTableComputeWidths(tablePtr, (TableItem *) NULL, 2);
		tablePtr->flags |= UPDATE_V_SCROLLBAR;
	}
    
    pixelWidth = width*tablePtr->xScrollUnit
	  + 2*tablePtr->inset
	  + 2*tablePtr->selBorderWidth;
    pixelHeight = height*tablePtr->lineHeight
	  + 2*tablePtr->borderWidth;
    
    if (((TkWindow *)tablePtr->tkwin)->reqWidth != pixelWidth ||
    	((TkWindow *)tablePtr->tkwin)->reqHeight != pixelHeight)
	{
		Tk_GeometryRequest   (tablePtr->tkwin, pixelWidth, pixelHeight);
	}
    Tk_SetInternalBorder (tablePtr->tkwin, tablePtr->borderWidth);
    if (tablePtr->setGrid)
	{
		Tk_SetGrid(tablePtr->tkwin, width, height, tablePtr->xScrollUnit,
				   tablePtr->lineHeight);
	}
    tablePtr->flags |= UPDATE_V_SCROLLBAR|UPDATE_H_SCROLLBAR;
    TreeTableRedrawRange(tablePtr);
    return TCL_OK;
}

#if _WINDOWS
static char * my_WindowsNativePath (char *str, int len)
{
	static char *newp=NULL, *q;
	static int newplen = 0;
	if (len > newplen)
	{
		newplen = len+1;
		if (newp)
		{
			newp = (char*)ckrealloc (newp, newplen);
		}
		else
		{
			newp = (char*)ckalloc (newplen);
		}
	}
	
	if (strchr (str, '/') == NULL)
	{
		return str;
	}
	
	strcpy (newp, str);
	for (q=newp; *q; q++)
	{
		if (*q == '/')
		{
			*q = '\\';
		}
	}
	return newp;
}
#endif

int
Hase_Root (char * str, int len)
{
	int i;
	for (i=0; i<len; i++)
	{
		if (IS_ROOT (str[i]))
		{
			return 1;
		}
	}
	return 0;
}

static char *
TreeTable_truncate_string (TreeTable* tablePtr, char *buf, int column_len, int len)
{
	static char tmp[1024];
	int truncate_methode;
	
	/*
	 * by AUTO-truncating, it looks if the string is a path
	 * True:  cut the prefix
	 * False: cut the suffix
	 */
	if (tablePtr->TruncateMethode == TRUNCATE_AUTO)
	{
		if (Hase_Root (buf, column_len))
		{
			truncate_methode = TRUNCATE_PATH;
		}
		else
		{
			truncate_methode = TRUNCATE_NORMAL;
		}
	}
	else
	{
		truncate_methode = tablePtr->TruncateMethode;
	}
	
	/*
	 * truncate
	 *
	 * /home/user/foo.c ==> /..user/foo.c
	 * relative/path    ==> ..ative/path
	 * C:\Bad\Windows   ==> C:\..dows
	 */
	if (truncate_methode == TRUNCATE_PATH)
	{
		int beg_trunc = 0;
		
#if _WINDOWS
		/*
		 * On Windows a full path begins with "C:\" or "C:/"
		 */
		if (IS_ROOT(buf[0]) || (column_len > 1 && buf[1] == ':' && IS_ROOT(buf[2])))
		{
			beg_trunc ++;
		}
#else
		if (IS_ROOT(buf[0]))
		{
			beg_trunc ++;
		}
#endif

#if _WINDOWS
		if (buf[1] == ':')
		{
			beg_trunc ++;
			if (IS_ROOT (buf[2]))
			{
				beg_trunc ++;
			}
		}
#endif
		if (len < beg_trunc)
		{
			beg_trunc = 0;
		}
		strncpy (tmp, buf, beg_trunc);
		if (beg_trunc < len)
		{
			tmp[beg_trunc ++] = '.';
		}
		if (beg_trunc < len)
		{
			tmp[beg_trunc ++] = '.';
		}

		strncpy (tmp+beg_trunc, buf + Max (0, beg_trunc+column_len-len), len);
		tmp[len] = 0;
	}
	else
	{
		char *p;
		strncpy (tmp, buf, len);
							
		if (len > 2)
		{
			p = tmp + len - 2;
			*p ++ = '.';
			*p ++ = '.';
		}
		else if (len == 2)
		{
			p = tmp + len - 1;
			*p ++ = '.';
		}
		else
		{
			p = tmp + 1;
		}
		p[0] = 0;
	}
	return tmp;
}

#define ItemIndent(itemPtr) tablePtr->indentWidth

static int
DisplayRecursive (register TreeTable *tablePtr,
				  register TableItem * itemPPtr,
				  register int* seenpos,
				  register int* realpos,
				  int limit,
				  Pixmap pixmap,
				  int disp)
{
    GC gc=0;
    Tk_Font font, oldFont=NULL;
    Tk_FontMetrics metrics;
    TableItem *parentPtr;
    TableItem * itemPtr;
    int real_x, real_text_x, real_bitm_x, real_plus_x, horiz_x, horiz_x2, vert_x, vert_x2;
    int real_y, real_text_y, real_bitm_y, real_plus_y, horiz_y, horiz_y2, vert_y, vert_y2;
    int draw_text_x, draw_text_y;
    int hidden;
    int redisplay = 0, windowWidth = -1;
    int lineHeight = tablePtr->lineHeight;
#ifdef USE_PATHFINDER
    PathFinder_t *Pf=tablePtr->pathFinder;
#endif
    
    real_x = tablePtr->inset - tablePtr->xOffset;
    
    itemPtr = itemPPtr;
	
#ifdef USE_PATHFINDER
    if (Pf)
	{
		/* we only use the Pathfinder on the first level
		 * of the tree (root) ***/
    	if (itemPtr->parentPtr) /* parent availiable */
		{
			Pf = NULL;
		}
		else
		{
			PathFinder_t *pf=Pf;
			for (pf=Pf->next; pf; pf=pf->next)
			{
				if (pf->itemPos < tablePtr->topIndex)
				{
					*realpos = pf->itemPos;
					*seenpos = pf->seenPos;
					itemPtr  = pf->itemPtr;
				}
			}
			/* find last path finder */
			for (;Pf->next; Pf=Pf->next);
		}
	}
#endif
	
    while (*seenpos < tablePtr->topIndex && itemPtr != NULL)
	{
#ifdef USE_PATHFINDER
		if (Pf && Pf->itemPos < *realpos && (*realpos%PATHFINDER_STEP)==0)
		{
	    	   Pf->next = (PathFinder_t*)ckalloc (sizeof (PathFinder_t));
		    memset (Pf->next, 0, sizeof (PathFinder_t));
			Pf=Pf->next;
			Pf->itemPos = *realpos;
			Pf->seenPos = *seenpos;
			Pf->itemPtr = itemPtr;
		}
#endif
		
	    *seenpos += 1;
	    *realpos += 1;
	    
	    if (itemPtr->succPtr != NULL && ! (itemPtr->flags&ITEM_HIDDEN_SUBTREE))
		{
			if (*seenpos+itemPtr->seenNum < tablePtr->topIndex)
			{
				*realpos += itemPtr->succNum;
				*seenpos += itemPtr->seenNum;
			}
			else
			{
				redisplay += DisplayRecursive
				  (tablePtr, itemPtr->succPtr, seenpos, realpos, limit, pixmap,
				   disp);
			}
		}
	    else
		{
			*realpos += itemPtr->succNum;
			*seenpos += itemPtr->seenNum;
		}
	    itemPtr = itemPtr->nextPtr;
	}
    while ((itemPtr != NULL) && (*seenpos <= limit))
	{
#ifdef USE_PATHFINDER
		/* Add only the root items in the pathfinder */
    	if (Pf && Pf->itemPos < *realpos && (*realpos%PATHFINDER_STEP)==0)
		{
			Pf->next = (PathFinder_t*)ckalloc (sizeof (PathFinder_t));
			memset (Pf->next, 0, sizeof (PathFinder_t));
			Pf=Pf->next;
			Pf->itemPos = *realpos;
			Pf->seenPos = *seenpos;
			Pf->itemPtr = itemPtr;
		}
#endif
		hidden = itemPtr->flags&ITEM_HIDDEN_SUBTREE;
		
		/* we must know witch font we are playing with */
		font = ((itemPtr->fontPtr != NULL) ? itemPtr->fontPtr
				: tablePtr->defFontPtr);
		if (oldFont != font)
		{
			Tk_GetFontMetrics (font, &metrics);
			oldFont = font;
		}
		itemPtr->fontHeight = metrics.linespace;
		
		/*
		 * Compute starting coordinates for every thing before starting
		 * drawing
		 */
		real_plus_x = real_x + itemPtr->indent*tablePtr->indentWidth;
		real_bitm_x = real_plus_x;
		if (Have_PlusMinus(tablePtr))
		{
			real_bitm_x += tablePtr->plusWidth + tablePtr->bitmapSpace;
		}
		
		/*
		 * Increment x position by separator space.
		 */
		real_text_x = real_bitm_x;
		if (itemPtr->bitmap != None || itemPtr->image)
		{
			real_text_x += itemPtr->bitmapWidth + tablePtr->bitmapSpace;
		}
		
		/* y-coordinate for bitmap */
		real_y = ((*seenpos - tablePtr->topIndex) * lineHeight)
		  + tablePtr->inset;
		/*
		 * Compute y coordinate to center the text vertically on the line.
		 */
		real_text_y  = real_y + (lineHeight-itemPtr->fontHeight)/2;
		draw_text_y  = real_text_y + lineHeight - 2 * metrics.descent;
		real_bitm_y  = real_y + (lineHeight-itemPtr->bitmapHeight)/2;
		real_plus_y  = real_y + (lineHeight-tablePtr->plusHeight)/2;
		
		/*
		 * If this line is part of the selection, draw the higlight now 
		 */
		if (disp && (itemPtr->flags&ITEM_SELECTED))
		{
			if (tablePtr->fillSelection && windowWidth < 0)
			{
				windowWidth = TREETABLE_WIDTH(tablePtr);
			}
			Tk_Fill3DRectangle(tablePtr->tkwin, pixmap,
							   tablePtr->selBorder,
							   real_text_x - tablePtr->selBorderWidth,
							   real_text_y - tablePtr->selBorderWidth,
							   (tablePtr->fillSelection)
									? Max (windowWidth, itemPtr->lineWidth) /*+ tablePtr->selBorderWidth*2*/
									: itemPtr->lineWidth - real_bitm_x - tablePtr->xOffset,
							   lineHeight,
							   tablePtr->selBorderWidth,
							   TK_RELIEF_RAISED);
		}
		
		/*
		 * Lines must be drawn before text and images
		 */
		
		/*
		 * Draw the line back to the parent if necessary
		 */
		if (disp && Have_PlusMinus(tablePtr))
		{
			/* compute coordinates for horizontal line */
			horiz_y = real_plus_y + tablePtr->plusHeight/2;
			horiz_x2 = real_bitm_x+itemPtr->bitmapWidth/2;
			horiz_x  = horiz_x2 - tablePtr->indentWidth;
			horiz_y2 = horiz_y;
			
			/* draw horizontal line */
			XDrawLine(tablePtr->display, pixmap,
#ifdef USE_BITMAP_COLOR
						itemPtr->lineGC != None ? itemPtr->lineGC : tablePtr->defLineGC,
#else
						tablePtr->defLineGC,
#endif
						horiz_x, horiz_y, horiz_x2, horiz_y);
			
			/* Condition:
			 * -item has succ's
			 * Action:
			 * -draw a line from the bitmap
			 */
			if (!hidden && itemPtr->succPtr)
			{
				int succ_x, succ_y, succ_x2, succ_y2;
				succ_x = real_bitm_x+itemPtr->bitmapWidth/2;
				succ_x2 = succ_x;
				succ_y = real_bitm_y+itemPtr->bitmapHeight/2;
				succ_y2 = real_y + tablePtr->lineHeight;
				
				XDrawLine(tablePtr->display, pixmap,
#ifdef USE_BITMAP_COLOR
						  itemPtr->lineGC != None ? itemPtr->lineGC : tablePtr->defLineGC,
#else
						  tablePtr->defLineGC,
#endif
						  succ_x, succ_y, succ_x2, succ_y2);
			}
		}
		else if (disp)
		/* The tree uses the older mode (without plus/minus) */
		{
			/*
			 * if there is a parent, draw a horizontal line back to the parent */
			if (itemPtr->parentPtr != NULL)
			{
				horiz_y  = real_bitm_y + itemPtr->bitmapHeight/2;
				horiz_x  = real_bitm_x - ItemIndent(itemPtr) + itemPtr->bitmapWidth/2;
				horiz_x2 = real_bitm_x;
				horiz_y2 = horiz_y;
				XDrawLine(tablePtr->display, pixmap,
#ifdef USE_BITMAP_COLOR
						  itemPtr->lineGC != None ? itemPtr->lineGC : tablePtr->defLineGC,
#else
						  tablePtr->defLineGC,
#endif
						  horiz_x, horiz_y, horiz_x2, horiz_y2);
			}
			
			/*
			 * if the item has sons, draw a line to the first succ */
			if (itemPtr->succPtr != NULL && ! hidden)
			{
				vert_x = real_bitm_x + itemPtr->bitmapWidth/2;
				vert_x2 = vert_x;
				vert_y = real_y + tablePtr->lineHeight/2;
				vert_y2 = real_y + tablePtr->lineHeight;
				XDrawLine(tablePtr->display, pixmap,
#ifdef USE_BITMAP_COLOR
						  itemPtr->lineGC != None ? itemPtr->lineGC : tablePtr->defLineGC,
#else
						  tablePtr->defLineGC,
#endif
						  vert_x, vert_y, vert_x2, vert_y2);
			}
		}
		
		/* parse the parents back to the first level (not first level)
		 * and draw the lines if a next item of a parent is availiable
		 */
		if (disp /*&& Have_PlusMinus(tablePtr)*/ )
		{
			int i;
			int diff = ItemIndent(itemPtr);
			TableItem * Ptr;
			for (i=1, Ptr=itemPtr, parentPtr = itemPtr->parentPtr;
				 parentPtr != NULL;
				 Ptr=parentPtr, parentPtr = parentPtr->parentPtr, i++)
			{
				int left_x, left_x2, left_y, left_y2;
				
				if (Ptr->nextPtr == NULL && Ptr != itemPtr)
				{
					continue;
				}
				
				left_x = real_bitm_x+itemPtr->bitmapWidth/2 - i*diff;
				left_x2 = left_x;
				left_y = real_y;
				if (Ptr->nextPtr != NULL)
				{
					left_y2 = real_y+tablePtr->lineHeight;
				}
				else
				{
					left_y2 = real_y+tablePtr->lineHeight/2;
				}
				
				/* draw line */
				XDrawLine(tablePtr->display, pixmap,
#ifdef USE_BITMAP_COLOR
						  parentPtr->lineGC != None ? parentPtr->lineGC : tablePtr->defLineGC,
#else
						  tablePtr->defLineGC,
#endif
						  left_x, left_y, left_x2, left_y2);
			}
		}

		/*
		 * Draw +/-, if the item contains sons
		 */
		if (disp && Have_PlusMinus(tablePtr) && (itemPtr->succPtr != NULL || itemPtr->unknownFlag))
		{
			Tk_Image image;
		  	if (itemPtr->unknownFlag && tablePtr->unknownImage)
			{
				image = tablePtr->unknownImage;
			}
			else
			{
				image = hidden ? tablePtr->plusImage : tablePtr->minusImage;
			}
			Tk_RedrawImage(image,
						   0, 0,
						   tablePtr->plusWidth, tablePtr->plusHeight,
						   pixmap,
						   real_plus_x, real_plus_y);
		}
		
		if (disp && tablePtr->hiddenImage == NULL)
		{
			/* 
			 * If line in selection, use bitmapSelectGC
			 */
			if (itemPtr->flags&ITEM_SELECTED)
			{
#ifdef USE_BITMCAP_COLOR
				gc = tablePtr->selectGC;
				if (itemPtr->bitmapSelectGC != None)
				{
					gc = itemPtr->bitmapSelectGC;
				}
				else
				{
					if (tablePtr->defBitmapSelectGC != None)
					{
						gc = tablePtr->defBitmapSelectGC;
					}
					else
					{
						gc = tablePtr->selectGC;
					}
				}
#else
				if (tablePtr->defBitmapSelectGC != None)
				{
					gc = tablePtr->defBitmapSelectGC;
				}
				else if (tablePtr->selectGC != None)
				{
					gc = tablePtr->selectGC;
				}
				else
				{
					gc = tablePtr->defBitmapGC;
				}
#endif
			} else {
				if (itemPtr->bitmapGC != None)
				{
					gc = itemPtr->bitmapGC;
				}
				else
				{
					gc = tablePtr->defBitmapGC;
				}
			}
		}
			
		/*
		 * Draw hidden image/bitmap, if sub tree is closed (older style).
		 */
		if (disp && ! Have_PlusMinus(tablePtr) &&
			hidden &&
			(tablePtr->hiddenBitmap != None || tablePtr->hiddenImage != NULL))
		{
			/* draw image if available and no hidden image/bitmap availiable */
			if (tablePtr->hiddenImage)
			{
				Tk_RedrawImage(tablePtr->hiddenImage,
							   0, 0,
							   tablePtr->hiddenWidth, tablePtr->hiddenHeight,
							   pixmap,
							   real_bitm_x, real_bitm_y);
			}
			else
			{
				XCopyPlane(tablePtr->display, tablePtr->hiddenBitmap,
						   pixmap,
						   gc,
						   0, 0,
						   tablePtr->hiddenWidth, tablePtr->hiddenHeight,
						   real_bitm_x, real_bitm_y,
						   1);
			}
		}
		
		/*
		 * Draw the bitmap if an image/bitmap availiable
		 */
		else if (disp && (itemPtr->bitmap != None || itemPtr->image != NULL))
		{
			
			/* draw image if available and no hidden image/bitmap availiable */
			if (itemPtr->image)
			{
				Tk_RedrawImage(itemPtr->image,
							   0, 0,
							   itemPtr->bitmapWidth, itemPtr->bitmapHeight,
							   pixmap,
							   real_bitm_x, real_bitm_y);
			}
			else
			{
				XCopyPlane(tablePtr->display, itemPtr->bitmap,
						   pixmap,
						   gc,
						   0, 0,
						   itemPtr->bitmapWidth, itemPtr->bitmapHeight,
						   real_bitm_x, real_bitm_y,
						   1);
			}
		}
		
		if (disp)
		{
			if (itemPtr->flags&ITEM_SELECTED)
			{
				gc = tablePtr->selectGC;
			}
			else if (itemPtr->textGC != None)
			{
				gc = itemPtr->textGC;
			}
			else
			{
				gc = tablePtr->defTextGC;
			}
		}
		
		/*
		 * we support tab stops too
		 */
		if (tablePtr->tabsNum > 0 && tablePtr->tabs != NULL)
		{
			char *nextTab, *p = itemPtr->text;
			int len;
			int tab_x = real_text_x, tab_text_x = real_text_x;
			int i = 0, tab_width;
			int column_len;
			
#if _WINDOWS
			/*
			 * Convert paths to windows native paths
			 */
			if (tablePtr->nativeWindowsMode)
			{
				p = my_WindowsNativePath(itemPtr->text, itemPtr->textLength);
			}
#endif
			
			while (p != NULL)
			{
				NextChar (nextTab, p, '\t', column_len);
				/*
				 * len is variable and could be changed to shrink the
				 * displayed string in a column
				 */
				len = column_len;
				
				/*
				 * We need to now the width for every column
				 */
				tab_width = Tk_TextWidth (font, p, column_len);
				
				/* Calculate tab stops if auto fit is enabled*/
				if (!disp && tablePtr->AutoFit)
				{
					int xx = tab_width+tablePtr->tabsMinSpace;
					if (p == itemPtr->text)
					{
						xx += real_text_x + tablePtr->xOffset; /* contains previous image spaces */
					}
					if (tablePtr->tabs[i] < xx)
					{
						tablePtr->tabs[i] = xx;
					}
				}
				
				/* calculate max. width, if tab stops availiable */
				if (nextTab == NULL)
				{
					if (itemPtr->lineWidth != tab_text_x + tab_width + tablePtr->xOffset)
					{
						itemPtr->lineWidth = tab_text_x + tab_width + tablePtr->xOffset;
					}
					if (tablePtr->maxWidth < itemPtr->lineWidth)
					{
						tablePtr->maxWidth = itemPtr->lineWidth;
					}
				}
				
				/* if truncate is enabled, truncate the rest of the
				 * text if it overrides the next tab stop.
				 * It uses a binary search O(log n)
				 */
				if (tablePtr->Truncate && i<tablePtr->tabsNum-1)
				{
					int tmp_tab_offset;
					int tmp_tab_width, calc_tab_width;
					
					if (i == 0)
					{
						tmp_tab_offset = tab_text_x + tablePtr->xOffset;
						calc_tab_width = tmp_tab_offset+tab_width;
					}
					else
					{
						calc_tab_width = tab_width;
						tmp_tab_offset=0;
					}
					
					if (tablePtr->tabsHidden[i])
					{
#ifdef SPACE_PROBLEM
						tmp_tab_width = 0;
#else
						tmp_tab_width = tablePtr->tabsMinSpace;
#endif
					}
					else
					{
						tmp_tab_width = tablePtr->tabs[i];
					}
					if (calc_tab_width > tmp_tab_width-tablePtr->tabsMinSpace)
					{
						char *q = p;
						int step;
						/*
						 * use binary algorithm O(log n) to truncate the text
						 */
						len += 2;
						step = len;
						do
						{
							step /= 2;
							if (calc_tab_width > tmp_tab_width-tablePtr->tabsMinSpace)
							{
								len -= step;
							}
							else
							{
								len += step;
							}
							p = TreeTable_truncate_string (tablePtr, q, column_len, len);
							
							tab_width = Tk_TextWidth (font, p, len);
							if (i == 0)
							{
								calc_tab_width = tmp_tab_offset+tab_width;
							}
							else
							{
								calc_tab_width = tab_width;
							}
							
						} while (step > 1);
						
						/* 
						 * After the binary algorithm to truncate the text
						 * The text can be one or two characters larger than it
						 * fit in the tab column. Truncate it linear.
						 */
						while (len > 0 && calc_tab_width > tmp_tab_width-tablePtr->tabsMinSpace)
						{
							len --;
							p = TreeTable_truncate_string (tablePtr, q, column_len, len);
							
							tab_width = Tk_TextWidth (font, p, len);
							if (i == 0)
							{
								calc_tab_width = tmp_tab_offset+tab_width;
							}
							else
							{
								calc_tab_width = tab_width;
							}
						}
					}
				}
				
				if (disp && ! tablePtr->tabsHidden[i])
				{
					/* right-justify */
					if (tablePtr->tabsJustify && tablePtr->tabsJustify[i])
					{
						int tmp_tab_width, ww = Tk_TextWidth(font, p, len);
						if (tablePtr->tabsHidden[i])
						{
#ifdef SPACE_PROBLEM
							tmp_tab_width = 0;
#else
							tmp_tab_width = tablePtr->tabsMinSpace;
#endif
						}
						else
						{
							tmp_tab_width = tablePtr->tabs[i];
						}
						draw_text_x = tab_text_x + tmp_tab_width-ww-tablePtr->tabsMinSpace/2;
					}
					else
					{
						draw_text_x = tab_text_x + (i==0 ? 0 : tablePtr->tabsMinSpace/2);
					}

					Tk_DrawChars (tablePtr->display, pixmap, gc, font,
					        p, len,
					        draw_text_x, draw_text_y);

					if (disp && (*realpos == tablePtr->active) && (tablePtr->flags & GOT_FOCUS)) {
					    Tk_UnderlineChars(tablePtr->display, pixmap, gc, font,
					            p, draw_text_x, draw_text_y, 0, len);
					}	
				}
				
				/* calculate the next tab stop position */
				if (nextTab)
				{
					p = nextTab + 1;
				}
				else
				{
					p = nextTab;
				}
				
				if (tablePtr->tabsHidden[i])
				{
					if (i == 0)
					{
						tab_x += ITEM_TEXT_X(itemPtr);
					}
					else
					{
#ifdef SPACE_PROBLEM
						tab_x += 0;
#else
						tab_x += tablePtr->tabsMinSpace;
#endif
					}
				}
				else
				{
					tab_x += tablePtr->tabs[i];
				}
				
				/* if truncate is seted, we cut the rest of the string */
				if (!tablePtr->Truncate)
				{
					tab_text_x = Max (tab_text_x+tab_width+tablePtr->tabsMinSpace, tab_x)
							   - (real_text_x + tablePtr->xOffset);
				}
				else
				{
					tab_text_x = tab_x-(real_text_x + tablePtr->xOffset);
				}
				
				if (i<tablePtr->tabsNum-1)
				{
					i++;
				}
			}
		}
		else
		{
			if (disp)
			{
			  	char*p=itemPtr->text;
#if _WINDOWS
				/*
				 * Convert paths to windows native paths
				 */
				if (tablePtr->nativeWindowsMode)
				{
					p = my_WindowsNativePath(itemPtr->text, itemPtr->textLength);
				}
#endif
				draw_text_x = real_text_x;
				Tk_DrawChars (tablePtr->display, pixmap, gc, font,
							  p, itemPtr->textLength,
							  draw_text_x, draw_text_y);

				if (disp && (*realpos == tablePtr->active) && (tablePtr->flags & GOT_FOCUS)) {
				    Tk_UnderlineChars(tablePtr->display, pixmap, gc, font,
				            p, draw_text_x, draw_text_y, 0, itemPtr->textLength);
				}
			}
			if (itemPtr->lineWidth > tablePtr->maxWidth)
			{
				tablePtr->maxWidth = itemPtr->lineWidth;
			}
		}
		
		*seenpos += 1;
		*realpos += 1;
		if (itemPtr->succPtr != NULL && !hidden)
		{
			redisplay += DisplayRecursive
			  (tablePtr, itemPtr->succPtr, seenpos, realpos, limit, pixmap,
			   disp);
		}
		else
		{
			*realpos += itemPtr->succNum;
		}
		itemPtr = itemPtr->nextPtr;
	}
    return redisplay;
}

/*
 *--------------------------------------------------------------
 *
 * DisplayTreeTable --
 *
 *      This procedure redraws the contents of a treetable window.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      Information appears on the screen.
 *
 *--------------------------------------------------------------
 */
#define USE_PIXMAP
static void
DisplayTreeTable(clientData)
  ClientData clientData;            /* Information about window. */
{
    register TreeTable *tablePtr = (TreeTable *) clientData;
    register Tk_Window tkwin = tablePtr->tkwin;
    int limit, pos, realpos, oldMaxWidth;
    Pixmap pixmap=0;
    int redisplay;
    int i;

    /*
     * Check to see if the size of the widget has changed.
     * If it has then we may want to resize the column headers
     * (tabs) by exec a tcl script passed as -resizecommand.
     */

    if ((tablePtr->oldHeight != Tk_Height (tablePtr->tkwin)) ||
        (tablePtr->oldWidth != Tk_Width(tablePtr->tkwin))) {

        tablePtr->oldHeight = Tk_Height (tablePtr->tkwin);
        tablePtr->oldWidth = Tk_Width (tablePtr->tkwin);

        /*
         * The column headers need updating.
         */

        if (tablePtr->ResizeCmd != NULL) {
            int result;
            /*
             * We must hold onto the interpreter because the data referred to at
             * tablePtr might be freed as a result of the call to Tcl_VarEval.
             */
    
  
            Tcl_Preserve((ClientData) tablePtr->interp);
            result = Tcl_VarEval(tablePtr->interp, tablePtr->ResizeCmd, (char *) NULL,
						 (char *) NULL);
            if (result != TCL_OK) {
                Tcl_AddErrorInfo(tablePtr->interp,"\n (resize command executed by treetable)");
                Tcl_BackgroundError(tablePtr->interp);
            }
            Tcl_Release((ClientData) tablePtr->interp);
        }              

    }
    /*
     * Iterate through all of the items in the treetable,
     * displaying each in turn. 
     */
    limit = tablePtr->topIndex + tablePtr->numLines + 1;
    if (limit >= tablePtr->numItems)
	{
		limit = tablePtr->numItems-1;
	}
    
    /* calculate best auto fit for tab stops */
    if (tablePtr->AutoFit && tablePtr->tabsNum > 0)
	{
		for (i=0; i<tablePtr->tabsNum; i++)
		{
			tablePtr->tabs[i] = tablePtr->defTabs[i];
		}
    	pos = realpos = 0;
		tablePtr->maxWidth = 0;
    	DisplayRecursive(tablePtr, tablePtr->itemPtr,
						 &pos, &realpos,
						 limit, pixmap,
						 0);
	}
    
    tablePtr->flags &= ~REDRAW_PENDING;
    if (tablePtr->flags & UPDATE_V_SCROLLBAR)
	{
		TreeTableUpdateVScrollbar(tablePtr);
	}
    tablePtr->flags &= ~(REDRAW_PENDING|UPDATE_V_SCROLLBAR);
    if ((tablePtr->tkwin == NULL) || !Tk_IsMapped(tkwin))
	{
		return;
	}
	
    /*
     * Redrawing is done in a temporary pixmap that is allocated here and freed
     * at the end of the procedure.  All drawing is done to the pixmap,
     * and the pixmap is copied to the screen at the end of the procedure.
     * This provides the smoothest possible visual effects (no flashing
     * on the screen).
     */
#ifndef USE_PIXMAP
    pixmap = Tk_WindowId(tablePtr->tkwin);
    Tcl_Preserve((ClientData) tablePtr->interp);
#else
    pixmap = Tk_GetPixmap(tablePtr->display, Tk_WindowId(tkwin),
						  Tk_Width(tkwin), Tk_Height(tkwin),
						  Tk_Depth(tkwin));
#endif
	
    Tk_Fill3DRectangle(tkwin, pixmap, tablePtr->normalBorder,
					   0, 0, Tk_Width(tkwin), Tk_Height(tkwin), 
					   tablePtr->borderWidth, tablePtr->relief);
    
    oldMaxWidth = tablePtr->maxWidth;
    tablePtr->maxWidth = 0;
    pos = 0;
    realpos = 0;
    redisplay = DisplayRecursive (tablePtr, tablePtr->itemPtr,
								  &pos, &realpos,
								  limit, pixmap,
								  1);
	
    /* if drawing lines is enabled */
    if (tablePtr->SplitLines && tablePtr->tabsNum > 0)
	{
    	int px = tablePtr->tabsMinSpace/2 - tablePtr->xOffset - 1;
		int hight = Tk_Height (tablePtr->tkwin);
		for (i=0; i<tablePtr->tabsNum-1; i++)
		{
			if (tablePtr->tabsHidden[i])
			{
				if (i==0 && tablePtr->itemPtr!=NULL)
				{
					px += ITEM_TEXT_X(tablePtr->itemPtr);
				}
				else
				{
#ifdef SPACE_PROBLEM
					px += 0;
#else
					px += tablePtr->tabsMinSpace;
#endif
				}
			}
			else
			{
				px += tablePtr->tabs[i];
			}
			if (px > 0)
			{
				XDrawLine (tablePtr->display, pixmap, tablePtr->defSplitLineGC,
						   px,
						   0,
						   px, hight);
			}
		}
	}
    if ((tablePtr->flags & UPDATE_H_SCROLLBAR) || oldMaxWidth != tablePtr->maxWidth)
	{
        TreeTableUpdateHScrollbar(tablePtr);
    	tablePtr->flags &= ~UPDATE_H_SCROLLBAR;
	}
    
    /*
     * Redraw the border to ensure it convers the text items of
     * the lines in the treetable.
     */
    Tk_Draw3DRectangle(tkwin, pixmap, 
					   tablePtr->normalBorder, 0, 0,
					   Tk_Width(tkwin), Tk_Height(tkwin),
					   tablePtr->borderWidth,
					   tablePtr->relief);
	
    if (tablePtr->highlightWidth > 0 && tablePtr->highlightBgColor)
	{
		GC gc;
		
		if (tablePtr->flags & GOT_FOCUS)
		{
			gc = Tk_GCForColor(tablePtr->highlightColor, pixmap);
		} else
			{
			  gc = Tk_GCForColor(tablePtr->highlightBgColor, pixmap);
			}
		Tk_DrawFocusHighlight(tkwin, gc, tablePtr->highlightWidth, pixmap);
	}
    
    /* if idle command is seted, execute it */
    if (tablePtr->idlecommand != NULL)
	{
		Tcl_Eval (tablePtr->interp, tablePtr->idlecommand);
	}
    
#ifdef USE_PIXMAP
    XCopyArea(tablePtr->display, pixmap, Tk_WindowId(tkwin),
			  tablePtr->defTextGC, 0, 0, Tk_Width(tkwin),
			  Tk_Height(tkwin), 0, 0);
    Tk_FreePixmap(tablePtr->display, pixmap);
#else
    Tcl_Release((ClientData) tablePtr->interp);
#endif
}

/*
 * This procedure is called, when an item is added or deleted and
 * the flag 'bestfit' is enabled
 */
static void
ComputeTabStops (TreeTable*tablePtr, TableItem *itemPtr)
{
	char *nextTab, *p = itemPtr->text;
    int len;
    int tx, px;
    int i = 0, width;
    int real_x, x, x1;
    
    if (tablePtr->tabs == NULL)
	{
		return;
	}
    
    /* first x position fot text displaying */
    real_x = tablePtr->inset - tablePtr->xOffset;
    x = real_x + itemPtr->indent*tablePtr->indentWidth;
    x1 = x + tablePtr->bitmapSpace;
    if (itemPtr->bitmap != None || itemPtr->image)
	{
		x1 += itemPtr->bitmapWidth;
	}
    tx = real_x;
    px = x1;
    
    while (p != NULL)
	{
		NextChar (nextTab, p, '\t', len);
		
		/*
		 * there is no tab stops availiable, so we don't need to
		 * recalc the list width
		 */
		width = Tk_TextWidth ((itemPtr->fontPtr != NULL)
							  ? itemPtr->fontPtr : tablePtr->defFontPtr,
							  p, len);
		
		
		/* Calculate best fit */
		if (tablePtr->BestFit)
		{
			int xx = width+tablePtr->tabsMinSpace;
			if (p == itemPtr->text)
			{
				int x11;
				x11 = itemPtr->indent*tablePtr->indentWidth
				  + tablePtr->bitmapSpace
				  + itemPtr->bitmapWidth
				  + tablePtr->inset;
				xx += x11;
			}
			if (tablePtr->tabs[i] < xx)
			{
				tablePtr->tabs[i] = xx;
			}
		}
		
		/* calculate max. width */
		if (nextTab == NULL)
		{
			if (itemPtr->lineWidth != px + width)
			{
				itemPtr->lineWidth = px + width + tablePtr->xOffset;
			}
			if (tablePtr->maxWidth < itemPtr->lineWidth)
			{
				tablePtr->maxWidth = itemPtr->lineWidth;
			}
		}
		
		/* calculate the next tab stop position */
		if (nextTab)
		{
			p = nextTab + 1;
		}
		else
		{
			p = nextTab;
		}
		tx += tablePtr->tabs[i];
		
		px = tx;
		
		if (i<tablePtr->tabsNum-1)
		{
			i++;
		}
	}
}

static int
TreeTableFindIndex_x (TableItem *itemPtr, TableItem *srcPtr,
					  int index, int* pos)
{
    TableItem *Ptr;
	int i = index;
	for (Ptr=itemPtr; i>=0 && Ptr; Ptr=Ptr->nextPtr)
	{
    	if (Ptr == srcPtr)
		{
			*pos = i;
			return -1;
		}
		i++;
		if (Ptr->succPtr != NULL)
		{
			i = TreeTableFindIndex_x (Ptr->succPtr, srcPtr, i, pos);
		}
	}
    return i;
}

static int
TreeTableFindIndex(tablePtr, itemPtr) 
  TreeTable *tablePtr;    /* treetable to search in */
TableItem *itemPtr;     /* the item we're looking for */
{
    int pos = -1;
    
    RET_CACHED_INDEX (itemPtr);
    
    TreeTableFindIndex_x (tablePtr->itemPtr, itemPtr, 0, &pos);
    
    /*SET_CACHED(itemPtr, pos);*/
    
    return pos;
}

/*
 *----------------------------------------------------------------------
 *
 * TreeTableSelectImageProc --
 *
 *	This procedure is invoked by the image code whenever the manager
 *	for an image does something that affects the size of contents
 *	of the image displayed in a tree table when it is selected.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	May arrange for the button to get redisplayed.
 *
 *----------------------------------------------------------------------
 */
static void
TreeTableImageProc(clientData, x, y, width, height, imgWidth, imgHeight)
  ClientData clientData;		/* Pointer to widget record. */
  int x, y;				/* Upper left pixel (within image)
						 * that must be redisplayed. */
  int width, height;			/* Dimensions of area to redisplay
					 * (may be <= 0). */
  int imgWidth, imgHeight;		/* New dimensions of image. */
{
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureTreeTableItem --
 *
 *      Add a new item to the tree table.
 *      
 * Results:
 *      None.
 *
 * Side effects:
 *      New information gets added to tablePtr;  it will be redisplayed
 *      soon, but not immediately.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureTreeTableItem(tablePtr, itemPtr, argc, argv, flags, mode) 
  TreeTable *tablePtr;             /* table the item is located in */
  TableItem  *itemPtr;             /* Item to configure */
  int argc;                        /* number of arguments */
  char **argv;                     /* argument strings */
  int flags;                       /* Flags to pass to Tk_ConfigureWidget */
  int mode;
{
    XGCValues gcValues;
    XColor *oldtextfg;
#if USE_BITMAP_COLORS
    XColor *oldbitselfg, *oldbitselbg, *oldbitbg, *oldlinefg, *oldbitfg;
#endif
    Pixmap oldbitmap;
    Tk_Font oldfont;
    Tk_FontMetrics metrics;
    char *oldtext;
    int oldparent;
    int oldmaxwidth;
    int oldlinewidth;
    int oldindent;
    int recompute_height = 0;
    int recompute_width = 0, oldSuccNum, oldSeenNum;
    char *oldImageString = NULL;
	
    oldbitmap = itemPtr->bitmap;
    oldtext = itemPtr->text;
    oldtextfg = itemPtr->textFgColor;
#if USE_BITMAP_COLORS
    oldbitfg = itemPtr->bitmapFgColor;
    oldbitbg = itemPtr->bitmapBgColor;
    oldbitselfg = itemPtr->bitmapSelectFgColor;
    oldbitselbg = itemPtr->bitmapSelectBgColor;
    oldlinefg = itemPtr->lineFgColor;
#endif
    oldfont = itemPtr->fontPtr;
    oldmaxwidth = tablePtr->maxWidth;
    oldparent = itemPtr->parent;
    oldindent = itemPtr->indent;
    oldlinewidth = itemPtr->lineWidth;
    oldImageString = itemPtr->imageString;
    oldSuccNum = itemPtr->succNum;
    oldSeenNum = itemPtr->seenNum;
    
    if (Tk_ConfigureWidget(tablePtr->interp, tablePtr->tkwin, itemConfigSpecs,
						   argc, argv, (char *) itemPtr, flags) != TCL_OK) {
	  
	  return TCL_ERROR;
}
    
    /** succnum field is read only */
    if (oldSuccNum != itemPtr->succNum)
	{
		itemPtr->succNum = oldSuccNum;
		Tcl_AppendResult (tablePtr->interp,
						  "children number is readonly field and can't be changed",
						  NULL);
		return TCL_ERROR;
	}
    /** seennum field is read only */
    if (oldSeenNum != itemPtr->seenNum)
	{
		itemPtr->seenNum = oldSeenNum;
		Tcl_AppendResult (tablePtr->interp,
						  "seen number of children is readonly and can't be changed",
						  NULL);
		return TCL_ERROR;
	}
    
    if (itemPtr->indent != oldindent)
	{
		recompute_width = 1;
	}
    if ((oldtextfg != itemPtr->textFgColor) || (oldfont != itemPtr->fontPtr))
	{
		if (itemPtr->textFgColor != None)
		{
			gcValues.foreground = itemPtr->textFgColor->pixel;
		}
		else
		{
			gcValues.foreground = tablePtr->defTextFgColor->pixel;
		}
		if (itemPtr->fontPtr != None)
		{
			gcValues.font = Tk_FontId (itemPtr->fontPtr);
		}
		else
		{
			gcValues.font = Tk_FontId (tablePtr->defFontPtr);
		}
		gcValues.graphics_exposures = False;
		if (itemPtr->textGC != None)
		{
			Tk_FreeGC(tablePtr->display, itemPtr->textGC);
		}
		if (oldfont != itemPtr->fontPtr)
		{
			Tk_GetFontMetrics (itemPtr->fontPtr, &metrics);
			itemPtr->fontHeight = metrics.linespace;
			recompute_height = 1;
		}
		itemPtr->textGC = Tk_GetGC(tablePtr->tkwin,
								   GCForeground|GCFont|GCGraphicsExposures,
								   &gcValues);
	}
#if USE_BITMAP_COLORS
    if ((oldbitfg != itemPtr->bitmapFgColor) || 
		(oldbitbg != itemPtr->bitmapBgColor))
	{
		if (itemPtr->bitmapFgColor != None)
		{
			gcValues.foreground = itemPtr->bitmapFgColor->pixel;
		}
		else
		{
			gcValues.foreground = tablePtr->defBitmapFgColor->pixel;
		}
		if (itemPtr->bitmapBgColor != None)
		{
			gcValues.background = itemPtr->bitmapBgColor->pixel;
		}
		else
		{
			gcValues.background = tablePtr->defBitmapBgColor->pixel;
		}
		gcValues.graphics_exposures = False;
		if (itemPtr->bitmapGC != None)
		{
			Tk_FreeGC(tablePtr->display, itemPtr->bitmapGC);
		}
		itemPtr->bitmapGC = Tk_GetGC(tablePtr->tkwin, 
									 GCForeground|GCBackground|
									 GCGraphicsExposures, &gcValues);
	}
	
    if ((oldbitselfg != itemPtr->bitmapSelectFgColor) ||
		(oldbitselbg != itemPtr->bitmapSelectBgColor))
	{
		if (itemPtr->bitmapSelectFgColor != None)
		{
			gcValues.foreground = itemPtr->bitmapSelectFgColor->pixel;
		}
		else
		{
			gcValues.foreground = tablePtr->defBitmapSelectFgColor->pixel;
		}
		if (itemPtr->bitmapSelectBgColor != None)
		{
			gcValues.background = itemPtr->bitmapSelectBgColor->pixel;
		}
		else
		{
			/* Zsolt Koppany 5-jul-96 */
			if (!tablePtr->defBitmapSelectBgColor)
			  gcValues.background = tablePtr->defBitmapFgColor->pixel;
			else
			  gcValues.background = tablePtr->defBitmapSelectBgColor->pixel;
		}
		gcValues.graphics_exposures = False;
		if (itemPtr->bitmapSelectGC != None)
		{
			Tk_FreeGC(tablePtr->display, itemPtr->bitmapSelectGC);
		} 
		itemPtr->bitmapSelectGC = Tk_GetGC(tablePtr->tkwin,
										   GCForeground|GCBackground|
										   GCGraphicsExposures, &gcValues);
	}
#endif
	
    if (itemPtr->lineWidth == atoi(NO_TABLEITEM_LINEWIDTH))
	{
		itemPtr->lineWidth = tablePtr->defLineWidth;
	}
#ifdef USE_BITMAP_COLOR
    if ((oldlinefg != itemPtr->lineFgColor) ||
		(oldlinewidth != itemPtr->lineWidth))
	{
		if (itemPtr->lineFgColor != None)
		{
			gcValues.foreground = itemPtr->lineFgColor->pixel;
		}
		else
		{
			gcValues.foreground = tablePtr->defLineFgColor->pixel;
		}
		gcValues.line_width = itemPtr->lineWidth;
		gcValues.graphics_exposures = False;
		if (itemPtr->lineGC != None)
		{
			Tk_FreeGC(tablePtr->display, itemPtr->lineGC);
		}
		itemPtr->lineGC = Tk_GetGC(tablePtr->tkwin, 
								   GCForeground|GCGraphicsExposures|GCLineWidth,
								   &gcValues);
	}
#endif
    if ((oldtext != itemPtr->text) || (oldfont != itemPtr->fontPtr))
	{
		if (itemPtr->text != (char *) NULL)
		{
			itemPtr->textLength = strlen(itemPtr->text);
		}
		else
		{
			itemPtr->textLength = 0;
		}
		recompute_width = 1;
	}
    
    if (oldbitmap != itemPtr->bitmap)
	{
		if (itemPtr->bitmap != None)
		{
			int width, height;
			Tk_SizeOfBitmap(tablePtr->display, itemPtr->bitmap,
							&width, &height);
			itemPtr->bitmapWidth = (short) width;
			itemPtr->bitmapHeight = (short) height;
		}
		recompute_height = recompute_width = 1;
	}
    /* load image **/
    else if (oldImageString != itemPtr->imageString)
	{
		/* find the image in the list */
		ImageList_t *img, *prevImg;
		for (img=prevImg=tablePtr->Images; img; prevImg=img, img = img->next)
		{
			if (strcmp (img->name, itemPtr->imageString) == 0)
			{
				break;
			}
		}
		/*
		 * Image not found, create it
		 */
		if (img == NULL)
		{
			img = (ImageList_t*)ckalloc (sizeof (ImageList_t));
			memset(img, 0, sizeof (ImageList_t));
			if (img == NULL)
			{
				return TCL_ERROR;
			}
			img->name = (char*)ckalloc (strlen (itemPtr->imageString)+1);
			strcpy (img->name, itemPtr->imageString);
			img->image = Tk_GetImage(tablePtr->interp, tablePtr->tkwin,
									 itemPtr->imageString,
									 TreeTableImageProc,
									 (ClientData) NULL);
			if (img->image == NULL)
			{
				return TCL_ERROR;
			}
			Tk_SizeOfImage(img->image, &img->width, &img->height);
			/* add the created image to the list */
			if (prevImg)
			{
				prevImg->next = img; /* add at the end of list (Sorting?) */
			}
			else
			{
				tablePtr->Images = img; /* first item */
			}
		}
		itemPtr->image = img->image;
		itemPtr->bitmapWidth  = (short)img->width;
		itemPtr->bitmapHeight = (short)img->height;
		recompute_height = recompute_width = 1;
	}
    
    if (! (mode & CONFIG_OPTIONS_ONLY) && recompute_height)
	{
		int oldlh;
		
		oldlh = tablePtr->lineHeight;
		TreeTableComputeLineHeight(tablePtr, itemPtr, 0);
		if (oldlh != tablePtr->lineHeight)
		{
			tablePtr->flags |= UPDATE_V_SCROLLBAR;
		}
	}
    if ((mode & CONFIG_OPTIONS_ONLY) == 0 && recompute_width)
	{
		TreeTableComputeWidths(tablePtr, itemPtr, 0);
		if (oldmaxwidth != tablePtr->maxWidth)
		{
			tablePtr->flags |= UPDATE_H_SCROLLBAR;
		}
	}	
    if (! (mode & CONFIG_OPTIONS_ONLY))
	{
    	TreeTableRedrawRange(tablePtr);
	}
    return TCL_OK;
}

/*
 * find items in a given range and append result to
 * the Tcl iterpreter result string
 */
/*{*/
static int
TreeTableGetItems_x (TreeTable *tablePtr, TableItem *itemPtr,
					 int index, int from, int to)
{
    TableItem *item = itemPtr;
	int i;
	
	for (i=index; i<=to && item != NULL; item = item->nextPtr)
	{
		if (i >= from && i<= to)
		{
			Tcl_AppendElement (tablePtr->interp, item->text);
		}
		i++;
		if (item->succPtr != NULL)
		{
			/* we don't need to seach in sub trees, when succ number does't
			 * succeed
			 */
			if (i+item->succNum < from)
			{
				i += item->succNum;
			}
			else
			{
				i = TreeTableGetItems_x (tablePtr, item->succPtr, i, from, to);
			}
		}
	}
	
	return i;
}

static void
TreeTableGetItems (TreeTable *tablePtr, int from, int to)
{
    TreeTableGetItems_x (tablePtr, tablePtr->itemPtr, 0,
						 Min (from, to), Max(from, to));
}
/*}*/

/*{*/
static int
TreeTableIndex (TableItem *itemPtr, int index, int pos, TableItem **itemRet)
{
    TableItem *item = itemPtr;
	int i;
	
	for (i=pos; i<=index && item != NULL; item = item->nextPtr)
	{
		if (i == index)
		{
			*itemRet = item;
			return -1;
		}
		i++;
		if (item->succPtr != NULL)
		{
			/* we don't need to seach in sub trees, when succ number does't
			 * succeed
			 */
			if (i+item->succNum < index)
			{
				i += item->succNum;
			}
			else
			{
				i = TreeTableIndex (item->succPtr, index, i, itemRet);
				if (i < 0)
				{
					return i;
				}
			}
		}
	}
	
	return i;
}
static TableItem * TreeTableFindItem (TreeTable *tablePtr, int index)
{
    int pos = 0;
    TableItem* itemRet = NULL;
    TableItem* itemPtr = tablePtr->itemPtr;
	
    if (index < 0)
	  return itemRet;
	
    RET_CACHED_ITEM(index);
    
    /* we can use the accelerator to speed up the location of
     * the item
     */
    FIND_IN_PATHFINDER(index, itemPtr, pos);
	
    TreeTableIndex (itemPtr, index, pos, &itemRet);
	
    if (itemRet)
	{
		SET_CACHED(itemRet, index);
	}
	
    return itemRet;
}
/*}*/

/*{*/
/* find not hidden element
 * next == 0 : return parent, if the element is hidden
 * next == 1 : return next not hidden item
 */
static int
TreeTableFindNotHiddenItem_x (TableItem *itemPtr, int index,
							  int pos, TableItem **itemRet, int next)
{
    TableItem *item = itemPtr;
	int i;
	
	for (i=pos; item != NULL; item = item->nextPtr)
	{
		if (i >= index)
		{
			*itemRet = item;
			return -1;
		}
		i++;
		if (item->succPtr != NULL)
		{
			/* don't look for hidden sub trees */
			if ((item->flags & ITEM_HIDDEN_SUBTREE) || i+item->succNum < index)
			{
				i += item->succNum;
			}
			else
			{
				i = TreeTableFindNotHiddenItem_x (item->succPtr, index, i, itemRet, next);
				if (i < 0)
				{
					return i;
				}
			}
		}
		if (i > index && next <= 0)
		{
			*itemRet = item;
			return -1;
		}
	}
	
	return i;
}
static TableItem * TreeTableFindNotHiddenItem (TreeTable *tablePtr, int index, int next)
{
    TableItem* itemRet = NULL;
	TableItem* Ptr = tablePtr->itemPtr;
	int pos = 0;
	
	if (index < 0)
	  return itemRet;
	
	/*FIND_IN_PATHFINDER(index, Ptr, pos);*/
	
	/*RET_CACHED_ITEM(index);*/
	
	TreeTableFindNotHiddenItem_x (Ptr, index, pos, &itemRet, next);
	
	/*SET_CACHED (itemRet, index);*/
	
	return itemRet;
}
/*}*/

/*
 * function return a boolean value if the item is inserted in a hidden
 * sub tree (not viewed)
 */
static int
TreeTableInsertListItem (register TreeTable* tablePtr,
						 register TableItem*newPtr,
						 register TableItem*prevPtr,
						 register int index,
						 register int *newIndex)
{
    register TableItem* parentPtr = NULL, *posPtr = NULL;
    register TableItem* thisPtr = NULL;
    register int hidden;
	
    /* add item after given position */
    if (prevPtr != NULL)
	{
		newPtr->parentPtr = prevPtr->parentPtr;
		newPtr->parent    = prevPtr->parent;
		newPtr->indent    = prevPtr->indent;
		newPtr->nextPtr   = prevPtr->nextPtr;
		prevPtr->nextPtr  = newPtr;
	}
    /* find position to add new item */
    else
	{
		/*
		 * find parent
		 */
		if (newPtr->parent >= 0)
		{
#ifdef USE_PARENT_CACHE
			parentPtr = GET_CACHED_PARENT(tablePtr, newPtr->parent);
#else
			parentPtr = TreeTableFindItem (tablePtr, newPtr->parent);
#endif
			if (parentPtr != NULL)
			{
				newPtr->parentPtr = parentPtr;
				newPtr->indent    = parentPtr->indent + 1;
				SET_PARENT_CACHE (parentPtr, newPtr->parent);
			}
			else
			{
				fprintf (stderr, "TreeTableInsertListItem: parent %i not availiable\n",
						 newPtr->parent);
			}
		}
		
		if (parentPtr != NULL)
		{
			/* calculate the correct position of the new item */
			*newIndex = newPtr->parent+1;
			
			/* first item on the list */
			if (parentPtr->succPtr == NULL)
			{
				parentPtr->succPtr = newPtr;
			}
			else
			{
				/* find the previos item in the succ list */
				posPtr = NULL;
				if (parentPtr->succPtr != NULL)
				{
					int i;
					/* if sorted insertion is enabled, search for the correct position
					 * in the children list
					 */
					if (tablePtr->sortedInsertion)
					{
						posPtr = TreeTableSortFindPrev
						  (parentPtr->succPtr,
						   newPtr,
						   tablePtr->sortColumn,
						   tablePtr->sortNoCase,
						   newIndex);
					}
					else
					{
						GET_CACHED_ITEM(posPtr, index-1);
						if (posPtr == NULL || posPtr->parentPtr != parentPtr)
						{
							i = index - newPtr->parent - 1;
							thisPtr = parentPtr->succPtr;
							for (; thisPtr != NULL && i>0; thisPtr = thisPtr->nextPtr, i--)
							{
								*newIndex += 1 + thisPtr->succNum;
								posPtr = thisPtr;
							}
						}
						else
						{
							*newIndex = index;
						}
					}
				}
				
				/* add item as first element in the list */
				if (posPtr == NULL)
				{
					newPtr->nextPtr = parentPtr->succPtr;
					parentPtr->succPtr = newPtr;
				}
				/* insert item in the middle or at end of list */
				else
				{
					newPtr->nextPtr = posPtr->nextPtr;
					posPtr->nextPtr = newPtr;
				}
			}
		}
		else
		{
			/* first position */
			*newIndex = 0;
			
			/* insert at begin of list */
			if (index <= 0 || tablePtr->itemPtr == NULL)
			{
				newPtr->nextPtr = tablePtr->itemPtr;
				tablePtr->itemPtr = newPtr;
				
			}
			else
			{
				/* if sorted insertion is enabled, search for the correct position
				 * in the children list
				 */
				if (tablePtr->sortedInsertion)
				{
					posPtr = TreeTableSortFindPrev
					  (tablePtr->itemPtr,
					   newPtr,
					   tablePtr->sortColumn,
					   tablePtr->sortNoCase,
					   newIndex);
				}
				else
				{
					/* find item before the insertion position */
					posPtr = TreeTableFindItem (tablePtr, index - 1);
					if (posPtr)
					{
						*newIndex = index;
					}
				}
				
				/*
				 * insert the new item after found position
				 */
				if (posPtr != (TableItem *) NULL) 
				{
					/* we must be sure, that the item is added at root
					 * without any parent
					 */
					for (; posPtr->parentPtr != NULL; posPtr=posPtr->parentPtr)
					  ;
					
					newPtr->parentPtr = posPtr->parentPtr;
					newPtr->indent    = posPtr->indent;
					
					newPtr->nextPtr = posPtr->nextPtr;
					posPtr->nextPtr = newPtr;
					
				}
				/* insert item on top of list */
				else
				{
					newPtr->nextPtr = tablePtr->itemPtr;
					tablePtr->itemPtr = newPtr;
				}
			}
		}
	}
    
    /* increment item number of parents to quicke searching */
    hidden = 0;
    for (parentPtr=newPtr->parentPtr; parentPtr; parentPtr=parentPtr->parentPtr)
	{
		if (parentPtr->flags&ITEM_HIDDEN_SUBTREE)
		{
			hidden = 1;
		}
		parentPtr->succNum ++;
		if (!hidden)
		{
			parentPtr->seenNum ++;
		}
	}
    
    FREE_RONG_CACHE (*newIndex);
    FREE_RONG_PARENT_CACHE(*newIndex);
    SET_CACHED (newPtr, *newIndex);
    
    /* Delete entries from Pathfinder after this position */
    DELETE_FROM_PATHFINDER(*newIndex);
    
    return hidden;
}
/*
 *----------------------------------------------------------------------
 *
 * TreeTableInsertItem --
 *
 *      Add a new item to the tree table.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      New information gets added to tablePtr;  it will be redisplayed
 *      soon, but not immediately.
 *
 *----------------------------------------------------------------------
 */
static TableItem*
TreeTableInsertItem(register TreeTable *tablePtr,   /* TreeTable that is to get the new
													 * items. */
					register int index,                      /* Add the new items before this
															  * line */
					register TableItem* prevPtr,
					register int argc,                       /* argument count for argv */
					register char **argv)                    /* argument strings */
{
    register TableItem *newPtr;
	int hidden;
    int oldmaxwidth, newIndex = -1;
	
    /*
     * Find the item before which the new ones will be inserted.
     */
    if (index <= 0)
	{
		index = 0;
	}
    if (index > tablePtr->numItems)
	{
		index = tablePtr->numItems;
	}
    
    newPtr = (TableItem *) ckalloc(sizeof(TableItem));
	memset (newPtr, 0, sizeof(TableItem));
    if (newPtr==NULL)
	{
		fprintf (stderr, "TreeTableInsertItem: FATAL ERROR: Can't allocate memory\n");
		return NULL;
	}
	
    /*
     * initialize new item 
     */
    newPtr->lineWidth    = tablePtr->defLineWidth;
    newPtr->parent       = -1;
    
    newPtr->fontHeight = tablePtr->defFontHeight;
    
    /*
     * Call lineconfigure for creation options 
     */
    if (ConfigureTreeTableItem(tablePtr, newPtr, argc,
							   argv,
							   TK_CONFIG_ARGV_ONLY,
							   CONFIG_OPTIONS_ONLY)
		!= TCL_OK)
	{
		ViewArgs ("couldn't add item", argc, argv, 0);
		
		TreeTableRemoveItem (tablePtr, newPtr, -1, NULL, 0, 0);
		return NULL;
	}
#if 0
    if (newPtr->text == NULL)
	{
    	ViewArgs ("FATAL ERROR: item->text == NULL", argc, argv);
		TreeTableRemoveItem (tablePtr, newPtr, -1, NULL, 0, 0);
		return NULL;
	}
#endif
    
    /*
     * Khamis: 26-03-97
     * Add item to the list structure
     */
    hidden = TreeTableInsertListItem (tablePtr, newPtr, prevPtr, index, &newIndex);
    InsertedNewPosition = newIndex;
    
    /* 
     * Update item count, adjust selection.
     */
    tablePtr->numItems++;
    
    /* correct anchor and active */
    if (tablePtr->selectAnchor >= index)
	{
		tablePtr->selectAnchor ++;
	}
    if (tablePtr->active >= index)
	{
		tablePtr->active ++;
	}
    
    /* recompute width and height if item is viewed and not in the
     * hidden sub tree
     */
    if (!hidden)
	{
		TreeTableComputeLineHeight(tablePtr, newPtr, 1);
		tablePtr->flags |= UPDATE_V_SCROLLBAR;
		
		oldmaxwidth = tablePtr->maxWidth;
		TreeTableComputeWidths(tablePtr, newPtr, 0);
		if (oldmaxwidth != tablePtr->maxWidth)
		{
			tablePtr->flags |= UPDATE_H_SCROLLBAR;
		}
		
		TreeTableRedrawRange(tablePtr);
	}
    if (tablePtr->BestFit)
	{
		ComputeTabStops (tablePtr, newPtr);
	}
    return newPtr;
}

/*
 * add many items on the same position with same options **
 */
static int
TreeTableInsertItems (TreeTable* tablePtr,
					  int index, Tcl_Obj *itemlist,
					  int argc, Tcl_Obj *objv[])
{
    TableItem*newPtr=NULL;
	int i, oldmaxwidth, size;
	int nargc = argc + 2, targpos;
    static char **nargv = NULL;
    int ret = TCL_OK;
    int itemnum;
	Tcl_Obj *next = NULL;
    
    /*
     * free older allocations
     */
    if (nargv != NULL)
    {
    	ckfree ((char*)nargv);
    }
    nargv = (char**)ckalloc (sizeof(char**)*nargc);
    for (targpos=0; targpos<argc; targpos++)
	{
    	nargv[targpos] = O_STR(targpos);
	}
    nargv[targpos++] = "-text";
    
	if (Tcl_ListObjLength (tablePtr->interp, itemlist, &itemnum) != TCL_OK)
		return TCL_ERROR;
		
    for (i = 0; i<itemnum; i++)
	{
		if (Tcl_ListObjIndex (tablePtr->interp, itemlist, i, &next) != TCL_OK)
		{
			break;
		}
		
		nargv[targpos] = Tcl_GetStringFromObj (next, &size);
		if (size == 0)
		{
			continue; /* don't add empty strings (core dump) */
		}
		
		newPtr = TreeTableInsertItem(tablePtr, index, newPtr, nargc, nargv);
		if (newPtr == NULL)
		{
			ret = TCL_ERROR;
			break;
		}
		if (!tablePtr->tabs)
		{
			oldmaxwidth = tablePtr->maxWidth;
			TreeTableComputeWidths(tablePtr, newPtr, 0);
			if (oldmaxwidth != tablePtr->maxWidth)
			{
				tablePtr->flags |= UPDATE_H_SCROLLBAR;
			}
		}
	}
    return ret;
}

/*
 * Delete only the line indicated, which requires us to remove all
 * references to this node as a parent.
 */
static void
TreeTableDeleteRange(tablePtr, start, end) 
  TreeTable *tablePtr;    /* table in which target item resides */
  int start,end;          /* range of items to delete */ 
{
    TableItem *posPtr,*Ptr;
    int i;
    int oldmaxwidth, width = 0, children = 0;
	
    /* First check to make sure there any items to delete.  If there
     * are not, simply return TCL_OK */
    if (!tablePtr->numItems || start > end)
	{
		return;
	}
    
    /* delete all */
    if (start <= 0 && end >= tablePtr->numItems-1)
	{
		int j;
		children = 1;
		i = j = 0;
		for (posPtr=tablePtr->itemPtr; posPtr != NULL;)
		{
			Ptr=posPtr->nextPtr;
			j = i + 1 + posPtr->succNum;
			TreeTableRemoveItem (tablePtr, posPtr, i, &width, 0, children);
			i = j;
			posPtr=Ptr;
		}
		FREE_RONG_CACHE(0);
		FREE_RONG_PARENT_CACHE(0);
		tablePtr->itemPtr = NULL;
	}
    else for (i=end; i>=start; i--)
	{
    	posPtr = TreeTableFindItem(tablePtr,i);
		if (posPtr == NULL)
		{
			continue;
		}
		TreeTableRemoveItem (tablePtr, posPtr, i, &width, 0, children);
	}
    
    /* if BestFit is enabled and no items are availiable
     * then set the tab stops to the default value
     */
    if (tablePtr->BestFit && tablePtr->numItems == 0)
	{
		for (i=0; i<tablePtr->tabsNum; i++)
		{
			if (tablePtr->tabsHidden[i])
			{
				tablePtr->tabs[i] = tablePtr->tabsMinSpace;
			}
			else
			{
				tablePtr->tabs[i] = tablePtr->defTabs[i];
			}
		}
	}
    
    /* if width is changed */
    if (width >= tablePtr->maxWidth)
	{
		oldmaxwidth = tablePtr->maxWidth;
		TreeTableComputeWidths (tablePtr, (TableItem *) NULL, 1);
		if (oldmaxwidth != tablePtr->maxWidth)
		{
			tablePtr->flags |= UPDATE_H_SCROLLBAR;
		}
	}
    TreeTableComputeLineHeight (tablePtr, NULL, 0);
    tablePtr->flags |= UPDATE_V_SCROLLBAR;
    
    /* correct top index */
    if (tablePtr->topIndex > tablePtr->numViewed - tablePtr->numLines)
	  tablePtr->topIndex = Max (0, tablePtr->numViewed - tablePtr->numLines);
	
    TreeTableRedrawRange(tablePtr);
}

/*
 * Delete the indicated item and all of its descendants.
 *
 * return count of deleted items
 */
static int
TreeTableRemoveItem(TreeTable *tablePtr,        /* table in which the target item resides */
					TableItem *itemPtr,         /* item to delete */
					int lineNum,
					int* lineWidth,
					int original_item,
					int children                /* only true if this is the original item
												 * marked for deletion.  Children of the
												 * target item will be deleted, but this
												 * flag will not be set for them. */
					)
{
    TableItem *Ptr, *tmpPtr, *prevPtr, *topPtr;
	int i, index;
	int deleted = 1, hidden, width;
	
    /*
     * go through and mark references to parents after us as being
     * one line less, and delete any children we have.  Have to use
     * indexes instead of following the linked list because the 
     * list itself will be modified as we travel through the loop.
     */
    if (itemPtr == (TableItem *) NULL)
	{
		fprintf(stderr, "TreeTableRemoveItem: Attempt to delete NULL item.\n");
		return 0;
	}
    
    /* correct selection range */
    if (lineNum == -1)
	{
		index = TreeTableFindIndex(tablePtr, itemPtr);
	}
    else
	{
		index = lineNum;
	}
    
    /*
     * no such node in tree anymore -- should not happen
     */
    if (index == -1)
	{
		fprintf(stderr, "Fatal Error: TreeTableRemoveItem: could not find index.\n");
    	fprintf(stderr, "line: %s, image: %s\n",
				itemPtr->text ? itemPtr->text : "NULL",
				itemPtr->imageString ? itemPtr->imageString : "NULL");
		TreeTableFreeItem(tablePtr, itemPtr);
		return 0;
	}
    if (tablePtr->active > index)
	{
		tablePtr->active --;
	}
    
    /* delete sub tree of this item */
    for (tmpPtr=itemPtr->succPtr, i=index+1; tmpPtr != NULL; )
	{
		int r;
		Ptr = tmpPtr->nextPtr;
    	r = TreeTableRemoveItem (tablePtr, tmpPtr, i, lineWidth, 0, 1);
		i   += r;
		deleted += r;
		tmpPtr = Ptr;
	}
	
    /* decrement succ number of parents */
    hidden = 0;
    for (Ptr=itemPtr->parentPtr; Ptr != NULL; Ptr=Ptr->parentPtr)
	{
		Ptr->succNum --;
		if (Ptr->flags & ITEM_HIDDEN_SUBTREE)
		{
			hidden = 1;
		}
		if (!hidden)
		{
			Ptr->seenNum --;
		}
	}
	
    if (!children)
	{
		FREE_RONG_CACHE(index);
		FREE_RONG_PARENT_CACHE(index);
		
		/*
		 * Now remove this item from the list of items.
		 */
		if (itemPtr->parentPtr != NULL)
		{
			topPtr = itemPtr->parentPtr->succPtr;
		}
		else
		{
			topPtr = tablePtr->itemPtr;
		}
		if (topPtr != NULL)
		{
			/* first item in the list */
			if (topPtr == itemPtr)
			{
				if (itemPtr->parentPtr != NULL)
				{
					itemPtr->parentPtr->succPtr = itemPtr->nextPtr;
				}
				else
				{
					tablePtr->itemPtr = itemPtr->nextPtr;
				}
			}
			else
			{
				/* concat the tow parts */
				for (prevPtr=topPtr; prevPtr != NULL; prevPtr=prevPtr->nextPtr)
				{
					if (prevPtr->nextPtr != NULL && prevPtr->nextPtr == itemPtr)
					{
						prevPtr->nextPtr = itemPtr->nextPtr;
						break;
					}
				}
				if (prevPtr == NULL)
				{
					fprintf(stderr, "Fatal Error: TreeTableRemoveItem: previos list element not found");
					exit (1);
				}
			}
		}
		else
		{
			fprintf(stderr, "fatal error: TreeTableRemoveItem: couldn't find root to delete item (%s)\n",
					itemPtr->text);
		}
	}
    
    width = itemPtr->lineWidth;
    if (lineWidth && *lineWidth < width)
	{
		*lineWidth = width;
	}
    
    /* decrement number of items */
    tablePtr->numItems --;
    
    /* decrement number of selected items, if item is selected */
    if (itemPtr->flags&ITEM_SELECTED)
	{
		tablePtr->numSelected --;
	}
    
    /* Delete the entries of the Pathfinder after this index */
    DELETE_FROM_PATHFINDER(index);
    
    /* now we can destroy item */
    TreeTableFreeItem(tablePtr, itemPtr);
    
    /* correct anchor and active */
    if (tablePtr->selectAnchor > index)
	{
		tablePtr->selectAnchor --;
	}
    if (tablePtr->active > index)
	{
		tablePtr->active --;
	}
    
    if (original_item && !hidden)
	{
		int oldmaxwidth;
		if (width >= tablePtr->maxWidth)
		{
			oldmaxwidth = tablePtr->maxWidth;
			TreeTableComputeWidths(tablePtr, (TableItem *) NULL, 1);
			if (oldmaxwidth != tablePtr->maxWidth) {
			  tablePtr->flags |= UPDATE_H_SCROLLBAR;
			}
		}
		TreeTableComputeLineHeight(tablePtr, NULL, - deleted);
		tablePtr->flags |= UPDATE_V_SCROLLBAR;
		TreeTableRedrawRange(tablePtr);
	}
    
    return deleted;
}


/*
 *----------------------------------------------------------------------
 *
 * TreeTableFreeItem --
 *
 *      Free up the resources used by the item, then free the item itself.
 *      Note that this does nothing more than free the structure -- it 
 *      does not adjust the item list or anything else.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      Memory gets freed.
 *
 *----------------------------------------------------------------------
 */
static void
TreeTableFreeItem(tablePtr, itemPtr)
  register TreeTable *tablePtr;  /* Treetable widget to modify. */
  TableItem *itemPtr;            /* Item to free */
{
    Tk_FreeOptions(itemConfigSpecs, (char *) itemPtr, tablePtr->display, 0);
	if (itemPtr->textGC != None)
	{
		Tk_FreeGC(tablePtr->display, itemPtr->textGC);
	}
    if (itemPtr->bitmapGC != None)
	{
		Tk_FreeGC(tablePtr->display, itemPtr->bitmapGC);
	}
#ifdef USE_BITMAP_COLOR
    if (itemPtr->bitmapSelectGC != None)
	{
		Tk_FreeGC(tablePtr->display, itemPtr->bitmapSelectGC);
	}
    if (itemPtr->lineGC != None)
	{
		Tk_FreeGC(tablePtr->display, itemPtr->lineGC);
	}
#endif
    if (itemPtr->bitmap != None)
	{
		Tk_FreeBitmap(tablePtr->display, itemPtr->bitmap);
	}
    ckfree ((char *) itemPtr);
}

static int
TreeTableToggle (TreeTable* tablePtr, int xx, int yy, int testonly)
{
    int ret;
    int pos = 0, realpos = 0;
    
    if (xx < 0 || yy < 0)
	{
		return ITEM_TEXT;
	}
    
    /* try to toggle or test if we can toggle */
    ret = TreeTableToggleSubTree (tablePtr, tablePtr->itemPtr, &realpos, &pos, xx, yy, testonly);
    
    /* index not found */
    if (ret >= 0)
	  return ITEM_NOT_FOUND;
    
    /* bitmap clicked or text selection*/
    return ret;
}

static void
TreeTableToggleIt (TreeTable* tablePtr, TableItem*Ptr)
{
    TableItem *pPtr;
	int j, seen = 0, hidden = 0;
	if (Ptr->flags&ITEM_HIDDEN_SUBTREE)
	{
		Ptr->flags &= ~ITEM_HIDDEN_SUBTREE;
		
		/* recalculate seen items in sub tree and in parents too */
		Ptr->seenNum = TreeTableCountNotHidden_x (Ptr->succPtr);
		for (pPtr=Ptr->parentPtr; pPtr != NULL; pPtr=pPtr->parentPtr)
		{
			if (pPtr->flags&ITEM_HIDDEN_SUBTREE)
			{
				hidden = 1;
				break;
			}
			pPtr->seenNum += Ptr->seenNum;
		}
		seen = Ptr->seenNum;
	}
    else
	{
		Ptr->flags |= ITEM_HIDDEN_SUBTREE;
		
		/* no seen items in it's sub tree and in parents too */
		for (pPtr=Ptr->parentPtr; pPtr != NULL; pPtr=pPtr->parentPtr)
		{
			if (pPtr->flags&ITEM_HIDDEN_SUBTREE)
			{
				hidden = 1;
				break;
			}
			pPtr->seenNum -= Ptr->seenNum;
		}
		seen = - Ptr->seenNum;
		Ptr->seenNum = 0;
	}
	
    /* lost selected items in the sub tree */
    j = 0;
    TreeTableLostSel_x (tablePtr, Ptr->succPtr, &j, 0, tablePtr->numItems-1);
	
    /* recompute vertical */
    if (hidden)
	{
    	return;
	}
    tablePtr->numViewed += seen;
    
    /* recompute horizontal */
    TreeTableComputeWidths  (tablePtr, NULL, 1);
    
    tablePtr->flags |= UPDATE_V_SCROLLBAR|UPDATE_H_SCROLLBAR;
    TreeTableRedrawRange (tablePtr);
}

static int
TreeTableToggleSubTree (TreeTable* tablePtr, TableItem* itemPtr,
						int* realpos, int* pos, int xx, int yy, int testonly)
{
    TableItem *Ptr;
    
    char indexStr[64];
    
    sprintf (indexStr, "@%i,%i", xx, yy);
    
	if (GetTreeTableIndex (tablePtr, indexStr, 0, realpos) != TCL_OK)
	{
		return ITEM_UNKNOWN;
	}
	Ptr = TreeTableFindItem (tablePtr, *realpos);
	if (Ptr == NULL)
	{
		return ITEM_NOT_FOUND;
	}
	
	if (xx >= ITEM_X(Ptr) && xx <= ITEM_X(Ptr)+Ptr->bitmapWidth+2)
	{
		if (Ptr->succPtr != NULL)
		{
			if (testonly)
			{
				if (Ptr->flags&ITEM_HIDDEN_SUBTREE)
				  return ITEM_HIDDEN;
				else
				  return ITEM_VIEWED;
			}
			
			TreeTableToggleIt (tablePtr, Ptr);
			
			/* we don't need to force forward */
			if (Ptr->flags&ITEM_HIDDEN_SUBTREE)
			{
				return ITEM_HIDDEN;
			}
			else
			{
				return ITEM_VIEWED;
			}
		}
		/* item bitmap without sub tree is clicked */ 
		else
		{
			return ITEM_UNKNOWN;
		}
	}
	else if (xx > ITEM_X(Ptr)+Ptr->bitmapWidth) /* text is clicked */
	{
		return ITEM_TEXT;
	}
	else
	{
		return ITEM_NOT_FOUND;
	}
}

static int
TreeTableToggleItem (TreeTable* tablePtr, TableItem *itemPtr, int hide)
{
    int ret = TCL_OK;
	
	/* have no children */
	if (itemPtr->succNum == 0)
	{
		return ret;
	}
	
	/* already hidden or viewed */
	if (((itemPtr->flags&ITEM_HIDDEN_SUBTREE) && hide) ||
		(!(itemPtr->flags&ITEM_HIDDEN_SUBTREE) && !hide))
	{
		return ret;
	}
    TreeTableToggleIt (tablePtr, itemPtr);
    return ret;
}

/*
 *--------------------------------------------------------------
 *
 * TreeTableEventProc --
 *
 *      This procedure is invoked by the Tk dispatcher for various
 *      events on treetables.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      When the window gets deleted, internal structures get
 *      cleaned up.  When it gets exposed, it is redisplayed.
 *
 *--------------------------------------------------------------
 */
static void
TreeTableEventProc(
				   ClientData clientData,      /* Information about window. */
				   XEvent *eventPtr)           /* Information about event. */
{
    TreeTable *tablePtr = (TreeTable *) clientData;

    if (eventPtr->type == Expose)
	{
		TreeTableRedrawRange(tablePtr);
	}
	else if (eventPtr->type == DestroyNotify)
	{
		/* Zsolt Koppany, 3-jan-97 */
		if (tablePtr->tkwin != NULL)
		{
			if (tablePtr->setGrid) {
			  Tk_UnsetGrid(tablePtr->tkwin);
			}
			tablePtr->tkwin = NULL;
			Tcl_DeleteCommand(tablePtr->interp,
							  Tcl_GetCommandName(tablePtr->interp,tablePtr->widgetCmd));
		}
		
		if (tablePtr->flags & REDRAW_PENDING)
		{
			Tcl_CancelIdleCall(DisplayTreeTable, (ClientData) tablePtr);
		}
		Tcl_EventuallyFree((ClientData) tablePtr, DestroyTreeTable);
	}
	else if (eventPtr->type == ConfigureNotify)
	{
		Tcl_Preserve((ClientData) tablePtr);
		
		tablePtr->numLines = TREETABLE_NUM_LINES(tablePtr);
		
		tablePtr->flags |= UPDATE_V_SCROLLBAR|UPDATE_H_SCROLLBAR;
		TreeTableRedrawRange(tablePtr);
        Tcl_Release((ClientData) tablePtr);
	}
	else if (eventPtr->type == FocusIn)
	{
		if (eventPtr->xfocus.detail != NotifyInferior)
		{
			tablePtr->flags |= GOT_FOCUS;
			TreeTableRedrawRange(tablePtr);
		}
	}
	else if (eventPtr->type == FocusOut)
	{
		if (eventPtr->xfocus.detail != NotifyInferior)
		{
			tablePtr->flags &= ~GOT_FOCUS;
			TreeTableRedrawRange(tablePtr);
		}
	}
}

/*
 *--------------------------------------------------------------
 *
 * GetTreeTableIndex --
 *
 *      Parse an index into a treetable and return either its value
 *      or an error.
 *
 * Results:
 *      A standard Tcl result.  If all went well, then *indexPtr is
 *      filled in with the index corresponding to
 *      string.  Otherwise an error message is left in interp->result.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static int
GetTreeTableIndex(
				  TreeTable *tablePtr,         /* TreeTable for which the index is being
												* specified. */
				  char *string,               /* Numerical index into tablePtr's item
											   * list, or "end" to refer to last item. */
				  int endAfter,               /* 0 means "end" refers to the index of the
											   * last item, 1 means it refers to the
											   * item after the last one. */
				  int *indexPtr)              /* Where to store converted index. */
{
    int length = strlen (string);
    if (string[0] == 'e' && strncmp(string, "end", length) == 0)
	{
		*indexPtr = tablePtr->numItems;
	}
    else if (string[0] == 'a' && strncmp(string, "active", length) == 0)
	{
		*indexPtr = tablePtr->active;
	}
    else if (string[0] == '@')
	{
	    int x, y;
	    char *p, *end;
		
	    p = string+1;
	    x = strtol(p, &end, 0);
	    if ((end == p) || (*end != ','))
		{
	        goto badIndex;
		}
	    p = end+1;
	    y = strtol(p, &end, 0);
	    if ((end == p) || (*end != 0))
		{
	        goto badIndex;
		}
	    *indexPtr = NearestTreeTableItem(tablePtr, y);
	}
    else if ((string[0] == 'a') && (strncmp(string, "anchor", length) == 0) && (length >= 2))
	{
	    *indexPtr = tablePtr->selectAnchor;
	}
    else
	{
        if (Tcl_GetInt(tablePtr->interp, string, indexPtr) != TCL_OK)
		{
            Tcl_ResetResult(tablePtr->interp);
            goto badIndex;
		}
	}
    
    if (endAfter)
	{
		if (*indexPtr > tablePtr->numItems)
		{
			*indexPtr = tablePtr->numItems;
		}
	}
    else if (*indexPtr >= tablePtr->numItems)
	{
		*indexPtr = tablePtr->numItems-1;
	}
    if (tablePtr->numItems <= 0)
	{
		*indexPtr = 0;
	}
    else if (*indexPtr < 0)
	{
		*indexPtr = 0;
	}
    return TCL_OK;
	
badIndex:
    Tcl_AppendResult(tablePtr->interp, "bad treetable index \"", string,
					 "\"", (char *) NULL);
    return TCL_ERROR;
}


/*
 *----------------------------------------------------------------------
 *
 * ChangeTreeTableView --
 *
 *      Change the view on a treetable widget.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      What's displayed on the screen is changed.  If there is a
 *      scrollbar associated with this widget, then the scrollbar
 *      is instructed to change its display too.
 *
 *----------------------------------------------------------------------
 */

static void
ChangeTreeTableView(register TreeTable *tablePtr,         /* Information about widget. */
					int index,                            /* Index of item in tablePtr */
					int redraw)
{
    int count;

    count = TreeTableCountNotHidden (tablePtr, 0);
    if (index >= (count - tablePtr->numLines))
	{
        index = count - tablePtr->numLines;
	}
	
    if (index < 0) {
	  index = 0;
}
    
    if (tablePtr->topIndex != index || redraw)
	{
        tablePtr->topIndex = index;
        if (!(tablePtr->flags & REDRAW_PENDING))
		{
            Tk_DoWhenIdle(DisplayTreeTable, (ClientData) tablePtr);
            tablePtr->flags |= REDRAW_PENDING;
		}
        tablePtr->flags |= UPDATE_V_SCROLLBAR;
	}
}



/*
 *----------------------------------------------------------------------
 *
 * ChangeTreeTableOffset --
 *
 *      Change the horizontal offset for a treetable.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The treetable may be redrawn to reflect its new horizontal
 *      offset.
 *
 *----------------------------------------------------------------------
 */
static void
ChangeTreeTableOffset(tablePtr, offset)
    register TreeTable *tablePtr;       /* Information about widget. */
    int offset;                         /* Desired new "xOffset" for
                                         * treetable. */
{
    int maxOffset;

    /*
     * Make sure that the new offset is within the allowable range, and
     * round it off to an even multiple of xScrollUnit.
     */
    maxOffset = tablePtr->maxWidth
	          + (tablePtr->xScrollUnit-1)
	          - (Tk_Width(tablePtr->tkwin)
				 - 2 * (tablePtr->selBorderWidth+tablePtr->inset)
				 - tablePtr->xScrollUnit);
    if (offset > maxOffset)
	{
		offset = maxOffset;
	}
    if (offset < 0)
	{
        offset = 0;
	}
    offset -= offset%tablePtr->xScrollUnit;
    if (offset != tablePtr->xOffset)
	{
        tablePtr->xOffset = offset;
        tablePtr->flags |= UPDATE_H_SCROLLBAR;
        TreeTableRedrawRange(tablePtr);
	}
}


/*
 *----------------------------------------------------------------------
 *
 * TreeTableScanTo --
 *
 *      Given a point (presumably of the curent mouse location)
 *      drag the view in the window to implement the scan operation.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The view in the window may change.
 *
 *----------------------------------------------------------------------
 */
static void
TreeTableScanTo(tablePtr, x, y)
  register TreeTable *tablePtr;       /* Information about widget. */
  int x;                              /* X-coordinate to use for scan
									   * operation. */
  int y;                              /* Y-coordinate to use for scan
									   * operation. */
{
    int newTopIndex, newOffset;

    /*
     * Compute new top line for screen by amplifying the difference
     * between the current position and the place where the scan
     * started (the "mark" position).  If we run off the top or bottom
     * of the list, then reset the mark point so that the current
     * position continues to correspond to the edge of the window.
     * This means that the picture will start dragging as soon as the
     * mouse reverses direction (without this reset, might have to slide
     * mouse a long ways back before the picture starts moving again).
     */
    int count = TreeTableCountNotHidden (tablePtr, 0);
    
    newTopIndex = tablePtr->scanMarkYIndex
	  - (10*(y - tablePtr->scanMarkY))/tablePtr->lineHeight;
    if (newTopIndex >= count)
	{
        newTopIndex = tablePtr->scanMarkYIndex = count-1;
        tablePtr->scanMarkY = y;
	}
	else if (newTopIndex < 0)
	{
		newTopIndex = tablePtr->scanMarkYIndex = 0;
		tablePtr->scanMarkY = y;
	}
    ChangeTreeTableView(tablePtr, newTopIndex, 0);
	
    /*
     * Compute new left edge for display in a similar fashion by amplifying
     * the difference between the current position and the place where the
     * scan started.
     */
	
    newOffset = tablePtr->scanMarkXOffset - (10*(x - tablePtr->scanMarkX));
    if (newOffset >= tablePtr->maxWidth)
	{
		newOffset = tablePtr->scanMarkXOffset = tablePtr->maxWidth;
		tablePtr->scanMarkX = x;
	}
	else if (newOffset < 0)
	{
        newOffset = tablePtr->scanMarkXOffset = 0;
        tablePtr->scanMarkX = x;
	}
    ChangeTreeTableOffset(tablePtr, newOffset);
}

/*{*/
/*
 * Caluclate the realy index of a view index
 */
static int
TreeTableRealyIndex_x (register TableItem *itemPtr, int*pos, int* rindex, int index)
{
    TableItem*Ptr;
    for (Ptr=itemPtr; *pos <= index && Ptr; Ptr=Ptr->nextPtr)
	{
    	/* found realy position */
		if (*pos == index)
		{
			return -1;
		}
		*pos += 1;
		*rindex += 1;
		if (!(Ptr->flags&ITEM_HIDDEN_SUBTREE) && Ptr->succPtr != NULL)
		{
			int ret;
			
			if (*pos+Ptr->seenNum < index)
			{
				*pos += Ptr->seenNum;
				*rindex += Ptr->succNum;
			}
			else
			{
				ret = TreeTableRealyIndex_x (Ptr->succPtr, pos, rindex, index);
				if (ret < 0)
				  return ret;
			}
		}
		else
		{
			*rindex += Ptr->succNum;
		}
	}
    return *rindex;
}

static int
TreeTableRealyIndex (register TreeTable *tablePtr, int index)
{
    TableItem*Ptr=tablePtr->itemPtr;
	int rindex, pos, ret;
    
    /* there is nothing hidden, so the realy index is eq. viewed index */
    if (tablePtr->numViewed == tablePtr->numItems)
	{
		return index;
	}
    
    FIND_IN_PATHFINDER(index, Ptr, pos);
    
    /* calculate the realy index */
    rindex = pos = 0;
    ret = TreeTableRealyIndex_x(Ptr, &pos, &rindex, index);
    
    if (ret < 0)
	  return rindex;
	
    return -1;
}
/*}*/

/*{*/
/*
 * calculate the viewed index of a realy index
 */
static int
TreeTableViewedIndex_x (register TableItem *itemPtr, int*realpos, int* vindex, int index)
{
    TableItem*Ptr;
    for (Ptr=itemPtr; *realpos <= index && Ptr; Ptr=Ptr->nextPtr)
	{
    	/* found viewed position */
		if (*realpos == index)
		{
			return -1;
		}
		*realpos += 1;
		*vindex += 1;
		if (Ptr->succPtr != NULL)
		{
			if (!(Ptr->flags&ITEM_HIDDEN_SUBTREE))
			{
				int ret;
				
				if (*realpos+Ptr->succNum < index)
				{
					*realpos += Ptr->succNum;
					*vindex  += Ptr->seenNum;
				}
				else
				{
					ret = TreeTableViewedIndex_x (Ptr->succPtr, realpos, vindex, index);
					if (ret < 0)
					  return ret;
				}
			}
			/* if item is in a hidden sub tree, then return his first viewed parent */
			else if (*realpos+Ptr->succNum >= index)
			{
				*vindex -= 1;
				return -1;
			}
			else
			{
				*realpos += Ptr->succNum;
				*vindex  += Ptr->seenNum;
			}
		}
	}
    return *vindex;
}
static int
TreeTableViewedIndex (register TreeTable *tablePtr, int index)
{
    int vindex, realpos, ret;
    
    /* there is nothing hidden */
    if (tablePtr->numViewed == tablePtr->numItems)
	{
		return index;
	}
    
    /* calculate the viewed index of a realy index */
    vindex = realpos = 0;
    ret = TreeTableViewedIndex_x(tablePtr->itemPtr, &realpos, &vindex, index);
	
    if (ret < 0)
	  return vindex;
	
    return -1;
}
/*}*/

/*
 *----------------------------------------------------------------------
 *
 * NearestTreeTableItem --
 *
 *      Given a y-coordinate inside a treetable, compute the index of
 *      the item under that y-coordinate (or closest to that
 *      y-coordinate).
 *
 * Results:
 *      The return value is an index of an element of itemPtr.  If
 *      itemPtr has no elements, then 0 is always returned.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */
static int
NearestTreeTableItem(tablePtr, y)
  register TreeTable *tablePtr;      /* Information about widget. */
  int y;                             /* Y-coordinate in tablePtr's window. */
{
    int index;
    int count;
    
    count = TreeTableCountNotHidden (tablePtr, 0);
    
    if (y < 0) y = 0;
    index = (y - tablePtr->borderWidth)/tablePtr->lineHeight;
    
    /* correct the index in usable range */
    if (index < 0)
	{
        index = 0;
	}
    if (index > tablePtr->numLines)
	{
		index = tablePtr->numLines - 1;
	}
    index += tablePtr->topIndex;
    if (index >= count)
	{
        index = count - 1;
	}
	
    /* return realy index in the tree */
    return TreeTableRealyIndex (tablePtr, index);
}


/*
 *  TreeTableComputeLineHeight
 *
 *          Cycles through all of the items in the table and computes
 *          the maximum line height.
 *
 *
 */
static int
TreeTableComputeLineHeight_x (TableItem* itemPPtr, TableItem* thisPtr, int maxHeight)
{
	TableItem* itemPtr = itemPPtr;
	int height = maxHeight;
	
	while (itemPtr != (TableItem *) NULL)
	{
	    if (itemPtr->fontHeight > height)
		{
			height = itemPtr->fontHeight;
		}
	    if (itemPtr->bitmapHeight > height)
		{
			height = itemPtr->bitmapHeight;
		}
		
	    /* force only the first item */
	    if (thisPtr != NULL)
		  break;
		
	    if (itemPtr->succPtr != NULL)
		{
			height =
			  TreeTableComputeLineHeight_x
			  (itemPtr->succPtr, thisPtr, height);
		}
	    itemPtr = itemPtr->nextPtr;
	}
	return height;
}
static void
TreeTableComputeLineHeight(TreeTable *tablePtr, TableItem* thisPtr, int cnt)
{
    register TableItem *itemPtr;
    register int maxHeight;
	
	
    if (cnt == 0)
	{
		if (thisPtr != NULL)
		{
			itemPtr = thisPtr;
		}
		else
		{
			itemPtr = tablePtr->itemPtr;
		}
		maxHeight =
		  TreeTableComputeLineHeight_x
		  (itemPtr, thisPtr, tablePtr->defFontHeight);
    	tablePtr->lineHeight = maxHeight + 1 + 2*tablePtr->selBorderWidth;
		TreeTableCountNotHidden(tablePtr, 1);
	}
    else
	{
    	tablePtr->numViewed += cnt;
	}
}


static void
TreeTableComputeWidths_x (TreeTable* tablePtr, TableItem* itemPtr,
			  TableItem* thisPtr, int checkall)
{
    TableItem* travPtr;
    
    if (thisPtr != NULL)
	{
		travPtr = thisPtr;
	}
    else
	{
		travPtr = itemPtr;
	}
    
    for (; travPtr != (TableItem *) NULL;
		 travPtr = travPtr->nextPtr)
	{
		if (tablePtr->BestFit && tablePtr->tabs != NULL)
		{
			ComputeTabStops (tablePtr, travPtr);
		}
		else
		{
			/* find max. width only */
			if (thisPtr != NULL || checkall == 2)
			  travPtr->lineWidth =
				Tk_TextWidth (travPtr->fontPtr != NULL ? travPtr->fontPtr : tablePtr->defFontPtr,
							  travPtr->text, 
							  travPtr->textLength)
				+ (travPtr->indent*tablePtr->indentWidth)
				+ travPtr->bitmapWidth 
				+ tablePtr->bitmapSpace
				+ tablePtr->inset
				- tablePtr->xOffset;
			if (travPtr->lineWidth > tablePtr->maxWidth)
			{
				tablePtr->maxWidth = travPtr->lineWidth;
			}
		}
		
		/*
		 * calulate the max. width relative to only one given item
		 */
		if (thisPtr != NULL)
		{
			return;
		}
		
		if (!(travPtr->flags&ITEM_HIDDEN_SUBTREE) && travPtr->succPtr != NULL)
		{
			TreeTableComputeWidths_x (tablePtr, travPtr->succPtr, NULL, checkall);
		}
	}
}

/*
 *----------------------------------------------------------------------
 *
 * TreeTableComputeWidths --
 *
 *      This procedure is invoked to recompute the width of either 
 *      a single item (if itemPtr != NULL), or the entire table
 *      (fontChanged = 1).  
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      tablePtr->maxWidth may be modified 
 *
 *----------------------------------------------------------------------
 */
static void
TreeTableComputeWidths(tablePtr, itemPtr, checkAll)
  TreeTable *tablePtr;        /* TreeTable whos geometry is to be
							   * recomputed. */
  TableItem *itemPtr;         /* Item in the table which has changed
							   * or NULL if none has. */
  int checkAll;               /* Non-zero means to check the entire
							   * list of table items in order to 
							   * recompute the max width.
							   *
							   * 0: compute only the width only for
							   *    given 'itemPtr'
							   * 1: find maximum width of lines
							   * 2: recompute line width
							   * 3: If BestFit enabled, recompute the
							   *    best fit for tab stops
							   */
{
    tablePtr->xScrollUnit = Tk_TextWidth (tablePtr->defFontPtr, "0", 1);
  
    if (checkAll == 3 && tablePtr->BestFit && tablePtr->tabs != NULL)
	{
		int i;
		for (i=0; i<tablePtr->tabsNum; i++)
		{
			if (tablePtr->tabsHidden[i])
			{
#ifdef SPACE_PROBLEM
				tablePtr->tabs[i] = 0;
#else
				tablePtr->tabs[i] = tablePtr->tabsMinSpace;
#endif
			}
			else
			{
				tablePtr->tabs[i] = tablePtr->defTabs[i];
			}
		}
	}
    /* we don't need to compute width if tab stops availiable */
    else if (tablePtr->tabs != NULL)
	{
		return;
	}
    
    if (itemPtr == (TableItem *) NULL)
	{
		tablePtr->maxWidth = 10;
	}
    
    TreeTableComputeWidths_x (tablePtr, tablePtr->itemPtr, itemPtr, checkAll);
}


static int
TreeTableSelectFromTo (
					   TreeTable*tablePtr,
					   TableItem *itemPtr,
					   int pos,
					   int start,
					   int end)
{
    TableItem*Ptr=itemPtr;
	int i = pos;
	
	/* Accelerate the location of the fist item 
	 */
	FIND_IN_PATHFINDER (start, Ptr, i);
	
	for (; i <= end && Ptr; Ptr=Ptr->nextPtr)
	{
		if (i >= start && ! (Ptr->flags&ITEM_SELECTED))
		{
			Ptr->flags |= ITEM_SELECTED;
			
			if (tablePtr->selectFirst > i)
			{
				tablePtr->selectFirst = i;
			}
			if (tablePtr->numSelected == 0)
			{
				tablePtr->lastSelected = Ptr;
			}
			tablePtr->numSelected ++;
		}
		
		i++;
		if (Ptr->succPtr != NULL)
		{
			if (!(Ptr->flags&ITEM_HIDDEN_SUBTREE))
			{
				i = TreeTableSelectFromTo
				  (tablePtr, Ptr->succPtr,
				   i,
				   start, end);
			}
			else
			{
				i += Ptr->succNum;
			}
		}	
	}
    return i;
}

static int
TreeTableCountSelected (TableItem* itemPtr, int cnt)
{
    TableItem* Ptr;
	int i = cnt;
	for (Ptr=itemPtr; Ptr!=NULL; Ptr=Ptr->nextPtr)
	{
		if (Ptr->flags&ITEM_SELECTED)
		  i++;
		if (Ptr->succPtr != NULL && !(Ptr->flags&ITEM_HIDDEN_SUBTREE))
		{
			i = TreeTableCountSelected (Ptr->succPtr, i);
		}
	}
    return i;
}

static int
TreeTableSetText (TableItem* itemPtr, char*argv[], int cnt)
{
    TableItem* Ptr;
    int i = cnt;
    for (Ptr=itemPtr; Ptr!=NULL; Ptr=Ptr->nextPtr)
	{
		if (Ptr->flags&ITEM_SELECTED)
		{
			argv[i++] = Ptr->text;
		}
		if (Ptr->succPtr != NULL)
		{
            i = TreeTableSetText (Ptr->succPtr, argv, i);
		}
	}
    return i;
}
/*
 *----------------------------------------------------------------------
 *
 * TreeTableFetchSelection --
 *
 *      This procedure is called back by Tk when the selection is
 *      requested by someone.  It returns part or all of the selection
 *      in a buffer provided by the caller.
 *
 * Results:
 *      The return value is the number of non-NULL bytes stored
 *      at buffer.  Buffer is filled (or partially filled) with a
 *      NULL-terminated string containing part or all of the selection,
 *      as given by offset and maxBytes.  The selection is returned
 *      as a Tcl list with one list element for each item in the
 *      treetable.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */
static int
TreeTableFetchSelection(clientData, offset, buffer, maxBytes)
  ClientData clientData;              /* Information about treetable
									   * widget. */
  int offset;                         /* Offset within selection of first
									   * byte to be returned. */
  char *buffer;                       /* Location in which to place
									   * selection. */
  int maxBytes;                       /* Maximum number of bytes to place
									   * at buffer, not including terminating
									   * NULL character. */
{
    register TreeTable *tablePtr = (TreeTable *) clientData;
    char **argv, *selection;
    int length, count, argc;
	
    if (!tablePtr->exportSelection)
	{
		return -1;
	}
	
    /*
     * Use Tcl_Merge to format the treetable items into a suitable
     * Tcl list.
     */
    argc = TreeTableCountSelected (tablePtr->itemPtr, 0);
    if (argc == 0)
	{
		return -1;
	}
    argv = (char **)ckalloc((unsigned) (argc*sizeof(char *)));
    TreeTableSetText (tablePtr->itemPtr, argv, 0);
    
    selection = Tcl_Merge(argc, argv);
	
    /*
     * Copy the requested portion of the selection to the buffer.
     */
	
    length = strlen(selection);
    count = length - offset;
    if (count <= 0)
	{
        count = 0;
        goto done;
	}
    if (count > maxBytes)
	{
        count = maxBytes;
	}
    memcpy((VOID *) buffer, (VOID *) (selection + offset), count);
	
done:
    buffer[count] = '\0';
    ckfree ((char*)selection);
    ckfree ((char *) argv);
    return count;
}



/*
 *----------------------------------------------------------------------
 *
 * TreeTableLostSelection --
 *
 *      This procedure is called back by Tk when the selection is
 *      grabbed away from a treetable widget.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The existing selection is unhighlighted, and the window is
 *      marked as not containing a selection.
 *
 *----------------------------------------------------------------------
 */
static int
TreeTableLostSel_x (register TreeTable *tablePtr,
					register TableItem* itemPtr,
					int *pos, int first, int last)
{
    register TableItem* Ptr;
    int j = 0;
    
    for (Ptr=itemPtr; tablePtr->numSelected > 0 && *pos<=last && Ptr != NULL;
		Ptr=Ptr->nextPtr)
	{
		if (*pos >= first && *pos<=last && (Ptr->flags&ITEM_SELECTED))
		{
			Ptr->flags &= ~ITEM_SELECTED;
			tablePtr->numSelected --;
			j++;
		}
		*pos += 1;
		if (Ptr->succPtr != NULL)
		{
            j += TreeTableLostSel_x (tablePtr, Ptr->succPtr, pos, first, last);
		}
	}
    return j;
}

static int
TreeTableLostSel (register TreeTable *tablePtr, int first, int last)
{
    TableItem* Ptr=tablePtr->itemPtr;
	int i=0, ret;
	
	if (tablePtr->numSelected == 1 && tablePtr->lastSelected != NULL)
	{
		tablePtr->lastSelected->flags &= ~ITEM_SELECTED;
		tablePtr->lastSelected = NULL;
		
		tablePtr->numSelected = 0;
		ret = 1;
	}
    else
	{
    	FIND_IN_PATHFINDER(first, Ptr, i);
    	return TreeTableLostSel_x(tablePtr, Ptr, &i, first, last);
	}
    tablePtr->numSelected = 0;
    
    return ret;
}

static void
TreeTableLostSelection(clientData)
  ClientData clientData;              /* Information about listbox widget. */
{
    register TreeTable *tablePtr = (TreeTable *) clientData;
	
	/* set items as not selected */
	if (tablePtr->exportSelection &&
        TreeTableLostSel (tablePtr, 0, tablePtr->numItems-1) > 0)
	{
		TreeTableRedrawRange(tablePtr);
	}
}



/*
 *----------------------------------------------------------------------
 *
 * TreeTableRedrawRange --
 *
 *      Ensure that a given range of elements is eventually redrawn on
 *      the display (if those elements in fact appear on the display).
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      Information gets redisplayed.
 *
 *----------------------------------------------------------------------
 */

/*
 * Calculate the seen number in a sub tree 
 */
static int
TreeTableCountNotHidden_x (register TableItem*itemPtr)
{
    register TableItem*Ptr;
    register int i = 0;
    
    for (Ptr=itemPtr; Ptr != NULL; Ptr=Ptr->nextPtr)
	{
		i += 1 + Ptr->seenNum;
	}
    return i;
}

/*
 * Calculate the number of seen items on the tree
 */
static int
TreeTableCountNotHidden (register TreeTable*tablePtr, int flag)
{
    if (flag || tablePtr->numViewed == -1)
	{
		register TableItem*Ptr;
		register int i = 0;
		for (Ptr=tablePtr->itemPtr; Ptr != NULL; Ptr=Ptr->nextPtr)
		{
			i += 1 + Ptr->seenNum;
		}
		tablePtr->numViewed = i;
	}
	return tablePtr->numViewed;
}

/*
 *----------------------------------------------------------------------
 *
 * TreeTableUpdateVScrollbar --
 *
 *      This procedure is invoked whenever information has changed in
 *      a treetable in a way that would invalidate a vertical scrollbar
 *      display.  If there is an associated scrollbar, then this command
 *      updates it by invoking a Tcl command.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      A Tcl command is invoked, and an additional command may be
 *      invoked to process errors in the command.
 *
 *----------------------------------------------------------------------
 */
static void
TreeTableUpdateVScrollbar(
     register TreeTable *tablePtr          /* Information about widget. */
     )
{
    char string[60];
    int result, last;
    int count;
	
    if (tablePtr->yScrollCmd == NULL)
	{
        return;
	}
    count = TreeTableCountNotHidden (tablePtr, 0);
    
    last = tablePtr->topIndex + tablePtr->numLines - 1;
    if (last >= count)
	{
        last = count-1;
	}
    if (last < tablePtr->topIndex)
	{
        last = tablePtr->topIndex;
	}
    sprintf(string, " %d %d %d %d",
    	    count,
			tablePtr->numLines,
            tablePtr->topIndex,
			last);
	
    Tcl_Preserve((ClientData) tablePtr->interp);
    result = Tcl_VarEval(tablePtr->interp, tablePtr->yScrollCmd, string,
						 (char *) NULL);
    if (result != TCL_OK)
	{
        Tcl_AddErrorInfo(tablePtr->interp,
						 "\n    (vertical scrolling command executed by listbox)");
        Tk_BackgroundError(tablePtr->interp);
	}
    Tcl_Release((ClientData) tablePtr->interp);
}


/*
 *----------------------------------------------------------------------
 *
 * TreeTableUpdateHScrollbar --
 *
 *      This procedure is invoked whenever information has changed in
 *      a treetable in a way that would invalidate a horizontal scrollbar
 *      display.  If there is an associated horizontal scrollbar, then
 *      this command updates it by invoking a Tcl command.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      A Tcl command is invoked, and an additional command may be
 *      invoked to process errors in the command.
 *
 *----------------------------------------------------------------------
 */
static void
TreeTableUpdateHScrollbar(
	register TreeTable *tablePtr /* Information about widget. */
	)
{
    char string[60];
    Tcl_Interp *interp;
    int result, windowWidth;
    double first, last;
	
    if (tablePtr->xScrollCmd == NULL)
	{
        return;
	}
    windowWidth = TREETABLE_WIDTH(tablePtr);
    if (tablePtr->maxWidth == 0)
	{
		first = 0;
		last = 1.0;
	}
    else
	{
		first = ((double)tablePtr->xOffset)/((double) tablePtr->maxWidth);
		last = (tablePtr->xOffset + windowWidth)
		  /((double) tablePtr->maxWidth);
		if (last > 1.0)
		{
			last = 1.0;
		}
	}
	
    sprintf(string, " %g %g", first, last);
	
    /*
     * We must hold onto the interpreter because the data referred to at
     * tablePtr might be freed as a result of the call to Tcl_VarEval.
     */
    
    interp = tablePtr->interp;
    Tcl_Preserve((ClientData) interp);
    result = Tcl_VarEval(interp, tablePtr->xScrollCmd, string,
						 (char *) NULL);
    if (result != TCL_OK) {
	  Tcl_AddErrorInfo(interp,
					   "\n    (horizontal scrolling command executed by listbox)");
	  Tcl_BackgroundError(interp);
}
    Tcl_Release((ClientData) interp);
}

enum
{
  SRCH_EXACT = 1,
  SRCH_GLOB,
  SRCH_REGEXP,
  SRCH_BEGINS
};

typedef struct
{
  char *pattern, *varName;
  Tcl_DString line;
  int backwards, exact, noCase;
  int matchLength;
  int patLength;
  int index, stopIndex;
  int lineNum;
  Tcl_RegExp regexp;
  int (* cmpProc) (char*, char*, int);
  int code;
  int realindex;
} searchParam_t;

static int
TreeTableSearchCmd_x (TreeTable* tablePtr, TableItem* linePtr,
					  searchParam_t* prm)
{
    int found = 0;
	char *p;
	
    if (prm->exact != SRCH_EXACT && prm->exact != SRCH_BEGINS)
	{
		Tcl_DStringInit(&prm->line);
	}
    
    for (;linePtr;)
	{
		/* only by searching forwards */
		if (! prm->backwards)
		{
			if (prm->lineNum < prm->index || prm->lineNum > prm->stopIndex)
			{
				goto nextLine;
			}
		}
		else
		{
			if (prm->lineNum >= tablePtr->numItems) {
			  /*
			   * Don't search the dummy last line of the text.
			   */
			  goto nextLine;
			}
			if (prm->realindex)      /* find real items */
			{
				linePtr = TreeTableFindItem (tablePtr, prm->lineNum);
			}
			else                     /* find virtual items */
			{
				linePtr = TreeTableFindNotHiddenItem(tablePtr, prm->lineNum, 0);
			}
			if (linePtr == NULL)
			{
				goto nextLine;
			}
		}
		
		if (prm->exact != SRCH_EXACT && prm->exact != SRCH_BEGINS)
		{
			Tcl_DStringAppend(&prm->line, linePtr->text, linePtr->textLength);
		}
		if (prm->exact == SRCH_REGEXP)
		{
			Tcl_DStringSetLength(&prm->line, Tcl_DStringLength(&prm->line)-1);
		}
		
		/*
		 * If we're ignoring case, convert the line to lower case.
		 */
		if (prm->noCase && prm->exact != SRCH_EXACT && prm->exact != SRCH_BEGINS)
		{
			for (p = Tcl_DStringValue(&prm->line); *p != 0; p++)
			{
				if (isupper(UCHAR(*p)))
				{
					*p = tolower(UCHAR(*p));
				}
			}
		}
		
		if (prm->exact == SRCH_EXACT)
		{
			if (prm->patLength == linePtr->textLength && prm->cmpProc (linePtr->text, prm->pattern, prm->patLength) == 0)
			{
				found = 1;  /* string found */
				goto done;
			}
		}
		else if (prm->exact == SRCH_BEGINS)
		{
			if (prm->cmpProc (linePtr->text, prm->pattern, prm->patLength) == 0)
			{
				found = 1;  /* string found */
				goto done;
			}
		}
		else if (prm->exact == SRCH_REGEXP)
		{
			switch (prm->matchLength = Tcl_RegExpExec(tablePtr->interp, prm->regexp,
													  Tcl_DStringValue(&prm->line),
													  Tcl_DStringValue(&prm->line)))
			{
			  case -1:
				prm->code = TCL_ERROR;
				goto done;
				break;
			  case 0:                 /* Not found ! */
				break;
			  default:		/* found */
				found = 1;
				goto done;
				break;
				
			}
		}
		else if (prm->exact == SRCH_GLOB)
		{
			if (Tcl_StringMatch(Tcl_DStringValue(&prm->line), prm->pattern))
			{
				found = 1;
				goto done;
			}
		}
		
		/*
		 * Go to the next (or previous) line;
		 */
	  nextLine:
		if (prm->backwards)
		{
			prm->lineNum--;
			if (prm->lineNum < prm->stopIndex)
			{
				prm->code = TCL_OK;
				return -1;
			}
			else if (prm->lineNum < 0)
			{
				prm->lineNum = tablePtr->numItems-1;
			}
		}
		else
		{
			prm->lineNum++;
			if (linePtr->succPtr != NULL && !(linePtr->flags&ITEM_HIDDEN_SUBTREE))
			{
				int ret;
				ret = TreeTableSearchCmd_x (tablePtr, linePtr->succPtr, prm);
				if (ret < 0)
				{
					return ret;
				}
			}
			else
			{
				prm->lineNum += linePtr->succNum;
			}
			linePtr = linePtr->nextPtr;
			if (linePtr == NULL)
			{
				return 0;
			}
			
			if (prm->lineNum > prm->stopIndex)
			{
				return -1;
			}
			else if (prm->lineNum >= tablePtr->numItems)
			{
				prm->lineNum = 0;
			}
		}
		
		if (prm->exact != SRCH_EXACT && prm->exact != SRCH_BEGINS)
		{
			Tcl_DStringSetLength(&prm->line, 0);
		}
	}
done:
    
    if (found)
	{
		char tmp[20];
		sprintf (tmp, "%i", prm->lineNum);
		if (prm->varName != NULL)
		{
			sprintf(tmp, "%d", prm->matchLength);
			if (Tcl_SetVar(tablePtr->interp, prm->varName, tmp, TCL_LEAVE_ERR_MSG) == NULL)
			{
				prm->code = TCL_ERROR;
				return -1;
			}
		}
		Tcl_AppendElement (tablePtr->interp, tmp);
		return -1;
	}
    return 0;
}

static int
TreeTableSearchCmd(TreeTable *tablePtr, int argc, char **argv)
{
    searchParam_t prm;
    int i, argsLeft;
    size_t length;
    int c;
    char *arg, *p;
    Tcl_DString patDString;
	
    /*
     * Parse switches and other arguments.
     */
    memset (&prm, 0, sizeof (prm));
    prm.exact = SRCH_EXACT;
    prm.backwards = 0;
    prm.noCase = 0;
    prm.varName = NULL;
    prm.realindex = 0; /* default, look for a not hidden item */
    for (i = 2; i < argc; i++)
	{
		arg = argv[i];
		if (arg[0] != '-')
		{
			break;
		}
		length = strlen(arg);
		if (length < 2)
		{
		  badSwitch:
			Tcl_AppendResult(tablePtr->interp, "bad switch \"", arg,
							 "\": must be -forward, -backward, -exact, -begins, -glob, -regexp, ",
							 "-nocase, -count, -realindex or --", (char *) NULL);
			return TCL_ERROR;
		}
		c = arg[1];
		if ((c == 'b') && (strncmp(argv[i], "-backwards", length) == 0))
		{
			prm.backwards = 1;
		}
		else if ((c == 'c') && (strncmp(argv[i], "-count", length) == 0))
		{
			if (i >= (argc-1))
			{
				tablePtr->interp->result = "no value given for \"-count\" option";
				return TCL_ERROR;
			}
			i++;
			prm.varName = argv[i];
		}
		else if ((c == 'e') && (strncmp(argv[i], "-exact", length) == 0))
		{
			prm.exact = SRCH_EXACT;
		}
		else if ((c == 'b') && (strncmp(argv[i], "-begins", length) == 0))
		{
			prm.exact = SRCH_BEGINS;
		}
		else if ((c == 'f') && (strncmp(argv[i], "-forwards", length) == 0))
		{
			prm.backwards = 0;
		}
		else if ((c == 'n') && (strncmp(argv[i], "-nocase", length) == 0))
		{
			prm.noCase = 1;
		}
		else if ((c == 'r') && (strncmp(argv[i], "-regexp", length) == 0))
		{
			prm.exact = SRCH_REGEXP;
		}
		else if ((c == 'g') && (strncmp(argv[i], "-glob", length) == 0))
		{
			prm.exact = SRCH_GLOB;
		}
		else if ((c == 'r') && (strncmp(argv[i], "-realindex", length) == 0))
		{
			prm.realindex = 1;
		}
		else if ((c == '-') && (strncmp(argv[i], "--", length) == 0))
		{
			i++;
			break;
		}
		else
		{
			goto badSwitch;
		}
	}
    argsLeft = argc - (i+2);
    if ((argsLeft != 0) && (argsLeft != 1))
	{
		Tcl_AppendResult(tablePtr->interp, "wrong # args: should be \"",
						 argv[0], " search ?switches? pattern index ?stopIndex?",
						 (char *) NULL);
		return TCL_ERROR;
	}
    prm.pattern = argv[i];
	
    /*
     * Convert the pattern to lower-case if we're supposed to ignore case.
     */
    if (prm.noCase && prm.exact != SRCH_EXACT && prm.exact != SRCH_BEGINS)
	{
		Tcl_DStringInit(&patDString);
		Tcl_DStringAppend(&patDString, prm.pattern, -1);
		prm.pattern = Tcl_DStringValue(&patDString);
		for (p = prm.pattern; *p != 0; p++)
		{
			if (isupper(UCHAR(*p)))
			{
				*p = tolower(UCHAR(*p));
			}
		}
	}
	
    /* verify if the list isn't empty */
    if (tablePtr->itemPtr == NULL)
	{
		return TCL_OK;
	}
    
    if (GetTreeTableIndex(tablePtr, argv[i+1], 0, &prm.index) != TCL_OK)
	{
		return TCL_ERROR;
	}
    if (argsLeft == 1)
	{
		if (GetTreeTableIndex(tablePtr, argv[i+2], 0, &prm.stopIndex) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (prm.stopIndex >= tablePtr->numItems)
		{
			prm.stopIndex = tablePtr->numItems-1;
		}
	}
    else
	{
		prm.stopIndex = tablePtr->numItems-1;
	}
    
    if (prm.index >= tablePtr->numItems)
	{
    	prm.index = tablePtr->numItems-1;
	}
    
    /*
     * Scan through all of the lines of the text circularly, starting
     * at the given index.
     */
    prm.matchLength = prm.patLength = 0;	/* Only needed to prevent compiler
											 * warnings. */
    if (prm.exact != SRCH_REGEXP)
	{
		prm.patLength = strlen(prm.pattern);
	}
    else
	{
		prm.regexp = Tcl_RegExpCompile(tablePtr->interp, prm.pattern);
		if (prm.regexp == NULL)
		{
			return TCL_ERROR;
		}
	}
    
    prm.lineNum = 0;
    prm.cmpProc = (int(*)(char*,char*,int)) ((prm.noCase) ? strncasecmp : strncmp);
    TreeTableSearchCmd_x (tablePtr, tablePtr->itemPtr, &prm);
    
    if (prm.exact != SRCH_EXACT && prm.exact != SRCH_BEGINS)
	{
		Tcl_DStringFree(&prm.line);
		if (prm.noCase)
		{
			Tcl_DStringFree(&patDString);
		}
	}
    return prm.code;
}

/*
 * Sort items on a column number, options:
 * -column <num>:	column number to sort items on it
 * -nocase:		ignore lower/upper case
 */
#define PosTabStr(s, p, n) i=0; p=s; while(i < n && *p) {\
  if (*p == '\t') i++; \
  p++; \
  if (n == i) \
				break; \
}

static int sortColNum;
static int sortNoCase;

static int
TreeTableSortCmp(TableItem **Ptr11, TableItem **Ptr22)
{
    TableItem *Ptr1=*Ptr11;
    TableItem *Ptr2=*Ptr22;
    register char *p, *q;
    int ret, i;
	
    if (sortColNum <= 0)
	{
		p = Ptr1->text;
		q = Ptr2->text;
	}
    else
	{
		PosTabStr (Ptr1->text, p, sortColNum);
		PosTabStr (Ptr2->text, q, sortColNum);
	}
    if (*p == '~') p ++;
    if (*q == '~') q ++;
    
    if (sortNoCase)
	{
    	/* Check first the only first characters! */
    	if ((ret = (tolower(*p) - tolower(*q))) || *p == '\0')
		{
			return ret;
		}
		else
		{
			ret = strcasecmp (p + 1, q + 1);
		}
		/* collapse the string comparision, if the text
		 * is identical
		 */
		if (ret == 0 && sortColNum > 0)
		{
			ret = strcasecmp (Ptr1->text, Ptr2->text);
		}
	}
    else
	{
    	/* Check first the only first characters! */
		if (*p != *q || *p == '\0')
		{
			return *p - *q;
		}
		else
		  ret = strcmp (q + 1, q + 1);
		
		/* collapse the string comparision, if the text
		 * is identical
		 */
		if (ret == 0 && sortColNum > 0)
		{
			ret = strcmp (Ptr1->text, Ptr2->text);
		}
	}
    return ret;
}

static TableItem *
TreeTableSortFindPrev(TableItem *listPtr, TableItem *itemPtr, int col, int noCase, int *index)
{
    TableItem *Ptr, *posPtr=NULL;
	char *p, *q;
	int ret, i;
	for (Ptr=listPtr; Ptr; Ptr=Ptr->nextPtr)
	{
		if (col <= 0)
		{
			p = itemPtr->text;
			q = Ptr->text;
		}
		else
		{
			PosTabStr (itemPtr->text, p, col);
			PosTabStr (Ptr->text, q, col);
		}
    	if (noCase)
		{
			ret = strcasecmp (p, q);
			/* collapse the string comparision, if the text
			 * is identicale
			 */
			if (ret == 0 && col > 0)
			{
				ret = strcasecmp (itemPtr->text, Ptr->text);
			}
		}
		else
		{
			if (*p != *q)
			  ret = *p - *q;
			else
			  ret = strcmp (q, q);
			/* collapse the string comparision, if the text
			 * is identicale
			 */
			if (ret == 0 && col > 0)
			{
				ret = strcmp (itemPtr->text, Ptr->text);
			}
		}
		if (ret <= 0)
		{
			break;
		}
		if (index)
		{
			*index += 1 + Ptr->succNum;
		}
		posPtr = Ptr;
	}
    return posPtr;
}

void qqsort(char *base, int n, int size, int (*compar) (const void*,const void*));

static int
TreeTableSort_x(TreeTable *tablePtr, TableItem* parentPtr, int col, int noCase)
{
    TableItem *Ptr, *cutPtr, **list;
    int ret, size, i;
    
    /* sort children of parent children items recursively */
    if (parentPtr)
	  Ptr=parentPtr->succPtr;
    else
	  Ptr=tablePtr->itemPtr;
    for (; Ptr; Ptr=Ptr->nextPtr)
	{
		if (Ptr->succPtr != NULL)
		{
			ret = TreeTableSort_x (tablePtr, Ptr, col, noCase);
			if (ret != TCL_OK)
			  return ret;
		}
	}
    
    if (parentPtr == NULL)
	{
		cutPtr = tablePtr->itemPtr;
	}
    else
	{
		cutPtr = parentPtr->succPtr;
	}
    
    /* 
     * Make list to use qsort for sorting the list, this is
     * more faster as to insert an item on sorted list
     */
    for (size=0, Ptr=cutPtr; Ptr; Ptr = Ptr->nextPtr, size++)
	{
		/* Empty Loop */
	}
    /*
     * No Items or One Item
     */
    if (size<=1)
	{
		return TCL_OK;
	}
    
    /* cut list from tree table */
    if (parentPtr == NULL)
	{
		tablePtr->itemPtr = NULL;
	}
    else
	{
		parentPtr->succPtr = NULL;
	}
	
    list = (TableItem**)ckalloc (size * sizeof (TableItem*));
    for (i=0, Ptr=cutPtr; Ptr; Ptr = Ptr->nextPtr, i++)
	{
		list [i] = Ptr;
	}
    sortColNum = col;
    sortNoCase = noCase;
    qqsort ((char*) list, size, sizeof (TableItem*), (int (*)(const void*,const void*)) TreeTableSortCmp);
    
    /*
     * now add the sorted list to the tree
     */
    if (parentPtr == NULL)
	{
		tablePtr->itemPtr = list[0];
	}
    else
	{
		parentPtr->succPtr = list[0];
	}
    for (i=1; i<size; i++)
	{
		list[i-1]->nextPtr = list[i];
	}
    list[i-1]->nextPtr = NULL;
    
    /* get allocated memory free */
    ckfree ((char*)list);
    return TCL_OK;
}
static int
TreeTableSort(TreeTable *tablePtr, int argc, char **argv)
{
    int col = -1, noCase = 0;
	char * arg, c;
	int i, length, ret;
	for (i=0; i<argc; i++)
	{
		arg = argv[i];
		c = arg[1];
		length = strlen (arg);
		if (((c == 'c') && (strncmp(argv[i], "-column", length) == 0))
			|| ((c == 't') && (strncmp(argv[i], "-tab", length) == 0)))
		{
			
			if (i >= (argc-1))
			{
				Tcl_AppendResult(tablePtr->interp, "no value given for \"", arg,
								 "\" option",
								 (char *) NULL);
				return TCL_ERROR;
			}
			i++;
			col = atoi (argv[i]);
			/* if sortedInsertion flag is enabled, then 
			 * save the column number to sort new inserted items
			 * at the same column
			 */
			if (tablePtr->sortedInsertion)
			{
				tablePtr->sortColumn = col;
			}
		}
		else if ((c == 'n') && (strncmp(argv[i], "-nocase", length) == 0))
		{
			noCase = 1;
		}
		else
		{
			Tcl_AppendResult(tablePtr->interp, "bad switch \"", arg,
							 "\": must be -column <num>, -tab <num> or -nocase ",
							 (char *) NULL);
			return TCL_ERROR;
		}
	}
    
    /* free cached items */
    FREE_CACHE();
    FREE_PARENT_CACHE();
    
    ret = TreeTableSort_x (tablePtr, NULL, col, noCase);
    TreeTableRedrawRange (tablePtr);
    
    return ret;
}

/*
 * Get the BBox of a treetable item. The important thing here is
 * that the coordinates are returned as the coordinates of the
 * virtual position, that meens the hidden items haven't bbox coordinates
 * and coordinates requierment of an hidden item produces an ERROR.
 */
static int
TreeTableGetBBox (TreeTable *tablePtr, int index, int *x, int *y, int *w, int *h)
{
    TableItem *Ptr;
    *x = *y = *w = *h = 0;
    Ptr = TreeTableFindItem (tablePtr, index);
    if (Ptr == NULL)
	{
		Tcl_AppendResult (tablePtr->interp, "bad treetable index for BBox", NULL);
		return TCL_ERROR;
	}
    Ptr = TreeTableFindNotHiddenItem (tablePtr, index, 0);
    if (Ptr == NULL)
	{
		Tcl_AppendResult (tablePtr->interp,
						  "Can't get BBox of item, "
						  "the item is in a hidden sub tree", NULL);
		return TCL_ERROR;
	}
    
    *x = ITEM_X(Ptr);
    *y = ITEM_Y(Ptr, index);
    *w = *x + ITEM_WIDTH(Ptr);
    *h = *y + ITEM_HEIGHT();
	
    return TCL_OK;
}
/*******************************************************************/
/** QUICKSORT ******************************************************/
/** It can be added to another separate file !!!!!                **/
/*******************************************************************/
static void qst(char *base, char *max);
/*
 * qsort.c: Our own version of the system qsort routine which is faster by an
 * average of 25%, with lows and highs of 10% and 50%. The THRESHold below is
 * the insertion sort threshold, and has been adjusted for records of size 48
 * bytes. The MTHREShold is where we stop finding a better median.
 */

#define THRESH  4		       /* threshold for insertion */
#define MTHRESH 6		       /* threshold for median */

static int qsz;			       /* size of each record */
static int thresh;		       /* THRESHold in chars */
static int mthresh;		       /* MTHRESHold in chars */

static int	(*qcmp) (const void*,const void*); /* the comparison routine */
static void	qst (char *base, char *max);
/*
 * qqsort: First, set up some global parameters for qst to share.  Then,
 * quicksort with qst(), and then a cleanup insertion sort ourselves.  Sound
 * simple? It's not...
 */

void
qqsort(char *base, int n, int size, int (*compar) (const void*,const void*))
{
	register char *i;
	register char *j;
	register char *lo;
	register char *hi;
	register char *min;
	register char c;
	char   *max;

	if (n <= 1)
	  return;
	qsz = size;
	qcmp = compar;
	thresh = qsz * THRESH;
	mthresh = qsz * MTHRESH;
	max = base + n * qsz;
	if (n >= THRESH)
	{
		qst(base, max);
		hi = base + thresh;
	}
	else
	{
		hi = max;
	}
	/*
	 * First put smallest element, which must be in the first THRESH, in the
	 * first position as a sentinel.  This is done just by searching the
	 * first THRESH elements (or the first n if n < THRESH), finding the min,
	 * and swapping it into the first position.
	 */
	for (j = lo = base; (lo += qsz) < hi;)
	{
		if ((*qcmp) (j, lo) > 0)
		  j = lo;
	}
	if (j != base)
	{		       /* swap j into place */
		for (i = base, hi = base + qsz; i < hi;)
		{
			c = *j;
			*j++ = *i;
			*i++ = c;
		}
	}
	/*
	 * With our sentinel in place, we now run the following hyper-fast
	 * insertion sort. For each remaining element, min, from [1] to [n-1],
	 * set hi to the index of the element AFTER which this one goes. Then, do
	 * the standard insertion sort shift on a character at a time basis for
	 * each element in the frob.
	 */
	for (min = base; (hi = min += qsz) < max;)
	{
		while ((*qcmp) (hi -= qsz, min) > 0);
		if ((hi += qsz) != min)
		{
			for (lo = min + qsz; --lo >= min;)
			{
				c = *lo;
				for (i = j = lo; (j -= qsz) >= hi; i = j)
				  *i = *j;
				*i = c;
			}
		}
	}
}

/*
 * qst: Do a quicksort.  First, find the median element, and put that one in
 * the first place as the discriminator.  (This "median" is just the median
 * of the first, last and middle elements).  (Using this median instead of
 * the first element is a big win). Then, the usual partitioning/swapping,
 * followed by moving the discriminator into the right place.  Then, figure
 * out the sizes of the two partions, do the smaller one recursively and the
 * larger one via a repeat of this code.  Stopping when there are less than
 * THRESH elements in a partition and cleaning up with an insertion sort (in
 * our caller) is a huge win. All data swaps are done in-line, which is
 * space-losing but time-saving. (And there are only three places where this
 * is done).
 */
static void qst(char *base, char *max)
{
	register char *i;
	register char *j;
	register char *jj;
	register char *mid;
	register int ii;
	register char c;
	char   *tmp;
	int     lo;
	int     hi;
	
	lo = (int)(max - base);		/* number of elements as chars */
	do
	{
		/*
		 * At the top here, lo is the number of characters of elements in the
		 * current partition.  (Which should be max - base). Find the median
		 * of the first, last, and middle element and make that the middle
		 * element.  Set j to largest of first and middle.  If max is larger
		 * than that guy, then it's that guy, else compare max with loser of
		 * first and take larger.  Things are set up to prefer the middle,
		 * then the first in case of ties.
		 */
		mid = i = base + qsz * ((unsigned) (lo / qsz) >> 1);
		if (lo >= mthresh)
		{
			j = ((*qcmp) ((jj = base), i) > 0 ? jj : i);
			if ((*qcmp) (j, (tmp = max - qsz)) > 0)
			{
				/* switch to first loser */
				j = (j == jj ? i : jj);
				if ((*qcmp) (j, tmp) < 0)
				  j = tmp;
			}
			if (j != i)
			{
				ii = qsz;
				do
				{
					c = *i;
					*i++ = *j;
					*j++ = c;
				} while (--ii);
			}
		}
		/* Semi-standard quicksort partitioning/swapping */
		for (i = base, j = max - qsz;;)
		{
			while (i < mid && (*qcmp) (i, mid) <= 0)
			  i += qsz;
			while (j > mid)
			{
				if ((*qcmp) (mid, j) <= 0)
				{
					j -= qsz;
					continue;
				}
				tmp = i + qsz;	       /* value of i after swap */
				if (i == mid)
				{		       /* j <-> mid, new mid is j */
					mid = jj = j;
				}
				else
				{		       /* i <-> j */
					jj = j;
					j -= qsz;
				}
				goto swap;
			}
			if (i == mid)
			{
				break;
			}
			else
			{		       /* i <-> mid, new mid is i */
				jj = mid;
				tmp = mid = i;	       /* value of i after swap */
				j -= qsz;
			}
		  swap:
			ii = qsz;
			do
			{
				c = *i;
				*i++ = *jj;
				*jj++ = c;
			} while (--ii);
			i = tmp;
		}
		/*
		 * Look at sizes of the two partitions, do the smaller one first by
		 * recursion, then do the larger one by making sure lo is its size,
		 * base and max are update correctly, and branching back. But only
		 * repeat (recursively or by branching) if the partition is of at
		 * least size THRESH.
		 */
		i = (j = mid) + qsz;
		if ((lo = (int)(j - base)) <= (hi = (int)(max - i)))
		{
			if (lo >= thresh)
			  qst(base, j);
			base = i;
			lo = hi;
		}
		else
		{
			if (hi >= thresh)
			  qst(i, max);
			max = j;
		}
	} while (lo >= thresh);
}

void
ViewArgs (char *reason, int argc, char *argv[], int mode)
{
    int i;
	if (reason)
	{
		fprintf (stderr, "%s\nused arguments:\n", reason);
	}
	for (i=0; i<argc; i++)
	{
		if (mode)
		{
			fprintf (stderr, "%s ", argv[i]);
		}
		else
		{
			fprintf (stderr, "argv[%i] = [%s]\n", i, argv[i]);
		}
	}
    if (mode && mode != 2)
	{
		fprintf (stderr, "\n");
	}
}
#ifdef TEST_TREE

#ifdef USE_CACHE
static void
ViewCache(TreeTable*tablePtr)
{
    int i;
	for (i=0; i<MAX_CACHED; i++)
	{
		fprintf (stderr, "cache(%i)=[%i, %s]\n",
				 i,
				 tablePtr->cachedPos [i],
				 tablePtr->cachedItem[i] != NULL
				 ? tablePtr->cachedItem[i]->text
				 : "NULL");
	}
	printf ("parentcache[%i, %s]\n",
			tablePtr->cachedParent,
			tablePtr->cachedParentPtr
			? tablePtr->cachedParentPtr->text
			: "NULL");
}
#endif /* USE_CACHE */
#endif /* TEST_TREE */
