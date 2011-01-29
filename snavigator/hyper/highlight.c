
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

#include "highlight.h"
#include "mxlogger.h"

#include <tcl.h>
#include "tclInt.h"
#include "tkText.h"

#ifdef WIN32
#include "windows.h"
#endif

struct Paf_High_Position paf_high_pos;

int tcl_highlight_lex();
int c_highlight_lex();
int java_highlight_lex();
int ch_highlight_lex();
int py_highlight_lex();
void tcl_highlight_init_func(int maxs, int lineno, int charno, void (**func) (), void *idx1, void *idx2);
void c_highlight_init_func(int maxs, int lineno, int charno, void (**func) (), void *idx1, void *idx2);
void ch_highlight_init_func(int maxs, int lineno, int charno, void (**func) (), void *idx1, void *idx2);
void java_highlight_init_func(int maxs, int lineno, int charno, void (**func) (), void *idx1, void *idx2);
void py_highlight_init_func(int maxs, int lineno, int charno, void (**func) (), void *idx1, void *idx2);

static char *high_tag_names[] = {
	NULL, "rem", "key", "str", NULL
};

static int eos_read;

#if (TCL_MAJOR_VERSION >= 8) && (TCL_MINOR_VERSION >= 1)
#define CHARINDEX byteIndex
#else
#define CHARINDEX charIndex
#endif

int lexinput_tktext(char *buf, int max_size, int buf_size, void *index1, void *index2)
{
	int length, nbytes;
	Tcl_DString internal, temp;

	static int first = 1;
	static Tcl_DString external;
	static Tcl_Encoding encoding;

	if(first) {
		/* The lexers require ASCII encoding. */
		encoding = Tcl_GetEncoding(NULL, "ascii");
		if(encoding == NULL) {
			/* No ASCII encoding available. */
			return 0;
		}
		Tcl_DStringInit(&external);
		first = 0;
	}
	Tcl_DStringInit(&internal);

	if(Tcl_DStringLength(&external) == 0) {
		/* Translate the text to `external'. */
		if(tk_text_buffer(&internal, buf_size, index1, index2) > 0) {
			Tcl_UtfToExternalDString(encoding, Tcl_DStringValue(&internal), Tcl_DStringLength(&internal), &external);
		} else {
			return 0;
		}
	}

	/* Fill up the user-provided buffer as much as possible. */
	length = Tcl_DStringLength(&external);
	nbytes = (length > max_size) ? max_size : length;
	memcpy(buf, Tcl_DStringValue(&external), nbytes);

	/* I wish DStrings had a copy constructor. In fact, sometimes I wish
	 Tcl was written in C++. */
	if(length > nbytes) {
		Tcl_DStringInit(&temp);
		Tcl_DStringAppend(&temp, Tcl_DStringValue(&external) + nbytes, length - nbytes);
		Tcl_DStringFree(&external);
		Tcl_DStringInit(&external);
		Tcl_DStringAppend(&external, Tcl_DStringValue(&temp), Tcl_DStringLength(&temp));
		Tcl_DStringFree(&temp);
	} else {
		Tcl_DStringFree(&external);
		Tcl_DStringInit(&external);
	}

	/* Clean up. */
	Tcl_DStringFree(&internal);

	return nbytes;
}

static void remove_tag(register TkText * textPtr, Tcl_Interp * interp, char *text_widget, int del, char *idx1, char *idx2)
{
	Tcl_HashEntry *hPtr;
	Tcl_HashSearch search;
	TkTextTag *tagPtr;
	int wargc;
	char *wargv[10];

	wargc = 0;
	wargv[wargc++] = text_widget;
	wargv[wargc++] = "tag";
	wargv[wargc++] = "remove";
	wargv[wargc++] = NULL;
	wargv[wargc++] = idx1;
	wargv[wargc++] = idx2;

	for (hPtr = Tcl_FirstHashEntry(&textPtr->tagTable, &search); hPtr != NULL; hPtr = Tcl_NextHashEntry(&search)) {
		tagPtr = (TkTextTag *) Tcl_GetHashValue(hPtr);
		if(tagPtr == textPtr->selTagPtr) {
			continue;
		}
		if(del == -1) {		/* Should we delete all ? */
			/* In Source-Navigator blanks are used in symbol
			 * definitions tags, highlighting tags don't contain
			 * any blanks.
			 */
			if(strchr(tagPtr->name, ' ')) {
				wargv[2] = "delete";
				wargv[3] = tagPtr->name;
				wargc = 4;
			} else {
				wargv[2] = "remove";
				wargv[3] = tagPtr->name;
				wargc = 6;
			}
		} else {
			wargv[3] = tagPtr->name;
		}
		TkTextTagCmd(textPtr, interp, wargc, wargv);
	}
}

int Sn_Syntax_Highlight(ClientData clientData, Tcl_Interp * interp, int argc, char **argv)
{
	register TkText *textPtr = NULL;
	Tcl_CmdInfo infoPtr;
	int result = TCL_OK;
	int del = 0;
	char *argv0 = argv[0];
	int wargc;
	char *wargv[10];
	char *language;
	char *idx1;
	char *idx2;
	char *text_widget;
	char endpos[TK_POS_CHARS];
	char begpos[TK_POS_CHARS];
	int val;
	int (*high_func) () = NULL;
	void (*flush_lex_scanner) () = NULL;
	void (*high_init_func) (int ms, int lineno, int charno, void (**func) (), void *idx1, void *idx2) = NULL;
	Tcl_CmdProc *text_proc;
	int update = 0;
	TkTextIndex lex_index1;
	TkTextIndex lex_index2;
	TkTextIndex lex_end_index;
	char prev_begpos[TK_POS_CHARS];
	int lex_buf_size = 0;

	eos_read = 0;

	while (argc > 1 && argv[1][0] == '-') {
		if(strncmp(argv[1], "-delete", 5) == 0) {
			del = 1;
			argc--;
			argv++;
		} else if(strncmp(argv[1], "-delall", 5) == 0) {
			del = -1;
			argc--;
			argv++;
		} else if(strncmp(argv[1], "-update", 4) == 0) {
			lex_buf_size = 256;	/* It is probably not necessary to fill */
			/* the complete lex buffer. */
			update = 1;
			argc--;
			argv++;
		} else
			break;
	}

	if(argc != 4 && argc != 5) {
		Tcl_AppendResult(interp, "wrong # args: should be \"", argv0, " ?-delete? ?-delall? language text_widget index1 ?index2?", NULL);

		return TCL_ERROR;
	}

	language = argv[1];
	text_widget = argv[2];
	idx1 = argv[3];
	idx2 = argc == 5 ? argv[4] : idx1;

	LOGGER((LOGFP, "highlight lang: <%s> text: <%s> idx1: <%s> idx2: <%s>\n", language, text_widget, idx1, idx2));
	if(strcmp(language, "tcl") == 0) {
		high_func = tcl_highlight_lex;
		high_init_func = tcl_highlight_init_func;
	} else if(strcmp(language, "c++") == 0) {
		high_func = c_highlight_lex;
		high_init_func = c_highlight_init_func;
	} else if(strcmp(language, "java") == 0) {
		high_func = java_highlight_lex;
		high_init_func = java_highlight_init_func;
	} else if(strcmp(language, "chill") == 0) {
		high_func = ch_highlight_lex;
		high_init_func = ch_highlight_init_func;
	} else if(strcmp(language, "python") == 0) {
		high_func = py_highlight_lex;
		high_init_func = py_highlight_init_func;
	}

	if(!high_func) {
		return TCL_OK;
	}

	if(!Tcl_GetCommandInfo(interp, text_widget, &infoPtr)) {
		Tcl_AppendResult(interp, "wrong # \"", text_widget, "\" does not exist", (char *) NULL);
		return TCL_ERROR;
	}

	textPtr = (TkText *) infoPtr.clientData;
	Tcl_Preserve((ClientData) textPtr);
	text_proc = (Tcl_CmdProc *) infoPtr.proc;

	if(del) {
		remove_tag(textPtr, interp, text_widget, del, idx1, idx2);
	}

	TkTextGetIndex(interp, textPtr, "end", &lex_end_index);

	if(TkTextGetIndex(interp, textPtr, idx1, &lex_index1) != TCL_OK || TkTextGetIndex(interp, textPtr, idx2, &lex_index2) != TCL_OK) {
		result = TCL_ERROR;
		goto done;
	}
	if(TkTextIndexCmp(&lex_index1, &lex_index2) >= 0) {
		result = TCL_OK;
		goto done;
	}
	high_init_func(lex_buf_size, TkBTreeLineIndex(lex_index1.linePtr) + 1,
		       lex_index1.CHARINDEX, &flush_lex_scanner, (void *) &lex_index1, (void *) &lex_end_index);

	wargc = 0;
	wargv[wargc++] = text_widget;
	wargv[wargc++] = "tag";
	wargv[wargc++] = "add";
	wargv[wargc++] = NULL;
	wargv[wargc++] = begpos;
	wargv[wargc++] = endpos;

	strcpy(prev_begpos, idx1);
	while ((val = (*high_func) ()) != 0) {
		sprintf(begpos, "%d.%d", paf_high_pos.beg_lineno, paf_high_pos.beg_charno);
		sprintf(endpos, "%d.%d", paf_high_pos.end_lineno, paf_high_pos.end_charno);
		wargv[3] = high_tag_names[val];

		if(update) {
			int tag_argc = 0;
			char *tag_argv[10];
			char *p_end;
			int cou;
			int tag_off;

			tag_argv[tag_argc++] = text_widget;
			tag_argv[tag_argc++] = "tag";
			tag_argv[tag_argc++] = "prevrange";
			tag_off = tag_argc;
			tag_argv[tag_argc++] = NULL;
			tag_argv[tag_argc++] = endpos;
			for (cou = 1; cou <= PAF_HIGH_STRING; cou++) {
				tag_argv[tag_off] = high_tag_names[cou];
				Tcl_ResetResult(interp);
				TkTextTagCmd(textPtr, interp, tag_argc, tag_argv);

				p_end = strchr(interp->result, ' ');
				/* Check whether it is synchronized ! */
				if(p_end && strcmp(p_end + 1, endpos) == 0) {
					eos_read = 1;
					/* It will get flex recognized that no further
					 * input is available. */
					flush_lex_scanner();
					break;
				}
			}
			LOGGER((LOGFP, "Add %s %d %s %s\n", high_tag_names[val], val, begpos, endpos));
		}
		if(update) {
			int tag_argc = 0;
			char *tag_argv[10];
			int cou;
			int tag_off;

			tag_argv[tag_argc++] = text_widget;
			tag_argv[tag_argc++] = "tag";
			tag_argv[tag_argc++] = "remove";
			tag_off = tag_argc;
			tag_argv[tag_argc++] = NULL;
			tag_argv[tag_argc++] = prev_begpos;
			tag_argv[tag_argc++] = endpos;

			for (cou = 1; cou <= PAF_HIGH_STRING; cou++) {
				tag_argv[tag_off] = high_tag_names[cou];
				TkTextTagCmd(textPtr, interp, tag_argc, tag_argv);
			}
			strcpy(prev_begpos, endpos);
		}
		TkTextTagCmd(textPtr, interp, wargc, wargv);
	}
done:
	Tcl_Release((ClientData) textPtr);

	return result;
}

int tk_text_buffer(Tcl_DString * buf, int buf_size, void *idx1, void *idx2)
{
	int i, size;
	TkTextIndex *lex_index1 = (TkTextIndex *) idx1;
	TkTextIndex *lex_end_index = (TkTextIndex *) idx2;

	/* Get a handle to tkTextCharType. */
#ifndef WIN32
	static Tk_SegType *tk_text_tp = &tkTextCharType;
#else
	/* Under Win32, we may need to load the DLL. */
	static int first = 1;
	static Tk_SegType *tk_text_tp;

	if(first) {
		HINSTANCE tkdll;
		char dllname[MAX_PATH];

		first = 0;

		sprintf(dllname, "rhtk%d%d%s.dll", TK_MAJOR_VERSION, TK_MINOR_VERSION, TK_DBGX);
		LOGGER((LOGFP, "GetModuleHandle: <%s>\n", dllname));
		tkdll = LoadLibrary(dllname);

		if(!tkdll) {
			panic("could not load dll \"%s\"\n", dllname);
		}

		tk_text_tp = (Tk_SegType *) GetProcAddress(tkdll, "tkTextCharType");
		FreeLibrary(tkdll);
	}
#endif				/* WIN32 */

	/* What does this do? */
	if(eos_read) {
		return 0;
	}

	/* Extract characters from the Tk text widget. */
	for (size = 0, i = 0; (buf_size == 0 || i < buf_size); i++) {
		int offset, last;
		TkTextSegment *segPtr;

		segPtr = TkTextIndexToSeg(lex_index1, &offset);
		last = segPtr->size;

		/* Are we on the last line of the Tk text buffer? */
		if(lex_index1->linePtr == lex_end_index->linePtr) {
			int last2;

			/* Is this the end of the Tk text buffer? */
			if(lex_end_index->CHARINDEX == lex_index1->CHARINDEX) {
				break;
			}
			last2 = lex_end_index->CHARINDEX - lex_index1->CHARINDEX + offset;
			if(last2 < last) {
				last = last2;
			}
		}

		/* Is the segement element a piece of text? */
		if(segPtr->typePtr == tk_text_tp) {
			int size2 = last - offset;

			Tcl_DStringAppend(buf, segPtr->body.chars + offset, size2);
			size += size2;

			TkTextIndexForwBytes(lex_index1, size2, lex_index1);
		} else {
			/* No.  Just skip over it. */
			TkTextIndexForwBytes(lex_index1, last - offset, lex_index1);
		}
	}
	return size;
}
