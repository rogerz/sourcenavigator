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

#include <direct.h>
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#include <shellapi.h>

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

enum
{
	SN_PATH_WINDOWS, /* convert path to windows native */
	SN_PATH_UNIX     /* convert path to unix native */
};
static void sn_internal_convert_path (char*path, int mode)
{
	char *p;
	char slash1, slash2;
	if (mode == SN_PATH_WINDOWS)
	{
		slash1 = '/';
		slash2 = '\\';
	}
	else
	{
		slash1 = '\\';
		slash2 = '/';
	}
	for (p = path; *p; p++)
	{
		if (*p == slash1)
			*p = slash2;
	}
}

static void sn_append_option_to_command_line (char*cmd, char *arg)
{
	int have_blanks;
	if (*arg == 0)
	{
		return;
	}
	if (*cmd)
	{
		strcat (cmd, " ");
	}
	if (strchr (arg, ' ') != NULL)
	{
		strcat (cmd, "\"");
		have_blanks = 1;
	}
	else
	{
		have_blanks = 0;
	}
	strcat (cmd, arg);
	if (have_blanks)
	{
		strcat (cmd, "\"");
	}
}

static char *
sn_next_cmd_argument (char *cmd, char *buf, int size)
{
	char *p=cmd, *q=buf;
	int i, quoted;
	
	// skip spaces
	while (*p == ' ') p++;
	// skip quotes
	if (*p == '\"')
	{
		p++;
		quoted = 1;
	}
	else
	{
		quoted = 0;
	}
	for (i=0; *p; p++)
	{
		if (! quoted && *p == ' ')
		{
			break;
		}
		if (quoted && *p == '\"')
		{
			p++;
			break;
		}
		if (i < size-1)
		{
			*q ++ = *p;
			i++;
		}
	}
	*q = 0;
	return p;
}

int WINAPI WinMain( HINSTANCE cur, HINSTANCE prev, LPSTR cmd, int show)
{
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	HANDLE testFile;
	char sn_loc[MAXPATHLEN];
	char hyper_cmd_line[MAXPATHLEN];
	char etc_dir[MAXPATHLEN];
	char tmp[MAXPATHLEN];
	char installDir[MAXPATHLEN];
	char *cmdLine;
	char *relativePtr;

	/*
	 * Get the name of the executable.
	 */

	cmdLine = GetCommandLine();
	sn_next_cmd_argument(cmdLine, sn_loc, sizeof(sn_loc));

	if (GetFullPathName(sn_loc, MAXPATHLEN, sn_loc, 0)) {
		if (strstr(strlwr(sn_loc), "snpdbg.exe")) {
			sn_loc[strlen(sn_loc) - strlen("snpdbg.exe")] = 0;
		} else if (strstr(strlwr(sn_loc), "snpdbg")) {
			sn_loc[strlen(sn_loc) - strlen("snpdbg")] = 0;
		}
	} else {
		/*
		 * set path to current directory
		 */
		if (!GetCurrentDirectory(MAXPATHLEN, sn_loc)) {
			MessageBox(0, "Failed to start snpdbg", "Error", MB_OK|MB_ICONERROR);
			exit(1);
		}
	}

	if (sn_loc[strlen(sn_loc) - 1] == '\\') {
		sn_loc[strlen(sn_loc) - 1] = '\0';
	}

	/*
	 * Now, we have to find the etc/snpdbg file.
	 */

	strcpy(tmp, sn_loc);

	relativePtr = strrchr(tmp, '\\');
	if (relativePtr == NULL) {
		MessageBox(0, "Failed to start snpdbg", "Error", MB_OK|MB_ICONERROR);
		exit(1);
	}
	strcpy(relativePtr, "\\share\\snavigator\\etc\\snpdbg");

	testFile = CreateFile(tmp, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
	if (INVALID_HANDLE_VALUE == testFile) {
		strcpy(tmp, sn_loc);
		relativePtr = strrchr(tmp, '\\');
		relativePtr[0] = '\0';
		relativePtr = strrchr(tmp, '\\');
		if (NULL == relativePtr) {
			MessageBox(0, "Failed to start snpdbg", "Error", MB_OK|MB_ICONERROR);
			exit(1);
		}

		strcpy(relativePtr, "\\share\\etc\\snpdbg");
		testFile = CreateFile(tmp, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
		if (INVALID_HANDLE_VALUE == testFile) {
			MessageBox(0, "Failed to start snpdbg", "Error", MB_OK|MB_ICONERROR);
			exit(1);
		}
	}
	CloseHandle(testFile);

	strcpy(etc_dir, tmp);

	/*
	 * relativePtr is still set to "\\share\\etc\\snpdbg"
	 */

	relativePtr[0] = '\0';

 	/*
 	 * Make sure all slashes are POSIX style.
 	 */
 	sn_internal_convert_path(tmp, SN_PATH_UNIX);
 	SetEnvironmentVariable("SN_INSTALL_DIR", tmp);


	ZeroMemory(&si, sizeof(si));
	si.cb = sizeof(si);

	/*
	 * Allow SN to understand parameters also,
	 * do not use relative paths
	 *
	 * construct a command "hyper -f <file>"
	 */
	hyper_cmd_line[0] = 0;
	sprintf (tmp, "%s\\%s", sn_loc, "hyper");
	sn_append_option_to_command_line (hyper_cmd_line, tmp);
	sn_append_option_to_command_line (hyper_cmd_line, "-f");
	sn_append_option_to_command_line (hyper_cmd_line, etc_dir);

	/* add "--" after -file to make sure that SN parameters
	 * don't colapse with wish parameters
	 * as example: SN -c == -colormap for wish
	 */
	sn_append_option_to_command_line (hyper_cmd_line, "--");
	
	/*
	 * Command line arguments are already quoted with ".." when they
	 * contain blanks, so no need to rework them.
	 */
	if (cmd != NULL && cmd[0])
	{
		strcat (hyper_cmd_line, " ");
		strcat (hyper_cmd_line, cmd);
	}

	if(CreateProcess(
		NULL,
		hyper_cmd_line	/*"hyper -f share\\etc\\snpdbg"*/ , /* pointer to command line string */
		NULL,	/* pointer to process security attributes */
		NULL,	/* pointer to thread security attributes */
		FALSE,	/* handle inheritance flag */
		0,		/* creation flags */
		NULL,	/* pointer to new environment block */
		NULL,	/* pointer to current directory name */
		&si,	/* pointer to STARTUPINFO */
		&pi 	/* pointer to PROCESS_INFORMATION   */
	) == 0) {
		MessageBox(0, "Failed to start snpdbg", "Error", MB_OK|MB_ICONERROR);
		return GetLastError();
	}

	WaitForSingleObject(pi.hProcess, INFINITE);
	CloseHandle(pi.hProcess);
	CloseHandle(pi.hThread);

	return 0;
}
