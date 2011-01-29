/*
 * winCon.c --
 *
 *  Main entry point for wish and other Tk-based applications
 *      when using win32 native console instead of TkConsole window
 *
 * Copyright (c) 1998 Kai Morich
 */

#include <windows.h>
#include <stdio.h>

#define BUFSIZE 4096


HANDLE hChildStdinRd, hChildStdinWr, hChildStdinWrDup,
   hChildStdoutRd, hChildStdoutWr, hChildStdoutRdDup,
   hChildStderrRd, hChildStderrWr, hChildStderrRdDup, 
   hSaveStdin, hSaveStdout, hSaveStderr;
HANDLE endEvent;    

void ErrorExit (char *msg)
{ 
    LPVOID lpMsgBuf;
    DWORD     l;

    CloseHandle(hChildStdinWrDup);
    CloseHandle(hChildStdoutRdDup);
    CloseHandle(hChildStderrRdDup);
    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
                  NULL,  GetLastError(),
                  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                 (LPTSTR) &lpMsgBuf,  0,  NULL);
#define MSG "Error in startup: "
    WriteFile(hSaveStderr, MSG,      strlen(MSG),      &l, NULL);
    WriteFile(hSaveStderr, msg,      strlen(msg),      &l, NULL);
    WriteFile(hSaveStderr, ": ",     2,                &l, NULL);
    WriteFile(hSaveStderr, lpMsgBuf, strlen(lpMsgBuf), &l, NULL);
    ExitProcess(1);
} 

/*
 * Read input, and write to child's STDIN
 */
DWORD WINAPI WriteStdinPipe(LPVOID x)
{
    DWORD   dwRead, dwWritten;
    CHAR    chBuf[BUFSIZE];

    /* Read from a file and write its contents to a pipe. */
    while (1) {
        FlushFileBuffers(hChildStdinWrDup);
        if (!ReadFile (hSaveStdin, chBuf, BUFSIZE, &dwRead, NULL) || dwRead == 0)
            break; 
        if (!WriteFile(hChildStdinWrDup, chBuf, dwRead, &dwWritten, NULL)) 
            break; 
    }
    SetEvent(endEvent);
    return 0;
} 

/*
 * Read output from the child process, and write to parent's STDOUT
 */
DWORD WINAPI ReadStdoutPipe(LPVOID x)
{
    DWORD dwRead, dwWritten;
    CHAR chBuf[BUFSIZE];

    while (1) {
        if (!ReadFile (hChildStdoutRdDup, chBuf, BUFSIZE, &dwRead, NULL) || dwRead == 0)
            break;
        if (!WriteFile(hSaveStdout, chBuf, dwRead, &dwWritten, NULL)) 
            break; 
    } 
    SetEvent(endEvent);
    return 0;
}

/*
 * Read error from the child process, and write to parent's STDERR
 */
DWORD WINAPI ReadStderrPipe(LPVOID x)
{
    DWORD dwRead, dwWritten;
    CHAR chBuf[BUFSIZE];

    while (1) {
        if (!ReadFile (hChildStderrRdDup, chBuf, BUFSIZE, &dwRead, NULL) || dwRead == 0)
            break;
        if (!WriteFile(hSaveStderr, chBuf, dwRead, &dwWritten, NULL))
            break;
    }
    SetEvent(endEvent);
    return 0;
} 



int main(int argc, char* argv[])
{
    PROCESS_INFORMATION piProcInfo;
    STARTUPINFO siStartInfo;
    SECURITY_ATTRIBUTES saAttr; 
    char cmd[BUFSIZE];
    DWORD threadId;
    long l;

    hSaveStdin = GetStdHandle(STD_INPUT_HANDLE); 
    hSaveStdout = GetStdHandle(STD_OUTPUT_HANDLE); 
    hSaveStderr = GetStdHandle(STD_ERROR_HANDLE); 

    /* Set the bInheritHandle flag so pipe handles are inherited. */
    saAttr.nLength = sizeof(SECURITY_ATTRIBUTES); 
    saAttr.bInheritHandle = TRUE; 
    saAttr.lpSecurityDescriptor = NULL; 

    /* create pipes */
    if (! CreatePipe(&hChildStdoutRd, &hChildStdoutWr, &saAttr, 0)) 
        ErrorExit("Stdout pipe creation failed"); 

    if (! CreatePipe(&hChildStderrRd, &hChildStderrWr, &saAttr, 0)) 
        ErrorExit("Stderr pipe creation failed"); 

    if (! CreatePipe(&hChildStdinRd, &hChildStdinWr, &saAttr, 0)) 
        ErrorExit("Stdin pipe creation failed\n"); 

    /* Duplicate the handles to the pipes so it is not inherited. */
    if (!DuplicateHandle(GetCurrentProcess(), hChildStdinWr, 
                         GetCurrentProcess(), &hChildStdinWrDup, 0, 
                         FALSE, DUPLICATE_SAME_ACCESS))
        ErrorExit("DuplicateHandle failed"); 
    CloseHandle(hChildStdinWr); 

    if (!DuplicateHandle(GetCurrentProcess(), hChildStdoutRd,
                         GetCurrentProcess(), &hChildStdoutRdDup , 0,
                         FALSE, DUPLICATE_SAME_ACCESS))
        ErrorExit("DuplicateHandle failed");
    CloseHandle(hChildStdoutRd);

    if (!DuplicateHandle(GetCurrentProcess(), hChildStderrRd,
                         GetCurrentProcess(), &hChildStderrRdDup , 0,
                         FALSE, DUPLICATE_SAME_ACCESS))
        ErrorExit("DuplicateHandle failed");
    CloseHandle(hChildStderrRd);

    /* create event object */
    endEvent = CreateEvent(NULL, 0, 0, NULL);
    if (endEvent == INVALID_HANDLE_VALUE)
        ErrorExit("CreateEvent failed");

    /* Set up members of STARTUPINFO structure. */
    ZeroMemory( &siStartInfo, sizeof(STARTUPINFO) );
    siStartInfo.cb = sizeof(STARTUPINFO);
    siStartInfo.dwFlags = STARTF_USESTDHANDLES;
    siStartInfo.hStdInput = hChildStdinRd;
    siStartInfo.hStdOutput= hChildStdoutWr;
    siStartInfo.hStdError = hChildStderrWr;

    /* create command string */
    strcpy(cmd, APPNAME);
    if (strchr(GetCommandLine(), ' '))
        strcat(cmd, strchr(GetCommandLine(), ' '));
    
    /* Create the child process. */
    if ( !CreateProcess (NULL,
                         cmd,           /* command line                        */
                         NULL,          /* process security attributes         */
                         NULL,          /* primary thread security attributes  */
                         TRUE,          /* handles are inherited               */
                         0,             /* creation flags                      */
                         NULL,          /* use parent's environment            */
                         NULL,          /* use parent's current directory      */
                         &siStartInfo,  /* STARTUPINFO pointer                 */
                         &piProcInfo))  /* receives PROCESS_INFORMATION        */
        ErrorExit("CreateProcess failed");
    WaitForInputIdle(piProcInfo.hProcess, 5000);

    /* Close pipe ends used by client. */
    if (!CloseHandle(hChildStdinRd)) 
        ErrorExit("Closing handle failed"); 
    if (!CloseHandle(hChildStdoutWr)) 
        ErrorExit("Closing handle failed");
    if (!CloseHandle(hChildStderrWr)) 
        ErrorExit("Closing handle failed"); 

    /* 
     * now use hChildStdinWrDup, hChildStdoutRdDup, hChildStderrRdDup 
     */

    CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE) WriteStdinPipe, 0, 0, &threadId);
    CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE) ReadStdoutPipe, 0, 0, &threadId);
    CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE) ReadStderrPipe, 0, 0, &threadId);

    /* 
     * wait until end
     */
    WaitForSingleObject(endEvent, INFINITE);

    CloseHandle(hChildStdinWrDup);
    CloseHandle(hChildStdoutRdDup);
    CloseHandle(hChildStderrRdDup);
    /* fixme: get returncode from wish */
    ExitProcess(0);
    return 0;
}
