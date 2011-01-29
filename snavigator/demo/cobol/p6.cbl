       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN01A.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           PRINTER IS PRINTER-DISPLAY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *************************************************************
      *                                                           *
      *  KEY-SWITCH IS USED FOR THE RETURN VALUE OF INKEY         *
      *                                                           *
      *  KEY-SWITCH-? IS THE VALUE OF FUNCTION KEYS               *
      *                                                           *
      *************************************************************

       01 KEY-SWITCH-MENU                         PIC X.
       01 KEY-SWITCH-F1                           PIC X
                                                  VALUE ";".
       01 KEY-SWITCH-F2                           PIC X
                                                  VALUE "<".
       01 KEY-SWITCH-F3                           PIC X
                                                  VALUE "=".
       01 KEY-SWITCH-F4                           PIC X
                                                  VALUE ">".
       01 KEY-SWITCH-F5                           PIC X
                                                  VALUE "?".
       01 KEY-SWITCH-F6                           PIC X
                                                  VALUE "@".
       01 KEY-SWITCH-F7                           PIC X
                                                  VALUE "A".
       01 KEY-SWITCH-F8                           PIC X
                                                  VALUE "B".
       01 KEY-SWITCH-F9                           PIC X
                                                  VALUE "C".
       01 KEY-SWITCH-F10                          PIC X
                                                  VALUE "D".
      **************************************************************
      *                                                            *
      *   SCRN-FUNC-KEYS ARE THE 24 LINE SCREEN MENU DISPLAY AREA  *
      *                                                            *
      *   MOVING THE APROP. COMMAND DISC IN THE FUNCTION KEY DISP  *
      *                                                            *
      *                                                            *
      **************************************************************

       01 SCRN-FUNC-KEYS.
          05 F1                                   PIC X(6)
                                                  VALUE SPACES.
          05 F2                                   PIC X(6)
                                                  VALUE SPACES.
          05 F3                                   PIC X(6)
                                                  VALUE SPACES.
          05 F4                                   PIC X(6)
                                                  VALUE SPACES.
          05 F5                                   PIC X(6)
                                                  VALUE SPACES.
          05 F6                                   PIC X(6)
                                                  VALUE SPACES.
          05 F7                                   PIC X(6)
                                                  VALUE SPACES.
          05 F8                                   PIC X(6)
                                                  VALUE SPACES.
          05 F9                                   PIC X(6)
                                                  VALUE SPACES.
          05 F10                                  PIC X(6)
                                                  VALUE SPACES.
          05 F1-25-L                              PIC X(6)
                                                  VALUE SPACES.
          05 F2-25-L                              PIC X(6)
                                                  VALUE SPACES.
          05 F3-25-L                              PIC X(6)
                                                  VALUE SPACES.
          05 F4-25-L                              PIC X(6)
                                                  VALUE SPACES.
          05 F5-25-L                              PIC X(6)
                                                  VALUE SPACES.
          05 F6-25-L                              PIC X(6)
                                                  VALUE SPACES.
          05 F7-25-L                              PIC X(6)
                                                  VALUE SPACES.
          05 F8-25-L                              PIC X(6)
                                                  VALUE SPACES.
          05 F9-25-L                              PIC X(6)
                                                  VALUE SPACES.
          05 F10-25-L                             PIC X(6)
                                                  VALUE SPACES.


      ***************************************************************
      *                                                             *
      *   WORK-NAMES-RECORD IS THE WORKING STORAGE COPY OF THE      *
      *                                                             *
      *   DATA FILE, ALL EDITING, DISPLAYING, AND DATA ENTRY USES   *
      *                                                             *
      *   THESE FIELDS.                                             *
      ***************************************************************


       01 PASSWORD-ACCESS-FLAG                   PIC X.
       01 CALLING-PRGM-NAME                      PIC X(12)
                                                 VALUE "MAINMENU.EXE".
       SCREEN SECTION.

       01 SCREEN1-ADDRESS-ENTRY.
          05 LINE 1 COLUMN 16       VALUE "L I B R A R Y"
                                    HIGHLIGHT.
          05 LINE 1 COLUMN 33       VALUE "M A N A G M E N T"
                                    HIGHLIGHT.
          05 LINE 1 COLUMN 57       VALUE "S Y S T E M"
                                    HIGHLIGHT.
          05 LINE 2 COLUMN 1        HIGHLIGHT
             VALUE "****************************************".
          05 LINE 2 COLUMN 41       HIGHLIGHT
             VALUE "****************************************".


       01 FUNC-KEYS.
          05 LINE 23 COLUMN 2       FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    VALUE "  F1  ".  
          05 LINE 23 COLUMN 10      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    VALUE "  F2  ".  
          05 LINE 23 COLUMN 18      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    VALUE "  F3  ".  
          05 LINE 23 COLUMN 26      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    VALUE "  F4  ".  
          05 LINE 23 COLUMN 34      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    VALUE "  F5  ".  
          05 LINE 23 COLUMN 42      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    VALUE "  F6  ".  
          05 LINE 23 COLUMN 50      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    VALUE "  F7  ".  
          05 LINE 23 COLUMN 58      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    VALUE "  F8  ".  
          05 LINE 23 COLUMN 66      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    VALUE "  F9  ".  
          05 LINE 23 COLUMN 74      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    VALUE "  F10 ".  


          05 LINE 24 COLUMN 2       FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)         
                                    FROM F1.
          05 LINE 24 COLUMN 10      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F2.
          05 LINE 24 COLUMN 18      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F3.
          05 LINE 24 COLUMN 26      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F4.
          05 LINE 24 COLUMN 34      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F5.
          05 LINE 24 COLUMN 42      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F6.
          05 LINE 24 COLUMN 50      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F7.
          05 LINE 24 COLUMN 58      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F8.
          05 LINE 24 COLUMN 66      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F9.
          05 LINE 24 COLUMN 74      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F10.

          05 LINE 25 COLUMN 2       FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F1-25-L.
          05 LINE 25 COLUMN 10      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F2-25-L.
          05 LINE 25 COLUMN 18      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F3-25-L.
          05 LINE 25 COLUMN 26      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F4-25-L.
          05 LINE 25 COLUMN 34      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F5-25-L.
          05 LINE 25 COLUMN 42      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F6-25-L.
          05 LINE 25 COLUMN 50      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F7-25-L.
          05 LINE 25 COLUMN 58      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F8-25-L.
          05 LINE 25 COLUMN 66      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F9-25-L.
          05 LINE 25 COLUMN 74      FOREGROUND-COLOR 1
                                    BACKGROUND-COLOR 7
                                    PIC X(6)
                                    FROM F10-25-L.



       01 CLS.
          05 FOREGROUND-COLOR 6   BACKGROUND-COLOR 1
                                  BLANK SCREEN.


       PROCEDURE DIVISION.
       C000-MAIN-LINE SECTION.
       C020-MAIN-LINE-LOGIC.
           PERFORM DO-ACCEPT-1-COMMAND UNTIL KEY-SWITCH-MENU 
                   EQUAL KEY-SWITCH-F10.
           STOP RUN.
       C200-DISPLAY-NEW-SCREEN.
           DISPLAY CLS.
           DISPLAY FUNC-KEYS.
           DISPLAY SCREEN1-ADDRESS-ENTRY.
       MENU-1-MAIN.
           MOVE "  ADD " TO F1.
           MOVE "  ADD " TO F2.
           MOVE "      " TO F3.
           MOVE "      " TO F4.
           MOVE "      " TO F5.
           MOVE "      " TO F6.
           MOVE "      " TO F7.
           MOVE "      " TO F8.
           MOVE "      " TO F9.
           MOVE " EXIT " TO F10.
           MOVE "PATRON" TO F1-25-L.
           MOVE " BOOKS" TO F2-25-L.
           MOVE "      " TO F3-25-L.
           MOVE "      " TO F4-25-L.
           MOVE "      " TO F5-25-L.
           MOVE "      " TO F6-25-L.
           MOVE "      " TO F7-25-L.
           MOVE "      " TO F8-25-L.
           MOVE "      " TO F9-25-L.
           MOVE "      " TO F10-25-L.

       DO-ACCEPT-1-COMMAND.
           PERFORM MENU-1-MAIN.
           PERFORM C200-DISPLAY-NEW-SCREEN.
           PERFORM GET-IT.

       GET-IT.
      ***************************************************************
      *  BECAUSE OF AN ERROR IN THE COMPILER ON ESCAPE KEY FUNCTION *
      *  I HAVE WRITTEN ASSMB CODE TO REPLACE ESCAPE KEY ENTRENCE IS*
      *  CALL "INKEY" USING KEY-SWITCH                              *
      *  IF A FUNCTION KEY IS PRESSED THE CHAR OF SUCH IS RETURNED  *
      *  ELSE THE ORG VALUE IS MAINTAINED.                          *
      *  VALUES ARE  F1 = <                                         *
      *              F2 = =                                         *
      *              F3 = >                                         *
      *              F4 = ?                                         *
      *              F5 = @                                         *
      *              F6 = A                                         *
      *              F7 = B                                         *
      *              F8 = C                                         *
      *              F9 = D                                         *
      *              F10 = E                                         *
      *  OTHER VALUES ARE VALID DEPENDING ON ALT AND CTRL COMBINES   *
      ****************************************************************



           MOVE "1" TO PASSWORD-ACCESS-FLAG.
           MOVE "N" TO KEY-SWITCH-MENU.
           CALL "INKEY" USING KEY-SWITCH-MENU.
           IF  KEY-SWITCH-MENU EQUAL KEY-SWITCH-F1
               CHAIN "LIB0001A.EXE" USING PASSWORD-ACCESS-FLAG 
                                   CALLING-PRGM-NAME
           ELSE IF KEY-SWITCH-MENU EQUAL KEY-SWITCH-F2
               DISPLAY "ENTRY NOT YET VALID".
