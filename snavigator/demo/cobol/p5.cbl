       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIB001A .
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           PRINTER IS PRINTER-DISPLAY.



       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NAMES-LIST ASSIGN TO DISK
           ACCESS MODE RANDOM
           ORGANIZATION INDEXED
           RECORD KEY PATRON-NUMBER.
           SELECT CONTROL-FILE ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.

       copy teste.


       WORKING-STORAGE SECTION.
      *************************************************************
      *                                                           *
      *  KEY-SWITCH IS USED FOR THE RETURN VALUE OF INKEY         *
      *                                                           *
      *  KEY-SWITCH-? IS THE VALUE OF FUNCTION KEYS               *
      *                                                           *
      *************************************************************

       01 KEY-SWITCH                              PIC X.
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


       01 WORK-NAMES-RECORD.
          05 WORK-PATRON-NUMBER                       PIC X(5).
          05 WORK-PATRON-NAME.
             10 WORK-PATRON-NAME-FIRST                PIC X(15).
             10 WORK-PATRON-NAME-MIDDLE               PIC X.
             10 WORK-PATRON-NAME-LAST                 PIC X(20).
          05 WORK-PATRON-ADDRESS.
             10 WORK-PATRON-ADDRESS-ADD               PIC X(30).
             10 WORK-PATRON-ADDRESS-CITY              PIC X(9).
             10 WORK-PATRON-ADDRESS-STATE             PIC X(2).
             10 WORK-PATRON-ADDRESS-ZIP               PIC X(5).
          05 WORK-PATRON-NEW-CARD-DATE.
             10 WORK-PATRON-NEW-CARD-DATE-YY          PIC 99.
             10 WORK-PATRON-NEW-CARD-DATE-MM          PIC 99.
             10 WORK-PATRON-NEW-CARD-DATE-DD          PIC 99.
          05 WORK-PATRON-EXP-CARD-DATE.
             10 WORK-PATRON-EXP-CARD-DATE-YY          PIC 99.
             10 WORK-PATRON-EXP-CARD-DATE-MM          PIC 99.
             10 WORK-PATRON-EXP-CARD-DATE-DD          PIC 99.
          05 WORK-PATRON-PHONE-NUMBER.
             10 WORK-PATRON-PHONE-NUMBER-PRE          PIC X(3).
             10 WORK-PATRON-PHONE-NUMBER-SUF          PIC X(4).
          05 WORK-PATRON-LATE-BOOKS-FLAG              PIC X.
          05 WORK-PATRON-LATE-MOVIE-FLAG              PIC X.
       01 FILE-ACCESS-FLAG                       PIC 9 VALUE +0.
       01 PASSWORD-ACCESS-FLAG                   PIC X.
       01 CALLING-PRGM-NAME                      PIC X(12)
                                                 VALUE "LIB0001A.EXE".
       01 CALLED-FROM-PRGM-NAME                  PIC X(12).
       01 FILE-EOF-FLAG                          PIC 9(9).
       01 WORKING-CONTROL                        PIC 9(9).
       SCREEN SECTION.

       01 SCREEN1-ADDRESS-ENTRY.
          05 LINE 1 COLUMN 16       VALUE "P A T R O N S"
                                    HIGHLIGHT.
          05 LINE 1 COLUMN 33       VALUE "I N F O R M A T I O N"
                                    HIGHLIGHT.
          05 LINE 1 COLUMN 57       VALUE "E N T R Y"
                                    HIGHLIGHT.
          05 LINE 2 COLUMN 1        HIGHLIGHT
             VALUE "****************************************".
          05 LINE 2 COLUMN 41       HIGHLIGHT
             VALUE "****************************************".
          05 LINE 4 COLUMN 2        HIGHLIGHT
             VALUE "PATRON CARD NUMBER".
          05 LINE 4 COLUMN 30       HIGHLIGHT
             VALUE "FIRST NAME".
          05 LINE 4 COLUMN 47       HIGHLIGHT
             VALUE "MIDDLE".
          05 LINE 4 COLUMN 63       HIGHLIGHT
             VALUE "LAST NAME".

          05 CARD-NUMBER            LINE 5 COLUMN 8
                                    PIC X(5)
                                    FOREGROUND-COLOR 7 
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    USING WORK-PATRON-NUMBER.
          05 FIRST-NAME             LINE 5 COLUMN 27
                                    PIC X(15)
                                    FOREGROUND-COLOR 7 
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    USING WORK-PATRON-NAME-FIRST.
          05 MIDDLE-NAME            LINE 5 COLUMN 50
                                    PIC X
                                    FOREGROUND-COLOR 7 
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    USING WORK-PATRON-NAME-MIDDLE.
          05 LAST-NAME              LINE 5 COLUMN 57
                                    PIC X(20)
                                    FOREGROUND-COLOR 7 
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    USING WORK-PATRON-NAME-LAST.
          05 LINE 7 COLUMN 2        HIGHLIGHT
                                    VALUE "CARD ISSUED".
          05 LINE 7 COLUMN 14       HIGHLIGHT
                                    VALUE "CARD EXPIRES". 
          05 LINE 8 COLUMN 4        FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC 99
                                    FROM WORK-PATRON-NEW-CARD-DATE-MM.
          05 LINE 8 COLUMN 6        FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    VALUE "/".
          05 LINE 8 COLUMN 7        FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC 99
                                    FROM WORK-PATRON-NEW-CARD-DATE-DD.
          05 LINE 8 COLUMN 9        FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    VALUE "/".
          05 LINE 8 COLUMN 10       FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC 99
                                    FROM WORK-PATRON-NEW-CARD-DATE-YY.

          05 LINE 8 COLUMN 16       FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC 99
                                    FROM WORK-PATRON-EXP-CARD-DATE-MM.
          05 LINE 8 COLUMN 18        FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    VALUE "/".
          05 LINE 8 COLUMN 19        FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC 99
                                    FROM WORK-PATRON-EXP-CARD-DATE-DD.
          05 LINE 8 COLUMN 21        FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    VALUE "/".
          05 LINE 8 COLUMN 22        FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC 99
                                    FROM WORK-PATRON-EXP-CARD-DATE-YY.
          05 LINE 10 COLUMN 11       HIGHLIGHT
                                    VALUE "STREET ADDRESS".
          05 STREET-ADDRESS         LINE 11 COLUMN 2
                                    FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC X(30)
                                    USING WORK-PATRON-ADDRESS-ADD.
          05 LINE 10 COLUMN 40      HIGHLIGHT
                                    VALUE "CITY".
          05 CITY                   LINE 11 COLUMN 38
                                    FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC X(9)
                                    USING WORK-PATRON-ADDRESS-CITY.
          05 LINE 10 COLUMN 52      HIGHLIGHT
                                    VALUE "STATE".
          05 STATE                  LINE 11 COLUMN 54
                                    FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC X(2)
                                    USING WORK-PATRON-ADDRESS-STATE.
          05 LINE 10 COLUMN 61      HIGHLIGHT
                                    VALUE "ZIPCODE".
          05 ZIP                    LINE 11 COLUMN 62
                                    FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC X(5)
                                    USING WORK-PATRON-ADDRESS-ZIP.
          05 LINE 13 COLUMN 4       HIGHLIGHT
                                    VALUE "PHONE NUMBER".
          05 PREFIX-NUMBER          LINE 14 COLUMN 5
                                    FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC X(3)                                   
                                    USING WORK-PATRON-PHONE-NUMBER-PRE.
          05 LINE 14 COLUMN 8       HIGHLIGHT
                                    VALUE "-".
          05 SUFFIX-NUMBER          LINE 14 COLUMN 9
                                    FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC X(4)
                                    USING WORK-PATRON-PHONE-NUMBER-SUF.
          05 LINE 17 COLUMN 5       HIGHLIGHT
                                    VALUE "LATE BOOKS".
          05 LATE-BOOKS-FLAG        LINE 18 COLUMN 10
                                    FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC X
                                    USING WORK-PATRON-LATE-BOOKS-FLAG.
          05 LINE 17 COLUMN 20      HIGHLIGHT
                                    VALUE "LATE MOVIES".
          05 LATE-MOVIES-FLAG       LINE 18 COLUMN 26
                                    FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC X
                                    USING WORK-PATRON-LATE-MOVIE-FLAG.
 


       01 SCREEN2-ADDRESS-ENTRY.
          05 LINE 1 COLUMN 16       VALUE "P A T R O N S"
                                    HIGHLIGHT.
          05 LINE 1 COLUMN 33       VALUE "I N F O R M A T I O N"
                                    HIGHLIGHT.
          05 LINE 1 COLUMN 57       VALUE "E N T R Y"
                                    HIGHLIGHT.
          05 LINE 2 COLUMN 1        HIGHLIGHT
             VALUE "****************************************".
          05 LINE 2 COLUMN 41       HIGHLIGHT
             VALUE "****************************************".
          05 LINE 4 COLUMN 2        HIGHLIGHT
             VALUE "PATRON CARD NUMBER".
          05 LINE 4 COLUMN 30       HIGHLIGHT
             VALUE "FIRST NAME".
          05 LINE 4 COLUMN 47       HIGHLIGHT
             VALUE "MIDDLE".
          05 LINE 4 COLUMN 63       HIGHLIGHT
             VALUE "LAST NAME".

          05 CARD-NUMBER            LINE 5 COLUMN 8
                                    PIC X(5)
                                    FOREGROUND-COLOR 7 
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    FROM WORK-PATRON-NUMBER.
          05 FIRST-NAME             LINE 5 COLUMN 27
                                    PIC X(15)
                                    FOREGROUND-COLOR 7 
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    FROM  WORK-PATRON-NAME-FIRST.
          05 MIDDLE-NAME            LINE 5 COLUMN 50
                                    PIC X
                                    FOREGROUND-COLOR 7 
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    FROM  WORK-PATRON-NAME-MIDDLE.
          05 LAST-NAME              LINE 5 COLUMN 57
                                    PIC X(20)
                                    FOREGROUND-COLOR 7 
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    FROM  WORK-PATRON-NAME-LAST.
          05 LINE 7 COLUMN 2        HIGHLIGHT
                                    VALUE "CARD ISSUED".
          05 LINE 7 COLUMN 14       HIGHLIGHT
                                    VALUE "CARD EXPIRES". 
          05 LINE 8 COLUMN 4        FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC 99
                                    USING  WORK-PATRON-NEW-CARD-DATE-MM.
          05 LINE 8 COLUMN 6        FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    VALUE "/".
          05 LINE 8 COLUMN 7        FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC 99
                                    USING WORK-PATRON-NEW-CARD-DATE-DD.
          05 LINE 8 COLUMN 9        FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    VALUE "/".
          05 LINE 8 COLUMN 10       FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC 99
                                    USING WORK-PATRON-NEW-CARD-DATE-YY.

          05 LINE 8 COLUMN 16       FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC 99
                                    FROM WORK-PATRON-EXP-CARD-DATE-MM.
          05 LINE 8 COLUMN 18        FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    VALUE "/".
          05 LINE 8 COLUMN 19        FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC 99
                                    FROM WORK-PATRON-EXP-CARD-DATE-DD.
          05 LINE 8 COLUMN 21        FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    VALUE "/".
          05 LINE 8 COLUMN 22        FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 6
                                    HIGHLIGHT
                                    PIC 99
                                    FROM WORK-PATRON-EXP-CARD-DATE-YY.
          

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


       PROCEDURE DIVISION CHAINING PASSWORD-ACCESS-FLAG 
                                   CALLED-FROM-PRGM-NAME.
       C000-MAIN-LINE SECTION.
       C020-MAIN-LINE-LOGIC.
           OPEN INPUT CONTROL-FILE.
           READ CONTROL-FILE.
           MOVE RECORD-COUNTER TO WORKING-CONTROL.
           CLOSE CONTROL-FILE.
           PERFORM CALC-DATE.
           PERFORM C200-DISPLAY-NEW-SCREEN.
           OPEN OUTPUT NAMES-LIST.
           MOVE WORK-NAMES-RECORD TO NAMES-RECORD.
           CLOSE NAMES-LIST.
           OPEN I-O NAMES-LIST.
           MOVE 1 TO FILE-EOF-FLAG.
           PERFORM READ-EOF UNTIL WORKING-CONTROL EQUAL FILE-EOF-FLAG.
           PERFORM CALC-DATE.
           PERFORM DO-ACCEPT-1-COMMAND UNTIL KEY-SWITCH 
                   EQUAL KEY-SWITCH-F10.
           CLOSE NAMES-LIST.
           CHAIN CALLED-FROM-PRGM-NAME.
       C200-DISPLAY-NEW-SCREEN.
     
           DISPLAY CLS.
           DISPLAY FUNC-KEYS.
           MOVE SPACES TO        WORK-PATRON-NUMBER
                                 WORK-PATRON-NAME-FIRST
                                 WORK-PATRON-NAME-MIDDLE
                                 WORK-PATRON-NAME-LAST
                                 WORK-PATRON-ADDRESS-ADD. 
           MOVE "JESUP" TO       WORK-PATRON-ADDRESS-CITY.
           MOVE "GA" TO          WORK-PATRON-ADDRESS-STATE.
           MOVE 31545 TO         WORK-PATRON-ADDRESS-ZIP.
           MOVE "427" TO         WORK-PATRON-PHONE-NUMBER-PRE.
           MOVE SPACES TO        WORK-PATRON-PHONE-NUMBER-SUF.
           MOVE "N" TO           WORK-PATRON-LATE-BOOKS-FLAG
                                 WORK-PATRON-LATE-MOVIE-FLAG.        
           DISPLAY SCREEN1-ADDRESS-ENTRY.
       MENU-1-MAIN.
           MOVE " DATE " TO F1.
           MOVE "      " TO F2.
           MOVE "      " TO F3.
           MOVE "      " TO F4.
           MOVE "  ADD " TO F5.
           MOVE "      " TO F6.
           MOVE "      " TO F7.
           MOVE "      " TO F8.
           MOVE "      " TO F9.
           MOVE " EXIT " TO F10.
           MOVE "CHANGE" TO F1-25-L.
           MOVE "      " TO F2-25-L.
           MOVE "      " TO F3-25-L.
           MOVE "      " TO F4-25-L.
           MOVE "RECORD" TO F5-25-L.
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



           OPEN I-O CONTROL-FILE. 
           MOVE "N" TO KEY-SWITCH.
           CALL "INKEY" USING KEY-SWITCH.
           IF  KEY-SWITCH EQUAL KEY-SWITCH-F1
               DISPLAY SCREEN2-ADDRESS-ENTRY
               ACCEPT SCREEN2-ADDRESS-ENTRY
               PERFORM DO-UPDATE-DATE
               DISPLAY SCREEN1-ADDRESS-ENTRY
               ACCEPT SCREEN1-ADDRESS-ENTRY 
               MOVE WORK-NAMES-RECORD TO NAMES-RECORD
               WRITE NAMES-RECORD
               READ CONTROL-FILE
               ADD 1 TO RECORD-COUNTER
               REWRITE RECORD-COUNTER
               CLOSE CONTROL-FILE
           ELSE IF KEY-SWITCH EQUAL KEY-SWITCH-F5
               DISPLAY SCREEN1-ADDRESS-ENTRY
               ACCEPT SCREEN1-ADDRESS-ENTRY
               MOVE WORK-NAMES-RECORD TO NAMES-RECORD
               WRITE NAMES-RECORD
               READ CONTROL-FILE
               ADD 1 TO RECORD-COUNTER
               REWRITE RECORD-COUNTER
               CLOSE CONTROL-FILE.

       CALC-DATE.
           ACCEPT WORK-PATRON-NEW-CARD-DATE FROM DATE.
           PERFORM DO-UPDATE-DATE.
       DO-UPDATE-DATE.
           MOVE WORK-PATRON-NEW-CARD-DATE TO WORK-PATRON-EXP-CARD-DATE.
           ADD 1 TO WORK-PATRON-EXP-CARD-DATE-YY.
       READ-EOF.
           READ NAMES-LIST.
           ADD 1 TO FILE-EOF-FLAG.

