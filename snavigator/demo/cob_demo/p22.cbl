       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PROG901.
      *                                                             *
      ***************************************************************
      *                                                             *
      * 1. DATE-WRITTEN. 08/03/90.                                  *
      *                                                             *
      * 2. THIS PROGRAM ACCEPTS ENTRIES FROM THE CONSOLE DISPLAY    *
      *    AND DISPLAYS THEM ON THE PRINTER. THE FIELDS ARE:        *
      *    A) FIELD 1 - NAME                                        *
      *    B) FIELD 2 - ADDRESS                                     *
      *    C) FIELD 3 - CITY/STATE                                  *
      *                                                             *
      ***************************************************************
      *
      *
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
      *
       SPECIAL-NAMES.
      *
           PRINTER IS PRINTER-DISPLAY.
      *
      *
       DATA DIVISION.
      *
      ***************************************************************
      *                                                             *
       WORKING-STORAGE SECTION.
      *                                                             *
      ***************************************************************
      *
      * USES OF W005-KEYBOARD-KEY-SWITCH
      *    ] . F9 KEY -- END OF JOB.
      *
       01  W005-KEYBOARD-KEY-SWITCH        PIC 99.
           88  W005-F9-KEY-ACTIVATED           VALUE 10.
      *
       01  W005-RECORD-ACCEPTED-COUNT      PIC 9(4) VALUE +0.
      *        
       01  W010-EMPLOYEE-WK-RECORD.
           05  W010-EMPLOYEE-WK-NAME       PIC X(30).
           05  W010-EMPLOYEE-WK-STREET     PIC X(25).
           05  W010-EMPLOYEE-WK-CITY       PIC X(25).
      /
      ***************************************************************
      *                                                             *
       SCREEN SECTION.         
      *                                                             *
      ***************************************************************
      *
      *
       01  SCREEN1-ADDRESS-ENTRY.
           05  BLANK SCREEN.
           05  LINE 1  COLUMN 16       VALUE "E M P L O Y E E"           
                                       HIGHLIGHT.                        
           05  LINE 1  COLUMN 35       VALUE "A D D R E S S"             
                                       HIGHLIGHT.                        
           05  LINE 1  COLUMN 52       VALUE "E N T R Y"                 
                                       HIGHLIGHT.                        
           05  LINE 3  COLUMN 24       HIGHLIGHT                         
                                       VALUE "NAME:".                    
           05  SCREEN1-NAME            LINE 3  COLUMN 30                 
                                       PIC X(30)                         
                                       REVERSE-VIDEO                     
                                       USING W010-EMPLOYEE-WK-NAME.      
           05  LINE 5  COLUMN 22       HIGHLIGHT                         
                                       VALUE "STREET:".                  
           05  SCREEN1-STREET          LINE 5  COLUMN 30                 
                                       PIC X(25)                         
                                       REVERSE-VIDEO                     
                                       USING W010-EMPLOYEE-WK-STREET.    
           05  LINE 7 COLUMN 18        HIGHLIGHT                         
                                       VALUE "CITY/STATE:".              
           05  SCREEN1-CITY            LINE 7  COLUMN 30                 
                                       PIC X(25)                         
                                       REVERSE-VIDEO                     
                                       USING W010-EMPLOYEE-WK-CITY.      
      /
       PROCEDURE DIVISION.
      *
      ***************************************************************
      *                                                             *
       C000-MAIN-LINE SECTION. 
      *                                                             *
      ***************************************************************
      *
       C020-MAIN-LINE-LOGIC.
           MOVE 98 TO W005-KEYBOARD-KEY-SWITCH.
           PERFORM C120-DISPLAY-NEW-SCREEN.
           PERFORM C040-PROCESS-NAME-ENTRY
                   THRU C100-PROCESS-SCREEN-EXIT
                   UNTIL W005-F9-KEY-ACTIVATED.
           PERFORM C980-EOJ-ROUTINE.
           STOP RUN.
      *
       C040-PROCESS-NAME-ENTRY.
           ACCEPT ( 3, 30 )
                  W010-EMPLOYEE-WK-NAME.
           ACCEPT W005-KEYBOARD-KEY-SWITCH FROM ESCAPE KEY.
           IF W005-F9-KEY-ACTIVATED
                  GO TO C100-PROCESS-SCREEN-EXIT.
      *
       C060-PROCESS-STREET-ENTRY.
           ACCEPT ( 5, 30 )
                  W010-EMPLOYEE-WK-STREET.
           ACCEPT W005-KEYBOARD-KEY-SWITCH FROM ESCAPE KEY.
           IF W005-F9-KEY-ACTIVATED
                  GO TO C100-PROCESS-SCREEN-EXIT.
      *
       C080-PROCESS-CITY-ENTRY.
           ACCEPT ( 7, 30 )
                  W010-EMPLOYEE-WK-CITY.
           ACCEPT W005-KEYBOARD-KEY-SWITCH FROM ESCAPE KEY.
           IF W005-F9-KEY-ACTIVATED
                  GO TO C100-PROCESS-SCREEN-EXIT.
           EXHIBIT NAMED W010-EMPLOYEE-WK-NAME   UPON PRINTER-DISPLAY.
           EXHIBIT NAMED W010-EMPLOYEE-WK-STREET UPON PRINTER-DISPLAY.
           EXHIBIT NAMED W010-EMPLOYEE-WK-CITY   UPON PRINTER-DISPLAY.
           ADD 1 TO W005-RECORD-ACCEPTED-COUNT.
           PERFORM C120-DISPLAY-NEW-SCREEN.
       C100-PROCESS-SCREEN-EXIT.   EXIT.
      *
       C120-DISPLAY-NEW-SCREEN.
           MOVE  SPACES  TO  W010-EMPLOYEE-WK-NAME
                            W010-EMPLOYEE-WK-STREET
                            W010-EMPLOYEE-WK-CITY.
           DISPLAY SCREEN1-ADDRESS-ENTRY.
      /
       C980-EOJ-ROUTINE.
           IF W005-RECORD-ACCEPTED-COUNT  GREATER THAN ZEROS
                DISPLAY "JOB PROG901: SUCCESSFUL ENTRY COMPLETED"
                                     UPON PRINTER-DISPLAY
           ELSE DISPLAY "JOB PROG901: UNSUCCESSFUL ENTRY"
                                     UPON PRINTER-DISPLAY.
           EXHIBIT NAMED W005-RECORD-ACCEPTED-COUNT
                                     UPON PRINTER-DISPLAY.
