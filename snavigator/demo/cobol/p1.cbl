       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCEPT4 .
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           PRINTER IS PRINTER-DISPLAY.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NAMES-LIST ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD NAMES-LIST
          LABEL RECORDS STANDARD
          VALUE OF FILE-ID "NAMES.LST".
       01 NAMES-RECORD                           PIC X(80).
       WORKING-STORAGE SECTION.
       01 W005-KEYBOARD-KEY-SWITCH               PIC 99.
          88 W005-F1-KEY-ACTIVATED               VALUE 02.
          88 W005-F7-KEY-ACTIVATED               VALUE 08.
       01 W005-RECORD-ACCEPTED-COUNT             PIC 9(4) VALUE +0.
       01 W010-EMPLOYEE-WK-RECORD.
          05 W010-EMPLOYEE-WK-NAME.
             10 W010-EMPLOYEE-WK-NAME-CHAR1      PIC X.
             10 FILLER                           PIC X(29).
          05 W010-EMPLOYEE-WK-STREET.
             10 W010-EMPLOYEE-WK-STREET-CHAR1    PIC X.
             10 FILLER                           PIC X(24).
          05 W010-EMPLOYEE-WK-CITY               PIC X(25).
       01 W025-SCREEN1-ALL-ERRORS.
          05 W025-SCREEN1-ERROR-FIELDS.
             10 W025-SCREEN1-ERROR1                 PIC X(6).
             10 W025-SCREEN1-ERROR2                 PIC X(6).
          05 W025-SCREEN1-ERROR-FIELDS2
             REDEFINES W025-SCREEN1-ERROR-FIELDS.
             10 W025-SCREEN1-ERROR               OCCURS 2
                                                 INDEXED BY W025-ERROR-I
                                                 PIC X(6).
       01 W025-ERROR-LITERAL                     PIC X(10).
       01 FILE-ACCESS-FLAG                       PIC 9 VALUE +0.
       SCREEN SECTION.
       01 SCREEN1-ADDRESS-ENTRY.
          05 BLANK SCREEN.
          05 LINE 1 COLUMN 16       VALUE "E M P L O Y E E"
                                    HIGHLIGHT.
          05 LINE 1 COLUMN 35       VALUE "A D D R E S S"
                                    HIGHLIGHT.
          05 LINE 1 COLUMN 52       VALUE "E N T R Y"
                                    HIGHLIGHT.
          05 LINE 3 COLUMN 24       HIGHLIGHT
                                    VALUE "NAME:".
          05 SCREEN1-NAME           LINE 3 COLUMN 30
                                    PIC X(30)
                                    REVERSE-VIDEO
                                    USING W010-EMPLOYEE-WK-NAME.
          05 LINE 5 COLUMN 22       HIGHLIGHT
                                    VALUE "STREET:".
          05 SCREEN1-STREET         LINE 5 COLUMN 30
                                    PIC X(25)
                                    REVERSE-VIDEO
                                    USING W010-EMPLOYEE-WK-STREET.
          05 LINE 7 COLUMN 18       HIGHLIGHT
                                    VALUE "CITY/STATE:".
          05 SCREEN1-CITY           LINE 7 COLUMN 30
                                    PIC X(25)
                                    REVERSE-VIDEO
                                    USING W010-EMPLOYEE-WK-CITY.
          05 SCREEN1-ERR-LIT        LINE 10 COLUMN 27
                                    PIC X(10)
                                    FROM W025-ERROR-LITERAL.
          05 SCREEN1-ERROR1         LINE 10 COLUMN 38
                                    HIGHLIGHT
                                    BLINK
                                    PIC X(6)
                                    FROM W025-SCREEN1-ERROR1.
          05 SCREEN1-ERROR2         LINE 11 COLUMN 38
                                    HIGHLIGHT
                                    BLINK
                                    PIC X(6)
                                    FROM W025-SCREEN1-ERROR2.
       PROCEDURE DIVISION.
       C000-MAIN-LINE SECTION.
       C020-MAIN-LINE-LOGIC.
           PERFORM C990-INPUT-OUTPUT-FILE-ACCESS.
           MOVE 98 TO W005-KEYBOARD-KEY-SWITCH.
           PERFORM C200-DISPLAY-NEW-SCREEN.
           PERFORM C040-PROCESS-SCREEN1-ENTRY
                                   THRU C160-PROCESS-SCREEN1-EXIT
                                   UNTIL W005-F7-KEY-ACTIVATED.
           PERFORM C980-EOJ-ROUTINE.
           CLOSE NAMES-LIST.
           STOP RUN.
       C040-PROCESS-SCREEN1-ENTRY.
           ACCEPT SCREEN1-ADDRESS-ENTRY.
           ACCEPT W005-KEYBOARD-KEY-SWITCH FROM ESCAPE KEY.
           IF W005-F1-KEY-ACTIVATED
                PERFORM C200-DISPLAY-NEW-SCREEN
                GO TO C160-PROCESS-SCREEN1-EXIT
           ELSE IF W005-F7-KEY-ACTIVATED
                GO TO C160-PROCESS-SCREEN1-EXIT.
           MOVE SPACES TO W025-SCREEN1-ERROR-FIELDS.
           SET W025-ERROR-I TO 0.
       C060-PROCESS-NAME-ENTRY.
           IF W010-EMPLOYEE-WK-NAME-CHAR1 ALPHABETIC
              AND W010-EMPLOYEE-WK-NAME-CHAR1 NOT EQUAL TO SPACES
              NEXT SENTENCE
           ELSE SET W025-ERROR-I UP BY 1
              MOVE "NAME" TO W025-SCREEN1-ERROR (W025-ERROR-I)
              IF W025-ERROR-I EQUAL TO 1
                 MOVE "===>" TO W025-ERROR-LITERAL
              ELSE IF W025-ERROR-I EQUAL TO 2
                 GO TO C120-END-OF-EDITING.
       C080-PROCESS-STREET-ENTRY.
           IF W010-EMPLOYEE-WK-STREET-CHAR1 NOT EQUAL TO SPACE
              NEXT SENTENCE
           ELSE SET W025-ERROR-I UP BY 1
              MOVE "STREET" TO W025-SCREEN1-ERROR (W025-ERROR-I)
              IF W025-ERROR-I EQUAL TO 1
                 MOVE "===>" TO W025-ERROR-LITERAL
              ELSE IF W025-ERROR-I EQUAL TO 2
                 GO TO C120-END-OF-EDITING.
       C100-PROCESS-CITY-ENTRY.
           IF W010-EMPLOYEE-WK-CITY NOT EQUAL TO SPACE
              NEXT SENTENCE
           ELSE SET W025-ERROR-I UP BY 1
              MOVE "CITY" TO W025-SCREEN1-ERROR (W025-ERROR-I)
              IF W025-ERROR-I EQUAL TO 1
                 MOVE "===>" TO W025-ERROR-LITERAL
           ELSE IF W025-ERROR-I EQUAL TO 2
              GO TO C120-END-OF-EDITING.
       C120-END-OF-EDITING.
           IF W025-ERROR-I NOT EQUAL TO ZERO
              PERFORM C220-DISPLAY-ERROR-MESSAGES
              GO TO C160-PROCESS-SCREEN1-EXIT.
       C140-VALID-SCREEN1-EXIT.
           PERFORM C990-INPUT-OUTPUT-FILE-ACCESS.
           ADD 1 TO W005-RECORD-ACCEPTED-COUNT.
           PERFORM C200-DISPLAY-NEW-SCREEN.
       C160-PROCESS-SCREEN1-EXIT.  EXIT.
       C200-DISPLAY-NEW-SCREEN.
           MOVE SPACES TO        W010-EMPLOYEE-WK-NAME
                                 W010-EMPLOYEE-WK-STREET
                                 W010-EMPLOYEE-WK-CITY
                                 W025-SCREEN1-ERROR-FIELDS
                                 W025-ERROR-LITERAL.
           DISPLAY SCREEN1-ADDRESS-ENTRY.
       C220-DISPLAY-ERROR-MESSAGES.
           DISPLAY SCREEN1-ADDRESS-ENTRY.
       C980-EOJ-ROUTINE.
           IF W005-RECORD-ACCEPTED-COUNT  GREATER THAN ZEROS
              DISPLAY "JOB ACCEPT4 : SUCCESSFUL ENTRY COMPLETED" 
           ELSE DISPLAY "JOB ACCEPT4 : UNSUCCESSFUL ENTRY ".
           EXHIBIT NAMED W005-RECORD-ACCEPTED-COUNT.
       C990-INPUT-OUTPUT-FILE-ACCESS.
           IF FILE-ACCESS-FLAG EQUAL 0
              OPEN OUTPUT NAMES-LIST
              ADD 1 TO FILE-ACCESS-FLAG
           ELSE MOVE W010-EMPLOYEE-WK-RECORD TO NAMES-RECORD
              WRITE NAMES-RECORD.
         