       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PROG501.
      *
       DATE-WRITTEN. 08/13/90.
      *
       AUTHOR. BOB NOWECK.
      *
       SECURITY. NONE.
      *
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
      *
           SELECT EMPLOYEE-ADDRESS-MASTER
                  ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT EMPLOYEE-ADDRESS-PRINT
                  ASSIGN TO PRINTER.
      *
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD  EMPLOYEE-ADDRESS-MASTER
           LABEL RECORDS STANDARD
           VALUE OF FILE-ID IS "FIOPRT.DAT".
       01  EMPLOYEE-ADDRESS-MASTER-RECORD.
           05  EMPLOYEE-ADDRESS-MASTER-NAME            PIC  X(30).
           05  EMPLOYEE-ADDRESS-MASTER-STREET          PIC  X(30).
           05  EMPLOYEE-ADDRESS-MASTER-CITY            PIC  X(30).
      *
       FD  EMPLOYEE-ADDRESS-PRINT
           LABEL RECORDS OMITTED.
       01  EMPLOYEE-ADDRESS-PRINT-LINE                PIC  X(30).
      *
      *
       WORKING-STORAGE SECTION.
      *
       01  W005-END-OF-FILE-SWITCH                    PIC X.
           88  W005-END-OF-FILE                       VALUE "1".
       01  W005-LINE-SKIP                             PIC 99.
      *
       PROCEDURE DIVISION.
      *
       C000-MAIN-LINE-LOGIC.
      *
           OPEN INPUT EMPLOYEE-ADDRESS-MASTER
                OUTPUT EMPLOYEE-ADDRESS-PRINT.
           MOVE "0" TO W005-END-OF-FILE-SWITCH.
           PERFORM C060-READ-EMPLOYEE-MASTER.
           PERFORM C020-PROCESS-EMPLOYEE-MASTER
                UNTIL W005-END-OF-FILE.
           CLOSE EMPLOYEE-ADDRESS-MASTER
                 EMPLOYEE-ADDRESS-PRINT.
           STOP RUN.
      *
       C020-PROCESS-EMPLOYEE-MASTER.
      *
           MOVE EMPLOYEE-ADDRESS-MASTER-NAME
                         TO EMPLOYEE-ADDRESS-PRINT-LINE.
           MOVE 3        TO W005-LINE-SKIP.
           PERFORM C040-WRITE-DETAIL-LINE.
           MOVE EMPLOYEE-ADDRESS-MASTER-STREET
                         TO EMPLOYEE-ADDRESS-PRINT-LINE.
           MOVE 3        TO W005-LINE-SKIP.
           PERFORM C040-WRITE-DETAIL-LINE.
           MOVE EMPLOYEE-ADDRESS-MASTER-CITY
                         TO EMPLOYEE-ADDRESS-PRINT-LINE.
           MOVE 3        TO W005-LINE-SKIP.
           PERFORM C040-WRITE-DETAIL-LINE.
           PERFORM C060-READ-EMPLOYEE-MASTER.
      *
       C040-WRITE-DETAIL-LINE.
      *
           WRITE EMPLOYEE-ADDRESS-PRINT-LINE
                         AFTER ADVANCING W005-LINE-SKIP LINES.
      *
       C060-READ-EMPLOYEE-MASTER.
      *
           READ EMPLOYEE-ADDRESS-MASTER
                AT END, MOVE "1" TO W005-END-OF-FILE-SWITCH.
