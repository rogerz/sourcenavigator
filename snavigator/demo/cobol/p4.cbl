       IDENTIFICATION DIVISION.
       PROGRAM-ID. INST.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTROL-FILE ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD CONTROL-FILE
          LABEL RECORDS STANDARD
          VALUE OF FILE-ID "CONTROL.DAT".
       01 RECORD-COUNTER                                 PIC 9(9). 
       WORKING-STORAGE SECTION.
       01 FILE-EOF-FLAG                          PIC 9(9).

       PROCEDURE DIVISION.
       C000-MAIN-LINE SECTION.
       C020-MAIN-LINE-LOGIC.
           OPEN OUTPUT CONTROL-FILE.
           MOVE 1 TO RECORD-COUNTER.
           WRITE RECORD-COUNTER.
           CLOSE CONTROL-FILE.
           STOP RUN.
