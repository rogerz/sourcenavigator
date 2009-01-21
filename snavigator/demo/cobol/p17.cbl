       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. CHAP422.
      *
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
      * 
       SPECIAL-NAMES.
      *
           PRINTER IS PRINTER-DISPLAY.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
           01  W005-DIVISOR                     PIC 9  VALUE 0.
      *
           01  W005-DIVIDEND                    PIC 99 VALUE 5.
      *
           01  W005-QUOTIENT                    PIC 9.
      *
      * 
       PROCEDURE DIVISION.
      *
       MAIN-LINE-LOGIC.
           PERFORM PRINT-COMPUTED-VALUE.
           STOP RUN.
      *
      *
       PRINT-COMPUTED-VALUE.
           DIVIDE W005-DIVISOR INTO W005-DIVIDEND GIVING W005-QUOTIENT
                       ON SIZE ERROR
                          DISPLAY "ON SIZE ERROR ENCOUNTERED"
                          DISPLAY "PROGRAM ABORTING"
                          STOP RUN.
           DISPLAY "No On Size error".
           DISPLAY "PROGRAM EXITING NORMALLY".
           STOP RUN.
