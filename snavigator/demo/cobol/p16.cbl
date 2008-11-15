       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PROG420.
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
      *
       WORKING-STORAGE SECTION.
      *
           01  W005-DIVISOR                     PIC 9  VALUE 2.
      *
           01  W005-DIVIDEND                    PIC 99 VALUE 12.
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
           DIVIDE W005-DIVISOR INTO W005-DIVIDEND GIVING W005-QUOTIENT.
           DISPLAY "12/2 IS".
           DISPLAY W005-QUOTIENT.
