       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PROG406.
      *
      ***************************************************
      *                                                 *
      * 1. DATE-WRITTEN. 08/01/90.                      *
      *                                                 *
      * 2. THIS PROGRAM DISPLAYS ON THE PRINTER         *
      *    A VALUE COMPUTED FROM AN ADD OPERATION.      *
      *                                                 *
      ***************************************************
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
      *
      ****************************************************
      *                                                  *
       WORKING-STORAGE SECTION.
      *                                                  *
      ****************************************************
      *
      *
       01  W005-FIRST-OPERAND              PIC 9   VALUE 2.  
      *
       01  W005-SECOND-OPERAND             PIC 99  VALUE 12.
      *
      *
       PROCEDURE DIVISION.
      *
       PRINT-COMPUTED-VALUE.
           ADD W005-FIRST-OPERAND TO W005-SECOND-OPERAND.
           DISPLAY "PROGRAM PROG406: THE SUM OF 2 + 12 IS:"
                                       UPON PRINTER-DISPLAY.
           DISPLAY W005-SECOND-OPERAND UPON PRINTER-DISPLAY.
           STOP RUN.
