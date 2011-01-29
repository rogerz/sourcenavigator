       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PROG408.
      *
      ***************************************************
      *                                                 *
      * 1. DATE-WRITTEN. 08/01/90.                      *
      *                                                 *
      * 2. THIS PROGRAM DISPLAYS ON THE PRINTER         *
      *    A VALUE COMPUTED FROM A SUBTRACT OPERATION.  *
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
       01  W005-SUBTRAHEND                 PIC 9   VALUE 2.  
      *
       01  W005-MINUEND                    PIC 99  VALUE 12.
      *
      *
       PROCEDURE DIVISION.
      *
       PRINT-COMPUTED-VALUE.
           SUBTRACT W005-SUBTRAHEND FROM  W005-MINUEND.
           DISPLAY "PROGRAM PROG408: THE RESULT OF 12 - 2 IS:"
                                       UPON PRINTER-DISPLAY.
           DISPLAY W005-MINUEND UPON PRINTER-DISPLAY.
           STOP RUN.
