       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PROG410.
      *
      ***************************************************
      *                                                 *
      * 1. DATE-WRITTEN. 08/01/90.                      *
      *                                                 *
      * 2. THIS PROGRAM DISPLAYS ON THE PRINTER         *
      *    A VALUE COMPUTED FROM A MULTIPLY OPERATION.  *
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
       01  W005-MULTIPLICAND               PIC 9   VALUE 2.  
      *
       01  W005-MULTIPLIER                 PIC 99  VALUE 12.
      *
      *
       PROCEDURE DIVISION.
      *
       PRINT-COMPUTED-VALUE.
           MULTIPLY W005-MULTIPLICAND BY W005-MULTIPLIER.
           DISPLAY "PROGRAM PROG410: THE PRODUCT OF 2 * 12 IS:"
                                       UPON PRINTER-DISPLAY.
           DISPLAY W005-MULTIPLIER UPON PRINTER-DISPLAY.
           STOP RUN.
