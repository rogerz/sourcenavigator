       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PROG414.
      *
      ***************************************************
      *                                                 *
      * 1. DATE-WRITTEN. 08/01/90.                      *
      *                                                 *
      * 2. THIS PROGRAM DISPLAYS ON THE PRINTER         *
      *    A VALUE COMPUTED FROM A DIVIDE   OPERATION.  *
      *                                                 *
      * 3. THE QUOTIENT IS PLACED INTO A DIFFERENT FIELD*
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
       01  W005-DIVISOR                    PIC 9   VALUE 2.  
      *
       01  W005-DIVIDEND                   PIC 99  VALUE 12.
      *
       01  W005-QUOTIENT                   PIC 9.
      *
       PROCEDURE DIVISION.
      *
       PRINT-COMPUTED-VALUE.
           DIVIDE W005-DIVISOR INTO W005-DIVIDEND GIVING W005-QUOTIENT.
           DISPLAY "PROGRAM PROG412: THE QUOTIENT OF 12 / 2 IS:"
                                       UPON PRINTER-DISPLAY.
           DISPLAY W005-QUOTIENT       UPON PRINTER-DISPLAY.
           STOP RUN.
