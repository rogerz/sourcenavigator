       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PROG412.
      *
      ***************************************************
      *                                                 *
      * 1. DATE-WRITTEN. 08/01/90.                      *
      *                                                 *
      * 2. THIS PROGRAM DISPLAYS ON THE PRINTER         *
      *    A VALUE COMPUTED FROM A DIVIDE   OPERATION.  *
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
      *
       PROCEDURE DIVISION.
      *
       PRINT-COMPUTED-VALUE.
           DIVIDE W005-DIVISOR INTO W005-DIVIDEND.
           DISPLAY "PROGRAM PROG412: THE QUOTIENT OF 12 / 2 IS:"
                                       UPON PRINTER-DISPLAY.
           DISPLAY W005-DIVIDEND       UPON PRINTER-DISPLAY.
           STOP RUN.
