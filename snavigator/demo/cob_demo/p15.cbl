       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PROG416.
      *
      *********************************************************
      *                                                       *
      * 1. DATE-WRITTEN. 08/01/90.                            *
      *                                                       *
      * 2. THIS PROGRAM DISPLAYS ON THE PRINTER               *
      *    A VALUE COMPUTED FROM A DIVIDE   OPERATION.        *
      *                                                       *
      * 3. THE QUOTIENT IS PLACED INTO A DIFFERENT FIELD      *
      *                                                       *
      * 4. THE DIVIDE OPERATION AND DISPLAY WILL BE DONE TWICE*
      *    A) TRY #1 - QUOTIENT IS NOT ROUNDED OFF.           *
      *    B) TRY #2 - QUOTIENT IS ROUNDED OFF.               *
      *                                                       *
      *********************************************************
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
       01  W005-DIVISOR                    PIC 9   VALUE 3.  
      *
       01  W005-DIVIDEND                   PIC 9   VALUE 5.
      *
       01  W005-QUOTIENT                   PIC 9.
      *
      *
       PROCEDURE DIVISION.
      *
       PRINT-COMPUTED-VALUE.
           DIVIDE W005-DIVISOR INTO W005-DIVIDEND GIVING W005-QUOTIENT.
           DISPLAY "PROGRAM PROG414: THE QUOTIENT OF 5 / 3 UNROUNDED IS
      -        ":"                   UPON PRINTER-DISPLAY.
           DISPLAY W005-QUOTIENT     UPON PRINTER-DISPLAY.
           DIVIDE W005-DIVISOR INTO W005-DIVIDEND 
                                    GIVING W005-QUOTIENT ROUNDED.
           DISPLAY "PROGRAM PROG414: THE QUOTIENT OF 5 / 3 ROUNDED IS:"
                                     UPON PRINTER-DISPLAY.
           DISPLAY W005-QUOTIENT     UPON PRINTER-DISPLAY.
           STOP RUN.
