       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PROG402.
      *
      ***************************************************
      *                                                 *
      * 1. DATE-WRITTEN. 08/01/90.                      *
      *                                                 *
      * 2. THIS PROGRAM DISPLAYS ON THE PRINTER         *
      *    A MESSAGE ENTERED BY THE OPERATOR.           *
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
       01  W005-MESSAGE-FIELD             PIC X(32).
      *
      *
       PROCEDURE DIVISION.
      *
       DISPLAY-ACCEPTED-MESSAGE.
           DISPLAY "PROGRAM PROG402:  PRINT DISPLAY OUTPUT:"
                                      UPON PRINTER-DISPLAY. 
           ACCEPT W005-MESSAGE-FIELD.
           DISPLAY W005-MESSAGE-FIELD UPON PRINTER-DISPLAY.
           STOP RUN.
