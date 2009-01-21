       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PROG404.
      *
      ***************************************************
      *                                                 *
      * 1. DATE-WRITTEN. 08/01/90.                      *
      *                                                 *
      * 2. THIS PROGRAM DISPLAYS ON THE PRINTER         *
      *    A MESSAGE ENTERED BY THE OPERATOR.           *
      *                                                 *
      * 3. THE MESSAGE IS INITIALLY ENTERED INTO ONE    *
      *    WHICH IS THEN MOVED TO ANOTHER FIELD BEFORE  *
      *    BEING PRINTED ON THE PRINTER.                *
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
       01  W005-ACCEPT-MESSAGE-FIELD             PIC X(32).
      *
       01  W005-DISPLAY-MESSAGE-FIELD            PIC X(32).
      *
      *
       PROCEDURE DIVISION.
      *
       DISPLAY-ACCEPTED-MESSAGE.
           DISPLAY "PROGRAM PROG404:  PRINT DISPLAY OUTPUT:"
                                      UPON PRINTER-DISPLAY. 
           ACCEPT W005-ACCEPT-MESSAGE-FIELD.
           MOVE   W005-ACCEPT-MESSAGE-FIELD
                              TO W005-DISPLAY-MESSAGE-FIELD.
           DISPLAY W005-DISPLAY-MESSAGE-FIELD 
                                       UPON PRINTER-DISPLAY.
           STOP RUN.
