       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. PROG401.
      *
      ***************************************************
      *                                                 *
      * 1. DATE-WRITTEN. 08/01/90.                      *
      *                                                 *
      * 2. THIS PROGRAM DISPLAYS ON THE CONSOLE DISPLAY *
      *    A MESSAGE ENTERED BY THE OPERATOR.           *
      *                                                 *
      ***************************************************
      *
      *
       ENVIROMENT DIVISION.
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
          ACCEPT W005-MESSAGE-FIELD.
          DISPLAY W005-MESSAGE-FIELD.
          STOP RUN.
