       IDENTIFICATION DIVISION.
       PROGRAM-ID. asmtest.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WORK-AREAS.
          05 LPARM1              PIC 9 COMP-0 VALUE 6.
          05 LPARM2              PIC 99 COMP-0 VALUE 20.
          05 LPARM3              PIC 99 COMP-0 VALUE 40.
          05 LPARM4 PIC 9 COMP-0 VALUE 8.
          05 LPARM5 PIC 99 COMP-0 VALUE 79.
          05 LPARM6 PIC X VALUE 'S'.
          05 LPARM7 PIC X VALUE 'R'.
      * SOUND ON IS ANYTHING NOT 0
          05 LPARM8 PIC 9 COMP-0 VALUE 0.
          05 LPARM9 PIC 9 COMP-0 VALUE 1.
       01 TESTKEY PIC X.
       SCREEN SECTION.
       01 BLNK-SCREEN.
          03 BLANK SCREEN.
       01 RED-SCREEN.
          03 BLANK SCREEN
             BACKGROUND-COLOR 1 FOREGROUND-COLOR 3.
       PROCEDURE DIVISION.
        01-START.
            DISPLAY BLNK-SCREEN.
            display 'this is a test to show how'.
            display 'you may use windows'.
            call 'SAVRST' using lparm6.
            DISPLAY BLNK-SCREEN.
            DISPLAY "SOUND IS OFF".
            CALL 'WINDOW' USING LPARM1 LPARM2 LPARM3 LPARM4 LPARM5 
                                LPARM8 . 
      	    ACCEPT TESTKEY.
            DISPLAY BLNK-SCREEN.
            DISPLAY "SOUND IS ON".
            CALL 'WINDOW' USING LPARM1 LPARM2 LPARM3 LPARM4 LPARM5 
                                LPARM9 . 
      	    ACCEPT TESTKEY.
            call 'SAVRST' using lparm7.
            ACCEPT TESTKEY.
            DISPLAY BLNK-SCREEN.
            STOP RUN.
