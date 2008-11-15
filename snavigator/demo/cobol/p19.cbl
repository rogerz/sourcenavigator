      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. CHAP426.
      *
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
      *
       SPECIAL-NAMES.
           PRINTER IS PRINTER-DISPLAY.
      *
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
      *
           01  W005-GROSS-PAY                   PIC 9(4)V99.
      *
           01  W005-BASE-PAY                    PIC 999V99 VALUE 500.
      *
           01  W005-WEEK-HOURS                  PIC 99V99  VALUE 39.5.
      *
      *
       PROCEDURE DIVISION.
      *
       MAIN-LINE-LOGIC.
           GO TO PRINT-COMPUTED-VALUE.
      *
      *
       PRINT-COMPUTED-VALUE.
           COMPUTE W005-GROSS-PAY ROUNDED =
             W005-BASE-PAY * ( 1 + 1.5 * ( W005-WEEK-HOURS - 35 ) / 35 ).
           DISPLAY W005-GROSS-PAY.
           COMPUTE W005-GROSS-PAY ROUNDED =
             W005-BASE-PAY * ( 1 + ( 1.5 / 35 ) 
                           * ( W005-WEEK-HOURS - 35)).
           DISPLAY W005-GROSS-PAY.
           STOP RUN.
