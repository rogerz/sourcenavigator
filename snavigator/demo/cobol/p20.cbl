      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. CHAP428.
      *
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
      *
       SPECIAL-NAMES.
           PRINTER IS PRINTER-DISPLAY.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
           01  W005-FIRST-OPERAND               PIC 9.
           01  W005-SECOND-OPERAND              PIC 9.
      *
      *
       PROCEDURE DIVISION.
      *
       MAIN-LINE-LOGIC.
           DISPLAY "THE RESULTS OF TWO IF STATEMENTS".
           MOVE 1 TO W005-FIRST-OPERAND.
           MOVE 1 TO W005-SECOND-OPERAND.
           IF W005-FIRST-OPERAND EQUAL TO W005-SECOND-OPERAND
              DISPLAY " THE VALUES ARE EQUAL "
           ELSE
              DISPLAY " THE VALUES ARE NOT EQUAL ".
           MOVE 0 TO W005-FIRST-OPERAND.
           IF W005-FIRST-OPERAND EQUAL TO W005-SECOND-OPERAND
              DISPLAY " THE VALUES ARE EQUAL "
           ELSE
              DISPLAY " THE VALUES ARE NOT EQUAL ".
           STOP RUN.
