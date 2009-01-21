       IDENTIFICATION DIVISION.
         PROGRAM-ID.    PROBE.
         AUTHOR.        J GROSCH.
         DATE-WRITTEN.  26-05-94.
         DATE-COMPILED. 26-05-94.
      *
       ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
           SOURCE-COMPUTER. PC.
           OBJECT-COMPUTER. PC.
      *
       DATA DIVISION.
         FILE SECTION.
         WORKING-STORAGE SECTION.
       77  I PIC 99.
       01  NAMEN.
           05 VORNAME  PIC X(12).
           05 NACHNAME PIC X(12).
      *
       PROCEDURE DIVISION.
         MAIN.
           IF I EQUAL 0
              MOVE 1 TO I
           ELSE
              IF I EQUAL 2 THEN
                 ADD 2 TO I
              ELSE
                 SUBTRACT 1 FROM I
                 DISPLAY I.
       END PROGRAM x. 
       IDENTIFICATION DIVISION.
         PROGRAM-ID.    PROBE.
         AUTHOR.        J GROSCH.
         DATE-WRITTEN.  26-05-94.
         DATE-COMPILED. 26-05-94.
      *
       ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
           SOURCE-COMPUTER. PC.
           OBJECT-COMPUTER. PC.
      *
       DATA DIVISION.
         FILE SECTION.
         WORKING-STORAGE SECTION.
       77  I PIC 99.
       01  NAMEN.
           05 VORNAME  PIC X(12).
           05 NACHNAME PIC X(12).
      *
	   exec sql quatsch end-exec.
      *
       PROCEDURE DIVISION.
         MAIN.
           IF I EQUAL 0
              MOVE 1 TO I
           ELSE
              IF I EQUAL 2
                 ADD 2 TO I NOT ON SIZE ERROR EXIT END-ADD
              ELSE
                 SUBTRACT 1 FROM I
                 DISPLAY I.
           IF (X) THEN EXIT ELSE EXIT.
           IF (X OR Y) THEN EXIT ELSE EXIT.
           IF (X + Y) THEN EXIT ELSE EXIT.
           IF A IS NOT EQUAL TO B THEN EXIT ELSE EXIT.
           IF A    NOT EQUAL TO B THEN EXIT ELSE EXIT.
           IF A    NOT EQUAL    B THEN EXIT ELSE EXIT.
      *    IF   IS NOT EQUAL TO B THEN EXIT ELSE EXIT.
      *    IF      NOT EQUAL TO B THEN EXIT ELSE EXIT.
      *    IF      NOT EQUAL    B THEN EXIT ELSE EXIT.
           IF      NOT          B THEN EXIT ELSE EXIT.
           IF a > b AND NOT < c OR d THEN EXIT ELSE EXIT.
           IF a > b AND NOT < c AND d THEN EXIT ELSE EXIT.
           IF a NOT EQUAL b OR c THEN EXIT ELSE EXIT.
           IF a NOT EQUAL b AND c THEN EXIT ELSE EXIT.
           IF NOT a = b OR c THEN EXIT ELSE EXIT.
           IF NOT a = b AND c THEN EXIT ELSE EXIT.
           IF NOT (a GREATER b OR < c) THEN EXIT ELSE EXIT.
           IF NOT (a GREATER b AND < c) THEN EXIT ELSE EXIT.
           IF NOT (a GREATER b AND c) THEN EXIT ELSE EXIT.
           IF NO
      -        T (a NOT > b AND c AND NOT d) THEN EXIT ELSE EXIT.
           IF x > a OR y AND z THEN EXIT ELSE EXIT.
           IF x > a OR (y AND z) THEN EXIT ELSE EXIT.
           IF x > (a OR y) AND z THEN EXIT ELSE EXIT.
           IF x (= a OR > b) THEN EXIT ELSE EXIT.
           IF x = a AND (> b OR < z) THEN EXIT ELSE EXIT.
           IF a EQUAL b OR NOT GREATER OR EQUAL c OR d THEN EXIT.
           IF a EQUAL b OR >= c OR d THEN EXIT ELSE EXIT.

           IF a > b or c THEN EXIT ELSE EXIT.
           IF a > b or not c THEN EXIT ELSE EXIT.
           IF a > b or = c THEN EXIT ELSE EXIT.
           IF a > b or not = c THEN EXIT ELSE EXIT.
           IF a > b or d = c THEN EXIT ELSE EXIT.
           IF a > b and c THEN EXIT ELSE EXIT.
           IF a > b and not c THEN EXIT ELSE EXIT.
           IF a > b and = c THEN EXIT ELSE EXIT.
           IF a > b and not = c THEN EXIT ELSE EXIT.
           IF a > b and d = c THEN EXIT ELSE EXIT.
           IF a > b or '7' THEN EXIT ELSE EXIT.
           IF a > b or not '7' THEN EXIT ELSE EXIT.
           IF a > b or = '7' THEN EXIT ELSE EXIT.
           IF a > b or not = '7' THEN EXIT ELSE EXIT.
           IF a > b or d = '7' THEN EXIT ELSE EXIT.
           IF a > b and '7' THEN EXIT ELSE EXIT.
           IF a > b and not '7' THEN EXIT ELSE EXIT.
           IF a > b and = '7' THEN EXIT ELSE EXIT.
           IF a > b and not = '7' THEN EXIT ELSE EXIT.
           IF a > b and d = '7' THEN EXIT ELSE EXIT.
           IF 0 < '0' OR > '3' AND '6' AND '7' EXIT.
      *    IF 0 < '0' OR > '3' AND NOT = '6' AND '7' EXIT.
      *
           MOVE "abc
      -         "def" TO I.
           MOVE 123
      -          456 TO I.
           MOVE VOR
      -          NAME TO I.
           MO
      -      VE VORNAME TO I.
           inspect v tallying w for characters before initial '.'
                              x for characters after  initial '.'
                                               before initial space.
           move length of x to y.
           move x'C0' to z.
           move X'C0' to z.
           move ='C0' to z.
           exec sql quatsch end-exec.
1
12
123
1234
12345
123456
123456 
           move a to b.                                                   value ' 
           move a to b.                                                   value " 
