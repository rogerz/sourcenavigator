       IDENTIFICATION DIVISION.
         PROGRAM-ID.    PROBE.
         AUTHOR.        J. GROSCH.
         DATE-WRITTEN.  26.05.94.
         DATE-COMPILED. 26.05.94.
      *
       ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
           SOURCE-COMPUTER. PC.
           OBJECT-COMPUTER. PC.
         INPUT-OUTPUT SECTION.
           FILE-CONTROL.
             SELECT D224000 ASSIGN TO EXTERNAL DD224000
                  ORGANIZATION IS SEQUENTIAL
        	  RESERVE  1  ALTERNATE AREA.
	     COPY x.
	     REPLACE OFF.
	     EXEC SQL END-EXEC.
      *
       DATA DIVISION.
         FILE SECTION.
	   COPY x.
	   REPLACE OFF.
	   EXEC SQL END-EXEC.
       FD D224000.
	 01 a.
	   20 aaaaaaaaaaaaaaaaaaaaaa
      - 	 aa OCCURS 33 INDEXED BY J.
	 01 b usage is pointer.
	 01 c value is 4711.
	   COPY x.
	   REPLACE OFF.
	   EXEC SQL END-EXEC.
         WORKING-STORAGE SECTION.
       01  DECKNAME.
	   88 C1 VALUE is 14.
           05 VORNAME  PIC X(12).
           05 NACHNAME PIC X(12).
	   05 KUNST-NAME redefines nachname.
	     88 C2 VALUE is 13.
       01  ALIAS   REDEFINES DECKNAME.
       01  ALIAS-2 REDEFINES DECKNAME.
       66  R1 renames deckname.
       77  I PIC 99.
       78  k value is 19.
	   COPY x.
	   REPLACE OFF.
	   EXEC SQL END-EXEC.
	 LOCAL-STORAGE SECTION.
	   COPY x.
	   REPLACE OFF.
	   EXEC SQL END-EXEC.
	 LINKAGE SECTION.
	   COPY x.
	   REPLACE OFF.
	   EXEC SQL END-EXEC.
	 COMMUNICATION SECTION.
	   COPY x.
	   REPLACE OFF.
	   EXEC SQL END-EXEC.
           CD n For         inPUT .
           CD n For         OUTPUT .
           CD n For         i-o .
	 REPORT SECTION.
	   COPY x.
	   REPLACE OFF.
	   EXEC SQL END-EXEC.
	 SCREEN SECTION.
	   COPY x.
	   REPLACE OFF.
	   EXEC SQL END-EXEC.
      *
       PROCEDURE DIVISION.
	  COPY x.
	  REPLACE OFF.
	  EXEC SQL END-EXEC.
       P.
	  COPY x.
	  REPLACE OFF.
	  EXEC SQL END-EXEC.
       S SECTION.
	  COPY x.
	  REPLACE OFF.
	  EXEC SQL END-EXEC.
	  IF c1 then exit.
	  IF c1 in deckname then exit.
	  IF c2 then exit.
	  IF c2 in KUNST-NAME then exit.
	  IF c2 in deckname then exit.
	  IF c2 in KUNST-NAME of deckname then exit.
	  move k to r1.
	  COPY x.
	  REPLACE OFF.
	  EXEC SQL END-EXEC.
