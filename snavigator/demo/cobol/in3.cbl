       IDENTIFICATION DIVISION.
         PROGRAM-ID.    'PROBE'.
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
             SELECT D224001 ASSIGN TO EXTERNAL DD224001.
             SELECT D224000 ASSIGN TO EXTERNAL DD224000
                  ORGANIZATION IS SEQUENTIAL
		  RECORD KEY i
		  RECORD KEY b
		  RECORD KEY c of a
      * 	  RECORD KEY k
      * 	  RECORD KEY k of a
        	  RESERVE  1  ALTERNATE AREA.
      *
       DATA DIVISION.
         FILE SECTION.
       FD D224000.
         WORKING-STORAGE SECTION.
       77  I.
       88  v value is 0815.
       01  DECKNAME.
           05 VORNAME PIC AAAAA VALUE IS 'Josef'.
           05 NACHNAME.
       88  v value is 0815.
       01  a.
           05 b.
	     10 c.
	     88 w value is 4711.
	     10 d.
           05 e.
	     10 f.
	     10 g.
      *
       PROCEDURE DIVISION.
	 move 0 to i.
	 move 0 to a.
	 move 0 to b.
	 move 0 to b of a.
	 move 0 to c.
	 move 0 to c of b.
	 move 0 to c of b of a.
	 move 0 to c of a.
	 move 0 to d.
	 move 0 to d of b.
	 move 0 to d of b of a.
	 move 0 to d of a.
	 move 0 to e.
	 move 0 to e of a.
	 move 0 to f.
	 move 0 to f of e.
	 move 0 to f of e of a.
	 move 0 to g.
	 move 0 to g of e.
	 move 0 to g of e of a.
	 move 0 to v.
	 move 0 to v in deckname.
	 move 0 to w.
	 move 0 to h.
	 move 0 to h of e.
	 move 0 to h of a.
         move 0 to h of h.
           DISPLAY "SORTIERFEHLER D224000" UPON CONSOLE.
	   EJECT
	   EJECT.
	   SKIP1
	   SKIP1.
	   SKIP2
	   SKIP2.
	   SKIP3
	   SKIP3.
	   TITLE 'here and now'
	   TITLE 'SPACE OR TIME'.
