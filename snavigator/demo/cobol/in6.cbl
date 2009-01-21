       IDENTIFICATION DIVISION.
         PROGRAM-ID.    PROBE is common program comment_entry
         AUTHOR.        J. GROSCH.
         installation.        comment_entry
         DATE-WRITTEN.  26.05.94.
         DATE-COMPILED. 26.05.94.
         security.        comment_entry
         remarks.        comment_entry
      *
       ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
           SOURCE-COMPUTER. PC 47.11.
           OBJECT-COMPUTER. PC 08.15
	       memory size 13 modules
	       program collating sequence is quak
	       segment-limit is 14.
             special-names.
               switch-0 is a
               switch-1 is a on is b off status is c
               a off status is c on is b
             alphabet quak is native
             alphabet quark ascii
             alphabet quarks 0 1 thru 2 3 also 4 also 5
             symbolic characters a b c is 1 2 3
                                 d e f is 4 5 6
                                 g h i is 1 2 3 in quak
             symbolic characters a b c is 1 2 3
                                 d e f is 4 5 6
                                 g h i is 1 2 3 in quark
             symbolic characters a b c is 1 2 3
                                 d e f is 4 5 6
                                 g h i is 1 2 3
	     class c is 'a' 'b' 'c' thru 'd'
	     class d is 'a' 'b' 'c' thru 'd'
	     class e is 'a' 'b' 'c' thru 'd'
	     currency sign is '$'
	     decimal-point is comma
	     numeric sign is trailing separate
	     call-convention 3 is aufruf
	     console is crt
	     cursor is cur crt status is stat.

         INPUT-OUTPUT SECTION.
           FILE-CONTROL.
             SELECT D1 ASSIGN TO EXTERNAL DD224000
                  ORGANIZATION IS SEQUENTIAL
        	  RESERVE  1  ALTERNATE AREA.
             SELECT D2 ASSIGN TO EXTERNAL DD224000
                  ORGANIZATION IS SEQUENTIAL
        	  RESERVE  1  ALTERNATE AREA.
             SELECT optional D3
               ASSIGN TO EXTERNAL D4 D5 D6
               ASSIGN TO DYNAMIC DISK D4 D5 D6
	       ORGANIZATION IS SEQUENTIAL
	       reserve no alternate area
	       padding character is ' '
	       record delimiter is standard-1
	       access mode is SEQUENTIAL
	       lock mode is manual with lock on multiple records
	       file status is n1 n2
               password is n3
	       record key is n4 = n5 n6 n7
	       alternate record key is n4 = n5 n6 n7 n8 = n9 n10 n11
	         with duplicates password is n12 suppress when all '1'
	       RESERVE  1  ALTERNATE AREA
	       sort status is n0.

           I-O-CONTROL.
             rerun on f1 every end of reel of f2
             rerun on f2 every 13 records of f3
             rerun on f3 every 14 clock-units
             rerun on f4 cond
             same record area for f1 f2 f3
             same sort area for f4 f5 f6
             same sort-merge area for f7 f8 f9
             multiple file tape contains f1 position 2 f2 f3
             multiple file tape contains f1 position 3 f2 f3
             multiple file tape contains f1 position 4 f2 f3
             apply write-only on f4 f5 f6
             apply core-index on f7 f8 f9
             apply record-overflow on f10 f11 f12
             apply reorg-criteria to dat on f13
             apply reorg-criteria to dat on f14.
      *
       DATA DIVISION.

         FILE SECTION.

           sd h.
	     01  DECKNAME.
           fd g
	     reports are r1 r2 r3.
	     01  DECKNAME.
           fd f
             is external
             is global
             block contains 1 to 2 records
             record contains 3
             record is varying in size from 5 to 6 characters depending
               on nn
             label record is standard
             label record is  n1 n2 n3
             value of i1 is '1' n4 is n5 n6 n7
             value of file-id is n8
             recording mode is s
             data records are r1 r2 r3
             linage is 9 lines with footing at 9 lines at top 10 lines
               at bottom 11
	     code-set is quark for i1 i2 i3.
	  01  DECKNAME.
	    02 VORNAME PIC X(12).
	      03 NACHNAME PIC X(12).

         WORKING-STORAGE SECTION.

	   77 I
	     redefines x
	     is external
	     is global
	     PIC is 99
	     usage is comp
	     occurs 3 times
	       ascending key is k1 k2 k3
	       descending key is k4 k5 k6
	       indexed by i1 i2 i3
	     occurs 4 to 5 times depending on nn
	       ascending key is k1 k2 k3
	       descending key is k4 k5 k6
	       indexed by i1 i2 i3
	     sign is trailing separate
	     sync left
	     just right
	     blank when zeroes
	     value is 3.14.
	   01 DECKNAME.
	     05 VORNAME  PIC X(12).
	     05 NACHNAME PIC X(12).
	   66 n0 renames n1.
	   66 n0 renames n1 through n2.
	   88 n9 values are 1 2 thru 3 4 when set to false 5.
	   78 c0 value is start of n0 + length of n1.

         local-STORAGE SECTION.
	   01 DECKNAME.
	     05 VORNAME  PIC X(12).
	     05 NACHNAME PIC X(12).

         linkage SECTION.
	   01 DECKNAME.
	     05 VORNAME  PIC X(12).
	     05 NACHNAME PIC X(12).

         communication SECTION.

	   CD n0 For Initial INPUT
	      Symbolic QUEUE		Is nn
	      Symbolic SUB-QUEUE-1	Is nn
	      Symbolic SUB-QUEUE-2	Is nn
	      Symbolic SUB-QUEUE-3	Is nn
	      MESSAGE DATE		Is nn
	      MESSAGE TIME		Is nn
	      Symbolic SOURCE		Is nn
	      TEXT LENGTH		Is nn
	      END KEY			Is nn
	      STATUS KEY		Is nn
	      Message COUNT		Is nn.
	   01 DECKNAME.
	     05 VORNAME  PIC X(12).
	     05 NACHNAME PIC X(12).

	   CD n0 For Initial INPUT nn nn nn nn nn nn
	      nn nn nn nn nn.

	   CD n0 For         OUTPUT
	      DESTINATION COUNT		Is nn
	      TEXT LENGTH		Is nn
	      STATUS KEY		Is nn
	      DESTINATION TABLE OCCURS 3 Times indexed by i1 i2 i3
	      ERROR KEY			Is nn
		       DESTINATION	Is nn
	      SYMBOLIC DESTINATION	Is nn.

	   CD n0 For Initial I-O
	      MESSAGE DATE		Is nn
	      MESSAGE TIME		Is nn
	      Symbolic TERMINAL		Is nn
	      TEXT LENGTH		Is nn
	      END KEY			Is nn
	      STATUS KEY		Is nn.

	   CD n0 For Initial I-O nn nn nn nn nn nn.

         report SECTION.
           rd r0.
           rd r1.
           rd r2
             is global
             with code 'abc'
             control is FINAL d1 d2 d3
             page limits are 2 lines
               heading 3
	       first detail 3
	       last detail 4
	       footing 5.

             01 nn
               line number is 12 on next page
	       next group is next page
	       type is control heading mm
	       usage is display.

	       02 mm
		 picture xxx
		 display
		 sign is leading separate
		 justified
		 blank when zero
		 line number is plus 22
		 column number is 33
		 source is i1
		 value is 'c'
		 sum i1 i2 i3 upon d1 d2 d3 reset on final
		 sum i4 i5 i6 upon d4 d5 d6 reset on d7
		 group indicate.

         screen SECTION.
           01 s1.
             02 filler
		BLANK SCREEN
		BLANK LINE
		BELL
		BEEP
		BLINK
		ERASE EOL
		ERASE EOS
		HIGHLIGHT
		LOWLIGHT
		GRID
		LEFTLINE
		OVERLINE
		REVERSE-VIDEO
		UNDERLINE
		SIZE Is identifier-or-integer
		LINE
		LINE Number Is plus identifier-or-integer
		COLUMN
		COLUMN Number Is - identifier-or-integer
		COL
		COL    Number Is + identifier-or-integer
		FOREGROUND-COLOR Is identifier-or-integer
		BACKGROUND-COLOR Is identifier-or-integer
		CONTROL Is identifier
		VALUE Is 'literal'
		'string'
		PICTURE Is XXX
		FROM identifier-or-literal
		TO identifier
		USING identifier
		USAGE Is DISPLAY
		         DISPLAY
		BLANK When ZERO
		           ZERO
		JUSTIFIED Right
		SIGN  Is TRAILING SEPARATE Character
		         LEADING  SEPARATE Character
		AUTO
		AUTO-SKIP
		SECURE
		NO-ECHO
		REQUIRED
		EMPTY-CHECK
		PROMPT
		PROMPT CHARACTER Is identifier-or-literal
		PROMPT           IS identifier-or-literal
		PROMPT              identifier
		PROMPT              'string'
		OCCURS 77 Times
		FULL
		LENGTH-CHECK
		ZERO-FILL.
      *
       PROCEDURE DIVISION mnemo
         using d1 d2 by reference d3 d4 by value d5 d6 d7.

000016 DECLARATIVES.
         s1 section.
000017     USE AFTER ERROR PROCEDURE ON FILE-1.                         93.07.07
000018          MOVE 100 TO FIELD-A.                                    93.07.07
         s2 section.
000019     USE AFTER EXCEPTION PROCEDURE ON I-O.                        93.07.07
000020          DISPLAY 'GIVE UP'                                       93.07.07
000021          PERFORM EOJ.                                            93.07.07
         s3 section.
000019     USE global AFTER standard ERROR PROCEDURE ON f1 f2 f3
	      giving d1 d2.                        
         s4 section.
000019     USE AFTER standard ending file label PROCEDURE ON f1 f2 f3.
         s5 section.
           use for debugging on a all references of b all procedures.
000022 END DECLARATIVES.                                                94.10.19
000023                                                                  93.07.07
	     GO TO X1.
	   P0.
	     CLOSE a d  b.
         S1 SECTION.
	     cancel d f b.
	   P1.
	     ALTER a to b.
	   P2.
	     EXIT.
         S2 SECTION.
      *    IF m GO TO S2.
	   ALTER a to b
	   ALTER a to b.
	   ALTER a to b
	   EXIT.
	   if a then commit.
	   if a then exit.
	   if a commit exit exhibit named i.
	   if a then commit else exit.
	   if a then commit else exec sql 'm' end-exec.
	   if a then next sentence else next sentence.
	   if a then
	      next sentence
	      if b then exit end-if
	      next sentence
	   else
	      next sentence
	      if b then exit else commit end-if
	      next sentence
	   end-if.
         S3 SECTION.
	   accept i
	   accept i end-accept
	   accept i on exception commit stop run end-accept
	   add a to b
	   add a to b size error exit not size error exit.
	   chain i
	   call a
	   call a end-call
	   call b not on exception commit end-call
           call b not on exception commit
	   .
	   compute x = 7
	   compute x = 7 size error cancel j end-compute
	   compute x = 7 size error continue
	   .
	   delete f
	   delete d invalid continue end-delete
	   display y
	   display z exception move a to b end-display
	   divide a into b
	   divide a by c giving d on size error alter a to b end-divide
	   evaluate a
	      when 1 exit
	      when 2 add a to b
	      when other goback
	   end-evaluate
	   multiply a by b
	   multiply a by c not on size error open input f end-multiply
	   on a move b to c
	   on a move b to c else next sentence
	   .
	   perform m
	   perform m 3 times
	   perform m until c
	   perform m until exit
	   perform m varying i from 1 by 1 until i = 10
	   perform open input f
	   perform 4 times open input f
	   perform until c open input f
	   perform until exit open input f
	   perform varying i from 1 by 1 until i = 10 open input f
	   .
	   read f
	   read f end purge c
	   .
	   receive m message into i
	   receive m message into i data release r
	   .
	   return a end rollback
	   return a end rollback not end send s from t end-return
	   .
	   rewrite r
	   rewrite r invalid continue end-rewrite
	   .
	   search i when c next sentence
	   search i end commit when c next sentence end-search
	   .
	   start f
	   start f invalid exit end-start
	   .
	   stop run
	   .
	   string i delimited by size into j
	   string i delimited by size into j overflow stop run end-string
	   .
	   subtract i from j
	   subtract i from j size error suppress end-subtract
	   .
	   unstring i into j
	   unstring i into j overflow stop run end-unstring
	   .
	   write r
	   write r eop continue end-write
	   write r eop continue
	   .
         S4 SECTION.
	   accept nn with auto bell beep
	   add a b c to x y z
	   add a b c to d giving x y z rounded
	   alter a to b c to d e to f
	   call p using
	      a b c
	      by reference d e f
	      by content 6 77 888
	      by value i j k
	      by value 6 77 988
	   chain p using
	      a b c
	      by reference d e f
	      address of d e f
	      by content 6 77 888
	   close f lock g disp h
	   compute a b c rounded = 1 + z
	   delete file f g h
	   display a b c with no advancing
	   display a line 1 col 2 with bell beep blink
	   divide a into x y z
	   divide a into d giving x y z rounded
	   divide a by d giving x y z rounded
	   entry a using
	      i j k
	      by reference d e f
	      by value i j k
	   evaluate i also j also k
	      when 1 also '2' also v3
	      when 4 also '5' also v6
		 go to p q r depending on x
		 if x then
		    initialize a b c replacing
		       national data by x
		       numeric data by x
		       alphabetic data by x
		    initiate r s t
		 else
		    inspect i tallying
		       j for
			  characters before k after m
			  all
			     k before k after m
			     j before k after m
		       i for
			  characters before k after m
			  all
			     k before k after m
			     j before k after m
		    inspect i replacing
			  characters by i before k after m
			  leading
			     k by n before k after m
			     j by '2' before k after m
		    inspect i tallying
		       j for
			  characters before k after m
			  all
			     k before k after m
			     j before k after m
		       i for
			  characters before k after m
			  all
			     k before k after m
			     j before k after m
		       replacing
			  characters by j before k after m
			  leading
			     k by n before k after m
			     j by '2' before k after m
		    inspect i converting j to k before k after m
		 end-if
	      when 1 also '2' also v3
		 merge f
		    ascending key a1 a2 a3
		    descending key d1 d2 d3
		    using f1 f2 f3
		    giving g1 g2 g3
		 move a to b c d
		 move corr a to b c d
	      when other
		 multiply 1 by x y z
		 multiply 1 by 2 giving x y z rounded
	   end-evaluate
	   open
	      input f1 f2 f3
	      output f1 f2 f3
	      i-o f1 f2 f3
	      extend f1 f2 f3
	   perform p of q thru s in t
	      varying i from 1 by 2 until t
	      after   j from 1 by 2 until t
	      after   k from 1 by 2 until t
	   read i at end
	      search x
		 when c1 exit
		 when c2 exit
		 when c3 exit
	      end-search
	      search all y
		 when d1 = 1 and d2 = 2 and d3 = 3
		    sort f
		       descending key d1 d2 d3
		       ascending key a1 a2 a3
		       using f1 f2 f3
		       giving g1 g2 g3
	      end-search
	   end-read
	   set a b c to on i j k to off
	   set a b c to true i j k to false
	   set a b c to address of i
	   set a b c to i
	   set a b c to '2'
	   set a b c to NULL
	   set address of a address of b address of c to NULLS
	   set a b c up by j
	   set a b c down by length of j
	   string
	      a b c delimited by i
	      d e f delimited by 'j'
	      g h i delimited by SIZE
	      into x
	   subtract a b 3 from x y z rounded
	   subtract a b 3 from x giving x y z rounded
	   terminate r1 r2 r3
	   unstring i
	      delimited by all a or b or all c
	      into
		 x delimiter in y count in z
		 i delimiter in j count in k
	   write a from x of b of c (1 y z + 2)
	   move function max (a b - 3 4) to s
	   .
       end PROGRAM p.
       IDENTIFICATION DIVISION.
         PROGRAM-ID.    p2 is common program comment_entry
	  IDENTIFICATION DIVISION.
	    PROGRAM-ID.    q1 is common program comment_entry
	  ENVIRONMENT DIVISION.
	    CONFIGURATION SECTION.
	      SOURCE-COMPUTER. PC 47.11.
	      OBJECT-COMPUTER. SIEMENS 4.567.
	  end PROGRAM p.
	  IDENTIFICATION DIVISION.
	    PROGRAM-ID.    q2 is common program comment_entry
	  ENVIRONMENT DIVISION.
	    CONFIGURATION SECTION.
	      SOURCE-COMPUTER. PC.
	      OBJECT-COMPUTER. TR 440.
	  end PROGRAM p.
	  IDENTIFICATION DIVISION.
	    PROGRAM-ID.    q3 is common program comment_entry
	  end PROGRAM p.
       end PROGRAM p.
       IDENTIFICATION DIVISION.
         PROGRAM-ID.    p3 is common program comment_entry
