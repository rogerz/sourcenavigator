       IDENTIFICATION DIVISION.
         PROGRAM-ID.    PROBE.
         AUTHOR.        J GROSCH.
         DATE-WRITTEN.  26-05-94.
         DATE-COMPILED. 26-05-94.
      *
       ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
           SOURCE-COMPUTER. COPY csc.
           OBJECT-COMPUTER. COPY coc.
         SPECIAL-NAMES. COPY csn.
         INPUT-OUTPUT SECTION.
           FILE-CONTROL. COPY cfc.
           I-O-CONTROL. COPY cic.
      *
       DATA DIVISION.
         FILE SECTION.

         WORKING-STORAGE SECTION.
       77  I PIC 99.
       77  J.

         local-STORAGE SECTION.
           01 DECKNAME.
             05 VORNAME  PIC X(12).
             05 NACHNAME PIC X(12).

         linkage SECTION.
           01 DECKNAME.
             05 VORNAME  PIC X(12).
             05 NACHNAME PIC X(12).

         communication SECTION.

           CD n0 For Initial INPUT nn nn nn nn nn nn
              nn nn nn nn nn.

           CD n0 For         OUTPUT
              DESTINATION COUNT         Is nn
              TEXT LENGTH               Is nn
              STATUS KEY                Is nn
              DESTINATION TABLE OCCURS 3 Times indexed by i1 i2 i3
              ERROR KEY                 Is nn
                       DESTINATION      Is nn
              SYMBOLIC DESTINATION      Is nn.

           CD n0 For Initial I-O
              MESSAGE DATE              Is nn
              MESSAGE TIME              Is nn
              Symbolic TERMINAL         Is nn
              TEXT LENGTH               Is nn
              END KEY                   Is nn
              STATUS KEY                Is nn.

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
       PROCEDURE DIVISION.
           MAIN. COPY cpp.
         A SECTION. COPY csp.

       END PROGRAM p1.

       IDENTIFICATION DIVISION.
         PROGRAM-ID.    PROBE.
         AUTHOR.        J GROSCH.
         DATE-WRITTEN.  26-05-94.
         DATE-COMPILED. 26-05-94.
      *
       ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
           SOURCE-COMPUTER. NOTE COPY csc.
           OBJECT-COMPUTER. NOTE COPY coc.
         SPECIAL-NAMES. NOTE COPY csn.
         INPUT-OUTPUT SECTION.
           FILE-CONTROL. NOTE COPY cfc.
             NOTE SELECT f COPY cse.
           I-O-CONTROL. NOTE COPY cic.
      *
       DATA DIVISION.
         FILE SECTION.
           FD D224000. NOTE COPY cfd.
           SD D224000. NOTE COPY cfd.

         WORKING-STORAGE SECTION.
       77  I PIC 99.
       01  DECKNAME. NOTE COPY cd1.
       01  FILLER. NOTE COPY cd1.
       NOTE 01  COPY cd1.
       01  DECKNAME REDEFINES I. NOTE COPY cd1.
       77  DECKNAME. NOTE COPY cd7.
       77  FILLER. NOTE COPY cd7.
       NOTE 77  COPY cd7.
       77  DECKNAME REDEFINES I. NOTE COPY cd7.
       77  J.

         local-STORAGE SECTION.
           01 DECKNAME.
             05 VORNAME  PIC X(12).
             05 NACHNAME PIC X(12).

         linkage SECTION.
           01 DECKNAME.
             05 VORNAME  PIC X(12).
             05 NACHNAME PIC X(12).

         communication SECTION.

           NOTE CD n0 COPY ccd.

           CD n0 For Initial INPUT nn nn nn nn nn nn
              nn nn nn nn nn.

           CD n0 For         OUTPUT
              DESTINATION COUNT         Is nn
              TEXT LENGTH               Is nn
              STATUS KEY                Is nn
              DESTINATION TABLE OCCURS 3 Times indexed by i1 i2 i3
              ERROR KEY                 Is nn
                       DESTINATION      Is nn
              SYMBOLIC DESTINATION      Is nn.

           CD n0 For Initial I-O
              MESSAGE DATE              Is nn
              MESSAGE TIME              Is nn
              Symbolic TERMINAL         Is nn
              TEXT LENGTH               Is nn
              END KEY                   Is nn
              STATUS KEY                Is nn.

           CD n0 For Initial I-O nn nn nn nn nn nn.

         report SECTION.
           RD D224000. NOTE COPY crd.
           RD D224000 CODE m. NOTE COPY crd.
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
       PROCEDURE DIVISION.
           MAIN. NOTE COPY cpp.
         A SECTION. NOTE COPY csp.
