      SUBROUTINE READEC
*-----------------------------------------------------------------------
*
*--- extracts one complete routine from input file, buffers it.
*    the routine must end with an 'END' statement, or EOF
*
*   Routines longer than MXSIMA lines are split.
*
*   Blocks of comment lines in front of routines are treated as
*   separate entities.
*
*   The statements are counted, start and end of each statement
*   (including comments between cont. lines) kept. Blocks of
*   comment lines are treated like statements.
*
*--- output
*    SIMA           COMMON/ALCAZA/ statement images
*    NLINES,NSTAMM,NFSTAT,NKEEPL    ,/STATE/
*    NLTYPE(1..NLINES), NFLINE(1..NSTAMM), NLLINE(1..NSTAMM),
*    ICLASS(1..NSTAMM)                         /STATE/
*    NSTATC(..)  statistics
*    STATUS(1), STATUS(2), STATUS(5), STATUS(6),   /FLAGS/
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'flags.h'
      include 'state.h'
      include 'class.h'
*
      STATUS(6)=STATUS(5)
      NCOMM=0
      NFSTAT=0
      NSTAMM=0
      NPL=NLINES
      NLINES=0
   10 CONTINUE
*--- loop over input lines until E.O.F., or END, or
*    start of a new routine, or routine too long which
*    will then be split behind a convenient statement.
      IF(NLINES.EQ.MXSIMA)  THEN
*--- buffer full
         STATUS(5)=.TRUE.
         IF (.NOT.STATUS(6)) WRITE (MPUNIT,10000)MXSIMA,SIMA(1)
         CALL READSB(NCOMM,NST,ICL)
         IF (NST.GT.0)  THEN
*--- last FORTRAN statement could be incomplete - split before;
*    check as well for routine header
            IF (NST.EQ.NSTAMM.OR.(NFSTAT.GT.1.AND.ISTMDS(14,ICL).NE.0))
     +      THEN
               NSTAMM=NSTAMM-1
               NFSTAT=NFSTAT-1
               NKEEPL=NLINES-NLLINE(NSTAMM)
            ENDIF
         ENDIF
         GOTO 999
      ENDIF
      IF(NKEEPL.EQ.0)  THEN
         IF (.NOT.STATUS(1)) CALL INLINE(MIUNIT,SIMA(NLINES+1),STATUS(1)
     +   ,NLTYPE(NLINES+1))
         IF (STATUS(1))  THEN
*--- EOF on input file
            STATUS(2)=NLINES.EQ.0
            IF (STATUS(2)) GOTO 999
            STATUS(5)=.FALSE.
            CALL READSB(NCOMM,NST,ICL)
*--- last FORTRAN statement could be routine header
            IF (NFSTAT.GT.1)  THEN
               IF (ISTMDS(14,ICL).NE.0)  THEN
*--- leave routine header for next time
                  NSTAMM=NST-1
                  NFSTAT=NFSTAT-1
                  NKEEPL=NLINES-NLLINE(NSTAMM)
               ENDIF
            ENDIF
            GOTO 999
         ENDIF
      ELSE
*--- transfer buffered lines to start of buffer
         NKEEPL=NKEEPL-1
         NLTYPE(NLINES+1)=NLTYPE(NPL-NKEEPL)
         SIMA(NLINES+1)=SIMA(NPL-NKEEPL)
      ENDIF
*--- now a new line in SIMA(NLINES+1), with type NLTYPE(NLINES+1)
      IF(NLTYPE(NLINES+1).EQ.0)  THEN
*--- comment line
         NCOMM=NCOMM+1
         NLINES=NLINES+1
      ELSEIF (NLTYPE(NLINES+1).EQ.2)  THEN
*--- this is a cont. line - accept comment lines in between
         NCOMM=0
         NLINES=NLINES+1
      ELSE
*--- start of FORTRAN statement
         CALL READSB(NCOMM,NST,ICL)
         NLINES=NLINES+1
         IF (NST.GT.0)  THEN
*--- previous statement could be END
            IF (ICL.EQ.IEND.or.icl.eq.iend+71)  THEN
               NKEEPL=1
               STATUS(5)=.FALSE.
               GOTO 999
*--- or routine header ?
            ELSEIF (ISTMDS(14,ICL).NE.0)  THEN
               IF (NFSTAT.GT.1)  THEN
                  NSTAMM=NST-1
                  NFSTAT=NFSTAT-1
                  NKEEPL=NLINES-NLLINE(NSTAMM)
                  STATUS(5)=.FALSE.
                  GOTO 999
               ELSE
                  STATUS(6)=.FALSE.
               ENDIF
            ENDIF
         ENDIF
*--- accept the new line as start of a statement
         NSTAMM=NSTAMM+1
         NFLINE(NSTAMM)=NLINES
      ENDIF
      GOTO 10
10000 FORMAT(/' +++++++++ WARNING - deck with more than ',I5,
     +' lines encountered, deck split'/' first line =',A90)
  999 END
