      SUBROUTINE OPRSLT(STYP1,SOPER,STYP2,IERR,SRSLT)
C! Get the type of an operator result
C
C for a given pair of operands, with a given operator,
C returns the type of the result, and indicates whether
C expression was mixed mode by IERR=0 (not mixed),
C IERR=1 (mixed).
C
      CHARACTER*6 SOPER
      CHARACTER*1 STYP1,STYP2,SRSLT
C
C throw out SOME operators
C
      IF(SOPER(:1).EQ.'*'.OR.
     &   SOPER(:1).EQ.'/'.OR.SOPER(:1).EQ.'+'.OR.
     &   SOPER(:1).EQ.'-') GOTO 5
        IERR = 0
        SRSLT=STYP1
        GOTO 999
    5 CONTINUE
C
      SRSLT = ' '
      IF(STYP1.EQ.'I') THEN
C INTEGER 1
        IF(STYP2.EQ.'I') SRSLT='I'
        IF(STYP2.EQ.'R') SRSLT='R'
        IF(STYP2.EQ.'D') SRSLT='D'
        IF(STYP2.EQ.'K') SRSLT='K'
      ELSE IF(STYP1.EQ.'R') THEN
C REAL 1
        IF(STYP2.EQ.'I') SRSLT='R'
        IF(STYP2.EQ.'R') SRSLT='R'
        IF(STYP2.EQ.'D') SRSLT='D'
        IF(STYP2.EQ.'K') SRSLT='K'
      ELSE IF(STYP1.EQ.'D') THEN
C DOUBLE PRECISION
        IF(STYP2.EQ.'I') SRSLT='D'
        IF(STYP2.EQ.'R') SRSLT='D'
        IF(STYP2.EQ.'D') SRSLT='D'
      ELSE IF(STYP1.EQ.'K') THEN
C COMPLEX
        IF(STYP2.EQ.'I') SRSLT='K'
        IF(STYP2.EQ.'R') SRSLT='K'
        IF(STYP2.EQ.'K') SRSLT='K'
      ENDIF
      IF(SRSLT.EQ.' ') THEN
C UNRECOGNISED TYPE
        SRSLT='$'
        IERR = 0
        GOTO 999
      ENDIF
C CHECK FOR EXPONENTIATION
      IF(SOPER(:2).EQ.'**') THEN
        SRSLT = STYP1
        IERR = 0
        GOTO 999
      ENDIF
C CHECK FOR MIXED MODE
      IF(STYP1.NE.STYP2) THEN
        IERR = 1
        GOTO 999
      ENDIF
      IERR = 0
  999 CONTINUE
      RETURN
      END
