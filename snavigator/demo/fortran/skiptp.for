      SUBROUTINE SKIPTP(ITYPE,STRING,ICC1,ICC2,HOLFLG,KPOS,ILEV)
*-----------------------------------------------------------------------
* positions on the last character of a string of the requested type
* input
* ITYPE      1 = numeric
*            2 = alpha
*            3 = alpha-numeric
*            4 = special
*            5 = FORTRAN-name
*            6 = expression ( no [,] at level 0 )
* STRING     string
* ICC1       first ch, in string
* ICC2       last   -    -  -
* HOLFLG     if TRUE, hollerith included
* output
* KPOS       position of last ch. of given type, if ICC1 is of that
*            type, otherwise = 0
* ILEV       level (including KPOS) relative to input level 0
*-----------------------------------------------------------------------
      LOGICAL HOLFLG
      CHARACTER STRING*(*),STEMP*1
      include 'convex.h'
      ILEV=0
      KPOS=0
      NCNT=0
      ISSTR=0
      ILBASE=-1
      JC=ICC1-1
   10 JC=JC+1
      IF (JC.GT.ICC2) GOTO 999
      STEMP=STRING(JC:JC)
*--- skip blanks outside strings
      IF (STEMP.EQ.' '.AND.ISSTR.EQ.0) GOTO 10
      IF(STEMP.EQ.'{')  THEN
*--- start of character string
         ISSTR=1
         IF (.NOT.HOLFLG) THEN
            ISSTR=0
            I=INDEX(STRING(JC:ICC2),'}')
            IF (I.EQ.0) GOTO 999
            JC=I+JC-2
         ENDIF
         GOTO 10
      ELSEIF(STEMP.EQ.'}')  THEN
         ISSTR=0
         IF(ITYPE.EQ.6)  THEN
            KPOS=JC
         ELSE
            GOTO 10
         ENDIF
      ELSEIF(ITYPE.EQ.1)  THEN
         IF (NUMCH(STEMP)) KPOS=JC
      ELSEIF(ITYPE.EQ.2)  THEN
         IF (ALPHCH(STEMP)) KPOS=JC
      ELSEIF(ITYPE.EQ.3)  THEN
         IF (ANUMCH(STEMP)) KPOS=JC
      ELSEIF(ITYPE.EQ.4)  THEN
         IF (SPECCH(STEMP))  THEN
            KPOS=JC
            IF (STEMP.EQ.'(')  THEN
               ILEV=ILEV+1
            ELSEIF (STEMP.EQ.')')  THEN
               ILEV=ILEV-1
            ENDIF
         ENDIF
      ELSEIF(ITYPE.EQ.5)  THEN
         IF (NCNT.EQ.0)  THEN
            IF (ALPHCH(STEMP))  THEN
               KPOS=JC
               NCNT=NCNT+1
            ENDIF
         ELSEIF (ANUMCH(STEMP))  THEN
            KPOS=JC
         ENDIF
      ELSEIF(ITYPE.EQ.6)  THEN
         IF (KPOS.EQ.0.AND..NOT.(ANUMCH(STEMP).OR.STEMP.EQ.'('.OR.STEMP.
     +   EQ.'+'.OR.STEMP.EQ.'-'.OR.STEMP.EQ.''''))GOTO 999
         IF (STEMP.EQ.'(')  THEN
            ILEV=ILEV+1
         ELSEIF (ILBASE.LT.0)  THEN
            ILBASE=ILEV
         ENDIF
         IF (STEMP.EQ.')')  ILEV=ILEV-1
         IF ((STEMP.NE.','.OR.ILEV-ILBASE.GT.0).AND.ILEV.GE.0) KPOS=JC
      ENDIF
      IF (KPOS.EQ.JC) GOTO 10
  999 END
