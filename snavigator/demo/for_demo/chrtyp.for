      SUBROUTINE CHRTYP(ITYPE,STRING,ICC1,ICC2,HOLFLG,KPOS,ILEV)
*-----------------------------------------------------------------------
* returns first ch. of specified type, or 0
* input
* ITYPE       type
*            1 = numeric
*            2 = alpha
*            3 = alpha-numeric
*            4 = special
*            5 = FORTRAN-name
* string      string to be looked up
* ICC1        first ch. in string
* ICC2        last ch. in string
* HOLFLG      if TRUE, hollerith included in search
* output
* KPOS        position of first ch. of specified type, or 0
* ILEV        relative level, including KPOS
*
*-----------------------------------------------------------------------
      LOGICAL HOLFLG
      CHARACTER STRING*(*),STEMP*1
      include 'convex.h'
      ILEV=0
      KPOS=0
      NCNT=0
      JC=ICC1-1
   10 JC=JC+1
      IF (JC.GT.ICC2) GOTO 999
      STEMP=STRING(JC:JC)
      IF(STEMP.EQ.'{')  THEN
*--- start of character string
         IF (.NOT.HOLFLG) THEN
            I=INDEX(STRING(JC:ICC2),'}')
            IF (I.EQ.0) GOTO 999
            JC=I+JC-1
         ENDIF
         GOTO 10
      ELSEIF(STEMP.EQ.'}')  THEN
         GOTO 10
      ELSEIF(STEMP.EQ.'(')  THEN
         ILEV=ILEV+1
      ELSEIF(STEMP.EQ.')')  THEN
         ILEV=ILEV-1
      ENDIF
      IF(ITYPE.EQ.1)  THEN
         IF (NUMCH(STEMP)) KPOS=JC
      ELSEIF(ITYPE.EQ.2)  THEN
         IF (ALPHCH(STEMP)) KPOS=JC
      ELSEIF(ITYPE.EQ.3)  THEN
         IF (ANUMCH(STEMP)) KPOS=JC
      ELSEIF(ITYPE.EQ.4)  THEN
         IF (SPECCH(STEMP)) KPOS=JC
      ELSEIF(ITYPE.EQ.5)  THEN
         IF (NCNT.EQ.0)  THEN
            IF (ALPHCH(STEMP))  THEN
               KPOS=JC
               NCNT=NCNT+1
            ENDIF
         ELSEIF (ANUMCH(STEMP))  THEN
            KPOS=JC
         ENDIF
      ENDIF
      IF (KPOS.NE.JC) GOTO 10
  999 END
