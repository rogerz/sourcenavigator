      SUBROUTINE SKIPLV(STRING,ICC1,ICC2,HOLFLG,KPOS,ILEV)
*-----------------------------------------------------------------------
* scans back to right bracket corresponding to last left one
* input
* STRING    string to be looked up
* ICC1      first ch. in LSTRNG
* ICC2      last ch.       -
* HOLFLG    if TRUE, hollerith included
* output
* KPOS      position of right bracket or 0
* ILEV      relative level, including KPOS (i.e. -1, if found)
*-----------------------------------------------------------------------
      LOGICAL HOLFLG
      CHARACTER STRING*(*),STEMP*1
      ILEV=0
      KPOS=0
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
      ELSEIF(STEMP.EQ.'(')  THEN
         ILEV=ILEV+1
      ELSEIF(STEMP.EQ.')')  THEN
         ILEV=ILEV-1
         IF (ILEV.LT.0) GOTO 20
      ENDIF
      GOTO 10
   20 CONTINUE
      KPOS=JC
  999 END
