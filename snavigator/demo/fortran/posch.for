      SUBROUTINE POSCH(SFIND,STRING,ICC1,ICC2,HOLFLG,MLEV,KPOS,ILEV)
*-----------------------------------------------------------------------
* positions on a specified character
* input
* SFIND     character looked for
* STRING    string to be looked up
* ICC1      first ch. in LSTRNG
* ICC2      last ch.       -
* HOLFLG    if TRUE, hollerith included
* MLEV      max. level allowed for character (relative to ICC1...ICC2)
* output
* KPOS      position of ICOMP in LSTRNG, or 0
* ILEV      relative level, including KPOS
*-----------------------------------------------------------------------
      include 'condec.h'
      LOGICAL HOLFLG
      CHARACTER STRING*(*),SFIND*1,STEMP*1
      ILEV=0
      KPOS=0
      JC=ICC1-1
   10 JC=JC+1
      IF (JC.GT.ICC2) GOTO 999
      STEMP=STRING(JC:JC)
      IF(STEMP.EQ.'(')  THEN
         ILEV=ILEV+1
      ELSEIF(STEMP.EQ.')')  THEN
         ILEV=ILEV-1
      ENDIF
      IF(STEMP.EQ.SFIND.AND.ILEV.LE.MLEV)  THEN
         KPOS=JC
         GOTO 999
      ENDIF
      if((index(sbase,stemp).eq.index(sbase,sfind)+26).and.
     &   ilev.le.mlev) then
         kpos=jc
         goto 999
      endif
      IF(STEMP.EQ.'{')  THEN
*--- start of character string
         IF (.NOT.HOLFLG) THEN
            I=INDEX(STRING(JC:ICC2),'}')
            IF (I.EQ.0) GOTO 999
            JC=I+JC-1
         ENDIF
      ENDIF
      GOTO 10
  999 END
