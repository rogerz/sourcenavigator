      SUBROUTINE GETNAM(STRING,K1,K2,KFCH,KLCH)
*-----------------------------------------------------------------------
*
*--- finds one name at a time
*
*--- input
*    STRING           input string
*    K1, K2           first and last ch. in STRING for scan
*--- output
*    KFCH             start of name in STRING, or 0 if none
*    KLCH             end of name in STRING
*
*-----------------------------------------------------------------------
      CHARACTER STRING*(*), STEMP*1, SLAST*1
      LOGICAL STARTD,SKIP
      include 'convex.h'
      SLAST=' '
      STARTD=.FALSE.
      SKIP=.FALSE.
      KNB=0
      KFCH=0
      JC=K1-1
   10 JC=JC+1
      KLCH=KNB
      IF (JC.GT.K2) GOTO 999
      STEMP=STRING(JC:JC)
*--- skip blanks
      IF (STEMP.EQ.' ') GOTO 10
      IF(STEMP.EQ.'{')  THEN
*--- start of string - quit or skip
         IF (STARTD) GOTO 999
         I=INDEX(STRING(JC+1:K2),'}')
         IF (I.EQ.0) GOTO 999
         JC=I+JC
         GOTO 10
      ENDIF
      KNB=JC
      IF(SPECCH(STEMP))  THEN
         IF (STARTD) GOTO 999
*--- 'SKIP' helps to ignore .ge. etc
         SKIP=STEMP.EQ.'.'.AND.(.NOT.SKIP.OR.SLAST.EQ.'.')
      ELSEIF(ALPHCH(STEMP))  THEN
         IF (.NOT.(SKIP.OR.NUMCH(SLAST)))  THEN
*--- preceding if is to catch 1E3 etc
            IF (.NOT.STARTD) KFCH=JC
            STARTD=.TRUE.
         ENDIF
      ELSE
*--- numeric
         SKIP=.FALSE.
*--- this is necessary for 1.E3 etc.
      ENDIF
*--- keep last character
      SLAST=STEMP
      GOTO 10
  999 END
