      LOGICAL FUNCTION LEXARS(NNAM)
*-----------------------------------------------------------------------
*
*--- returns TRUE if name NNAM in current statement is both in an
*    EXTERNAL statement, and is passed as an argument
*---Input
*   NNAM        position of name in current statement list
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'treecom.h'
      include 'state.h'
      include 'cursta.h'
      CHARACTER STEMP*1
      LEXARS=.FALSE.
      IF(NSEND(NNAM).LT.NCHST.AND.NNAM.GT.1)  THEN
         DO 10 I=1,NEXEL
            IF(SNAMES(ISNAME+NNAM).EQ.SEXEL(I)) GOTO 20
   10    CONTINUE
         GOTO 999
   20    CONTINUE
         K=NSEND(NNAM)
         STEMP=SSTA(K+1:K+1)
         IF(STEMP.EQ.' ') THEN
            STEMP=SSTA(K+2:K+2)
         ENDIF
         LEXARS=STEMP.NE.'('
      ENDIF
  999 END
