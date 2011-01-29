      LOGICAL FUNCTION SAMEST(IST)
*-----------------------------------------------------------------------
*
*   Compares statement IST in SIMA with the new image SNEWST, returns
*   .TRUE. if they are identical.
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'state.h'
      include 'cursta.h'
      SAMEST=.FALSE.
      N=0
      DO 10 I=NFLINE(IST),NLLINE(IST)
         IF(NLTYPE(I).NE.0) N=N+1
   10 CONTINUE
      IF(N.NE.NEWOUT) GOTO 999
      N=0
      DO 20 I=NFLINE(IST),NLLINE(IST)
         IF(NLTYPE(I).NE.0) THEN
            N=N+1
            IF(SNEWST(N)(:72).NE.SIMA(I)(:72)) GOTO 999
         ENDIF
   20 CONTINUE
      SAMEST=.TRUE.
  999 END
