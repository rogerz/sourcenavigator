      FUNCTION ITBIT(N,I)
*-----------------------------------------------------------------------
*
*   Tests bit I ( 0 < I < 26)  in word N , rightmost = 1 .
*
*-----------------------------------------------------------------------
      DIMENSION NP(26)
      DATA IFIRST/0/
      SAVE IFIRST
      SAVE NP
      IF(IFIRST.EQ.0)  THEN
         IFIRST=1
         NP(1)=1
         DO 10 J=2,26
            NP(J)=2*NP(J-1)
   10    CONTINUE
      ENDIF
      IF(I.GT.0.AND.I.LE.25)  THEN
         ITBIT=MOD(N,NP(I+1))/NP(I)
      ELSE
         ITBIT=0
      ENDIF
      END
