      FUNCTION NEXTIN(STRING,KFCH,KLCH)
*-----------------------------------------------------------------------
*
*   returns as function value the integer extracted from string
*   'STRING' between KFCH and KLCH, by ignoring all non-numeric
*   characters. default value is therefore 0.
*
*-----------------------------------------------------------------------
      include 'param.h'
      CHARACTER *(*) STRING
      include 'convex.h'
      N=0
*--- convert external zero into internal
      NZERO=ICVAL('0')
*--- construct integer
      DO 10 J=KFCH,KLCH
         I=ICVAL(STRING(J:J))-NZERO
         IF (I.GE.0.AND.I.LE.9) N=10*N+I
   10 CONTINUE
      NEXTIN=N
      END
