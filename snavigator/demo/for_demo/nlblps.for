      FUNCTION NLBLPS(STRING,KFCH,KLCH)
*-----------------------------------------------------------------------
*
*   returns as function value the position of the last blank in string
*   'STRING' between KFCH and KLCH.
*   This value is KFCH-1 if the first character is not blank.
*
*-----------------------------------------------------------------------
      CHARACTER *(*) STRING
      NLBLPS=KFCH-1
      DO 10 I=KFCH,KLCH
         IF (STRING(I:I).NE.' ') GOTO 20
         NLBLPS=I
   10 CONTINUE
   20 CONTINUE
      END
