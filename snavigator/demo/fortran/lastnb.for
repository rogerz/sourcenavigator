      FUNCTION LASTNB(STRING,KFCH,KLCH)
*-----------------------------------------------------------------------
*
*   Returns as function value the position of the last non-blank in stri
*   'STRING' between KFCH and KLCH.
*   This value is KFCH-1 if STRING consists of blanks only.
*
*-----------------------------------------------------------------------
      CHARACTER *(*) STRING
      LASTNB=KFCH-1
      DO 10 I=KLCH,KFCH,-1
         IF(STRING(I:I).NE.' ') THEN
            LASTNB=I
            GOTO 999
         ENDIF
   10 CONTINUE
  999 END
