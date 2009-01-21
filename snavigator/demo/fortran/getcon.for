      SUBROUTINE GETCON(STRING,I1,I2,KLCH,STYP)
*-----------------------------------------------------------------------
*
*--- returns a numeric constant, and its type. Constant must start on I1
*--- input
*    STRING(I1:I2)  string
*--- output
*    KLCH           last pos. of const., or 0 if none
*    STYP           type of constant:
*                  'I' = integer
*                  'R' = real
*                  'D' = double prec.
*                  'K' = complex
*                  '$' = not specified
*
*-----------------------------------------------------------------------
      CHARACTER *(*)  STRING
      CHARACTER*1 STYP,STEMP,SLAST,SLOG*7
      character*7 touppr
      external touppr
      include 'convex.h'
c
      STYP='$'
      KLCH=0
      STEMP=STRING(I1:I1)
      IF(STEMP.EQ.'{')  THEN
*--- string, hollerith, etc., all treated as CHARACTER
         KPOS=INDEX(STRING(I1:I2),'}')
         IF(KPOS.NE.0) THEN
            KLCH=I1+KPOS-1
            STYP='C'
         ENDIF
      ELSEIF(STEMP.EQ.'.')  THEN
*--- logical constant ?
         CALL GETNBL(STRING(I1:I2),SLOG,NN)
         slog = touppr(slog)
         IF(NN.GE.5) THEN
            IF(SLOG(:5).EQ.'.NOT.'.OR.SLOG(:6).EQ.'.TRUE.'
     +      .OR.SLOG.EQ.'.FALSE.') THEN
               CALL POSCH('.',STRING,I1+1,I2,.FALSE.,0,KLCH,ILEV)
               IF(KLCH.NE.0) THEN
                  STYP='L'
                  GOTO 999
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF(NUMCH(STEMP).OR.STEMP.EQ.'.')  THEN
*--- integer, real, or double precision
         KLCH=I1
         IF(STEMP.EQ.'.')  THEN
            STYP='R'
         ELSE
            STYP='I'
         ENDIF
         SLAST=STEMP
         DO 10 I=I1+1,I2
            STEMP=STRING(I:I)
            IF(STEMP.EQ.' ') GOTO 10
            IF(.NOT.NUMCH(STEMP)) THEN
               IF(STEMP.EQ.'.'.OR.STEMP.EQ.'E') THEN
                  STYP='R'
               ELSEIF(STEMP.EQ.'D') THEN
                  STYP='D'
               ELSEIF((STEMP.EQ.'+'.OR.STEMP.EQ.'-').AND. (SLAST.EQ.'E'
     +         .OR.SLAST.EQ.'D')) THEN
                  CONTINUE
               ELSE
                  GOTO 20
               ENDIF
            ENDIF
            KLCH=I
            SLAST=STEMP
   10    CONTINUE
   20    CONTINUE
      ELSEIF(STEMP.EQ.'(')  THEN
*--- complex
         CALL SKIPLV(STRING,I1+1,I2,.FALSE.,KLCH,ILEV)
         IF(KLCH.GT.0) THEN
            CALL POSCH(',',STRING,I1+1,KLCH-1,.FALSE.,0,KPOS,ILEV)
            IF(KPOS.NE.0) STYP='K'
         ENDIF
      ENDIF
  999 END
