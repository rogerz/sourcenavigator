      SUBROUTINE NXITEM(STRING,ICC1,ICC2,LAST)
*-----------------------------------------------------------------------
*
*   Cuts statement into pieces, one part at a time (called by REFORM)
*
*---Input
*     STRING                string to be chopped
*     ICC1                  starting position for next piece
*     ICC2                  string size
*     LAST                  last ch. position of piece
*
*-----------------------------------------------------------------------
      CHARACTER STRING*(*), STEMP*1
      LOGICAL NUFL,DEFL
      include 'convex.h'
*--- max. length for inclusive brackets
      MAXL=12
*--- skip leading blanks
      DO 10 IC1=ICC1,ICC2
         IF (STRING(IC1:IC1).NE.' ') GOTO 20
   10 CONTINUE
      LAST=ICC2
      GOTO 999
   20 CONTINUE
      NSTST=INDEX(STRING(IC1:ICC2),'{')-1
      IF(NSTST.GT.0)  THEN
*--- always stop before start of next string
         IC2=IC1+NSTST-1
      ELSE
         IC2=ICC2
      ENDIF
      IF(STRING(IC1:IC1).EQ.'{')  THEN
*--- get string
         IN=IC1+INDEX(STRING(IC1+1:IC2),'}')
         IF (IN.EQ.IC1.OR.IN.EQ.IC2)  THEN
            LAST=IC2
         ELSE
            LAST=IN
         ENDIF
         GOTO 999
      ELSE
*--- no string
         DO 30 I=IC1,IC2
            STEMP=STRING(I:I)
            IF (INDEX(':(=*/+-',STEMP).EQ.0)GOTO 40
            LAST=I
            IF (STEMP.EQ.'(') THEN
               CALL SKIPLV(STRING,I+1,IC2,.FALSE.,KPOS,ILEV)
               IF (KPOS.GT.0.AND.KPOS-ICC1.LT.MAXL)  THEN
                  LAST=KPOS
                  GOTO 90
               ENDIF
            ENDIF
   30    CONTINUE
         GOTO 999
   40    CONTINUE
         IF (I.EQ.IC1) LAST=I
         IF (STEMP.EQ.'.')  THEN
*--- look for relational symbol
            CALL POSCH('.',STRING,I+1,IC2,.FALSE.,9999,KPOS,ILEV)
            IF (KPOS.EQ.0) GOTO 999
            DO 50 J=I+1,KPOS-1
               IF (.NOT.ALPHCH(STRING(J:J))) GOTO 999
   50       CONTINUE
            LAST=KPOS
         ELSEIF (ANUMCH(STEMP))  THEN
            NUFL=NUMCH(STEMP)
            DEFL=.FALSE.
            DO 70 J=I,IC2
               STEMP=STRING(J:J)
               IF (STEMP.EQ.' '.OR.NUMCH(STEMP)) GOTO 60
               IF (.NOT.NUFL.AND.ALPHCH(STEMP)) GOTO 60
               IF (NUFL.AND.STEMP.EQ.'.') GOTO 60
               IF (DEFL.AND.(STEMP.EQ.'+'.OR.STEMP.EQ.'-')) GOTO 60
               DEFL=STEMP.EQ.'D'.OR.STEMP.EQ.'E'
               NUFL=.FALSE.
               IF (DEFL) GOTO 60
               GOTO 80
   60          LAST=J
   70       CONTINUE
            GOTO 999
   80       CONTINUE
            IF (STEMP.EQ.')') LAST=J
         ENDIF
      ENDIF
   90 CONTINUE
      IF(LAST.LT.IC2.AND.STRING(LAST+1:LAST+1).EQ.',')  THEN
         LAST=LAST+1
      ELSEIF(LAST+1.LT.IC2.AND.STRING(LAST+1:LAST+1).EQ.' ' .AND.STRING
     +(LAST+2:LAST+2).EQ.',') THEN
         LAST=LAST+2
      ENDIF
  999 END
