      SUBROUTINE GETNBL(STRING,SNBLK,NN)
*-----------------------------------------------------------------------
*
*--- extracts non-blank characters
*--- input
*    STRING     input string - full length taken
*--- output
*    SNBLK      string of non-blank (to max. length)
*    NN         # of non-blank put in SNBLK
*-----------------------------------------------------------------------
      CHARACTER *(*) STRING,SNBLK,STEMP*1
      LUP=LEN(SNBLK)
      NN=0
      DO 10 I=1,LEN(STRING)
         STEMP=STRING(I:I)
         IF (STEMP.EQ.' ') GOTO 10
         IF (NN.EQ.LUP) GOTO 999
         NN=NN+1
         SNBLK(NN:NN)=STEMP
   10 CONTINUE
  999 END
