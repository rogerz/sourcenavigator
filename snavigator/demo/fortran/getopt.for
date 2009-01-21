      SUBROUTINE GETOPT(SLINE,NLEN,SOPT,LOPT,IERR)
C find if character string SLINE is a recognised operator, and if so
C return that operator (minus any blanks) in SOPT. The operator does
C not need to necessarily fill the whole of SLINE.
      PARAMETER (NOPER=22,LTEMP=100)
      CHARACTER*(*) SLINE
      CHARACTER*(LTEMP) STEMP,touppr
      CHARACTER*6 SOPER(NOPER),SOPT
      INTEGER LOPER(NOPER)
      external touppr
C all possible operators
      DATA SOPER /'=     ','(     ',')     ',',     ',':     ',
     &            '.EQV. ','.NEQV.','.OR.  ','.AND. ','.NOT. ',
     &            '.GT.  ','.GE.  ','.LT.  ','.LE.  ','.EQ.  ',
     &            '.NE.  ','//    ','+     ','-     ','**    ',
     &            '/     ','*     '/
      DATA LOPER /1,1,1,1,1,5,6,4,5,5,4,4,4,4,4,4,2,1,1,2,1,1/
      NC = 0
C loop over characters in the line segment and remove blanks
      DO 10 I=1,NLEN
        IF(SLINE(I:I).EQ.' ') GOTO 10
        NC = NC + 1
        STEMP(NC:NC) = SLINE(I:I)
   10 CONTINUE
      IF(NC.EQ.0.OR.NC.GT.LTEMP) GOTO 900
      stemp = touppr(stemp)
C find the operator. Note that ** is found correctly due to its order
C in the SOPER list. Similarly for //
      DO 20 I=1,NOPER
        IF(LOPER(I).GT.NC) GOTO 20
        IF(STEMP(:LOPER(I)).NE.SOPER(I)(:LOPER(I))) GOTO 20
        SOPT(:LOPER(I)) = SOPER(I)(:LOPER(I))
        LOPT = LOPER(I)
        IERR = 0
        RETURN
   20 CONTINUE
  900 IERR = 1
      RETURN
      END
