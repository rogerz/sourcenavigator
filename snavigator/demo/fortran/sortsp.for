      SUBROUTINE SORTSP(N1,IARR,N2)
*-----------------------------------------------------------------------
*  Sorts integers, suppresses multiple occurrences
*  Input
*  N1       = no. of integers
*  Input/Output
*  IARR     = array containing integers
*  Output
*  N2       = new number of integers
*-----------------------------------------------------------------------
      DIMENSION IARR(*)
   10 CONTINUE
      IND=0
      DO 20 J=2,N1
         IF (IARR(J).LT.IARR(J-1))  THEN
            K=IARR(J)
            IARR(J)=IARR(J-1)
            IARR(J-1)=K
            IND=1
         ENDIF
   20 CONTINUE
      IF (IND.NE.0) GOTO 10
      N2=MIN(N1,1)
      DO 30 J=2,N1
         IF (IARR(J).GT.IARR(J-1))  THEN
            N2=N2+1
            IARR(N2)=IARR(J)
         ENDIF
   30 CONTINUE
      END
