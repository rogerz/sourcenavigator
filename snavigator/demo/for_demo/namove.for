      SUBROUTINE NAMOVE(SLIST,K1,K2,N2)
*-----------------------------------------------------------------------
*
*   moves a set of names from one place in a list to another
*
*   Input
*   SLIST           table
*   K1              start-1 of target position
*   K2              start-1 of source position
*   N2              number of names to move
*
*   Output
*   SLIST is rearranged
*
*-----------------------------------------------------------------------
      include 'param.h'
      PARAMETER (MBUFF=200)
      CHARACTER *(MXNMCH) SLIST(*),SBUFF(MBUFF)
      N=N2
      KADD=K1
      K=K2
      NMOV=ABS(K1-K2)
   10 CONTINUE
      NT=MIN(N,MBUFF)
      DO 20 I=1,NT
         SBUFF(I)=SLIST(K+I)
   20 CONTINUE
      IF(K2.GT.K1)  THEN
         DO 30 I=K,K-NMOV+1,-1
            SLIST(NT+I)=SLIST(I)
   30    CONTINUE
         DO 40 I=1,NT
            SLIST(KADD+I)=SBUFF(I)
   40    CONTINUE
         IF(NT.LT.N) THEN
            N=N-NT
            K=K+NT
            KADD=KADD+NT
            GOTO 10
         ENDIF
      ELSEIF(K2.LT.K1)  THEN
         NMOV=NMOV-NT
         KADD=K1-NT
         DO 50 I=K2+1,K2+NMOV
            SLIST(I)=SLIST(NT+I)
   50    CONTINUE
         DO 60 I=1,NT
            SLIST(KADD+I)=SBUFF(I)
   60    CONTINUE
         IF(NT.LT.N) THEN
            N=N-NT
            NMOV=NMOV+NT
            GOTO 10
         ENDIF
      ENDIF
      END
