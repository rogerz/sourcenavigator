      SUBROUTINE LMERGE(SLIST,NACC,FLACC,IS,N1,N2)
*-----------------------------------------------------------------------
*
*--- merges two successive, alphabetically sorted lists
*    in SLIST in place, updates NACC
*
*--- input
*    SLIST     list containing all names
*    NACC      array to be re-arranged with sort
*    FLACC     if true, NACC is actually updated
*    IS        start-1 of first list in IS
*    N1        length of first list
*    N2        length of second list
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'flwork.h'
      CHARACTER *(MXNMCH)  SLIST(*)
      DIMENSION NACC(*)
      LOGICAL FLACC
      KADD=0
      K2=N1
      DO 20 I=1,N1
         II=I
   10    IF (K2.EQ.N1+N2) GOTO 40
         IF (SLIST(IS+I).GT.SLIST(IS+K2+1))  THEN
            K2=K2+1
            IWS(K2)=I+KADD
            KADD=KADD+1
            GOTO 10
         ELSE
            IWS(I)=I+KADD
         ENDIF
   20 CONTINUE
      DO 30 I=K2+1,N1+N2
   30 IWS(I)=I
      GOTO 60
   40 CONTINUE
      DO 50 I=II,N1
   50 IWS(I)=I+KADD
   60 CONTINUE
*
*--- put in place
*
      CALL SHUFFL(SLIST,NACC,FLACC,IS,N1+N2)
      END
