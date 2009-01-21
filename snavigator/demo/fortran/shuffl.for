      SUBROUTINE SHUFFL(SLIST,NACC,FLACC,IS,NS)
*-----------------------------------------------------------------------
*
*--- puts the names in a list in the order given in an array.
*    Updates NACC.
*
*--- input
*    SLIST     list containing all names
*    NACC      array to be re-arranged with sort
*    FLACC     if true, NACC is actually updated
*    IS         start-1 of list in SLIST
*    NS         # of elements
*    IWS        array containing for element I its target place L,
*                /FLWORK/
* ++++++++ warning +++++++++++  IWS is destroyed +++++++++++++++
*-----------------------------------------------------------------------
      include 'param.h'
      include 'flwork.h'
      CHARACTER *(MXNMCH) SLIST(*), SW(2)
      DIMENSION KEEP(2),NACC(*)
      LOGICAL STD,FLACC
      K=1
      I=1
   10 STD=.TRUE.
   20 CONTINUE
      L=IWS(I)
      IF(L.EQ.I)  THEN
         IWS(I)=0
         I=I+1
         IF (I.LE.NS) GOTO 10
      ELSEIF(L.GT.0)  THEN
         IF (STD)  THEN
            SW(K)=SLIST(IS+I)
            IF(FLACC)  KEEP(K)=NACC(IS+I)
            STD=.FALSE.
         ENDIF
         SW(3-K)=SLIST(IS+L)
         IF(FLACC)  KEEP(3-K)=NACC(IS+L)
         SLIST(IS+L)=SW(K)
         IF(FLACC)  NACC(IS+L)=KEEP(K)
         K=3-K
         IWS(I)=0
         I=L
         GOTO 20
      ELSE
*--- look for new non-zero element to start with
         DO 30 I=1,NS
            IF (IWS(I).GT.0) GOTO 10
   30    CONTINUE
      ENDIF
      END
