      SUBROUTINE INDECZ(ISTR1,ISTR2)
*-----------------------------------------------------------------------
*
*  Checks consistency between replacement strings, kills illegal ones
*
*--- Input
*    ISTR1        ref. to string to be replaced (rel. to KKYSTA, KKYEND)
*    ISTR2        ref. to replacing string
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'keycom.h'
      include 'flwork.h'
      include 'condec.h'
      DIMENSION ICT1(10),ICT2(10),IREF1(MXNAME/20,10), IREF2(MXNAME/20,
     +10)
      EQUIVALENCE (IREF1(1,1),IWS(1)),(IREF2(1,1),IWS(MXNAME/2+1))
      CHARACTER *40 STEXT(4)
      DATA STEXT/'too many special symbols', 'unclosed [...] in string',
     +'replacement count [n] too high',
     +'unclosed quote string inside string'/
 
      include 'condat.h'
      IF(ISTR1.GT.0.AND.ISTR2.GT.0)  THEN
*--- extract special symbols from first string
         CALL SPECCT(1,ISTR1,NTOT1,ICT1,IREF1,IERR)
         IF (IERR.NE.0) GOTO 30
*--- second string
         CALL SPECCT(2,ISTR2,NTOT2,ICT2,IREF2,IERR)
         IF (IERR.NE.0) GOTO 30
         IF (NTOT2.GT.0)  THEN
*--- there are special symbols in the replacement string -
*    check that no count in [...] higher than actually present
            DO 20 I=1,LEN(SPCHAR)
               DO 10 J=1,ICT2(I)
                  IF (ICT1(I).LT.IREF2(J,I))  THEN
                     IERR=3
                     GOTO 30
                  ENDIF
   10          CONTINUE
   20       CONTINUE
         ENDIF
      ENDIF
      GOTO 999
   30 CONTINUE
*--- error condition - suppress string (or name+string) replacement
      WRITE (MPUNIT,10000) STEXT(IERR)
      I1=KKYSTA(ISTR1)-1
      I2=KKYEND(ISTR1)
      L=(I2-I1-1)/MXLINE+1
      DO 40 I=1,L
         SIMA(I)=SKYSTR(I1+1:MIN(I2,I1+MXLINE))
         I1=I1+MXLINE
   40 CONTINUE
      CALL FLPRNT(0,'replace',L,SIMA,I1)
      I1=KKYSTA(ISTR2)-1
      I2=KKYEND(ISTR2)
      L=(I2-I1-1)/MXLINE+1
      DO 50 I=1,L
         SIMA(I)=SKYSTR(I1+1:MIN(I2,I1+MXLINE))
         I1=I1+MXLINE
   50 CONTINUE
      CALL FLPRNT(0,'by string',L,SIMA,I1)
      ISTR1=-IERR
10000 FORMAT(/' +++++++ WARNING - ',A,' in following replacement ',
     +'request, request ignored')
  999 END
