      SUBROUTINE PROIND
*-----------------------------------------------------------------------
*
*   Prepares indentation by updating current DO and IF levels
*   Modified by JJB for ENDDO
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'class.h'
      include 'cursta.h'
      include 'state.h'
      DIMENSION IDO(100)
      SAVE IDO
*--- get external class number
      ICLEXT=ISTMDS(6,ICURCL(1))
      IF(ICLEXT.EQ.33)  THEN
*--- FORMAT, do not indent
         INDCNT=0
         GOTO 999
      ELSE
         INDCNT=KNTDO+KNTIF
      ENDIF
      IF(ICLEXT.EQ.39)  THEN
*--- IF...THEN
         KNTIF=KNTIF+1
      ELSEIF(ICLEXT.EQ.23.OR.ICLEXT.EQ.24)  THEN
*--- ELSE or ELSEIF
         INDCNT=INDCNT-1
      ELSEIF(ICLEXT.EQ.27)  THEN
*--- ENDIF
         KNTIF=KNTIF-1
         INDCNT=INDCNT-1
      ELSEIF(ICLEXT.EQ.20)  THEN
*--- DO loop
         IF (KNTDO.LT.100)  THEN
            KNTDO=KNTDO+1
            CALL GETINT(SSTA,1,NCHST,KFCH,KLCH,NN)
            IDO(KNTDO)=NN
         ENDIF
      elseif(iclext.eq.71) then
*--- ENDDO
         kntdo=kntdo-1
         indcnt=indcnt-1
      ELSEIF(KNTDO.GT.0)  THEN
*--- check for (possibly multiple) end of DO loop
         K=NEXTIN(SIMA(NFLINE(NSTREF)),1,5)
         KST=KNTDO
         DO 10 I=KST,1,-1
            IF (IDO(I).NE.K) GOTO 20
            KNTDO=KNTDO-1
            INDCNT=INDCNT-1
   10    CONTINUE
   20    CONTINUE
      ENDIF
      INDCNT=MAX(0,INDCNT)
  999 END
