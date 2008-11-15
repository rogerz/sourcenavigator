      SUBROUTINE REPSUB(KREF1,KREF2,NSPEC,KSP1,KSP2,NCH)
*-----------------------------------------------------------------------
*
*   Sub-task of inserting the replacement string (for REPNAM, REPSTR)
*
*--- Input
*    KREF1         ref. to string to be replaced (cf. KKYSTA, KKYEND)
*    KREF2         ref. to replacement string
*    NSPEC         no. of special symbols in STR1
*    KSP1, KSP2    start and end of special symbol matches in STR1
*---Input/Output
*    NCH           occupation of NCH before and after replacement
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'keycom.h'
      include 'flwork.h'
      DIMENSION KSP1(*),KSP2(*)
      DIMENSION ICT(10),ICT1(10),ICT2(10),IREF1(MXNAME/20,10), IREF2
     +(MXNAME/20,10)
      EQUIVALENCE (IREF1(1,1),IWS(1)),(IREF2(1,1),IWS(MXNAME/2+1))
      CHARACTER STEMP*1
      LOGICAL SKIPFL
      include 'convex.h'
      CALL SPECCT(1,KREF1,NTOT1,ICT1,IREF1,IERR)
      CALL SPECCT(2,KREF2,NTOT2,ICT2,IREF2,IERR)
      SKIPFL=.FALSE.
      DO 10 I=1,10
         ICT(I)=0
   10 CONTINUE
      INSTR=0
      DO 30 I=KKYSTA(KREF2),KKYEND(KREF2)
         STEMP=SKYSTR(I:I)
         IF (SKIPFL) GOTO 20
         IF (STEMP.EQ.'''') INSTR=1-INSTR
         IN=INDEX(SPCHAR,STEMP)
         IF (IN.EQ.0.OR.INSTR.NE.0)  THEN
*--- normal character
            NCH=NCH+1
            IF (NCH.GT.MXLENG) GOTO 999
            SSTR(NCH:NCH)=STEMP
         ELSE
*--- count
            ICT(IN)=ICT(IN)+1
*--- get count in [...], or default
            N=IREF2(ICT(IN),IN)
            K=IREF1(N,IN)
            L=KSP2(K)-KSP1(K)+1
            IF (L.GT.0)  THEN
               IF (NCH+L.GT.MXLENG)  THEN
                  NCH=MXLENG+1
                  GOTO 999
               ENDIF
               SSTR(NCH+1:NCH+L)=SSTA(KSP1(K):KSP2(K))
               NCH=NCH+L
               SKIPFL=SKYSTR(I+1:I+1).EQ.'['
            ENDIF
         ENDIF
         GOTO 30
   20    CONTINUE
         SKIPFL=STEMP.NE.']'
   30 CONTINUE
  999 END
