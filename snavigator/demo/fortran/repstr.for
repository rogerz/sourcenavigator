      SUBROUTINE REPSTR
*-----------------------------------------------------------------------
*
*   Performs string replacements
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'flags.h'
      include 'cursta.h'
      include 'state.h'
      include 'keycom.h'
      include 'jobsum.h'
      DIMENSION KSP1(100),KSP2(100)
      CHARACTER*1 STEMP
      NMOD=IMODIF(NSTREF)
*--- check for 'REP' key
      DO 10 IK=1,NGLSET
         IF (KEYREF(IK,1).EQ.9) GOTO 20
   10 CONTINUE
      GOTO 999
   20 CONTINUE
*--- check for string replacement
      IF (KEYREF(IK,6).EQ.0) GOTO 999
      DO 50 I=KEYREF(IK,7)+1,KEYREF(IK,7)+KEYREF(IK,6)
         NCH=0
         IPT=0
         KREF1=KSTREF(I,1)
         KREF2=KSTREF(I,2)
*--- check illegal
         IF (KREF1.LE.0) GOTO 50
         K1=KKYSTA(KREF1)
         K2=KKYEND(KREF1)
         IF (SKYSTR(K1:K1).NE.'#')  THEN
*--- insert '#' for free match
            KST=1
            K1=K1-1
            STEMP=SKYSTR(K1:K1)
            SKYSTR(K1:K1)='#'
         ELSE
            KST=0
         ENDIF
   30    CONTINUE
         CALL MATCH(SKYSTR,K1,K2,SSTA,IPT+1,NCHST,.TRUE.,KPOS,ILEV,NSPEC
     +   ,KSP1,KSP2)
         IF (KPOS.EQ.0) GOTO 40
*--- string does match
*--- set modify flag
         IF (NMOD.LT.10) NMOD=NMOD+10
*--- transfer additional '#' if there
         IF (KST.NE.0)  THEN
            L=KSP2(1)-IPT
            IF (L.GT.0)  THEN
               SSTR(NCH+1:NCH+L)=SSTA(IPT+1:IPT+L)
               NCH=NCH+L
            ENDIF
         ENDIF
         IPT=KPOS
         IF (KREF2.GT.0)  THEN
*--- non-empty replacement string exists
            CALL REPSUB(KREF1,KREF2,NSPEC-KST,KSP1(KST+1),KSP2(KST+1),
     +      NCH)
            IF (NCH.GT.MXLENG) GOTO 60
         ENDIF
         IF (IPT.LT.NCHST) GOTO 30
   40    CONTINUE
         IF (KST.NE.0) SKYSTR(K1:K1)=STEMP
         IF (IPT.NE.0)  THEN
*--- copy SSTR to SSTA, NCH to NCHST
            L=NCHST-IPT
            IF (L.GT.0)  THEN
               IF (NCH+L.GT.MXLENG) GOTO 60
               SSTR(NCH+1:NCH+L)=SSTA(IPT+1:NCHST)
               NCH=NCH+L
            ENDIF
            NCHST=NCH
            SSTA(:NCH)=SSTR(:NCH)
         ENDIF
   50 CONTINUE
      IMODIF(NSTREF)=NMOD
      GOTO 999
   60 CONTINUE
      WRITE (MPUNIT,10000)
      CALL FLPRNT(1,'OVERFLOW',NLLINE(NSTREF)-NFLINE(NSTREF)+1, SIMA
     +(NFLINE(NSTREF)),NDUMMY)
      NSTATC(6)=NSTATC(6)+1
      STATUS(11)=.TRUE.
10000 FORMAT(/' ++++++ Warning - replacements would lead to overflow',
     +' in following statement, not done')
  999 END
