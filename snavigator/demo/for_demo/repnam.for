      SUBROUTINE REPNAM
*-----------------------------------------------------------------------
*
*   Performs replacements of names, or names+strings attached
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
      NCH=0
      IPT=0
      NMOD=IMODIF(NSTREF)
*--- check for 'REP' key
      DO 10 IK=1,NGLSET
         IF (KEYREF(IK,1).EQ.9) GOTO 20
   10 CONTINUE
      GOTO 999
   20 CONTINUE
*--- check for name replacement
      IF (KEYREF(IK,4).EQ.0) GOTO 999
      DO 30 I=1,NSNAME
         CALL NAMSRC(SNAMES(ISNAME+I),SKEYLS(KEYREF(IK,5)+1),
     +   KEYREF(IK,4),IPOS,LAST)
         IF (IPOS.EQ.0) GOTO 30
         IPOS=IPOS+KEYREF(IK,5)
         KREF1=KNAMRF(IPOS,1)
*--- check illegal
         IF (KREF1.LT.0) GOTO 30
*--- name must behind last replacement
         IF (NSSTRT(I).GT.IPT)  THEN
*--- check for string following
            KPOS=0
            NSPEC=0
            IF (KREF1.GT.0)  THEN
               CALL MATCH(SKYSTR,KKYSTA(KREF1),KKYEND(KREF1),SSTA,NSEND(
     +         I)+1,NCHST,.TRUE.,KPOS,ILEV,NSPEC,KSP1,KSP2)
               IF (KPOS.EQ.0) GOTO 30
            ENDIF
*--- name (+string) do match
*--- set modify flag
            IF (NMOD.LT.10)  NMOD=NMOD+10
*--- copy from pointer up to name
            L=NSSTRT(I)-IPT-1
            IF (L.GT.0)  THEN
               IF (NCH+L.GT.MXLENG) GOTO 40
               SSTR(NCH+1:NCH+L)=SSTA(IPT+1:IPT+L)
               NCH=NCH+L
            ENDIF
            IPT=MAX(NSEND(I),KPOS)
            KREF2=KNAMRF(IPOS,2)
            IF (KREF2.GT.0)  THEN
*--- non-empty replacement string exists
               L=KKYEND(KREF2)-KKYSTA(KREF2)+1
               IF (NSPEC.EQ.0)  THEN
                  IF (NCH+L.GT.MXLENG) GOTO 40
*--- replace name by string
                  SSTR(NCH+1:NCH+L)=SKYSTR(KKYSTA(KREF2):KKYEND(KREF2))
                  NCH=NCH+L
               ELSE
                  CALL REPSUB(KREF1,KREF2,NSPEC,KSP1,KSP2,NCH)
                  IF (NCH.GT.MXLENG) GOTO 40
               ENDIF
            ENDIF
         ENDIF
   30 CONTINUE
      IF(NMOD.GE.10)  THEN
*--- copy SSTR to SSTA, NCH to NCHST
         L=NCHST-IPT
         IF (L.GT.0)  THEN
            IF (NCH+L.GT.MXLENG) GOTO 40
            SSTR(NCH+1:NCH+L)=SSTA(IPT+1:NCHST)
            NCH=NCH+L
         ENDIF
         IMODIF(NSTREF)=NMOD
         NCHST=NCH
         SSTA(:NCH)=SSTR(:NCH)
      ENDIF
      GOTO 999
   40 CONTINUE
      WRITE (MPUNIT,10000)
      CALL FLPRNT(1,'OVERFLOW',NLLINE(NSTREF)-NFLINE(NSTREF)+1, SIMA
     +(NFLINE(NSTREF)),NDUMMY)
      NSTATC(6)=NSTATC(6)+1
      STATUS(11)=.TRUE.
10000 FORMAT(/' ++++++ Warning - replacements would lead to overflow',
     +' in following statement, not done')
  999 END
