      SUBROUTINE REFORM
*-----------------------------------------------------------------------
*
*   Re-formats the statement after a change.
*   Modified by JJB to take account of inline comments
*
*---Input
*     SSTA, NCHST, JPLINK
*--- Output
*     SIMA, and NFLINE, NLLINE, NLINES possibly updated.
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'flags.h'
      include 'cursta.h'
      include 'state.h'
      include 'jobsum.h'
      include 'flwork.h'
      include 'class.h'
*--- RETRY flag for second attempt without indentation if overflow
      LOGICAL RETRY
*--- IUPPER = line fill of SIMA, max. indented statement starts in
*    IMAX+7
      DATA IUPPER/72/, IMAX/30/
      RETRY=.TRUE.
      NMOD=IMODIF(NSTREF)
      I1=NFLINE(NSTREF)
   10 CONTINUE
*--- start of complete statement reformatting
      IF(RETRY)  THEN
*--- get user indentation
         imaxp = imax+6
         if(jplink(i1).ne.0) imaxp = min(7,imaxp,jplink(i1)-1)
         INU=NLBLPS(SIMA(I1),7,imaxp)
         IF(ACTION(21))  THEN
*--- indent corresponding to level (from PROIND)
            INB=6+MIN(IMAX,INDFAC*INDCNT)
*--- return if not modified and indentation correct
            IF (IMODIF(NSTREF).LT.10.AND.INU.EQ.INB) THEN
               DO 20 I=I1+1,NLLINE(NSTREF)
                  IF(NLTYPE(I).EQ.2)  THEN
                     IF(NLBLPS(SIMA(I),7,imaxp).NE.INU) GOTO 30
                  ENDIF
   20          CONTINUE
               GOTO 999
            ENDIF
   30       CONTINUE
            IF(NMOD.LT.10)  NMOD=NMOD+10
         ELSE
            INB=INU
         ENDIF
      ELSE
*--- second pass - try without indentation
         INB=6
      ENDIF
      NEWOUT=0
      INSTR=0
      INTRA=0
      IPTRA=0
      LTRA=0
      LAST=0
   40 CONTINUE
      NEWOUT=NEWOUT+1
*--- start of a new line  (statement number pre-set in PROCES or RENUMB)
      IF(NEWOUT.EQ.20)  THEN
         IF (RETRY)  THEN
            RETRY=.FALSE.
            GOTO 10
         ELSE
            WRITE (MPUNIT,10000)
            CALL FLPRNT(1,'OVERFLOW',NLLINE(NSTREF)-NFLINE(NSTREF)+1,
     +      SIMA(NFLINE(NSTREF)),NDUMMY)
            NSTATC(6)=NSTATC(6)+1
            STATUS(11)=.TRUE.
            GOTO 999
         ENDIF
      ELSEIF(NEWOUT.GT.1)  THEN
         IF(INSTR.GE.0)  THEN
            SNEWST(NEWOUT)(1:6)='     +'
         ELSE
*--- split statement into several at '<'
            SNEWST(NEWOUT)(1:6)='      '
            INSTR=0
         ENDIF
      ENDIF
*--- clear rest of statement
      SNEWST(NEWOUT)(7:MXLINE)=' '
      IF(INSTR.EQ.0)  THEN
*--- outside string
         IPS=INB
      ELSE
         IPS=6
      ENDIF
      IF(IPTRA.LT.LTRA)  THEN
*--- add those items already prepared by call to NXITEM
         L=MIN(IUPPER-IPS,LTRA-IPTRA)
         SNEWST(NEWOUT)(IPS+1:IPS+L)=SSTA(IPTRA+1:IPTRA+L)
         IPTRA=IPTRA+L
         IPS=IPS+L
         IF (IPTRA.LT.LTRA) GOTO 40
      ENDIF
      INSTR=0
      IPT=LAST
   50 CONTINUE
      IF (IPT.EQ.NCHST) GOTO 60
*--- chop into nice little pieces
      CALL NXITEM(SSTA,IPT+1,NCHST,LAST)
      IF(SSTA(IPT+1:IPT+1).EQ.' ')  THEN
         IF (IPS.LT.IUPPER) IPS=IPS+1
         IPT=IPT+1
         IF (IPT.EQ.NCHST) GOTO 60
      ENDIF
      IF(SSTA(IPT+1:IPT+1).EQ.'{')  THEN
         IPTRA=IPT+1
      ELSEIF(SSTA(IPT+1:IPT+1).EQ.'<')  THEN
*--- split statement into several
         IPTRA=LTRA
         INSTR=-1
         GOTO 40
      ELSE
         IPTRA=IPT
      ENDIF
      IF(SSTA(LAST:LAST).EQ.'}')  THEN
         LTRA=LAST-1
      ELSE
         LTRA=LAST
      ENDIF
      L=LTRA-IPTRA
      IF(L.LE.0)  THEN
         IPT=LAST
         GOTO 50
      ENDIF
      IF(L.LE.IUPPER-IPS)  THEN
         SNEWST(NEWOUT)(IPS+1:IPS+L)=SSTA(IPTRA+1:LTRA)
         IPS=IPS+L
         IPT=LAST
         GOTO 50
      ELSE
         IF (L.GT.IUPPER-INB)  THEN
*--- split
            SNEWST(NEWOUT)(IPS+1:IUPPER)=SSTA(IPTRA+1:)
            INSTR=1
            IPTRA=IPTRA+IUPPER-IPS
         ELSE
            INSTR=0
         ENDIF
*--- start a new line
         GOTO 40
      ENDIF
   60 CONTINUE
      IF(ACTION(28))  THEN
*--- right-adjust GOTO statements
         IF(ICURCL(1).EQ.IIF.or.icurcl(1).eq.iif+71) THEN
            ICLE=ISTMDS(6,ICURCL(2))
         ELSE
            ICLE=ISTMDS(6,ICURCL(1))
         ENDIF
         IF(ICLE.EQ.37) THEN
            CALL MATCH('#GOTO@;',1,7,SNEWST(NEWOUT),7,
     +      LASTNB(SNEWST(NEWOUT),7,72), .FALSE.,KPOS,ILEV,NSPEC,
     +      IWS,IWS(1001))
            IF(KPOS.GT.0.AND.KPOS.LT.72) THEN
               DO 70 I=72,7,-1
                  SNEWST(NEWOUT)(I:I)=SNEWST(NEWOUT)(KPOS:KPOS)
                  IF(SNEWST(NEWOUT)(I:I).EQ.'G') GOTO 80
                  KPOS=KPOS-1
   70          CONTINUE
   80          CONTINUE
               SNEWST(NEWOUT)(KPOS:I-1)=' '
            ENDIF
            CALL MATCH('#goto@;',1,7,SNEWST(NEWOUT),7,
     +      LASTNB(SNEWST(NEWOUT),7,72), .FALSE.,KPOS,ILEV,NSPEC,
     +      IWS,IWS(1001))
            IF(KPOS.GT.0.AND.KPOS.LT.72) THEN
               DO 71 I=72,7,-1
                  SNEWST(NEWOUT)(I:I)=SNEWST(NEWOUT)(KPOS:KPOS)
                  IF(SNEWST(NEWOUT)(I:I).EQ.'g') GOTO 81
                  KPOS=KPOS-1
   71          CONTINUE
   81          CONTINUE
               SNEWST(NEWOUT)(KPOS:I-1)=' '
            ENDIF
         ENDIF
      ENDIF
      IMODIF(NSTREF)=NMOD
c
c re-insert any in-line comments that were in the original source lines
c (they will go either in the same column, or to the immediate right of
c of the new line)
c   
      nfl = nfline(nstref)
      do 82 is=nfl,nlline(nstref)
         inout = is-nfl+1
         if(inout.gt.newout) goto 82
         if(jplink(is).eq.0) goto 82
         iposo = max(1+lenocc(snewst(inout)),jplink(is))
         lencom = lenocc(sima(is))-jplink(is)
         snewst(inout)(iposo:iposo+lencom) = sima(is)(jplink(is):)
   82 continue
*--- re-formatted statement now in SNEWST
10000 FORMAT(/' +++++++++ WARNING - re-formatting leads to overflow,
     + statement not changed:')
  999 END
