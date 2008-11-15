      SUBROUTINE RENUMB
*-----------------------------------------------------------------------
*
*   Processes one routine statement by statement:
*   filtering, replacements
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'class.h'
      include 'flags.h'
      include 'cursta.h'
      include 'state.h'
      include 'jobsum.h'
      include 'flwork.h'
      include 'condec.h'
      DIMENSION IKL(3)
*   IKL(1) = last ch. of 'FMT=' or 0
*   IKL(2) =             'ERR='
*   IKL(3) =             'END='
      LOGICAL FMTFL
*--- FMTFL true when 'FMT=' found
      CHARACTER SKL(3)*5,STEMP*1,STEMP3*3, SBUFF*5
 
      DATA SKL/'#FMT=','#ERR=','#END='/
      include 'condat.h'
*--- if no statement numbers, return
      IF (NSTANU.EQ.0) GOTO 999
*--- replace statement number if any
      NN=NEXTIN(SIMA(NFLINE(NSTREF)),1,5)
      IF (NN.NE.0)  THEN
*--- get number from table
         IF (NSTANU.LE.40)  THEN
            DO 10 J=1,NSTANU
               IF (NN.EQ.KSTANU(J)) GOTO 20
   10       CONTINUE
            J=0
   20       CONTINUE
         ELSE
            CALL BINSRC(NN,KSTANU,NSTANU,J,L)
         ENDIF
         IF (J.GT.0)  THEN
            NN=KSTARE(J)
         ELSE
            NN=0
         ENDIF
         IF(NN.GT.0)  THEN
            IF (IMODIF(NSTREF).LT.10) IMODIF(NSTREF)=IMODIF(NSTREF)+10
            WRITE (SNEWST(1),'(I5)') NN
         ENDIF
      ENDIF
      NMOD=IMODIF(NSTREF)
      ICL=ICURCL(1)
      IF(ICL.EQ.IIF.or.icl.eq.iif+71)  THEN
*--- get class of second part
         ICL=ICURCL(2)
*--- ISTIND specifies tpyes
         ISTIND=ISTMDS(10,ICL)
         IF (ISTIND.EQ.0) GOTO 999
*--- set pointer after first bracket
         IPT=INDEX(SSTA(:NCHST),'(')
         IF (IPT.EQ.0) GOTO 999
         CALL SKIPLV(SSTA,IPT+1,NCHST,.FALSE.,IPT,ILEV)
         IF (IPT.EQ.0) GOTO 999
         SSTR(:IPT)=SSTA(:IPT)
         IPS=IPT
      ELSE
         ISTIND=ISTMDS(10,ICL)
         IF (ISTIND.EQ.0) GOTO 999
         IPT=0
         IPS=0
*--- IPS = pointer in new string SSTR, IPT in old SSTA
      ENDIF
*--- now IPT in front of statement
*
*--- treat the five different cases
      IF(ISTIND.EQ.1)  THEN
*--- one number, directly behind key
         KFCH=0
         CALL SKIPTP(2,SSTA,IPT+1,NCHST,.FALSE.,IPTT,ILEV)
         IF(IPTT.EQ.0.OR.IPTT.EQ.NCHST) GOTO 60
         STEMP=SSTA(IPTT+1:IPTT+1)
         IF(STEMP.EQ.' '.AND.IPTT+2.LE.NCHST)  STEMP=SSTA(IPTT+2:IPTT+2)
         IF(NUMCH(STEMP))  THEN
            CALL GETINT(SSTA,IPT+1,NCHST,KFCH,KLCH,NN)
         ENDIF
         GOTO 60
      ENDIF
*--- for all other cases, find bracket
      LL=INDEX(SSTA(IPT+1:NCHST),'(')+IPT
      IF (LL.EQ.IPT) GOTO 999
      CALL SKIPLV(SSTA,LL+1,NCHST,.FALSE.,LR,ILEV)
      IF (LR.EQ.0) GOTO 999
*--- first bracket between LL and LR
*--- look for 'FMT=' etc.
      DO 30 I=1,3
         CALL MATCH(SKL(I),1,5,SSTA,LL,LR,.FALSE.,IKL(I),ILEV,NSPEC,IWS,
     +   IWS)
         IF (I.EQ.1) FMTFL=IKL(1).GT.0
   30 CONTINUE
      N=0
*--- count and order
      DO 40 I=1,3
         IF (IKL(I).GT.0)  THEN
            N=N+1
            IKL(N)=IKL(I)
         ENDIF
   40 CONTINUE
      IF (N.GT.1)  THEN
         CALL SORTSP(N,IKL,NSPEC)
      ELSE
         NSPEC=N
      ENDIF
*--- NFL is a flag for different passes
      NFL=0
   50 CONTINUE
      KFCH=0
      IF (ISTIND.EQ.2)  THEN
*--- all numbers inside first bracket
         CALL GETINT(SSTA,LL+1,LR,KFCH,KLCH,NN)
         LL=KLCH
      ELSEIF (ISTIND.EQ.3)  THEN
*--- all numbers follow first bracket
         CALL GETINT(SSTA,LR+1,NCHST,KFCH,KLCH,NN)
         LR=KLCH
      ELSEIF (ISTIND.EQ.4)  THEN
*--- inside first bracket 'FMT=' etc.,
*   or if no 'FMT=', second item
         IF (.NOT.FMTFL)  THEN
            CALL POSCH(',',SSTA,LL+1,LR-1,.FALSE.,0,IPOS,ILEV)
            IF(IPOS.EQ.0) GOTO 999
            CALL GETNBL(SSTA(IPOS+1:LR),STEMP,N)
            IF(N.GT.0.AND.NUMCH(STEMP))  THEN
               CALL GETINT(SSTA,IPOS+1,LR,KFCH,KLCH,NN)
               LL=KLCH
            ENDIF
         ELSE
            NFL=NFL+1
            IF (NFL.LE.NSPEC)  THEN
               CALL GETNBL(SSTA(IKL(NFL)+1:LR),STEMP,N)
               IF(N.GT.0.AND.NUMCH(STEMP))  THEN
                  CALL GETINT(SSTA,IKL(NFL)+1,LR,KFCH,KLCH,NN)
                  LL=KLCH
               ENDIF
            ENDIF
         ENDIF
      ELSEIF (ISTIND.EQ.5)  THEN
*--- alternate returns, '(*' or ',*'
         IF (NFL.EQ.0)  THEN
            STEMP3='#(*'
         ELSE
            STEMP3='#,*'
         ENDIF
         NFL=NFL+1
         CALL MATCH(STEMP3,1,3,SSTA,LL,LR,.FALSE.,KPOS,ILEV,NSPEC,IWS,
     +   IWS)
         IF (KPOS.GT.0)  THEN
            LL=KPOS
            CALL GETINT(SSTA,LL+1,LR,KFCH,KLCH,NN)
            LL=KLCH
         ENDIF
      ENDIF
   60 CONTINUE
*--- if KFCH > 0, number found
      IF (KFCH.GT.0)  THEN
         IF (NMOD.LT.10) NMOD=NMOD+10
*--- transmit part up to pointer
         N=KFCH-IPT-1
         IF (N.GT.0)  THEN
            SSTR(IPS+1:IPS+N)=SSTA(IPT+1:IPT+N)
            IPS=IPS+N
            IPT=KLCH
         ENDIF
*--- get number from table
         IF (NSTANU.LE.40)  THEN
            DO 70 J=1,NSTANU
               IF (NN.EQ.KSTANU(J)) GOTO 80
   70       CONTINUE
            J=0
   80       CONTINUE
         ELSE
            CALL BINSRC(NN,KSTANU,NSTANU,J,L)
         ENDIF
         IF (J.GT.0)  THEN
            NN=KSTARE(J)
         ELSE
            NN=0
         ENDIF
*--- add to SSTR
         WRITE (SBUFF,'(I5)') NN
         DO 90 J=1,5
            STEMP=SBUFF(J:J)
            IF (STEMP.NE.' ')  THEN
               IPS=IPS+1
               SSTR(IPS:IPS)=STEMP
            ENDIF
   90    CONTINUE
         FMTFL=.TRUE.
         IF (ISTIND.GT.1) GOTO 50
      ENDIF
      IF (NFL.EQ.1.AND.ISTIND.EQ.5) GOTO 50
      IF (.NOT.FMTFL.AND.ISTIND.EQ.4)  THEN
         FMTFL=.TRUE.
         GOTO 50
      ENDIF
*--- transfer rest
      N=NCHST-IPT
      IF (N.GT.0)  THEN
         SSTR(IPS+1:IPS+N)=SSTA(IPT+1:NCHST)
         IPS=IPS+N
      ENDIF
      IF (NMOD.GT.10)  THEN
         IF (IPS.LE.MXLENG)  THEN
            IMODIF(NSTREF)=NMOD
            NCHST=IPS
            SSTA(:IPS)=SSTR(:IPS)
         ELSE
            WRITE (MPUNIT,10000)
            CALL FLPRNT(1,'OVERFLOW',NLLINE(NSTREF)-NFLINE(NSTREF)+1,
     +      SIMA(NFLINE(NSTREF)),NDUMMY)
            NSTATC(6)=NSTATC(6)+1
            STATUS(11)=.TRUE.
         ENDIF
      ENDIF
10000 FORMAT(/' ++++++ Warning - renumbering would lead to overflow',
     +' in following statement, not done')
  999 END
