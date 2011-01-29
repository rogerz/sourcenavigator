      SUBROUTINE GETALL
*-----------------------------------------------------------------------
*
*--- gets all names in one statement
*    modified by JJB for mixed case Fortran
*
*--- input
*    SSTA      statement in /ALCAZA/
*    ICURCL etc. from /CURSTA/
*--- output
*    NSNAME    no. of names /STATE/
*    SNAMES(ISNAME+1)...SNAMES(ISNAME+NSNAME)  /ALCAZA/ = names
*    NSSTRT, NSEND   /STATE/ = start and end of each name in SSTA
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'class.h'
      include 'flags.h'
      include 'cursta.h'
      include 'state.h'
      include 'flwork.h'
      CHARACTER STEMP*1,ssta_t*(mdimst),touppr*(mdimst)
      external touppr
      NSNAME=0
      ssta_t(:nchst) = touppr(ssta(:nchst))
      IF(ICURCL(1).EQ.ILL.or.icurcl(1).eq.ill+71)  THEN
         IUP=0
      ELSEIF(ICURCL(1).EQ.IIF.or.icurcl(1).eq.iif+71)  THEN
         IUP=2
*--- find end of IF(...)
         JPT=INDEX(SSTA_t(:NCHST),'(')
         CALL SKIPLV(SSTA_t,JPT+1,NCHST,.FALSE.,KND,ILEV)
      ELSE
         IUP=1
         KND=NCHST
      ENDIF
      DO 30 IPART=1,IUP
         IF (IPART.EQ.1)  THEN
            ILOC=ICURCL(1)
            KST=1
         ELSE
            ILOC=ICURCL(2)
            KST=KND+1
            KND=NCHST
         ENDIF
         IF (ISTMDS(12,ILOC).NE.0)  THEN
*--- this part of the statement may contain names
*    prepare key match necessary for name scan
            IK=ISTMDS(8,ILOC)
            IF (IK.EQ.0)  THEN
               KMT=KST-1
            ELSEIF (IK.EQ.99)  THEN
               CALL MATCH(SSTM,ISTMDS(3,ILOC),ISTMDS(4,ILOC),
     +         SSTA_t,KST,
     +         NCHST,.FALSE.,KMT,ILEV,NDUMMY,IWS,IWS)
            ELSE
               CALL MATCH(SSTM,ISTMDS(3,ILOC),ISTMDS(3,ILOC)+IK-1,
     +         SSTA_t,KST,NCHST,.FALSE.,KMT,ILEV,NDUMMY,IWS,IWS)
            ENDIF
            IF (MOD(ISTMDS(13,ILOC),2).NE.0)  THEN
*--- there are special keys like in READ(UNIT=..,  ) etc.
               I=INDEX(SSTA_t(KST:KND),'(')+KST
               CALL SKIPLV(SSTA_t,I,KND,.FALSE.,JRBPOS,ILEV)
            ELSE
               JRBPOS=0
            ENDIF
*--- set start and end of scan for names
            K1=KMT+1
*--- remove trailing key (THEN)
            NTRAIL=0
            DO 10 K2=KND,KST,-1
               IF(SSTA_t(K2:K2).NE.' ') THEN
                  NTRAIL=NTRAIL+1
                  IF(NTRAIL.GT.ISTMDS(9,ILOC)) GOTO 20
               ENDIF
   10       CONTINUE
   20       CONTINUE
*--- start of name search loop
*--- (note special treatment for 'INCLUDE')
            if(sstm(istmds(3,iloc):istmds(4,iloc)).ne.'INCLUDE') then
               CALL GETNAM(SSTA_t,K1,K2,KFCH,KLCH)
               IF (KFCH.EQ.0) GOTO 30
               K1=KLCH+1
               IF (K1.LE.K2.AND.KFCH.LE.JRBPOS)  THEN
*--- exclude special keys like 'UNIT=' etc.
                 CALL GETNBL(SSTA_t(K1:),STEMP,NN)
                 IF (STEMP.EQ.'='.AND.NN.GT.0) GOTO 20
               ENDIF
               IF (ISNAME+NSNAME.GE.MXNAME) CALL ERREX1
               NSNAME=NSNAME+1
               NSSTRT(NSNAME)=KFCH
               NSEND(NSNAME)=KLCH
*--- Here we use the original mixed case source for the name extraction
               SNAMES(ISNAME+NSNAME)=' '
               CALL GETNBL(SSTA(KFCH:KLCH),SNAMES(ISNAME+NSNAME),NN)
*--- continue if all names to be found
               IF (.NOT.(ACTION(10).OR.ISTMDS(12,ILOC).EQ.1)) GOTO 20
             else
*--- this is an INCLUDE ... strip quotes or brackets to get filename
               if (isname+nsname.ge.mxname) call errex1
               nsname = nsname + 1
               SNAMES(ISNAME+NSNAME)=' '
               ifi = 0
               ila = k2
               do 40 ipo=k1,k2
                  stemp = ssta(ipo:ipo)
                  if(stemp.eq.'{'.or.stemp.eq.'}') goto 40
                  if(ifi.eq.0.and.stemp.eq.' ') goto 40
                  ifi = ifi+1
                  ila = ipo
                  snames(isname+nsname)(ifi:ifi) = stemp
                  if(ifi.eq.1) nsstrt(nsname) = ipo
   40          continue
               nsend(nsname) = ila
             endif               
         ENDIF
   30 CONTINUE
      END
