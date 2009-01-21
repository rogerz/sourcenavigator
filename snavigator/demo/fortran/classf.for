      SUBROUTINE CLASSF
*-----------------------------------------------------------------------
*
*--- classifies a FORTRAN statement.
*    modified by JJB to understand mixed case Fortran
*
*--- input
*    SSTA       string containing the statement
*    NCHST      last ch. of statement in SSTA
*    SSTM (,ISTMDS,IALPHA,IPRIOR,IHEADR) statement descriptors
*--- output
*    ICURCL = statement numbers of first part and second part
*          ( ICURCL(2) set for ICURCL(1) = IIF = logical IF, else = ILL)
*                 ICURCL(1) = ILL for illegal statements
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'class.h'
      include 'flags.h'
      include 'flwork.h'
      include 'cursta.h'
      CHARACTER STEMP*1,STRING*25,ssta_t*(mdimst),touppr*(mdimst)
      external touppr
      include 'convex.h'
      ICURCL(1)=ILL
      ICURCL(2)=ILL
*--- if illegal during extraction (EXTRAC), return
      IF (STATUS(3)) GOTO 999
      if (nchst.eq.0) then
          icurcl(1) = 0
          goto 999
      endif
      KSTART=1
      ssta_t(:nchst) = touppr(ssta(:nchst))
*--- loop over (possibly) two parts of statement
      DO 50 IPRTS=1,2
         KPOS=0
   10    STEMP=SSTA_t(KSTART:)
         IF (STEMP.EQ.' ')  THEN
*--- skip blanks
            KSTART=KSTART+1
            GOTO 10
         ENDIF
*--- check priority statements first if '=' present
         IF(INDEX(SSTA_t(KSTART:NCHST),'=').NE.0) THEN
            DO 20 JS=1,NPRIOR
               JSS=IPRIOR(JS)
               CALL MATCH(SSTM,ISTMDS(3,JSS),ISTMDS(4,JSS),
     &              SSTA_t,KSTART,NCHST,.FALSE.,KPOS,ILEV,
     &              NDUMMY,IWS,IWS)
               IF (KPOS.NE.0) GOTO 40
   20       CONTINUE
         ENDIF
*--- no match yet - get alphabetic group and compare
         IF (ALPHCH(STEMP))  THEN
            K=ICVAL(STEMP)
         ELSE
            K=53
         ENDIF
         if(k.le.0.or.k.gt.53) goto 999
C?J         IF(K.LE.0.OR.K.GT.53) GOTO 999
*--- KBLP = pos. of first blank after start of keyword,
         KBLP=INDEX(SSTA_t(KSTART:NCHST),' ')
         DO 30 JSS=IALPHA(1,K),IALPHA(2,K)
            IF (ISTMDS(7,JSS).EQ.0.AND.ISTMDS(3,JSS).NE.0)  THEN
               IF(ISTMDS(13,JSS).GE.2) THEN
*--- simple match is sufficient
                  I1=ISTMDS(3,JSS)
                  I2=ISTMDS(4,JSS)
                  N1=I2-I1
                  N2=N1+1
                  IF(KBLP.EQ.0.OR.KBLP.GT.N2) THEN
                    IF(SSTA_t(KSTART:KSTART+N1).EQ.SSTM(I1:I2))KPOS=1
                  ELSE
                    CALL GETNBL(SSTA_t(KSTART:NCHST),STRING(1:N2),KEXT)
                    IF(KEXT.GE.N2) THEN
                      IF(STRING(:N2).EQ.SSTM(I1:I2)) KPOS=1
                    ENDIF
                  ENDIF
               ELSE
                  CALL MATCH(SSTM,ISTMDS(3,JSS),ISTMDS(4,JSS),SSTA_t,
     +            KSTART, NCHST,.FALSE.,KPOS,ILEV,NDUMMY,IWS,IWS)
               ENDIF
               IF (KPOS.NE.0) GOTO 40
            ENDIF
   30    CONTINUE
*--- exit if no match at all
         GOTO 999
   40    CONTINUE
*--- matched
         IF (IPRTS.EQ.1)  THEN
            ICURCL(1)=JSS
            IF (ICURCL(1).NE.IIF.and.icurcl(1).ne.iif+71) GOTO 999
*--- skip to end of if(...)
            KMT=INDEX(SSTA_t(1:NCHST),'(')
            CALL SKIPLV(SSTA_t,KMT+1,NCHST,.FALSE.,KPOS,ILEV)
            KSTART=KPOS+1
         ELSE
*--- second part matched
            ICURCL(2)=JSS
         ENDIF
   50 CONTINUE
  999 END
