      SUBROUTINE SECPAS(NGLOBF,LIMPNO)
      include 'param.h'
      include 'alcaza.h'
      include 'class.h'
      include 'cursta.h'
      include 'flwork.h'
      include 'keycom.h'
      include 'typdef.h'
      include 'jobsum.h'
      include 'state.h'
      include 'flags.h'
      include 'usigno.h'
      include 'uslist.h'
      include 'usgcom.h'
      include 'usstmt.h'
      include 'usunit.h'
      include 'usargs.h'
      include 'usltyd.h'
      include 'checks.h'
      PARAMETER (MNUMP=100)
      CHARACTER*(MXNMCH) CNAM,CNAMF,CNAMP(MNUMP)
      CHARACTER*(NOARG) CSTRIN,CDIM,CDIMN(10)
      CHARACTER*(MDIMST) CSTAT,touppr
      character*1000 clout
      INTEGER ICNAMP(MNUMP),NSEND2(700)
      INTEGER IDO(100)
      LOGICAL LIMPNO,BTEST,written
      external touppr
c
      IOSM = 0
      IOSP = 0
      IOSD = 0
      IOSS = 0
      IOSO = 0
      IOSE = 0
      NSTFUN = 0
      NUMP = 0
      NUMF = 0
      NSTFIN = 0
      DO 10 II=1,MNUMP
         CNAMP(II)(1:MXNMCH)=' '
         ICNAMP(II) = 0
   10 CONTINUE
      DO 20 I=1,100
         IDO(I) = 0
   20 CONTINUE
      MNTDO=0
      MNTIF=0
      NKALL=0
      LIMPNO = .FALSE.
      action(30) = .false.
      DO 330 IST=1,NSTAMM
         written = .false.
         ICL1 = ICLASS(IST,1)
         ICL2 = ICLASS(IST,2)
         NST = NFLINE(IST)
         NFI = NLLINE(IST)
         IF(ICL1.EQ.0.OR.ICL1.EQ.999)                           GOTO 700
C GET STATEMENT NAMES
         ICURCL(1)=ICL1
         ICURCL(2)=ICL2
         ISNAME = IRNAME+NRNAME
         CALL EXTRAC(IST,'FULL')
         CALL GETALL
C make check for MIXED MODE EXPRESSIONS
         IF(LCHECK(37)) CALL MIXMOD(NGLOBF)
C if TREE info, find current DO/IF level. After Grote.
         IF(ACTION(29)) THEN
            ICLE=ISTMDS(6,ICURCL(1))
            IF(ICLE.EQ.39) THEN
               MNTIF=MNTIF+1
            ELSEIF(ICLE.EQ.27) THEN
               MNTIF=MNTIF-1
            ELSEIF(ICLE.EQ.20) THEN
               IF(MNTDO.LT.100) THEN
                  MNTDO=MNTDO+1
                  CALL GETINT(SSTA,1,NCHST,KFCH,KLCH,NN)
                  IDO(MNTDO)=NN
               ENDIF
            ELSEIF(MNTDO.GT.0) THEN
               K=NEXTIN(SIMA(NFLINE(NSTREF)),1,5)
               KST=MNTDO
               DO 30 I=KST,1,-1
                  IF(IDO(I).NE.K)                                GOTO 40
                  MNTDO=MNTDO-1
   30          CONTINUE
   40          CONTINUE
            ENDIF
C check for CALL
            IF(ICLE.EQ.7) THEN
               IF(NKALL.LT.MKALL) THEN
                  NKALL = NKALL + 1
                  CKALLN(NKALL) = SNAMES(ISNAME+1)
                  KALLIF(NKALL) = MNTIF
                  KALLDO(NKALL) = MNTDO
               ENDIF
            ELSE IF(ICL1.EQ.IIF.or.icl1.eq.iif+71) THEN
               IF(ISTMDS(6,ICURCL(2)).EQ.7) THEN
                  IF(NKALL.LT.MKALL) THEN
                     INDB=INDEX(SSTA,'(')+1
                     CALL SKIPLV(SSTA,INDB,NCHST,.FALSE.,IEN,ILEV)
                     INDB=IEN+1
                     IFOU=999
                     DO 50 ISN=1,NSNAME
                        IF(NSSTRT(ISN).GT.INDB.AND.NSSTRT(ISN).LT.IFOU)
     +                  THEN
                           IFOU=NSSTRT(ISN)
                           ISNF=ISN
                        ENDIF
   50                CONTINUE
                     NKALL = NKALL + 1
                     CKALLN(NKALL) = SNAMES(ISNAME+ISNF)
                     KALLIF(NKALL) = MNTIF+1
                     KALLDO(NKALL) = MNTDO
                  ENDIF
               ENDIF
            ENDIF
C check for use of FUNCTIONs
            IF(ICLE.EQ.2.OR.ISTMDS(6,ICURCL(2)).EQ.2) THEN
C this is an assignment statement
               DO 80 IS=1,NSNAME
                  DO 60 IR=1,NRNAME
                     IF(SNAMES(IR+IRNAME).NE.SNAMES(IS+ISNAME))  GOTO 60
                                                                 GOTO 70
   60             CONTINUE
                                                                 GOTO 80
   70             IF(.NOT.BTEST(NAMTYP(IR+IRNAME),16))           GOTO 80
                  IF(NKALL.GE.MKALL)                             GOTO 90
                  NKALL = NKALL+1
                  CKALLN(NKALL) = SNAMES(IR+IRNAME)
                  KALLIF(NKALL) = MNTIF
                  KALLDO(NKALL) = MNTDO
                  IF(ICLE.EQ.IIF.or.icle.eq.iif+71) 
     &               KALLIF(NKALL) = MNTIF+1
   80          CONTINUE
   90          CONTINUE
            ENDIF
         ENDIF  ! End of TREE processing IF
C remove all blanks in statement
         DO 100 IS=1,NSNAME
            NSEND2(IS)=NSEND(IS)
  100    CONTINUE
         NCHAS = 0
         DO 120 IC=1,NCHST
            IF(SSTA(IC:IC).EQ.' ') THEN
C update NSEND into NSEND2
               DO 110 ISN=1,NSNAME
                  IF(NSEND2(ISN).GT.IC) NSEND2(ISN)=NSEND2(ISN)-1
  110          CONTINUE
                                                                GOTO 120
            ENDIF
            NCHAS = NCHAS + 1
            CSTAT(NCHAS:NCHAS) = SSTA(IC:IC)
  120    CONTINUE
C
C trap IMPLICIT NONE ! or IMPLICIT LOGICAL(A-Z)
         IF(INDEX(touppr(CSTAT),'IMPLICITNONE').NE.0) then
            LIMPNO=.TRUE.
         endif
c         IF(INDEX(CSTAT,'IMPLICITLOGICAL(A-Z)').NE.0) LIMPNO=.TRUE.
         IF(ICL1.EQ.ILL)                                        GOTO 700
C
C At module start, find argument list if any
         IF(LMODUS(ICL1)) THEN
            NARGS = NSNAME - 1
            DO 130 IA=1,NARGS
               CARGNM(IA) = SNAMES(ISNAME+1+IA)
  130       CONTINUE
         ENDIF
C
C within module, check for dimensionality of items in argument list
         IF(ICL1.EQ.0.OR.ICL1.EQ.999.OR.LIFF(ICL1))             GOTO 250
         DO 240 ISN=1,NSNAME
C find name in routine list for NAMTYP check
            DO 140 IRN=1,NRNAME
               IF(SNAMES(IRN+IRNAME).EQ.SNAMES(ISN+ISNAME))     GOTO 150
  140       CONTINUE
                                                                GOTO 240
  150       NTYP = NAMTYP(IRN+IRNAME)
            CNAM = ' '
            CNAM = SNAMES(ISN+ISNAME)
            ILEN1 = INDEX(CNAM,' ')-1
            IF(ILEN1.EQ.-1) ILEN1 = MXNMCH
            IFOU = 0
            DO 160 IARG=1,NARGS
               ILEN2 = INDEX(CARGNM(IARG),' ')-1
               IF(ILEN2.EQ.-1) ILEN2 = MXNMCH
               IF(ILEN2.NE.ILEN1)                               GOTO 160
               IF(CARGNM(IARG)(:ILEN2).NE.CNAM(:ILEN1))         GOTO 160
               IFOU = IARG
               IFOULEN = ILEN2
                                                                GOTO 170
  160       CONTINUE
  170       IF(IFOU.EQ.0)                                       GOTO 240
C found in argument list
C
            IF(.NOT.BTEST(NTYP,17).AND..NOT.BTEST(NTYP,5)) THEN
C fill info in USARGS
               IF(ACTION(29)) THEN
                  IF(CARGTY(IFOU).EQ.' ') THEN
                     IF(BTEST(NTYP,4)) CARGTY(IFOU)='DOUBLEPRECISION'
                     LG = INDEX(CARGTY(IFOU),' ')
                     IF(BTEST(NTYP,0)) CARGTY(IFOU)(LG:)='INTEGER'
                     IF(BTEST(NTYP,1)) CARGTY(IFOU)(LG:)='REAL'
                     IF(BTEST(NTYP,2)) CARGTY(IFOU)(LG:)='LOGICAL'
                     IF(BTEST(NTYP,3)) CARGTY(IFOU)(LG:)='COMPLEX'
                  ENDIF
               ENDIF
                                                                GOTO 240
            ENDIF
            IF(LDIMEN(ICL1)) THEN
C dimensioned or character variable
C first treat CHARACTER*() cases
C
               IC1 = 13
               ISTAR = 0
C
C First look for CHARACTER*(*) declarations
C
               iposs = index(touppr(cstat),'CHARACTER*') + 10
               IF(iposs.NE.10) THEN
                  IC1 = 12
                  ILEV = 0
                  CDIM = ' '
                  N = 0
c
c argument is everything between top-level paretheses
c
                  DO 180 IC=IPOSS,NCHAS
                     IF(CSTAT(IC:IC).EQ.'(') THEN
                        ILEV = ILEV + 1
                        IF(N.GT.0.AND.ILEV.EQ.1)                GOTO 190
                        IF(ILEV.EQ.1)                           GOTO 180
                     ELSE IF(CSTAT(IC:IC).EQ.')') THEN
                        ILEV = ILEV - 1
                        IF(ILEV.EQ.0)                           GOTO 190
                     ENDIF
                     N = N+1
                     CDIM(N:N) = CSTAT(IC:IC)
  180             CONTINUE
  190             CONTINUE
C fill info in USARGS
                  IF(N.EQ.0) THEN
                     N = 1
                     CDIM(1:1) = '?'
                  ENDIF
                  if(cdim(1:n).eq.'*') istar = 1
                  if(isn.eq.1) then
                   IF(ACTION(29)) THEN
                      CARGTY(IFOU) = 'CHARACTER*('//CDIM(:N)//')'
                      NARGDI(IFOU) = 0
                   ENDIF
                   IF(LCHECK(38).AND.CDIM(1:1).NE.'*') THEN
                      WRITE(MZUNIT,500) CNAM(:lenocc(cnam))
                      NGLOBF = NGLOBF + 1
                                                                GOTO 240
                   ENDIF
                  endif
               ENDIF
C
C now CHARACTER with length later or modified length
C
               IPOS = NSEND2(ISN)+1
               IF(LCHARC(ICL1).OR.IC1.EQ.12) THEN
                  N = 0
                  ILEV = 0
                  CDIM = ' '
                  DO 200 IC=IPOS,NCHAS
                     IF(CSTAT(IC:IC).EQ.'(') THEN
                        ILEV = ILEV + 1
                                                                GOTO 200
                     ELSE IF(CSTAT(IC:IC).EQ.')') THEN
                        ILEV = ILEV - 1
                                                                GOTO 200
                     ELSE IF(CSTAT(IC:IC).EQ.'*') THEN
                        IF(ILEV.EQ.0) THEN
                           ISTAR = 1
                                                                GOTO 200
                        ENDIF
                     ENDIF
                     IF(ILEV.EQ.0.AND.CSTAT(IC:IC).EQ.',')      GOTO 210
                     IF(ISTAR.EQ.0)                             GOTO 200
                     N = N + 1
                     CDIM(N:N) = CSTAT(IC:IC)
  200             CONTINUE
  210             CONTINUE
C fill info in USARGS
                  if(n.eq.0.and.istar.eq.1.and.isn.eq.1) then
                     n = 1
                     cdim(1:1) = '*'
                  endif
                  IF(N.EQ.0) THEN
                     N = 1
                     CDIM(:1) = '?'
                  ENDIF
                  IF(ACTION(29)) THEN
                     CARGTY(IFOU) = 'CHARACTER*('//CDIM(:N)//')'
                     NARGDI(IFOU) = 0
                  ENDIF
                  IF(LCHECK(38)) THEN
                     IF((CDIM(1:1).NE.'*'.AND.IC1.EQ.13).OR. 
     +                  (IC1.EQ.12.AND.CDIM(1:1).NE.'*')) THEN
                        WRITE(MZUNIT,500) CNAM(:lenocc(cnam))
                        NGLOBF = NGLOBF + 1
                                                                GOTO 240
                     ENDIF
                  ENDIF
                                                                GOTO 240
               ENDIF
C a dimensioned non-character variable
               IPOS2 = INDEX(CSTAT(IPOS:NCHAS),'(')+IPOS
               IF(IPOS2.EQ.IPOS)                                GOTO 240
               IF(IPOS2.NE.IPOS+1)                              GOTO 240
               CALL SKIPLV(CSTAT,IPOS2,NCHAS,.FALSE.,IEN,ILEV)
C dimension clause spans IPOS2 to IEN-1
               ISTA = IPOS2
               IFIN = IEN-1
               NDIM = 0
               CDIM = ' '
               N = 0
               DO 220 IC=ISTA,IFIN
                  IF(CSTAT(IC:IC).EQ.',') THEN
                     NDIM = NDIM + 1
                     CDIMN(NDIM) = ' '
                     CDIMN(NDIM) = CDIM(:N)
                     CDIM = ' '
                     N = 0
                                                                GOTO 220
                  ENDIF
                  N = N + 1
                  CDIM(N:N) = CSTAT(IC:IC)
  220          CONTINUE
               IF(N.EQ.0) THEN
                  N = 1
                  CDIM(1:1) = '?'
               ENDIF
               NDIM = NDIM + 1
               CDIMN(NDIM) = ' '
               CDIMN(NDIM) = CDIM(:N)
               CARGTY(IFOU) = ' '
C fill info in USARGS
               IF(ACTION(29)) THEN
                  IF(BTEST(NTYP,4)) CARGTY(IFOU)='DOUBLEPRECISION'
                  LG = INDEX(CARGTY(IFOU),' ')
                  IF(BTEST(NTYP,0)) CARGTY(IFOU)(LG:)='INTEGER'
                  IF(BTEST(NTYP,1)) CARGTY(IFOU)(LG:)='REAL'
                  IF(BTEST(NTYP,2)) CARGTY(IFOU)(LG:)='LOGICAL'
                  IF(BTEST(NTYP,3)) CARGTY(IFOU)(LG:)='COMPLEX'
 
                  NARGDI(IFOU) = NDIM
                  DO 230 I=1,NDIM
                     CDIM=CDIMN(I)
                     ICOLON=INDEX(CDIM,':')
                     IF(ICOLON.NE.0) THEN
                        CARGDI(I,1,IFOU)=CDIM(1:ICOLON-1)
                        CARGDI(I,2,IFOU)=CDIM(ICOLON+1:INDEX(CDIM,' ')
     +                  -1)
                     ELSE
                        CARGDI(I,1,IFOU)='1'
                        CARGDI(I,2,IFOU)=CDIM
                     ENDIF
  230             CONTINUE
               ENDIF
               IF(NDIM.EQ.0)                                    GOTO 240
               ICOLON = INDEX(CDIMN(NDIM),':')
               IF(ICOLON.NE.0) THEN
                  ILEN = INDEX(CDIMN(NDIM),' ')-1
                  IF(ILEN.EQ.-1) ILEN = NOARG
                  CDIM = CDIMN(NDIM)(ICOLON+1:ILEN)
               ELSE
                  CDIM = CDIMN(NDIM)
               ENDIF
               IF(LCHECK(44).AND.CDIM(1:1).NE.'*') THEN
                  WRITE(MZUNIT,510) CNAM(:lenocc(cnam))
                  NGLOBF = NGLOBF + 1
                                                                GOTO 240
               ENDIF
            ENDIF  ! for LDIMEN(ICL1)
  240    CONTINUE
  250    CONTINUE
         IF(LMODUS(ICL1)) THEN
C Module start
            IF(LCHECK(39).AND.IOSE+IOSO+IOSS+IOSD+IOSP.NE.0) THEN
               WRITE(MZUNIT,550) (SIMA(I),I=NST,NFI)
            ENDIF
            IOSM = 1
         ELSE IF(LDECLR(ICL1)) THEN
C PARAMETER etc
            IF(LCHECK(39).AND.IOSD+IOSS+IOSO+IOSE.NE.0) THEN
               WRITE(MZUNIT,550) (SIMA(I),I=NST,NFI)
               NGLOBF = NGLOBF + 1
            ENDIF
            IOSP = 1
         ELSE IF(LDATA(ICL1)) THEN
C DATA Statement
            IF(LCHECK(39).AND.IOSS+IOSO+IOSE.NE.0) THEN
               WRITE(MZUNIT,550) (SIMA(I),I=NST,NFI)
               NGLOBF = NGLOBF + 1
            ENDIF
            IOSD = 1
         ELSE IF(ICL1.EQ.IEND.or.icl1.eq.iend+71) THEN
C END Statement
            IOSE = 1
         ELSE IF(LASIGN(ICL1)) THEN
C Possible statement function
            IFOUN = 0
            DO 270 IN=1,NRNAME
               IF(.NOT.BTEST(NAMTYP(IRNAME+IN),9))              GOTO 270
               CNAM = SNAMES(IRNAME+IN)
               ILEN = INDEX(CNAM,' ')-1
               IF(ILEN.EQ.-1) ILEN = MXNMCH
C Search for the statement function name at the left of
C an '=' sign . Simple approach but probably not rigorous .
               IND = INDEX(SIMA(NST),CNAM(:ILEN))
C
C CONFIRM THAT THIS IS THE FIRST NAME ON THE LINE
C
               DO 259 ICHP=7,IND-1
                  IF(SIMA(NST)(ICHP:ICHP).NE.' ') GOTO 270
  259          CONTINUE
               INDE = INDEX(SIMA(NST),'=')
               IF(INDE.LT.IND)                                  GOTO 270
               IF(IND.EQ.0)                                     GOTO 270
               DO 260 ILOC=IND+ILEN,MXLINE
                  IF(SIMA(NST)(ILOC:ILOC).EQ.' ')               GOTO 260
                  IF(SIMA(NST)(ILOC:ILOC).EQ.'=') THEN
                     IFOUN = 1
                     CNAMF = CNAM
                                                                GOTO 280
                  ELSE IF(SIMA(NST)(ILOC:ILOC).EQ.'(') THEN
                     NP = 0
                     IF(NUMP.GE.MNUMP) THEN
                        WRITE(MZUNIT,520)
                                                                GOTO 280
                     ENDIF
                     NUMP = NUMP + 1
                                                                GOTO 260
                  ENDIF
                  IF(SIMA(NST)(ILOC:ILOC).GE.'A'.AND. SIMA(NST)
     +            (ILOC:ILOC) .LE.'Z') THEN
                     NP = NP + 1
                     IF(NP.GT.MXNMCH)                           GOTO 260
                     CNAMP(NUMP)(NP:NP) = SIMA(NST)(ILOC:ILOC)
                  ENDIF
                  IF(SIMA(NST)(ILOC:ILOC).EQ.',') THEN
                     NP = 0
                     IF(NUMP.GE.MNUMP) THEN
                        WRITE(MZUNIT,520)
                                                                GOTO 280
                     ENDIF
                     NUMP = NUMP + 1
                  ENDIF
  260          CONTINUE
  270       CONTINUE
  280       CONTINUE
            IF(IFOUN.EQ.1) THEN
               NUMF = NUMF + 1
C Check that statement function surrounded by comment cards
               IF(NSTFUN.EQ.0) THEN
                  NSTFUN = NST
                  IF(LCHECK(40)) THEN
                     IF(SIMA(NST-1)(1:1).NE.'C'.AND.SIMA(NST-1)(1:1).NE.
     +               '*') THEN
                        WRITE(MZUNIT,530) CNAMF(:lenocc(cnamf))
                        NGLOBF = NGLOBF + 1
                     ENDIF
                  ENDIF
               ENDIF
               NSTFIN = NFI+1
               IOSS = 1
               IF(LCHECK(39).AND.IOSO+IOSE.NE.0) THEN
                  WRITE(MZUNIT,550) (SIMA(I),I=NST,NFI)
                  NGLOBF = NGLOBF + 1
               ENDIF
            ELSE
C OTHER Statement
               IF(LCHECK(39).AND.IOSE.EQ.1) THEN
                  WRITE(MZUNIT,550) (SIMA(I),I=NST,NFI)
                  NGLOBF = NGLOBF + 1
               ENDIF
               IOSO = 1
            ENDIF
C Single occurences of names forced here
            DO 300 II=1,NUMP-1
               CNAM=CNAMP(II)
               DO 290 IJ=II+1,NUMP
                  IF(CNAM.EQ.CNAMP(IJ)) ICNAMP(IJ)=ICNAMP(II)
  290          CONTINUE
  300       CONTINUE
C Check that statement function variables are not used elsewhere
            IF(IFOUN.EQ.0) THEN
               DO 320 ISN=1,NSNAME
                  CNAM = SNAMES(ISNAME+ISN)
                  DO 310 ISN2=1,NUMP
                     IF(touppr(CNAM).EQ.touppr(CNAMP(ISN2))) THEN
                        IF(LCHECK(41).AND.ICNAMP(ISN2).EQ.0) THEN
                           WRITE(MZUNIT,540) CNAM(:lenocc(cnam))
                           NGLOBF = NGLOBF + 1
                        ENDIF
                        ICNAMP(ISN2) = 1
                                                                GOTO 320
                     ENDIF
  310             CONTINUE
  320          CONTINUE
            ENDIF
         ENDIF
  700 continue
  330 CONTINUE
      IF(LCHECK(40)) THEN
         IF(NUMF.GT.1.AND.SIMA(NSTFIN)(1:1).NE.'C'.AND.SIMA(NSTFIN)
     +   (1:1).NE.'*'.and.sima(nstfin)(1:1).ne.'c'.and.sima(nstfin)
     +   (1:1).ne.'!') THEN
            WRITE(MZUNIT,530) CNAMF(:lenocc(cnamf))
            NGLOBF = NGLOBF + 1
         ENDIF
      ENDIF
C
C Check for Implicit None
C
      IF(LCHECK(13).and..not.limpno) then
            WRITE(MZUNIT,560) 
            NGLOBF = NGLOBF + 1
      endif
      RETURN
  500 FORMAT(1X,'!!! WARNING ... ARGUMENT ',A,' PASSED TO THIS ',
     +'MODULE, IS NOT CHARACTER*(*)')
  510 FORMAT(1X,'!!! WARNING ... ARGUMENT ',A,' PASSED TO THIS ',
     +'MODULE, DOES NOT HAVE LAST DIMENSION "*"')
  520 FORMAT(1X,'!!! NON-FATAL ERROR IN SECPAS . MNUMP EXCEEDED')
  530 FORMAT(1X,'!!! WARNING ... STATEMENT FUNCTION ',A,' IS NOT',
     +' SURROUNDED BY COMMENTS')
  540 FORMAT(1X,'!!! WARNING ... VARIABLE ',A,
     +',IN STATEMENT FUNCTION DEFINITION, IS USED ELSEWHERE')
  550 FORMAT(1X,'!!! WARNING ... FOLLOWING STATEMENT IS',
     +' OUT OF ORDER ',(/,1X,A80))
  560 format(1x,'!!! WARNING ... MODULE IS MISSING "IMPLICIT NONE"')
      END
