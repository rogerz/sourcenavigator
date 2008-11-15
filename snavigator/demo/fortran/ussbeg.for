      SUBROUTINE USSBEG
*-----------------------------------------------------------------------
*
*--- user start of filtered statement (treat names here)
*
*-----------------------------------------------------------------------
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
      include 'uscomn.h'
      include 'usstmt.h'
      include 'usigno.h'
      include 'uslist.h'
      include 'usunit.h'
      include 'usargs.h'
      include 'usinfn.h'
      include 'usltyd.h'
      include 'checks.h'
      CHARACTER*(MXNMCH) CNAM
      character*(mxsima) touppr
      CHARACTER*25 C25NAM
      external touppr
      LOGICAL FOK
      DATA ICALL /0/
      IF(UNFLP) RETURN
      IF(ICALL.EQ.0) THEN
         ISGLOB = 0
         ICALL = 1
      ENDIF
C Determine whether this module is to be processed
      IF(.NOT.RPROCS) RETURN
      NST = NFLINE(NSTREF)
      NFI = NLLINE(NSTREF)
      ICL1 = ICURCL(1)
      ICL2 = ICURCL(2)
C ICL1 is class of first part of statement
C ICL2 is class of second part if ICL1 is an IF statement
      IF(LMODUS(ICL1)) THEN
C Module start
C
         IF(NIGNOS.NE.0) THEN
            CNAM = SNAMES(ISNAME+1)
            ILEN = INDEX(CNAM,' ')-1
            IF(ILEN.EQ.-1) ILEN = MXNMCH
            DO 10 IGN=1,NIGNOS
               IF(LIGNOS(IGN).NE.ILEN)                           GOTO 10
               IF(CIGNOS(IGN).EQ.CNAM) THEN
                  NFAULT = 0
                  RPROCS = .FALSE.
                  RETURN
               ENDIF
   10       CONTINUE
         ENDIF
         WRITE(MZUNIT,550) (SIMA(II)(7:lenocc(sima(ii))),II=NST,NFI)
         NCOMN = 0
         NCOMT = 0
         IFUNC = 0
C Set FUNCTION flag
         IF(LFUNCT(ICL1)) IFUNC = 1
         ICLOLD = ICL1
         NFIOLD = NFI
c
c make check for at least 3 comment lines after start of routine
c
         IF(LCHECK(14)) THEN
            ncomnt = 0
            do 11 is=nfi+1,min(nfi+4,nlines)
               if(sima(is)(1:1).eq.' ') goto 11
               if(sima(is)(1:1).eq.'c') ncomnt = ncomnt+1
               if(sima(is)(1:1).eq.'C') ncomnt = ncomnt+1
               if(sima(is)(1:1).eq.'!') ncomnt = ncomnt+1
               if(sima(is)(1:1).eq.'*') ncomnt = ncomnt+1
   11       continue
            if(ncomnt.lt.3) then
               WRITE(MZUNIT,580)
               NFAULT = NFAULT + 1
            endif
         endif
c
         IF(LCHECK(11).AND.NSTREF.NE.1) WRITE(MZUNIT,560)
C Make check for module names the same as intrinsic functions
         CNAM = SNAMES(ISNAME+1)
         ILEN = lenocc(cnam)
         IF(LCHECK(12)) THEN
            DO 20 I=1,LIF
               IF(ILEN.NE.lenocc(cinfun(i)))                     GOTO 20
               IF(CNAM(:ILEN).NE.CINFUN(I)(:ILEN))               GOTO 20
               if(touppr(cnam(:ilen)).ne.cinfun(i)(:ilen))       goto 20
               WRITE(MZUNIT,570) CNAM(:ilen),cinfun(i)(:ilen)
               NFAULT = NFAULT + 1
                                                                 GOTO 30
   20       CONTINUE
   30       CONTINUE
         ENDIF
C First statement in input should be module declaration
      ELSE IF(LCHECK(13).AND.ISGLOB.EQ.0.AND.NFIOLD.EQ.0) THEN
         WRITE(MZUNIT,500)
         NFAULT = NFAULT + 1
      ENDIF
      IF(NST-NFIOLD.GT.1) THEN
         IF(USFULL) WRITE(MZUNIT,510) (II+ISGLOB,SIMA(II), II=NFIOLD+1,
     +   NST-1)
C Check comment lines
         ICMSET = 0
         DO 40 I=NFIOLD+1,NST-1
            IF(NLTYPE(I).EQ.0) THEN
C Store comment line if TREE option requested
               IF(ACTION(29).AND.(SIMA(I)(1:2).EQ.'C!'.or.
     &                            sima(i)(1:2).eq.'c!')) THEN
                  IF(ICMSET.EQ.0) CMMNT = SIMA(I)(3:LARC+2)
                  ICMSET = 1
               ENDIF
C comment lines should start with C or * or !
               IF(LCHECK(15).AND.(SIMA(I)(1:1).NE.'C'.and.
     &                            sima(i)(1:1).ne.'c'.and.
     &                            sima(i)(1:1).ne.'*'.and.
     &                            sima(i)(1:1).ne.'!')) THEN
                  IF(.NOT.USFULL) WRITE(MZUNIT,510) I+ISGLOB,SIMA(I)
                  WRITE(MZUNIT,590)
                  NFAULT = NFAULT + 1
               ENDIF
            ENDIF
   40    CONTINUE
      ENDIF
      NFIOLD = NFI
C Write all statements to MZUNIT if USFULL set
      IF(USFULL) THEN
        WRITE(MZUNIT,510) (II+ISGLOB,SIMA(II),II=NST,NFI)
      ENDIF
C
C Check for comment lines in between continuations
      IF(LCHECK(16).AND.NFI-NST.GT.0) THEN
         DO 50 IST=NST+1,NFI-1
            IF(SIMA(IST)(:5).NE.'      ') THEN
               IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II),II
     +         =NST,NFI)
               WRITE(MZUNIT,610)
               NFAULT = NFAULT + 1
                                                                 GOTO 60
            ENDIF
   50    CONTINUE
   60    CONTINUE
      ENDIF
C Check for standard variable types
      IF(LCHECK(17).AND.LNSVT(ICL1)) THEN
         IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II),II=NST,
     +   NFI)
         WRITE(MZUNIT,520)
         NFAULT = NFAULT + 1
      ENDIF
C Collect list of COMMON names used in this routine
      IF(LCOMMN(ICL1)) THEN
C First check that only one COMMON name per COMMON statement
         IPOS1 = INDEX(SSTA(:NCHST),'/')
         IF(IPOS1.EQ.0) GOTO 70
         IPOS2 = INDEX(SSTA(IPOS1+1:NCHST),'/')
         IF(IPOS2.EQ.0) GOTO 70
         IPOS3 = INDEX(SSTA(IPOS1+IPOS2+1:NCHST),'/')
         IF(IPOS3.NE.0.and.lcheck(18)) THEN
            IF(.NOT.USFULL) WRITE(MZUNIT,850)
     &                   (II+ISGLOB,SIMA(II),II =NST,NFI)
            WRITE(MZUNIT,620)
            NFAULT = NFAULT + 1
         ENDIF
   70    CONTINUE
         NCOMT = NCOMT + 1
         IF(NCOMT.GT.MCOMT) THEN
            NCOMT = NCOMT-1
            WRITE(MZUNIT,630)
                                                                GOTO 110
         ENDIF
C Take account of blank COMMON
         IF(INDEX(SSTA(:NCHST),'//').NE.0.OR.
     &      INDEX(SSTA(:NCHST),'/ /').NE.0) THEN
            SCTITL(NCOMT) = 'BLANKCOM'
            IST = 1
         ELSE
            SCTITL(NCOMT) = SNAMES(ISNAME+1)
            IST = 2
         ENDIF
         ICTITL(NCOMT) = NCOMN + 1
         DO 100 ISN=IST,NSNAME
C We ensure that the list of names for this COMMON block does not
C include parameters. This is done by checking for no hanging parentheses
            IBEG = NSSTRT(ISN)
            ICOUNB = 0
            DO 95 ICH=1,IBEG-1
               IF(SSTA(ICH:ICH).EQ.'(') THEN
                 ICOUNB=ICOUNB+1
               ELSE IF(SSTA(ICH:ICH).EQ.')') THEN
                 ICOUNB=ICOUNB-1
               ENDIF
   95       CONTINUE
            IF(ICOUNB.NE.0) GOTO 100
            NCOMN = NCOMN + 1
            IF(NCOMN.GT.MCOMN) THEN
               NCOMN = NCOMN-1
               WRITE(MZUNIT,640)
                                                                GOTO 110
            ENDIF
            SCNAME(NCOMN) = SNAMES(ISNAME+ISN)
            ICNAME(NCOMN) = NCOMT
  100    CONTINUE
  110    CONTINUE
      ENDIF
C Check for statements which dimension outside COMMON
      IF(LCHECK(19).AND.LDIMEN(ICL1)) THEN
         IOVER = 0
         DO 150 I=1,NSNAME
            CNAM = SNAMES(I+ISNAME)
            ilen = lenocc(cnam)
            MATCH = 0
            DO 130 IC=1,NCOMN
               ILEN1 = lenocc(SCNAME(IC))
               IF(ILEN1.NE.ILEN)                                GOTO 130
               IF(CNAM.NE.SCNAME(IC))                           GOTO 130
               MATCH = 1
C Now have found a declaration of a name in COMMON
C Search for position of name in the statement
               INDE = NSEND(I)+1
C Search for ( or , and ignore blanks
               DO 120 IPL = INDE,NCHST
                  IF(SSTA(IPL:IPL).EQ.' ')                      GOTO 120
                  IF(SSTA(IPL:IPL).EQ.',')                      GOTO 140
                  IF(SSTA(IPL:IPL).EQ.'(') THEN
C array declaration
                     IF(IOVER.EQ.0.AND..NOT.USFULL) WRITE(MZUNIT,850)
     +               (II+ ISGLOB, SIMA(II),II=NST,NFI)
                     WRITE(MZUNIT,650) CNAM(:lenocc(cnam))
                     NFAULT = NFAULT + 1
                     IOVER = 1
                                                                GOTO 150
                  ELSE
                                                                GOTO 140
                  ENDIF
  120          CONTINUE
  130       CONTINUE
  140       CONTINUE
  150    CONTINUE
      ENDIF
C Check for embedded blanks in names
      IF(LCHECK(20)) THEN
         IDONE = 0
         DO 160 I=1,NSNAME
            CNAM=SNAMES(I+ISNAME)
            ILEN1 = lenocc(cnam)
            NS = NSSTRT(I)
            NE = NSEND(I)
            ILEN2 = NE-NS+1
            IF(ILEN2.NE.ILEN1) THEN
               IF(IDONE.EQ.0.AND..NOT.USFULL) WRITE(MZUNIT,850) (II
     +         +ISGLOB, SIMA(II),II=NST, NFI)
               WRITE(MZUNIT,660) CNAM(:ilen1)
               IDONE = 1
               NFAULT = NFAULT + 1
            ENDIF
  160    CONTINUE
      ENDIF
C Now check for embedded blanks in  syntactic entities
      NF1 = ISTMDS(3,ICL1)
      NL1 = ISTMDS(4,ICL1)
      IF(LIFF(ICL1)) THEN
         NF2 = ISTMDS(3,ICL2)
         NL2 = ISTMDS(4,ICL2)
      ELSE
         NF2 = 0
      ENDIF
      IF(LCHECK(21)) THEN
C DEFSTA returns FOK=.TRUE. if statement ICL1 is to be checked
         CALL DEFSTA(ICL1,ILEN,C25NAM,FOK)
         IF(FOK) THEN
            inde = index(touppr(sima(nst)),c25nam(:ilen))
            IF(INDE.EQ.0) THEN
               IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II),II
     +         =NST, NFI)
               WRITE(MZUNIT,670) C25NAM(:lenocc(c25nam))
               NFAULT = NFAULT + 1
            ELSE
               iafter = inde+ilen
               IF(SIMA(NST)(iafter:iafter).NE.' '.AND.
     +            sima(nst)(iafter:iafter).ne.'(') THEN
                  IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II),
     +            II =NST,NFI)
                  WRITE(MZUNIT,680) C25NAM(:lenocc(c25nam))
                  NFAULT = NFAULT + 1
               ENDIF
            ENDIF
         ENDIF
C Special treatment of GO TO and ELSE IF
         IF(LELSE(ICL1)) THEN
            INDE = INDEX(touppr(SSTA(:NCHST)),'ELSE')
            IF(INDE.EQ.0) THEN
               IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II), II
     +         =NST,NFI)
               WRITE(MZUNIT,690)
               NFAULT = NFAULT + 1
            ELSE
               if(nchst.ne.inde+3.and.index(touppr(ssta(:nchst)),
     &              'IF').eq.0) then
                        IF(.NOT.USFULL) WRITE(MZUNIT,850) 
     &                                  (II+ISGLOB,SIMA(II),II=NST,NFI)
                        WRITE(MZUNIT,690)
                        NFAULT = NFAULT + 1
               endif
            ENDIF
         ENDIF
         IF(LGOTO(ICL1)) THEN
            INDE = 0
            INDE1 = INDEX(touppr(SSTA(:NCHST)),'GO TO')
            IF(INDE1.EQ.0) INDE = INDEX(touppr(SSTA(:NCHST)),'GOTO')
            IF(INDE.EQ.0.AND.INDE1.EQ.0) THEN
               IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II),II
     +         =NST, NFI)
               WRITE(MZUNIT,710)
               NFAULT = NFAULT + 1
            ELSE IF(INDE1.NE.0.AND.(INDEX(SSTA(:NCHST),'GO TO ').EQ.0
     +              .and.index(ssta(:nchst),'go to ').eq.0)) then
               IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II),II
     +         =NST, NFI)
               WRITE(MZUNIT,720)
               NFAULT = NFAULT + 1
            ELSE IF(INDE.NE.0.AND.(INDEX(SSTA(:NCHST),'GOTO ').EQ.0
     +              .and.index(ssta(:nchst),'goto ').eq.0)) then
               IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II),II
     +         =NST, NFI)
               WRITE(MZUNIT,730)
               NFAULT = NFAULT + 1
            ENDIF
         ENDIF
C End special treatment for ICL1
         IF(NF2.NE.0) THEN
            CALL DEFSTA(ICL2,ILEN,C25NAM,FOK)
            IF(FOK) THEN
               DO 190 IJ=NST,NFI
                  INDE = INDEX(touppr(SIMA(IJ)),C25NAM(:ILEN))
                  IF(INDE.NE.0) THEN
                     iafter = inde+ilen
                     IF(SIMA(IJ)(Iafter:Iafter).NE.' '.and.
     &                  SIMA(IJ)(Iafter:Iafter).NE.'(') THEN
                        IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,
     +                  SIMA(II),II =NST,NFI)
                        WRITE(MZUNIT,680) C25NAM(:lenocc(c25nam))
                        NFAULT = NFAULT + 1
                     ENDIF
                                                                GOTO 200
                  ENDIF
  190          CONTINUE
               IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II),II
     +         =NST, NFI)
               WRITE(MZUNIT,670) C25NAM(:lenocc(c25nam))
               NFAULT = NFAULT + 1
  200          CONTINUE
            ENDIF
         ENDIF
C Special treatment of GO TO after IF statement
         IF(LGOTO(ICL2).AND.NF2.NE.0) THEN
            DO 210 IJ=NST,NFI
               INDE = 0
               INDE1 = INDEX(touppr(SIMA(IJ)),'GO TO')
               IF(INDE1.EQ.0) INDE = INDEX(touppr(SIMA(IJ)),'GOTO')
               IF(INDE.NE.0) THEN
                  IF(INDEX(touppr(SIMA(IJ)),'GOTO ').EQ.0) THEN
                     IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA
     +               (II),II =NST,NFI)
                     WRITE(MZUNIT,740)
                     NFAULT = NFAULT + 1
                  ENDIF
                                                                GOTO 220
               ELSE IF(INDE1.NE.0) THEN
                  IF(INDEX(touppr(SIMA(IJ)),'GO TO ').EQ.0) THEN
                     IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA
     +               (II),II =NST,NFI)
                     WRITE(MZUNIT,750)
                     NFAULT = NFAULT + 1
                  ENDIF
                                                                GOTO 220
               ELSE IF(IJ.EQ.NFI) THEN
                  IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II),
     +            II =NST,NFI)
                  WRITE(MZUNIT,760)
                  NFAULT = NFAULT + 1
                                                                GOTO 220
               ENDIF
  210       CONTINUE
  220       CONTINUE
         ENDIF
      ENDIF
C End special treatment for ICL2 GOTO
      IF(LCHECK(22).AND.(LPRINT(ICL1).OR.LPRINT(ICL2))) THEN
C PRINT statement
         IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II),II=NST,
     +   NFI)
         WRITE(MZUNIT,770)
         NFAULT = NFAULT + 1
      ELSE IF(LCHECK(23).AND.(ICL1.EQ.IEND.or.icl1.eq.iend+71)) THEN
C END statement
         IF(SIMA(NST)(:5).NE.'     ') THEN
            IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II),II
     +      =NST, NFI)
            WRITE(MZUNIT,790)
            NFAULT = NFAULT + 1
         ENDIF
      ELSE IF(LWRITE(ICL1).OR.LWRITE(ICL2)) THEN
C WRITE statement
         IF(LCHECK(24)) THEN
            ILOC = INDEX(SSTA(:NCHST),'WRITE')+5
            if(iloc.eq.5) iloc = index(ssta(:nchst),'write')+5
            ILOC1 = INDEX(SSTA(ILOC:NCHST),'(')
            IF(ILOC1.EQ.0.OR.ILOC.EQ.0)                         GOTO 240
            ILOC = ILOC1 + ILOC
            DO 230 IL=ILOC,MXLINE
               IF(SSTA(IL:IL).EQ.' ')                           GOTO 230
               IF(SSTA(IL:IL).EQ.'*') THEN
                  IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II),
     +            II =NST,NFI)
                  WRITE(MZUNIT,800)
                  NFAULT = NFAULT + 1
               ELSE
                                                                GOTO 240
               ENDIF
  230       CONTINUE
  240       CONTINUE
         ENDIF
C Check for WRITE in FUNCTION
         if(lcheck(25).and.ifunc.eq.1) then
            IF(.NOT.USFULL)WRITE(MZUNIT,850)(II+ISGLOB,SIMA(II),II=NST,
     +      NFI)
            WRITE(MZUNIT,860)
            NFAULT = NFAULT + 1
         endif 
      ENDIF
      IF(LCHECK(26).AND.(LPAUSE(ICL1).OR.LPAUSE(ICL2))) THEN
C PAUSE statement
         IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II),II=NST,
     +   NFI)
         WRITE(MZUNIT,810)
         NFAULT = NFAULT + 1
      ENDIF
C check for statement labels beginning in column 1
      IF(LCHECK(27)) THEN
         IF(LLE(SIMA(NST)(1:1),'9').AND.LGE(SIMA(NST)(1:1),'0')) THEN
            IF(.NOT.USFULL)WRITE(MZUNIT,850)(II+ISGLOB,SIMA(II),II=NST,
     +      NFI)
            WRITE(MZUNIT,530)
            NFAULT = NFAULT + 1
         ENDIF
      ENDIF
      IF(LCHECK(28).AND.LSTOP(ICL1)) THEN
C STOP statement (unconditional)
         IF(.NOT.LWRITE(ICLOLD)) THEN
            IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II),II
     +      =NST, NFI)
            WRITE(MZUNIT,820)
            NFAULT = NFAULT + 1
         ENDIF
      ENDIF
C Check for ENTRY in FUNCTION
      IF(LCHECK(29).AND.LENTRY(ICL1).AND.IFUNC.EQ.1) THEN
         IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II),II=NST,
     +   NFI)
         WRITE(MZUNIT,830)
         NFAULT = NFAULT + 1
      ENDIF
c check for entry at all
      if(lcheck(46).and.lentry(icl1)) then
         if(.not.usfull) write(mzunit,850) (ii+isglob,sima(ii),ii=nst,
     &   nfi)
         write(mzunit,870)
         nfault = nfault + 1
      endif
C Check for I/O in FUNCTION
      IF(LCHECK(30).AND.IFUNC.EQ.1) THEN
         IF(LIO(ICL1)) THEN
            IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II),II
     +      =NST,NFI)
            WRITE(MZUNIT,780)
            NFAULT = NFAULT + 1
         ENDIF
         IF(LIO(ICL2)) THEN
            IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA(II),II
     +      =NST,NFI)
            WRITE(MZUNIT,780)
            NFAULT = NFAULT + 1
         ENDIF
      ENDIF
C check for alternate RETURN
      IF(LCHECK(31).AND.(LRETRN(ICL1).OR.LRETRN(ICL2))) THEN
         IPOSR=INDEX(SSTA(:NCHST),'RETURN')
         if(iposr.eq.0) iposr=index(ssta(:nchst),'return')
         IF(IPOSR.NE.0.AND.IPOSR+5.NE.NCHST) THEN
            IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB, SIMA(II),II
     +      =NST, NFI)
            WRITE(MZUNIT,540)
            NFAULT = NFAULT + 1
         ENDIF
      ENDIF
C Check for COMMON block title clash with variable name
      IF(.NOT.LCOMMN(ICL1).AND..NOT.LSAVE(ICL1)) THEN
         DO 280 IS=1,NSNAME
            ILEN = INDEX(SNAMES(IS+ISNAME),' ')-1
            DO 250 ICT=1,NCOMT
               ILEN2 = INDEX(SCTITL(ICT),' ')-1
               IF(ILEN2.NE.ILEN)                                GOTO 250
               IF(LCHECK(32)) THEN
                  IF(SNAMES(IS+ISNAME).EQ.SCTITL(ICT)) THEN
                     IF(.NOT.USFULL) WRITE(MZUNIT,850) (II+ISGLOB,SIMA
     +               (II),II =NST,NFI)
                     WRITE(MZUNIT,840) SCTITL(ICT)(:lenocc(sctitl(ict)))
     +                                ,SCTITL(ICT)(:lenocc(sctitl(ict)))
                     NFAULT = NFAULT + 1
                                                                GOTO 260
                  ENDIF
               ENDIF
  250       CONTINUE
  260       CONTINUE
C Mark COMMON block variables as used
            DO 270 ICN=1,NCOMN
               ILEN2 = INDEX(SCNAME(ICN),' ')-1
               IF(ILEN2.NE.ILEN)                                GOTO 270
               IF(SCNAME(ICN).EQ.SNAMES(IS+ISNAME)) THEN
                  ICM = ICNAME(ICN)
                  ICTITL(ICM) = -IABS(ICTITL(ICM))
               ENDIF
  270       CONTINUE
  280    CONTINUE
      ENDIF
C Make ICLOLD last executable statement
      IF(ISTMDS(11,ICL1).EQ.1) THEN
         ICLOLD = ICL2
         IF(ICL1.NE.IIF.and.icl1.ne.iif+71) ICLOLD = ICL1
      ENDIF
C
  500 FORMAT(/,1X,'!!! WARNING ... SOURCE DOES NOT BEGIN',
     +' WITH MODULE DECLARATION EG "PROGRAM  ... "')
  510 FORMAT((1X,I6,'. ',A80))
  520 FORMAT(1X,'!!! WARNING ... USE STANDARD FORTRAN TYPES')
  530 FORMAT(1X,'!!! STATEMENT HAS LABEL BEGINNING IN COLUMN 1')
  540 FORMAT(1X,'!!! STATEMENT USES THE ALTERNATE RETURN FACILITY')
  550 FORMAT(1X,10('+'), ' BEGIN MODULE CHECKS FOR ',A,1x,
     +          10('+'),(/,36X,A))
  560 FORMAT(1X,'!!! WARNING ... AVOID COMMENT LINES',
     +' BEFORE MODULE DECLARATION')
  570 FORMAT(1X,'!!! WARNING ... MODULE ',A,
     +' CLASHES WITH INTRINSIC FUNCTION ',A)
  580 FORMAT(1X,'!!! WARNING ... NOT ENOUGH (<3) COMMENT',
     +' LINES AT START OF MODULE')
  590 FORMAT(1X,'!!! COMMENT DOES NOT START WITH "C"')
  600 FORMAT(1X,'    IT SHOULD BE A HISTORIAN "CALL" ANYWAY')
  610 FORMAT(1X,'!!! STATEMENT HAS COMMENT PLACED BEFORE CONTINUATION')
  620 FORMAT(1X,'!!! STATEMENT CONTAINS >1 COMMON DEFINITION')
  630 FORMAT(1X,'!!! NON-FATAL ERROR IN USSBEG . MCOMT EXCEEDED')
  640 FORMAT(1X,'!!! NON-FATAL ERROR IN USSBEG . MCOMN EXCEEDED')
  650 FORMAT(1X,'!!! STATEMENT DIMENSIONS ',A,' OUTSIDE COMMON')
  660 FORMAT(1X,'!!! NAME ',A,' HAS EMBEDDED BLANKS AT SOURCE')
  670 FORMAT(1X,'!!! THE KEYWORD ',A,' CONTAINS BLANKS')
  680 FORMAT(1X,'!!! STATEMENT HAS NO BLANK AFTER KEYWORD ',A)
  690 FORMAT(1X,'!!! KEYWORD "ELSE IF" CONTAINS MISPLACED BLANKS')
  700 FORMAT(1X,'!!! STATEMENT HAS NO BLANK AFTER "ELSEIF"')
  710 FORMAT(1X,'!!! KEYWORD "GO TO" CONTAINS MISPLACED BLANKS')
  720 FORMAT(1X,'!!! STATEMENT HAS NO BLANK AFTER "GO TO"')
  730 FORMAT(1X,'!!! STATEMENT HAS NO BLANK AFTER "GOTO"')
  740 FORMAT(1X,'!!! STATEMENT HAS NO BLANK AFTER "GOTO"')
  750 FORMAT(1X,'!!! STATEMENT HAS NO BLANK AFTER "GO TO"')
  760 FORMAT(1X,'!!! STATEMENT CONTAINS EMBEDDED BLANKS IN "GO TO"')
  770 FORMAT(1X,'!!! STATEMENT SHOULD BE A WRITE STATEMENT')
  780 FORMAT(1X,'!!! AVOID I/O IN FUNCTIONS')
  790 FORMAT(1X,'!!! STATEMENT SHOULD NOT HAVE LABEL')
  800 FORMAT(1X,'!!! STATEMENT SHOULD NOT HAVE LUN=*')
  810 FORMAT(1X,'!!! PAUSE STATEMENTS ARE FROWNED UPON')
  820 FORMAT(1X,'!!! STATEMENT SHOULD BE PRECEDED BY A "WRITE"')
  830 FORMAT(1X,'!!! AVOID ENTRY STATEMENTS IN FUNCTIONS')
  840 FORMAT(1X,'!!! ',A,' CLASHES WITH COMMON BLOCK NAME ',A)
  860 FORMAT(1x,'!!! FUNCTIONS SHOULD NOT CONTAIN "WRITE"')
  870 format(1x,'!!! AVOID THE USE OF THE ENTRY STATEMENT')
  850 FORMAT((1X,I6,'. ',A80))
      END
