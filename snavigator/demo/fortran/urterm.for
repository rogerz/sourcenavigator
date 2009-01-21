      SUBROUTINE URTERM
*-----------------------------------------------------------------------
*
*--- user routine termination
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
      include 'usgcom.h'
      include 'uscomn.h'
      include 'usstmt.h'
      include 'usigno.h'
      include 'usinfn.h'
      include 'usunit.h'
      include 'usargs.h'
      include 'checks.h'
      PARAMETER (NFS=28)
      DIMENSION IB(21)
      CHARACTER*(MXNMCH) CNAM,CNAMOL,TOUPPR
      CHARACTER*1 CFRST
      CHARACTER*6 CFORT(NFS)
      CHARACTER*131 CZUN
      LOGICAL LIMPNO,BTEST
      EXTERNAL TOUPPR
      DATA CFORT /'ASSIGN', 'CALL  ','COMMON','CLOSE ', 'DATA  ',
     +'DO    ','DECODE','DOUBLE', 'END   ','ENDIF ','ENTRY ', 'ELSE  ',
     +'ELSEIF','ENCODE','FORMAT', 'GOTO  ','IF    ', 'OPEN  ','PRINT ',
     +'PAUSE ', 'PUNCH ','READ  ','REAL  ','RETURN', 'REWIND','SAVE  ',
     +'STOP  ','WRITE '/
C
      IF(UNFLP) RETURN
      if(nfault.ne.0) then
         WRITE(MZUNIT,500) NFAULT
      endif
      WRITE(MZUNIT,560)
C Update statement number for input file
      ISGLOB = ISGLOB + NLINES - 1
      NGLOBF = 0
C Check that module is to be processed
      IF(.NOT.RPROCS)                                           GOTO 190
C Check for comment lines after end of module
      IF(LCHECK(1).AND.NLINES-1.GT.NFIOLD) THEN
         WRITE(MZUNIT,570)
         NGLOBF = NGLOBF + 1
      ENDIF
C Check that module ended with END
      IF(LCHECK(2).AND.ICLOLD.NE.IEND.and.iclold.ne.iend+71) THEN
         WRITE(MZUNIT,580)
         NGLOBF = NGLOBF + 1
      ENDIF
      IF(LCHECK(3)) THEN
C Check for COMMON blocks remaining unused
         DO 20 IC=1,NCOMT
            IF(ICTITL(IC).GT.0) THEN
               LEN=INDEX(SCTITL(IC),' ')-1
               DO 10 IGN=1,NIGNOR
                  IF(LIGNOR(IGN).NE.LEN)                         GOTO 10
                  IF(SCTITL(IC)(:LEN).EQ.CIGNOR(IGN)(:LEN))      GOTO 20
   10          CONTINUE
               WRITE(MZUNIT,590) SCTITL(IC)(:lenocc(sctitl(ic)))
               NGLOBF = NGLOBF + 1
            ENDIF
   20    CONTINUE
      ENDIF
C Check that COMPLEX and DOUBLE PRECISION variables occur
C at the start of a COMMON block
      IF(LCHECK(4)) THEN
         CNAMOL = '        '
         DO 70 IC=1,NCOMN
            IF(SCTITL(ICNAME(IC)).NE.CNAMOL) THEN
C Change of COMMON block name ... reset counters
C NLAST = 1 signifies last variable in common was real/doublep
C NLAST = 0 signifies otherwise
               CNAMOL = SCTITL(ICNAME(IC))
               NLAST = 1
            ENDIF
            CNAM = SCNAME(IC)
            ILEN = INDEX(CNAM,' ')
C Search for NAMTYP
            MATCH = 0
            DO 30 IN=1,NRNAME
               IF(ILEN.NE.INDEX(SNAMES(IN+IRNAME),' '))          GOTO 30
               IF(CNAM.NE.SNAMES(IN+IRNAME))                     GOTO 30
               NTYP = NAMTYP(IN+IRNAME)
C The variable must be a COMMON variable (not a dimensionality)
               IF(.NOT.BTEST(NTYP,19))                           GOTO 40
               MATCH = 1
                                                                 GOTO 40
   30       CONTINUE
   40       IF(MATCH.EQ.0)                                       GOTO 70
            IF(.NOT.BTEST(NTYP,3).AND..NOT.BTEST(NTYP,4)) THEN
               NLAST = 0
            ELSE IF(NLAST.EQ.0) THEN
               DO 50 IGN=1,NIGNOR
                  IF(LIGNOR(IGN).NE.INDEX(CNAM,' ')-1)           GOTO 50
                  IF(CNAM(:LIGNOR(IGN)).EQ.CIGNOR(IGN)(:LIGNOR(IGN)))
     +                                                           GOTO 60
   50          CONTINUE
               WRITE(MZUNIT,600) CNAM(:lenocc(cnam)),
     +                           CNAMOL(:lenocc(cnamol))
               NGLOBF = NGLOBF + 1
   60          NLAST = 0
            ENDIF
   70    CONTINUE
      ENDIF
C Check for clashes in COMMON definitions
      IF(LCHECK(5)) THEN
         DO 140 IT=1,NCOMT
            ILEN1 = INDEX(SCTITL(IT),' ')-1
            DO 80 IGN=1,NIGNOR
               IF(LIGNOR(IGN).NE.ILEN1)                          GOTO 80
               IF(SCTITL(IT)(:ILEN1).EQ.CIGNOR(IGN)(:ILEN1))    GOTO 140
   80       CONTINUE
            IFOUN = 0
            DO 110 ITG=1,NGCOT
               ILEN2 = INDEX(SGCTIT(ITG),' ')-1
               IF(ILEN2.NE.ILEN1)                               GOTO 110
               IF(SCTITL(IT).NE.SGCTIT(ITG))                    GOTO 110
               IFOUN = 1
               IST1 = IABS(ICTITL(IT))
               IST2 = IABS(IGCTIT(ITG))
               DO 90 IN1=IST1,NCOMN
                  IF(ICNAME(IN1).NE.IT.AND.IGCNAM(IST2+IN1-IST1). EQ.
     +            ITG) THEN
                     WRITE(MZUNIT,510) SCTITL(IT)(:lenocc(sctitl(it)))
                     NGLOBF = NGLOBF + 1
                                                                GOTO 100
                  ENDIF
                  IF(ICNAME(IN1).NE.IT)                         GOTO 100
                  IF(IGCNAM(IST2+IN1-IST1).NE.ITG.OR. SCNAME(IN1).NE.
     +            SGCNAM (IST2+IN1-IST1)) THEN
                     WRITE(MZUNIT,510) SCTITL(IT)(:lenocc(sctitl(it)))
                     NGLOBF = NGLOBF + 1
                                                                GOTO 100
                  ENDIF
   90          CONTINUE
  100          CONTINUE
  110       CONTINUE
            IF(IFOUN.EQ.0) THEN
               NGCOT = NGCOT + 1
               IF(NGCOT.GT.MGCOT) THEN
                  WRITE(MZUNIT,520)
                                                                GOTO 140
               ENDIF
               SGCTIT(NGCOT) = SCTITL(IT)
               IST1 = NGCON + 1
               IGCTIT(NGCOT) = -IST1
               IST2 = IABS(ICTITL(IT))
               IMX = NCOMN-IST2+1
               DO 120 INEW=1,IMX
                  IF(ICNAME(IST2+INEW-1).NE.IT)                 GOTO 130
                  IF(NGCON.GE.MGCON) THEN
                     WRITE(MZUNIT,530)
                                                                GOTO 130
                  ENDIF
                  NGCON = NGCON + 1
                  IGCNAM(NGCON) = NGCOT
                  SGCNAM(NGCON) = SCNAME(IST2+INEW-1)
  120          CONTINUE
  130          CONTINUE
            ENDIF
  140    CONTINUE
      ENDIF
C Make second pass over statements in this module to check
C for statement function definitions and correct ordering
C of all statements
C
C Also check argument types of module (dimensionality etc)
C
      CALL SECPAS(NGLOBF,LIMPNO)
C Loop over routine names
      DO 180 IN=1,NRNAME
C Skip GEANT3 names if flag GALEPH
         CNAM = SNAMES(IRNAME+IN)
         lennam = lenocc(cnam)
         IF(GALEPH) THEN
            IF(CNAM(1:1).EQ.'G'.OR.CNAM(2:2).EQ.'G')            GOTO 180
         ENDIF
         DO 150 IGN=1,NIGNOR
            IF(LIGNOR(IGN).NE.INDEX(SNAMES(IRNAME+IN),' ')-1)   GOTO 150
            IF(CNAM(:LIGNOR(IGN)).EQ.CIGNOR(IGN)(:LIGNOR(IGN))) GOTO 180
  150    CONTINUE
         NTYP = NAMTYP(IRNAME+IN)
c
c Tests on functions used in this module
c
         if(btest(ntyp,16)) then
            ifound = 0
            do 300 inf=1,lif
               if(index(cinfun(inf),' ')-1.eq.lennam.and.
     &            cinfun(inf).eq.touppr(cnam)) then
                  ifound = inf
                  goto 310
               endif
  300       continue
  310       continue            
C Check for clash between intrinsic and local functions
            if(lcheck(34).and.ifound.ne.0.and.btest(ntyp,11)) then
               write(mzunit,710) cnam(:lennam),cinfun(ifound)
               nglobf = nglobf + 1
            endif
C Check for non-intrinsic functions that were not declared EXTERNAL
            if(lcheck(35).and.ifound.eq.0.and..not.btest(ntyp,11)) then
              if(cnam.ne.scrout) then
               write(mzunit,700) cnam(:lennam)
               nglobf = nglobf + 1
              endif
            endif
         endif
c
         DO 160 II=1,21
C Interrogate bit pattern for type of name IN
            IB(II)=0
            IF(BTEST(NTYP,II-1)) THEN
               IB(II)=1
            ENDIF
  160    CONTINUE
C now extract the first blank in the name
         ILEN = INDEX(CNAM,' ')-1
         IF((ILEN.GT.6.OR.ILEN.EQ.-1).AND.LCHECK(6)) THEN
            WRITE(MZUNIT,620) CNAM(:lenocc(cnam))
            NGLOBF = NGLOBF + 1
         ENDIF
C now enforce some rules
         IF(IB(20).EQ.1.AND.LCHECK(7).AND.ILEN.NE.6) THEN
C in a common block
            WRITE(MZUNIT,630) CNAM(:lenocc(cnam))
            NGLOBF = NGLOBF + 1
         ENDIF
         if(lcheck(45).and.ib(20).eq.1.and.ilen.lt.6) then
            write(mzunit,690) cnam(:lenocc(cnam))
            nglobf = nglobf + 1
         endif
         IF(LCHECK(8).AND.ILEN.GE.6.AND.IB(8)+IB(10)+IB(11)+IB(12)+ IB
     +   (13)+IB(14)+IB(15)+IB(16)+IB(17)+IB(20).EQ.0) THEN
C variable name in routine (not COMMON,FUNCTION etc)
            WRITE(MZUNIT,640) CNAM(:lenocc(cnam))
            NGLOBF = NGLOBF + 1
         ENDIF
         IF(LCHECK(43).AND.ILEN.GE.6.AND.IB(8)+IB(10)+IB(11)+IB(12)+ IB
     +   (13)+IB(14)+IB(15)+IB(16)+IB(17)+IB(20)+IB(7).EQ.0) THEN
C variable name in routine (not COMMON,FUNCTION,PARAMETER etc)
            WRITE(MZUNIT,640) CNAM(:lenocc(cnam))
            NGLOBF = NGLOBF + 1
         ENDIF
         CFRST = CNAM(1:1)
         IF(LCHECK(9).AND.IB(1).EQ.1.AND..NOT.LIMPNO) THEN
C integer name
            if(lge(cfrst,'i').and.lle(cfrst,'n')) goto 171
            if(lge(cfrst,'I').and.lle(cfrst,'N')) goto 171
            WRITE(MZUNIT,650) CNAM(:lenocc(cnam))
            NGLOBF = NGLOBF + 1
  171       continue
         ENDIF
         IF(LCHECK(9).AND.IB(2).EQ.1.AND..NOT.LIMPNO) THEN
C real name
            if((lge(cfrst,'i').and.lle(cfrst,'n')).or.
     &         (lge(cfrst,'I').and.lle(cfrst,'N'))) then
               WRITE(MZUNIT,660) CNAM(:lenocc(cnam))
               NGLOBF = NGLOBF + 1
            ENDIF
         ENDIF
C now check that the variable isn't a FORTRAN key-word
C Except in the case of 'REAL' which is a KEYWORD and a generic function
         IF(LCHECK(10)) THEN
            IF(CNAM.EQ.'REAL    ')                              GOTO 180
            if(cnam.eq.'real    ')                              goto 180
            DO 170 II=1,NFS
               ILENF = INDEX(CFORT(II),' ')-1
               IF(ILENF.LE.0) ILENF = 6
               IF(ILENF.NE.ILEN)                                GOTO 170
               IF(CNAM.EQ.CFORT(II).or.touppr(cnam).eq.cfort(ii)) THEN
                  WRITE(MZUNIT,670) CNAM(:lenocc(cnam)),
     +                              CFORT(II)(:lenocc(cfort(ii)))
                  NGLOBF = NGLOBF + 1
               ENDIF
  170       CONTINUE
         ENDIF
C
  180 CONTINUE
      WRITE(MZUNIT,540) NGLOBF
      WRITE(MZUNIT,680) SCROUT(:lenocc(scrout))
C Now rewind MZUNIT and check for non-zero errors before
C  copying to MPUNIT
  190 CONTINUE
      REWIND(MZUNIT)
      IF(NGLOBF+NFAULT.NE.0.AND.RPROCS) THEN
  200    READ(MZUNIT,550,ERR=210,END=210) CZUN
         WRITE(MPUNIT,550) CZUN
                                                                GOTO 200
  210    REWIND(MZUNIT)
         ENDFILE(MZUNIT)
         REWIND(MZUNIT)
      ENDIF
C Reset NFAULT to zero ready for next module
      NFAULT = 0
      RPROCS = .TRUE.
  500 FORMAT(/,1X,'!!! ',I3,' STATEMENT WARNING(S) IN THIS MODULE ')
  510 FORMAT(1X,'!!! WARNING ... COMMON ',A,
     +' HAS CHANGED IN DEFINITION')
  520 FORMAT(1X,'!!! NON-FATAL ERROR IN URTERM . MGCOT EXCEEDED')
  530 FORMAT(1X,'!!! NON-FATAL ERROR IN URTERM . MGCON EXCEEDED')
  540 FORMAT(/,1X,'!!! ',I3,' GLOBAL WARNING(S) IN THIS MODULE ')
  550 FORMAT(A131)
  560 FORMAT(1X,10('+'),' BEGIN GLOBAL CHECKS IN MODULE ',
     +10('+'))
  570 FORMAT(1X,'!!! WARNING ... COMMENT OR BLANK LINES AFTER "END"')
  580 FORMAT(1X,'!!! WARNING ... MODULE DOES NOT HAVE "END"')
  590 FORMAT(1X,'!!! WARNING ... COMMON ',A,
     +' DECLARED BUT NOT USED IN THIS MODULE')
  600 FORMAT(1X,'!!! WARNING ... VARIABLE ',A, ' IN COMMON ',A,
     +',COMPLEX OR DOUBLE PRECISION, SHOULD BE AT START OF COMMON')
  610 FORMAT(1X,'!!! WARNING ... VARIABLE ',A,
     +' CONTAINS "$" AND IS ILLEGAL')
  620 FORMAT(1X,'!!! WARNING ... VARIABLE ',A,
     +' HAS LENGTH OF >6 CHARACTERS')
  630 FORMAT(1X,'!!! WARNING ... VARIABLE ',A,
     +' IS IN COMMON AND IS NOT 6 CHARACTERS LONG')
  640 FORMAT(1X,'!!! WARNING ... VARIABLE ',A,
     +' IS A VARIABLE WITH LENGTH >5')
  650 FORMAT(1X,'!!! WARNING ... VARIABLE ',A,
     +' IS INTEGER BUT DOES NOT START I -> N')
  660 FORMAT(1X,'!!! WARNING ... VARIABLE ',A,
     +' IS REAL BUT STARTS WITH I -> N')
  670 FORMAT(1X,'!!! WARNING ... VARIABLE ',A,
     +' CLASHES WITH FORTRAN KEY-WORD ',A)
  680 FORMAT(1X,10('+'), ' END MODULE CHECKS FOR ',A,1x,10('+'))
  690 FORMAT(1X,'!!! WARNING ... VARIABLE ',A,
     +' IS IN COMMON AND IS LESS THAN 6 CHARACTERS LONG')
  700 FORMAT(1X,'!!! WARNING ... FUNCTION ',A,
     +' IS NOT DECLARED AS BEING "EXTERNAL"')
  710 FORMAT(1X,'!!! WARNING ... FUNCTION ',A,' CLASHES WITH ',
     +A,' (A FORTRAN INTRINSIC FUNCTION)')
      END
