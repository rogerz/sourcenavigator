      SUBROUTINE CHKOBS(CNAME,IWARN)
C
C Check that input CNAME (subroutine or function name) is
C not contained in list of obsolete CERN program library
C routines. If it is, set IWARN > 0, otherwise = 0.
C
C JJB march 86
C
      PARAMETER (LOBSO=176,LOBS1=90)
      CHARACTER*6   COBSOL(LOBSO)
      CHARACTER*(*) CNAME
C
C 'LOBSO' OBSOLETE PROGRAM LIBRARY ROUTINES FOR FLOPPY CHECKS.
C FROM CNL-180 AND LIST (B101,D114,F106,G112,G903,Z035,Z041,Z203,
C                        C327/330,E402/405)
C
      DATA (COBSOL(I),I=1,LOBS1) / 'ABEND ','AFROMI','ATG   ','BESIN ',
     +'BESJN ', 'BITBYT','CBYT  ','CCMAD ','CCMAD1','CCMAD2', 'CCMAD3',
     +'CCMPY ','CCMPY1','CCMPY2','CCMPY3', 'CCMUB ','CCMUB1','CCMUB2',
     +'CCMUB3','CHCOF1', 'CHECOF','CHMOVE','CHSUM1','CHSUM2','CMXPAK',
     +'CRMAD ','CRMAD1','CRMAD2','CRMAD3','CRMPY ', 'CRMPY1','CRMPY2',
     +'CRMPY3','CRMUB ','CRMUB1', 'CRMUB2','CRMUB3','DBESIN','DBESJN',
     +'DIGITN', 'DOTI  ','FLOARG','FUNLAN','GENLAN','GETSST', 'HIST  ',
     +'IDENZB','IDIGIT','IFROMA','INTARG', 'IOFMAS','IUBACK','IUEND ',
     +'IUFORW','IULOOK', 'IUMODE','IUNEXT','JBIT  ','JBYT  ','JBYTET',
     +'JRSBYT','LINEQ1','LINEQ2','LOCF  ','LOCHAR', 'MATIN1','MATIN2',
     +'MXEQU ','MXEQU1','MXMAD ', 'MXMAD1','MXMAD2','MXMAD3','MXMLRT',
     +'MXMLTR', 'MXMPY ','MXMPY1','MXMPY2','MXMPY3','MXMUB ', 'MXMUB1',
     +'MXMUB2','MXMUB3','MXTRP ','MXUTY ', 'NOARG ','PKCHAR','QNEXTE',
     +'RANNOR','RCMAD '/
      DATA (COBSOL(I),I=LOBS1+1,LOBSO) / 'RCMAD1','RCMAD2','RCMAD3',
     +'RCMPY ','RCMPY1', 'RCMPY2','RCMPY3','RCMUB ','RCMUB1','RCMUB2',
     +'RCMUB3','RIWIAD','RRMAD ','RRMAD1','RRMAD2', 'RRMAD3','RRMPY ',
     +'RRMPY1','RRMPY2','RRMPY2', 'RRMPY3','RRMUB ','RRMUB1','RRMUB2',
     +'RRMUB3', 'SBIT  ','SBIT0 ','SBIT1 ','SBYT  ','SBYTOR', 'SETFMT',
     +'SMXINV','SORTX ','STAP  ','SYMINV', 'TLERR ','TLRES ','TLS   ',
     +'TLSC  ','TRAAT ', 'TRAL  ','TRALT ','TRAS  ','TRASAT','TRATA ',
     +'TRATS ','TRATSA','TRCHLU','TRCHUL','TRINV ', 'TRLA  ','TRLTA ',
     +'TRPCK ','TRQSQ ','TRSA  ', 'TRSAT ','TRSINV','TRSMLU','TRSMUL',
     +'TRUPCK', 'UBITS ','UBLANK','UBLOW ','UBLOW1','UBNCH1', 'UBUNCH',
     +'UCOPIV','UCOPY2','UCTOH ','UCTOH1', 'UFILL ','UFLINT','UHOLLR',
     +'UHTOC ','UHTOC1', 'ULEFT ','UOPT  ','UPKCH ','URIGHT','USET  ',
     +'USWOP ','UTRANS','UZERO ','VBLANK','VOMAS ', 'XINOUT'/
      IWARN = 0
C-----------------------------------------------------------------------
C AFTER M.METCALF
C-----------------------------------------------------------------------
      IPOS=0
      LAST=0
      N=LOBSO
      IF(N.GT.0)  THEN
        KPOS=0
    5   M=(N+1)/2
        LAST=KPOS+M
        IF (CNAME.LT.COBSOL(LAST)) THEN
          N=M
          LAST=LAST-1
          IF (N.GT.1) GOTO 5
        ELSEIF (CNAME.GT.COBSOL(LAST)) THEN
          KPOS=LAST
          N=N-M
          IF (N.GT.0) GOTO 5
        ELSE
          IWARN=LAST
        ENDIF
      ENDIF
      RETURN
      END
