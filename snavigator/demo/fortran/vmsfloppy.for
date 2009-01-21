C***********************************************************************
C This version of FLOPPY dated October 1993. Version 7.0
C (c) J.J.Bunn 1991,1992,1993
C Contact JULIAN@vxcrna.cern.ch or VXCERN::JULIAN for more documentation
C***********************************************************************
      PROGRAM FLOPPY
C-----------------------------------------------------------------------
C Floppy VAX VMS interface routine.
C Sets up various required input files for Floppy.
C
C Julian Bunn 1987
C-----------------------------------------------------------------------
      PARAMETER (MLEN=256,MXLIN=80)
      include 'param.h'
      include 'usunit.h'
      include 'usigno.h'
      include 'ushtml.h'
      INTEGER STATUS,CLI$GET_VALUE,CLI$PRESENT
      INTEGER LIB$FIND_FILE,LIB$FIND_FILE_END
      INCLUDE '($SSDEF)'
      INCLUDE '($RMSDEF)'
      INCLUDE '($LBRDEF)'
      EXTERNAL CLI$_PRESENT,CLI$_DEFAULTED,CLI$_ABSENT,CLI$_NEGATED
      CHARACTER*(MXLIN) CIN,CINS,CIN2,CARD
      CHARACTER*(MLEN)  CFILE,CIFOR,CSCRT,CIGNO,CFORT,CFLOP,CTREE,CTEMP
      character*(mlen)  chtml,chtml_dict
      CHARACTER*(MXLIN) CTEMPL
      CHARACTER*(MLEN) CFORAN
      LOGICAL LOG,tidy,fexist
      CHARACTER*(MLEN) CMMND
C
      LOG = .FALSE.
      tidy = .false.
      html = .false.
C
C LOG
C
      STATUS = CLI$PRESENT('LOG')
      IF(STATUS.EQ.%LOC(CLI$_PRESENT).OR.
     &   STATUS.EQ.%LOC(CLI$_DEFAULTED)) THEN
        LOG = .TRUE.
      ENDIF
C
C INPUT FORTRAN
C
      STATUS = CLI$GET_VALUE('P1',CIN)
      IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
      IB = INDEX(CIN,']')
      IF(IB.EQ.0) THEN
        IP = INDEX(CIN,'.')
      ELSE
        IP = INDEX(CIN(IB:),'.')
      ENDIF
      IF(IP.EQ.0) CIN = CIN(:LENOCC(CIN))//'.FOR'
      IF(LOG) WRITE(6,'(2A)') ' Floppy --> Input Fortran  :',
     &                         CIN(:LENOCC(CIN))
C
C EXTRACT STEM NAME
C
      NFILE = 0
   88 CONTINUE
      STATUS = LIB$FIND_FILE(CIN,CTEMP,I)
      IF(.NOT.STATUS.AND.NFILE.EQ.0) THEN
        IF(LOG)WRITE(6,'(3A)') ' File ',CIN(:LENOCC(CIN)),' absent !'
        CALL LIB$SIGNAL(%VAL(STATUS))
        GOTO 1000
      ENDIF
      IF(.NOT.STATUS) GOTO 99
      NFILE = NFILE + 1
      IF(NFILE.EQ.1) THEN
        CIFOR = CTEMP
        IPOSE = INDEX(CTEMP,']')
        IPOSD = INDEX(CTEMP(IPOSE:MLEN),'.')
        IF(IPOSE.EQ.0.OR.IPOSD.EQ.0) GOTO 998
        CFILE = CTEMP(IPOSE+1:IPOSE+IPOSD-2)
        LEN   = IPOSD-2
      ELSE IF(NFILE.EQ.2) THEN
        OPEN(11,FILE='FLOPTEMP.FOR',STATUS='SCRATCH',ERR=999)
        OPEN(66,FILE=CIFOR(:LENOCC(CIFOR)),STATUS='OLD',READONLY)
   77   READ(66,'(A)',END=76,ERR=76) CARD
        WRITE(11,'(A)') CARD
        GOTO 77
   76   CLOSE(66)
      ELSE
        OPEN(66,FILE=CTEMP(:LENOCC(CTEMP)),STATUS='OLD',READONLY)
   75   READ(66,'(A)',END=74,ERR=74) CARD
        WRITE(11,'(A)') CARD
        GOTO 75
   74   CLOSE(66)
      ENDIF
      GOTO 88
   99 STATUS = LIB$FIND_FILE_END(I)
      IF(NFILE.GT.1) REWIND(11)
      IF(LOG)WRITE(6,'(A,I2,A)') ' Floppy --> ',NFILE,
     &       ' file(s) of input FORTRAN'
C
C OPEN FLOP INPUT FILE
C
      CSCRT = CFILE(:LEN)//'.FLOPINP'
      OPEN(MCUNIT,FILE=CSCRT(:LEN+8),ACCESS='SEQUENTIAL',
     &     CARRIAGECONTROL='LIST',STATUS='SCRATCH',ERR=999)
C
C WRITE USUAL FLOP INPUT CARDS
C
      WRITE(MCUNIT,'(A)') 'LIST,GLOBAL,TYPE;'
      WRITE(MCUNIT,'(A)') 'PRINT,ILLEGAL;'
      WRITE(MCUNIT,'(A)') 'OPTIONS,USER;'
C
C IGNORE FILE
C
      IOLD = 0
      CIGNO = CFILE(:LEN)//'.FLOPIGN'
      STATUS = CLI$PRESENT('OLD')
      IF(STATUS.EQ.%LOC(CLI$_PRESENT)) THEN
        STATUS = CLI$GET_VALUE('OLD',CIN)
        IF(.NOT.STATUS) THEN
          CIN = CIGNO
        ENDIF
        IOLD = 1
      ENDIF
      STATUS = LIB$FIND_FILE(CIN,CTEMP,I)
      IF(.NOT.STATUS.AND.IOLD.EQ.1) THEN
      IF(LOG)WRITE(6,'(3A)') ' File ',CIN(:LENOCC(CIN)),' absent !'
        CALL LIB$SIGNAL(%VAL(STATUS))
        GOTO 999
      ENDIF
      STATUS = LIB$FIND_FILE_END(I)
      IOPIG = 0
      IF(IOLD.EQ.1) THEN
C
C COPY OLD IGNORE FILE INTO BUFFER
C
        ICHK = 0
        OPEN(94,FILE=CIN,READONLY,STATUS='OLD')
        REWIND(94)
        LIGNO = INDEX(CIGNO,' ')-1
        OPEN(15,FILE=CIGNO(:LIGNO),ACCESS='SEQUENTIAL',
     &       STATUS='SCRATCH',ERR=999)
        IOPIG = 1
   10   READ(94,'(A)',ERR=20,END=20) CARD
        WRITE(15,'(A)') CARD
        IF(INDEX(CARD,'CHECK RULE').NE.0) ICHK = 1
        GOTO 10
   20   CONTINUE
        CLOSE(94)
        CIGNO = CIN
      ENDIF
      LIGNO = LENOCC(CIGNO)
      IF(LOG)WRITE(6,'(A,A)')
     &' Floppy --> Ignore File    :',CIGNO(:LIGNO)
C
C FLOPPY OUTPUT
C
      CFLOP = ' '
      STATUS = CLI$PRESENT('OUTPUT')
      IF(STATUS.EQ.%LOC(CLI$_PRESENT)) THEN
        CFLOP = CFILE(:LEN)//'.FLOPOUT'
        STATUS = CLI$GET_VALUE('OUTPUT',CIN)
        IF(STATUS) CFLOP = CIN
        IF(LOG)WRITE(6,'(A,A)') ' Floppy --> Output Listing :',
     &                   CFLOP(:LENOCC(CFLOP))
      ENDIF
C
C SOURCE FILE NUMBERS
C
      STATUS = CLI$PRESENT('FULL')
      IF(STATUS.EQ.%LOC(CLI$_PRESENT)) THEN
        IF(LOG)WRITE(6,'(A,A)')
     &  ' Floppy --> List source code line numbers'
        IF(IOPIG.EQ.0) OPEN(15,FILE=CIGNO(:LIGNO),ACCESS='SEQUENTIAL',
     &                 STATUS='NEW',ERR=999)
        IOPIG = 1
        WRITE(15,'(A)') '*FULL'
      ENDIF
C
C IGNORABLE NAMES
C
      STATUS = CLI$PRESENT('IGNORE')
      IF(STATUS.EQ.%LOC(CLI$_PRESENT)) THEN
        IF(LOG)WRITE(6,'(A,A)') ' Floppy --> Ignore following names'
        IF(IOPIG.EQ.0) OPEN(15,FILE=CIGNO(:LIGNO),ACCESS='SEQUENTIAL',
     &                 STATUS='NEW',ERR=999)
        IOPIG = 1
        NVALU = 0
   50   STATUS = CLI$GET_VALUE('IGNORE',CIN)
        IF(STATUS.NE.%LOC(CLI$_ABSENT)) THEN
          WRITE(15,'(A)') CIN(:LENOCC(CIN))
          NVALU = NVALU+1
          IF(LOG)WRITE(6,'(A,I3,A,A)')
     &    ' Floppy --> Ignore name',NVALU,' = ',CIN(:LENOCC(CIN))
          GOTO 50
        ENDIF
      ENDIF
C
C SPECIAL PROCESSING
C
      iset_checks = 0
      STATUS = CLI$PRESENT('SPECIAL')
      IF(STATUS.EQ.%LOC(CLI$_PRESENT)) THEN
        STATUS= CLI$GET_VALUE('SPECIAL',CIN)
        IF(IOPIG.EQ.0) OPEN(15,FILE=CIGNO(:LIGNO),ACCESS='SEQUENTIAL',
     &                 STATUS='NEW',ERR=999)
        IOPIG = 1
        WRITE(15,'(A)') '*'//CIN(:20)
        IF(LOG)WRITE(6,'(A,A)')
     &   ' Floppy --> Invoke special process for  :',
     &                   CIN(:LENOCC(CIN))
        if (cin(:5).eq.'ALEPH'.or.cin(:5).eq.'ATLAS') iset_checks=1
c      ELSE IF(STATUS.EQ.%LOC(CLI$_DEFAULTED)) THEN
c        IF(IOPIG.EQ.0) OPEN(15,FILE=CIGNO(:LIGNO),ACCESS='SEQUENTIAL',
c     &                 STATUS='NEW',ERR=999)
c        IOPIG = 1
c        WRITE(15,'(A)') '*CHECK RULE *'
c        IF(LOG)WRITE(6,'(A)')
c     &   ' Floppy --> Check standard set of rules'
      ENDIF
C
C RULE CHECKING
C
      STATUS = CLI$PRESENT('CHECKS')
      IF(STATUS.EQ.%LOC(CLI$_DEFAULTED)) THEN
        IF(IOPIG.EQ.0) OPEN(15,FILE=CIGNO(:LIGNO),ACCESS='SEQUENTIAL',
     &                 STATUS='NEW',ERR=999)
        IOPIG = 1
        IF(ICHK.EQ.0.and.iset_checks.eq.0) THEN
           WRITE(15,'(A)') '*CHECK RULE *'
           IF(LOG) WRITE(6,'(A)')
     &     ' Floppy --> Check standard set of rules'
        ELSE if(ichk.ne.0) then
           IF(LOG) WRITE(6,'(A,A)') ' Floppy --> Check rules ',
     &                              'specified in OLD file'
        ENDIF
      ELSE IF(STATUS.EQ.%LOC(CLI$_NEGATED)) THEN
        IF(IOPIG.EQ.0) OPEN(15,FILE=CIGNO(:LIGNO),ACCESS='SEQUENTIAL',
     &                 STATUS='NEW',ERR=999)
        IOPIG = 1
        WRITE(15,'(A)') '*CHECK RULE -99'
        IF(LOG) WRITE(6,'(A)') ' Floppy --> No rule checking'
      ELSE IF(STATUS.EQ.%LOC(CLI$_PRESENT)) THEN
        IF(IOPIG.EQ.0) OPEN(15,FILE=CIGNO(:LIGNO),ACCESS='SEQUENTIAL',
     &                 STATUS='NEW',ERR=999)
        IOPIG = 1
        CTEMPL(:MXLIN) = ' '
        NRULE = 0
   30   STATUS = CLI$GET_VALUE('CHECKS',CIN)
        IF(STATUS.NE.%LOC(CLI$_ABSENT)) THEN
          IF(LENOCC(CIN).EQ.1) CIN(:2) = ' '//CIN(:1)
          IF(INDEX(CIN,'-').EQ.0.OR.LENOCC(CIN).EQ.2) THEN
             WRITE(15,'(A,A)') '*CHECK RULE  ',CIN
          ELSE
             WRITE(15,'(A,A)') '*CHECK RULE ',CIN
          ENDIF
          IF(CTEMPL.NE.' ') THEN
             CTEMPL = CTEMPL(:LENOCC(CTEMPL))//','//CIN(:LENOCC(CIN))
          ELSE
             CTEMPL = CIN(:LENOCC(CIN))
          ENDIF
          NRULE = NRULE + 1
          IF(LENOCC(CTEMPL).GT.MXLIN-20) THEN
            IF(LOG) WRITE(6,'(A,I2,A)') ' Floppy --> Check ',NRULE,
     &              ' rules :'//CTEMPL(:LENOCC(CTEMPL))
            CTEMPL(:MXLIN) = ' '
          ENDIF
          GOTO 30
        ENDIF
        IF(LOG.AND.LENOCC(CTEMPL).GT.0)
     &          WRITE(6,'(A,I2,A)')' Floppy --> Check ',NRULE,
     &          ' rules :'//CTEMPL(:LENOCC(CTEMPL))
      ENDIF
C
C TREE PROGRAM
C
      STATUS = CLI$PRESENT('TREE')
      CTREE = ' '
      IF(STATUS.EQ.%LOC(CLI$_PRESENT)) THEN
        WRITE(MCUNIT,'(A)') 'OPTIONS,TREE;'
        CTREE = CFILE(:LEN)//'.FLOPTRE'
        IF(LOG)WRITE(6,'(A,A)')
     &  ' Floppy --> Tree output    : ',CTREE(:LENOCC(CTREE))
      ENDIF
c
c HTML option
c
      status = cli$present('HTML')
      if(status.eq.%loc(cli$_present)) then
         html = .true.
         Chtml = CFILE(:LEN)//'.html'
         if(log) write(6,'(A)') ' Floppy --> Generate HTML in '//
     &           chtml(:lenocc(chtml))
         status = cli$present('ANCHORS')
         if(status.eq.%loc(cli$_negated).or.
     &      status.eq.%loc(cli$_absent)) then
            anchor_list = 0
            if(log)write(6,'(a)') ' Floppy --> No Anchor Dictionary'
            goto 24
         else 
            if(log)write(6,'(a)') 
     &      ' Floppy --> Use or create Anchor Dictionary'
            anchor_list = 2
         endif
         status = cli$get_value('ANCHORS',cin)
         if(status) then
           Chtml_dict = cin
         else
           Chtml_dict = cfile(:len)//'.htmldict'
         endif
         if(log)write(6,'(a)') ' Floppy --> Anchor Dictionary '//
     &          chtml_dict(:lenocc(chtml_dict))
         if(anchor_list.eq.2) then
            inquire(file=chtml_dict(:lenocc(chtml_dict)),exist=fexist)

            if(fexist) then

            if(log)write(6,'(a)') ' Floppy --> Reading anchors ...'
            open(mdunit,file=chtml_dict(:lenocc(chtml_dict)),
     &      status='old',readonly,err=999)
            nanchr = 0
   22       read(mdunit,'(a)',end=23,err=999) ctemp
            ibl = index(ctemp,' ')
            if(ibl.eq.0) goto 22
            if(nanchr.ge.mxanch) then
               status=lib$put_output('Floppy --> Too many anchors')
               goto 23
            endif
            nanchr = nanchr+1
            canchr(nanchr) = ctemp(:ibl-1)
            cancht(nanchr) = ctemp(ibl+1:lenocc(ctemp))
            goto 22
   23       close(mdunit)
            if(log)write(6,'(a,i4,a)') ' Floppy - a total of ',
     &             nanchr,' anchor(s) were found.'
            open(mdunit,file=chtml_dict(:lenocc(chtml_dict)),
     &      status='old',access='append',err=999)

            else

            open(mdunit,file=chtml_dict(:lenocc(chtml_dict)),
     &      status='new',carriagecontrol='list',err=999)

            endif
         endif   
   24    continue
      endif
C
C TIDY OPTION
C
      STATUS = CLI$PRESENT('TIDY')
      IF(STATUS.NE.%LOC(CLI$_PRESENT)) goto 100
      tidy = .true.
      IF(LOG)WRITE(6,'(A,A)')   ' Floppy --> FLOP options to tidy code '
C
C OUTPUT FORTRAN
C
      CFORT = CFILE(:LEN)//'.FLOPFOR'
      STATUS = CLI$PRESENT('FORTRAN')
      IF(STATUS.EQ.%LOC(CLI$_PRESENT)) THEN
        STATUS = CLI$GET_VALUE('FORTRAN',CIN)
        IF(STATUS) CFORT = CIN
      ENDIF
      WRITE(MCUNIT,'(A)') 'OUTPUT,FULL,COMPRESS;'
      IF(LOG)WRITE(6,'(A,A)') ' Floppy --> Output Fortran :',
     &                 CFORT(:LENOCC(CFORT))
C
C INDENT OPTION
C
      STATUS = CLI$PRESENT('INDENT')
      IF(STATUS.EQ.%LOC(CLI$_PRESENT)) THEN
        STATUS = CLI$GET_VALUE('INDENT',CIN)
        IF(LOG)WRITE(6,'(A,A)')
     &  ' Floppy --> Indent by ',CIN(:LENOCC(CIN))
        WRITE(MCUNIT,'(A)') 'OPTIONS,INDENT='//CIN(:LENOCC(CIN))//';'
      ENDIF
C
C GROUPF
C
      STATUS = CLI$PRESENT('GROUPF')
      IF(STATUS.EQ.%LOC(CLI$_PRESENT)) THEN
        IF(LOG)WRITE(6,'(A)')
     &  ' Floppy --> Group FORMAT at end of module'
        WRITE(MCUNIT,'(A)') 'STATEMENTS,SEPARATE;'
      ENDIF
C
C GOTOS
C
      STATUS = CLI$PRESENT('GOTOS')
      IF(STATUS.EQ.%LOC(CLI$_PRESENT)) THEN
        IF(LOG)WRITE(6,'(A)') ' Floppy --> Shift GOTOs to the right'
        WRITE(MCUNIT,'(A)') 'STATEMENTS,GOTO;'
      ENDIF
C
C RENUMBER FORMATS
C
      STATUS = CLI$PRESENT('FORMAT')
      IF(STATUS.EQ.%LOC(CLI$_PRESENT)) THEN
        STATUS = CLI$PRESENT('FORMAT.START')
        CINS = '500'
        IF(STATUS.EQ.%LOC(CLI$_PRESENT).OR.
     &     STATUS.EQ.%LOC(CLI$_DEFAULTED)) THEN
          STATUS = CLI$GET_VALUE('FORMAT.START',CIN)
          IF(LOG)WRITE(6,'(A,A)')
     &    ' Floppy --> Renumber FORMAT, start at ',
     &                      CIN(:LENOCC(CIN))
          CINS = CIN
        ENDIF
        STATUS = CLI$PRESENT('FORMAT.STEP')
        CIN2 = '10'
        IF(STATUS.EQ.%LOC(CLI$_PRESENT).OR.
     &     STATUS.EQ.%LOC(CLI$_DEFAULTED)) THEN
          STATUS = CLI$GET_VALUE('FORMAT.STEP',CIN)
          IF(LOG)WRITE(6,'(A,A)')
     &    ' Floppy --> Renumber FORMAT, step by ',
     &                     CIN(:LENOCC(CIN))
          CIN2 = CIN
        ENDIF
        WRITE(MCUNIT,'(A,A)') 'STATEMENTS,FORMAT='//
     &                   CINS(:LENOCC(CINS))//','
     &                  ,CIN2(:LENOCC(CIN2))//';'
      ENDIF
C
C RENUMBER STATEMENTS
C
      STATUS = CLI$PRESENT('STMNTS')
      IF(STATUS.EQ.%LOC(CLI$_PRESENT)) THEN
        STATUS = CLI$PRESENT('STMNTS.START')
        CINS = '10'
        IF(STATUS.EQ.%LOC(CLI$_PRESENT).OR.
     &     STATUS.EQ.%LOC(CLI$_DEFAULTED)) THEN
          STATUS = CLI$GET_VALUE('STMNTS.START',CIN)
          IF(LOG)WRITE(6,'(A,A)')
     &    ' Floppy --> Renumber STATEMENTS, start at',
     &                     CIN(:LENOCC(CIN))
          CINS = CIN
        ENDIF
        STATUS = CLI$PRESENT('STMNTS.STEP')
        CIN2 = '10'
        IF(STATUS.EQ.%LOC(CLI$_PRESENT).OR.
     &     STATUS.EQ.%LOC(CLI$_DEFAULTED)) THEN
          STATUS = CLI$GET_VALUE('STMNTS.STEP',CIN)
          IF(LOG)WRITE(6,'(A,A)')
     &    ' Floppy --> Renumber STATEMENTS, step by ',
     &                     CIN(:LENOCC(CIN))
          CIN2 = CIN
        ENDIF
        WRITE(MCUNIT,'(A,A)') 'STATEMENTS,NUMBER='//
     &                   CINS(:LENOCC(CINS))//','
     &                  ,CIN2(:LENOCC(CIN2))//';'
      ENDIF
C
      WRITE(MCUNIT,'(A)') 'END;'
C
  100 CONTINUE
C
      IF(LOG)WRITE(6,'(A)')
     &' Floppy --> Finished parsing command string'
C
C
C open LUNs for FLOPPY
C
      IF(NFILE.EQ.1) THEN
      OPEN(11,FILE=CIFOR(:LENOCC(CIFOR)),READONLY,STATUS='OLD',ERR=999)
      ENDIF
      OPEN(MZUNIT,FILE='FLOPTEMP.TXT',STATUS='SCRATCH',ERR=999)
      IF(IOPIG.NE.0) THEN
        REWIND(15)
      ELSE
        OPEN(15,FILE='FLOPTEMP.IGN',STATUS='SCRATCH',ERR=999)
      ENDIF
c
      if(tidy) then
        OPEN(MOUNIT,FILE=CFORT(:LENOCC(CFORT)),STATUS='NEW',
     &       CARRIAGECONTROL='LIST',ERR=999)
      else if(html) then
        OPEN(MhUNIT,FILE=Chtml(:LENOCC(Chtml)),STATUS='NEW',
     &       carriagecontrol='list',recl=500,err=999)
      else
        OPEN(MOUNIT,FILE='FLOPTEMP.FOR',STATUS='SCRATCH',ERR=999)
      ENDIF

      IOUT = 0
      IF(CFLOP.NE.' ') THEN
        OPEN(6,FILE=CFLOP(:LENOCC(CFLOP)),STATUS='NEW',ERR=999)
        IOUT = 1
      ENDIF
      ITRE = 0
      IF(CTREE.NE.' ') THEN
        OPEN(MJUNIT,FILE=CTREE(:LENOCC(CTREE)),STATUS='NEW',
     &       FORM='UNFORMATTED',ERR=999)
        ITRE = 1
      ENDIF
      REWIND(MCUNIT)
C
C now call floppy
C
      CALL ALLPRO
C
      CLOSE(15)
      IF(ITRE.EQ.1) CLOSE(MJUNIT)
      IF(tidy.or.html) CLOSE(MOUNIT)
      if(html.and.anchor_list.ne.0) close(mdunit)
      CLOSE(11)
      CLOSE(MZUNIT)
      IF(IOUT.EQ.1) CLOSE(6)
C
      GOTO 2000
C
  998 CONTINUE
      WRITE(6,'(A)') ' Error parsing source Fortran name '
      GOTO 1000
  999 CONTINUE
      WRITE(6,'(A)') ' Error opening a Floppy file '
 1000 WRITE(6,500)
  500 FORMAT( /,1X,'***********************************************',
     &        /,1X,'*                 F L O P P Y                 *',
     &        /,1X,'*                   ABORTED                   *',
     &        /,1X,'*          in job preparation stage.          *',
     &        /,1X,'***********************************************')
 2000 CONTINUE
      CALL SYS$EXIT(%VAL(1))
      END
