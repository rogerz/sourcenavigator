      SUBROUTINE MIXMOD(NGLOBF)
C! Checks for Mixed Mode expressions
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
      include 'stack.h'
      CHARACTER*1 STYP
      CHARACTER*(LOPER) SOPT
      INTEGER ISTART(2),IFINIS(2)
      CHARACTER*6 CREL(11)
      CHARACTER*256 STEMP,touppr
      INTEGER LPS(256)
      INTEGER LREL(11)
      external touppr
      DATA CREL /'.EQV. ','.NEQV.','.OR.  ','.AND. ','.NOT. ',
     &           '.GT.  ','.GE.  ','.LT.  ','.LE.  ','.EQ.  ',
     &           '.NE.  '/
      DATA LREL /5,6,4,5,5,4,4,4,4,4,4/
C
C CALLED FROM URTERM FOR EACH STATEMENT IN THE MODULE
C
      ICL1 = ICURCL(1)
      ICL2 = ICURCL(2)
C
C RETURN UNLESS AN ASSIGNMENT STATEMENT
C
      IF(LIFF(ICL1)) THEN
        IF(.NOT.LASIGN(ICL2)) RETURN
        IUP = 2
C find end of IF
        JPT = INDEX(SSTA(:NCHST),'(')
        CALL SKIPLV(SSTA,JPT+1,NCHST,.FALSE.,KND,ILEV)
        ISTART(1) = JPT+1
        ISTART(2) = KND+1
        IFINIS(1) = KND-1
        IFINIS(2) = NCHST
      ELSE IF(LASIGN(ICL1)) THEN
        IUP = 1
        KND = NCHST
        ISTART(1) = 1
        IFINIS(1) = NCHST
      ELSE
        RETURN
      ENDIF
C loop over parts of the statement
      DO 20 IPART=1,IUP
C zero stack address
        NLEVL = 0
        IF(IPART.EQ.1) THEN
          ICL=ICL1
        ELSE
          ICL=ICL2
          IF(.NOT.LASIGN(ICL))                                   GOTO 20
        ENDIF
C KST and KND mark the start and end of the assignment part of the statement
        KST = ISTART(IPART)
        KND = IFINIS(IPART)
C       WRITE(6,'(A,A)') ' Statement : ',SSTA(KST:KND)
C this part of statement is an assignment or is inside IF clause
C move from left to right, character by character
        NLO1 = 1
        ICHR = KST
    5   CONTINUE
        IF(ICHR.EQ.KND+1) THEN
C put end of expression operator
          CALL PUTOPT('END',3,ICHR,IERR)
          IF(IERR.GT.0)                                          GOTO 25
          IF(IERR.LT.0) THEN
c            NGLOBF = NGLOBF + 1
c            write(mzunit,510) ' Operator on stack (I)'
            GOTO 40
          ENDIF
                                                                 GOTO 20
        ENDIF
        IF(SSTA(ICHR:ICHR).EQ.' ') THEN
C ignore blanks
          ICHR = ICHR + 1
          GOTO 5
        ENDIF
C NLO is the index to the statement name last found
        NLO = NLO1
        ICHRE = 0
C find if this character is start of a name
        DO 10 ISN=NLO,NSNAME
          IF(NSSTRT(ISN).NE.ICHR)                                GOTO 10
          NLO1 = ISN + 1
          ICHRE = NSEND(ISN)
C convert the name type to the smaller subset
          CALL TY2TYP(ISN,STYP)
C add this operand to the stack
          CALL PUTOPA(SNAMES(ISN+ISNAME),STYP,ICHR,ICHRE,IERR)
          IF(IERR.NE.0)                                          GOTO 30
          ICHR = ICHRE + 1
C go for the next character after this name
                                                                  GOTO 5
   10   CONTINUE
C next name is at NLO1
        IF(NLO1.GT.NSNAME) THEN
          IFIN = KND
        ELSE
          IFIN = NSSTRT(NLO1)-1
        ENDIF
        ISTA = ICHR
C analyse this part of statement (ISTA:IFIN) since it is
C not a name, may be an operator
        ILEN = IFIN-ISTA+1
        CALL GETOPT(SSTA(ISTA:IFIN),ILEN,SOPT,LOPT,IERR)
        IF(IERR.NE.0)                                            GOTO 15
C found an operator of length LOPT, called SOPT
C put the operator on the stack
        CALL PUTOPT(SOPT,LOPT,ICHR,IERR)
        IF(IERR.GT.0)                                            GOTO 15
        IF(IERR.LT.0) THEN
          write(mzunit,510) ' Operator on stack (II)'
          NGLOBF = NGLOBF + 1
        ENDIF
        ICHR = ICHR + LOPT
                                                                  GOTO 5
   15   CONTINUE
C not a name, not an operator, so
C check if start of a constant. Remove blanks first
   98   NC=0
        DO 97 IC=ISTA,IFIN
          IF(SSTA(IC:IC).EQ.' ') GOTO 97
          NC=NC+1
          LPS(NC)=IC-ISTA
          STEMP(NC:NC) = SSTA(IC:IC)
   97   CONTINUE
C remove .EQ. etc which confuse GETCON
        DO 95 IREL=1,11
          LP=INDEX(touppr(STEMP(:NC)),CREL(IREL)(:LREL(IREL)))
          IF(LP.EQ.0) GOTO 95
          IFIN = ISTA + LPS(LP) - 1
          GOTO 98
   95   CONTINUE
        CALL GETCON(touppr(SSTA),ISTA,IFIN,KLCH,STYP)
        IF(KLCH.NE.0) THEN
C found a constant. place on the stack
          CALL PUTOPA(SSTA(ISTA:KLCH),STYP,ICHR,KLCH,IERR)
          IF(IERR.NE.0)                                          GOTO 35
          ICHR = KLCH + 1
                                                                  GOTO 5
        ENDIF
C not a name,operand or constant. this is an error. type the offender
        LCST = MIN(70,NCHST)
        WRITE(MZUNIT,500) SSTA(1:LCST)
   20 CONTINUE
                                                                 GOTO 40
   25 CONTINUE
   30 CONTINUE
   35 CONTINUE
   40 CONTINUE
      RETURN
  500 FORMAT(1X,'!!! NON-FATAL ERROR IN MIXMOD ...',
     +' UNABLE TO PARSE: ',A)
  510 FORMAT(1x,'!!! NON-FATAL ERROR IN MIXMOD DOING ',A)
      END
