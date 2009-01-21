      SUBROUTINE TY2TYP(ISN,STYP)
C! Reduces types of operand to smaller set
      include 'param.h'
      include 'alcaza.h'
      include 'class.h'
      include 'state.h'
      include 'usinfn.h'
      LOGICAL BTEST
C
C Here we attempt to evaluate the type of a FLOP statement
C 'name' using e.g. generic intrinsic function rules etc.
C
      CHARACTER*(*) STYP
      CHARACTER*1 STYPE(7)
C I=integer R=real D=doubleprecision K=complex L=logical C=complex $=aaa
      DATA STYPE /'I','R','D','K','L','C','$'/
      STYP = STYPE(7)
      DO 10 IR=1,NRNAME
        IF(SNAMES(ISN+ISNAME).NE.SNAMES(IR+IRNAME)) GOTO 10
        NTYP = NAMTYP(IR+IRNAME)
C check for generic intrinsic function
        IF(BTEST(NTYP,16)) THEN
C marked as a function
          IFOUN = 0
          LEN = INDEX(SNAMES(IR+IRNAME),' ')-1
          DO 20 INFUN=1,LIF
            IF(CINFUN(INFUN)(:LEN).NE.SNAMES(IR+IRNAME)(:LEN)) GOTO 20
            IF(INFUNG(INFUN).EQ.0) GOTO 20
C generic function
            IFOUN = INFUN
   20     CONTINUE
          IF(IFOUN.NE.0) THEN
C? is this correct ?
            STYP = CTYFUN(IFOUN)
            RETURN
          ENDIF
        ENDIF
        IF(BTEST(NTYP,0)) THEN
          STYP = STYPE(1)
          RETURN
        ELSE IF(BTEST(NTYP,1)) THEN
          STYP = STYPE(2)
          RETURN
        ELSE IF(BTEST(NTYP,3)) THEN
          STYP = STYPE(4)
          RETURN
        ELSE IF(BTEST(NTYP,4)) THEN
          STYP = STYPE(3)
          RETURN
        ELSE IF(BTEST(NTYP,2)) THEN
          STYP = STYPE(5)
          RETURN
        ELSE IF(BTEST(NTYP,5)) THEN
          STYP = STYPE(6)
          RETURN
        ENDIF
        RETURN
   10 CONTINUE
      RETURN
      END
