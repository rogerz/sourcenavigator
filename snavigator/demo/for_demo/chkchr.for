      SUBROUTINE CHKCHR
C Checks that incorrect relational operators
C are not used to compare
C character strings in IF clauses.
C INPUT ; current statement description
C OUTPUT ; NFAULT
C
      include 'param.h'
      include 'alcaza.h'
      include 'class.h'
      include 'flags.h'
      include 'cursta.h'
      include 'state.h'
      include 'usstmt.h'
      include 'usunit.h'
      include 'usltyd.h'
      include 'usigno.h'
      include 'checks.h'
      LOGICAL BTEST
      IF(UNFLP) RETURN
      IF(.NOT.LCHECK(42)) RETURN
      ICL1 = ICURCL(1)
      IF(.NOT.LIFF(ICL1)) RETURN
C Find end of IF
      ILOC = INDEX(SSTA(:NCHST),'(')
      IF(ILOC.EQ.0)  RETURN
      CALL SKIPLV(SSTA,ILOC+1,NCHST,.FALSE.,ILOCE,ILEV)
      IF(ILOCE.EQ.0) RETURN
      DO 40 I=1,NSNAME
C Looping over variable names in the statement
         IF(NSSTRT(I).GT.ILOCE) RETURN
C Variable is inside IF clause
         IF(.NOT.BTEST(NAMTYP(ISNAME+I),5))                      GOTO 40
C Character variable
         DO 10 IPOS=NSSTRT(I)-1,ILOC+1,-1
            IF(SSTA(IPOS:IPOS).EQ.' ')                           GOTO 10
            IF(SSTA(IPOS:IPOS).EQ.'(')                           GOTO 20
            IF(SSTA(IPOS:IPOS).NE.'.')                           GOTO 20
C Check for incorrect relational operators
            IF(SSTA(IPOS-3:IPOS).EQ.'.OR.')                      GOTO 20
            IF(SSTA(IPOS-3:IPOS).EQ.'.EQ.')                      GOTO 20
            IF(SSTA(IPOS-3:IPOS).EQ.'.NE.')                      GOTO 20
            IF(SSTA(IPOS-4:IPOS).EQ.'.AND.')                     GOTO 20
            IF(ILOCE-ILOC.GT.20) ILOCE=ILOC+20
            WRITE(MZUNIT,500) SSTA(ILOC:ILOCE)
            NFAULT = NFAULT + 1
            RETURN
   10    CONTINUE
   20    ILEV = 0
         DO 30 IPOS=NSEND(I)+1,ILOCE-1
            IF(SSTA(IPOS:IPOS).EQ.' ')                           GOTO 30
            IF(SSTA(IPOS:IPOS).EQ.'(') ILEV=ILEV+1
            IF(SSTA(IPOS:IPOS).EQ.')') ILEV=ILEV-1
            IF(SSTA(IPOS:IPOS).EQ.')')                           GOTO 30
            IF(ILEV.NE.0)                                        GOTO 30
            IF(SSTA(IPOS:IPOS).NE.'.')                           GOTO 40
            IF(SSTA(IPOS:IPOS+3).EQ.'.OR.')                      GOTO 40
            IF(SSTA(IPOS:IPOS+3).EQ.'.EQ.')                      GOTO 40
            IF(SSTA(IPOS:IPOS+3).EQ.'.NE.')                      GOTO 40
            IF(SSTA(IPOS:IPOS+4).EQ.'.AND.')                     GOTO 40
            IF(ILOCE-ILOC.GT.20) ILOCE=ILOC+20
            WRITE(MZUNIT,500) SSTA(ILOC:ILOCE)
            NFAULT = NFAULT + 1
            RETURN
   30    CONTINUE
   40 CONTINUE
      RETURN
  500 FORMAT(1X,'!!! WARNING ... IF CLAUSE ',A,' USES',
     +' INCORRECT RELATIONAL OPERATORS FOR CHARACTER TYPE')
      END
