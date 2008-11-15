      SUBROUTINE PRTCOM
*-----------------------------------------------------------------------
*
*   Prints common block usage and variables referenced
*   as prepared by routine PROCOM (option COMMON).
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'state.h'
      CHARACTER*(MXNMCH) SLOC(5)
      DIMENSION ILOC(5)
      IF(NCBNAM.GT.0)  THEN
         NUSE=0
         DO 10 I=1,NCBNAM
            IF(LCBNAM(I).GT.0) NUSE=NUSE+1
   10    CONTINUE
         WRITE(MPUNIT,10000) SCROUT(:lenocc(scrout)),NCBNAM,NUSE
         WRITE(MPUNIT,10010) (SCBNAM(I),LCBNAM(I),I=1,NCBNAM)
         IF(ICBPRT.GT.0) THEN
            WRITE(MPUNIT,10020) ICBPRT
            DO 40 I=1,NCBNAM
               N=0
               NT=0
               K=KCBGRP(I)
               DO 20 J=1,NCBGRP(I)
                  IF(LCBVAR(K+J).NE.0) THEN
                     N=N+1
                     NT=NT+1
                     SLOC(N)=SCBVAR(K+J)
                     ILOC(N)=LCBVAR(K+J)
                     IF(NT.EQ.ICBPRT) GOTO 30
                     IF(N.EQ.5) THEN
                        IF(NT.LE.5) THEN
                           WRITE(MPUNIT,10030) SCBNAM(I),(SLOC(M),ILOC
     +                     (M),M=1,N)
                        ELSE
                           WRITE(MPUNIT,10040) (SLOC(M),ILOC(M),M=1,N)
                        ENDIF
                        N=0
                     ENDIF
                  ENDIF
   20          CONTINUE
   30          CONTINUE
               IF(N.GT.0) THEN
                  IF(NT.LE.5) THEN
                     WRITE(MPUNIT,10030) SCBNAM(I),(SLOC(M),ILOC(M),M=1,
     +               N)
                  ELSE
                     WRITE(MPUNIT,10040) (SLOC(M),ILOC(M),M=1,N)
                  ENDIF
               ENDIF
   40       CONTINUE
         ENDIF
      ENDIF
10000 FORMAT(/' +++ routine ',A,' has ',I5,' common blocks ',
     +'of which ',I5,' are used')
10010 FORMAT('    c.b. name + no. of var. used  ',T45, A,I4,3X,A,I4, 3
     +X,A,I4,3X,A,I4,3X,A,I4/ (T45,A,I4,3X,A,I4,3X,A,I4,3X,A,I4,3
     +X,A,I4))
10020 FORMAT(/'     list of first ',I5,' common variables in each ',
     +'block + number of references'/)
10030 FORMAT('  /',A,'/',T20,5(A,I4,3X))
10040 FORMAT(T20,5(A,I4,3X))
      END
