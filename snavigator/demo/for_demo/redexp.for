      SUBROUTINE REDEXP(IOP,IERR)
C! Reduce the expression on the stack
      include 'param.h'
      include 'cursta.h'
      include 'stack.h'
      include 'alcaza.h'
      include 'usunit.h'
      CHARACTER*(MDIMST) CTEMP
      CHARACTER*(LCOPD) SNEW
      CHARACTER*1 SNUTY
      include 'opprec.h'
C
C     WRITE(6,100)
C  100 FORMAT(//,1X,'Now reduce the expression on the stack')
C
      IERR = 0
    5 CONTINUE
      IF(NLEVL.LE.1) THEN
        IERR = 1
        GOTO 900
      ENDIF
C
       L1 = MAX(1,LOPD(NLEVL-1))
       L2 = MAX(1,INDEX(COPT(NLEVL-1),' ' )-1)
       L3 = MAX(1,LOPD(NLEVL))
       L = L1+L2+L3
C The exepression to be reduced is SNEW
       SNEW(:L)=COPD(NLEVL-1)(:L1)//COPT(NLEVL-1)(:L2)//COPD(NLEVL)(:L3)
C
C check for generic intrinsic function
C if so, then assign the type of the expression in parentheses
C to the function
C
       IF(CTYP(NLEVL-1).EQ.'$'.AND.COPT(NLEVL-1)(:1).EQ.'(') THEN
         CTYP(NLEVL-1) = CTYP(NLEVL)
       ENDIF
C
C check for mixed mode operation
C
       CALL OPRSLT(CTYP(NLEVL-1),COPT(NLEVL-1),CTYP(NLEVL),
     &             IERR,SNUTY)
       IF(IERR.EQ.1) THEN
         DO 10 ICH=1,NCHST
           CTEMP(ICH:ICH) = ' '
           IF(ICH.EQ.IPOS(NLEVL-1)) CTEMP(ICH:ICH) = '^'
   10    CONTINUE
C        WRITE(6,110) SSTA(1:NCHST),CTEMP(:NCHST)
         IFINT=MIN(NCHST,100,lenocc(ssta))
         WRITE(MZUNIT,110) SSTA(1:IFINT),CTEMP(1:IFINT)
  110    FORMAT(1X,'!!! MIXED MODE EXPRESSION (BAD OPERATOR IS MARKED)',
     &   /,1X,A,/,1X,A)
         GOTO 900
       ENDIF
C
C treat matching parantheses specially
C
       IF(COPT(NLEVL-1).EQ.'('.AND.COPER(IOP).EQ.')') THEN
         IF(L1.EQ.0) THEN
           SNUTY = CTYP(NLEVL)
         ELSE
           SNUTY = CTYP(NLEVL-1)
         ENDIF
         SNEW(:L+1) = SNEW(:L)//')'
         L = L+1
         NLEVL = NLEVL - 1
         CTYP(NLEVL) = SNUTY
         COPD(NLEVL) = SNEW
         LOPD(NLEVL) = L
         COPT(NLEVL) = ' '
         IPOP(NLEVL) = 0
         IPOS(NLEVL) = IPOS(NLEVL+1)
         GOTO 900
       ENDIF
C
       NLEVL = NLEVL-1
       CTYP(NLEVL) = SNUTY
       COPD(NLEVL) = SNEW
       LOPD(NLEVL) = L
       COPT(NLEVL) = COPER(IOP)
       IPOP(NLEVL) = ILEFP(IOP)
       IPOS(NLEVL) = IPOS(NLEVL+1)
C
       IF(IRITP(IOP).GT.IPOP(NLEVL-1)) THEN
         GOTO 900
       ENDIF
C
C continue reduction
C
      GOTO 5
  900 CONTINUE
      RETURN
      END
