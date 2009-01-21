      SUBROUTINE PUTOPT(SOPT,LOPT,ICHR,IERR)
C! Put an operator on the stack
      include 'stack.h'
      CHARACTER*(*) SOPT
      include 'opprec.h'
C
C Here we use the operator precedence for Fortran to determine
C whether the addition of this operator will cause the stack
C to be reduced. Note both right and left precedence is needed.
C Thanks to Julian Blake for explaining operator precedence in f77.
C
      IERR = 0
      DO 10 I=1,LOPS
         IF(ILENO(I).NE.LOPT)                                 GOTO 10
         IF(SOPT(:LOPT).EQ.COPER(I)(:LOPT))                      GOTO 20
   10 CONTINUE
      IERR = 1
C not found ... not an operator
                                                                 GOTO 30
   20 CONTINUE
C found. Operator number I
      IOP = I
      IPREC = IRITP(IOP)
C check if operator already present
      IF(COPT(NLEVL)(:1).NE.' ') THEN
         NLEVL = NLEVL + 1
         CTYP(NLEVL) = '$'
         COPD(NLEVL)(:LCOPD) = ' '
         LOPD(NLEVL) = 0
         COPT(NLEVL)(:LOPER) = ' '
         COPT(NLEVL)(:LOPT) = SOPT(:LOPT)
         IPOP(NLEVL) = ILEFP(IOP)
         IPOS(NLEVL) = ICHR-LOPT+1
         IERR = 0
                                                                 GOTO 30
      ENDIF
C place operator on stack
      COPT(NLEVL)(:LOPER) = ' '
      COPT(NLEVL)(:LOPT) = SOPT(:LOPT)
      IPOP(NLEVL) = ILEFP(IOP)
      IPOS(NLEVL) = ICHR-LOPT+1
C check for reduction of stack
      IF(NLEVL.EQ.1) THEN
         IERR = 0
                                                                 GOTO 30
      ENDIF
      IF(IRITP(IOP).GT.IPOP(NLEVL-1)) THEN
         IERR = 0
                                                                 GOTO 30
      ENDIF
C expression must be reduced
      CALL REDEXP(IOP,IERR)
      IERR = -IERR
   30 CONTINUE
      END
