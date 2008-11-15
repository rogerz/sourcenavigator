      SUBROUTINE PUTOPA(SNAME,STYP,ICHR,ICHRE,IERR)
C! Put an operand on the stack.
      include 'stack.h'
      CHARACTER*(*) SNAME,STYP
      NLEVL = NLEVL+1
      IF(NLEVL.GT.MLEVL) GOTO 900
      CTYP(NLEVL)(:LCTYP) = STYP(:LCTYP)
      LSN = INDEX(SNAME,' ')-1
      IF(LSN.LE.0) LSN = LEN(SNAME)
      LOPD(NLEVL) = MIN(LSN,LCOPD)
      COPD(NLEVL)(:LOPD(NLEVL)) = SNAME(:LOPD(NLEVL))
      COPT(NLEVL) = ' '
      IPOS(NLEVL) = 0
      IERR = 0
      GOTO 999
  900 IERR = NLEVL
  999 CONTINUE
      RETURN
      END
