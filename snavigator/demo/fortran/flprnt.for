      SUBROUTINE FLPRNT(NBLANK,SHEAD,N,STRING,NCOUNT)
*-----------------------------------------------------------------------
*
*--- writes lines onto PRINT output file (MPUNIT)
*
*--- input
*    NBLANK      # of blank lines to print in front
*    SHEAD       string to be put into header part of line 1
*    N           # of lines
*    STRING      lines
*--- input/output
*    NCOUNT      counter to be increased by N
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'usunit.h'
      CHARACTER *(*) STRING(*),SHEAD,SLOC*15
      DO 10 I=1,NBLANK
         WRITE (MZUNIT,'('' '')')
   10 CONTINUE
      SLOC=SHEAD
      WRITE(MZUNIT,'(1X,A15,A)')  SLOC,STRING(1)
      DO 20 I=2,N
         WRITE (MZUNIT,'(16X,A)') STRING(I)
   20 CONTINUE
      NCOUNT=NCOUNT+N
      END
