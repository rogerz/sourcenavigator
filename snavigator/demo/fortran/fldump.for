      SUBROUTINE FLDUMP(NUN,N,STRING,NCOUNT)
*-----------------------------------------------------------------------
*
*--- writes lines onto output file
*
*--- input
*    NUN         output unit
*    N           # of lines
*    STRING      lines
*--- input/output
*    NCOUNT      counter to be increased by N
*
*-----------------------------------------------------------------------
      CHARACTER *(*) STRING(*)
      DO 10 I=1,N
         WRITE (NUN,'(A)') STRING(I)
   10 CONTINUE
      NCOUNT=NCOUNT+N
      END
