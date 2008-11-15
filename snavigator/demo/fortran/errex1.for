      SUBROUTINE ERREX1
*-----------------------------------------------------------------------
*
*--- error exit and stop when name buffer overflow
*
*-----------------------------------------------------------------------
      include 'param.h'
      N=MXNAME
      WRITE (MPUNIT,10000) N
      STOP
10000 FORMAT (//' ++++++++++++++++++++++++++++++++++++++++'/
     +' +                                      +'/
     +' +      NAME BUFFER OVERFLOW, STOP      +'/
     +' +      ACTUAL SIZE =',I5,T41,'+'/
     +' +                                      +'/
     +' ++++++++++++++++++++++++++++++++++++++++')
      END
