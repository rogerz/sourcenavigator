      SUBROUTINE SUMMRY
*-----------------------------------------------------------------------
*
*--- Prints job summary
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'jobsum.h'
      include 'state.h'
      include 'flags.h'
      IF(ACTION(26).AND.NGNAME.GT.0)  THEN
*--- print list of global names first
C         WRITE (MPUNIT,10000) NGNAME
         IF (ACTION(20))  THEN
*--- print name list with types
C            CALL PRNAMF(IGNAME+1,IGNAME+NGNAME)
         ELSE
C            WRITE (MPUNIT,10010) (SNAMES(IGNAME+J),J=1,NGNAME)
         ENDIF
      ENDIF
C      CALL STSUMM
      IF(.NOT.STATUS(2))  THEN
C         WRITE (MPUNIT,10020)
      ENDIF
      IF(STATUS(4))  THEN
C         WRITE (MPUNIT,10030)
      ENDIF
      WRITE (MPUNIT,10040)
      WRITE (MPUNIT,10050) (NSTATC(J),J=1,8)
10000 FORMAT(//' Global list of',I6,' names'/)
10010 FORMAT(1X,10A10)
10020 FORMAT(//1X,10('*=*='),' WARNING - EOF not reached on input')
10030 FORMAT(//1X,10('*=*='),' WARNING - ending job at time limit')
10040 FORMAT(//1X,8('****'),' Job Summary ',8('****'))
10050 FORMAT(' no. of lines read                 =',I10/
     +' no. of lines out                  =',I10/
     +' no. of statements                 =',I10/
     +' no. of filtered stmts.            =',I10/
     +' no. of changed  stmts.            =',I10/
     +' no. of stmts. unable to change    =',I10/
     +' no. of comment lines              =',I10/
     +' no. of lines printed              =',I10)
10060 FORMAT(/' time (sec)              =',F10.3/
     +' time per statement(msec)=',F10.3)
      END
