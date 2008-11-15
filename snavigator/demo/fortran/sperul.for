      SUBROUTINE SPERUL
      include 'param.h'
      include 'checks.h'
      include 'usunit.h'
      CHARACTER*3 CDEF,CTMP
      WRITE(MPUNIT,100) MCHEKS
  100 FORMAT(//,1X,'Interactive Specification of Rules to Check',
     &        /,1X,'-------------------------------------------',
     &        /,1X,'A maximum of ',I5,' rules may be checked',
     &        /,1X,'Answer YES or NO for each rule')
      DO 1 IRULE=1,MCHEKS
        IF(CCHECK(IRULE)(:4).EQ.'$$$$') GOTO 1
        WRITE(MPUNIT,'(A,A)') ' ',CCHECK(IRULE)
        CDEF = 'NO '
        IF(LCHECK(IRULE)) CDEF = 'YES'
        WRITE(MPUNIT,'(A,A,A)') ' Check this rule ? [CR=',CDEF,']'
        READ(MSUNIT,'(A)',END=1,ERR=1) CTMP
        IF(CTMP(1:1).EQ.'y'.OR.CTMP(1:1).EQ.'Y') LCHECK(IRULE)=.TRUE.
        IF(CTMP(1:1).EQ.'n'.OR.CTMP(1:1).EQ.'N') LCHECK(IRULE)=.FALSE.
        IF(CTMP(1:1).EQ.' '.AND.CDEF.EQ.'YES') LCHECK(IRULE)=.TRUE.
        IF(CTMP(1:1).EQ.' '.AND.CDEF.EQ.'NO ') LCHECK(IRULE)=.FALSE.
    1 CONTINUE
      RETURN
      END
