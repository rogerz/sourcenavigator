      SUBROUTINE INDECS(I1,I2,*)
*-----------------------------------------------------------------------
*
*  Sub-task of routine INDECO.
*  Stores string without {} from SSTA(I1:I2) into SKYSTR,
*  sets NKYSTR, LKYSTR, KKYSTA, KKYEND.
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'keycom.h'
*
      L=I2-I1-1
      IF(NKYSTR.EQ.MXKNAM.OR.LKYSTR+L.GT.MDIMST)  THEN
         WRITE (MPUNIT,10000) NKYSTR,MXKNAM,MDIMST
         RETURN 1
      ENDIF
      NKYSTR=NKYSTR+1
      KKYSTA(NKYSTR)=LKYSTR+1
      SKYSTR(LKYSTR+1:LKYSTR+L)=SSTA(I1+1:I2-1)
      LKYSTR=LKYSTR+L
      KKYEND(NKYSTR)=LKYSTR
10000 FORMAT(/1X,8('*-*-'),' WARNING - no. of strings in commands =',
     +I5,' has reached maximum =',I5/ 33X,
     +' or total length has reached maximum =',I5,' rest ignored')
      END
