      SUBROUTINE UTTERM
*-----------------------------------------------------------------------
*
*--- user total termination
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'class.h'
      include 'cursta.h'
      include 'flwork.h'
      include 'keycom.h'
      include 'typdef.h'
      include 'jobsum.h'
      include 'state.h'
      include 'flags.h'
      include 'usigno.h'
      include 'usinfn.h'
      include 'checks.h'
      LOGICAL BTEST
      CHARACTER*(MXNMCH) CNAM,touppr
      external touppr
      IF(UNFLP) RETURN
      WRITE(MPUNIT,500)
      DO 70 I=1,NGNAME
         NTYP = NAMTYP(IGNAME+I)
         CNAM = SNAMES(IGNAME+I)
         DO 10 IGN=1,NIGNOR
            IF(LIGNOR(IGN).NE.INDEX(CNAM,' ')-1)                 GOTO 10
            IF(CIGNOR(IGN)(:LIGNOR(IGN)).EQ.CNAM(:LIGNOR(IGN)))  GOTO 70
   10    CONTINUE
C check for use of obsolete CERN library routines
         IF(LCHECK(33).AND.(BTEST(NTYP,16).OR.BTEST(NTYP,14))) THEN
            CALL CHKOBS(touppr(CNAM),IWARN)
            IF(IWARN.NE.0) WRITE(MPUNIT,560) CNAM(:lenocc(cnam))
         ENDIF
         IF(LCHECK(32).AND.BTEST(NTYP,7)) THEN
C sort common block names.
            DO 20 II=0,19
               IF(II.EQ.7)                                       GOTO 20
               IF(BTEST(NTYP,II)) WRITE(MPUNIT,510) CNAM(:lenocc(cnam))
   20       CONTINUE
         ENDIF
         IF(BTEST(NTYP,16)) THEN
C FUNCTION
            ILEN = INDEX(CNAM,' ')-1
               DO 30 INF=1,LIF
                  IF(INDEX(CINFUN(INF),' ')-1.EQ.ILEN) THEN
                     IF(CINFUN(INF).EQ.touppr(CNAM)) THEN
                        IF(LCHECK(34).AND.BTEST(NTYP,11))
     &          WRITE(MPUNIT,520) CNAM(:lenocc(cnam))
                                                                 GOTO 40
                     ENDIF
                  ENDIF
   30          CONTINUE
   40       CONTINUE
         ENDIF
C Check for clashes between SUBROUTINE,BLOCKDATA,PROGRAM,ENTRY,FUNCTION
         IF(LCHECK(36)) THEN
            DO 60 ITY=12,16
               IF(.NOT.BTEST(NTYP,ITY))                          GOTO 60
               DO 50 ITY2=12,16
                  IF(ITY.EQ.ITY2)                                GOTO 50
                  IF(.NOT.BTEST(NTYP,ITY2))                      GOTO 50
                  WRITE(MPUNIT,540) CNAM(:lenocc(cnam))
                                                                 GOTO 70
   50          CONTINUE
   60       CONTINUE
         ENDIF
   70 CONTINUE
      WRITE(MPUNIT,550)
  500 FORMAT(/,1X,10('+'),' BEGIN GLOBAL MODULE CHECKS ',10('+'))
  510 FORMAT(1X,'!!! WARNING ... VARIABLE ',A,
     +' IS NAME OF COMMON BLOCK AND OTHER')
  520 FORMAT(1X,'!!! WARNING ... FUNCTION ',A,
     +' IS EXTERNAL BUT CLASHES WITH INTRINSIC FUNCTION')
  540 FORMAT(1X,'!!! WARNING ... MODULE ',A,
     +' CLASHES IN NAME WITH OTHER MODULE')
  550 FORMAT(1X,10('+'),  '  END GLOBAL MODULE CHECKS  ',10('+'),//)
  560 FORMAT(1X,'!!! WARNING ... "',A,
     +'" IS AN OBSOLETE CERN LIBRARY ROUTINE')
      END
