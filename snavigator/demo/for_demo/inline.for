      SUBROUTINE INLINE(IUNIT,STRING,EOFFLG,NTYP)
*-----------------------------------------------------------------------
*
*--- reads one line from input
*
*--- input
*    IUNIT       logical unit number
*--- output
*    STRING      line read (up to MXLINE characters)
*    EOFFLG      TRUE when end of file
*    NTYP        type if line : 0 comment line
*                               1 start of statement
*                               2 contination line
*
*-----------------------------------------------------------------------
      include 'param.h'
      CHARACTER STRING*(MXLINE),STEMP*1
      LOGICAL EOFFLG
      include 'convex.h'
      EOFFLG=.FALSE.
      READ (IUNIT,'(A)',END=40) STRING
c
c remove tab at position 1, if there
c
      if(string(1:1).eq.char(9)) then
         string = '      '//string(2:mxline-5)
      endif
      DO 10 I=1,72
         IF (STRING(I:I).NE.' ') GOTO 20
   10 CONTINUE
*--- all blank = comment
      NTYP=0
      GOTO 999
   20 CONTINUE
*--- check for comment
      IF(I.LE.6)  THEN
         DO 30 J=I,5
            STEMP=STRING(J:J)
            IF (.NOT.(STEMP.EQ.' '.OR.NUMCH(STEMP))) THEN
               NTYP=0
               GOTO 999
            ENDIF
   30    CONTINUE
*--- not a comment line - check for continuation
         STEMP=STRING(6:6)
         IF (STEMP.EQ.' '.OR.STEMP.EQ.'0')  THEN
            NTYP=1
         ELSE
            NTYP=2
         ENDIF
      ELSE
         NTYP=1
      ENDIF
      GOTO 999
   40 CONTINUE
      EOFFLG=.TRUE.
  999 END
