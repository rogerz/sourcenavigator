      SUBROUTINE INUSER
*-----------------------------------------------------------------------
* reads command lines, compacts them (blanks outside strings suppressed)
* finds no. of OR-sets, marks strings with '{}'
*
*  Output : common blocks (mis-)used for input decoding only
*  NLINES = total number of lines read
*  NSTAMM = total number of commands
*  NFLINE(I) = first line of command I
*  NLLINE(I) = last   -         -
*  NLTYPE(J) = pos. of last character, or of ';' in command line J
*  NSSTRT(I) = first command of OR-set number I
*  NSEND(I)  = last    -         -
*
*  Output correctly stored for later use:
*  NORSET    = number of OR-sets
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'state.h'
      include 'keycom.h'
      CHARACTER*1 STEMP,SQUOTE
      include 'convex.h'
*
      N=0
c      WRITE (MPUNIT,10020)
   10 CONTINUE
      READ (MCUNIT,'(A)',END=20) SIMA(N+1)
      N=N+1
c      WRITE(MPUNIT,'(1X,A)') SIMA(N)
      IF (SIMA(N)(1:3).EQ.'END') GOTO 20
      IF(N.EQ.MXSIMA)  THEN
         WRITE (MPUNIT,10030) N
      ELSE
         GOTO 10
      ENDIF
   20 CONTINUE
c      WRITE (MPUNIT,10040)
      NLINES=N
*--- suppress blanks outside strings, and in strings to be replaced
*    find end of each line
      N=0
      NFLINE(1)=1
      IN=0
      IR=0
      IS=0
      DO 50 I=1,NLINES
         NPOS=0
         DO 30 J=1,MXLINE
            STEMP=SIMA(I)(J:J)
            IF (INDEX(SPILL,STEMP).NE.0)  THEN
*--- illegal character encountered - replace by '/'
               WRITE (MPUNIT,10010) STEMP
               STEMP='/'
            ENDIF
            IF (IN.EQ.1)  THEN
*--- inside quote string
               IF (STEMP.EQ.SQUOTE) THEN
                  IN=0
                  STEMP='}'
                  IR=0
               ELSEIF (STEMP.EQ.'''')  THEN
                  IS=1-IS
               ENDIF
               IF(STEMP.NE.' '.OR.IR+IS.GT.0)  THEN
                  NPOS=NPOS+1
                  SIMA(I)(NPOS:NPOS)=STEMP
               ENDIF
            ELSE
*--- outside quote string
               IF (STEMP.NE.' ')  THEN
                  IF (STEMP.EQ.''''.OR.STEMP.EQ.'"')  THEN
                     IN=1
                     SQUOTE=STEMP
                     STEMP='{'
                  ELSEIF(STEMP.EQ.'=')  THEN
                     IR=1
                  ENDIF
                  NPOS=NPOS+1
                  SIMA(I)(NPOS:NPOS)=STEMP
                  IF (STEMP.EQ.';')  THEN
                     N=N+1
                     NLLINE(N)=I
                     NFLINE(N+1)=I+1
                     GOTO 40
                  ENDIF
               ENDIF
            ENDIF
   30    CONTINUE
   40    CONTINUE
         NLTYPE(I)=NPOS
   50 CONTINUE
      IF(NLINES.GT.0)  THEN
         IF (IN.NE.0)  THEN
            WRITE (MPUNIT,10050)
            K=MIN(NLTYPE(NLINES)+1,MXLINE)
            SIMA(NLINES)(K:K)='}'
            NLTYPE(NLINES)=K
         ENDIF
         K=NLTYPE(NLINES)
         STEMP=SIMA(NLINES)(K:K)
         IF(STEMP.NE.';')  THEN
            WRITE (MPUNIT,10000)
            IF (K.EQ.MXLINE.AND.STEMP.EQ.'}') SIMA(NLINES)(K-1:K-1)=
     +      STEMP
            K=MIN(K+1,MXLINE)
            SIMA(NLINES)(K:K)=';'
            NLTYPE(NLINES)=K
         ENDIF
      ENDIF
      NSTAMM=N
*--- now find number of OR-sets
      NORSET=1
      NSSTRT(1)=1
      DO 60 I=1,NSTAMM
         IF (SIMA(NFLINE(I))(1:3).EQ.'OR;')  THEN
            NSEND(NORSET)=I-1
            IF (NORSET.EQ.MXORST)  THEN
               WRITE (MPUNIT,10060) NORSET
               GOTO 999
            ENDIF
            NORSET=NORSET+1
            NSSTRT(NORSET)=I
         ENDIF
   60 CONTINUE
      NSEND(NORSET)=NSTAMM
10000 FORMAT(/1X,8('*-*-'),' WARNING - missing ";" added at end',/)
10010 FORMAT(/1X,8('*-*-'),' WARNING - illegal character ',A,
     +' replaced by "/"')
10020 FORMAT(/,1X,4('++++'),' Input Commands  ',4('++++'))
10030 FORMAT(//1X,8('*-*-'),' WARNING - max. buffer size for input',
     +' = ',I5,' lines reached, rest ignored',/)
10040 FORMAT(/,1X,4('++++'),' End of Commands ',4('++++'))
10050 FORMAT(//1X,8('*-*-'),' WARNING - unclosed string in commands',
     +' closed at the very end',/)
10060 FORMAT(//1X,8('*-*-'),' WARNING - max. number of OR-sets =', I5,
     +' reached, remainder ignored',/)
  999 END
