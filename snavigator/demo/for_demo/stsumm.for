      SUBROUTINE STSUMM
*-----------------------------------------------------------------------
*
*--- Prints statement count summary
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'flwork.h'
      include 'jobsum.h'
      include 'class.h'
      DIMENSION IREF(3,MXSTAT),IOUT(4,2)
      EQUIVALENCE (IREF(1,1),IWS(1))
      DO 10 I=1,NCLASS
         DO 10 J=1,3
   10 IREF(J,I)=0
*--- collect references to external classes
      DO 20 I=1,NCLASS
         K=ISTMDS(6,I)
         IREF(1,K)=I
         IREF(2,K)=IREF(2,K)+NFDCLS(I,1)
         IREF(3,K)=IREF(3,K)+NFDCLS(I,2)
   20 CONTINUE
      WRITE (MPUNIT,10000)
      N=0
      DO 30 I=1,NCLASS
         K=IREF(1,I)
         IF (K.NE.0)  THEN
            N=N+1
            IOUT(1,N)=I
            IOUT(2,N)=IREF(2,I)
            IOUT(3,N)=IREF(3,I)
            IOUT(4,N)=K
            IF (N.EQ.2)  THEN
               N=0
               WRITE (MPUNIT,10010) IOUT(1,1),SNAM(ISTMDS(1,IOUT(4,1)):
     +         ISTMDS(2,IOUT(4,1))),IOUT(2,1),IOUT(3,1),IOUT(1,2),SNAM(
     +         ISTMDS(1,IOUT(4,2)):ISTMDS(2,IOUT(4,2))),IOUT(2,2),IOUT
     +         ( 3,2)
            ENDIF
         ENDIF
   30 CONTINUE
      IF(N.GT.0)  THEN
         WRITE (MPUNIT,10010) IOUT(1,1),SNAM(ISTMDS(1,IOUT(4,1)):ISTMDS(
     +   2,IOUT(4,1))),IOUT(2,1),IOUT(3,1)
      ENDIF
10000 FORMAT('1',10('----'),' Summary for filtered statements ', 10(
     +'----')//
     +' Except for ILLEGAL (all occurrences in filtered routines),',
     +' only filtered statements counted.'/
     +' There are two types of counts, 1 = overall occurence, ',
     +'2 = behind logical IF'// ' number',15X,'name',T41,
     +' count-1 count-2', T61,' number',15X,'name',T101,
     +' count-1 count-2'/)
10010 FORMAT(1X,I6,4X,A29,2I8,T61,1X,I6,4X,A29,2I8)
      END
