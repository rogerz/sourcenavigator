      SUBROUTINE FLINIT
*-----------------------------------------------------------------------
*
*--- initializes FLOP
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'cursta.h'
      include 'flags.h'
      include 'jobsum.h'
      include 'state.h'
      include 'keycom.h'
      NSTBUF=0
      IGNAME=0
      NGNAME=0
      NKEEPL=0
      DO 10 I=1,10
   10 NSTATC(I)=0
      DO 20 I=1,MXFLAG
         ACTION(I)=.FALSE.
         STATUS(I)=.FALSE.
   20 CONTINUE
      NDUMMY=0
      NORSET=0
      NGLSET=0
      NKYINT=0
      NKYNAM=0
      NKYSTR=0
      NKYCHR=0
*--- LKYSTR must start at one to leave room for an extra '#'
      LKYSTR=1
      DO 30 I=1,MXORST
         NORCOM(I)=0
         KORCOM(I)=0
   30 CONTINUE
      DO 40 I=1,7
         DO 40 J=1,MXKEYS
            KEYREF(J,I)=0
   40 CONTINUE
      DO 50 I=1,2
         DO 50 J=1,MXKNAM
            KNAMRF(J,I)=0
            KSTREF(J,I)=0
   50 CONTINUE
      DO 60 I=1,MXKINT
         KEYINT(I)=0
   60 CONTINUE
      DO 70 I=1,2
         DO 70 J=1,MXSTAT
   70 NFDCLS(J,I)=0
      END
