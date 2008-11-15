      SUBROUTINE QUOSUB
*-----------------------------------------------------------------------
*
*   Removes {} = string indicators, changes " or ...H to ' if ACTION(11)
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'flags.h'
      include 'cursta.h'
      include 'state.h'
      include 'jobsum.h'
      CHARACTER *1 STEMP
      NMOD=IMODIF(NSTREF)
      NCH=0
      IPT=0
   10 CONTINUE
      IF (IPT.EQ.NCHST) GOTO 30
      IN=INDEX(SSTA(IPT+1:NCHST),'{')
      IF (IN.EQ.0) GOTO 30
      L=IN-1
      IN=IPT+IN
      IF(L.GT.0)  THEN
         IF (NCH+L.GT.MXLENG) GOTO 40
         SSTR(NCH+1:NCH+L)=SSTA(IPT+1:IPT+L)
         NCH=NCH+L
      ENDIF
      IPT=IN
      IN=INDEX(SSTA(IPT+1:NCHST),'}')
      L=IN-1
      IN=IPT+IN
      STEMP=SSTA(IPT+1:IPT+1)
      IF(STEMP.EQ.''''.OR..NOT.ACTION(11))  THEN
         IF (NCH+L.GT.MXLENG) GOTO 40
         SSTR(NCH+1:NCH+L)=SSTA(IPT+1:IN-1)
         NCH=NCH+L
      ELSE
*--- replace " or ...H, double up single quotes
         IF (NMOD.LT.10) NMOD=NMOD+10
         IF (STEMP.EQ.'"')  THEN
            I1=IPT+2
            I2=IN-2
         ELSE
*--- find H
            I1=IPT+INDEX(SSTA(IPT+1:NCHST),'H')+1
            I2=IN-1
         ENDIF
         NCH=NCH+1
         IF (NCH.GT.MXLENG) GOTO 40
         SSTR(NCH:NCH)=''''
         DO 20 I=I1,I2
            NCH=NCH+1
            IF (NCH.GT.MXLENG) GOTO 40
            STEMP=SSTA(I:I)
            SSTR(NCH:NCH)=STEMP
            IF (STEMP.EQ.'''')  THEN
               NCH=NCH+1
               IF (NCH.GT.MXLENG) GOTO 40
               SSTR(NCH:NCH)=STEMP
            ENDIF
   20    CONTINUE
         IF (IBLPAD.GT.1)  THEN
*--- blank pad string up to multiple of IBLPAD
            KPAD=MOD(I2+1-I1,IBLPAD)
            IF (KPAD.GT.0)  THEN
               I=IBLPAD-KPAD
               IF (NCH+I.GT.MXLENG) GOTO 40
               SSTR(NCH+1:NCH+I)=' '
               NCH=NCH+I
            ENDIF
         ENDIF
         NCH=NCH+1
         IF (NCH.GT.MXLENG) GOTO 40
         SSTR(NCH:NCH)=''''
      ENDIF
      IPT=IN
      GOTO 10
   30 CONTINUE
*--- transfer rest and swap if modified
      IF (IPT.EQ.0) GOTO 999
      L=NCHST-IPT+1
      IF(L.GT.0)  THEN
         IF (NCH+L.GT.MXLENG) GOTO 40
         SSTR(NCH+1:NCH+L)=SSTA(IPT+1:NCHST)
         NCH=NCH+L
      ENDIF
      IMODIF(NSTREF)=NMOD
      SSTA(:NCH)=SSTR(:NCH)
      NCHST=NCH
      GOTO 999
   40 CONTINUE
*--- error exit - statement too long
      WRITE (MPUNIT,10000)
      CALL FLPRNT(1,'OVERFLOW',NLLINE(NSTREF)-NFLINE(NSTREF)+1, SIMA
     +(NFLINE(NSTREF)),NDUMMY)
      NSTATC(6)=NSTATC(6)+1
      STATUS(11)=.TRUE.
10000 FORMAT(/' ++++++ Warning - replacements would lead to overflow',
     +' in following statement, not done')
  999 END
