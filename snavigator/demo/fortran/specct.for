      SUBROUTINE SPECCT(MODE,ISTR,NTOT,ICT,IREF,IERR)
*-----------------------------------------------------------------------
*  Extracts information on special characters from strings
*  Input
*  MODE     = 1 : treat a string which is to be replaced
*           = 2 : treat a replacement string
*  ISTR     = string ref. (relative to KKYSTA, KKYEND)
*  Output
*  NTOT     = total no. of special characters
*  ICT (I)   = count for character I (in SPCHAR)
*  IREF(J,I)= if MODE = 1 :
*             for the Jth character I, total count
*             if MODE = 2 :
*             for the Jth character I, count in [...]
*
*--- important: special characters inside '...' not counted !
*
*  IERR     = 0 : all OK
*           = 1 : buffer overflow
*           = 2 : unclosed [...]
*           = 3 : number in [...] out of range
*           = 4 : unclosed '...' inside string
*-----------------------------------------------------------------------
      include 'param.h'
      include 'keycom.h'
      DIMENSION ICT(*),IREF(MXNAME/20,*)
      include 'convex.h'
      IERR=0
      NTOT=0
      INSTR=0
      DO 10 I=1,7
         ICT(I)=0
   10 CONTINUE
      J=KKYSTA(ISTR)-1
      KEND=KKYEND(ISTR)
   20 CONTINUE
      J=J+1
      IF (J.GT.KEND) GOTO 30
      IF(SKYSTR(J:J).EQ.'''')  INSTR=1-INSTR
      IF (INSTR.NE.0) GOTO 20
      I=INDEX(SPCHAR,SKYSTR(J:J))
      IF(I.EQ.7)  THEN
*--- '>' found, look for ')' to follow
         IF (J.EQ.KEND)  THEN
            I=0
         ELSEIF (SKYSTR(J+1:J+1).EQ.')')  THEN
            J=J+1
         ELSE
            I=0
         ENDIF
      ENDIF
      IF(I.GT.0)  THEN
*--- check buffer size
         IF (ICT(I).EQ.MXNAME/2)  THEN
            IERR=1
            GOTO 999
         ENDIF
         NTOT=NTOT+1
         ICT(I)=ICT(I)+1
         IF (MODE.EQ.1)  THEN
            IREF(ICT(I),I)=NTOT
         ELSEIF (J.LT.KEND.AND.SKYSTR(J+1:J+1).EQ.'[')  THEN
            J=J+1
            IF (J.EQ.KEND)  THEN
               IERR=2
               GOTO 999
            ELSEIF (SKYSTR(J+1:J+1).EQ.']')  THEN
               IREF(ICT(I),I)=ICT(I)
            ELSE
*--- get integer in [...]
               CALL GETINT(SKYSTR,J+1,KEND,KFCH,KLCH,NN)
               IF (KFCH.EQ.0.OR.NN.EQ.0)  THEN
                  IERR=3
                  GOTO 999
               ELSE
                  IREF(ICT(I),I)=NN
                  IF (KLCH.EQ.KEND)  THEN
                     IERR=2
                     GOTO 999
                  ENDIF
                  J=KLCH+1
                  IF (SKYSTR(J:J).NE.']')  THEN
                     IERR=2
                     GOTO 999
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            IREF(ICT(I),I)=ICT(I)
         ENDIF
      ENDIF
      GOTO 20
   30 CONTINUE
      IF(INSTR.NE.0)  IERR=4
  999 END
