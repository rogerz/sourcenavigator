      SUBROUTINE MARKST(OPTION,IERR)
*-----------------------------------------------------------------------
*
* in SSTA, suppresses multiple blanks outside strings, puts strings
* in special characters,
* '{' and '}'. strings may be either ...H, or be
* included in single or double quotes.
*
*--- input
*    OPTION          (character) 'FULL' or 'PART' to extract
*                    all, or just start (up to first bracket)
*    NCHST           number of ch. in SSTA
*
*--- output
*    IERR          = 0 if everything OK, =1 if illegal characters found,
*                  or unclosed string.
*    SSTA            COMMON/ALCAZA/  FORTRAN fields 7-72 of SIMA
*    NCHST           COMMON/STATE/  last non-blank in SSTA
*
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'cursta.h'
      CHARACTER STEMP*1,SKEEP*1,SDUM*100,OPTION*4
      LOGICAL POSS,SPOSS,PARTFL,LASTBL
      include 'convex.h'
      PARTFL=OPTION.EQ.'PART'
      NCH=0
      NDUM=0
      ISKIP=0
*--- ISKIP = 0      outside string
*          = -1     inside hollerith string (nH...)
*          = +1     inside character string (' or ")
      NHOLL=0
      IERR=0
      POSS=.FALSE.
      SPOSS=.FALSE.
      STEMP=' '
      J=0
   10 CONTINUE
      J=J+1
      IF (J.GT.NCHST) GOTO 20
      LASTBL=STEMP.NE.' '
      STEMP=SSTA(J:J)
      IF (PARTFL)  THEN
         IF (STEMP.EQ.'(')GOTO 30
      ENDIF
      IF (INDEX(SPILL,STEMP).NE.0)  THEN
*--- illegal character
         GOTO 40
      ENDIF
      IF (ISKIP.EQ.0)  THEN
*--- not in string
         IF (STEMP.EQ.' ')  THEN
            IF (LASTBL)  THEN
               NCH=NCH+1
               SSTR(NCH:NCH)=' '
            ENDIF
         ELSEIF (NUMCH(STEMP))  THEN
            IF (POSS)  THEN
*--- count for ..H may start or continue
               IF (NHOLL.LT.10000) NHOLL=10*NHOLL+ICVAL(STEMP)-ICVAL('0'
     +         )
               NDUM=NDUM+1
*--- buffer digits
               SDUM(NDUM:NDUM)=STEMP
            ELSE
               NCH=NCH+1
               SSTR(NCH:NCH)=STEMP
            ENDIF
         ELSEIF (ALPHCH(STEMP))  THEN
            IF (NDUM.EQ.0)  THEN
*--- no digits (= holl. count ) buffered
               POSS=.FALSE.
               NCH=NCH+1
               SSTR(NCH:NCH)=STEMP
            ELSE
               IF (STEMP.EQ.'H')  THEN
                  NCH=NCH+1
                  SSTR(NCH:NCH)='{'
                  ISKIP=-1
                  SSTR(NCH+1:NCH+NDUM)=SDUM(:NDUM)
                  NCH=NCH+NDUM+1
                  SSTR(NCH:NCH)=STEMP
               ELSE
*--- other alphabetic ch. than H
                  POSS=.FALSE.
                  NHOLL=0
                  SSTR(NCH+1:NCH+NDUM)=SDUM(:NDUM)
                  NCH=NCH+NDUM+1
                  SSTR(NCH:NCH)=STEMP
               ENDIF
               NDUM=0
            ENDIF
         ELSE
*--- special character
            SPOSS=SPOSS.OR.STEMP.NE.'*'
*--- holl. count cannot start after '*'
            POSS=SPOSS
            IF (NDUM.NE.0)  THEN
               SSTR(NCH+1:NCH+NDUM)=SDUM(:NDUM)
               NCH=NCH+NDUM
               NDUM=0
            ENDIF
            NHOLL=0
            IF (STEMP.EQ.''''.OR.STEMP.EQ.'"')  THEN
               ISKIP=1
               SKEEP=STEMP
               NCH=NCH+1
               SSTR(NCH:NCH)='{'
            ENDIF
c
c In-line comment ... discard rest of statement
c
            if(stemp.eq.'!'.and.iskip.eq.0) goto 20
            NCH=NCH+1
            SSTR(NCH:NCH)=STEMP
*--- following ENDIF for IF(STEMP.EQ.' ')  THEN  etc.
         ENDIF
      ELSEIF (ISKIP.LT.0)  THEN
*--- inside a holl. string
         NHOLL=NHOLL-1
         NCH=NCH+1
         SSTR(NCH:NCH)=STEMP
         IF (NHOLL.EQ.0)  THEN
*--- end of holl. string reached
            ISKIP=0
            NCH=NCH+1
            SSTR(NCH:NCH)='}'
         ENDIF
      ELSE
*--- ISKIP GT 0
         IF (STEMP.EQ.''''.AND.SSTA(J+1:J+1).EQ.''''.AND.J.LT.NCHST)
     +   THEN
            SSTR(NCH+1:NCH+2)=SSTA(J:J+1)
            J=J+1
            NCH=NCH+2
         ELSEIF (SKEEP.EQ.STEMP)  THEN
*--- end of string
            ISKIP=0
            NCH=NCH+1
            SSTR(NCH:NCH)=STEMP
            NCH=NCH+1
            SSTR(NCH:NCH)='}'
         ELSE
            NCH=NCH+1
            SSTR(NCH:NCH)=STEMP
         ENDIF
      ENDIF
      GOTO 10
   20 CONTINUE
      IF(NDUM.GT.0)  THEN
*--- still some lonely digits hanging around
         SSTR(NCH+1:NCH+NDUM)=SDUM(:NDUM)
         NCH=NCH+NDUM
      ENDIF
      IF (ISKIP.NE.0) GOTO 40
   30 NCHST=NCH
      SSTA(:NCH)=SSTR(:NCH)
      GOTO 999
   40 CONTINUE
*--- illegal - either unclosed string, or illegal character
      IERR=1
  999 END
