      SUBROUTINE FILTER(KEY,NFLAG)
*-----------------------------------------------------------------------
*
*   Filters a statement according to user specifications.
*   Input:
*   KEY             = 10 : filter for routines
*                   = 11 : filter for names
*                   = 12 : filter for strings
*                   = 13 : filter for classes
*                   (see INDECO for input)
*   NFLAG           STATUS(NFLAG) will be true if accepted, false if not
*                   at return from FILTER
*   Output
*   STATUS(NFLAG)
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'class.h'
      include 'flags.h'
      include 'cursta.h'
      include 'state.h'
      include 'keycom.h'
      LOGICAL HASHFL
      DIMENSION LRL(MXORST)
      CHARACTER*1 STEMP
      SAVE NRL,LRL
      IF (IFILTR.LT.0)  THEN
*--- start of routine: reset to overall OR-sets
         NRL=NORSET
         NRORST=NORSET
         DO 10 I=1,NRL
            LRL(I)=I
            LRORST(I)=I
   10    CONTINUE
      ELSEIF (IFILTR.EQ.0)  THEN
*--- FILTER called first time for statement: reset to routine OR-set
         NRL=NRORST
         DO 20 I=1,NRL
            LRL(I)=LRORST(I)
   20    CONTINUE
      ENDIF
      IFILTR=1
*--- reset counter for new valid OR-sets
      NEW=0
*--- loop over currently valid OR-sets
      DO 90 I=1,NRL
         IOR=LRL(I)
         DO 30 JK=KORCOM(IOR)+1,KORCOM(IOR)+NORCOM(IOR)
*--- check whether key in this OR-set
            IF (KEYREF(JK,1).EQ.KEY) GOTO 40
   30    CONTINUE
*--- key not present - accept OR-set
         NEW=NEW+1
         LRL(NEW)=IOR
         GOTO 90
   40    CONTINUE
*--- KEY is present
         IF (KEY.EQ.10)  THEN
*--- routine name filter
            CALL NAMSRC(SCROUT,SKEYLS(KEYREF(JK,5)+1),KEYREF(JK,4),IPOS,
     +      LAST)
            IF (IPOS.GT.0)  THEN
*-- name found
               NEW=NEW+1
               LRL(NEW)=IOR
            ENDIF
         ELSEIF (KEY.EQ.11)  THEN
*--- names in statement
            DO 50 J=1,NSNAME
               CALL NAMSRC(SNAMES(ISNAME+J),SKEYLS(KEYREF(JK,5)+1),
     +         KEYREF(JK,4),IPOS,LAST)
               IF (IPOS.GT.0)  THEN
                  NEW=NEW+1
                  LRL(NEW)=IOR
                  GOTO 90
               ENDIF
   50       CONTINUE
         ELSEIF (KEY.EQ.12)  THEN
*--- string filter
            DO 60 J=KEYREF(JK,7)+1,KEYREF(JK,7)+KEYREF(JK,6)
               KREF=KSTREF(J,1)
*--- set '#' in front if not there
               K1=KKYSTA(KREF)
               K2=KKYEND(KREF)
               HASHFL=SKYSTR(K1:K1).NE.'#'
               IF (HASHFL)  THEN
                  K1=K1-1
                  STEMP=SKYSTR(K1:K1)
                  SKYSTR(K1:K1)='#'
               ENDIF
               CALL MATCH(SKYSTR,K1,K2,SSTA,1,NCHST,.TRUE.,KPOS,ILEV,N,
     +         IWS,IWS)
               IF (HASHFL) SKYSTR(K1:K1)=STEMP
               IF (KPOS.GT.0)  THEN
                  NEW=NEW+1
                  LRL(NEW)=IOR
                  GOTO 90
               ENDIF
   60       CONTINUE
         ELSEIF (KEY.EQ.13)  THEN
*---  classes
            K1=KEYREF(JK,3)+1
            N1=KEYINT(K1)
            N2=KEYINT(K1+N1+1)
*--- N1 counts simple classes, N2 those behind logical IF
            IF((ICURCL(1).NE.IIF.and.icurcl(1).ne.iif+71)
     &         .OR.N2.EQ.0)  THEN
               DO 70 J=K1+1,K1+N1
                  JC=KEYINT(J)
                  IF (JC.EQ.ISTMDS(6,ICURCL(1)).OR.
     &            ((ICURCL(1).EQ.IIF.or.icurcl(1).eq.iif+71).AND
     +            .JC.EQ.ISTMDS(6,ICURCL(2))))  THEN
                     NEW=NEW+1
                     LRL(NEW)=IOR
                     GOTO 90
                  ENDIF
   70          CONTINUE
            ELSE
               K1=K1+N1+1
               DO 80 J=K1+1,K1+N2
                  IF (KEYINT(J).EQ.ISTMDS(6,ICURCL(2)))  THEN
                     NEW=NEW+1
                     LRL(NEW)=IOR
                     GOTO 90
                  ENDIF
   80          CONTINUE
            ENDIF
         ENDIF
   90 CONTINUE
      NRL=NEW
      IF(KEY.EQ.10)  THEN
*--- set OR-set for routine
         NRORST=NRL
         DO 100 I=1,NRL
            LRORST(I)=LRL(I)
  100    CONTINUE
      ENDIF
      STATUS(NFLAG)=NRL.GT.0
      END
