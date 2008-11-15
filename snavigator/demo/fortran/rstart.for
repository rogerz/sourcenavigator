      SUBROUTINE RSTART
*-----------------------------------------------------------------------
*
*   Processes the routine start
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'class.h'
      include 'flags.h'
      include 'cursta.h'
      include 'state.h'
      include 'treecom.h'
      include 'usigno.h'
      LOGICAL FLOC
*--- reset modify and filter flag
      DO 10 I=1,NSTAMM
         IMODIF(I)=0
   10 CONTINUE
*--- only initialize for new routine if really true
      IF(.NOT.STATUS(6))  THEN
         IF(ACTION(24)) THEN
*--- reset counters and flags for c.b. names
            STATUS(12)=.FALSE.
            STATUS(13)=.FALSE.
            NCBNAM=0
            NEQNAM=0
            NCBVAR=0
            DO 20 I=1,MAXGRP
               LCBNAM(I)=0
   20       CONTINUE
            DO 30 I=1,MXNAME
               LCBVAR(I)=0
   30       CONTINUE
         ENDIF
         IF(ACTION(29)) THEN
*--- reset counters for TREE
            NCALLR=0
            NCALLD=0
            NEXEL=0
         ENDIF
*--- set flag to re-initialize filters
         IFILTR=-1
*--- 'print routine header' flag
         STATUS(9)=.TRUE.
*--- reset SUBROUTINE flag
         STATUS(14)=.FALSE.
*--- get routine name
         DO 40 I=1,NSTAMM
            IF (ICLASS(I,1).NE.0) GOTO 50
   40    CONTINUE
*--- only comments
         SCROUT='COMMENTS'
         GOTO 60
   50    CONTINUE
         CALL EXTRAC(I,'PART')
         CALL CLASSF
*--- find routine name
         IF (ISTMDS(14,ICURCL(1)).NE.0)  THEN
*--- proper routine header
            STATUS(14)=ISTMDS(6,ICURCL(1)).EQ.66
            FLOC=ACTION(10)
            ACTION(10)=.TRUE.
            ISNAME=IRNAME+NRNAME
            CALL GETALL
            ACTION(10)=FLOC
            IF(NSNAME.GT.0)  THEN
               SCROUT=SNAMES(ISNAME+1)
            ELSEIF(ISTMDS(6,ICURCL(1)).EQ.4)  THEN
               SCROUT='BLOCKDAT'
            ELSE
               SCROUT='NOHEADER'
            ENDIF
         ELSE
            SCROUT='NOHEADER'
         ENDIF
   60    CONTINUE
*--- reset variable type routine
         IF (ACTION(20)) CALL SETTYP(0)
*--- preset 'routine filtered' flag
         STATUS(7)=.TRUE.
*--- filter for routine names
         IF (ACTION(16)) CALL FILTER(10,7)
      ENDIF
*--- process only if routine selected
      IF (STATUS(7))  THEN
*--- classify all statements
         DO 70 J=1,NSTAMM
            IF (ICLASS(J,1).NE.0)  THEN
               CALL EXTRAC(J,'FULL')
               CALL CLASSF
               ICLASS(J,1)=ICURCL(1)
               ICLASS(J,2)=ICURCL(2)
            ENDIF
   70    CONTINUE
*--- prepare re-numbering if requested
         IF (ACTION(13)) CALL PRENUM
      ENDIF
*--- reset variables
      KNTDO=0
      KNTIF=0
      if(action(29)) WRITE(MPUNIT,'(2A)') ' +++ TREE for ',SCROUT
      END
