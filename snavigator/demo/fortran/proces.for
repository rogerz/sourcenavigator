      SUBROUTINE PROCES
*-----------------------------------------------------------------------
*
*   Processes one routine statement by statement:
*   filtering, replacements
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'class.h'
      include 'flags.h'
      include 'cursta.h'
      include 'state.h'
      include 'jobsum.h'
      include 'ushtml.h'
      LOGICAL SAMEST
*--- treat routine header and init
      CALL RSTART
*--- TREE preparation if no proper header
      IF(ACTION(29).AND.SCROUT.EQ.'NOHEADER')  CALL TREEST(0)
*--- user top of routine
      IF(ACTION(22))  CALL URINIT
 
*--- NP controls the number of blank lines for print headers
      NP=0
*--- process only if routine selected
      IF (STATUS(7))  THEN
*--- loop over all statements in routine
         DO 60 IST=1,NSTAMM
            STATUS(8)=.TRUE.
            STATUS(10)=.TRUE.
            STATUS(11)=.FALSE.
            IF (ICLASS(IST,1).GT.0)  THEN
*--- extract and set classes
               CALL EXTRAC(IST,'FULL')
               ICURCL(1)=ICLASS(IST,1)
               ICURCL(2)=ICLASS(IST,2)
               IF (ICURCL(1).EQ.ILL) NFDCLS(ILL,1)=NFDCLS(ILL,1)+1
               IF (ICURCL(1).EQ.ILL+71) NFDCLS(ILL,1)=NFDCLS(ILL,1)+1
            ENDIF
            IF ((ICLASS(IST,1).EQ.ILL.or.iclass(ist,1).eq.ill+71)
     &          .AND.ACTION(3))  THEN
               IF (STATUS(9))  THEN
                  STATUS(9)=.FALSE.
                  IF (ACTION(6))  THEN
c                     WRITE (MPUNIT,10000) 'all',SCROUT
                  ELSE
c                     WRITE (MPUNIT,10000) 'selected',SCROUT
                  ENDIF
               ENDIF
               STATUS(10)=.FALSE.
               CALL FLPRNT(0,'Floppy illegal',NLLINE(IST)-NFLINE(IST)+1,
     +         SIMA(NFLINE(IST)),NSTATC(8))
               NP=1
*--- print all if requested
            ELSEIF (ACTION(6))  THEN
*--- routine header
               IF (STATUS(9))  THEN
                  STATUS(9)=.FALSE.
                  WRITE (MPUNIT,10000) 'all',SCROUT
               ENDIF
               STATUS(10)=.FALSE.
               CALL FLPRNT(NP,' ',NLLINE(IST)-NFLINE(IST)+1,SIMA(NFLINE(
     +         IST)),NSTATC(8))
               NP=0
            ENDIF
*--- call user routine for ALL statements
            IF(ACTION(22))  CALL USSALL
*--- Generate HTML output if required
            if(html) then
               isname = irname + nrname
               call getall
               call tohtml(ist,iclass(ist,1))
            endif
*--- process only legal FORTRAN statements
            IF (ICLASS(IST,1).GT.0.AND.ICLASS(IST,1).NE.ILL.and.
     &          iclass(ist,1).ne.ill+71) then
*--- get statement number
               SNEWST(1)(1:6)=SIMA(NFLINE(IST))(1:6)
*--- filter for classes
               IF (ACTION(17)) CALL FILTER(13,8)
               IF (STATUS(8))  THEN
*--- get statement names
                  if(.not.html) then
                     ISNAME=IRNAME+NRNAME
                     CALL GETALL
                  endif
*--- filter for names
                  IF (ACTION(18)) CALL FILTER(11,8)
                  IF (STATUS(8))  THEN
*--- filter for strings
                     IF (ACTION(19)) CALL FILTER(12,8)
                     IF (STATUS(8))  THEN
*--- all filters passed - update statistics
                        IMODIF(IST)=1
                        NSTATC(4)=NSTATC(4)+1
                        NFDCLS(ICURCL(1),1)=NFDCLS(ICURCL(1),1)+1
                        IF (ICURCL(1).EQ.IIF.or.icurcl(1).eq.iif+71) 
     &                     NFDCLS(ICURCL(2),2)=NFDCLS
     +                  (ICURCL(2),2)+1
*--- user start of statement
                        IF(ACTION(22))  CALL USSBEG
*--- prepare indentation if requested
                        IF(ACTION(21))  CALL PROIND
*----get type for variables
                        IF (ACTION(20)) CALL SETTYP(1)
*--- check for incorrect relational operators in character type
                        CALL CHKCHR
*--- treat names further if any
                        IF(NSNAME.GT.0)  THEN
*--- prepare TREE output
                           IF(ACTION(29))  CALL TREEST(1)
*--- find used and unused common blocks
                           IF(ACTION(24).AND..NOT.STATUS(12))
     +                     CALL PROCOM
*--- perform name replacements
                           IF (ACTION(15)) CALL REPNAM
                           IF (STATUS(11)) GOTO 10
                           IF (ACTION(1).OR.ACTION(2))  THEN
*--- add names to routine name list
                              CALL LSORT(SNAMES(ISNAME+1),
     +                        NAMTYP(ISNAME+1),.TRUE.,NSNAME)
                              CALL LMERGE(SNAMES,NAMTYP,.TRUE.,IRNAME,
     +                        NRNAME,NSNAME)
                              CALL SUPMOR(SNAMES,NAMTYP,.TRUE.,IRNAME,
     +                        NRNAME+NSNAME,NRNAME)
                           ENDIF
                        ENDIF
                        IF (ACTION(5).AND.STATUS(10))  THEN
*--- print filtered
                           IF (STATUS(9))  THEN
                              WRITE (MPUNIT,10000) 'filtered',SCROUT
                              STATUS(9)=.FALSE.
                           ENDIF
                           STATUS(10)=.FALSE.
                           CALL FLPRNT(NP,' ',NLLINE(IST)-NFLINE(IST)+1,
     +                     SIMA(NFLINE(IST)),NSTATC(8))
                           NP=0
                        ENDIF
                        IF (ACTION(11).OR.ACTION(12)) THEN
*--- remove {} , change holl. to quotes if requested
                           CALL QUOS
			   IF(STATUS(11)) GOTO 10
*--- string replacement
                           IF(ACTION(12))  CALL REPSTR
                           IF (STATUS(11)) GOTO 10
*--- re-insert {} around strings for REFORM
                           CALL MARKST('FULL',IERR)
                           STATUS(11)=IERR.NE.0
                           IF (STATUS(11)) GOTO 10
                        ENDIF
*--- re-numbering if requested
 CALL QUOS
			   IF(STATUS(11)) GOTO 10
*--- string replacement
                           IF(ACTION(12))  CALL REPSTR
                           IF (STATUS(11)) GOTO 10
*--- re-insert {} around strings for REFORM
                           CALL MARKST('FULL',IERR)
                           STATUS(11)=IERR.NE.0
                           IF (STATUS(11)) GOTO 10
                        ENDIF

                        IF (ACTION(13)) CALL RENUMB
*--- user end of statement
                        IF(ACTION(22))  CALL USSEND
                     ENDIF
                  ENDIF
               ENDIF
*--- here you arrive without filter checks
   10          CONTINUE
               IFILTR=0
               IF (STATUS(11)) IMODIF(IST)=MOD(IMODIF(IST),10)
*--- reformat = put modified statement into SIMA
               IF (IMODIF(IST).GT.10.OR.ACTION(21).AND.IMODIF(IST).GT.0)
     +         THEN
                  CALL REFORM
*--- not changed if REFORM problem, or identical after REFORM
                  IF (STATUS(11).OR.SAMEST(IST))
     +            IMODIF(IST)=MOD(IMODIF(IST),10)
               ENDIF
               IF (IMODIF(IST).GT.10)  THEN
*--- count changed statements
                  NSTATC(5)=NSTATC(5)+1
                  IF (ACTION(4).AND.STATUS(10))  THEN
*--- print changed statements
                     IF (STATUS(9))  THEN
                        WRITE (MPUNIT,10000) 'changed',SCROUT
                        STATUS(9)=.FALSE.
                     ENDIF
                     CALL FLPRNT(1,' ',NLLINE(IST)-NFLINE(IST)+1,SIMA(
     +               NFLINE(IST)),NSTATC(8))
                  ENDIF
*--- re-formatted statement in SNEWST
*   put into SIMA, push SIMA if new longer than old, introduce blank
*   lines if new shorter than old
                  N=0
                  DO 20 I=NFLINE(IST),NLLINE(IST)
                     IF (NLTYPE(I).NE.0)  THEN
                        N=N+1
                        IF (N.GT.NEWOUT)  THEN
                           SIMA(I)=' '
                        ELSE
                           SIMA(I)=SNEWST(N)
                        ENDIF
                     ENDIF
   20             CONTINUE
                  NPUSH=NEWOUT-N
                  IF (NPUSH.GT.0)  THEN
                     DO 30 I=NLINES,NLLINE(IST)+1,-1
                        NLTYPE(I+NPUSH)=NLTYPE(I)
                        SIMA(I+NPUSH)=SIMA(I)
   30                CONTINUE
                     NLINES=NLINES+NPUSH
*---  loop over all statements since they might be in a different order
                     DO 40 I=1,NSTAMM
                        IF(NFLINE(I).GT.NFLINE(IST)) THEN
                           NFLINE(I)=NFLINE(I)+NPUSH
                           NLLINE(I)=NLLINE(I)+NPUSH
                        ENDIF
   40                CONTINUE
                     DO 50 I=1,NPUSH
                        SIMA(NLLINE(IST)+I)=SNEWST(N+I)
   50                CONTINUE
                     NLLINE(IST)=NLLINE(IST)+NPUSH
                  ENDIF
                  IF (ACTION(4))  THEN
                     CALL FLPRNT(0,'changed to',NLLINE(IST)-NFLINE(IST)+
     +               1,SIMA(NFLINE(IST)),NDUMMY)
                     NP=1
                  ENDIF
               ENDIF
            ENDIF
   60    CONTINUE
      ENDIF
*--- user end of routine
      IF(ACTION(22))  CALL URTERM
*--- TREE output if any
      IF(ACTION(29))  CALL TREESU
10000 FORMAT(/1X,10('++++'),A10,' statements, routine =',A10)
  999 END
