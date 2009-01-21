      SUBROUTINE ALLPRO
*-----------------------------------------------------------------------
*
*--- Overall control of FLOP run.
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'jobsum.h'
      include 'flags.h'
      include 'state.h'
      include 'ushtml.h'
*--- print header
      CALL HEADER
*--- initialize
      CALL FLINIT
      CALL STADEF
*--- read command lines
      CALL INDECO
      CALL INDECT
*--- user total initialization
      IF(ACTION(22))  CALL UTINIT
*--- start processing
   10 CONTINUE
*--- process if enough time left (only if CERN flag on)
      IF(.NOT.STATUS(4))  THEN
*--- read one complete routine
         CALL READEC
*--- process if still something read
         IF (.NOT.STATUS(2))  THEN
*--- count lines
            DO 20 I=NFLINE(1),NLLINE(NSTAMM)
               IF (NLTYPE(I).EQ.0) NSTATC(7)=NSTATC(7)+1
               IF (NLTYPE(I).EQ.1) NSTATC(3)=NSTATC(3)+1
   20       CONTINUE
            NSTATC(1)=NSTATC(1)+NLLINE(NSTAMM)-NFLINE(1)+1
*--- set pointer and count for routine name list
            NRNAME=0
            IRNAME=IGNAME+NGNAME
*--- process one complete routine
            CALL PROCES
            IF (NRNAME.GT.0)  THEN
               IF (ACTION(25))  THEN
*--- print list of routine names
                  WRITE (MPUNIT,10000) SCROUT(:lenocc(scrout)),NRNAME
                  IF (ACTION(20))  THEN
*--- print name list with types
                     CALL PRNAMF(IRNAME+1,IRNAME+NRNAME)
                  ELSE
                     WRITE (MPUNIT,10010) (SNAMES(IRNAME+J),J=1,NRNAME)
                  ENDIF
               ENDIF
               IF (ACTION(2))  THEN
*--- merge with global namelist
                  CALL LMERGE(SNAMES,NAMTYP,.TRUE.,IGNAME,NGNAME,NRNAME)
                  CALL SUPMUL(SNAMES,NAMTYP,.TRUE.,IGNAME,NGNAME+NRNAME,
     +            NGNAME)
               ENDIF
            ENDIF
            IF(ACTION(27).AND..NOT.STATUS(12))  THEN
*--- print common block information
               CALL PRTCOM
            ENDIF
*--- write output file
            CALL PUTOUT
            GOTO 10
         ENDIF
      ENDIF
*--- user total termination
      IF(ACTION(22))  CALL UTTERM
      if (html) then
         write(mhunit,'(a)') '</BODY>'
      endif
      CALL SUMMRY
10000 FORMAT(//' Routine = ',A,',  list of',I6,' names'/)
10010 FORMAT(1X,10A10)
      END
