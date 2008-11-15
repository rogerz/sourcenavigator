      SUBROUTINE READSB(NCOMM,NST,ICL)
*-----------------------------------------------------------------------
*
*   Purpose:    performs sub-task for READEC by accepting the start of
*               a new FORTRAN statement.
*
*   Input:      NCOMM     number of comment lines preceding the new line
*
*   Output:     NST       no. of last FORTRAN statement
*               ICL       class of last FORTRAN statement
*
*   Various variables in common are used and modified.
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'cursta.h'
      include 'state.h'
      NST=0
      IF(NSTAMM.GT.0)  THEN
*--- close previous if FORTRAN
         IF (NLTYPE(NFLINE(NSTAMM)).EQ.1)  THEN
            NLLINE(NSTAMM)=NLINES-NCOMM
            NFSTAT=NFSTAT+1
            ICLASS(NSTAMM,1)=999
            NST=NSTAMM
            CALL EXTRAC(NSTAMM,'PART')
            CALL CLASSF
            ICL=ICURCL(1)
         ENDIF
      ENDIF
      IF(NCOMM.GT.0)  THEN
*--- make comment line blocks into one statement
         NSTAMM=NSTAMM+1
         NFLINE(NSTAMM)=NLINES-NCOMM+1
         NLLINE(NSTAMM)=NLINES
         ICLASS(NSTAMM,1)=0
         NCOMM=0
      ENDIF
      END
