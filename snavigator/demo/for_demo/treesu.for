      SUBROUTINE TREESU
*-----------------------------------------------------------------------
*
*--- Writes TREE output for each routine
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'treecom.h'
      include 'state.h'
      include 'flags.h'
      include 'class.h'
      include 'cursta.h'
      include 'usunit.h'
      include 'usargs.h'
      CHARACTER SNAME*(MXNMCH),STEMP*1,ATYP*7,UNUCOM(MAXGRP)*1
      SAVE IFIRST
      DATA ATYP/'IRLKDC$'/
      DATA IFIRST/0/
*--- find types of callers, and argument list
      DO 30 I=1,NCALLR
         NUMBER=ICALLR(I)
         CERARG(I)=' '
         KODE(I)=' '
         IF(NUMBER.EQ.0) THEN
*--- routine without header - treat as program
            CERARG(I)(:4)='MAIN'
         ELSE
            CALL EXTRAC(NUMBER,'FULL')
            ICURCL(1)=ICLASS(NUMBER,1)
            ICURCL(2)=ICLASS(NUMBER,2)
*--- external class
            ICLE=ISTMDS(6,ICURCL(1))
            IF(ICLE.EQ.55) THEN
*--- PROGRAM
               CERARG(I)(:4)='MAIN'
            ELSEIF(ICLE.EQ.4)  THEN
*--- block data
               KODE(I)='B'
            ELSE
               CALL GETALL
               CALL SETTYP(1)
               NT=NAMTYP(ISNAME+1)
               DO 10 J=1,6
                  IF(ITBIT(NT,J).NE.0) THEN
                     KODE(I)=ATYP(J:J)
                                                                 GOTO 20
                  ENDIF
   10          CONTINUE
   20          CONTINUE
               JLBPOS=INDEX(SSTA(1:NCHST),'(')
               IF(JLBPOS.NE.0) THEN
                  CALL ARGTYP(SSTA,.TRUE.,JLBPOS,NCHST,CERARG(I))
               ENDIF
            ENDIF
         ENDIF
   30 CONTINUE
      IF(IFIRST.EQ.0)  THEN
*--- neg. version number for format check
         IVERS=-100.*(VERSIO+.001)
C        WRITE(MTUNIT)  IVERS
         IFIRST=1
      ENDIF
*--- common block names are added plus a flag UNUCOM:
*    ' ' if c.b. used in routine, otherwise '!'
      DO 40 I=1,NCBNAM
         IF(LCBNAM(I).EQ.0)  THEN
            UNUCOM(I)='!'
         ELSE
            UNUCOM(I)=' '
         ENDIF
   40 CONTINUE
C     WRITE(MTUNIT)  NCALLR,(CALLER(I),I=1,NCALLR),
C    +(CERARG(I),I=1,NCALLR),(KODE(I),I=1,NCALLR),
C    +NCALLD,(CALLED(I),I=1,NCALLD),(CEDARG(I),I=1,NCALLD),
C    +NCBNAM,(SCBNAM(I),I=1,NCBNAM),(UNUCOM(I),I=1,NCBNAM),0,0,0,0,0
      WRITE(MJUNIT)  NCALLR,(CALLER(I),I=1,NCALLR),
     +(CERARG(I),I=1,NCALLR),(KODE(I),I=1,NCALLR),
     +NCALLD,(CALLED(I),I=1,NCALLD),(CEDARG(I),I=1,NCALLD),
     +NCBNAM,(SCBNAM(I),I=1,NCBNAM),(UNUCOM(I),I=1,NCBNAM),
     +CMMNT,NARGS,(CARGNM(I),I=1,NARGS),(CARGTY(I),I=1,NARGS),
     +(NARGDI(I),I=1,NARGS),
     +(((CARGDI(III,II,I),II=1,2),III=1,NARGDI(I)),I=1,NARGS),
     +NKALL,(CKALLN(I),I=1,NKALL),(KALLIF(I),I=1,NKALL),
     +(KALLDO(I),I=1,NKALL),
     +0,0,0,0,0
      END
