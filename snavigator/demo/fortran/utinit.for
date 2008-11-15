      SUBROUTINE UTINIT
*-----------------------------------------------------------------------
*
*--- user total initialization
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'class.h'
      include 'cursta.h'
      include 'flwork.h'
      include 'keycom.h'
      include 'typdef.h'
      include 'jobsum.h'
      include 'state.h'
      include 'flags.h'
      include 'usigno.h'
      include 'uslist.h'
      include 'usgcom.h'
      include 'usstmt.h'
      include 'usunit.h'
      include 'ushtml.h'
      include 'checks.h'
      CHARACTER*80 CARD
      PARAMETER(ISTCHK=35,IALCHK=40,iatchk=36)
      INTEGER NSTCHK(ISTCHK)
      INTEGER NALCHK(IALCHK)
      integer natchk(iatchk)
      DATA NSTCHK /1,2,3,4,5,6,9,10,11,12,13,14,16,17,18,19,20,21,
     &             22,24,26,27,28,29,30,32,33,35,36,37,38,39,40,41,44/
      DATA NALCHK /1,2,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,
     &             20,21,22,23,24,26,27,28,29,30,31,32,33,34,36,37,
     &             38,39,40,41,42,43,44/
      data natchk /1,2,3,4,5,10,11,12,13,14,17,18,19,20,21,22,23,
     &             24,25,26,28,29,30,31,32,33,34,35,36,37,38,39,
     &             41,44,45,46/
      NFIOLD = 0
      NFAULT = 0
      USFULL = .FALSE.
      RPROCS = .TRUE.
      UNFLP = .FALSE.
      NGCON = 0
      NGCOT = 0
      WRITE(MPUNIT,500)
C
C Start of process ... define FORTRAN intrinsics
      CALL DEFINF
C
C Define comments for each rule
      CALL COMRUL
C
C Check for USERs list of variables to be ignored in checks.
      NIGNOR = 0
      NIGNOS = 0
      DO 10 I=1,MXIGNV
         CIGNOR(I) = '        '
         LIGNOR(I) = 0
   10 CONTINUE
      DO 20 I=1,MXIGNS
         CIGNOS(I) = '        '
         LIGNOS(I) = 0
   20 CONTINUE
      DO 30 I=1,MCHEKS
         LCHECK(I) = .FALSE.
   30 CONTINUE
      GALEPH = .FALSE.
      atlas = .false.
      ISTAN = 0
      IALEP = 0
   40 READ(MUUNIT,510,ERR=70,END=70) CARD
      ICHAR = 0
      LSUB = 0
      LVAR = 0
      DO 50 I=1,MXLINE-1
         IF(CARD(I:I).EQ.' ')                                    GOTO 50
         IF(CARD(I:I).EQ.'*') THEN
C SPECIAL PROGRAM
            IF(INDEX(CARD,'GALEPH').NE.0) GALEPH = .TRUE.
            if(index(card,'HTML').ne.0) html = .true.
            if(index(card,'ATLAS').ne.0) then
               atlas = .true.
               do 444 ia=1,iatchk
                  lcheck(natchk(ia)) = .true.
  444          continue
            endif
C FULL LIST OF SOURCE WITH LINE NUMBERS
            IF(INDEX(CARD,'FULL').NE.0) USFULL = .TRUE.
C CHECK FOR JUST FLOP
            IF(INDEX(CARD,'NOFLOPPY').NE.0) UNFLP = .TRUE.
C CHECK FOR INTERACTIVE RULE SPECIFICATION
            IF(INDEX(CARD,'SPECIFY RULE').NE.0) CALL SPERUL
C CHECK FOR ALEPH RULES
            IF(INDEX(CARD,'ALEPH').NE.0) THEN
               IALEP = 1
               DO 333 IA=1,IALCHK
                  LCHECK(NALCHK(IA)) = .TRUE.
  333          CONTINUE
            ENDIF
C CHECK FOR RULE NUMBER
            IF(INDEX(CARD,'*CHECK RULE').NE.0) THEN
               LOCE = INDEX(CARD,'*CHECK RULE')+12
               IF(CARD(LOCE:LOCE).EQ.'*') THEN
C CHECK STANDARD SET
                 if(ialep.eq.1.or.atlas) goto 60
                 ISTAN=1
                 DO 61 IR=1,ISTCHK
                   LCHECK(NSTCHK(IR)) = .TRUE.
   61            CONTINUE
                 GOTO 60
               ENDIF
               READ(CARD(LOCE:LOCE+2),'(I3)') IRULE
               IF(IRULE.EQ.99) THEN
                  DO 777 IC=1,MCHEKS
                     LCHECK(IC)=.TRUE.
  777             CONTINUE
               ELSE IF(IRULE.EQ.-99) THEN
                  DO 888 IC=1,MCHEKS
                     LCHECK(IC)=.FALSE.
  888             CONTINUE
               ELSE IF(IRULE.GE.1.AND.IRULE.LE.MCHEKS) THEN
                  LCHECK(IRULE) = .TRUE.
               ELSE IF (IRULE.LT.0.AND.IRULE.GE.-MCHEKS) THEN
                  LCHECK(IABS(IRULE)) = .FALSE.
               ELSE
                  WRITE(MPUNIT,580) CARD(LOCE:LOCE+2)
               ENDIF
            ENDIF
                                                                 GOTO 60
         ENDIF
         ICHAR = ICHAR + 1
         IF(ICHAR.GT.MXNMCH)                                     GOTO 60
         IF(ICHAR.EQ.1.AND.CARD(I:I).NE.'#') THEN
            NIGNOR = NIGNOR + 1
            IF(NIGNOR.GT.MXIGNV) THEN
               WRITE(MPUNIT,520) MXIGNV
                                                                 GOTO 70
            ENDIF
            LVAR = 1
         ELSEIF (ICHAR.EQ.1.AND.CARD(I:I).EQ.'#') THEN
            NIGNOS = NIGNOS + 1
            IF(NIGNOS.GT.MXIGNS) THEN
               WRITE(MPUNIT,560) MXIGNS
                                                                 GOTO 70
            ENDIF
            LSUB = 1
                                                                 GOTO 50
         ENDIF
         IF(LVAR.EQ.1) THEN
            CIGNOR(NIGNOR)(ICHAR:ICHAR) = CARD(I:I)
            LIGNOR(NIGNOR) = ICHAR
            IF(CARD(I+1:I+1).EQ.' ')                             GOTO 60
         ELSE IF(LSUB.EQ.1) THEN
            CIGNOS(NIGNOS)(ICHAR-1:ICHAR-1) = CARD(I:I)
            LIGNOS(NIGNOS) = ICHAR-1
            IF(CARD(I+1:I+1).EQ.' ')                             GOTO 60
         ENDIF
   50 CONTINUE
   60 CONTINUE
                                                                 GOTO 40
C
   70 CONTINUE
      IF(ISTAN.EQ.1) WRITE(MPUNIT,545)
      IF(IALEP.EQ.1) WRITE(MPUNIT,585)
      if(atlas) write(mpunit,590)
      IF(GALEPH) WRITE(MPUNIT,540)
      IF(USFULL) WRITE(MPUNIT,550)
      NCHK = 0
      DO 71 IR=1,MCHEKS
        IF(.NOT.LCHECK(IR)) GOTO 71
        IF(CCHECK(IR)(:4).EQ.'$$$$') GOTO 71
        NCHK = NCHK + 1
        WRITE(MPUNIT,543) IR,CCHECK(IR)(:lenocc(ccheck(ir)))
   71 CONTINUE

      IF(NCHK.EQ.0) WRITE(MPUNIT,544)

      IF(NIGNOR.NE.0) THEN
         WRITE(MPUNIT,530) NIGNOR,(CIGNOR(II),II=1,NIGNOR)
      ENDIF
      IF(NIGNOS.NE.0) THEN
         WRITE(MPUNIT,570) NIGNOS,(CIGNOS(II),II=1,NIGNOS)
      ENDIF
  500 FORMAT(//,' NB Check your source compiles before using FLOPPY!'
     +       /,' Non-standard F77 statements are treated as Comments!',
     +       /,' The quoted line numbers start at 1 and step by 1 in',
     +         ' the source file.',/)
  510 FORMAT(A80)
  520 FORMAT(1X,'MAXIMUM OF ',I3,' IGNORABLE NAMES EXCEEDED')
  530 FORMAT(1X,10('+'),' YOU HAVE CHOSEN TO IGNORE',I3,
     +' VARIABLES. THEIR NAMES FOLLOW',/,(2X,A))
  540 FORMAT(//,1X,10('+'),' SPECIAL PROCESSING FOR   G A L E P H  !',/)
  550 FORMAT(//,1X,10('+'),' FULL SOURCE LISTING WITH LINE NUMBERS !',/)
  560 FORMAT(1X,'MAXIMUM OF ',I3,' IGNORABLE SUBROUTINES EXCEEDED')
  570 FORMAT(1X,10('+'),' YOU HAVE CHOSEN TO IGNORE ',I3,
     +' SUBROUTINES. THEIR NAMES FOLLOW',/,(2X,A))
  543 FORMAT(1X,'Floppy Convention ',I3,' "',A,'"')
  544 FORMAT(/,1x,'NO CODING CONVENTIONS WILL BE CHECKED',/)
  580 FORMAT(1X,'INVALID CONVENTION NUMBER ',A,' CANNOT BE CHECKED')
  542 FORMAT(//,1X,' YOU SPECIFIED NO CONVENTION CHECKING !')
  545 FORMAT(/,1X,'THE STANDARD SET OF CONVENTIONS WILL BE CHECKED',/)
  585 FORMAT(1X,'THE ALEPH CODING CONVENTIONS WILL BE CHECKED:',/)
  590 format(1x,'The ATLAS Coding Conventions will be checked:',/)
      END
