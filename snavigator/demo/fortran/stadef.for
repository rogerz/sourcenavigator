      SUBROUTINE STADEF
*-----------------------------------------------------------------------
*
*--- initialises the statement classification by reading
*--- the statement descriptions from internal buffers (data
*--- statement) and filling the necessary arrays.
*
*--- output
*    all variables in common/CLASS/
*    SSTM       in COMMON/ALCAZA/
*    SNAM       in COMMON/ALCAZA/
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'class.h'
      include 'flwork.h'
      include 'condec.h'
      LOGICAL DOITFL
      SAVE DOITFL
      CHARACTER SDESCR(MXSTAT)*86,STEMP*1,SLAST*1,STR1*24,STR2*20
*--- SDESCR contains the FORTRAN statement description
*--- important for new entries:
*   - scan order is top - down (see e.g. INTEGER - INTEGERFUNCTION etc.)
*   - order is alphabetic
*   - special characters at the end
*
*   The numbers correspond to ISTMDS(6)...ISTMDS(22)
*
*                         no.+prty+name              descrpt.
*      l u s x n k h  type information
      DATA SDESCR(  1)/' 1 0 ASSIGN                  ASSIGN@TO          DEF
     +99 0 1 1 2 0 0  0  1  0  0  0  0  0  0'/                          DEF
      DATA SDESCR(  2)/' 3 0 BACKSPACE               DITO               DEF
     +99 0 0 1 2 2 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR(  3)/' 4 0 BLOCKDATA               DITO               DEF
     +99 0 0 0 1 2 1  0  1 14  0  0  0  0  0'/                          DEF
      DATA SDESCR(  4)/' 5 0 BUFFERIN                DITO               DEF
     +99 0 0 1 2 2 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR(  5)/' 6 0 BUFFEROUT               DITO               DEF
     +99 0 0 1 2 2 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR(  6)/'15 0 CONTINUE                DITO               DEF
     +99 0 0 1 0 2 0  0  0  0  0  0  0  0  0'/                          DEF
      DATA SDESCR(  7)/' 7 0 CALL                    DITO               DEF
     +99 0 5 1 2 2 0  1  1 15  2  0 17  0  0'/                          DEF
      DATA SDESCR(  8)/'12 0 COMMON                  DITO               DEF
     +99 0 0 0 2 2 0 21  1  8  3  0 18 20  0'/                          DEF
      DATA SDESCR(  9)/'14 0 COMPLEXFUNCTION         COMPLEX#FUNCTION   DEF
     +99 0 0 0 2 0 1  1  3  4 17 21  2  0 19'/                          DEF
      DATA SDESCR( 10)/'13 0 COMPLEX                 COMPLEX*@          DEF
     +99 0 0 0 2 0 0 10  2  4 18  0  0  0  0'/                          DEF
      DATA SDESCR( 11)/'13 0 COMPLEX                 DITO               DEF
     +99 0 0 0 2 2 0 10  2  4 18  0  0  0  0'/                          DEF
      DATA SDESCR( 12)/' 9 0 CHARACTERFUNCTION       CHARACTER#FUNCTION DEF
     +99 0 0 0 2 0 1  1  3  6 17 21  2  0 19'/                          DEF
      DATA SDESCR( 13)/' 8 0 CHARACTER               CHARACTER*@        DEF
     +99 0 0 0 2 0 0 10  2  6 18  0  0  0  0'/                          DEF
      DATA SDESCR( 14)/' 8 0 CHARACTER               DITO               DEF
     +99 0 0 0 2 2 0 10  2  6 18  0  0  0  0'/                          DEF
      DATA SDESCR( 15)/'10 0 CLOSE                   DITO               DEF
     +99 0 4 1 2 3 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 16)/'16 0 DATA                    DITO               DEF
     +99 0 0 0 2 2 0  0  1  0  0  0  0  0  0'/                          DEF
      DATA SDESCR( 17)/'19 0 DIMENSION               DITO               DEF
     +99 0 0 0 2 2 0 10  2  0 18  0  0  0  0'/                          DEF
      DATA SDESCR( 18)/'20 1 DO                      DO@,               DEF
     + 3 0 1 1 2 0 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 19)/'20 2 DO                      DO@?=!,            DEF
     + 3 0 1 1 2 0 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 20)/'20 3 DO                      DO?=!,             DEF
     + 3 0 1 1 2 0 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 21)/'20 4 DOWHILE                 DOWHILE(>);        DEF
     + 7 4 1 1 2 0 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 22)/'17 0 DECODE                  DITO               DEF
     +99 0 4 1 2 2 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 23)/'22 0 DOUBLEPRECISIONFUNCTION DITO               DEF
     +99 0 0 0 2 2 1  1  3  5 17 21  2  0 19'/                          DEF
      DATA SDESCR( 24)/'21 0 DOUBLEPRECISION         DITO               DEF
     +99 0 0 0 2 2 0 10  2  5 18  0  0  0  0'/                          DEF
      DATA SDESCR( 25)/'26 0 END                     END;               DEF
     +99 0 0 1 0 0 0  0  0  0  0  0  0  0  0'/                          DEF
      DATA SDESCR( 26)/'71 0 ENDDO                   ENDDO;             DEF
     +99 0 0 1 0 0 0  0  0  0  0  0  0  0  0'/                          DEF
      DATA SDESCR( 27)/'27 0 ENDIF                   DITO               DEF
     +99 0 0 1 0 2 0  0  0  0  0  0  0  0  0'/                          DEF
      DATA SDESCR( 28)/'28 0 ENDFILE                 DITO               DEF
     +99 0 0 1 2 2 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 29)/'29 0 ENTRY                   DITO               DEF
     +99 0 0 0 2 2 0  1  2  0 16  1  0  0  0'/                          DEF
      DATA SDESCR( 30)/'30 0 EQUIVALENCE             DITO               DEF
     +99 0 0 0 2 2 0  0  1  0  0  0  0  0  0'/                          DEF
      DATA SDESCR( 31)/'31 0 EXTERNAL                DITO               DEF
     +99 0 0 0 2 2 0  0  1 12  0  0  0  0  0'/                          DEF
      DATA SDESCR( 32)/'23 0 ELSE                    ELSE;              DEF
     +99 0 0 1 0 0 0  0  0  0  0  0  0  0  0'/                          DEF
      DATA SDESCR( 33)/'24 0 ELSEIF                  ELSEIF(>)THEN;     DEF
     + 6 4 0 1 2 0 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 34)/'25 0 ENCODE                  DITO               DEF
     +99 0 4 1 2 2 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 35)/'33 0 FORMAT                  DITO               DEF
     +99 0 0 0 0 2 0  0  0  0  0  0  0  0  0'/                          DEF
      DATA SDESCR( 36)/'34 0 FUNCTION                DITO               DEF
     +99 0 0 0 2 2 1  1  2  0 17  2  0 19  0'/                          DEF
      DATA SDESCR( 37)/'37 0 GOTO-(UNCOND.)          GOTO@              DEF
     +99 0 1 1 0 0 0  0  0  0  0  0  0  0  0'/                          DEF
      DATA SDESCR( 38)/'36 0 GOTO-(COMP.)            GOTO(              DEF
     +99 0 2 1 2 2 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 39)/'35 0 GOTO-(ASS.)             GOTO&              DEF
     + 4 0 2 1 2 0 0  0  1  0  0  0  0  0  0'/                          DEF
      DATA SDESCR( 40)/'39 0 IF-(BLOCK)              IF(>)THEN;         DEF
     + 3 4 0 1 2 0 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 41)/'40 0 IF-(LOGICAL)            IF(>)&             DEF
     + 3 0 0 1 2 0 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 42)/'38 0 IF-(ARITM.)             IF(>)@             DEF
     + 3 0 3 1 2 0 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 43)/'69 0 ILLEGAL                                    DEF
     + 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0'/                          DEF
      
      DATA SDESCR( 44)/'70 0 INCLUDE                 INCLUDE            DEF
     +99 0 0 0 1 0 0  0  1 19  0  0  0  0  0'/                          DEF

      DATA SDESCR( 45)/'44 0 INTEGERFUNCTION         DITO               DEF
     +99 0 0 0 2 2 1  1  3  1 17 21  2  0 19'/                          DEF
      DATA SDESCR( 46)/'43 0 INTEGER                 INTEGER*@          DEF
     +99 0 0 0 2 0 0 10  2  1 18  0  0  0  0'/                          DEF
      DATA SDESCR( 47)/'43 0 INTEGER                 DITO               DEF
     +99 0 0 0 2 2 0 10  2  1 18  0  0  0  0'/                          DEF
      DATA SDESCR( 48)/'41 0 IMPLICIT                DITO               DEF
     +99 0 0 0 0 2 0  2  0  0  0  0  0  0  0'/                          DEF
      DATA SDESCR( 49)/'42 0 INQUIRE                 DITO               DEF
     +99 0 4 1 2 3 0  0  1  0  0  0  0  0  0'/                          DEF
      DATA SDESCR( 50)/'45 0 INTRINSIC               DITO               DEF
     +99 0 0 0 2 2 0  0  1 11  0  0  0  0  0'/                          DEF
      DATA SDESCR( 51)/'48 0 LOGICALFUNCTION         DITO               DEF
     +99 0 0 0 2 2 1  1  3  3 17 21  2  0 19'/                          DEF
      DATA SDESCR( 52)/'47 0 LOGICAL                 LOGICAL*@          DEF
     +99 0 0 0 2 0 0 10  2  3 18  0  0  0  0'/                          DEF
      DATA SDESCR( 53)/'47 0 LOGICAL                 DITO               DEF
     +99 0 0 0 2 2 0 10  2  3 18  0  0  0  0'/                          DEF
      DATA SDESCR( 54)/'46 0 LEVEL                   DITO               DEF
     +99 0 0 0 2 2 0  0  1  0  0  0  0  0  0'/                          DEF
      DATA SDESCR( 55)/'49 0 NAMELIST                DITO               DEF
     +99 0 0 0 2 2 0  1  1  9  1  0  0  0  0'/                          DEF
      DATA SDESCR( 56)/'50 0 OPEN                    DITO               DEF
     +99 0 4 1 2 3 0  0  1  0  0  0  0  0  0'/                          DEF
      DATA SDESCR( 57)/'54 0 PRINT                   DITO               DEF
     +99 0 1 1 2 2 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 58)/'52 0 PARAMETER               DITO               DEF
     +99 0 0 0 2 2 0  0  2  0  7  0  0  0  0'/                          DEF
      DATA SDESCR( 59)/'53 0 PAUSE                   DITO               DEF
     +99 0 0 1 0 2 0  0  0  0  0  0  0  0  0'/                          DEF
      DATA SDESCR( 60)/'55 0 PROGRAM                 DITO               DEF
     +99 0 0 0 1 2 1  0  1 13  0  0  0  0  0'/                          DEF
      DATA SDESCR( 61)/'56 0 PUNCH                   DITO               DEF
     +99 0 1 1 2 2 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 62)/'58 0 READ(                   DITO               DEF
     +99 0 4 1 2 3 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 63)/'57 0 READ                    DITO               DEF
     +99 0 1 1 2 2 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 64)/'60 0 REALFUNCTION            DITO               DEF
     +99 0 0 0 2 2 1  1  3  2 17 21  2  0 19'/                          DEF
      DATA SDESCR( 65)/'59 0 REAL                    REAL*@             DEF
     +99 0 0 0 2 0 0 10  2  2 18  0  0  0  0'/                          DEF
      DATA SDESCR( 66)/'59 0 REAL                    DITO               DEF
     +99 0 0 0 2 2 0 10  2  2 18  0  0  0  0'/                          DEF
      DATA SDESCR( 67)/'61 0 RETURN                  DITO               DEF
     +99 0 0 1 0 2 0  0  0  0  0  0  0  0  0'/                          DEF
      DATA SDESCR( 68)/'62 0 REWIND                  DITO               DEF
     +99 0 0 1 2 2 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 69)/'63 0 SAVE                    DITO               DEF
     +99 0 0 0 2 2 0  0  1  0  0  0  0  0  0'/                          DEF
      DATA SDESCR( 70)/'65 0 STOP                    DITO               DEF
     +99 0 0 1 0 2 0  0  0  0  0  0  0  0  0'/                          DEF
      DATA SDESCR( 71)/'66 0 SUBROUTINE              DITO               DEF
     +99 0 0 0 2 2 1  1  1 15  2  0 19  0  0'/                          DEF
      DATA SDESCR( 72)/'68 0 WRITE                   DITO               DEF
     +99 0 4 1 2 3 0  0  2  0 17  0  0  0  0'/                          DEF
      DATA SDESCR( 73)/' 2 5 ASSIGNMENT              ?=                 DEF
     + 0 0 0 1 2 0 0  1  1  0  2  0 17  0  0'/                          DEF
      DATA SDESCR( 74)/' 2 6 ASSIGNMENT              ?(>)=              DEF
     + 0 0 0 1 2 0 0  1  2  0 10  2  0 17  0'/                          DEF
      DATA SDESCR( 75)/' 2 7 ASSIGNMENT              ?(>)(>)=           DEF
     + 0 0 0 1 2 0 0  1  1  0  2  0 17  0  0'/                          DEF


      DATA SLAST/' '/
      DATA DOITFL/.TRUE./
      include 'condat.h'
*
*--- do it only once
*
      IF(DOITFL)  THEN
         DOITFL=.FALSE.
         NHEADR=0
         NPRIOR=0
         NPNAM=0
         NPSTM=0
         NCLASS=MXSTAT
         DO 10 I=1,53
            IALPHA(1,I)=0
            IALPHA(2,I)=-1
   10    CONTINUE
c
         iif = 0
         iend = 0
         iformt = 0
         ill = 0
         iretur = 0
c
         DO 30 I=1,MXSTAT
            READ (SDESCR(I),'(2I2,44X,7I2,8i3)',err=88)
     &           (ISTMDS(J,I),J=6,MCLASS)
            NP=ISTMDS(7,I)
            IF (NP.GT.0.AND.NP.LE.NCLASS)  THEN
               NPRIOR=NPRIOR+1
               IPRIOR(NP)=I
            ENDIF
            READ (SDESCR(I),'(5X,A24,A20)') STR1,STR2
            NST1=INDEX(STR1,' ')-1
            NST2=INDEX(STR2,' ')-1
            SNAM(NPNAM+1:NPNAM+NST1)=STR1
            ISTMDS(1,I)=NPNAM+1
            NPNAM=NPNAM+NST1
            ISTMDS(2,I)=NPNAM
            IF (NST2.EQ.0)  THEN
*--- statement descriptor blank - indicate
               ISTMDS(3,I)=0
               IF (ISTMDS(6,I).EQ.69.and.ill.eq.0) ILL=I
            ELSEIF (STR2(1:4).EQ.'DITO')  THEN
*--- use name as descriptor
               SSTM(NPSTM+1:NPSTM+NST1)=STR1
               ISTMDS(3,I)=NPSTM+1
               NPSTM=NPSTM+NST1
               ISTMDS(4,I)=NPSTM
            ELSE
               SSTM(NPSTM+1:NPSTM+NST2)=STR2
               ISTMDS(3,I)=NPSTM+1
               NPSTM=NPSTM+NST2
               ISTMDS(4,I)=NPSTM
            ENDIF
*--- set some class references
            IF (ISTMDS(6,I).EQ.40.and.iif.eq.0)  THEN
*--- logical IF
               IIF=I
            ELSEIF (ISTMDS(6,I).EQ.26.and.iend.eq.0)  THEN
*--- END statement
               IEND=I
            ELSEIF (ISTMDS(6,I).EQ.33.and.iformt.eq.0)  THEN
*--- FORMAT
               IFORMT=I
            ELSEIF (ISTMDS(6,I).EQ.61.and.iretur.eq.0)  THEN
*--- RETURN
               IRETUR=I
            ENDIF
*--- get start of alphabetic group
            STEMP=SSTM(ISTMDS(3,I):ISTMDS(3,I))
            IF (ISTMDS(3,I).NE.0)  THEN
               IF (STEMP.NE.SLAST)  THEN
                  IF (SPECCH(STEMP))  THEN
                     K=53
                  ELSE
                     K=ICVAL(STEMP)
                  ENDIF
                  IALPHA(1,K)=I
                  IF (SLAST.NE.' ')  THEN
                     K=ICVAL(SLAST)
                     IALPHA(2,K)=I-1
                  ENDIF
                  SLAST=STEMP
               ENDIF
            ENDIF
            K=ISTMDS(3,I)-1
*--- find and store last alphabetic ch. in descr.
            DO 20 J=ISTMDS(3,I),ISTMDS(4,I)
               IF (ALPHCH(SSTM(J:J))) K=J
   20       CONTINUE
            ISTMDS(5,I)=K
*--- routine headers
            IF (ISTMDS(14,I).NE.0)  THEN
               NHEADR=NHEADR+1
               IHEADR(NHEADR)=I
            ENDIF
   30    CONTINUE
         IALPHA(2,53)=NCLASS
*--- end of IF(DOITFL)  following
      ENDIF
      return
   88 write(mpunit,'(A)') ' Error in STADEF ... Abort'
      END
