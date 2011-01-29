      SUBROUTINE DEFSTA(INDE,ILEN,CNAM,FOK)
C For statement class INDE returns length of FORTRAN
C keyword (ILEN), keyword name (CNAM*25) and logical
C FOK, which is set if the keyword is to be checked
C for embedded blanks.
C INPUT ; INDE
C OUTPUT; ILEN
C         CNAM
C         FOK
C
      include 'param.h'
      PARAMETER (Lnochk=24)
      CHARACTER*25 CFORTS(MXSTAT)
      character*(*) cnam
      logical fok
      integer nochk(lnochk)
      save cforts
c
      data nochk /8,9,10,12,13,21,23,24,26,33,37,38,39,45,46,51,
     &            52,64,65,73,74,75,27,14/
c
      DATA CFORTS(  1)/'ASSIGN                   '/
      DATA CFORTS(  2)/'BACKSPACE                '/
      DATA CFORTS(  3)/'BLOCKDATA                '/
      DATA CFORTS(  4)/'BUFFERIN                 '/
      DATA CFORTS(  5)/'BUFFEROUT                '/
      DATA CFORTS(  6)/'CONTINUE                 '/
      DATA CFORTS(  7)/'CALL                     '/
      DATA CFORTS(  8)/'COMMON                   '/
      DATA CFORTS(  9)/'COMPLEXFUNCTION          '/
      DATA CFORTS( 10)/'COMPLEX                  '/
      DATA CFORTS( 11)/'COMPLEX                  '/
      DATA CFORTS( 12)/'CHARACTERFUNCTION        '/
      DATA CFORTS( 13)/'CHARACTER                '/
      DATA CFORTS( 14)/'CHARACTER                '/
      DATA CFORTS( 15)/'CLOSE                    '/
      DATA CFORTS( 16)/'DATA                     '/
      DATA CFORTS( 17)/'DIMENSION                '/
      DATA CFORTS( 18)/'DO                       '/
      DATA CFORTS( 19)/'DO                       '/
      data cforts( 20)/'DO                       '/
      data cforts( 21)/'DOWHILE                  '/
      DATA CFORTS( 22)/'DECODE                   '/
      DATA CFORTS( 23)/'DOUBLEPRECISIONFUNCTION  '/
      DATA CFORTS( 24)/'DOUBLEPRECISION          '/
      DATA CFORTS( 25)/'END                      '/
      data cforts( 26)/'ENDDO                    '/
      DATA CFORTS( 27)/'ENDIF                    '/
      DATA CFORTS( 28)/'ENDFILE                  '/
      DATA CFORTS( 29)/'ENTRY                    '/
      DATA CFORTS( 30)/'EQUIVALENCE              '/
      DATA CFORTS( 31)/'EXTERNAL                 '/
      DATA CFORTS( 32)/'ELSE                     '/
      DATA CFORTS( 33)/'ELSEIF                   '/
      DATA CFORTS( 34)/'ENCODE                   '/
      DATA CFORTS( 35)/'FORMAT                   '/
      DATA CFORTS( 36)/'FUNCTION                 '/
      DATA CFORTS( 37)/'GOTO                     '/
      DATA CFORTS( 38)/'GOTO                     '/
      DATA CFORTS( 39)/'GOTO                     '/
      DATA CFORTS( 40)/'IF                       '/
      DATA CFORTS( 41)/'IF                       '/
      DATA CFORTS( 42)/'IF                       '/
      DATA CFORTS( 43)/'ILLEGAL                  '/
      data cforts( 44)/'INCLUDE                  '/
      DATA CFORTS( 45)/'INTEGERFUNCTION          '/
      DATA CFORTS( 46)/'INTEGER                  '/
      DATA CFORTS( 47)/'INTEGER                  '/
      DATA CFORTS( 48)/'IMPLICIT                 '/
      DATA CFORTS( 49)/'INQUIRE                  '/
      DATA CFORTS( 50)/'INTRINSIC                '/
      DATA CFORTS( 51)/'LOGICALFUNCTION          '/
      DATA CFORTS( 52)/'LOGICAL                  '/
      DATA CFORTS( 53)/'LOGICAL                  '/
      DATA CFORTS( 54)/'LEVEL                    '/
      DATA CFORTS( 55)/'NAMELIST                 '/
      DATA CFORTS( 56)/'OPEN                     '/
      DATA CFORTS( 57)/'PRINT                    '/
      DATA CFORTS( 58)/'PARAMETER                '/
      DATA CFORTS( 59)/'PAUSE                    '/
      DATA CFORTS( 60)/'PROGRAM                  '/
      DATA CFORTS( 61)/'PUNCH                    '/
      DATA CFORTS( 62)/'READ                     '/
      DATA CFORTS( 63)/'READ                     '/
      DATA CFORTS( 64)/'REALFUNCTION             '/
      DATA CFORTS( 65)/'REAL                     '/
      DATA CFORTS( 66)/'REAL                     '/
      DATA CFORTS( 67)/'RETURN                   '/
      DATA CFORTS( 68)/'REWIND                   '/
      DATA CFORTS( 69)/'SAVE                     '/
      DATA CFORTS( 70)/'STOP                     '/
      DATA CFORTS( 71)/'SUBROUTINE               '/
      DATA CFORTS( 72)/'WRITE                    '/
      DATA CFORTS( 73)/'ASSIGNMENT               '/
      DATA CFORTS( 74)/'ASSIGNMENT               '/
      DATA CFORTS( 75)/'ASSIGNMENT               '/
C
      FOK = .false.
      DO 10 I=1,Lnochk
         IF(INDE.EQ.nochk(i)) RETURN
   10 CONTINUE
      FOK = .TRUE.
      CNAM = CFORTS(INDE)
      ILEN = INDEX(CNAM,' ')-1
      END
