      SUBROUTINE PRNAMF(ICC1,ICC2)
*-----------------------------------------------------------------------
*
*   Prints name table with all attributes (types)
*
*   Input
*   ICC1           first name is SNAMES to be printed
*   ICC2           last             -          -
*
*   NAMTYP    , common /STATE/
*
*   Each type corresponds to a bit position (for testing use ITBIT).
*
*   Types are:
*
*   Bit          meaning
*
*     1          INTEGER
*     2          REAL
*     3          LOGICAL
*     4          COMPLEX
*     5          DOUBLE PRECISION
*     6          CHARACTER
*     7          PARAMETER
*     8          COMMON block name
*     9          NAMELIST name
*    10          statement function
*    11          INTRINSIC
*    12          EXTERNAL
*    13          PROGRAM name
*    14          BLOCK DATA name
*    15          SUBROUTINE
*    16          ENTRY
*    17          FUNCTION (intrinsic or external)
*    18          dimensioned
*    19          (routine or function) argument
*    20          in a COMMON block
*    21          strongly typed function (internal usage)
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'state.h'
      CHARACTER SLINE*120, STYP(MXTYPE)*18
      DIMENSION LTYP(MXTYPE)
      DATA STYP/'INTEGER','REAL','LOGICAL','COMPLEX','DOUBLE PRECISION',
     +'CHARACTER','PARAMETER','COMMON block','NAMELIST',
     +'statement function','INTRINSIC','EXTERNAL','PROGRAM',
     +'BLOCK DATA','SUBROUTINE','ENTRY','FUNCTION', 'array','argument',
     +'in COMMON'/
      DATA LTYP/7,4,7,7,16,9,9,12,8,18,9,8,7,10,10,5,8,5,8,9/
      IP=0
      SLINE=' '
      DO 20 I=ICC1,ICC2
         SLINE(IP+1:IP+MXNMCH)=SNAMES(I)
         IPT=IP+MXNMCH+3
         NT=NAMTYP(I)
         DO 10 J=1,MXTYPE
            IF (MOD(NT,2).NE.0)  THEN
               L=LTYP(J)
               IF (IPT+L.LE.IP+60)  THEN
                  SLINE(IPT+1:IPT+L)=STYP(J)(:L)
                  IPT=IPT+L+2
               ENDIF
            ENDIF
            NT=NT/2
   10    CONTINUE
         IF (IP.EQ.0)  THEN
            IP=60
         ELSE
            IP=0
            WRITE (MPUNIT,'(1X,A120)') SLINE
            SLINE=' '
         ENDIF
   20 CONTINUE
      IF(IP.NE.0)  THEN
         WRITE (MPUNIT,'(1X,A120)') SLINE
      ENDIF
      END
