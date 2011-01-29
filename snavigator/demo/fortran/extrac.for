      SUBROUTINE EXTRAC(NUMBER,OPTION)
*-----------------------------------------------------------------------
*
* extracts the FORTRAN field contents from the statement image.
* holl.  and character strings are included in special characters,
* '{' and '}'. strings may be either ...H, or be
* included in single or double quotes.
*
*--- input
*    NUMBER          number of the statement to be extracted
*    OPTION          (character) 'FULL' or 'PART' to extract
*                    all, or just start (up to first bracket)
*    SIMA            COMMON/ALCAZA/ (contains one complete routine)
*    NLTYPE,ICLASS,NFLINE,NLLINE,  COMMON/STATE/
*
*--- output
*    SSTA            COMMON/ALCAZA/  FORTRAN fields 7-72 of SIMA
*    NCHST           COMMON/STATE/  last non-blank in SSTA
*                    or =0 if statement consists of comment lines only
*    NLIMA, NLREF(1..NLIMA),   /STATE/
*    STATUS(3)      if illegal (containing '{', '}' )
*
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'flags.h'
      include 'cursta.h'
      include 'state.h'
      CHARACTER OPTION*4
      NCHST=0
      NSTREF=0
      IF (NUMBER.LE.0.OR.NUMBER.GT.NSTAMM) GOTO 999
      IF (ICLASS(NUMBER,1).EQ.0) GOTO 999
      NSTREF=NUMBER
*--- compact statement into SSTA
      CALL COMPAC(NUMBER)
      IF (NCHST.EQ.0) GOTO 999
*--- insert {} around strings, suppress multiple blanks
      CALL MARKST(OPTION,IERR)
      STATUS(3)=IERR.NE.0
  999 END
