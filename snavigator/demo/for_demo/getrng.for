      SUBROUTINE GETRNG(IST,LAST,IARR)
*-----------------------------------------------------------------------
*
*   Gives positions of '(' and ')' in SSTA (no string check !)
*
*   Input
*   IST     starting position of scan
*   LAST    last position of scan
*
*   Output
*   IARR(1)          # of '(...)'
*   IARR(2)          pos. of first '('
*   IARR(3)          pos. of first ')'
*   IARR(4)          pos. of second '('
*                    etc.
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      DIMENSION IARR(*)
      N=0
      IPT=IST-1
   10 CONTINUE
      IND=INDEX(SSTA(IPT+1:LAST),'(')
      IF (IND.EQ.0) GOTO 20
      IPT=IPT+IND
      CALL SKIPLV(SSTA,IPT+1,LAST,.FALSE.,IND,ILEV)
      IF (IND.EQ.0) GOTO 20
      N=N+1
      IARR(2*N)=IPT
      IARR(2*N+1)=IND
      IPT=IND
      IF (IPT.LT.LAST) GOTO 10
   20 CONTINUE
      IARR(1)=N
      END
