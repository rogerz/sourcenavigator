      SUBROUTINE NAMSRC(SNAME,SLIST,NLIST,IPOS,LAST)
*-----------------------------------------------------------------------
*
*   finds name in alphabetic table (binary search).
*
*   Input
*   SNAME           name to be looked up
*   SLIST           table
*   NLIST           length of table
*
*   Output
*   IPOS            = 0: name not in table
*                   > 0: position in table
*   LAST            for IPOS=0, position behind which name belongs
*
*-----------------------------------------------------------------------
      CHARACTER *(*) SNAME,SLIST(*)
      IPOS=0
      LAST=0
      N=NLIST
      IF(N.GT.0)  THEN
         KPOS=0
   10    M=(N+1)/2
         LAST=KPOS+M
         IF (SNAME.LT.SLIST(LAST))  THEN
            N=M
            LAST=LAST-1
            IF (N.GT.1) GOTO 10
         ELSEIF (SNAME.GT.SLIST(LAST))  THEN
            KPOS=LAST
            N=N-M
            IF (N.GT.0) GOTO 10
         ELSE
            IPOS=LAST
         ENDIF
      ENDIF
      END
