      SUBROUTINE BINSRC(KELEM,KLIST,NLIST,IPOS,LAST)
*-----------------------------------------------------------------------
*
*---Purpose:    finds number in sorted list (ascending)
*               with binary search.
*
*---Input
*   KELEM           number to be looked up
*   KLIST           table
*   NLIST           length of table
*
*---Output
*   IPOS            = 0: name not in table
*                   > 0: position in table
*   LAST            for IPOS=0, position behind which number belongs
*
*
*-----------------------------------------------------------------------
      DIMENSION KLIST(*)
      IPOS=0
      LAST=0
      N=NLIST
      IF(N.GT.0)  THEN
         KPOS=0
   10    M=(N+1)/2
         LAST=KPOS+M
         IF (KELEM.LT.KLIST(LAST))  THEN
            N=M
            LAST=LAST-1
            IF (N.GT.1) GOTO 10
         ELSEIF (KELEM.GT.KLIST(LAST))  THEN
            KPOS=LAST
            N=N-M
            IF (N.GT.0) GOTO 10
         ELSE
            IPOS=LAST
         ENDIF
      ENDIF
      END
