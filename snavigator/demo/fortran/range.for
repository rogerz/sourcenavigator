      LOGICAL FUNCTION RANGE(NUMBER,IARRAY)
*-----------------------------------------------------------------------
*
*   Purpose:    returns 'TRUE' if NUMBER is contained in ranges given
*               in IARRAY.
*
*   Input:      NUMBER    number to check
*               IARRAY    array containing ranges in the following way:
*                         word 1 = no. of ranges
*                         word 2 = lower limit, range 1
*                         word 3 = upper limit, range 1   etc.
*
*-----------------------------------------------------------------------
      DIMENSION IARRAY(*)
      RANGE=.FALSE.
      DO 10 I=1,IARRAY(1)
         IF (NUMBER.GE.IARRAY(2*I).AND.NUMBER.LE.IARRAY(2*I+1))  THEN
            RANGE=.TRUE.
            GOTO 999
         ENDIF
   10 CONTINUE
  999 END
