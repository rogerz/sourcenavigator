      SUBROUTINE INDECT
*-----------------------------------------------------------------------
*
*  Checks for invalid string replacement requests, kills them
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'keycom.h'
*
*--- loop over OR-sets (first OR-set is for global commands)
*--- loop over commands in OR-set
      DO 30 ICOM=1,NGLSET
*--- loop over strings behind names
         DO 10 JNAM=KEYREF(ICOM,5)+1,KEYREF(ICOM,5)+KEYREF(ICOM,4)
            CALL INDECZ(KNAMRF(JNAM,1),KNAMRF(JNAM,2))
   10    CONTINUE
*--- loop over stand-alone strings
         DO 20 JSTR=KEYREF(ICOM,7)+1,KEYREF(ICOM,7)+KEYREF(ICOM,6)
            CALL INDECZ(KSTREF(JSTR,1),KSTREF(JSTR,2))
   20    CONTINUE
   30 CONTINUE
      END
