      SUBROUTINE SUPMUL(SLIST,NACC,FLACC,IS,NS,NOUT)
*-----------------------------------------------------------------------
*
*--- suppresses multiple entries in sorted table, update NAMTYP
*
*--- input
*    SLIST     list containing all names
*    NACC      array to be re-arranged with sort
*    FLACC     if true, NACC is actually updated
*    IS      start-1 of table in SNAMES, /ALCAZA/
*    NS      length of table
*--- output
*    NOUT    new table length
*
*-----------------------------------------------------------------------
      include 'param.h'
      CHARACTER *(MXNMCH) SLIST(*)
      DIMENSION NACC(*)
      LOGICAL FLACC
      NQ=NS
      IF (NQ.LE.0)  THEN
         NOUT=0
      ELSE
         NOUT=1
         DO 10 I=2,NQ
            IF (SLIST(IS+I).NE.SLIST(IS+NOUT))  THEN
               NOUT=NOUT+1
               IF (I.NE.NOUT) THEN
                  SLIST(IS+NOUT)=SLIST(IS+I)
                  IF(FLACC)  NACC(IS+NOUT)=NACC(IS+I)
               ENDIF
            ENDIF
   10    CONTINUE
      ENDIF
      END
