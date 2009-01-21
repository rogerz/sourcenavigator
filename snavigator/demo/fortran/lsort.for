      SUBROUTINE LSORT(SLIST,NACC,FLACC,NS)
*-----------------------------------------------------------------------
*
*--- sorts a list in itself alphabetically, updates NACC
*
*--- input
*    SLIST     list containing all names
*    NACC      array to be re-arranged with sort
*    FLACC     if true, NACC is actually updated
*    NS          # of elements
*-----------------------------------------------------------------------
      include 'param.h'
      CHARACTER *(MXNMCH)  SLIST(*),SLOC
      DIMENSION NACC(*)
      LOGICAL ENDFL,FLACC
      IF(NS.GT.1)  THEN
   10    CONTINUE
         ENDFL=.TRUE.
         DO 20 I=2,NS
            IF (SLIST(I-1).GT.SLIST(I)) THEN
               ENDFL=.FALSE.
               SLOC=SLIST(I-1)
               SLIST(I-1)=SLIST(I)
               SLIST(I)=SLOC
               IF(FLACC) THEN
                  NLOC=NACC(I-1)
                  NACC(I-1)=NACC(I)
                  NACC(I)=NLOC
               ENDIF
            ENDIF
   20    CONTINUE
         IF (.NOT.ENDFL) GOTO 10
      ENDIF
      END
