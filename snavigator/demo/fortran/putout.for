      SUBROUTINE PUTOUT
*-----------------------------------------------------------------------
*
*   Writes the FORTRAN code output file
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'flags.h'
      include 'state.h'
      include 'jobsum.h'
      LOGICAL OUTFL
      DO 20 I=1,NSTAMM
         OUTFL=ACTION(7).AND.IMODIF(I).GT.10.OR.ACTION(8).AND.IMODIF(I).
     +   GT.0.OR.ACTION(9)
         IF (OUTFL)  THEN
            DO 10 J=NFLINE(I),NLLINE(I)
               IF(ACTION(23)) THEN
*--- compressed output = only up to last non-blank written
                  NUP=LASTNB(SIMA(J),2,MXLINE)
               ELSE
                  NUP=MXLINE
               ENDIF
               WRITE (MOUNIT,'(A)') SIMA(J)(:NUP)
               NSTATC(2)=NSTATC(2)+1
   10       CONTINUE
         ENDIF
   20 CONTINUE
      END
