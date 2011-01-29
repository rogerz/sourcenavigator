      SUBROUTINE INEXTR(SKEY,I1,I2,N)
*-----------------------------------------------------------------------
*  compacts all occurrences of a given key in the range indicated,
*  removes the key-words
*
*   Input
*   SKEY         = key to look for
*   I1           = start of input command range in SIMA
*   I2           = end         -         -
*   Output
*   N            = no. of characters in compacted string
*                  or -1 if key not found.
*   SSTA, common /ALCAZA/  contains the string
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'state.h'
      CHARACTER*3 SKEY
      N=-1
      DO 20 I=I1,I2
         IF (SKEY.EQ.SIMA(NFLINE(I))(1:3))  THEN
*--- key found - skip key-word, string, replace ';' by ','
            IF (N.LT.0) N=0
            IS=NFLINE(I)
            IL=NLLINE(I)
            IP=NLTYPE(IL)
            SIMA(IL)(IP:IP)=','
            IND=INDEX(SIMA(IS),',')
            IF (IND.EQ.0.OR.IND.EQ.NLTYPE(IS))  THEN
               KADD=1
            ELSE
               KADD=0
            ENDIF
            DO 10 J=IS+KADD,IL
               IF (J.EQ.IS)  THEN
                  IT=IND+1
               ELSE
                  IT=1
               ENDIF
               L=NLTYPE(J)+1-IT
               IF (N+L.GT.MDIMST)  THEN
                  WRITE (MPUNIT,10000) SKEY,MDIMST
                  N=-1
                  GOTO 999
               ENDIF
               SSTA(N+1:N+L)=SIMA(J)(IT:NLTYPE(J))
               N=N+L
   10       CONTINUE
         ENDIF
   20 CONTINUE
10000 FORMAT(/1X,8('*=*='),' WARNING - total length of key ', A,
     +' more than ',I5,' characters, key ignored')
  999 END
