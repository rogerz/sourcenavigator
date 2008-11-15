      SUBROUTINE SETIMP
*-----------------------------------------------------------------------
*
*   Sets the default type list for an IMPLICIT statement, updates the
*   already existing routine names  (except for strongly typed).
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'condec.h'
      include 'flwork.h'
      include 'cursta.h'
      include 'typdef.h'
      CHARACTER STYP(6)*16,STEMP*1,SPREV*1,STEMP2*2
      DIMENSION LTYP(6)
      DATA STYP/'#INTEGER','#REAL','#LOGICAL','#COMPLEX',
     +'#DOUBLEPRECISION','#CHARACTER'/
      DATA LTYP/8,5,8,8,16,10/
      include 'condat.h'
      IPT=0
   10 CONTINUE
      IND=NCHST
      DO 20 I=1,6
         CALL MATCH(STYP(I),1,LTYP(I),SSTA,IPT+1,NCHST,.FALSE.,IPOS,ILEV
     +   ,NSPEC,IWS,IWS)
         IF (IPOS.GT.0.AND.IPOS.LE.IND)  THEN
            IND=IPOS
            IT=I
         ENDIF
   20 CONTINUE
      IF (IND+3.GT.NCHST) GOTO 999
      IPT=IND
*--- skip possible '*(...)' following the key
      CALL GETNBL(SSTA(IPT+1:NCHST),STEMP2,NN)
      IF (NN.LT.2) GOTO 999
      IF(STEMP2.EQ.'*(')  THEN
         IPT=IPT+INDEX(SSTA(IPT+1:NCHST),'(')
         CALL SKIPLV(SSTA,IPT+1,NCHST,.FALSE.,IPOS,ILEV)
         IF (IPOS.EQ.0) GOTO 999
         IPT=IPOS
      ENDIF
*--- get start and end of bracket following type
      IND=INDEX(SSTA(IPT+1:NCHST),'(')
      IF (IND.EQ.0) GOTO 999
      IPT=IPT+IND
      CALL SKIPLV(SSTA,IPT+1,NCHST,.FALSE.,IPOS,ILEV)
      IF (IPOS.EQ.0) GOTO 999
*--- loop over bracket, set type, reset types routine name table
      SPREV=' '
      KP=53
      DO 40 I=IPT+1,IPOS-1
         STEMP=SSTA(I:I)
         IF (STEMP.EQ.' ') GOTO 40
         K=ICVAL(STEMP)
         IF (K.GT.0.AND.K.LE.26)  THEN
            IF (SPREV.EQ.'-')  THEN
               DO 30 J=KP,K
                  KVTYPE(J)=IT
   30          CONTINUE
            ELSE
               KVTYPE(K)=IT
            ENDIF
            KP=K
         ENDIF
         SPREV=STEMP
   40 CONTINUE
      IPT=IPOS
      GOTO 10
  999 END
