      SUBROUTINE PRENUM
*-----------------------------------------------------------------------
*
*  Makes a list of statement numbers, replaces old by new in label field
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'flags.h'
      include 'class.h'
      include 'state.h'
      include 'keycom.h'
      include 'flwork.h'
      LOGICAL FORMFL,RETFL,ENDFL
      NSTANU=0
      N=0
      DO 10 I=1,NSTAMM
         IF (ICLASS(I,1).NE.0)  THEN
            NN=NEXTIN(SIMA(NFLINE(I)),1,5)
            IF (NN.NE.0)  THEN
               N=N+1
               IWS(N)=NN
            ENDIF
         ENDIF
   10 CONTINUE
      IF (N.EQ.0) GOTO 999
      CALL SORTSP(N,IWS,NSTANU)
      IF(NSTANU.GT.MAXNUM)  THEN
         WRITE (MPUNIT,10000) MAXNUM,SCROUT
         NSTANU=0
         GOTO 999
      ENDIF
*--- get values for starts, steps etc.
      DO 20 IKY=1,NGLSET
         IF (KEYREF(IKY,1).EQ.7) GOTO 30
   20 CONTINUE
      GOTO 120
   30 CONTINUE
      KKS=KEYREF(IKY,3)
*--- start and step for normal statements
      KST=KEYINT(KKS+1)
      NST=KEYINT(KKS+2)
*--- FORMAT statements
      KFOR=KEYINT(KKS+3)
      NFOR=KEYINT(KKS+4)
*--- RETURN
      KRET=KEYINT(KKS+5)
      NRET=KEYINT(KKS+6)
*--- END
      NEND=KEYINT(KKS+7)
      FORMFL=KFOR.GT.0
      RETFL=KRET.GT.0
      ENDFL=NEND.GT.0
      KST=KST-NST
      KFOR=KFOR-NFOR
      KRET=KRET-NRET
      DO 40 I=1,NSTANU
         KSTANU(I)=IWS(I)
         KSTARE(I)=0
   40 CONTINUE
*--- count FORMAT statements which have to be displaced
      NF=0
      DO 70 I=1,NSTAMM
         ICL=ICLASS(I,1)
         IF (ICL.NE.0)  THEN
            IF(ICL.EQ.IIF.or.icl.eq.iif+71)  ICL=ICLASS(I,2)
            NN=NEXTIN(SIMA(NFLINE(I)),1,5)
            IF (NN.NE.0)  THEN
*--- find statement number in sorted table.
*    The value of 40 for switching from direct to binary search is
*    valid for VAX/780, but probably reasonable elsewhere as well.
               IF (NSTANU.LE.40)  THEN
                  DO 50 J=1,NSTANU
                     IF (KSTANU(J).EQ.NN) GOTO 60
   50             CONTINUE
                  GOTO 120
   60             CONTINUE
                  IPOS=J
               ELSE
                  CALL BINSRC(NN,KSTANU,NSTANU,IPOS,LAST)
                  IF (IPOS.EQ.0) GOTO 120
               ENDIF
               IF(KSTARE(IPOS).EQ.0)  THEN
                  IF (FORMFL.AND.ICL.EQ.IFORMT)  THEN
                     KFOR=KFOR+NFOR
                     NEW=KFOR
                  ELSEIF (RETFL.AND.(ICL.EQ.IRETUR.or.icl.eq.iretur+71))  
     &            THEN
                     KRET=KRET+NRET
                     NEW=KRET
                  ELSEIF (ENDFL.AND.(ICL.EQ.IEND.or.icl.eq.iend+71))
     &            THEN
                     NEW=NEND
                  ELSE
                     KST=KST+NST
                     NEW=KST
                  ENDIF
                  KSTARE(IPOS)=NEW
               ENDIF
               IF (ACTION(14).AND.ICL.EQ.IFORMT.AND.NF.LT.1000)  THEN
*--- remember FORMAT statements to be put at end
                  NF=NF+1
                  IWS(NF)=I
                  IWS(1000+NF)=NFLINE(I)
                  IWS(2000+NF)=NLLINE(I)
                  IWS(3000+NF)=ICLASS(I,1)
                  IWS(4000+NF)=ICLASS(I,2)
                  IWS(5000+NF)=IMODIF(I)
               ENDIF
            ENDIF
         ENDIF
   70 CONTINUE
      IF(NF.GT.0)  THEN
*--- put FORMAT statements in front of last statement
         DO 80 ILAST=NSTAMM,1,-1
            IF(ICLASS(ILAST,1).NE.0) GOTO 90
   80    CONTINUE
   90    CONTINUE
*--- ILAST is last FORTRAN statement
         IS=IWS(1)
         K=IS-1
         N=1
         DO 100 I=IS,ILAST-1
            IF (I.EQ.IWS(N).AND.N.LE.NF)  THEN
               N=N+1
            ELSE
               K=K+1
               NFLINE(K)=NFLINE(I)
               NLLINE(K)=NLLINE(I)
               ICLASS(K,1)=ICLASS(I,1)
               ICLASS(K,2)=ICLASS(I,2)
               IMODIF(K)=IMODIF(I)
            ENDIF
  100    CONTINUE
         K=ILAST-NF-1
         DO 110 I=1,NF
            NFLINE(K+I)=IWS(1000+I)
            NLLINE(K+I)=IWS(2000+I)
            ICLASS(K+I,1)=IWS(3000+I)
            ICLASS(K+I,2)=IWS(4000+I)
            IMODIF(K+I)=IWS(5000+I)
  110    CONTINUE
      ENDIF
      GOTO 999
  120 CONTINUE
      WRITE (MPUNIT,10010) SCROUT
      NSTANU=0
10000 FORMAT(/' ++++++ Warning - more than',I5,' statement numbers',
     +'in routine ',A,' , not renumbered')
10010 FORMAT(/' ++++++ Warning - serious error in routine PRENUM ',
     +'when processing routine ',A,' , not renumbered')
  999 END
