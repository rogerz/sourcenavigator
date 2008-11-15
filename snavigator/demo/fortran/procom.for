      SUBROUTINE PROCOM
*-----------------------------------------------------------------------
*
*   Processes common blocks: collects name lists,
*   marks the variables referenced in each routine
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'class.h'
      include 'flags.h'
      include 'cursta.h'
      include 'flwork.h'
      include 'state.h'
      LOGICAL RANGE
      CHARACTER*(mxnmch) SCB
*--- get external statement number
      ICLE=ISTMDS(6,ICURCL(1))
      IF(ICLE.EQ.12)  THEN
*--- common block
         IV=0
         ICOMMB=ISTMDS(17,ICURCL(1))
         ICOMMV=ISTMDS(21,ICURCL(1))
   10    CONTINUE
*--- find c.b. name, and first and last variable in this c.b.
         IF(ITBIT(NAMTYP(ISNAME+IV+1),ICOMMB).EQ.0) THEN
*--- first name not c.b. name ---> blank common
            SCB='BLANKCOM'
         ELSE
            SCB=SNAMES(ISNAME+IV+1)
            IV=IV+1
         ENDIF
*--- last var. in this c.b. is min. pos. of '//', or c.b. name
         IPT=NSSTRT(IV+1)-1
         INS=INDEX(SSTA(IPT+1:NCHST),'//')
         IND=INDEX(SSTA(IPT+1:NCHST),'/ /')
         IF(IND.GT.0) THEN
            IF(INS.GT.0) THEN
               INS=MIN(IND,INS)
            ELSE
               INS=IND
            ENDIF
         ENDIF
         IF(INS.GT.0) THEN
            INS=IPT+INS
         ELSE
            INS=NCHST+1
         ENDIF
*--- collect variable name ref.s in IWS
         N=0
         ILOW=IV+1
         DO 20 I=ILOW,NSNAME
            K=ISNAME+I
            NT=NAMTYP(K)
            IF(ITBIT(NT,ICOMMB).NE.0) GOTO 30
            IF(NSSTRT(I).GT.INS) GOTO 30
            IF(ITBIT(NT,ICOMMV).NE.0) THEN
               IF(NCBVAR+N.EQ.MXNAME) GOTO 180
               N=N+1
               SCBVAR(NCBVAR+N)=SNAMES(K)
            ENDIF
            IV=I
   20    CONTINUE
   30    CONTINUE
*--- store in name list for this common block
         IF(N.GT.0) THEN
            CALL LSORT(SCBVAR(NCBVAR+1),IWS,.FALSE.,N)
*--- look for name in c.b. name table
            CALL NAMSRC(SCB,SCBNAM,NCBNAM,IPOS,LAST)
            IF(IPOS.EQ.0) THEN
*--- not in table - add to existing
               IF(NCBNAM.EQ.MAXGRP) GOTO 190
               DO 40 I=NCBNAM,LAST+1,-1
                  SCBNAM(I+1)=SCBNAM(I)
                  NCBGRP(I+1)=NCBGRP(I)
                  KCBGRP(I+1)=KCBGRP(I)
   40          CONTINUE
               NCBNAM=NCBNAM+1
               SCBNAM(LAST+1)=SCB
               NCBGRP(LAST+1)=N
               KCBGRP(LAST+1)=NCBVAR
            ELSE
*--- already in table - add in place, and merge
               CALL NAMOVE(SCBVAR,KCBGRP(IPOS)+NCBGRP(IPOS),NCBVAR,N)
               CALL LMERGE(SCBVAR,IWS,.FALSE.,KCBGRP(IPOS),NCBGRP(IPOS),
     +         N)
               DO 50 I=1,NCBNAM
                  IF(KCBGRP(I).GT.KCBGRP(IPOS)) KCBGRP(I)=KCBGRP(I)+N
   50          CONTINUE
               DO 60 I=1,NEQNAM
                  IF(KEQGRP(I).GT.KCBGRP(IPOS)) KEQGRP(I)=KEQGRP(I)+N
   60          CONTINUE
               NCBGRP(IPOS)=NCBGRP(IPOS)+N
            ENDIF
            NCBVAR=NCBVAR+N
         ENDIF
         IF(IV.LT.NSNAME) GOTO 10
      ELSEIF(ICLE.EQ.30)  THEN
*--- EQUIVALENCE
         IV=0
         IPT=0
   70    CONTINUE
         ILB=INDEX(SSTA(IPT+1:NCHST),'(')
         IF(ILB.GT.0) THEN
            ILB=ILB+IPT
            CALL SKIPLV(SSTA,ILB+1,NCHST,.FALSE.,IRB,ILEV)
            IF(IRB.GT.0) THEN
               IPT=IRB
*--- only names outside brackets (inside each group)
               CALL GETRNG(ILB+1,IRB-1,IWS)
               ILOW=IV+1
               N=0
               DO 80 I=ILOW,NSNAME
                  IF(NSSTRT(I).GT.IRB) GOTO 90
                  IF(.NOT.RANGE(NSSTRT(I),IWS)) THEN
                     IF(NCBVAR+N.EQ.MXNAME) GOTO 180
                     N=N+1
                     SCBVAR(NCBVAR+N)=SNAMES(ISNAME+I)
                  ENDIF
                  IV=I
   80          CONTINUE
   90          CONTINUE
               IF(N.GT.0) THEN
                  IF(NEQNAM.EQ.MAXGRP) GOTO 200
                  CALL LSORT(SCBVAR(NCBVAR+1),IWS,.FALSE.,N)
                  NEQNAM=NEQNAM+1
                  KEQGRP(NEQNAM)=NCBVAR
                  NEQGRP(NEQNAM)=N
                  NCBVAR=NCBVAR+N
               ENDIF
               IF(IPT.LT.NCHST) GOTO 70
            ENDIF
         ENDIF
      ELSEIF(ICLE.EQ.16.OR.ISTMDS(11,ICURCL(1)).EQ.1)  THEN
*--- DATA statement, or executable, i.e. start of routine
         IF(.NOT.STATUS(13)) THEN
*--- merge all equiv. groups with common blocks
            STATUS(13)=.TRUE.
  100       CONTINUE
            DO 150 IE=1,NEQNAM
               KEQ=KEQGRP(IE)
               NEQ=NEQGRP(IE)
               DO 140 IEI=1,NEQ
                  DO 130 IC=1,NCBNAM
                     CALL NAMSRC(SCBVAR(KEQGRP(IE)+IEI),SCBVAR(KCBGRP
     +               (IC)+1), NCBGRP(IC),IPOS,LAST)
                     IF(IPOS.NE.0) THEN
*--- equiv. group var. is in this c.b., add complete group
                        CALL NAMOVE(SCBVAR,KCBGRP(IC)+NCBGRP(IC),KEQ,
     +                  NEQ)
                        KCB=KCBGRP(IC)
                        DO 110 I=1,NCBNAM
                           IF(KEQ.LT.KCB) THEN
                              IF(KCBGRP(I).LE.KCB.AND.KCBGRP(I).GT.KEQ)
     +                        KCBGRP(I)=KCBGRP(I)-NEQ
                           ELSE
                              IF(KCBGRP(I).GT.KCB.AND.KCBGRP(I).LT.KEQ)
     +                        KCBGRP(I)=KCBGRP(I)+NEQ
                           ENDIF
  110                   CONTINUE
                        DO 120 I=1,NEQNAM
                           IF(KEQ.LT.KCB) THEN
                              IF(KEQGRP(I).LE.KCB.AND.KEQGRP(I).GT.KEQ)
     +                        KEQGRP(I)=KEQGRP(I)-NEQ
                           ELSE
                              IF(KEQGRP(I).GT.KCB.AND.KEQGRP(I).LT.KEQ)
     +                        KEQGRP(I)=KEQGRP(I)+NEQ
                           ENDIF
  120                   CONTINUE
                        CALL LMERGE(SCBVAR,IWS,.FALSE.,KCBGRP(IC),NCBGRP
     +                  (IC),NEQ)
                        CALL SUPMUL(SCBVAR,IWS,.FALSE.,KCBGRP(IC),
     +                  NCBGRP(IC)+NEQ,N)
                        NCBGRP(IC)=N
                        NEQGRP(IE)=0
*--- restart search
                        GOTO 100
                     ENDIF
  130             CONTINUE
  140          CONTINUE
  150       CONTINUE
         ENDIF
*--- look for any name in statement being in a c.b.
         DO 170 I=1,NSNAME
            DO 160 IC=1,NCBNAM
               CALL NAMSRC(SNAMES(ISNAME+I),SCBVAR(KCBGRP(IC)+1),NCBGRP
     +         (IC), IPOS,LAST)
               IF(IPOS.GT.0) THEN
*--- name is in this c.b. - set flag, count
                  IF(LCBVAR(KCBGRP(IC)+IPOS).EQ.0)
     +            LCBNAM(IC)=LCBNAM(IC)+1
                  LCBVAR(KCBGRP(IC)+IPOS)=LCBVAR(KCBGRP(IC)+IPOS)+1
                  GOTO 170
               ENDIF
  160       CONTINUE
  170    CONTINUE
      ENDIF
      GOTO 999
*--- error - name buffer overflow
  180 CONTINUE
      STATUS(12)=.TRUE.
      WRITE(MPUNIT,10000) MXNAME,SCROUT(:lenocc(scrout))
      GOTO 999
  190 CONTINUE
      STATUS(12)=.TRUE.
      WRITE(MPUNIT,10010) MAXGRP,SCROUT(:lenocc(scrout))
      GOTO 999
  200 CONTINUE
      STATUS(12)=.TRUE.
      WRITE(MPUNIT,10020) MAXGRP,SCROUT(:lenocc(scrout))
10000 FORMAT(/' +++++++++ WARNING - more than',I8,' variable names',
     +' in COMMON and EQUIV., routine ',A,' skipped')
10010 FORMAT(/' +++++++++ WARNING - more than',I8,' common block names',
     +', routine ',A,' skipped')
10020 FORMAT(/' +++++++++ WARNING - more than',I8,' groups',
     +' in EQUIVALENCE, routine ',A,' skipped')
  999 END
