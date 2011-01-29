      SUBROUTINE SETTYP(MODE)
*-----------------------------------------------------------------------
*
*   Sets variable types for a given statement, or updates default list
*   and names so far in case of IMPLICIT.
*
*   Only sensible if called for all statements in a routine, and while
*   establishing a name list for that routine.
*
*   Input
*   MODE      = 0 : reset default type table, no further action
*             > 0 : process statement
*   SSTA (statement), NSNAME, NRNAME etc.
*   Output
*   NAMTYP in common /STATE/
*
*   Each type corresponds to a bit position (for testing use ITBIT).
*
*   Types are:
*
*   Bit          meaning
*
*     1          INTEGER
*     2          REAL
*     3          LOGICAL
*     4          COMPLEX
*     5          DOUBLE PRECISION
*     6          CHARACTER
*     7          PARAMETER
*     8          COMMON block name
*     9          NAMELIST name
*    10          statement function
*    11          INTRINSIC
*    12          EXTERNAL
*    13          PROGRAM name
*    14          BLOCK DATA name
*    15          SUBROUTINE
*    16          ENTRY
*    17          FUNCTION (intrinsic or external)
*    18          dimensioned
*    19          (routine or function) argument
*    20          in a COMMON block
*    21          strongly typed function (internal usage)
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'class.h'
      include 'flwork.h'
      include 'flags.h'
      include 'cursta.h'
      include 'state.h'
      include 'typdef.h'
      include 'condec.h'
      CHARACTER STEMP*1 ,STEMP1*1
      LOGICAL RANGE
      DIMENSION ILOC(MCLASS),KDEFTP(26),NLIM1(2),NLIM2(2)
*--- KDEFTP = default FORTRAN types (REAL and INTEGER) for first letter
*    KILOC  = last location of ISTMDS not relevant for ILOC
*    ILOC   = local copy of type descriptors from ISTMDS
      DATA KDEFTP/8*2,6*1,12*2/, KILOC/14/
      include 'condat.h'
      IF(MODE.EQ.0)  THEN
*--- routine header: reset default type table
         DO 10 I=1,26
            KVTYPE(I)=KDEFTP(I)
   10    CONTINUE
         GOTO 999
      ENDIF
      DO 20 I=ISNAME+1,ISNAME+NSNAME
         NAMTYP(I)=0
   20 CONTINUE
      IF(ICURCL(1).EQ.IIF.or.icurcl(1).eq.iif+71)  THEN
         IUP=2
*--- find end of IF(...)
         JPT=INDEX(SSTA(:NCHST),'(')
         CALL SKIPLV(SSTA,JPT+1,NCHST,.FALSE.,KND,ILEV)
         NLIM1(1)=1
         DO 30 I=1,NSNAME
            IF(NSSTRT(I).GT.KND) GOTO 40
   30    CONTINUE
         I=NSNAME+1
   40    CONTINUE
         NLIM2(1)=I-1
         NLIM1(2)=I
         NLIM2(2)=NSNAME
      ELSE
         IUP=1
         KND=NCHST
         NLIM1(1)=1
         NLIM2(1)=NSNAME
      ENDIF
      DO 120 IPART=1,IUP
         IF (IPART.EQ.1)  THEN
            ICL=ICURCL(1)
            KST=1
         ELSE
            ICL=ICURCL(2)
            KST=KND+1
            KND=NCHST
         ENDIF
*--- get flags, counts, and types
         DO 50 I=1,MCLASS-KILOC
            ILOC(I)=ISTMDS(KILOC+I,ICL)
   50    CONTINUE
         IFLG2=ILOC(1)/10
         IFLG1=ILOC(1)-10*IFLG2
         ILPT=2
         IULOOP=1
         IF(IFLG2.NE.0) THEN
*--- take only names outside brackets, get ranges for this
            CALL GETRNG(KST,KND,IWS)
         ENDIF
         IF(IFLG2.EQ.2) THEN
*--- treat COMMON block names specially
            IULOOP=2
            ICOMMB=ILOC(ILPT+1)
            NLPT=ILOC(ILPT)
         ENDIF
         IF(IFLG1.EQ.0) THEN
*--- treat all names the same
            ILOW=NLIM1(IPART)
            INUP=NLIM2(IPART)
            NLOOP=1
         ELSEIF(IFLG1.EQ.1) THEN
*--- different types for first name, and rest
            NLOOP=2
         ELSE
*--- special treatment for IMPLICIT statement
            CALL SETIMP
*--- update the already existing names except strongly typed
            DO 60 I=1,NRNAME
               NT=NAMTYP(IRNAME+I)
*--- do not change type of strongly typed function, nor parameter
               IF (ITBIT(NT,7).EQ.0.AND.ITBIT(NT,21).EQ.0) THEN
                  K=ICVAL(SNAMES(IRNAME+I)(1:1))
                  if(k.gt.26) k = k-26
                  NT=NT-MOD(NT,64)
                  CALL ISBIT(NT,KVTYPE(K))
                  NAMTYP(IRNAME+I)=NT
               ENDIF
   60       CONTINUE
            GOTO 999
         ENDIF
*--- the following IF(...) must stay here because of IMPLICIT
         IF (NSNAME.EQ.0.OR.ILOC(2).EQ.0) GOTO 999
         DO 110 ILOOP=IULOOP,NLOOP
            IF (IFLG1.NE.0) THEN
               IF (ILOOP.EQ.1) THEN
                  ILOW=NLIM1(IPART)
                  INUP=NLIM1(IPART)
               ELSE
                  IF(IFLG2.EQ.2) THEN
                     ILOW=NLIM1(IPART)
                  ELSE
                     ILOW=NLIM1(IPART)+1
                  ENDIF
                  INUP=NLIM2(IPART)
                  ILPT=ILPT+NLPT+1
               ENDIF
            ENDIF
            NLPT=ILOC(ILPT)
*--- loop over names
            DO 100 JN=ILOW,INUP
               IF (IFLG2.NE.0) THEN
*--- take only names outside brackets
                  IF (RANGE(NSSTRT(JN),IWS)) GOTO 100
               ENDIF
*--- check whether already typed in this statement (except COMMON)
            IF(IFLG2.LT.2)  THEN
               DO 70 JL=1,JN-1
                  IF (SNAMES(ISNAME+JL).EQ.SNAMES(ISNAME+JN)) THEN
                     NT=NAMTYP(ISNAME+JL)
                     IPOS=0
                     GOTO 90
                  ENDIF
   70          CONTINUE
               ENDIF
*--- check against existing routine name table
               CALL NAMSRC(SNAMES(ISNAME+JN),SNAMES(IRNAME+1),NRNAME,
     +         IPOS, LAST)
               IF (IPOS.EQ.0) THEN
*--- not yet in table
                  NT=0
               ELSE
                  NT=NAMTYP(IRNAME+IPOS)
               ENDIF
               IF(IFLG2.EQ.2) THEN
*--- common block
*--- look for common block name = /.../
                     NFCB=NSSTRT(JN)-1
                     STEMP=SSTA(NFCB:NFCB)
                     IF(STEMP.EQ.' ') THEN
                        NFCB=NFCB-1
                        STEMP=SSTA(NFCB:NFCB)
                     ENDIF
                     IF(STEMP.EQ.'/') THEN
                        NSCB=NSEND(JN)+1
                        IF(NSCB.LT.NCHST) THEN
                           STEMP=SSTA(NSCB:NSCB)
                           IF(STEMP.EQ.' ') STEMP=SSTA(NSCB+1:NSCB+1)
                           IF(STEMP.EQ.'/') THEN
                              NFCB=NFCB-1
                              STEMP1=SSTA(NFCB:NFCB)
                              IF(STEMP1.EQ.' ') STEMP1=SSTA(NFCB-1:NFCB
     +                        -1)
                              JNL=MAX(JN-1,1)
                              IF((JN.EQ.1.OR.ITBIT(NAMTYP(ISNAME+JNL),
     +                        ICOMMB).EQ.0).AND.STEMP1.NE.'/') THEN
                                 NT=0
                                 CALL ISBIT(NT,ICOMMB)
                                 NAMTYP(ISNAME+JN)=NT
                                 GOTO 100
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
               ENDIF
*--- loop over types (for first, or second, or all)
               DO 80 JT=ILPT+1,ILPT+NLPT
                  ITYP=ILOC(JT)
                  IF (ITYP.EQ.0) THEN
*--- skip if already typed (REAL, INTEGER, etc.)
                     IF (MOD(NT,64).NE.0) GOTO 80
*--- skip if ENTRY in SUBROUTINE
                     IF(STATUS(14).AND.ISTMDS(6,ICL).EQ.29) GOTO 80
*--- take default type
                     icvs = icval(snames(isname+jn)(1:1))
                     if(icvs.gt.26) icvs = icvs - 26
                     ITYP=KVTYPE(icvs)
                  ELSEIF (ITYP.LE.6) THEN
*--- strong typing - reset other types
                     NT=NT-MOD(NT,64)
                  ELSEIF (ITYP.EQ.10) THEN
*--- check for statement function declaration (not dimensioned)
                     IF (ITBIT(NT,18).NE.0) GOTO 80
*--- no':' allowed in bracket
                     JLB=INDEX(SSTA(KST:KND),'(')+KST-1
                     JRB=INDEX(SSTA(KST:KND),')')+KST-1
                     CALL POSCH(':',SSTA,JLB+1,JRB-1,.FALSE.,0,KPOS,
     +               ILEV)
                     IF (KPOS.NE.0) GOTO 80
                  ELSEIF (ITYP.EQ.17.OR.ITYP.EQ.18) THEN
*--- function (17) or array (18)
*    get next non-blank behind name
                     IF (NSEND(JN).EQ.KND) GOTO 80
                     CALL GETNBL(SSTA(NSEND(JN)+1:KND),STEMP,NN)
                     IF (NN.EQ.0.OR.STEMP.NE.'(')GOTO 80
                     IF (ITYP.EQ.17) THEN
*--- only function if not dimensioned
                        IF (ITBIT(NT,18).NE.0) GOTO 80
*--- should not be statement function
                        IF (ITBIT(NT,10).NE.0) GOTO 80
*--- no ':' allowed on zero level in bracket following
                        JLB=NSEND(JN)+INDEX(SSTA(NSEND(JN)+1:KND),'(')
                        CALL SKIPLV(SSTA,JLB+1,KND,.FALSE.,JRB,ILEV)
                        CALL POSCH(':',SSTA,JLB+1,JRB-1,.FALSE.,0,KPOS,
     +                  ILEV )
                        IF (KPOS.NE.0) GOTO 80
                     ENDIF
                  ENDIF
*--- type is accepted for this variable - set
                  CALL ISBIT(NT,ITYP)
   80          CONTINUE
   90          CONTINUE
               NAMTYP(ISNAME+JN)=NT
               IF (IPOS.GT.0) THEN
                  NAMTYP(IRNAME+IPOS)=NT
               ENDIF
  100       CONTINUE
  110    CONTINUE
  120 CONTINUE
  999 END
