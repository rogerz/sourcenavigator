      SUBROUTINE TREEST(MODE)
*-----------------------------------------------------------------------
*
*--- Prepares TREE file output (called for each statement)
*
*---Input
*   MODE        =0 : headerless routine start
*               >0 : normal routine start, or statement
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'treecom.h'
      include 'state.h'
      include 'flags.h'
      include 'class.h'
      include 'cursta.h'
      CHARACTER SNAME*(MXNMCH),STEMP*1
      LOGICAL LEXARS
      NCALLP=NCALLD
      IF(MODE.EQ.0)  THEN
*--- headerless routine start
         NCALLR=NCALLR+1
         ICALLR(NCALLR)=0
         CALLER(NCALLR)=SCROUT
      ELSE
*--- external class number
         ICLE=ISTMDS(6,ICURCL(1))
*--- routine header or entry
         IF(ISTMDS(14,ICURCL(1)).NE. 0.OR.ICLE.EQ.29) THEN
            IF(ICLE.EQ.29) THEN
               SNAME=SNAMES(ISNAME+1)
            ELSE
               SNAME=SCROUT
            ENDIF
*--- keep argument name list
            NARGEL=MAX(0,MIN(NSNAME-1,NOARG))
            DO 10 I=1,NARGEL
               SARGEL(I)=SNAMES(ISNAME+I+1)
   10       CONTINUE
*--- add routine name to list
            IF(NCALLR.LT.KENT) THEN
*--- keep statement ref. for callers
               ICALLR(NCALLR+1)=NSTREF
               CALLER(NCALLR+1)=SNAME
               NCALLR=NCALLR+1
            ENDIF
         ELSEIF(ICLE.EQ.31) THEN
*--- EXTERNAL statement - keep names
            DO 20 I=1,NSNAME
               IF(NEXEL.LT.KALL) THEN
                  NEXEL=NEXEL+1
                  SEXEL(NEXEL)=SNAMES(ISNAME+I)
               ENDIF
   20       CONTINUE
         ELSEIF(ISTMDS(11,ICURCL(1)).NE.0) THEN
*--- executable - scan all names
            IF(ICURCL(1).EQ.IIF.or.icurcl(1).eq.iif+71) THEN
               ICLE=ISTMDS(6,ICURCL(2))
               IND=INDEX(SSTA,'(')
               CALL SKIPLV(SSTA,IND+1,NCHST,.FALSE., IPT,ILEV)
            ELSE
               ICLE=ISTMDS(6,ICURCL(1))
               IPT=0
            ENDIF
            IF(ICLE.EQ.7) THEN
*--- subroutine call
               DO 30 I=1,NSNAME
                  IF(NSSTRT(I).GT.IPT) GOTO 40
   30          CONTINUE
               GOTO 999
   40          CONTINUE
*--- keep name ref. of call
               ISTC=I
*--- check against argument list, drop if argument
               DO 50 J=1,NARGEL
                  IF(SNAMES(ISNAME+I).EQ.SARGEL(J)) GOTO 55
   50          CONTINUE
               IF(NCALLD.LT.KALL) THEN
                  NCALLD=NCALLD+1
                  CALLED(NCALLD)=SNAMES(ISNAME+I)
                  CEDARG(NCALLD)=' '
                  IND=INDEX(SSTA(IPT+1:NCHST),'(')
                  IF(IND.GT.0) THEN
                     CALL ARGTYP(SSTA,.FALSE.,IPT+IND,NCHST,
     +               CEDARG(NCALLD))
                  ENDIF
               ENDIF
            ELSE
               ISTC=0
            ENDIF
   55       CONTINUE
            DO 70 I=1,NSNAME
               IF(I.EQ.ISTC) GOTO 70
               IF((ITBIT(NAMTYP(ISNAME+I),17).NE.0
     +         .AND.SNAMES(ISNAME+I).NE.SCROUT)
     +         .OR.ITBIT(NAMTYP(ISNAME+I),12).NE.0) THEN
*--- name is a function, or EXTERNAL
*--- check against argument list, drop if argument
                  DO 60 J=1,NARGEL
                     IF(SNAMES(ISNAME+I).EQ.SARGEL(J)) GOTO 70
   60             CONTINUE
                  IF(NCALLD.LT.KALL) THEN
                     IPT=NSEND(I)+1
                     IF(LEXARS(I))  THEN
*--- name appears as argument to another routine
                        NCALLD=NCALLD+1
                        CALLED(NCALLD)=SNAMES(ISNAME+I)
                        CEDARG(NCALLD)='$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
     +$$$$$$$$$$$$$$$$$$'
                     ELSE
                        STEMP=SSTA(IPT:IPT)
                        IF(STEMP.EQ.' ') THEN
                           IPT=IPT+1
                           STEMP=SSTA(IPT:IPT)
                        ENDIF
                        IF(STEMP.EQ.'(') THEN
                           CALL SKIPLV(SSTA,IPT+1,NCHST,.FALSE., IPOS,
     +                     ILEV)
                           IF(IPOS.GT.0) THEN
                              NCALLD=NCALLD+1
                              CALLED(NCALLD)=SNAMES(ISNAME+I)
                              CEDARG(NCALLD)=' '
                              CALL ARGTYP(SSTA,.FALSE.,IPT,IPOS,
     +                        CEDARG(NCALLD))
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
   70       CONTINUE
         ENDIF
      ENDIF
*--- suppress multiple subsequent called routines with identical
*    argument type lists
      IF(NCALLP.GT.0.AND.NCALLD.GT.NCALLP)  THEN
         IF(CALLED(NCALLD).EQ.CALLED(NCALLD-1)
     +   .AND.CEDARG(NCALLD).EQ.CEDARG(NCALLD-1))  NCALLD=NCALLD-1
      ENDIF
  999 END
