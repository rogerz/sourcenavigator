      SUBROUTINE INDECO
*-----------------------------------------------------------------------
*
* Complete processing of user commands on input.
* The input is received from routine INUSER.
* The output is stored in commons  /FLAGS/, /KEYINP/, and /SKEYNP/
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'state.h'
      include 'keycom.h'
      include 'flags.h'
      include 'flwork.h'
      include 'class.h'
      include 'condec.h'
*
      DIMENSION NSUBKY(MTOTKY),KSUBKY(MTOTKY),KDEFKY(MTOTKY), KACTKY
     +(MTOTKY),KLISKY(MTOTKY),KKEYLS(MTOTKY),KKEYLG(MTOTKY), KSUBRF
     +(MSUBKY),KSUBIX(MSUBKY),KSUBAC(MSUBKY),KSUBLG(MSUBKY), KSUBLS
     +(MSUBKY),KDEFAU(7,2),IBIT(3)
*   NSUBKY(I) = # of sub-keys of key I
*   KSUBKY(I) = start-1 of sub-key list in KSUBRF
*   KDEFKY(I) = default flag if no sub-key given
*   KACTKY(I) = action flag to be set by key I
*   KLISKY(I) = cumulative 'type of input' indicator:
*               1   integer list given
*               2   name list given
*               4   string list given
*   KKEYLS(I) = for key I, ref. to KDEFAU for numerical default values
*   KKEYLG(I) = for key I, no. of numerical default values in KDEFAU
*   KSUBRF    = ref. list of sub-keys
*   KSUBIX(J) = for sub-key number J, 'type of action' indicator:
*               -2   insert list of non-executable statements
*               -1   insert list of executable statements
*               > 0: p, where p is the position of the first integer
*               behind the sub-key of the integer list (FORMAT=... etc.)
*   KSUBLG(J) = for sub-key number J, no. of words for default values
*   KSUBAC(J) = for sub-key number J, action flag to be set, or zero
*   KSUBLS(J) = for sub-key J, ref. to default integer list
*   KDEFAU(I,J) = for above ref., defaults
*   IBIT      = temporary storage for bits from KLISKY
      CHARACTER*3 STRKEY(MTOTKY),SUBKEY(MSUBKY)
*    STRKEY = list of keys
*   SUBKEY = list of sub-keys
      CHARACTER STEMP*1,STEMP3*3,SLNAM*(MXNMCH)
      DATA STRKEY/'OR;','END','PRI','LIS','OUT','FIR','STA','OPT', 'REP'
     +,'ROU','NAM','STR','CLA'/
      DATA SUBKEY/'CHA','END','FOR','FUL','GLO','ILL','IND','NUM', 'QUO'
     +,'RET','SEP','EXE','NEX','PAR','CHA','PAR','FUL','SEP', 'TYP',
     +'USE','COM','COM','GOT','TRE'/
      DATA NSUBKY/0,0,4,3,4,0,6,5,0,0,0,0,2/
      DATA KSUBKY/0,0,0,4,7,11,11,17,22,22,22,22,22/
      DATA KDEFKY/0,0,5,1,8,0,0,0,0,0,0,0,0/
      DATA KACTKY/0,0,0,0,0,10,13,0,0,16,18,19,17/
      DATA KLISKY/0,0,0,0,0,0,0,0,6,2,2,4,1/
      DATA KKEYLS/6*0,1,6*0/
      DATA KKEYLG/6*0,7,6*0/
      DATA KSUBRF/1,4,6,14,5,11,19,15,16,17,21,2,3,8,10,18,23,24,7,9,
     +20,22,12,13/
      DATA KSUBIX/0,7,3,0,0,0,1,1,2,5,0,-1,-2,8*0,3,2*0/
      DATA KSUBLG/0,7,7,0,0,0,3,7,3,7,11*0,3,2*0/
      DATA KSUBAC/4,0,0,6,2,3,21,0,11,0,1,0,0,5,7,8,9,14,20,22,23,27,
     +28,29/
      DATA KSUBLS/0,0,0,0,0,0,2,0,2,12*0,2,2*0/
*--- in KDEFAU, under 1:
*    defaults for statement numbers(2),formats(2),returns(2),end(1)
*    under 2: defaults for INDFAC (1), and IBLPAD (1)
      DATA KDEFAU/10,10,0,10,0,1,0, 3,1,0,4*0/
 
*
      include 'condat.h'
*--- read all input commands, pre-process, store in SIMA
      CALL INUSER
*--- check for illegal keys
      IPR=0
      DO 20 IS=1,NSTAMM
         STEMP3=SIMA(NFLINE(IS))(1:3)
         DO 10 IC=1,MTOTKY
            IF (STEMP3.EQ.STRKEY(IC)) GOTO 20
   10    CONTINUE
         WRITE (MPUNIT,10020) STEMP3
         IF (IPR.EQ.0)  THEN
            WRITE (MPUNIT,10030) STRKEY
            IPR=1
         ENDIF
   20 CONTINUE
*--- start decoding
      NKEY=0
*--- loop over global (IORSET=0) and local keys
      DO 160 IORSET=0,NORSET
         IF (IORSET.EQ.0)  THEN
            ILOW=3
            IUP=MGLOKY
            I1=1
            I2=NSTAMM
         ELSE
            ILOW=MGLOKY+1
            IUP=MTOTKY
         ENDIF
         DO 150 IKY=ILOW,IUP
            NSINT=0
            NFINT=0
            IF (IORSET.NE.0)  THEN
               I1=NSSTRT(IORSET)
               I2=NSEND(IORSET)
            ENDIF
*--- collect all occurences (either globally, or in this OR-set)
*    of this key
            CALL INEXTR(STRKEY(IKY),I1,I2,NL)
*--- complete key now in SSTA, length NL (characters), cleaned
*    from key-words.
            IF (NL.LT.0) GOTO 150
*--- set bit string for integer list etc.
            N=KLISKY(IKY)
            DO 30 J=3,1,-1
               IBIT(J)=N/2**(J-1)
               N=N-IBIT(J)*2**(J-1)
   30       CONTINUE
*--- count
            IF (IORSET.EQ.0)  THEN
               NGLSET=NGLSET+1
            ELSE
               IF (NORCOM(IORSET).EQ.0) KORCOM(IORSET)=NKEY
               NORCOM(IORSET)=NORCOM(IORSET)+1
            ENDIF
            NKEY=NKEY+1
            KEYREF(NKEY,1)=IKY
*--- set action flags
            IF (KACTKY(IKY).NE.0)  THEN
               ACTION(KACTKY(IKY))=.TRUE.
            ENDIF
*--- defaults for keys
            IF (KKEYLS(IKY).GT.0.AND.KEYREF(NKEY,2).EQ.0)  THEN
               NKS=KKEYLG(IKY)
               KEYREF(NKEY,2)=NKS
               KEYREF(NKEY,3)=NKYINT
               KK=KKEYLS(IKY)
               DO 40 JJ=1,NKS
                  NKYINT=NKYINT+1
                  KEYINT(NKYINT)=KDEFAU(JJ,KK)
   40          CONTINUE
            ENDIF
*--- sub-keys
            NSFD=0
            DO 80 JS=1,NSUBKY(IKY)
               JSC=KSUBKY(IKY)+JS
               JSN=KSUBRF(JSC)
               IF(NL.EQ.0)  THEN
                  IND=0
               ELSE
                  IND=INDEX(SSTA(:NL),SUBKEY(JSN))
               ENDIF
               IF (IND.GT.0)  THEN
*--- sub-key found
                  NSFD=1
                  CALL SKIPTP(2,SSTA,IND,NL,.FALSE.,JPT,ILEV)
                  IF (KSUBIX(JSN).GT.0)  THEN
*--- integers following
                     IF (KEYREF(NKEY,2).EQ.0)  THEN
*--- get length and reserve space
                        NKS=KSUBLG(JSN)
                        KEYREF(NKEY,2)=NKS
                        KEYREF(NKEY,3)=NKYINT
*--- set default values
                        KK=KSUBLS(JSN)
                        DO 50 JJ=1,NKS
                           NKYINT=NKYINT+1
                           KEYINT(NKYINT)=KDEFAU(JJ,KK)
   50                   CONTINUE
                     ENDIF
*--- integer position
                     IPOS=KSUBIX(JSN)
   60                CONTINUE
                     CALL GETNBL(SSTA(JPT+1:NL),STEMP,N)
                     IF(N.GT.0.AND.(STEMP.EQ.'='
     +               .OR.NUMCH(STEMP)))  THEN
*--- next comma position
                        JCOM=JPT+INDEX(SSTA(JPT+1:NL),',')
                        IF(JCOM.EQ.JPT) JCOM=NL
*--- get integer
                        CALL GETINT(SSTA,JPT,JCOM,KFCH,KLCH,NN)
                        IF (KFCH.GT.0) THEN
*--- integer found
                           IF(NN.GT.0) KEYINT(KEYREF(NKEY,3)+IPOS)=NN
                           IPOS=IPOS+1
                           JPT=JCOM
                           IF (IPOS.LE.NKS) GOTO 60
                        ENDIF
                     ENDIF
                  ELSEIF(KSUBIX(JSN).LT.0)  THEN
*--- EXE or NEX, add corresponding classes
                     NTYP=KSUBIX(JSN)+2
*--- collect in IWS first
                     DO 70 JCL=1,NCLASS
                        IF (ISTMDS(11,JCL).EQ.NTYP)  THEN
                           NSINT=NSINT+1
                           IWS(NSINT)=ISTMDS(6,JCL)
                        ENDIF
   70                CONTINUE
                  ENDIF
                  IF (KSUBAC(JSN).GT.0)  THEN
*--- action flag
                     ACTION(KSUBAC(JSN))=.TRUE.
                  ENDIF
               ENDIF
*--- end of sub-key loop
   80       CONTINUE
            IF (NSFD.EQ.0)  THEN
*--- no sub-key found - set default flag if any
               IF (KDEFKY(IKY).GT.0) ACTION(KDEFKY(IKY))=.TRUE.
            ENDIF
*--- get integers if any
            IF (IBIT(1).NE.0)  THEN
               JPT=0
               KADD=0
   90          CONTINUE
               CALL GETINT(SSTA,JPT+1,NL,KFCH,KLCH,NN)
               IF (KFCH.GT.0)  THEN
*--- integer found
                  JPT=KLCH
                  IF (KADD.EQ.0)  THEN
                     NSINT=NSINT+1
                     IWS(NSINT)=NN
                  ELSE
                     NFINT=NFINT+1
                     IWS(KADD+NFINT)=NN
                  ENDIF
                  IF (JPT.LT.NL)  THEN
*--- store those after IF ref. separately
                     IF (SSTA(JPT+1:JPT+1).EQ.'('.AND.KADD.EQ.0.AND.
     +               (ISTMDS(6,IIF).EQ.NN.or.istmds(6,iif+71).eq.nn)) 
     &               THEN
                        KADD=MXKINT
                     ELSEIF (SSTA(JPT+1:JPT+1).EQ.')')  THEN
                        KADD=0
                     ENDIF
                     GOTO 90
                  ENDIF
               ENDIF
*--- store integers (classes),in the following way:
*  # of simple, plus those following, # of classes behind IF,
*  plus those following
               IF (NSINT.GT.0)  THEN
                  KEYREF(NKEY,3)=NKYINT
*--- sort and suppress multiples
                  CALL SORTSP(NSINT,IWS,N)
                  KEYINT(NKYINT+1)=N
                  DO 100 J=1,N
                     KEYINT(NKYINT+J+1)=IWS(J)
  100             CONTINUE
                  CALL SORTSP(NFINT,IWS(MXKINT+1),NN)
                  KEYINT(NKYINT+N+2)=NN
                  DO 110 J=1,NN
                     KEYINT(NKYINT+N+J+2)=IWS(MXKINT+J)
  110             CONTINUE
                  KEYREF(NKEY,2)=N+NN+2
                  NKYINT=NKYINT+KEYREF(NKEY,2)
               ENDIF
            ENDIF
*--- get names if any
            IF (IBIT(2).NE.0)  THEN
               IPT=0
  120          CONTINUE
*--- find name outside string
               CALL GETNAM(SSTA,IPT+1,NL,KFCH,KLCH)
               IF (KFCH.GT.0)  THEN
*--- name found
                  IF (KEYREF(NKEY,4).EQ.0) KEYREF(NKEY,5)=NKYNAM
                  IF (NKYNAM.EQ.MXKNAM)  THEN
                     WRITE (MPUNIT,10000) NKYNAM
                     GOTO 150
                  ENDIF
                  SLNAM='        '
                  SLNAM(:KLCH+1-KFCH)=SSTA(KFCH:KLCH)
                  IPT=KLCH
*--- enter name in table (alphabetic for each key)
                  K=KEYREF(NKEY,5)
                  CALL NAMTAB(SLNAM,SKEYLS(K+1),NKYNAM-K,IPOS)
                  IF (IPOS.GT.0)  THEN
*--- name has been entered in table (otherwise already in)
                     IPOS=IPOS+K
                     DO 130 JJ=1,2
                        DO 130 J=NKYNAM,IPOS,-1
                           KNAMRF(J+1,JJ)=KNAMRF(J,JJ)
  130                CONTINUE
                     NKYNAM=NKYNAM+1
                     KEYREF(NKEY,4)=KEYREF(NKEY,4)+1
                     KNAMRF(IPOS,1)=0
                     KNAMRF(IPOS,2)=0
                  ENDIF
*--- check for string following if any
                  IF (IBIT(3).NE.0)  THEN
                     IF (SSTA(IPT+1:IPT+1).EQ.'{')  THEN
*--- delete string indicator (for string scan later on)
                        SSTA(IPT+1:IPT+1)=' '
                        IND=INDEX(SSTA(IPT+1:NL),'}')
                        IF (IND.GT.2.AND.IPOS.GT.0)  THEN
                           CALL INDECS(IPT+1,IPT+IND,*150)
                           KNAMRF(IPOS,1)=NKYSTR
                        ENDIF
                        IPT=IPT+MAX(IND,1)
                     ENDIF
*--- look for replacement string
                     IF (IPT+2.LT.NL.AND.SSTA(IPT+1:IPT+2).EQ.'={')
     +               THEN
                        IPT=IPT+1
                        SSTA(IPT+1:IPT+1)=' '
                        IND=INDEX(SSTA(IPT+1:NL),'}')
                        IF (IND.GT.2.AND.IPOS.GT.0)  THEN
                           CALL INDECS(IPT+1,IPT+IND,*150)
                           KNAMRF(IPOS,2)=NKYSTR
                           ACTION(15)=.TRUE.
                        ENDIF
                        IPT=IPT+MAX(IND,1)
                     ENDIF
                  ENDIF
                  GOTO 120
               ENDIF
            ENDIF
*--- check for strings to be replaced
            IF (IBIT(3).NE.0)  THEN
               IPT=0
  140          CONTINUE
               IND=INDEX(SSTA(IPT+1:NL),'{')
               IF (IND.GT.0)  THEN
                  IPT=IPT+IND-1
                  IND=INDEX(SSTA(IPT+1:NL),'}')
                  IF (IND.GT.2)  THEN
                     IF (NKYCHR.EQ.MXKNAM)  THEN
                        WRITE (MPUNIT,10010) NKYCHR
                        GOTO 150
                     ENDIF
                     CALL INDECS(IPT+1,IPT+IND,*150)
                     IF (KEYREF(NKEY,6).EQ.0) KEYREF(NKEY,7)=NKYCHR
                     KEYREF(NKEY,6)=KEYREF(NKEY,6)+1
                     NKYCHR=NKYCHR+1
                     KSTREF(NKYCHR,1)=NKYSTR
                  ENDIF
                  IPT=IPT+MAX(IND,1)
*--- look for replacement string
                  IF (IPT+2.LT.NL.AND.SSTA(IPT+1:IPT+2).EQ.'={')  THEN
                     IPT=IPT+1
                     IND=INDEX(SSTA(IPT+1:NL),'}')
                     IF (IND.GT.2)  THEN
                        CALL INDECS(IPT+1,IPT+IND,*150)
                        KSTREF(NKYCHR,2)=NKYSTR
                        ACTION(12)=.TRUE.
                     ENDIF
                     IPT=IPT+MAX(IND,1)
                  ENDIF
                  GOTO 140
               ENDIF
            ENDIF
  150    CONTINUE
  160 CONTINUE
*--- look for indentation multiple request
      INDFAC=0
      IBLPAD=1
      DO 170 I=1,NGLSET
         IF (KEYREF(I,1).EQ.8) GOTO 180
  170 CONTINUE
      GOTO 190
  180 CONTINUE
      IF(KEYREF(I,2).GT.0)  THEN
         IF(ACTION(21))  INDFAC=MIN(5,KEYINT(KEYREF(I,3)+1))
         IF(ACTION(11))  IBLPAD=MIN(10,KEYINT(KEYREF(I,3)+2))
         IF(ACTION(27))  ICBPRT=KEYINT(KEYREF(I,3)+3)
      ENDIF
  190 CONTINUE
      ACTION(25)=ACTION(1)
      ACTION(26)=ACTION(2)
*--- allow flags and options to be set directly
      CALL SETREQ
      ACTION(24)=ACTION(24).OR.ACTION(27).OR.ACTION(29)
      ACTION(27)=ACTION(27).AND..NOT.ACTION(29)
      ACTION(3)=ACTION(3).OR.ACTION(6)
*--- namelist / routine if common block option given, dito type
      ACTION(1)=ACTION(1).OR.ACTION(24)
      ACTION(20)=ACTION(20).OR.ACTION(24)
*--- print flags
      ACTION(5)=ACTION(5).OR.ACTION(6)
      ACTION(4)=ACTION(4).OR.ACTION(5)
10000 FORMAT(/1X,8('*=*='),' WARNING - max. no. of names =', I5,
     +' reached in commands, rest ignored')
10010 FORMAT(/1X,8('*=*='),' WARNING - max. no. of strings =', I5,
     +' reached in commands, rest ignored')
10020 FORMAT(/' *=*=*=*= WARNING - illegal key "',A,'" ignored',/)
10030 FORMAT(/' valid keys are:'/(1X,10A10))
      END
