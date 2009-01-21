      SUBROUTINE ARGTYP(STRING,CALLFL,I1,I2,ARG)
*-----------------------------------------------------------------------
*
*--- returns a list of argument types
*--- input
*    STRING(I1:I2) = '(...)' argument list
*    CALLFL        = .TRUE. if argument list of a caller, else .FALSE.
*--- output
*    ARG           character variable, 1 ch./argument
*                  'I' = integer
*                  'R' = real
*                  'D' = double prec.
*                  'K' = complex
*                  'C' = character
*                  'L' = logical
*                  'P' = procedure (subroutine or function passed)
*                  '*' = alternate ret.
*                  '$' = not determined
*
*   the rest is blank.
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'condec.h'
      include 'state.h'
      CHARACTER STRING*(*),ARG*(*),STYP*1, STEMP*1,SNAME*(MXNMCH),
     +ATYP*7
      LOGICAL BRNONE,CALLFL
      DATA ATYP/'IRLKDC$'/
      include 'condat.h'
      KPOS=I1
      N=0
   10 CONTINUE
      IPT=KPOS
*--- find end of each argument
      CALL POSCH(',',STRING,IPT+1,I2, .FALSE.,0,KPOS,ILEV)
      IF(KPOS.EQ.0)  KPOS=I2
      N=N+1
      STEMP=STRING(IPT+1:IPT+1)
      IF(STEMP.EQ.' ')  THEN
         IPT=IPT+1
         STEMP=STRING(IPT+1:IPT+1)
      ENDIF
      IF(STEMP.EQ.'*')  THEN
         ARG(N:N)='*'
      ELSE
         IF(STEMP.EQ.'+'.OR.STEMP.EQ.'-') THEN
            IPT=IPT+1
            STEMP=STRING(IPT+1:IPT+1)
            IF(STEMP.EQ.' ') THEN
               IPT=IPT+1
               STEMP=STRING(IPT+1:IPT+1)
            ENDIF
         ENDIF
         IF(INDEX('0123456789(.{',STEMP).NE.0) THEN
            CALL GETCON(STRING,IPT+1,KPOS,KLCH,STYP)
            IF(KLCH.EQ.0) GOTO 60
            IF(KLCH+1.EQ.KPOS.OR.(KLCH+2.EQ.KPOS .AND.STRING(KLCH+1:KLCH
     +      +1).EQ.' ' )) THEN
*--- argument is a simple constant
               ARG(N:N)=STYP
            ELSE
               GOTO 60
            ENDIF
         ELSEIF(ALPHCH(STEMP)) THEN
            CALL GETNAM(STRING,IPT+1,KPOS,KFCH, KNAM)
            KLCH=KNAM
            STEMP=STRING(KLCH+1:KLCH+1)
            IF(STEMP.EQ.' ') THEN
               KLCH=KLCH+1
               STEMP=STRING(KLCH+1:KLCH+1)
            ENDIF
            IF(STEMP.EQ.'(') THEN
*--- check for dimensioned variable, or function
               CALL SKIPLV(STRING,KLCH+2,KPOS, .FALSE.,KLCH,ILEV)
               STEMP=STRING(KLCH+1:KLCH+1)
               IF(STEMP.EQ.' ') THEN
                  KLCH=KLCH+1
                  STEMP=STRING(KLCH+1:KLCH+1)
               ENDIF
               BRNONE=.FALSE.
            ELSE
               BRNONE=.TRUE.
            ENDIF
            IF(KLCH+1.EQ.KPOS) THEN
*--- simple argument
               SNAME=' '
               CALL GETNBL(STRING(KFCH:KNAM),SNAME, NN)
               DO 20 IPOS=1,NSNAME
                  IF(SNAME.EQ.SNAMES(ISNAME+IPOS)) GOTO 30
   20          CONTINUE
               GOTO 60
   30          CONTINUE
               NT=NAMTYP(ISNAME+IPOS)
               IF(BRNONE.AND. (CALLFL.AND.(ITBIT(NT,15).NE.0.OR.ITBIT
     +         (NT,17).NE.0) .OR.(.NOT.CALLFL.AND.ITBIT(NT,12).NE.0)))
     +         THEN
*--- subroutine or function passed as argument
                  ARG(N:N)='P'
               ELSE
                  DO 40 I=1,6
                     K=NT/2
                     IF(NT-2*K.GT.0) GOTO 50
                     NT=K
   40             CONTINUE
   50             CONTINUE
                  ARG(N:N)=ATYP(I:I)
               ENDIF
            ELSE
               GOTO 60
            ENDIF
         ELSE
            GOTO 60
         ENDIF
      ENDIF
      GOTO 70
   60 CONTINUE
      ARG(N:N)=ATYP(7:7)
   70 CONTINUE
      IF(KPOS.LT.I2) GOTO 10
  999 END
