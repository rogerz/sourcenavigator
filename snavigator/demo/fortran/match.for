      SUBROUTINE MATCH(SEARCH,IMC1,IMC2,STATEM,ICC1,ICC2, HOLFLG,KPOSM,
     +ILEVEL,NSPEC,KSPEC1,KSPEC2)
*-----------------------------------------------------------------------
*
* matches two strings
*
* blanks outside strings are ignored in STATEM
*
* input
* SEARCH      string (possibly with special symbols) to be matched
*             special symbols and their meanings are :
*             @  ( commercial at  )     numeric string
*             &  ( ampersand )          alphabetic string
*             $  ( dollar )             alphanumeric string
*             #  ( hash )               any string ( including null )
*             ?  ( questionmark )       FORTRAN name type ( length not
*                                                          limited)
*             !  ( exclam. mark )       expression (no [,] at level 0)
*             >)                        string up to open bracket level
*             ;  (semicolon)            nothing must follow, i.e.
*                                       ICC2 must be last matched ch.
* IMC1        first ch. in SEARCH
* IMC2        last ch. in SEARCH
* STATEM      input string (typically a statement)
* ICC1        first ch. in STATEM
* ICC2        last ch. in STATEM
* HOLFLG      if TRUE, hollerith included in STATEM
* output
* KPOSM       position of last ch. in STATEM for first fit of SEARCH.
*             if last ch. in SEARCH is a special ch.( as above),
*             the match will be performed to the ENDC of that type.
*             KPOSM = 0 in case of no match
* ILEVEL      round bracket level relative to input level 0, at KPOS
* NSPEC       no. of special ch. encountered in SEARCH
* KSPEC1(i)   start of i-th special ch. corresp. string in STATEM
* KSPEC2(i)   end   -  -     -      -     -        -    -   -
*             attention: KSPEC2(i) < KSPEC1(i) for null string match
*
*-----------------------------------------------------------------------
      DIMENSION KSPEC1(*),KSPEC2(*)
      LOGICAL HOLFLG,FREE,EVER,POSIT
      CHARACTER SEARCH*(*),STATEM*(*),STEMP*1,STEMP1*1
      include 'convex.h'
      KPOSM=0
      ILEVEL=0
      NSPEC=0
*--- INSTR = string indicator for SEARCH, ISSTR for STATEM
      INSTR=0
      ISSTR=0
      FREE=.FALSE.
      EVER=.FALSE.
*--- KSTR is the current ch. pos. in STATEM
      KSTR=ICC1
      KEEP=ICC2
      JC=IMC1-1
*--- loop over characters in 'SEARCH' string
*--- exits are:
*                 10   continue looping
*                 30   match exit
*                 40   nomatch exit
   10 JC=JC+1
      IF (JC.GT.IMC2) GOTO 30
      STEMP=SEARCH(JC:JC)
      IF(STEMP.EQ.'''')  INSTR=1-INSTR
      IF(INSTR.EQ.0)  THEN
*--- not inside quotes
         IF (STEMP.EQ.';')  THEN
*--- matches if nothing follows in STATEM
            IF (KSTR.GT.ICC2)  THEN
               GOTO 30
            ENDIF
            GOTO 40
         ENDIF
         IF (KSTR.GT.ICC2)  THEN
            IF (STEMP.EQ.'#'.AND.JC.EQ.IMC2)  THEN
*--- '#' at end of SEARCH string
               NSPEC=NSPEC+1
               KSPEC1(NSPEC)=KSTR
               FREE=.TRUE.
               GOTO 30
            ENDIF
            GOTO 40
         ENDIF
*
*--- for '#' and '>)', move the pointer forward
*
         IF (STEMP.EQ.'#')  THEN
*--- any string, including null
            JCFREE=JC
            FREE=.TRUE.
            EVER=.TRUE.
            NSPEC=NSPEC+1
            NSPECK=NSPEC
            KSPEC1(NSPEC)=KSTR
            GOTO 10
         ELSEIF (STEMP.EQ.'>')  THEN
*---  look for ')' (level jump)
            IF (JC.EQ.IMC2) GOTO 40
            IF (SEARCH(JC+1:JC+1).NE.')')GOTO 40
*--- ')' is next character - perform level jump
            CALL SKIPLV(STATEM,KSTR,ICC2,HOLFLG,KPOS,ILEV)
            IF (KPOS.EQ.0)  THEN
               IF (EVER)  THEN
                  JC=JCFREE
                  FREE=.TRUE.
                  KSTR=KEEP+1
                  GOTO 10
               ENDIF
               GOTO 40
            ENDIF
            NSPEC=NSPEC+1
            KSPEC1(NSPEC)=KSTR
            KSTR=KPOS
            KSPEC2(NSPEC)=KPOS-1
            GOTO 10
         ENDIF
*
*--- set ITYPE to indicate normal ch. (0) or special
*
         ITYPE=INDEX(SPCHAR,STEMP)
      ELSE
*--- inside quotes in SEARCH - treat as normal
         ITYPE=0
      ENDIF
      POSIT=.FALSE.
      IF(FREE)  THEN
*--- look for STEMP further upstream
         FREE=.FALSE.
         POSIT=.TRUE.
         IF (ITYPE.EQ.0)  THEN
*--- normal character
            CALL POSCH(STEMP,STATEM,KSTR,ICC2,HOLFLG,9999,KPOS,ILEV)
         ELSE
*--- special character
            CALL CHRTYP(ITYPE,STATEM,KSTR,ICC2,HOLFLG,KPOS,ILEV)
         ENDIF
*--- no match if not found
         IF (KPOS.EQ.0) GOTO 40
         KEEP=KPOS
         KSTR=KPOS
         ILEVEL=ILEVEL+ILEV
         KSPEC2(NSPEC)=KPOS-1
*--- following ENDIF for IF FREE
      ENDIF
*
*--- now STEMP must match, or be special
*
      IF(ITYPE.EQ.0)  THEN
*--- normal
   20    CONTINUE
         IF (KSTR.GT.ICC2) GOTO 40
         STEMP1=STATEM(KSTR:KSTR)
*--- skip blanks  outside strings
         IF (STEMP1.EQ.' '.AND.ISSTR.EQ.0)  THEN
            KSTR=KSTR+1
            GOTO 20
         ELSEIF (STEMP1.EQ.'{')  THEN
*--- start of character string
            IF (HOLFLG)  THEN
*--- strings are included in match
               KSTR=KSTR+1
               ISSTR=1
            ELSE
*--- skip over string
               I=INDEX(STATEM(KSTR:ICC2),'}')
               IF (I.EQ.0) GOTO 40
               KSTR=I+KSTR
            ENDIF
            GOTO 20
         ELSEIF (STEMP1.EQ.'}')  THEN
*--- skip
            KSTR=KSTR+1
            ISSTR=0
            GOTO 20
         ENDIF
*--- now match STEMP and STEMP1
         IF (STEMP.EQ.STEMP1)  THEN
            KSTR=KSTR+1
            IF (.NOT.POSIT)  THEN
               IF (STEMP.EQ.'(')  THEN
                  ILEVEL=ILEVEL+1
               ELSEIF (STEMP.EQ.')')  THEN
                  ILEVEL=ILEVEL-1
               ENDIF
            ENDIF
            GOTO 10
         ELSE
*--- try further upstream if possible
            IF (EVER)  THEN
               JC=JCFREE
               FREE=.TRUE.
               KSTR=KEEP+1
               NSPEC=NSPECK
               GOTO 10
            ENDIF
            GOTO 40
         ENDIF
      ELSE
*--- string of type ITYPE
         CALL SKIPTP(ITYPE,STATEM,KSTR,ICC2,.FALSE.,KPOS,ILEV)
         IF (KPOS.EQ.0)  THEN
            IF (EVER)  THEN
               JC=JCFREE
               KSTR=KEEP+1
               NSPEC=NSPECK
               FREE=.TRUE.
               GOTO 10
            ENDIF
            GOTO 40
         ELSE
*--- KPOS ne 0, i.e. found
            NSPEC=NSPEC+1
            KSPEC1(NSPEC)=KSTR
            KSPEC2(NSPEC)=KPOS
            KSTR=KPOS+1
            ILEVEL=ILEVEL+ILEV
            GOTO 10
         ENDIF
      ENDIF
   30 CONTINUE
*--- when arriving here, strings do match
      IF (FREE)  THEN
         KPOSM=ICC2
         KSPEC2(NSPEC)=ICC2
      ELSE
         KPOSM=KSTR-1
      ENDIF
   40 CONTINUE
      END
