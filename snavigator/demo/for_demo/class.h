      COMMON /CLASS/NCLASS,NPRIOR,NHEADR,IIF,IEND,IFORMT,IRETUR,ILL,
     + ISTMDS(MCLASS,MXSTAT),IALPHA(2,53),IPRIOR(MXSTAT),IHEADR(MXSTAT)
*-----------------------------------------------------------------------
*--- contains statement classification information
*       NCLASS      no. of (internal) classes
*       NPRIOR      no. of priority statements
*       NHEADR      no. of header statements
*       IIF         internal number of the logical IF
*       IEND        internal number of END statement
*       IFORMT      internal number of the FORMAT statement
*       ILL            -       -      -     illegal     -
*      ISTMDS(MCLASS,MXSTAT)  control words being
*                    1 = first ch. in SNAM
*                    2 = last  ch. in SNAM
*                    3 = first ch. in SSTM
*                    4 = last ch. in SSTM
*                    5 = last significant ch. in SSTM
*                    6 = external reference number ( class )
*                    7 = priority ( if 0, any order)
*                    8 = char. in descr. after which to start name
*                        scan.  if 99, start after match string.
*                    9 = stop name scan at stmt. end
*                   10 = statement number classifier, being
*                        0 if statement cannot contain stmt. numbers
*                        1 if (one) stmt. number must follow key immed.
*                        2 if all stmt. no.s are inside first bracket
*                        3 if all stmt. no.s follow immed. first bracket
*                        4 if (one) after FMT=, or second in first br.
*                   11 = exec flag ( 0 = non-executable)
*                   12 = names flag ( 0 = no names, 1 = one, 2 = any)
*                   13 = special treatement flag ( if 1) +2 * smflag
*                       where smflag = 1 allows for simple keyword match
*                   14 = routine header flag (0 no, 1 yes)
*                   15 = type flag:
*                        0 if types valid for all names
*                        1 if separate types for first name + rest
*                        2 if special treatment (IMPLICIT)
*                        to this, 10 is added if only names outside
*                        brackets to be taken, +10 for COMMON
*                   16 = n1 = no. of types for first or all
*                   17 to 16 + n1 = types
*                   17 + n1  = n2
*                   18 + n1 to 21 = types for rest (0 filled)
*       IALPHA(2,53)   for letters 1 to 26 (A to Z,a to z),
*                   first and last class under that letter.
*                   ( keys are in alphabetic order)
*                   53 for those not starting with any key.
*                   if not specified otherwise, those will be
*                   processed last
*       IPRIOR(MXSTAT)       refs of priority statements
*       IHEADR(MXSTAT)       refs of header statements
*-----------------------------------------------------------------------
