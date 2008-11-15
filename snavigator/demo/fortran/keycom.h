      COMMON/KEYINP/NORSET,NGLSET,NKYINT,NKYNAM,NKYSTR,LKYSTR,NKYCHR,
     1  NORCOM(MXORST),KORCOM(MXORST),KEYREF(MXKEYS,7),KEYINT(MXKINT),
     2  KNAMRF(MXKNAM,2),KSTREF(MXKNAM,2),KKYSTA(MXKNAM),KKYEND(MXKNAM)
      COMMON/SKEYNP/SKYSTR,SKEYLS(MXKNAM)
      CHARACTER SKYSTR*(MDIMST),SKEYLS*(MXNMCH)
*-----------------------------------------------------------------------
*    NORSET = no. of OR-sets
*    NGLSET = no. of global commands
*    NKYNAM = no. of names in SKEYLS
*    NKYSTR = no. of strings in SKYSTR
*    LKYSTR = occupation of  SKYSTR
*    NKYCHR = no. of string refs in KSTREF
*    NORCOM = no. of commands / OR-set
*    KORCOM = start-1 of each OR-set in KEYREF
*    KEYREF
*            (I,1) = ref. number (=pos.) of key
*            (I,2) = no. of integers in KEYINT
*            (I,3) = start-1 of integers in KEYINT
*            (I,4) = no. of names in SKEYLS
*            (I,5) = start-1 of names in SKEYLS
*            (I,6) = no. of string refs in KSTREF
*            (I,7) = start-1 of string refs in KSTREF
*    KEYINT = integer list for sub-keys etc.
*    KNAMRF
*            (I,1) = ref. to string following name, or zero if none,
*                   or < 0 if to be ignored (illegal)
*            (I,2) = ref. to replacement string, or zero
*    KSTREF
*            (I,1) = ref. to string (stand alone), or < 0 if illegal
*            (I,2) = ref. to replacement string for above, or zero
*    KKYSTA = start of string in SKYSTR
*    KKYEND = end of string in SKYSTR
*
*    SKEYLS = name list for input commands
*    SKYSTR = contains stand-alone or name-associated strings
*-----------------------------------------------------------------------
