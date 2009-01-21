      COMMON/STATE/NLINES,NKEEPL,NSTAMM,NFSTAT,ISNAME,NSNAME,IRNAME,
     1   NRNAME,IGNAME,NGNAME,INDCNT,INDFAC,KNTDO,KNTIF,IBLPAD,NRORST,
     2   NSTANU,ICBPRT,NCBNAM,NEQNAM,NCBVAR,
     +   NCBGRP(MAXGRP),KCBGRP(MAXGRP),LCBNAM(MAXGRP),LCBVAR(MXNAME),
     +   NEQGRP(MAXGRP),KEQGRP(MAXGRP),
     +   LRORST(MXORST),NAMTYP(MXNAME),NSSTRT(700),NSEND(700),
     3   KSTANU(MAXNUM),KSTARE(MAXNUM),NLTYPE(MXSIMA),ICLASS(MXSIMA,2),
     4   IMODIF(MXSIMA),NFLINE(MXSIMA),NLLINE(MXSIMA),JPLINK(MXSIMA)
*-----------------------------------------------------------------------
*      /STATE/    contains the information concerning the actual
*                 status of the program
*      NLINES     no. of lines in line image buffer SIMA
*      NKEEPL     buffered line number in READEC, or 0
*      NSTAMM     total no. of statements in current routine
*      NFSTAT     no. of FORTRAN statements in current routine
*      ISNAME     pointer to start-1 of stmt. names in SNAMES
*      NSNAME     no. of names found in statement
*      IRNAME     pointer to start-1 of names/routine in SNAMES
*      NRNAME     no. of names/routine
*      IGNAME     pointer to start-1 of global names in SNAMES
*      NGNAME     no. of global names
*      INDCNT     current indentation level (reset at routine start)
*      INDFAC     no. of ch./level to indent
*      KNTDO      current DO loop level (for indentation)
*      KNTIF      current IF...THEN level (for indentation)
*      IBLPAD     in QUOTES option, string blank-padded to multiples
*                 of IBLPAD (default = 1)
*      NRORST     no. of currently selected OR-sets in LRORST
*      NSTANU     no. of statement numbers in KSTANU, KSTARE
*      ICBPRT     no. of c.b. variables printed at ACTION(24)
*      NCBNAM     no. of c.b. names in NCBGRP, KCBGRP, SCBNAM
*      NEQNAM     no. of equiv. groups in NEQGRP, KEQGRP
*      NCBVAR     no. of names in SEQNAM
*      NCBGRP     no. of common block variables per c.b.
*      KCBGRP     pos.-1 of start of c.b. name list in  SCBNAM
*      LCBNAM     # of c.b. variables used in current routine
*      LCBVAR     counts number of times a variable is referenced
*      NEQGRP     no. of names in equiv. group
*      KEQGRP     pos.-1 of start of equiv. group in SCBNAM
*      LRORST     list of OR-sets valid for current routine
*      NAMTYP     variable type, parallel to SNAMES
*      NSSTRT     start of name I in SSTA
*      NSEND      end of name I in SSTA
*      KSTANU     statement numbers in routine (sorted)
*      KSTARE     new statement numbers, corresponding to KSTANU
*      NLTYPE     type of line I (0 comment, 1 start, 2 cont. of stmt. )
*      ICLASS(I,1)  type of statement I
*                   0 = comment
*                   999 = no comment, not classified
*                   class = ICURCL(1), common /CURSTA/
*      ICLASS(I,2)  type of second part of statement I if logical IF
*      IMODIF     10*n2 + n1
*                 n1 = 1 : statement has been filtered
*                 n2 = 1 : statement has been modified
*      NFLINE     start of statement I in SIMA
*      NLLINE     end of statement I in SIMA
*      JPLINK     position of start of inline comment (if present, else 0)
*-----------------------------------------------------------------------
