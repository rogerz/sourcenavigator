      PARAMETER (MCOMN=1000,MCOMT=50)
      COMMON /USCOMN/ NCOMN,NCOMT, SCNAME(MCOMN),SCTITL(MCOMT), ICNAME
     +(MCOMN),ICTITL(MCOMT)
      CHARACTER SCNAME*(MXNMCH),SCTITL*(MXNMCH)
*-----------------------------------------------------------------------
*   NCOMN   = number of variables in all COMMON blocks this routine
*   NCOMT   = number of COMMON block titles this routine
*   SCNAME  = name of variable I
*   SCTITL  = name of COMMON block J
*   ICNAME  = pointer to J for name I
*   ICTITL  = -(pointer to start of names in SCNAME for COMMON block J)
*-----------------------------------------------------------------------
