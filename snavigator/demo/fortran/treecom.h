      COMMON/STREE/CALLER(KENT),CALLED(KALL) ,CERARG(KENT),
     +CEDARG(KALL),KODE(KENT),SARGEL(NOARG),SEXEL(KALL)
      CHARACTER CALLER*(MXNMCH),CALLED* (MXNMCH),CERARG*(NOARG),
     +CEDARG*(NOARG),KODE*1,SARGEL*(MXNMCH),SEXEL*(MXNMCH)
      COMMON/TREE/NCALLR,NCALLD,NARGEL,NEXEL,ICALLR(KENT)
*-----------------------------------------------------------------------
*
*   CALLER             calling routiine, or entry in it
*   CALLED             called routine or function
*   CERARG             argument types of caller
*   CEDARG             argument types of called
*   KODE               type of caller or entry (S/R = blank)
*   NCALLR             # of callers in this routine
*   NCALLD             # of called in this routine
*   ICALLR             statement number of CALL
*-----------------------------------------------------------------------
