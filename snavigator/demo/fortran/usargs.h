      PARAMETER (MARGS=50,MARGD=10,MKALL=50,LARC=50)
      COMMON /USARGS/ NARGS,CARGNM(MARGS),CARGTY(MARGS), NARGDI(MARGS),
     +CARGDI(MARGD,2,MARGS)
      COMMON /USCOMM/ CMMNT
      COMMON /USCALL/ NKALL,CKALLN(MKALL),KALLIF(MKALL),KALLDO(MKALL)
      CHARACTER*(MXNMCH) CARGNM,CKALLN
      CHARACTER*(LARC) CARGTY,CARGDI,CMMNT
*-----------------------------------------------------------------------
*   NARGS   = number of arguments passed to current module
*   CARGNMi = name of argument i
*   CARGTYi = type of argument i (EG CHAR80, INTE2)
*   NARGDIi = number of dimensions of argument i
*   CARGDIji= 1) lower bound for jth. dimension of argument i
*             2) upper bound for jth. dimension of argument i
*   NKALL   = number of CALL statements in module
*   CKALLNi = name of subroutine ith. CALLed
*   KALLIFi = IF level of ith. subroutine CALLed
*   KALLDOi = DO level of ith. subroutine CALLed
*-----------------------------------------------------------------------
