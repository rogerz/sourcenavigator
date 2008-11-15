      PARAMETER (MGCON=2000,MGCOT=200)
      COMMON /USGCOM/ NGCON,NGCOT, SGCNAM(MGCON),SGCTIT(MGCOT), IGCNAM
     +(MGCON),IGCTIT(MGCOT)
      CHARACTER SGCNAM*(MXNMCH),SGCTIT*(MXNMCH)
*-----------------------------------------------------------------------
*   NGCON   = number of variables in all COMMON blocks all ROUTINES
*   NGCOT   = number of COMMON block titles all ROUTINES
*   SGCNAM  = name of variable I
*   SGCTIT  = name of COMMON block J
*   IGCNAM  = pointer to J for name I
*   IGCTIT  = -(pointer to start of names in SGCNAM for COMMON block J)
*-----------------------------------------------------------------------
