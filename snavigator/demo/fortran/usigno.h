      PARAMETER (MXIGNV=50,MXIGNS=50,MUUNIT=15)
      COMMON /USIGNO/ GALEPH,ADAMO,USAGE,UNFLP,atlas,
     &                NIGNOR,NIGNOS,
     &                CIGNOR(MXIGNV),LIGNOR(MXIGNS),
     &                CIGNOS(MXIGNS),LIGNOS(MXIGNS)
      CHARACTER*(MXNMCH) CIGNOR,CIGNOS
      LOGICAL GALEPH,ADAMO,USAGE,UNFLP,atlas
*-----------------------------------------------------------------------
*   MXIGNV  = Maximum number of variable names to ignore
*   MUUNIT  = LUN of USER list of variable names to ignore
*   NIGNOR  = Number of variable names found
*   NIGNOS  = Number of subroutine names found
*   CIGNOR  = Array of ignorable variable names
*   CIGNOS  = Array of ignorable subroutine names
*   LIGNOR  = Length of variable name
*   LIGNOS  = Length of subroutine name
*   GALEPH  = set .TRUE. if special GALEPH processing
*   ADAMO   = set .TRUE. if special ADAMO processing
*   USAGE   = set .TRUE. if check of COMMON variable usage
*   UNFLP   = set .TRUE. if NO coding convention checks !
*   ATLAS   = set .TRUE. if special ATLAS processing
*-----------------------------------------------------------------------
