      PARAMETER (LIF=109)
      COMMON /USINFN/ INFUNG(LIF),CINFUN(LIF),CTYFUN(LIF)
      CHARACTER*6 CINFUN
      CHARACTER*1 CTYFUN
*-----------------------------------------------------------------------
*   LIF      = number of intrinsic functions recognised
*   CINFUN   = name of intrinsic function
*   INFUNG   = "1" if generic, "0" if not
*   CTYFUN   = set only for generic types ... gives type of function
*            = 'I' integer
*            = 'R' real
*            = 'D' double precision
*            = 'K' complex
*            = 'L' logical
*            = 'C' character
*            = '$' takes type of argument(s)
*-----------------------------------------------------------------------
