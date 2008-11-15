      COMMON/CURSTA/NCHST,NSTREF,NLIMA,IFILTR,NLREF(20),ICURCL(2),
     +              NEWOUT,NDUMMY
*-----------------------------------------------------------------------
*      /CURSTA/    describes the "current" statement
*                  (after calls to EXTRAC  and CLASSF)
*      NCHST      no. of ch. in statement
*      NSTREF     no. of corresponding statement in SIMA
*      NLIMA      no. of corresponding image lines of current stmt.
*      IFILTR     flag: = -1 reset for routine, 0 reset for statement,
*                          1 do not reset
*      NLREF      ref. to n-th corresponding line in SIMA
*      ICURCL(1)  class of first part
*      ICURCL(2)  class of second part ( if ICURCL(1)=IIF), else ILL
*      NEWOUT     occupation of SNEWST in lines
*      NDUMMY     true dummy argument (to avoid integer overflows)
*-----------------------------------------------------------------------
