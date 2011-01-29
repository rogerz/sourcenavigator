      SUBROUTINE DEFINF
*-----------------------------------------------------------------------
* Define the table of FORTRAN intrinsic functions, and label the
* generic ones.
*-----------------------------------------------------------------------
      include 'usinfn.h'
      PARAMETER (NGEN=43)
      CHARACTER*6 CINF(LIF)
      CHARACTER*1 CGEN(NGEN)
      INTEGER IGEN(NGEN)
      DATA CINF/'INT   ','IFIX  ','IDINT ','IQINT ','REAL  ','FLOAT ',
     +'SNGL  ','DBLE  ','CMPLX ','ICHAR ','CHAR  ','AINT  ','DINT  ',
     +'ANINT ','DNINT ','NINT  ','IDNINT','ABS   ','IABS  ','DABS  ',
     +'CABS  ','MOD   ','AMOD  ','DMOD  ','SIGN  ','ISIGN ','DSIGN ',
     +'DIM   ','DDIM  ','DPROD ','MAX   ','MAX0  ','AMAX1 ','DMAX1 ',
     +'AMAX0 ','MAX1  ','MIN   ','MIN0  ','AMIN1 ','DMIN1 ','AMIN0 ',
     +'MIN1  ','LEN   ','INDEX ','IMAG  ','AIMAG ','CONJG ','SQRT  ',
     +'DSQRT ','CSQRT ','EXP   ','DEXP  ','CEXP  ','LOG   ','ALOG  ',
     +'DLOG  ','CLOG  ','LOG10 ','ALOG10','DLOG10','SIN   ','DSIN  ',
     +'CSIN  ','COS   ','DCOS  ','CCOS  ','TAN   ','DTAN  ','ASIN  ',
     +'DASIN ','ACOS  ','DACOS ','ATAN  ','DATAN ','ATAN2 ','DATAN2',
     +'SINH  ','DSINH ','COSH  ','DCOSH ','TANH  ','DTANH ','LGE   ',
     +'LGT   ','LLE   ','LLT   ','QEXT  ','DCMPLX','QCMPLX','CBRT  ',
     +'EXP2  ','EXP10 ','LOG2  ','COTAN ','ERF   ','ERFC  ','GAMMA ',
     +'LGAMMA','IRE   ','AMT   ','NOT   ','IAND  ','IOR   ','IEOR  ',
     +'ISHFT ','IBSET ','IBCLR ','BTEST ','REAL  '/
      DATA IGEN /1,5,8,9,12,14,16,18,22,25,28,31,37,45,47,48,51,54,58,
     &           61,64,67,69,71,73,75,77,79,81,87,88,89,90,91,92,93,
     &           94,95,96,97,98,99,100/
      DATA CGEN /'I','R','D','K','R','R','I',6*'$','R','K',14*'$','D',
     &           'D','K',9*'$','I','$'/
      DO 10 INF=1,LIF
        CINFUN(INF) = CINF(INF)
        INFUNG(INF) = 0
   10 CONTINUE
      DO 15 IG=1,NGEN
        INFUNG(IGEN(IG)) = 1
        CTYFUN(IGEN(IG)) = CGEN(IG)
   15 CONTINUE
      RETURN
      END
