      PARAMETER (LOPS=23)
      INTEGER ILEFP(LOPS),IRITP(LOPS),ILENO(LOPS)
      CHARACTER*(LOPER) COPER(LOPS)
      DATA COPER /'**    ','*     ','/     ','+     ','-     ','//    ',
     &            '.LT.  ','.GT.  ','.LE.  ','.GE.  ','.EQ.  ','.NE.  ',
     &            '.NOT. ','.AND. ','.OR.  ','.EQV. ','.NEQV.',':     ',
     &            ',     ','=     ','(     ',')     ','END   '/
      DATA ILENO /2,1,1,1,1,2,4,4,4,4,4,4,5,5,4,5,6,1,1,1,1,1,3/
C left precedence of operators
      DATA ILEFP /17      ,16      ,16      ,15      ,15      ,14      ,
     &            13      ,13      ,13      ,13      ,13      ,13      ,
     &            12      ,11      ,10      ,9       ,9       ,7       ,
     &            6       ,3       ,4       ,-1      ,2       /
C right precedence of operators
      DATA IRITP /18      ,16      ,16      ,15      ,15      ,14      ,
     &            13      ,13      ,13      ,13      ,13      ,13      ,
     &            12      ,11      ,10      ,9       ,9       ,7       ,
     &            6       ,3       ,20      ,4       ,2       /
