      COMMON /USSTMT/ ISGLOB,ICLOLD,NFIOLD,NFAULT,RPROCS
      LOGICAL RPROCS
*-----------------------------------------------------------------------
*   ISGLOB  = running count of statements in source deck
*   ICLOLD  = the class (ICLASS) of the last EXECUTABLE statement
*   NFIOLD  = the line number of the last statement in the module
*   NFAULT  = the number of WARNINGS in the module so far
*   RPROCS  = set .TRUE. if module is to be processed
*-----------------------------------------------------------------------
