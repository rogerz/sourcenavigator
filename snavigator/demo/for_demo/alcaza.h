      COMMON/ALCAZA/SCROUT,SSTM,SNAM,SSTA,SSTR,SNEWST(100),SIMA(MXSIMD),
     1      SNAMES(MXNAME),SCBVAR(MXNAME),SCBNAM(MAXGRP)
      CHARACTER SCROUT*(MXNMCH),SSTM*(MXSSTM),SNAM*(MXSSTM),
     1  SSTA*(MDIMST),SSTR*(MDIMST),SNEWST*(MXLINE),SIMA*(MXLINE),
     2  SNAMES*(MXNMCH),SCBVAR*(MXNMCH),SCBNAM*(MXNMCH)
*-----------------------------------------------------------------------
*--- SCROUT = name of current routine being processed
*--- SSTM   = string containing all statement descriptions
*--- SNAM   = string containing all statement descriptors
*--- SSTA   = string containing the actual statement, col. 7-72 (all)
*--- SSTR   = temporary statement buffer during replacement
*--- SNEWST = temporary statement image buffer during reformatting
*--- SIMA   = string containing one complete routine
*--- SNAMES = name list for global, routine, and statement names
*--- SCBVAR = list of c.b. variables in one routine (ACTION(24))
*--- SCBNAM = list of c.b. names in one routine
*-----------------------------------------------------------------------
