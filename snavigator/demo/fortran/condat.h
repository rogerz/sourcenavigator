      DATA SBASE(1:24)  /' :?!#&$@;><=()+-*/[],.''"'/
      DATA SBASE(25:27) /'{}\\'/
      DATA SBASE(28:53) /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA SBASE(54:79) /'abcdefghijklmnopqrstuvwxyz'/
      DATA SBASE(80:92) /'_0123456789%^'/
      DATA SPCHAR       /'@&$#?!>'/
      DATA SPILL        /'{}'/
*-----------------------------------------------------------------------
*--- statement function SPECCH = true for special character
      SPECCH(SDUMMY)=INDEX(SBASE(2:24),SDUMMY).NE.0
     1               .OR.INDEX(SBASE(91:),SDUMMY).NE.0
*--- statement function NUMCH = true for numeric character
      NUMCH(SDUMMY)=INDEX(SBASE(81:90),SDUMMY).NE.0
*--- statement function ALPHCH = true for alphabetic character
      ALPHCH(SDUMMY)=INDEX(SBASE(28:80),SDUMMY).NE.0
*--- statement function ANUMCH = true for alphanumeric character
      ANUMCH(SDUMMY)=INDEX(SBASE(28:90),SDUMMY).NE.0
*--- statement function STRGCH = true for string character
      STRGCH(SDUMMY)=INDEX(SBASE(3:10),SDUMMY).NE.0
*--- statement function for integer value (place) of character
      ICVAL(SDUMMY)=INDEX(SBASE(28:90),SDUMMY)
*-----------------------------------------------------------------------
