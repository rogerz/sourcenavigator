      SUBROUTINE GETINT(STRING,ICC1,ICC2,KFCH,KLCH,NN)
*-----------------------------------------------------------------------
*
* routine to extract one positive integer from a BCD string.
* input
* STRING   input string
* ICC1     starting pos. for scan
* ICC2     end      -    -    -
* output
* KFCH     pos. of first character of integer in STRING,
*          or 0 if no integer found.
* KLCH     pos. of last ch. in STRING.
* NN       integer in integer format. (set to zero when none found)
*
*-----------------------------------------------------------------------
      CHARACTER*(*) STRING
      NN=0
      CALL CHRTYP(1,STRING,ICC1,ICC2,.FALSE.,KFCH,ILEV)
      IF(KFCH.NE.0)  THEN
         CALL SKIPTP(1,STRING,KFCH,ICC2,.FALSE.,KLCH,ILEV)
         NN=NEXTIN(STRING,KFCH,KLCH)
      ENDIF
      END
