      SUBROUTINE NAMTAB(SNAME,SLIST,NLIST,IPOS)
*-----------------------------------------------------------------------
*
*   enters a name in an alphabetic table, or gives position if already i
*
*   input
*   SNAME                   name to be entered
*   SLIST                   name list
*   NUMTAB                  reference list to be updated (integers)
*   NLIST                   no. of names in SLIST
*   Output
*   IPOS                    <0: -pos of name already in table
*                           =0: NLIST <0
*                           >0: pos of newly entered name in table
*
*+++++++++++ IMPORTANT
*   In case the name has been entered, the user must increase the list
*   length himself.
*-----------------------------------------------------------------------
      CHARACTER *(*) SNAME,SLIST(*)
      IF(NLIST.LT.0)  THEN
         IPOS=0
      ELSEIF(NLIST.EQ.0)  THEN
         IPOS=1
         SLIST(1)=SNAME
      ELSE
         CALL NAMSRC(SNAME,SLIST,NLIST,KPOS,LAST)
         IF (KPOS.EQ.0)  THEN
*--- name not yet in table
            IPOS=LAST+1
            DO 10 I=NLIST,IPOS,-1
               SLIST(I+1)=SLIST(I)
   10       CONTINUE
            SLIST(IPOS)=SNAME
         ELSE
            IPOS=-KPOS
         ENDIF
      ENDIF
      END
