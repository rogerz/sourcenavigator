      character*(*) function touppr(char)
*-----------------------------------------------------------------------
*
*--- Converts a string to upper case
*
*--- Input
*    char - string to be converted
*--- Output
*    touppr - uppercase char as value of function
*-----------------------------------------------------------------------
      character*(*) char
      character*26 cup /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      character*26 clo /'abcdefghijklmnopqrstuvwxyz'/
      save cup,clo
      touppr = char
      do 1 i=len(char),1,-1
         ipos = index(clo,char(i:i))
         if(ipos.ne.0) touppr(i:i) = cup(ipos:ipos)
    1 continue
      end
