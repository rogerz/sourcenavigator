      integer function lenocc(c)
      character*(*) c
      lenocc = len(c)
      do 1 i=len(c),1,-1
         if(c(i:i).ne.' ') then
           lenocc = i
           return
         endif
   1  continue
      lenocc = 0
      end
