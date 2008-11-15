      character*(*) function anchor(cname)
      include 'param.h'
      include 'ushtml.h'
      character*(*) cname
      character*(mxnmch) touppr,ctemp
      external touppr
c 
      anchor = '<A HREF="dummy.html">'//cname(:lenocc(cname))//'</A>'
      if (anchor_list.eq.0) return
      anchor = ' '
c
      ctemp = touppr(cname)
c
c search for this anchor in the list
c
      do 2 ia=1,nanchr
         if (canchr(ia).eq.ctemp) then
            anchor = cancht(ia)
            return
         endif
    2 continue
c
c no ... write it out
c
      write(mdunit,'(a)') ctemp(:lenocc(ctemp))
      if(nanchr.ge.mxanch) return
      nanchr = nanchr+1
      canchr(nanchr) = ctemp
      cancht(nanchr) = ' '
      end         
     
