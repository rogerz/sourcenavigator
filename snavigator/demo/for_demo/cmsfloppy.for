      Program Floppy
      include (param)
      include (ushtml)
      character*(132) ctemp
      character*(80) errtext
      logical fexist
c
c Kludge for VM/CMS to determine whether we have an anchor dictionary
c for HTML.
c
      character*1 a_list
c
      call dmscsl('DMSCGR  ',iret,'ANCHOR_LIST',11,a_list,1,lout,0)
      errtext = 'Floppy --> Error getting REXX variable anchor_list'
      if(iret.ne.0) then
         write(6,*) ' IRET ',iret
         goto 999
      endif
      read(a_list,'(i1)') anchor_list
      nanchr = 0
      if(anchor_list.eq.2) then
c
c reading an old one
c
         errtext = 'Floppy --> error opening Anchor Dictionary'
         open(mdunit,status='old',err=999)
         nanchr = 0
         errtext = 'Floppy --> error reading Anchor Dictionary'
   22    read(mdunit,'(a)',end=23,err=999) ctemp
         ibl = index(ctemp,' ')
         if(ibl.eq.0) goto 22
         if(nanchr.ge.mxanch) then
            errtext = 'Floppy --> Too many anchors given'
            goto 999
         endif
         nanchr = nanchr+1
         canchr(nanchr) = ctemp(:ibl-1)
         cancht(nanchr) = ctemp(ibl+1:lenocc(ctemp))
         goto 22
   23    close(mdunit)
         open(mdunit,status='old',access='append',err=999)
      else if(anchor_list.eq.1) then
         open(mdunit,status='new',err=999)
         anchor_list = 2
      endif
c
c Do the processing
c
      Call Allpro
c
c
      errtext = ' '
  999 if(errtext.ne.' ') write(6,*) errtext(:lenocc(errtext))
      End
