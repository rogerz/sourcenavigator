      SUBROUTINE TOHTML(NUMBER,ICL1)
*-----------------------------------------------------------------------
*
*   Generates a new statement with statement names replaced
*   by HTML anchor points, if defined in the dictionary on unit MDUNIT.
*   Local variables are anchored automatically be Floppy
*
*---Input
*     SSTA, NCHST, JPLINK, NUMBER, ICL1
*--- Output
*     CLOUT (new line) for each statement line in SIMA
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'cursta.h'
      include 'state.h'
      include 'jobsum.h'
      include 'class.h'
      include 'usltyd.h'
      include 'usinfn.h'
      include 'ushtml.h'
c
      common /thtml/ ncll,callis
      character*(mxnmch) callis(300)
      integer ncll(300)
c
      logical declare,external,fixup
c
      integer issim(700),ifsim(700),insim(700)
      integer ncallis,curcal
c
      character*1500 clout,anchor,cname,ctemp1,ctemp2
      character*80 ctemp0,cseq
      character*(mxnmch) tname,curmod,touppr
      character*1 ssta1
c
      external anchor,touppr
      save curmod,ncallis
c
      ifi = nfline(number)
      ila = nlline(number)
c
c If statement is a comment, or has no names, write it out neat
c
      if(icl1.eq.0.or.nsname.eq.0) then
         write(mhunit,'(a)') (sima(i)(:lenocc(sima(i))),i=ifi,ila)
         if (icl1.eq.iend.or.icl1.eq.iend+71) 
     &             write(mhunit,'(a)') '</PRE>'
         goto 999
      endif
c
      declare = ldeclr(icl1)
c
c Prepare indexing for each name in SSTA to SIMA
c
      jname = 1
      do 6 in=1,nsname
         issim(in) = 0
         ifsim(in) = 0
         insim(in) = 0
    6 continue
      nc = 0
      fixup = .false.
      do 1 i=ifi,ila
         clout = sima(i)
         iendl = lenocc(clout)
         if (jplink(i).ne.0) iendl = jplink(i)-1
         jfirst = jname
         noff = 0
         if(fixup) nc = nc + 1
         fixup = .false.
         ipos = 6
    2    ipos = ipos+1
         if(ipos.le.iendl) then
            ssta1 = ssta(nc+1:nc+1)
            if(ssta1.eq.'{'.or.ssta1.eq.'}') then
               nc = nc+1
            else if(ssta1.eq.' '.and.ssta(nc+2:nc+2).eq.
     &              clout(ipos:ipos)) then
               nc = nc+1
c
c set fixup for strange feature of MARKST which adds extra blanks
c 
               fixup = .true.
            endif
            if(ssta(nc+1:nc+1).ne.clout(ipos:ipos)) goto 2
            nc = nc+1
            jjname = jname
            do 3 j=jjname,nsname
               if(nsstrt(j).eq.nc) then
                  issim(j) = ipos
               endif
               if(nsend(j).eq.nc) then
                  ifsim(j) = ipos
                  insim(j) = i
                  jname = jname+1 
               endif
    3       continue
            goto 2
         endif
         jlast = jname-1
    1 continue
c
c Check we found all the names
c
      do 7 in=1,nsname
         if(insim(in).eq.0) then
            write(mpunit,500)
            write(mpunit,510) (sima(i),i=ifi,ila)
            goto 999
         endif
    7 continue
c
c if statement is module start, write out the headers
c
c first assign the types ...
c
      call settyp(1)
c
      if(lmodus(icl1)) then
         ctemp0 = snames(isname+1)
         lt0 = lenocc(ctemp0)
         write(mhunit,'(a)') '*=*=*=*= '//ctemp0(:lt0)//
     &                       '.html =*=*=*=*'  
         write(mhunit,'(a)') '<HEADER>'
         write(mhunit,'(a)') '<TITLE>'//sima(ifi)(7:ifsim(1))//
     &                       '</TITLE>'
         write(mhunit,'(a)') '</HEADER>'
         write(mhunit,'(a)') '<BODY>'
         write(mhunit,'(a)') '<H1>'//sima(ifi)(7:ifsim(1))//
     &                       '</H1>'
         write(mhunit,'(/,a,/)') '<PRE>'
         curmod = snames(isname+1)
         ncallis = 0
      endif
c
c Loop over all lines and write out the new lines with replacements
c
      do 4 i=ifi,ila
         clout = sima(i)
         ioff = 0
         do 5 j=1,nsname
            if(insim(j).ne.i) goto 5
            tname = snames(isname+j)
            lt = lenocc(tname)
            ntype = namtyp(isname+j)
            external = itbit(ntype,17).ne.0.or.
     &                 itbit(ntype,12).ne.0.or.
     &                 itbit(ntype,15).ne.0
            if(external) then
c
c check not an intrinsic function
c
               do 8 in=1,lif
                  if(lenocc(cinfun(in)).ne.lt) goto 8
                  if(cinfun(in).ne.tname) goto 8
                  external = .false.
                  goto 9
    8          continue
    9          continue
            endif
c 
            if(linclu(icl1)) then
c
c name is an include file ... special treatment for TAG
c
               ctemp0 = tname
               lt0 = lenocc(ctemp0)
c               if(ctemp0(1:1).eq.'''') ctemp0 = ctemp0(2:)
c               if(ctemp0(1:1).eq.'(') ctemp0 = ctemp0(2:)
c               if(ctemp0(1:1).eq.'<') ctemp0 = '/usr/include/'//
c     &            ctemp0(:lenocc(ctemp0))
c               lt0 = lenocc(ctemp0)
c               if(index(ctemp0,'''').ne.0) lt0=index(ctemp0,'''')-1
c               if(index(ctemp0,')').ne.0) lt0=index(ctemp0,')')-1
c               if(index(ctemp0,'>').ne.0) lt0=index(ctemp0,'>')-1
               cname = anchor(ctemp0(:lt0))
               if(cname.eq.' ') then
c                   cname = '<A HREF="'//ctemp0(:lt0)//'">'//
c     &                     tname(:lt)//'</A>'
               endif
            else if(declare.or.(lmodus(icl1).and.j.eq.1)) then
c
c local name is declared ... place NAME URL
c
               cname = '<A NAME='//tname(:lt)//'>'//
     &                 tname(:lt)//'</A>'
            else if(.not.external.or.(lmodus(icl1).and.j.eq.1)) then
c
c local name is referenced
c
               cname = anchor(tname(:lt))
               if(cname.eq.' ') then
                  cname = '<A HREF=#'//tname(:lt)//'>'//
     &                    tname(:lt)//'</A>'
               endif
            else
c
c name is key into anchor dictionary
c
               cname = anchor(tname(:lt))
c
c do not anchor externals if they do not appear in the dictionary
c
               if(external.and.cname.eq.' ') then
                  cname = '<A >'//tname(:lt)//'</A>'
               endif
               if(external) then
                  do 10 icallis=1,ncallis
                     if(callis(icallis).eq.touppr(tname)) then
                        ncll(icallis) = ncll(icallis)+1
                        goto 11
                     endif
   10             continue  
                  ncallis = ncallis+1
                  ncll(ncallis) = 1
                  callis(ncallis) = touppr(tname)
                  icallis = ncallis
   11             continue
                  curcal = ncll(icallis)
                  lcurmod = lenocc(curmod)
                  cseq = curmod(:lcurmod)//'_'//tname(:lt)//'_'
                  cseq = touppr(cseq)
                  lseq = lt + 2 + lcurmod
                  if(curcal.le.9) then
                     write(cseq(lseq+1:lseq+1),'(i1)') curcal
                     lseq = lseq+1 
                  else if(curcal.le.99) then
                     write(cseq(lseq+1:lseq+2),'(i2)') curcal
                     lseq = lseq+2
                  else if(curcal.le.999) then
                     write(cseq(lseq+1:lseq+2),'(i3)') curcal
                     lseq = lseq+3
                  endif
                  if(cname(1:2).eq.'<A'.or.cname(1:2).eq.'<a') then
                     ctemp1 = cname(3:)
                     cname = '<A NAME='//cseq(:lseq)//' '//ctemp1
                  endif
               endif
            endif
            if(cname.eq.' ') goto 5
            lanch = lenocc(cname)
            is = issim(j)+ioff-1
            if = ifsim(j)+ioff+1
            ctemp1 = clout(:is)
            ctemp2 = clout(if:)
            clout = ctemp1(:is)//cname(:lanch)//ctemp2
            ioff = ioff + (lanch-if+is+1)
    5    continue
         lout = lenocc(clout)
         write(mhunit,'(A)') clout(:lout)
    4 continue
  500 format(/,1x,'NON-FATAL ERROR IN TOHTML ... NOT ALL NAMES FOUND')
  510 format((a))
c
  999 END
