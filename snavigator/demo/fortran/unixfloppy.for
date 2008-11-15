      PROGRAM FLOPPY
C-------------------------------------------------------------------------
C Floppy UNIX interface routine.
C Sets up various required input files for Floppy.
C
C Julian Bunn 1990
C-------------------------------------------------------------------------
      PARAMETER (MLEN=256,MXLIN=80,maxarg=100)
      include 'param.h'
      include 'usunit.h'
      include 'ushtml.h'
      character*(mxlin) argval
      character*1 key,char
      CHARACTER*(MLEN)  CFILE,COLD,CFORT,CTEMP,CBAD,CTREE
      character*(mlen) chtml,chtml_dict
      LOGICAL LOG,fexist,fold,fqold,tidy,tree
C$HP9000_800 INTRINSICS ON
c
c get all arguments
c
      numargs = iargc()
      if(numargs.gt.maxarg) then
         write(MPUNIT,'(A)') ' Floppy --> Too many arguments '
         goto 900
      endif
c
c get target filename(s)
c
      call getarg(numargs,cfile)
      lfile = index(cfile,' ')-1
      write(MPUNIT,'(A)') ' Floppy --> Target file '//cfile(:lfile)
      inquire(file=cfile(:lfile),exist=fexist)
      if(.not.fexist) then
         cfile = cfile(:lfile)//'.f'
         lfile = lfile + 2
         inquire(file=cfile(:lfile),exist=fexist)
         if(.not.fexist) then
            cfile = cfile(:lfile)//'or'
            lfile = lfile + 2
            inquire(file=cfile(:lfile),exist=fexist)
         endif
      endif
      if(.not.fexist) then
        write(MPUNIT,'(A)') ' Floppy --> Target file not found !'
        goto 900
      endif
c
      log = .false.
      fold = .false.
      fqold = .false.
      tidy = .false.
      html = .false.
      tree = .false.
      cfort = ' '
      ctree = ' '
      anchor_list = 0
c
      do 400 iarg=1,numargs-1
         call getarg(iarg,argval)
         if(argval(:2).eq.'-l') log = .true.
         if(argval(:2).eq.'-o') fqold = .true.
         if(argval(:2).eq.'-o') cold = argval(3:)
         if(argval(:2).eq.'-H') then
            html = .true.
            iend = index(cfile,'.')-1
            if(iend.eq.-1) iend = index(cfile,' ')-1
            chtml = cfile(:iend)//'.html'
         endif
  400 continue
c
      cbad = 'scratch'
      open(MCUNIT,status='scratch',err=999)
      WRITE(MCUNIT,'(A)') 'LIST,GLOBAL,TYPE;'
      WRITE(MCUNIT,'(A)') 'PRINT,ILLEGAL;'
      WRITE(MCUNIT,'(A)') 'OPTIONS,USER;'
      if(fqold) then
        if(cold(1:1).eq.' ') cold = cfile(:lfile)//'.flopold'
        lold = index(cold,' ')-1
        inquire(file=cold(:lold),exist=fold)
        if(log) write(MPUNIT,'(A)') ' Floppy --> Old file: '//
     &                              cold(:lold)
        if(.not.fold) then
           write(MPUNIT,'(A)') ' Floppy --> Old file not found !'
           goto 900
        endif
        cbad = cold
        open(15,file=cold,status='old',err=999)
  450   read(15,'(A)',end=451,err=999) ctemp
        goto 450
  451   continue
      else
        cold = cfile(:lfile)//'.flopold '
        lold = index(cold,' ')-1
        cbad = cold
        inquire(file=cold(:lold),exist=fexist)
        if(fexist) then
              open(15,file=cold(:lold),status='old')
              close(15,status='delete')
        endif
        open(15,file=cold(:lold),status='new',err=999)
      endif
c
c loop over all qualifiers
c
      icheck = 0
      iset_checks = 0
      do 500 iarg = 1,numargs-1
         call getarg(iarg,argval)
         larg = index(argval,' ')-1
         key = argval(2:2)
         if(key.eq.'l') then
           log = .true.
         else if(key.eq.'n') then
           if(argval(3:3).eq.' ') then
              write(MPUNIT,'(A)') ' Floppy --> Missing value for -n'
              goto 900
           endif
           cfort = argval(3:)
           lfort = index(cfort,' ')-1
           if(log) write(MPUNIT,'(A)') ' Floppy --> Tidied Fortran: '//
     &             cfort(:lfort)
         else if(key.eq.'o') then
c
         else if(key.eq.'f') then
           if(log) write(MPUNIT,'(A)')
     &             ' Floppy --> List source line numbers'
           write(15,'(a)') '*FULL'
         else if(key.eq.'i') then
           ctemp = argval(3:)
   50      iend = index(ctemp,',')
           if(iend.ne.0) then
             write(15,'(A)') ctemp(:iend-1)
             if(log) write(MPUNIT,'(A)')
     &         ' Floppy --> Ignore: '//ctemp(:iend-1)
             ctemp = ctemp(iend+1:)
             goto 50
           endif
           iend = index(ctemp,' ')
           write(15,'(A)') ctemp(:iend)
           if(log) write(MPUNIT,'(A)') ' Floppy --> Ignore: '//
     &                                 ctemp(:iend)
         else if(key.eq.'S') then
           ctemp = argval(3:)
           itemp = index(ctemp,' ')-1
           if(log) write(mpunit,'(a)') ' Floppy --> Special for : '//
     &             ctemp(:itemp)
           write(15,'(a)') '*'//ctemp(:itemp)
           if(ctemp(:5).eq.'ALEPH'.or.ctemp(:5).eq.'ATLAS')
     &        iset_checks = 1
         else if(key.eq.'c') then
           icheck = 1
           ctemp = argval(3:)
           if(ctemp.eq.'standard'.and.iset_checks.eq.0) then
             write(15,'(A)') '*CHECK RULE *'
             if(log) write(MPUNIT,'(A)')
     &               ' Floppy --> Check Standard rules'
           else if(ctemp.eq.' '.and.iset_checks.eq.0) then
             write(15,'(A)') '*CHECK RULE *'
             if(log) write(MPUNIT,'(A)')
     &               ' Floppy --> Check Standard rules'
           else if(ctemp.eq.'a') then
              write(15,'(A)') '*CHECK RULE 99'
              if(log) write(MPUNIT,'(A)') ' Floppy --> Check all rules'
           else if(ctemp.eq.'n') then
             write(15,'(A)') '*CHECK RULE -99'
              if(log) write(MPUNIT,'(A)') ' Floppy --> No rule checks'
           else
             ctemp = ctemp(:index(ctemp,' ')-1)
             if(log) write(MPUNIT,'(A)') ' Floppy --> Check rules: '//
     &               ctemp(:index(ctemp,' ')-1)
   51        iend = index(ctemp,',')
             if(iend.ne.0) then
               write(15,'(A)') '*CHECK RULE '//ctemp(:iend-1)
               ctemp = ctemp(iend+1:)
               goto 51
             endif
             write(15,'(A)') '*CHECK RULE '//ctemp
           endif
         else if(key.eq.'t') then
           write(MCUNIT,'(A)') 'OPTIONS,TREE;'
           ctree = cfile(:lfile)//'.floptre'
           ltree = index(ctree,' ')-1
           cbad = ctree
           if(log) write(MPUNIT,'(A)')
     &             ' Floppy --> Produce file for Flow: '//ctree(:ltree)
           inquire(file=ctree(:ltree),exist=fexist)
           if(fexist) then
              open(MJUNIT,file=ctree(:ltree),status='old')
              close(MJUNIT,status='delete')
           endif
           open(MJUNIT,file=ctree(:ltree),status='new',
     &          form='unformatted',err=999)
           tree = .true.
         else if(key.eq.'j') then
           char = argval(3:3)
           if(char.eq.' ') char = '3'
           write(MCUNIT,'(A)') 'OPTIONS,INDENT='//char//';'
           if(log) write(MPUNIT,'(A)')
     &             ' Floppy --> Indent clauses by '//char
           tidy = .true.
         else if(key.eq.'F') then
           write(MCUNIT,'(A)') 'STATEMENTS,SEPARATE;'
           if(log) write(MPUNIT,'(A)')
     &             ' Floppy --> Group FORMATs at end'
           tidy = .true.
         else if(key.eq.'g') then
           write(MCUNIT,'(A)') 'STATEMENTS,GOTO;'
           if(log) write(MPUNIT,'(A)') ' Floppy --> Right align GOTOs'
           tidy = .true.
         else if(key.eq.'r') then
           ctemp = argval(3:)
           iend = index(ctemp,',')
           if (iend.eq.0) ctemp = ctemp(:index(ctemp,' ')-1)//',10'
           write(MCUNIT,'(A)') 'STATEMENTS,FORMAT='//
     &                    ctemp(:index(ctemp,' ')-1)//';'
           if(log) write(MPUNIT,'(A)')
     &             ' Floppy --> Renumber FORMATs: '//
     &             'start,step '//ctemp(:index(ctemp,' '))
           tidy = .true.
         else if(key.eq.'s') then
           ctemp = argval(3:)
           iend = index(ctemp,',')
           if (iend.eq.0) ctemp = ctemp(:index(ctemp,' ')-1)//',10'
           write(MCUNIT,'(A)') 'STATEMENTS,NUMBER='//
     &                    ctemp(:index(ctemp,' ')-1)//';'
           if(log) write(MPUNIT,'(A)')
     &             ' Floppy --> Renumber statements: '//
     &             'start,step '//ctemp(:index(ctemp,' '))
           tidy = .true.
         else if(key.eq.'H') then
           html = .true.
           if (argval(3:3).ne.' ') then
              chtml_dict = argval(3:)
           else
              iend = index(chtml,'.')-1
              if(iend.eq.-1) iend=lenocc(chtml)
              chtml_dict = chtml(:iend)//'.htmldict'
           endif
           if(log) write(MPUNIT,'(a)') 
     &             ' Floppy --> Generate HTML in '//
     &             chtml(:lenocc(chtml))
         else if(key.eq.'A') then
           anchor_list = 2
           if(log)write(MPUNIT,'(A)') 
     &            ' Floppy --> Use or create Anchor Dictionary'
         else
           write(MPUNIT,'(A)')
     &     ' Floppy --> Unrecognized qualifier '//key
         endif
  500 continue
c
      if(html) then
         cbad = chtml
         inquire(file=chtml(:lenocc(chtml)),exist=fexist)
         if(fexist) then
            open(mhunit,file=chtml(:lenocc(chtml)),status='old')
            close(mhunit,status='delete')
         endif
         open(mhunit,file=chtml(:lenocc(chtml)),status='new',err=999)
      endif
      cbad = chtml_dict
      if(anchor_list.eq.2) then
         nanchr = 0
         inquire(file=chtml_dict(:lenocc(chtml_dict)),exist=fexist)
         if(fexist) then
            if(log)write(MPUNIT,'(A)') 
     &             ' Floppy --> Reading Anchor Dictionary '//
     &             chtml_dict(:lenocc(chtml_dict))
            open(mdunit,file=chtml_dict(:lenocc(chtml_dict)),
     &      status='old',err=999)
            nanchr = 0
   20       read(mdunit,'(a)',end=21,err=999) ctemp
            ibl = index(ctemp,' ')
            if(ibl.eq.0) goto 20
            if(nanchr.ge.mxanch) then
            write(MPUNIT,'(a)') ' Floppy --> Too many anchors in file'
               goto 21
            endif
            nanchr = nanchr+1
            canchr(nanchr) = ctemp(:ibl-1)
            cancht(nanchr) = ctemp(ibl+1:lenocc(ctemp))
            goto 20
   21       close(mdunit)
            if(log)write(MPUNIT,'(a,i4,a)') ' Floppy - a total of ',
     &             nanchr,' anchor(s) were read.'
            open(mdunit,file=chtml_dict(:lenocc(chtml_dict)),
     &      status='old',err=999)
         else
            open(mdunit,file=chtml_dict(:lenocc(chtml_dict)),
     &      status='new',err=999)
         endif
      endif
c
      if(tidy) then
         write(MCUNIT,'(A)') 'OUTPUT,FULL,COMPRESS;'
         if(cfort(1:1).eq.' ') then
           cfort = cfile(:lfile)//'.out'
           lfort = index(cfort,' ')-1
         endif
         cbad = cfort
         open(14,file=cfort(:lfort),status='unknown',err=999)
      endif
c
c default action is to check standard rules
c
      if(icheck.eq.0.and..not.fqold) then
         write(15,'(A)') '*CHECK RULE *'
      endif
 
      write(MCUNIT,'(A)') 'END;'
      if(log) write(MPUNIT,'(A)') ' Floppy --> Finished parsing command'
      rewind(MCUNIT)
      rewind(15)
      cbad = cfile
      open(MIUNIT,file=cfile(:lfile),status='old',err=999)
      cbad = 'scratch'
      open(MZUNIT,status='scratch',err=999)
c
      call allpro
c
      close(MIUNIT)
      if(tidy) close(14)
      if(tree) close(MJUNIT)
      close(MCUNIT)
      close(MZUNIT)
      write(MPUNIT,'(A)') ' Floppy --> has finished'
      goto 2000
C
  999 CONTINUE
      WRITE(MPUNIT,'(A)') ' Floppy --> Error opening '//
     &               cbad(:index(cbad,' '))
  900 write(MPUNIT,'(A)') ' Floppy aborted'
 2000 CONTINUE
C$HP9000_800 INTRINSICS OFF
      END
