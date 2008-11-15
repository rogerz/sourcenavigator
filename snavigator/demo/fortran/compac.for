      SUBROUTINE COMPAC(NUMBER)
*-----------------------------------------------------------------------
*
* extracts the FORTRAN field contents from the statement image.
* Modified by JJB to allow for inline comments starting with "!"
*
*--- input
*    NUMBER          number of the statement to be extracted
*    SIMA            COMMON/ALCAZA/ (contains one complete routine)
*    NLTYPE,NFLINE,NLLINE,  COMMON/STATE/
*
*--- output
*    SSTA            COMMON/ALCAZA/  FORTRAN fields 7-72 of SIMA
*    NCHST           COMMON/STATE/  last non-blank in SSTA
*                    or =0 if statement consists of comment lines only
*    NLIMA, NLREF(1..NLIMA),   /STATE/
*
*
*-----------------------------------------------------------------------
      include 'param.h'
      include 'alcaza.h'
      include 'cursta.h'
      include 'state.h'
c
      character*1 ch
      logical str_started
c
      nchst = 0
      nlima = 0
      nstart = nfline(number)
      nfinis = nlline(number)
      str_started = .false.
      do 10 jline = nstart,nfinis
         jplink(jline) = 0
         if(nltype(jline).eq.0) goto 10       ! ignore comment lines
         nlima = nlima+1
         nlref(nlima) = jline
         nquote = 0
         if(str_started) nquote = 1
         jend = 72
         laste = 0
         do 20 ich=7,72                       ! look at all characters in line
            ch = sima(jline)(ich:ich)
            if(ch.eq.'''') then
               nquote = nquote+1
               if(mod(nquote,2).eq.0) jend = ich
            else if(ch.eq.'!'.and.mod(nquote,2).eq.0) then
               laste = ich-1
               goto 30
            else if(ch.ne.' '.and.laste.eq.0) then
               jend = ich
            endif
   20    continue
   30    str_started = .false.
         if(mod(nquote,2).eq.1) str_started = .true.
         ssta(nchst+1:) = sima(jline)(7:jend)
         nchst = nchst + jend - 6
         jplink(jline) = laste
   10 continue
c                  
c
c              
c      NCHST=0
c      NLIMA=0
c*--- find last non-blank (only last line)
c      JEND=LASTNB(SIMA(NLLINE(NUMBER)),8,72)
c      DO 10 JLINE=NFLINE(NUMBER),NLLINE(NUMBER)
c         IF (NLTYPE(JLINE).EQ.0) GOTO 10
c         NLIMA=NLIMA+1
c         NLREF(NLIMA)=JLINE
c         IF (JLINE.EQ.NLLINE(NUMBER))  THEN
c            JLAST=JEND
c         ELSE
c            JLAST=72
c         ENDIF
c         L=JLAST-6
c         SSTA(NCHST+1:NCHST+L)=SIMA(JLINE)(7:JLAST)
c         NCHST=NCHST+L
c   10 CONTINUE
      END
