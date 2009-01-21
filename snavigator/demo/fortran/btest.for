      logical function btest(n,i)
      btest = .false.
      if(itbit(n,i+1).ne.0) btest = .true.
      end
