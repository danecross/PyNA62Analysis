        subroutine PULSEMAX(nsampl_lkr,ADC_sampl,Energy_sampl,
     &    it1,it2,ecut,nmax,itmax)
C
C Find maxima in time for a given Pulse
C -------------------------------------
C
C input :
C   nsampl_lkr = number of samples
C   ADC_sampl = 1000*Gain + ADC
C   Energy_sampl = Energy Pulse
c   it1,it2 = range to use to look for maxima
c   ecut = energy cut on maximum
C
C output :
C  nmax = number of maximum
C  itmax = time slices of maxima
C  it1,it2 = lowest and highest time slices allowed
C
C  condition for a maximum :
C    remove samples after gain switches up from 0
C    remove samples where high gains only stays for one sample
C    ask for a local maximum among surviving samples
C     (in ADC counts)
C    the calibrated energy for this sample should be above
C    a given cut
C    When the maximum is in gains 1-2 or 3, we also cut
C    on the ADC value, to avoid fluctuations in the undershoot
C    (in high gain the undershoot energy is positive rather
C     than negative, because of the non linearity and the
C     difference between offset and true pedestal)
C
       implicit none
       integer nsampl_max
       parameter(nsampl_max=200)
       integer nsampl_lkr,nmax,itmax(5),it1,it2
       real*8 ecut
       real*8 ADC_sampl(*),Energy_sampl(*)
       integer ngood,i,ig2,iadc,ienergy,ii
       integer isamp(nsampl_max)
       integer nfound,maxflag
c#define SLM 1
c
c get good samples
c
#ifdef SLM
       ngood=0
       do i=max(it1-1,1),min(it2+1,nsampl_lkr)
c
c gain 0, if not the first sample, we require that the
c sample before is also in gain 0
c
         if (ADC_sampl(i).lt.10000) then
             ig2=0 
             if (i.ge.2) then
               if (ADC_sampl(i-1).ge.10000) then
                 ig2=1
               endif
             endif
             if (ig2.eq.0) then
              ngood=ngood+1
              isamp(ngood)=i
             endif
         else
c if gain not 0, we have to look at sample before
c  => this cannot be the first sample
          if (i.ge.2) then
c
c samples before is in the same gain range
c
           if (abs(ADC_sampl(i)-ADC_sampl(i-1)).lt.1024) then
              ngood=ngood+1
              isamp(ngood)=i
           endif
          endif
         endif
       enddo
c
       nmax=0
       do i=2,ngood-1
        ii=Isamp(i)
        if (Energy_sampl(ii).gt.Ecut) then
        if (ii.gt.1.and.ii.lt.nsampl_lkr) then
c
c be careful with gt and ge to avoid missing case where ADC readings are egal...
c
         if (ADC_sampl(ii).gt.ADC_sampl(isamp(i-1)).and.
     &       ADC_sampl(ii).ge.ADC_sampl(isamp(i+1))) then
c
c if gain 1-2-3 require at least 60 ADC counts, to remove fluctuations
c  in undershoot and problems from pedestals ...
c
           if (ADC_sampl(ii).ge.10000) then
               ienergy=ADC_sampl(ii)
               iadc=mod(ienergy,10000)
           else
               iadc=1000
           endif
           if (iadc.ge.60) then
              nmax=nmax+1
              if (nmax.le.5) itmax(nmax)=ii
           endif
         endif
        endif
        endif
       enddo
#else
       ngood=0
       nfound = 0
       do i=max(it1,1),min(it2+1,nsampl_lkr)
        maxflag=0
        if (Energy_sampl(i).gt.Ecut) then
	  if (i.eq.max(it1,1) .and. ADC_sampl(i).gt.ADC_sampl(i+1)) 
     &    maxflag = 1
          if (i.eq.min(it2+1,nsampl_lkr) .and. 
     &   ADC_sampl(i).gt.ADC_sampl(i-1)) maxflag = 1
          if (i.gt.max(it1,1) .and. i.lt.min(it2+1,nsampl_lkr)) then
            if (ADC_sampl(i).gt.ADC_sampl(i-1).and.
     &         ADC_sampl(i).ge.ADC_sampl(i+1))  maxflag = 1
          endif 
          if (maxflag.eq.1) then
		itmax(1)=i
		nfound = nfound + 1
          endif
        endif
       enddo
       if (nfound.gt.0) nmax=1
#endif
c
       return
       end
C
