        subroutine PULSEPED(nsampl_lkr,ADC_sampl,Energy_sampl,nped)
c
c Correct undershoot using first samples
c --------------------------------------
c
c compute energy in first samples
c  assumes correct Pedestal has been used in calibration
c  so here, we only look for a 'flat' negative offset, which
c would mean we are on the undershoot of a earlier photon
c
c#define SLM 1
        implicit none
        integer nsampl_lkr,nped
        real*8 ADC_sampl(*),Energy_sampl(*)
        real ped_energy,ped_samp(20)
        real Eunder_cut
        data Eunder_cut/-0.020/
        real Eshift_cut
        data Eshift_cut/0.020/  
        integer i,n,nn,ibad(20),nbad
        real xnoise,chi2
        data xnoise/0.010/
c
        n=0
        ped_energy=0.
        do i=1,nped
#ifdef SLM
          if(ADC_sampl(i).lt.10000) then
               n=n+1
               ped_samp(n)=Energy_sampl(i)
               ped_energy=ped_energy+Energy_sampl(i)
           endif
#else
           n=n+1
           ped_samp(n)=Energy_sampl(i)
           ped_energy=ped_energy+Energy_sampl(i)
#endif
        enddo
        if (n.le.1) return
 
        if (ped_energy.ge.(Eshift_cut*n)) then   ! positive shift
c
c look if it is a constant shift
          ped_energy=ped_energy/float(n)
          chi2=0.
          do i=1,n
#ifdef SLM
            if (ADC_sampl(i).lt.10000) then
              chi2=chi2+((Energy_sampl(i)-ped_energy)/xnoise)**2
            endif
#else
            chi2=chi2+((Energy_sampl(i)-ped_energy)/xnoise)**2
#endif
          enddo
          if (chi2.le.10.*float(n-1)) then
           do i=1,nsampl_lkr
            Energy_sampl(i)=Energy_sampl(i)-ped_energy
           enddo
          endif
          return
 
        endif
c
c if we don't see negative energy return
c
        if (ped_energy.ge.(Eunder_cut*n)) return
c
        ped_energy=ped_energy/float(n)
c
c if we see negative energy, we have to check first that
c   if is real...
c take care of bad samples (missing bits for instance)
        nbad=0
        do i=1,n
          if (abs(ped_samp(i)-ped_energy).ge.0.500) then
             ibad(i)=1
             nbad=1
          else
             ibad(i)=0
          endif
        enddo
c
c if bad samples were found, recompute pedestal
c
        if (nbad.ge.1) then
           ped_energy=0.
           nn=0
           do i=1,n
             if (ibad(i).eq.0) then
                 nn=nn+1
                 ped_energy=ped_energy+ped_samp(i)
             endif
            enddo
            if (nn.ge.1) ped_energy=ped_energy/float(nn)
        endif
c
c if ped_energy is more than 2 sigma away from noise and
c  negative, we correct this assuming the undershoot is
c  flat (which is not true for central cells, on an
c   event by event basis)
c
        if (ped_energy.le.Eunder_cut) then
         do i=1,nsampl_lkr
           Energy_sampl(i)=Energy_sampl(i)-ped_energy
         enddo
        endif
c
        return
        end
        
