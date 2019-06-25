        subroutine PULSESEP(ix,iy,nsampl_lkr,ADC_sampl,Energy_sampl,
     &                      Nclust,Itseed,Tseed,Eseed,
     &                      E2Pulse,T2Pulse,I2gain,icase)
C
C Time reconstruction of signal with pulses at different times
C ------------------------------------------------------------
C
C input:
c  ix,iy cell id (0 to 127)
C  nsampl_lkr = number of sample
C  ADC_sampl = 10000*gain+ADC
C  Energy_sampl = Energy pulse
C  NClust = number of clusters contributing to this cell
C  Itseed = Time slice of each cluster
C  Tseed = Time estimate for each cluster from seed cell
C  Eseed = Energy estimate in this cell from spatial profil of shower.
C 
C output :
C   E2Pulse = energy in this cell for each cluster
C   T2pulse = time from this cell for each cluster
c   I2gain  = gain range for each cluster
C   icase = error flag for gain switching pathologies
C
C WARNING : Tseed and Eseed are overwritten by this routine .....
C
       implicit none
       integer ix,iy
       integer nsampl_max
       parameter(nsampl_max=200)
       integer Nsampl_lkr,Nclust
       real ADC_sampl(*),Energy_sampl(*)
       real Tseed(*),Eseed(*)
       integer Itseed(*)
       integer I2gain(10)
       real E2Pulse(10),T2Pulse(10)
       integer icase,icase2,itmax,igain_max
       integer j,i
       real SumE
       real Eguess(nsampl_max,10),Epulse(nsampl_max,10)
       integer Niter,iter
       data Niter/2/
       real Tfrac,time
       REAL ClockPeriod
       COMMON/clkCDE/CLOCKPERIOD
c
c initialization
c
       icase=1
c
c allow for some iterations
c
       do iter=1,Niter
c
c loop over time slices, 
c  for each time slice compute energy expected for each cluster
c
        do j=1,nsampl_lkr

         Time=ClockPeriod*float(j)
         SumE=0.
         do i=1,Nclust
            Eguess(j,i) = Eseed(i)*Tfrac(Time-Tseed(i))
            SumE=SumE+Eguess(j,i)
         enddo
c
c  do energy sharing
c
         do i=1,Nclust
            Epulse(j,i)=Energy_sampl(j)-Sume+Eguess(j,i)
         enddo
        enddo
c
c for each cluster, reconstruct shared pulse.
c  note for the ADC pulse we don't have to apply sharing, since it is
c  used only to figure out if we have switching problems ....
c
        do i=1,Nclust
  
        call PulseRec(ix,iy,nsampl_lkr,ADC_sampl,
     &               EPulse(1,i),ITseed(i),ITseed(i),Tseed(i),
     &               E2Pulse(i),T2Pulse(i),icase2,Igain_max,itmax)
        Eseed(i)=E2Pulse(i)
        TSeed(i)=T2Pulse(i)
        I2gain(i)=Igain_max
        if (icase2.gt.icase) icase=icase2
        enddo

      enddo     ! iterations

      return
      end
