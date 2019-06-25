        subroutine PULSEREC(ix,iy,nsampl_lkr,
     &                      ADC_sampl,Energy_sampl,i1,i2,Tguess,
     &                      Energy,Time,icase,igain,itmax)
C
C Reconstruct a simple Pulse
c --------------------------
c
c input:
C ix,iy = cell id (0 to 127)
c nsampl_lkr = number of samples
c ADC_sampl = 10000*Gain +ADC (Real array like PLKR bank)
C Energy_sampl = calibrated energy pulse
c i1,i2 = time sample range to search for maximum
c Tguess = guess of true time of pulse
c
c output :
c Energy = Energy
c Time = Time
c icase = quality flag       1 if all OK, 2 for late switchers, etc..
c                            if > 90 no energy was reconstructed
c                            if =-1  pulse maximum is at bad position
c                             (first or last sampl) and no energy is computed
c 
c igain = gain of ``true'' maximum
c itmax = position of found maximum
c
       implicit none
       integer ix,iy
       integer nsampl_lkr,i1,i2,icase
       real*8 Tguess
       real*8 ADC_sampl(*),Energy_sampl(*)
       real Energy,Time
       integer j,iadc,igain,itmax,ibads,jj,igmax
       integer jmax
       integer igtest(0:4),icc
       real*8 emax
       real pulse(4),phase0
       REAL ClockPeriod
       COMMON/clkCDE/CLOCKPERIOD
c#define SLM 1
c
c maximum already known
c
       if(i1.eq.i2) then
           itmax=i1
#ifdef SLM
           igain=ADC_sampl(itmax)*0.0001
#else
           igain=0
#endif
c
c find maximum (if not already done before )
       elseif (i1.lt.i2) then
        emax=0.
        itmax=0
        do j=max(2,i1),min(nsampl_lkr-1,i2)
c
          if (ADC_sampl(j).ge.emax) then
#ifdef SLM
c
c require sample before to be in same gain range
c
             if (abs(ADC_sampl(j-1)-ADC_sampl(j)).lt.1024.) then
                emax=ADC_sampl(j)
                itmax=j
                igain=ADC_sampl(j)*0.0001
             endif
#else
           emax = ADC_sampl(j)
           itmax=j
           igain=0
#endif
          endif
         enddo
       else
         return
       endif
c
        jmax=itmax-2
        if(itmax.ge.2.and.itmax.lt.nsampl_lkr) then
c
c check gain switching quality and saturation
           ibads=0
           igmax=0           
           do j=0,4     ! 0 is sample max-2, 4 is sample max+2
            jj=jmax+j   ! from max-2 to max+2
            if (jj.ge.1.and.jj.le.nsampl_lkr) then

               if (j.ge.1) pulse(j)=Energy_sampl(jj)
                                             !j=1 => max-1
c                                             j=2 => max
c                                             j=3 => max+1
c                                             j=4 => max+2
#ifdef SLM
               if (ADC_sampl(jj).le.10000) then ! gain 0 case is trivial
                 igtest(j)=0
                 if (ADC_sampl(jj).ge.1023) then
                   igtest(j)=100+igtest(j)
                   ibads=ibads+1
                 endif
               else
                  igtest(j)=ADC_sampl(jj)*0.0001
                  iadc=ADC_sampl(jj)-igtest(j)*10000
                  if (igtest(j).gt.igmax) igmax=igtest(j)
                  if (iadc.ge.1023) then     ! check saturation
                    igtest(j)=100+igtest(j)
                    ibads=ibads+1
                  endif
               endif
            else
                if (j.ge.1) pulse(j)=0.
                igtest(j)=0
            endif
           enddo
c
           if (ibads.eq.0.and.igmax.eq.0) then   ! no gain switching, no satur.
              icase=1
              icc=1
              goto 601   ! to pulse computation
           endif 
c
           if (ibads.eq.0) then            ! no saturation
                if (igtest(0).eq.igain.and.
     &              igtest(1).eq.igain.and.
     &              igtest(3).eq.igain) then
                      icase = 1   ! all OK
                      icc = 1
                      goto 601
                elseif(igtest(0).ne.igain.and.
     &                 igtest(1).eq.igain.and.
     &                 igtest(3).eq.igain) then
                     icase = 2  ! use max and max+1
                elseif(igtest(0).eq.igain.and.
     &                 igtest(1).eq.igain.and.
     &                 igtest(3).ne.igain) then
                     icase = 3   ! use max and max-1
                elseif(igtest(0).ne.igain.and.
     &                 igtest(1).eq.igain.and.
     &                 igtest(3).ne.igain.and.
     &                 igtest(4).eq.igtest(3)) then
                     icase=6    ! use max and max+2
                elseif(igtest(1).ne.igain.and.
     &                 igtest(3).eq.igain) then
                      if (igtest(0).eq.igtest(1)) then
                         icase=5    ! use max-1 and max+1
                      elseif(igtest(4).eq.igain) then
                         icase=4   ! use max+1 and max+2
                      else
                       icase=99
                      endif
                else
                    icase=99
                endif

           elseif(ibads.eq.1) then     ! saturation
               if (igtest(2).ge.100) then  ! maximum saturated
                  if (igtest(0).eq.igain.and.igtest(1).eq.igain.
     &                and.igtest(3).eq.igain) then
                       icase = 15   ! use max-1 and max+1
                  elseif(igtest(0).ne.igain.and.
     &                   igtest(3).eq.igain.and.
     &                   igtest(4).eq.igain) then
                       icase = 14 ! use max+1 and max+2
                  else
                      icase=99
                  endif
               else      ! maximum not saturated
                 if (igtest(0).eq.igain.and.
     &               igtest(1).eq.igain.and.
     &               igtest(3).eq.igain) then
                        icase=21
                 elseif(igtest(0).ne.igain.and.
     &                  igtest(1).eq.igain.and.
     &                  igtest(3).eq.igain) then
                        icase=22
                 elseif(igtest(0).ne.igain.and.
     &                  mod(igtest(1),100).eq.igain.and.
     &                  igtest(3).eq.igain) then
                        icase=22
                 elseif(igtest(0).eq.igain.and.
     &                  igtest(1).eq.igain) then
                      icase=23
                 elseif(igtest(0).ne.igain.and.
     &                  igtest(1).eq.igain.and.
     &                  igtest(3).ge.100.and.
     &                  mod(igtest(3),100).eq.igtest(4)) then
                      icase=26   ! use max and Max+2 to measure energy
                 else
                    icase=99
                 endif
               endif

           elseif(ibads.ge.2) then
              icase=98
           endif
#else
            else
                if (j.ge.1) pulse(j)=0.
                igtest(j)=0
            endif
           enddo
           icase=1
           icc=1
           goto 601
#endif
c
c
c do  digital filter
           icc=mod(icase,10)

 601       continue   ! jump here for simple pulses

c
c want to use max+2 sample. check that it is there and flag
c  with specific error if not
c
           if (icc.eq.4.or.icc.eq.6) then
              if ((itmax+2).gt.nsampl_lkr) then
                 icase=97
                 icc=7
               endif
           endif
           if (icc.ge.1.and.icc.le.6) then
c
c compute guess of phase = Dt between itmax and Tguess
c
                if (Tguess.gt.0.) then
                   phase0=ClockPeriod*itmax-Tguess
                else
                   phase0=0.
                endif
                call DigFilter(ix,iy,igain,icc,pulse,Energy,
     &                         Time,phase0)
           else
                Energy=0.
                Time=0.
           endif
           Time=ClockPeriod*(itmax-1)+Time
c
        else     ! pb with position of maximum
           Energy=0.
           Time=0.
           icase=-1

        endif    ! maximum position is OK       

        return
         end
