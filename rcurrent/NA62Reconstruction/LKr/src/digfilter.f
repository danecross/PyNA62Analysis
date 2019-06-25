c
        subroutine DigFilter(ix,iy,igain,icase,pulse,Energy,Time,Phase0)
c
c reconstruct energy using Digital Filter
c ---------------------------------------
c
c  input pulse(1->4) pulse.  maximum is at pulse(2)
c  output Energy and Time
c
c  ix,iy  = cell (0to 127)
c  igain = Gain from 0 to 3 !
c  icase = 1 use sample 1 2 3
c          2 use sample 2 3
c          3 use sample 1 2
c          4 use sample 3 4
c          5 use sample 1 3
c          6 use sample 2 4
c  Phase0 = guess of phase
c
c Start with coeff. corresponding to the guessed phase.
c  If energy is high enough, we iterate
c  If energy is small we don't
c  Cut on energy is 20 MeV (2.5 sigma of noise)
c
        implicit none
c 
       integer nref_max
       parameter(nref_max=20)
       real a(nref_max,4,-25:25,3)
       real b(nref_max,4,-25:25,3)
       real a2(nref_max,4,-40:40,2)
       real b2(nref_max,4,-40:40,2)
       real a3(nref_max,4,-40:20,2)
       real b3(nref_max,4,-40:20,2)
       integer Icell_shape(0:127,0:127)
       common/filCDE/Icell_shape,a,b,a2,b2,a3,b3
        integer ix,iy
        real pulse(4),Energy,Time
        real phase0
        real az(3),bz(3),xphase,x1,x2
        integer i1,i2,j
        integer igain,ig,iter
        real TTime
        integer icase,Iphase
        integer Iref
        real Ecut
       REAL ClockPeriod
       COMMON/clkCDE/CLOCKPERIOD
       data Ecut/0.150/
c
        ig=igain+1
c
c what reference shape should we use ?
c
CC        PRINT*,ix,iy
        Iref=Icell_shape(ix,iy)
    
        if (icase.eq.1) then
c 
c use max-1  max and max+1
c
         if (phase0.ge.0.) then
            Iphase=int(phase0)
         else
            Iphase=-1*int(-phase0)
         endif
         if (Iphase.gt.25.or.Iphase.lt.-25) Iphase=0

         Energy=a(Iref,ig,Iphase,1)*pulse(1)+
     &          a(Iref,ig,Iphase,2)*pulse(2)+
     &          a(Iref,ig,Iphase,3)*pulse(3)
         Time=b(Iref,ig,Iphase,1)*pulse(1)+
     &        b(Iref,ig,Iphase,2)*pulse(2)+
     &        b(Iref,ig,Iphase,3)*pulse(3)
         if (energy.le.1e-4) then
            time=0.
            return
         endif
         Time=Time/Energy-float(Iphase)
CCCC         PRINT*,'time ',b(Iref,ig,Iphase,1),b(Iref,ig,Iphase,2),
CCCC     &                  b(Iref,ig,Iphase,3),pulse(1),pulse(2),pulse(3),
CCCC     &                  Time,Energy 
         if (energy.le.Ecut) return    ! no iteration if low energy
c
         xphase=-Time
         if (xphase.gt.0.) then
           i1=int(xphase)
         else
           i1=-1*int(-xphase+1)
         endif
         if (i1.ge.-25.and.i1.le.25) then
c
            Energy=a(Iref,ig,i1,1)*pulse(1)+
     &             a(Iref,ig,i1,2)*pulse(2)+
     &             a(Iref,ig,i1,3)*pulse(3)
            Time=b(Iref,ig,i1,1)*pulse(1)+
     &           b(Iref,ig,i1,2)*pulse(2)+
     &           b(Iref,ig,i1,3)*pulse(3)
            Time=Time/Energy-float(i1)
         endif

        elseif (icase.eq.2) then
c
c use max and max+1
           if (phase0.ge.0.) then
            Iphase=int(phase0)
           else
            Iphase=-1*int(-phase0)
           endif
           if (Iphase.gt.40.or.Iphase.lt.-40) Iphase=0


           Energy=a2(Iref,ig,Iphase,1)*pulse(2)+
     &            a2(Iref,ig,Iphase,2)*pulse(3)
           Time=b2(Iref,ig,Iphase,1)*pulse(2)+
     &          b2(Iref,ig,Iphase,2)*pulse(3)
           if (energy.le.1e-3) then
              time=0.
              return
           endif
           Time=Time/Energy-float(Iphase)
           if (energy.le.Ecut) return    ! no iteration if low energy
           xphase=-Time

           do iter=1,3
           if (xphase.gt.0.) then
             i1=int(xphase)
             i2=i1+1
             x1=float(i1)
             x2=float(i2)
           else
             i1=-1*int(-xphase+1)
             i2=i1+1
             x1=float(i1)
             x2=float(i2)
           endif
           if (i1.ge.-40.and.i2.le.40) then
            do j=1,2
               az(j)=((x2-xphase)*a2(Iref,ig,i1,j)+
     &                (xphase-x1)*a2(Iref,ig,i2,j))/(x2-x1)
               bz(j)=((x2-xphase)*b2(Iref,ig,i1,j)+
     &                (xphase-x1)*b2(Iref,ig,i2,j))/(x2-x1)
            enddo
c
            Energy=
     &             az(1)*pulse(2)+
     &             az(2)*pulse(3)
            TTime=
     &           bz(1)*pulse(2)+
     &           bz(2)*pulse(3)
            TTime=TTime/Energy
            Time=Time + TTime
            xphase=-Time
           endif
           enddo

         elseif(icase.eq.3) then
c
c use max-1 and max
c  first guess of phase, maximum is at phase 0
c   i.e first measured sample at phase -25 ns
c
           if (phase0.ge.0.) then
            Iphase=int(phase0) -25
           else
            Iphase=-1*int(-phase0) -25
           endif
           if (Iphase.gt.40.or.Iphase.lt.-40) Iphase=-25

           Energy=a2(Iref,ig,Iphase,1)*pulse(1)+
     &             a2(Iref,ig,Iphase,2)*pulse(2)
           Time=b2(Iref,ig,Iphase,1)*pulse(1)+
     &           b2(Iref,ig,Iphase,2)*pulse(2)
           if (energy.le.1e-3) then
              time=0.
              return
           endif
           Time=Time/Energy-float(Iphase)
           if (energy.le.Ecut) then
               Time = Time-25.
               return    ! no iteration if low energy
           endif
           xphase=-Time

           do iter=1,3
           if (xphase.gt.0.) then
             i1=int(xphase)
             i2=i1+1
             x1=float(i1)
             x2=float(i2)
           else
             i1=-1*int(-xphase+1)
             i2=i1+1
             x1=float(i1)
             x2=float(i2)
           endif
           if (i1.ge.-40.and.i2.le.40) then
            do j=1,2
               az(j)=((x2-xphase)*a2(Iref,ig,i1,j)+
     &                (xphase-x1)*a2(Iref,ig,i2,j))/(x2-x1)
               bz(j)=((x2-xphase)*b2(Iref,ig,i1,j)+
     &                (xphase-x1)*b2(Iref,ig,i2,j))/(x2-x1)
            enddo
c
            Energy=
     &             az(1)*pulse(1)+
     &             az(2)*pulse(2)
            TTime=
     &           bz(1)*pulse(1)+
     &           bz(2)*pulse(2)
            TTime=TTime/Energy
c
            Time=Time+TTime
            xphase=-Time
           endif
           enddo

           Time=Time-25.   ! to compute time wrt to max and not max-1

         elseif(icase.eq.4) then
c
c use max+1 and max+2
c  first guess of phase + 30. for time of max+1 wrt to true maximum
c
           if (phase0.ge.0.) then
            Iphase=int(phase0) +30
           else
            Iphase=-1*int(-phase0) +30
           endif
           if (Iphase.gt.40.or.Iphase.lt.-40) Iphase=30

           Energy=a2(Iref,ig,Iphase,1)*pulse(3)+
     &            a2(Iref,ig,Iphase,2)*pulse(4)
           Time=b2(Iref,ig,Iphase,1)*pulse(3)+
     &          b2(Iref,ig,Iphase,2)*pulse(4)
           if (energy.le.1e-3) then
              time=0.
              return
           endif
           Time=Time/Energy-float(Iphase)
           if (energy.le.Ecut) then
            Time=TIme+25.
            return    ! no iteration if low energy
           endif
           xphase=-Time

           do iter=1,3
           if (xphase.gt.0.) then
             i1=int(xphase)
             i2=i1+1
             x1=float(i1)
             x2=float(i2)
           else
             i1=-1*int(-xphase+1)
             i2=i1+1
             x1=float(i1)
             x2=float(i2)
           endif
           if (i1.ge.-40.and.i2.le.40) then
            do j=1,2
               az(j)=((x2-xphase)*a2(Iref,ig,i1,j)+
     &                (xphase-x1)*a2(Iref,ig,i2,j))/(x2-x1)
               bz(j)=((x2-xphase)*b2(Iref,ig,i1,j)+
     &                (xphase-x1)*b2(Iref,ig,i2,j))/(x2-x1)
            enddo
c
            Energy=
     &             az(1)*pulse(3)+
     &             az(2)*pulse(4)
            TTime=
     &           bz(1)*pulse(3)+
     &           bz(2)*pulse(4)
            TTime=TTime/Energy
c
            Time=Time+TTime
            xphase=-Time
           endif
           enddo

           Time=TIme+25.   ! compute time wrt to max and not max+1

         elseif(icase.eq.5) then
c
c use max-1 and max+1
c  first guess of phase = -25 ns (time of max-1 wrt maximum)
c
           if (phase0.ge.0.) then
            Iphase=int(phase0) -25
           else
            Iphase=-1*int(-phase0) -25
           endif
           if (iphase.lt.-40.or.Iphase.gt.20.) Iphase=-25

           Energy=a3(Iref,ig,Iphase,1)*pulse(1)+
     &            a3(Iref,ig,Iphase,2)*pulse(3)
           Time=b3(Iref,ig,Iphase,1)*pulse(1)+
     &           b3(Iref,ig,Iphase,2)*pulse(3)
           if (energy.le.1e-3) then
              time=0.
              return
           endif
           Time=Time/Energy-float(Iphase)
           if (energy.le.Ecut) then
              Time=Time-25.
              return    ! no iteration if low energy
           endif

           xphase=-Time

           do iter=1,3
           if (xphase.gt.0.) then
             i1=int(xphase)
             i2=i1+1
             x1=float(i1)
             x2=float(i2)
           else
             i1=-1*int(-xphase+1)
             i2=i1+1
             x1=float(i1)
             x2=float(i2)
           endif
           if (i1.ge.-40.and.i2.le.20) then
            do j=1,2
               az(j)=((x2-xphase)*a3(Iref,ig,i1,j)+
     &                (xphase-x1)*a3(Iref,ig,i2,j))/(x2-x1)
               bz(j)=((x2-xphase)*b3(Iref,ig,i1,j)+
     &                (xphase-x1)*b3(Iref,ig,i2,j))/(x2-x1)
            enddo
c
            Energy=
     &             az(1)*pulse(1)+
     &             az(2)*pulse(3)
            TTime=
     &           bz(1)*pulse(1)+
     &           bz(2)*pulse(3)
            TTime=TTime/Energy
c
            Time=Time+TTime
            xphase=-Time
           endif
           enddo

           Time=Time-25.   ! compute time wrt to max and not max-1 

         elseif(icase.eq.6) then
c
c use max and max+2 (max is probably not the real one, but the
c  real one is badly measured because gain goes down)
c
           if (phase0.gt.0.) then
            Iphase=int(phase0)
           elseif(phase0.lt.0) then
            Iphase=-1*int(-phase0)
           else
            Iphase=-15   ! no guess of the phase from central cell

           endif
           if (iphase.lt.-40.or.Iphase.gt.20.) Iphase=-15

           Energy=a3(Iref,ig,-15,1)*pulse(2)+
     &             a3(Iref,ig,-15,2)*pulse(4)
           Time=b3(Iref,ig,-15,1)*pulse(2)+
     &           b3(Iref,ig,-15,2)*pulse(4)
           if (energy.le.1e-3) then
              time=0.
              return
           endif
           Time=Time/Energy-float(iphase)
           if (energy.le.Ecut) return    ! no iteration if low energy
           xphase=-Time

           do iter=1,3
           if (xphase.gt.0.) then
             i1=int(xphase)
             i2=i1+1
             x1=float(i1)
             x2=float(i2)
           else
             i1=-1*int(-xphase+1)
             i2=i1+1
             x1=float(i1)
             x2=float(i2)
           endif
           if (i1.ge.-40.and.i2.le.20) then
            do j=1,2
               az(j)=((x2-xphase)*a3(Iref,ig,i1,j)+
     &                (xphase-x1)*a3(Iref,ig,i2,j))/(x2-x1)
               bz(j)=((x2-xphase)*b3(Iref,ig,i1,j)+
     &                (xphase-x1)*b3(Iref,ig,i2,j))/(x2-x1)
            enddo
c
            Energy=
     &             az(1)*pulse(2)+
     &             az(2)*pulse(4)
            TTime=
     &           bz(1)*pulse(2)+
     &           bz(2)*pulse(4)
            TTime=TTime/Energy
c
            Time=Time+TTime
            xphase=-Time
           endif
           enddo


         endif
c
         Time=Time*ClockPeriod/25.
         return
         end
