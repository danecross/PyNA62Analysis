      SUBROUTINE FITDEAD(Ebox_dead,ixdead,iydead,x1,x2,y1,y2,
     &                   Xcl,Ycl,Ecl,Edead)
C
      implicit none
      real ebox_dead(-2:2,-2:2),x1,x2,y1,y2,Xcl,Ycl,Ecl,Edead
      integer ixdead,iydead
      INTEGER NCELX,NCELY
      PARAMETER(NCELX=128,NCELY=128)
      INTEGER INDHIGH
      REAL XCELL,YCELL
      REAL XCELL_LKR(0:NCELX-1),YCELL_LKR(0:NCELY-1)
      REAL EDGE_X,EDGE_Y,EDGE_D
      INTEGER NHMAX
      PARAMETER(NHMAX=250)
      REAL EHIGH,THIGH
      INTEGER IXHIGH,IYHIGH,ITHIGH,IUSED
      INTEGER NHIGH
      INTEGER IPOINTLKR
      COMMON /cunCDE/EHIGH(NHMAX),THIGH(NHMAX),
     &                INDHIGH(0:NCELX-1,0:NCELY-1),
     &                IXHIGH(NHMAX),IYHIGH(NHMAX),ITHIGH(NHMAX),
     &                IUSED(NHMAX),NHIGH,
     &                IPOINTLKR(0:NCELX-1,0:NCELY-1),XCELL,YCELL,
     &                EDGE_X,EDGE_Y,EDGE_D,XCELL_LKR,YCELL_LKR
      real dx0,dy0
      parameter(dx0=0.10)
      parameter(dy0=0.10)   ! x and y steps in cell units 
      real dx1,dy1
      parameter(dx1=0.04)
      parameter(dy1=0.04)
      integer i,j,nx,ny,Iteration,ix,iy
      real dist,dist_x,dist_y,dx,dy,x,y,energy
      real  SumE,Sumfr,fr(-2:2,-2:2),chi2,chi2_min,Sumfr_save
      real fr_save(-2:2,-2:2)
      real Efrac,Efrac2

      SumE=0.
      Ecl=0.
      do i=-2,2
         do j=-2,2
          if (i.ne.0.or.j.ne.0) then
            SumE=SumE+Ebox_dead(i,j)
          endif
         enddo
      enddo

c some initialization
      xcl=0.5*(x1+x2)
      ycl=0.5*(y1+y2)    
c
c first minimization, using 0.10 steps size in cell units
c
      dx=dx0
      dy=dy0
      Iteration=0
 101  Iteration=Iteration+1
      nx=int((x2-x1)/dx)
      ny=int((y2-y1)/dy)
c
c protect against pathologies
c
      if (nx.le.0.or.nx.ge.100.or.ny.le.0.or.ny.ge.100) then
       PRINT*, 'FITDEAD : Bad loop index ',
     &                 Iteration,x1,x2,y1,y2,nx,ny
       Ecl=SumE
       Edead=0.
       Xcl = Xcell*(Xcl-0.5*float(ncelx-1))
       Ycl = Ycell*(Ycl-0.5*float(ncely-1))
       return
      endif
      chi2_min=999999.

      do ix=1,nx
        x=x1+dx*(float(ix)-0.5) 
        do iy=1,ny 
           y=y1+dy*(float(iy)-0.5)

           Sumfr=0.
           do i=-2,2
             do j=-2,2
               if (i.ne.0.or.j.ne.0) then
                 dist_x = (float(ixdead+i)-x)*Xcell
                 dist_y = (float(iydead+j)-y)*Ycell
                 dist=sqrt(dist_x**2+dist_y**2)
                 if (dist.ge.3.0) then
                   fr(i,j)=Efrac(2.*Sume,dist)
                 else
                   fr(i,j)=Efrac2(dist_x,abs(dist_y))
                 endif
                 Sumfr=Sumfr+fr(i,j)
               endif
              enddo
           enddo

           if (Sumfr.gt.1e-9) then
           chi2=0.
           do i=-2,2
             do j=-2,2
               if (i.ne.0.or.j.ne.0) then
                 energy=SumE*fr(i,j)/Sumfr
                 chi2=chi2 + (energy-ebox_dead(i,j))**2
               endif
             enddo
           enddo

           if (chi2.le.chi2_min) then
              chi2_min=chi2
              Xcl=x
              Ycl=y
              Ecl=SumE/Sumfr
              Sumfr_save = Sumfr
              do i=-2,2
                do j=-2,2
                  fr_save(i,j)=fr(i,j)
                enddo
              enddo
           endif
           endif         ! Sumfr not egal to zero

        enddo
      enddo
c
c second iteration, using 0.04 step size in cell units
c
      if (iteration.eq.1) then
       x1=Xcl-dx
       x2=Xcl+dx
       y1=Ycl-dy
       y2=Ycl+dy
       dx=dx1
       dy=dy1
       goto 101
      endif
c
c estimated energy in dead cell
c
      dist_x = (float(ixdead)-xcl)*Xcell
      dist_y = (float(iydead)-ycl)*Ycell
      dist=sqrt(dist_x**2+dist_y**2)
      if (dist.ge.3.0) then
       fr(0,0)=Efrac(Sume,dist) 
      else
       fr(0,0)=Efrac2(dist_x,dist_y)
      endif
c
c check is something went wrong :
c
      if (dist.ge.10.) then     
        PRINT*, 
     +  ' FITDEAD : Warning, bad distance to dead cell',dist
        Ecl=0.
      endif

      if (Sumfr_save.gt.1e-9) then
      Edead=SumE/Sumfr_save*fr(0,0)
      else
      Edead=0.
      endif
c
c correct Xclu,Ycl to go to cm
c
      Xcl = Xcell*(Xcl-0.5*float(ncelx-1))
      Ycl = Ycell*(Ycl-0.5*float(ncely-1))

      return
      end
