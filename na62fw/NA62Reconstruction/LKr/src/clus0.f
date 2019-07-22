       SUBROUTINE CLUS0(IER)
C
C find clusters seed.
C first estimates of energy and position using Ecell and x,y in 3x3
c dead cell case is a little more tricky ..
C
       IMPLICIT NONE
*===========
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
      INTEGER NCMAX
      PARAMETER(NCMAX=14000)
      INTEGER ICLMAX
      PARAMETER(ICLMAX=15)
      INTEGER IXLKR,IYLKR,CALDEAD
      INTEGER*8 IBADC,IBENE
      INTEGER CALNCEL
      INTEGER NSAMPL
      REAL EPULSE,TPULSE
      INTEGER ICLUS6,NCLUS6,ICLUS13
      INTEGER IFLAG
      REAL EFIRST_SAMPLE
      INTEGER LPLKRE1,LPLKRA1
      COMMON /cadCDE/ IXLKR(NCMAX),IYLKR(NCMAX),
     &      IBENE(NCMAX),
     &      IBADC(NCMAX),CALDEAD(NCMAX),ICLUS6(NCMAX),
     &      NCLUS6(NCMAX,ICLMAX),ICLUS13(NCMAX),
     &       EPULSE(NCMAX),TPULSE(NCMAX),IFLAG(NCMAX),CALNCEL,NSAMPL,
     &      EFIRST_SAMPLE(NCMAX),LPLKRE1,LPLKRA1
      INTEGER NCLMAX
      PARAMETER(NCLMAX=25)
      REAL ECLUS,XCLUS,YCLUS,TSEED,ESEED
      REAL RMSX5,RMSY5
      INTEGER IXSEED,IYSEED,ITSEED
      LOGICAL DEAD33,DEAD55
      INTEGER NCLUS
      INTEGER NCCLUS
      INTEGER NBAD_CELL_MAX
      PARAMETER(NBAD_CELL_MAX=10)
      INTEGER NBAD_CELL
      INTEGER IBAD_CELL
      REAL EARRAY,TARRAY
      INTEGER IFLARRAY
      REAL ERAW
      INTEGER IFLAGREC
      REAL ENOISE,SPACECORR,ERCAL
      REAL ECLUSF,XCLUSF,YCLUSF
      REAL ENOISE7
      COMMON/cluCDE/NCLUS,ECLUS(NCLMAX),ERAW(NCLMAX),
     &      XCLUS(NCLMAX),YCLUS(NCLMAX),
     &      TSEED(NCLMAX),
     &      IXSEED(NCLMAX),IYSEED(NCLMAX),ITSEED(NCLMAX),
     &      RMSX5(NCLMAX),RMSY5(NCLMAX),NCCLUS(NCLMAX),
     &      DEAD33(NCLMAX),NBAD_CELL(NCLMAX),
     &      IBAD_CELL(NBAD_CELL_MAX,NCLMAX),DEAD55(NCLMAX),
     &      EARRAY(NCLMAX,11,11),TARRAY(NCLMAX,11,11),
     &      IFLARRAY(NCLMAX,11,11),IFLAGREC,ENOISE(NCLMAX),
     &      SPACECORR(NCLMAX),ERCAL(NCLMAX),
     &     ECLUSF(NCLMAX),XCLUSF(NCLMAX),YCLUSF(NCLMAX),ENOISE7(NCLMAX),
     &      ESEED(NCLMAX)
      REAL ACUT1,ACUT2
      PARAMETER(ACUT1=0.18)
      PARAMETER(ACUT2=0.8)
      REAL ClusCellT0

      INTEGER ISHIGH(NHMAX),NLEFT
      INTEGER I,IX,IY,IIX,IIY,IER,NAVE,IT,JX,JY,IPOINT,IPOINT2
      REAL SUME,DIST,AVER,XHIGH,YHIGH
      REAL CTOX,CTOY,EFRAC,ENERGY,ENONET(-1:1,-1:1)
      LOGICAL TEST,DEADCELL
      REAL TIME
      INTEGER ICASE,IGAIN,ITMAX,ISIZE
      integer ixdead,iydead
      real ebox_dead(-2:2,-2:2),x1,x2,y1,y2,Xcl,Ycl,Ecl,edead
      real corfrac
      real*8 qbene(8),qbadc(8)
C
C order by energy seeed cluster candidates
C
       CALL SORTZV(EHIGH,ISHIGH,NHIGH,1,1,0)
C
C zero CLUSTR informations
C
       NCLUS=0
       NLEFT=NHIGH
C
C loop over seed clusters candidates
C
       DO I=1,NHIGH
          IF (IUSED(ISHIGH(I)).EQ.1) GOTO 981
          IX=IXHIGH(ISHIGH(I))
          IY=IYHIGH(ISHIGH(I))
          IT=ITHIGH(ISHIGH(I))
          TEST=.TRUE.

C
C computing energy in 8 surrounding cells
C
          AVER=0.
          nave=0
          DeadCell=.false.
c
          DO IIX=MAX(0,IX-1),MIN(NCELX-1,IX+1)
           DO IIY=MAX(0,IY-1),MIN(NCELY-1,IY+1)
c
c reconstruct pulse
c
                 Ipoint=IpointLKR(IIX,IIY)
                 if (Ipoint.gt.0) then
                   if (Caldead(Ipoint).eq.1) then ! cell is dead
c
c if cell is dead, take other cells around dead cell to check
c maximum condition, to avoid to create ``fake cluster''
c
                     DeadCell=.true.
                     Enonet(iix-ix,iiy-iy)=0.
                     ixdead=iix
                     iydead=iiy
                     do jx=iix-1,iix+1
                      do jy=iiy-1,iiy+1
                       if (jx.ge.0.and.jx.lt.ncelx.and.
     &                     jy.ge.0.and.jy.lt.ncely) then
                         if (iabs(jx-ix).ge.2.or.
     &                       iabs(jy-iy).ge.2) then
                           Ipoint2=IpointLkr(jx,jy)
                           if (Ipoint2.gt.0) then
                            if (Caldead(ipoint2).eq.0) then
                             call Q(ibadc(ipoint2),qbadc)
                             call Q(ibene(ipoint2),qbene)
                             call PulseRec(jx,jy,Nsampl,
     &                         qbadc,
     &                         qbene,
     &                         IT-1,IT+1,0.,
     &                         Energy,Time,icase,igain,itmax)
                             IF (Energy.Gt.EHIGH(ISHIGH(I)))  then
                                TEST=.FALSE.
                                goto 981   ! try directly new seed
                             Endif
                            else
                             Energy=0.
                            endif
                           else
                            Energy=0.
                           endif
                           ebox_dead(jx-iix,jy-iiy)=Energy
                           aver=aver+Energy
                           nave=nave+1
                         endif
                       endif
                      enddo           ! loop over jy
                     enddo            ! loop over jx
                           
                   else     ! cell is not dead
                     if (ipoint.gt.0) then
                       if (ibadc(ipoint).gt.0) then
                         call Q(ibadc(ipoint),qbadc)
                         call Q(ibene(ipoint),qbene)
                         call PulseRec(iix,iiy,Nsampl,
     &                    qbadc,qbene,
     &                    IT-1,IT+1,0.,
     &                    Energy,Time,icase,igain,itmax)
                         Enonet(iix-ix,iiy-iy)=Energy
                       else
                         write (*,*) "[Clus0] WARNING: ibadc(",
     &                    ipoint,")= ", ibadc(ipoint)
                         Enonet(iix-ix,iiy-iy)=0
                       endif
                     else
                       write (*,*) "[Clus0] WARNING: ipoint= ", ipoint
                       Enonet(iix-ix,iiy-iy)=0
                     endif
c
c apply cuts to find maximum
                     if (iix.ne.ix.or.iiy.ne.iy) then
C
C require Eseed > E all 8 surrounding cells
C
                      IF (Energy.Gt.EHIGH(ISHIGH(I))) then
                       TEST=.FALSE.
                       goto 981  ! try directly new seed
                      Endif
                      aver=aver+Energy
                      nave=nave+1
                     endif
                   Endif        ! cell dead/alive
                 Endif          ! cell is readout
           ENDDO                ! loop over IIY=IY-1,IY+1
          ENDDO                 ! loop over IIX=IX-1,IX+1
          if (nave.ge.1) aver=aver/float(nave)
C
C compare Eseed to average 8 surrounding cells, apply cut
C
          if ((aver+acut1+aver*acut2).ge.ehigh(ishigh(i))) test=.false.
c
c do we have a cluster ?
c
          IF (TEST) THEN
C
C new cluster found !
C
            NCLUS=NCLUS+1
            IF (NCLUS.LE.NCLMAX) THEN
             ECLUS(NCLUS)=EHIGH(ISHIGH(I))
             XCLUS(NCLUS)=0.
             YCLUS(NCLUS)=0.
             NCCLUS(NCLUS)=0
             IXSEED(NCLUS)=IX
             IYSEED(NCLUS)=IY
             ITSEED(NCLUS)=IT
c
c add t0 to time from seed cell
c
             ESEED(NCLUS)=EHIGH(ISHIGH(I))
             TSEED(NCLUS)=THIGH(ISHIGH(I))+ClusCellT0(ix,iy)
             DEAD33(NCLUS)=DEADCELL
             DEAD55(NCLUS)=.false.
c
c check if there is a dead cell in 5*5 around seed
c
             DO IIX=MAX(0,IX-2),MIN(NCELX-1,IX+2)
              DO IIY=MAX(0,IY-2),MIN(NCELY-1,IY+2)
                Ipoint=IpointLkr(iix,iiy)
                if (Ipoint.ge.1) then
                  if (caldead(ipoint).eq.1) then
                       DEAD55(NCLUS)=.true.
                  endif
                 endif
              ENDDO
             ENDDO
c
             SUME=0.
C
C first x,y estimates using simple 3x3  (5x5 if there is a dead cell)
C
             IF (DEAD33(NCLUS)) THEN
              ISIZE=2
             ELSE
              ISIZE=1
             ENDIF
c
c if dead cell is adjacent to maximum, try to fit position
c using shower profile, instead of doing barycentre.
c
             if (dead33(nclus).and.(ixdead.eq.ix.or.iydead.eq.iy)) then
c
c x and y ranges to fit position : within the seed cell or the dead cell
c   in cell units
                x1=float(min(ixdead,ix))-0.5
                x2=float(max(ixdead,ix))+0.5
                y1=float(min(iydead,iy))-0.5
                y2=float(max(iydead,iy))+0.5
c
c ebox_dead is energy in 3*3 cells around dead cell
c   part of it is alread filled. Add missing part
c
                ebox_dead(0,0)=0.
                do iix=ixdead-2,ixdead+2
                 do iiy=iydead-2,iydead+2

                  if (iix.ge.0.and.iix.lt.ncelx.and.
     &                iiy.ge.0.and.iiy.lt.ncely) then
                   if (iabs(iix-ix).le.1.and.iabs(iiy-iy).le.1) then
                     ebox_dead(iix-ixdead,iiy-iydead)=
     &                   Enonet(iix-ix,iiy-iy)
                   elseif(iabs(iix-ixdead).ge.2.
     &                   or.iabs(iiy-iydead).ge.2) then
                     Ipoint2=IpointLkr(iix,iiy)
                     if (Ipoint2.gt.0.) then
                      if (Caldead(Ipoint2).eq.0) then
                       call Q(ibadc(ipoint2),qbadc)
                       call Q(ibene(ipoint2),qbene)
                       call PulseRec(iix,iiy,Nsampl,
     &                     qbadc,qbene,
     &                     IT-1,IT+1,0.,
     &                     Energy,Time,icase,igain,itmax)
                       ebox_dead(iix-ixdead,iiy-iydead)=Energy
                      else
                       ebox_dead(iix-ixdead,iiy-iydead)=0.
                      endif
                     else
                      ebox_dead(iix-ixdead,iiy-iydead)=0.
                     endif
                   endif
                  else
                   ebox_dead(iix-ixdead,iiy-iydead)=0.
                  endif

                 enddo
                enddo
c
c fit cluster position, Xclus and Yclus are in cm
c
                call FitDead(ebox_dead,ixdead,iydead,
     &                      x1,x2,y1,y2,Xcl,Ycl,Ecl,Edead)
                XCLUS(NCLUS)=Xcl
                YCLUS(NCLUS)=Ycl
                ECLUS(NCLUS)=Ecl
c
             else   ! no dead cell adjacent to maximum

              DO IIX=MAX(0,IX-ISIZE),MIN(NCELX-1,IX+ISIZE)
               DO IIY=MAX(0,IY-ISIZE),MIN(NCELY-1,IY+ISIZE)
c
c in cells outside 3x3 (if we need them) we have to reconstruct also energy
                if (iabs(iix-ix).ge.2.or.iabs(iiy-iy).ge.2) then
                 Ipoint=IpointLKR(iix,iiy)
                 if (Ipoint.gt.0) then
                  if (Caldead(ipoint).eq.0) then
                     call Q(ibadc(ipoint),qbadc)
                     call Q(ibene(ipoint),qbene)
                     call PulseRec(iix,iiy,Nsampl,
     &                qbadc,qbene,
     &                IT-1,IT+1,0.,
     &                Energy,Time,icase,igain,itmax)
                  else
                    Energy=0.
                  endif
                 else
                  Energy=0.
                 endif
                else
                 Energy=Enonet(iix-ix,iiy-iy)
                endif
                XCLUS(NCLUS)=XCLUS(NCLUS)+Energy*FLOAT(IIX)
                YCLUS(NCLUS)=YCLUS(NCLUS)+Energy*FLOAT(IIY)
                SUME=SUME+Energy
c
c remove cells in nonet from possible cluster seeds if same time slice
c
                IF (IABS(IIX-IX).LE.1.AND.IABS(IIY-IY).LE.1) THEN
                 IF (Ipoint.GT.0) THEN
                 IF (INDHIGH(IIX,IIY).GT.0) THEN
                  IF (IABS(ITHIGH(INDHIGH(IIX,IIY))-IT).LE.1.0.and.
     &                IUSED(INDHIGH(IIX,IIY)).eq.0) THEN
                   NLEFT=NLEFT-1
                   IUSED(INDHIGH(IIX,IIY))=1
                  ENDIF
                 ENDIF
                 ENDIF
                ENDIF

               ENDDO    !  loop over cells around seed
              ENDDO     !
 
              if (SUME.le.0.001) then
c                PRINT*,
c     &            'CLUS0 : Warning cluster with no energy, discarded !'
                 NCLUS=NCLUS-1
                 goto 431
              else

                XCLUS(NCLUS)=XCLUS(NCLUS)/SUME
                YCLUS(NCLUS)=YCLUS(NCLUS)/SUME
c
c apply corrections
                XCLUS(NCLUS)=CTOX(XCLUS(NCLUS),SUME)
                YCLUS(NCLUS)=CTOY(YCLUS(NCLUS),XCLUS(NCLUS),SUME)
c 
c compute distance to seed cell
c
                XHIGH=Xcell_lkr(ix)
                YHIGH=Ycell_lkr(iy)
                DIST=SQRT((XCLUS(NCLUS)-XHIGH)**2 +
     &                    (YCLUS(NCLUS)-YHIGH)**2 )
C
C fisrt energy estimate using seed cell content
C
                CORFRAC=EFRAC(SUME/0.67,DIST)
                if (CORFRAC.gt.0.) then
                 ECLUS(NCLUS)=ECLUS(NCLUS)/CORFRAC
                else
c                 PRINT*, ' CLUS0 : Wrong fraction',SUME,DIST,
c     &               ' discarded'
                 NCLUS=NCLUS-1
                 goto 431
                endif
              endif           ! SumE > 0
             endif            ! dead cell adjacent to maximum

c
c flag all cells that are within 41*41 of this seed
c
             DO IIX=MAX(0,IX-20),MIN(NCELX-1,IX+20)
              DO IIY=MAX(0,IY-20),MIN(NCELY-1,IY+20)
                 Ipoint=IpointLkr(iix,iiy)
                 If (Ipoint.gt.0) then
                    Iclus6(Ipoint)=Iclus6(Ipoint)+1
                    if (Iclus6(Ipoint).le.iclmax) then
                     Nclus6(Ipoint,Iclus6(Ipoint))=NCLUS
                    endif
                    if (iabs(iix-ix).le.6.and.iabs(iiy-iy).le.6) then
                     Iclus13(Ipoint)=Iclus13(Ipoint)+1
                  endif
                Endif
              ENDDO
             ENDDO
 431        continue

           ELSE
             PRINT*, ' CLUS0 : Too many clusters ',NCLUS
             IER=1
             NCLUS=MIN(NCLUS,NCLMAX)
             RETURN
           ENDIF                       ! too many clusters
          ENDIF                        ! new cluster found
          IF (NLEFT.EQ.0) GOTO 982

 981      CONTINUE
       ENDDO                    ! loop over high energy cells
C
C end loop over seed candidates
C
 982   CONTINUE
       NCLUS=MIN(NCLUS,NCLMAX)
       IER=0
       RETURN
       END
