      SUBROUTINE SAVEOUTPUT(IER)
      IMPLICIT NONE
      REAL XNOISE
      PARAMETER(XNOISE=0.008)
      INTEGER NCLMAX
      PARAMETER(NCLMAX=25)
      INTEGER NCELL77
      PARAMETER(NCELL77=50)
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
       integer ndead_max
       parameter(ndead_max=900)
       integer ndead
       real xdead,ydead
       common/deaCDE/ndead,xdead(ndead_max),ydead(ndead_max)
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
       REAL ClusCellT0
       COMMON/ct0CDE/ClusCellT0(0:127,0:127)
       REAL ClockPeriod
       COMMON/clkCDE/CLOCKPERIOD
       INTEGER IER,ICLUS
       REAL ERROR
       integer i,ierr,idead
       real dmin,dist
       integer idmin
       integer ix,iy,ixy_save,Ncell_save
       integer ix_cell,iy_cell
       parameter(ixy_save=3)   ! save 7*7 cells in RCEL bank
       real cory
       real Etot_rec,e77
       integer nchi2             ! for shower shape chi**2 computation
       real Ecell,Sige,Chi2,frac,Efrac,Efrac2
c      integer RDOFFSET, LPLKR, tslow, memaddr
      real    oddevt
c      INTEGER ICELL
      REAL TIMESTAMP
      COMMON /GLOBALEV/ TIMESTAMP
      REAL NCLUSTERS,ETOTAL,IRECFLAG
      INTEGER IDCLUS,NCELLS,IDSEEDCELL,STATFLAG,N77
      INTEGER IDCELL77,IFLCELL77
      REAL ENERGY,EENERGY,XPOS,YPOS,RMSX,RMSY,TIME,CHI2RMS,
     &     TLATCELL,DDEADCELL,UENERGY,E2SAMPALL,E77CLUS,
     &     SPACHARCORR,ECORRKE3,ENOIS77,UTIME
      REAL ECELL77,TCELL77,ESEEDCELL
      COMMON /outCDE/ NCLUSTERS,ETOTAL,IRECFLAG,IDCLUS(NCLMAX),
     &                NCELLS(NCLMAX),IDSEEDCELL(NCLMAX),
     &                ENERGY(NCLMAX),EENERGY(NCLMAX),
     &                STATFLAG(NCLMAX),XPOS(NCLMAX),YPOS(NCLMAX),
     &                RMSX(NCLMAX),RMSY(NCLMAX),TIME(NCLMAX),
     &                CHI2RMS(NCLMAX),TLATCELL(NCLMAX),
     &                DDEADCELL(NCLMAX),UENERGY(NCLMAX),
     &                E2SAMPALL(NCLMAX),E77CLUS(NCLMAX),
     &                SPACHARCORR(NCLMAX),ECORRKE3(NCLMAX),
     &                ENOIS77(NCLMAX),UTIME(NCLMAX),N77(NCLMAX),
     &                IDCELL77(NCLMAX,NCELL77),
     &                IFLCELL77(NCLMAX,NCELL77),
     &                ECELL77(NCLMAX,NCELL77),
     &                TCELL77(NCLMAX,NCELL77), 
     &                ESEEDCELL(NCLMAX)
C-----------------------------------------
       NCLUSTERS = FLOAT(NCLUS)
       ETOTAL = 0.
       Etot_rec=0. 
       IRECFLAG = FLOAT(IflagRec)
C-----------------------------------------
c  the time of the clusters is corrected
c  for odd/even event (if Super Event data!!)
c  ** LKR expert must check Read-out offset **
         oddevt=0.
c        RDOFFSET=1
c        IF (INDEX_SLKR.NE.0) THEN
c           tslow = TIMESTAMP ! timestamp least significant 16 bits
c           memaddr=tslow-RDOFFSET
c           IF(IAND(memaddr,1).EQ.1) oddevt=1.
c        ENDIF
c---------------------------------------------------
C
C loop over cluster
C
       DO ICLUS=1,NCLUS
c  put result of full computation also in Eclusf variables
          Eclusf(Iclus)=Eclus(Iclus)
          Xclusf(Iclus)=Xclus(Iclus)
          Yclusf(Iclus)=Yclus(Iclus)
C
         Etot_rec = Etot_rec + Eclus(iclus)
         ERROR=SQRT((0.035)**2*ECLUS(ICLUS) +
     &              NCCLUS(ICLUS)*XNOISE**2)
C
C fill content
C
         IDCLUS(ICLUS)=FLOAT(ICLUS)
         NCELLS(ICLUS)=FLOAT(NCCLUS(ICLUS))
         IDSEEDCELL(ICLUS)=1000.*IXSEED(ICLUS)+1.*IYSEED(ICLUS)
         ENERGY(ICLUS)=ECLUS(ICLUS)
         EENERGY(ICLUS)=ERROR
         ESEEDCELL(ICLUS)=ESEED(ICLUS)
c
c save information on pulse reconstruction problems
c
         STATFLAG(ICLUS)=0
         if (nbad_cell(iclus).ge.1) then
             ierr=0
             do i=1,min(nbad_cell(iclus),nbad_cell_max)
                if (ibad_cell(i,iclus).le.10) then
                   ierr=ibset(ierr,0)
                elseif(ibad_cell(i,iclus).le.90) then
                   ierr=ibset(ierr,1)
                elseif(ibad_cell(i,iclus).eq.98) then
                   ierr=ibset(ierr,2)
                elseif(ibad_cell(i,iclus).eq.99) then
                   ierr=ibset(ierr,3)
                elseif(ibad_cell(i,iclus).eq.97) then
                   ierr=ibset(ierr,4)
                endif
             enddo
             STATFLAG(ICLUS)=ierr
          endif
c
         XPOS(ICLUS)=XCLUS(ICLUS)
         YPOS(ICLUS)=YCLUS(ICLUS)
         RMSX(ICLUS)=RMSX5(ICLUS)
         RMSY(ICLUS)=RMSY5(ICLUS)
         TIME(ICLUS)=TSEED(ICLUS)
c
c  correct lateral cell time for vertical crosstalk effect
c
         TIME(ICLUS)=TIME(ICLUS)-cory(YCLUS(ICLUS))*ClockPeriod/25.
c
c  look at most energetic lateral cell and save its time
c
         if (mod(IflArraY(Iclus,5,6),100).ge.30.and.
     &       mod(IflArray(Iclus,7,6),100).ge.30) then
            TLATCELL(ICLUS)=0.    ! two "dead" cells
         elseif(mod(IflArray(Iclus,5,6),100).ge.30) then
            TLATCELL(ICLUS)=Tarray(Iclus,7,6)
         elseif(mod(IflArray(Iclus,7,6),100).ge.30) then
            TLATCELL(ICLUS)=Tarray(Iclus,5,6)
             elseif(Earray(Iclus,5,6).ge.Earray(Iclus,7,6)) then
            TLATCELL(ICLUS)=Tarray(Iclus,5,6)
         else
            TLATCELL(ICLUS)=Tarray(Iclus,7,6)
         endif
c
c  correct lateral cell time for vertical crosstalk effect
c
         TLATCELL(ICLUS)=TLATCELL(ICLUS)
     +                   -cory(YCLUS(ICLUS))*ClockPeriod/25.
c
c find closest dead cell
c
           TIME(ICLUS)=TIME(ICLUS)+ClockPeriod*oddevt
           TLATCELL(ICLUS)=TLATCELL(ICLUS)+ClockPeriod*oddevt
         dmin=99999.
         idmin=-1
         do idead=1,ndead
CCC           if (idead.eq.366) print*,xdead(idead),ydead(idead)
CCC           if (idead.eq.367) print*,xdead(idead),ydead(idead)
CCC           if (idead.eq.368) print*,xdead(idead),ydead(idead)
           dist=sqrt((xdead(idead)-xclus(iclus))**2+
     &               (ydead(idead)-yclus(iclus))**2)
           if (dist.le.dmin) then
             dmin=dist
             idmin=idead
           endif
         enddo
c
CC         if (ndead.gt.0) print*,dmin,xdead(idmin),
CC     &                          ydead(idmin),idmin,ndead
         DDEADCELL(ICLUS)=dmin 
c
c save also uncorrected cluster energy
c
         UENERGY(ICLUS)=ERAW(iclus)
c
c energy in average of first two time samples
c   
         E2SAMPALL(ICLUS)=ENOISE(iclus)
C
c value of the correction used for the x modulation in
c  case of space charge correction
c
         SPACHARCORR(ICLUS)=SpaceCorr(iclus)
c
c cluster energy corrected by Ke3 correction factor
c
         ECORRKE3(ICLUS)=ERCAL(iclus)
c
c time without t0 correction
c
         UTIME(ICLUS)=TIME(ICLUS)
     &              -ClusCellT0(ixseed(iclus),iyseed(iclus))
c
c energy in first two time samples over 7*7 cells
c
         ENOIS77(ICLUS)=ENOISE7(iclus)
c
c "raw" energy in 7*7 cells
c
         E77=0.
         Ncell_save=0
         Nchi2=0           ! to compute chi**2 of shower shape
         Chi2=0.
         do ix=-ixy_save,ixy_save
          do iy=-ixy_save,ixy_save
             ix_cell=IXSEED(iclus)+ix
             iy_cell=IYSEED(iclus)+iy
c
c save only cells with number in the range 0 to 127
c
           if (ix_cell.ge.0.and.iy_cell.ge.0.and.
     &         ix_cell.lt.ncelx.and.iy_cell.lt.ncely) then 
             if (ix.ge.-3.and.ix.le.3.and.
     &           iy.ge.-3.and.iy.le.3) then
               E77=E77+Earray(iclus,ix+6,iy+6)
c
c compute distance (cell-cluster) 
               dist=(xclus(iclus)-xcell_lkr(ix_cell))**2 +
     &              (yclus(iclus)-ycell_lkr(iy_cell))**2
c
c get energy profile 
               if (dist.le.9.) then
                  frac=Efrac2(xclus(iclus)-xcell_lkr(ix_cell),
     &                       abs(yclus(iclus)-ycell_lkr(iy_cell)))
               else
                  frac=Efrac(Eclus(iclus),sqrt(dist))
               endif
c 
c if fraction above 0.02 then consider this cell for the chi**2
               if (frac.ge.0.02) then
                  Ecell=Earray(iclus,ix+6,iy+6)      ! measured energy
                  if (Ecell.gt.0.) then
                  Nchi2=Nchi2+1
                  Sige=Ecell*
     &             sqrt((0.032)**2/Ecell+(0.12/Ecell)**2+(0.001)**2)
                  if (Sige.le.1e-6) Sige=9999.
                  chi2=chi2 + 
     &            ((Ecell-Eclus(iclus)*Frac)/Sige)**2
                  endif
               endif                        ! fraction above 0.02
             endif                          ! cell in 7*7
             IDCELL77(iclus,Ncell_save)= 1000*ix_cell+iy_cell
             IFLCELL77(iclus,Ncell_save)=IflArray(iclus,ix+6,iy+6)
             ECELL77(iclus,Ncell_save)=Earray(iclus,ix+6,iy+6)
             TCELL77(iclus,Ncell_save)=Tarray(iclus,ix+6,iy+6)
           Ncell_save=Ncell_save+1
           endif
          enddo
         enddo
c
c normalize chi2 to ndof
         if (nchi2.ge.1) then
          chi2=chi2/float(nchi2)
         else
          chi2=1000.
         endif
         if (chi2.gt.1000.) chi2=1000.
         N77(ICLUS)=Ncell_save
         E77CLUS(ICLUS)=E77
         CHI2RMS(ICLUS) = chi2   ! save chi**2 test of shower shape
C
       ENDDO
       ETOTAL = Etot_rec
C
CCC DEBUG
CCCC       PRINT*,'DEBUG OUTPUT'
CCCC       PRINT*,NCLUSTERS,ETOTAL,IRECFLAG
CCCC       DO ICLUS=1,NCLUS
CCCC         PRINT*,'CLUSTER',IDCLUS(ICLUS)
CCCC         PRINT*,'Number of cells',NCELLS(ICLUS)
CCCC         PRINT*,'Id seed cell',IDSEEDCELL(ICLUS)
CCCC         PRINT*,'Energy',ENERGY(ICLUS)
CCCC         PRINT*,'Energy error',EENERGY(ICLUS)
CCCC         PRINT*,'Status',STATFLAG(ICLUS)
CCCC         PRINT*,'X',XPOS(ICLUS)
CCCC         PRINT*,'Y',YPOS(ICLUS)
CCCC         PRINT*,'RMSX',RMSX(ICLUS)
CCCC         PRINT*,'RMSY',RMSY(ICLUS)
CCCC         PRINT*,'TIME',TIME(ICLUS)
CCCC         PRINT*,'Chi2 rms',CHI2RMS(ICLUS)
CCCC         PRINT*,'Time lateral cell',TLATCELL(ICLUS)
CCCC         PRINT*,'Distance dead cell',DDEADCELL(ICLUS)
CCCC         PRINT*,'Uncorrected energy',UENERGY(ICLUS)
CCCC         PRINT*,'Noise',E2SAMPALL(ICLUS)
CCCC         PRINT*,'Energy 7x7',E77CLUS(ICLUS)
CCCC         PRINT*,'Space charge correction',SPACHARCORR(ICLUS)
CCCC         PRINT*,'Energy Ke3 corrected',ECORRKE3(ICLUS)
CCCC         PRINT*,'Uncorrected time',UTIME(ICLUS)
CCCC         PRINT*,'E noise 7x7',ENOIS77(ICLUS)
CCCC         PRINT*,"N cell 7x7",N77(ICLUS)
CCCC         DO ICELL=1,N77(ICLUS)
CCCC           PRINT*,IDCELL77(ICLUS,ICELL),IFLCELL77(ICLUS,ICELL),
CCCC     &            ECELL77(ICLUS,ICELL),TCELL77(ICLUS,ICELL)
CCCC        
CCCC         ENDDO
CCCC       ENDDO
       IER=0
 7777  CONTINUE
       RETURN
       END
