       SUBROUTINE CLUS1(Iteration,IER)
C
C compute cluster energies.
C Loop over cells, For each cells find cluster(s) within clustering
C radius. If more than one, apply energy sharing according to
C exected energy profile distribution
C
C clusters seeds have to have been already found (CLUS0)
C and appropriate auxiliary common blocs filled
C
C Compute energy
C COmpute x,y position using barycentre in 3x3 cells
C Compute Shower RMS of Ener. distribution on 5x5 cells
C Apply appropriate corrections
C
       IMPLICIT NONE
*===========
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
      REAL RCLUS,RCLUS2
      PARAMETER(RCLUS=11.0)
      PARAMETER(RCLUS2=RCLUS**2)
      LOGICAL PED_CORR,XCAL_CORR,YCAL_CORR,ENERGY_CORR,RMS_CORR
      LOGICAL SPACE_CORR
      LOGICAL SCALE_CORR,HOLE_CORR
      LOGICAL HV_CORR
      INTEGER ISAMP1,ISAMP2,NSAMP_PED
      LOGICAL KL_RUN
      INTEGER NPEDCL
      COMMON/rcpCDE/PED_CORR,ISAMP1,ISAMP2,energy_corr,xcal_corr,
     &   ycal_corr,rms_corr,nsamp_ped,scale_corr,hole_corr,space_corr,
     &   hv_corr,kl_run,npedcl
      REAL ClusCellT0
      real Corrke3
      common/rkeCDE/Corrke3(0:127,0:127)
C
       INTEGER IER,Iteration
       REAL ENEW(NCLMAX),XNEW(NCLMAX),YNEW(NCLMAX),SUME(NCLMAX)
       REAL WT(NCLMAX),FRAC,XLOW,YLOW
       INTEGER ICELL_CLUST(NCLMAX)
       REAL sumx5(nclmax),sumy5(nclmax),sume5(nclmax)
       INTEGER ITEST(NCLMAX)
       INTEGER I,ICLUS,ISIZE,ICL
       INTEGER IDIST,JDIST
       REAL DIST,SUMWT,EADD
       REAL EFRAC,CTOX,CTOY,ECOR,RMSXC,RMSYC
       REAL EFRAC2
       logical testcl,test33
       REAL ENERGY,TIME
       INTEGER IGAIN,ICASE,ITMAX,ITC
       integer nclust_cell,i1,i2
       real tclust_cell(10),eclust_cell(10)
       integer iclust_cell(10),jclust_cell(10)
       integer itclust_cell(10)
       integer idx,idy
       real e2pulse(10),t2pulse(10)
       integer i2gain(10)
       integer i2pulse(10)
       real delta_t_max,delta_t
       real delta_t_cut
       data delta_t_cut/20.0/
       integer Igain_cell(NCMAX)
       Real Tguess,Rmin
       real SCC
       real*8 qbene(8),qbadc(8)
C
       IER=-1
       IF (NCLUS.EQ.0) GOTO 7777
C
C zero everything at the beginning
C
       CALL wzero(NCCLUS,NCLUS)
       CALL wzero(ENEW,NCLUS)
       CALL wzero(XNEW,NCLUS)
       CALL wzero(YNEW,NCLUS)
       call wzero(sume,nclus)
       call wzero(rmsx5,nclus)
       call wzero(rmsy5,nclus)
       call wzero(sumx5,nclus)
       call wzero(sumy5,nclus)
       call wzero(sume5,nclus)
       call wzero(EARRAY,121*nclmax)
       call wzero(enoise,nclus)
       call wzero(enoise7,nclus)
       call wzero(ercal,nclus)

       call wzero(nbad_cell,nclmax)
C
C loop over cells
C
       DO I=1,calncel
c
        if (Iclus13(i).eq.0) goto 876    ! this cell cannot be in any cluster

        if (Iclus6(i).gt.Iclmax) goto 876  ! way too many clusters for this cell
c
        XLOW=xcell_lkr(ixLKR(i))
        YLOW=ycell_lkr(iyLKR(i))

        if (Iclus6(i).eq.1)  then    ! only one cluster for this cell in 41*41
c                                    ! no energy sharing at all is needed
           ICL=NCLUS6(I,1)
           ITEST(1)=0
           nclust_cell=1
C
           DIST=(XCLUS(ICL)-XLOW)**2+(YCLUS(ICL)-YLOW)**2 
           IF (DIST.LE.RCLUS2) THEN
               ITEST(1)=1
               if (iabs(ixLKR(i)-ixseed(icl)).le.1.and.
     &             iabs(iyLKR(i)-iyseed(icl)).le.1) then
                test33=.true.
               else
                test33=.false.
               endif

               if (Caldead(i).eq.0) then    ! cell is not dead
                 WT(1)=1.
                 Sumwt=1.
                 ITC=Itseed(Icl)
                 Tguess=Tseed(Icl)-ClusCellT0(ixLKR(i),iyLKR(i))
               else                         ! cell is dead
                 if (DIST.le.9.0) then
                   FRAC=EFRAC2(XLOW-XCLUS(ICL),
     &                         ABS(YCLUS(ICL)-YLOW))
                 else
                   FRAC=EFRAC(ECLUS(ICL),SQRT(DIST))
                 endif
                 WT(1)=ECLUS(ICL)*FRAC
                 SUMWT=WT(1)
               endif 
           ELSE
               goto 876   ! this cell is not in a cluster
           ENDIF

        elseif(Iclus6(i).ge.2) then  ! do some energy sharing
          testcl=.false.
          test33=.false.
          SUMWT=0.
          nclust_cell=0
          rmin=99999.
C
C loop over clusters
          DO ICLUS=1,Iclus6(i)
             ICL=Nclus6(i,Iclus)     ! cluster number
             ITEST(ICLUS)=0
             WT(ICLUS)=0.
             DIST=(XCLUS(ICL)-XLOW)**2 +
     &            (YCLUS(ICL)-YLOW)**2 
C
C is cell within clsutering radius of cluster ?
C
             IF (DIST.LE.RCLUS2) THEN
                ITEST(ICLUS)=1
                testcl=.true.
                if (iabs(ixLKR(i)-ixseed(icl)).le.1.and.
     &              iabs(iyLKR(i)-iyseed(icl)).le.1) test33=.true.
                if (dist.le.rmin) then
                   itc=itseed(Icl)
c subtract T0 of this cell from seed time
                   Tguess=Tseed(Icl)-ClusCellT0(ixLKR(i),iyLKR(i))
                   rmin=dist
                endif
                nclust_cell=nclust_cell+1
                if (nclust_cell.le.10) then
                  tclust_cell(nclust_cell)=Tseed(icl)
                  iclust_cell(nclust_cell)=Icl
                  jclust_cell(nclust_cell)=Iclus
                  Icell_clust(Iclus)=Nclust_cell
                endif
             ENDIF
C
C compute fration for energy sharing, use all clusters within 25 cm radius
C
             IF (DIST.LE.4600.) THEN
               IF (DIST.LE.9.0.AND.CALDEAD(I).EQ.1) THEN
                 FRAC=EFRAC2(XLOW-XCLUS(ICL),
     &                       ABS(YCLUS(ICL)-YLOW))
               ELSE
                 FRAC=EFRAC(ECLUS(ICL),SQRT(DIST))
               ENDIF
               WT(ICLUS)=ECLUS(ICL)*FRAC
             ENDIF
             SUMWT=SUMWT+WT(ICLUS)
          ENDDO

          if (.not.testcl) goto 876  ! this cell does not belong to a cluster

          if (sumwt.le.1e-9) then
c            PRINT*, 
c     &       'CLUS1 : Wrong sum of expected energy ',SUMWT
            goto 876           ! ignore this pathological cell
          endif
         endif
c
c is there are 2 or more clusters contributing to this cell,
c  check if they are on time or not.
c
          delta_t_max=0.
c
c protection against cases with more than 10 clusters for one cell
c
          if (nclust_cell.ge.11) nclust_cell=10

          if (nclust_cell.ge.2) then
              do i1=1,nclust_cell-1
                do i2=i1+1,nclust_cell
                    delta_t=abs(tclust_cell(i1)-tclust_cell(i2)) 
                    if (delta_t.ge.delta_t_max) delta_t_max=delta_t
                enddo
              enddo
         endif
c
c if we have clusters at different time, try to fit pulse
c  shape to extract Energies corresponding to different clusters
c  as a guess estimate, use E and T from seed cell, times 
c  shower profile in space and pulse shape
c
         if (delta_t_max.ge.delta_t_cut.and.Caldead(i).eq.0) then
           
            do iclus=1,nclust_cell
             Itclust_cell(iclus)=itseed(iclust_cell(iclus))
             Eclust_cell(iclus)=Wt(jclust_cell(iclus))
             I2pulse(iclus)=0
            enddo
            call Q(ibadc(i),qbadc)
            call Q(ibene(i),qbene)
            call PulseSep(ixlkr(i),iylkr(i),
     &                    Nsampl,qbadc,qbene,
     &                    NClust_cell,Itclust_cell,
     &                    Tclust_cell,Eclust_cell,
     &                    E2Pulse,T2Pulse,I2gain,icase)
           Iflag(i)=icase
c
c check if returned energy looks OK, if not replace with
c  shower profile in shape.
C we require the energy to be greater than -0.05 GeV
c
c also if the time returned looks crazy, we use the energy profile
C
            do iclus=1,nclust_cell
             if (E2Pulse(iclus).le.-0.050.or.
     &        abs(T2Pulse(iclus)-Tseed(iclust_cell(iclus))).ge.100) then
                 E2Pulse(iclus)=Wt(jclust_cell(iclus))
                 T2Pulse(iclus)=Tseed(iclust_cell(iclus))-
     &             ClusCellT0(ixlkr(i),iylkr(i))
                 I2pulse(iclus)=1
             endif
            enddo
c
c if we are in the seed cell of one cluster,
c  we update the time measurement for this cluster
c
            if (test33) then
             do iclus=1,nclust_cell
               if (ixlkr(i).eq.ixseed(iclust_cell(iclus)).and.
     &             iylkr(i).eq.iyseed(iclust_cell(iclus))) then
                Tseed(Iclust_cell(iclus))=T2Pulse(iclus) +
     &            ClusCellT0(ixlkr(i),iylkr(i))
               endif
             enddo
            endif
c
         else     ! no sharing in time
c
c reconstruct energy for this cell. Only in first iteration ....
c
c  For low energy cells, it maybe better to fix the time to the
c time measured by the maximum cell, once we know relative time
c between cells...
c  center digital filtering around same sample as cluster seed
c
           if ((ITeration.eq.1.or.abs(epulse(i)).le.1e-6).and.
     &         Caldead(i).eq.0) then
              call Q(ibadc(i),qbadc)
              call Q(ibene(i),qbene)
              call PulseRec(ixlkr(i),iylkr(i),
     &                      Nsampl,qbadc,qbene,
     &                      ITC,ITC,Tguess,
     &                      Energy,Time,Icase,Igain,Itmax)
             EPulse(i)=Energy
             Tpulse(i)=Time
             Iflag(i)=Icase
           Igain_cell(i)=Igain
           Endif

         Endif      ! no sharing in time
c
C loop again over clusters. Select only clusters to which cell
C is expected to contribute
C
          DO ICLUS=1,Iclus6(I)
             IF (ITEST(ICLUS).EQ.1) THEN
              ICL=Nclus6(I,ICLUS)    ! cluster number
c
c save errors flags for gain switching pathlogies and saturations
              if (caldead(i).eq.0) then 
                 icase=Iflag(i)
                 if (icase.ne.1) then
                   nbad_cell(icl)=nbad_cell(icl)+1
                   if (nbad_cell(icl).le.nbad_cell_max) then
                       ibad_cell(nbad_cell(icl),icl)=icase
                   endif
                 endif
              endif   

              idx=ixlkr(i)-ixseed(icl)+6
              idy=iylkr(i)-iyseed(icl)+6
c
c save gain switching problem in Iflarray
c
              if (idx.ge.1.and.idx.le.11.and.
     &            idy.ge.1.and.idy.le.11) then
              if (caldead(i).eq.0) then
                  if (icase.eq.1) then
                   IflArray(icl,idx,idy)=0   ! no problem
                  elseif(icase.lt.90) then
                   IflArray(icl,idx,idy)=1   ! recoverable problem
                  else
                   IflArray(icl,idx,idy)=2   ! NOT recoverable problem
                  endif
                 endif
              endif

              IF (CALDEAD(I).EQ.0) THEN
                if (Delta_t_max.lt.Delta_t_cut) then  
                  EADD=EPulse(i)*WT(ICLUS)/SUMWT         ! only space sharing
c
                  if (idx.ge.1.and.idx.le.11.and.
     &                idy.ge.1.and.idy.le.11) then
                   Tarray(icl,idx,idy)=Tpulse(i)+
     &                                 ClusCellT0(ixlkr(i),iylkr(i))
                   if (nclust_cell.ge.2.and.
     &                 Mod(IflArray(icl,idx,idy),100).lt.10)
     &                  IflArray(icl,idx,idy)=IflArray(icl,idx,idy)+10
c save gain value of cell
                  igain=igain_cell(i)
                  if (igain.gt.0) then
                     IflArray(icl,idx,idy)=IflArray(icl,idx,idy)
     &                                    +100*igain
                   endif
                  endif
c
c if cell is outside 3*3 for this shower, and has switching problem
c  then is is probably because there is a large pulse from a accidental
c and the cluster from the accidental was not found (no maximum inside
c  time window)
c   in this case, use instead shower profile
                  if (Iflag(i).ne.1) then
                   if (iabs(ixlkr(i)-ixseed(icl)).gt.1.or.
     &                 iabs(iylkr(i)-iyseed(icl)).gt.1) then
                    if (Iclus6(i).ge.2) then  
                       EADD=WT(ICLUS)       ! energy profile already exists
                    else
                       EADD=ECLUS(ICL)*EFRAC(ECLUS(ICL),sqrt(DIST))
                    endif

                    if (idx.ge.1.and.idx.le.11.and.
     &                  idy.ge.1.and.idy.le.11) then
                     IflArray(icl,idx,idy)=30
                    endif

                   endif
                  endif

                else
c
c sharing in time
c from time reconstruction of pulse
                  EADD=E2Pulse(Icell_clust(Iclus)) 

                  if (idx.ge.1.and.idx.le.11.and.
     &                idy.ge.1.and.idy.le.11) then
                    Tarray(icl,idx,idy)=T2pulse(Icell_clust(iclus))+
     &                                  ClusCellT0(ixlkr(i),iylkr(i))
                    if(mod(IflArray(icl,idx,idy),100).lt.10)
     &                 IflArray(icl,idx,idy)=IflArray(icl,idx,idy)+20
                    if(mod(IflArray(icl,idx,idy),100).lt.20)
     &                 IflArray(icl,idx,idy)=IflArray(icl,idx,idy)+10
                    if (I2pulse(Icell_clust(Iclus)).eq.1)
     &                 IflArray(icl,idx,idy)=30
c save gain range of cell
                    if (I2gain(Icell_clust(Iclus)).gt.0) then
                      IflArray(icl,idx,idy)=
     &                 Mod(IflArray(icl,idx,idy),100)+
     &                 100*I2gain(Icell_clust(Iclus))
                    endif
                  endif
c
                endif
              ELSE
c cell is dead
               EADD=WT(ICLUS)   ! add expected energy in dead cell

               if (idx.ge.1.and.idx.le.11.and.
     &             idy.ge.1.and.idy.le.11) then
                 IflArray(icl,idx,idy)=30
                 Tarray(icl,idx,idy)=0.
               endif
              ENDIF
c
              if (idx.ge.1.and.idx.le.11.and.
     &            idy.ge.1.and.idy.le.11) then
               Earray(icl,idx,idy)=Eadd
              endif

C
C add cell energy in cluster energy, with correct energy sharing
C
              ENEW(ICL)=ENEW(ICL)+EADD
              NCCLUS(ICL)=NCCLUS(ICL)+1
c
c total energy in first tow samples +  Ke3 intercalibration
c
              if (Caldead(i).eq.0) then  ! cell alive
                ENOISE(ICL)=ENOISE(ICL)+Efirst_sample(i)
                ERCAL(ICL)=ERCAL(ICL)+EADD*CORRKE3(ixlkr(i),iylkr(i))
                if (iabs(ixlkr(i)-ixseed(icl)).le.3.and.
     &              iabs(iylkr(i)-iyseed(icl)).le.3) then
                 ENOISE7(ICL)=ENOISE7(ICL)+Efirst_sample(i)
                endif
              else
                ERCAL(ICL)=ERCAL(ICL)+EADD
              endif

C compute x,y position of this clusters, if cell is in 3x3 (5x5 if
c  one cell is dead in 3x3)
c
              IF (DEAD33(ICL)) THEN
                  ISIZE=2
              ELSE
                  ISIZE=1
              ENDIF
C
              IDIST=IXLKR(I)-IXSEED(ICL)
              JDIST=IYLKR(I)-IYSEED(ICL)
C
              IF (IABS(IDIST).LE.ISIZE.AND.
     &            IABS(JDIST).LE.ISIZE) THEN
                  XNEW(ICL)=XNEW(ICL)+EADD*FLOAT(IDIST)
                  YNEW(ICL)=YNEW(ICL)+EADD*FLOAT(JDIST)
                  SUME(ICL)=SUME(ICL)+EADD
              ENDIF
C
C same for RMS of cluster if cell is in 5x5
C
              IF (IABS(IDIST).LE.2.AND.
     &            IABS(JDIST).LE.2) THEN
                  sume5(icl)=sume5(icl)+eadd
                  sumx5(icl)=sumx5(icl)+EADD*FLOAT(IDIST)
                  sumy5(icl)=sumy5(icl)+EADD*FLOAT(JDIST)
                  rmsx5(icl)=rmsx5(icl)+
     &             float(IDIST**2)*eadd
                  rmsy5(icl)=rmsy5(icl)+
     &             float(JDIST**2)*eadd
              ENDIF

             ENDIF   ! cell contributes to this cluster
          ENDDO      ! loop over cluster
 876      continue
        ENDDO        ! loop over cells
C
C end loop over cells
C
C loop over clusters, finish computation of E,x,y,RMS and apply
C  corrections
C
        DO ICLUS=1,NCLUS
         if (SUME(ICLUS).le.0.001.or.ENEW(ICLUS).le.0.001) then
c           PRINT*,
c     &      'CLUS1 : Warning cluster with no energy found !'
            ECLUS(ICLUS)=0.
         else
           XNEW(ICLUS)=XNEW(ICLUS)/SUME(ICLUS)+FLOAT(IXSEED(ICLUS))
           YNEW(ICLUS)=YNEW(ICLUS)/SUME(ICLUS)+FLOAT(IYSEED(ICLUS))
           rmsx5(iclus)=(rmsx5(iclus)/sume5(iclus))-
     &                  (sumx5(iclus)/sume5(iclus))**2
           rmsx5(iclus)=sqrt(max(0.,rmsx5(iclus)))
           rmsy5(iclus)=(rmsy5(iclus)/sume5(iclus))-
     &                  (sumy5(iclus)/sume5(iclus))**2
           rmsy5(iclus)=sqrt(max(0.,rmsy5(iclus)))
           XCLUS(ICLUS)=CTOX(XNEW(ICLUS),ENEW(ICLUS))
           YCLUS(ICLUS)=CTOY(YNEW(ICLUS),XCLUS(ICLUS),ENEW(ICLUS))
           ECLUS(ICLUS)=ECOR(ENEW(ICLUS),XCLUS(ICLUS),
     &                        YCLUS(ICLUS),SCC)
           XCLUS(ICLUS)=CTOX(XNEW(ICLUS),ECLUS(ICLUS))
           YCLUS(ICLUS)=CTOY(YNEW(ICLUS),XCLUS(ICLUS),ECLUS(ICLUS))
           ERAW(ICLUS)=ENEW(ICLUS)
           ECLUS(ICLUS)=ECOR(ENEW(ICLUS),XCLUS(ICLUS),
     &                        YCLUS(ICLUS),SCC)
           SPACECORR(ICLUS)=SCC
           RMSX5(ICLUS)=RMSXC(RMSX5(ICLUS),XCLUS(ICLUS))
           RMSY5(ICLUS)=RMSYC(RMSY5(ICLUS),YCLUS(ICLUS))
           if (ERAW(iclus).ge.0.1) then
            ERCAL(ICLUS)=ECLUS(ICLUS)/ERAW(ICLUS)*ERCAL(ICLUS)
           else
            ERCAL(ICLUS)=0.
           endif

         endif
        ENDDO
  
C
C  done ...
C
        IER=0
 7777   CONTINUE
        RETURN
        END
C
C --------------------------------------------------------------------
C
