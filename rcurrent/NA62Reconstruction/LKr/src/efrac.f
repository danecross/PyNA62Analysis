        REAL FUNCTION EFRAC(ENER,DIST)
C
C expected energy fraction in a cell at a distance DIST from
C a photon of energy ENER
C
        IMPLICIT NONE
        REAL ENER,DIST
      INTEGER NEBIN
      INTEGER NDBIN
      PARAMETER(NEBIN=5,NDBIN=120)
      REAL DMAX
      PARAMETER(DMAX=25.)
      REAL EBIN
      REAL EPROF
      COMMON/proCDE/EBIN(NEBIN),EPROF(NEBIN,NDBIN)
      real LkrEnergyScale
      real AXCorr(2),BxCorr(2),CxCorr(2),DxCorr(2),ExCorr(2),FxCorr(2)
      real AyCorr(3),ByCorr(3)
      real par(6),par2(3)
      real RmsXpar,RmsYpar
      real EcHole(4)
      real EcOut(4)
      integer Nrad_bin
      parameter(Nrad_bin=5)
      integer Ntime_bin
      parameter(Ntime_bin=7)
      real Xpar(1:Nrad_bin,1:Ntime_bin,6)
      common/corCDE/LkrEnergyScale,
     &      AxCorr,BxCorr,CxCorr,DxCorr,ExCorr,FxCorr,
     &      AyCorr,ByCorr,
     &      par,par2,
     &      RmsXpar,RmsYpar,
     &      EcHole,
     &      EcOut,
     &      Xpar
C
C --------------------------------------------------------------
C
        LOGICAL FIRST
        REAL DX,D1,D2,E1,E2,DE,DXINV
        INTEGER ID1,ID2,IE1,IE2,i
        DATA FIRST/.TRUE./
        REAL DBIN(NDBIN)
        REAL SCALE
C
        SAVE FIRST
        SAVE DX,DXINV
        SAVE DBIN
        SAVE SCALE
C
        IF (FIRST) THEN
           CALL PROFINI
           FIRST=.FALSE.
           DX=25./FLOAT(NDBIN)
           DXINV=1./DX
           do i=1,ndbin
              Dbin(i)=DX*(float(i)-0.5)
           enddo
           Scale=0.935*1.204/LkrEnergyScale
        ENDIF
C
        IF (DIST.GE.25.) THEN
c use simple exponential extrapolation for Distance above 25 cm( from 100 kev cutoff MC)
           if (DIST.le.30.) then
             EFRAC=EXP(-6.6552-0.13689*DIST)
           else
             EFRAC=EXP(-6.8839-0.12969*DIST)
           endif
           RETURN
        ENDIF
C
        IF (ENER.LE.2.) THEN
           IE1=1
           IE2=1
        ELSEIF(ENER.LE.5.) THEN
           IE1=1
           IE2=2
        ELSEIF(ENER.LE.10.) THEN
           IE1=2
           IE2=3
        ELSEIF(ENER.LE.20.) THEN
           IE1=3
           IE2=4
        ELSEIF(ENER.LE.50.) THEN
           IE1=4
           IE2=5
        ELSE
           IE1=5
           IE2=5
        ENDIF
C
        ID1=INT(DIST*DXINV-0.5)+1
        ID2=ID1+1
        IF (ID1.LE.0) ID1=1
        IF (ID2.GT.NDBIN) ID2=NDBIN
C
        IF (IE1.EQ.IE2.AND.ID1.EQ.ID2) THEN
           EFRAC=EPROF(IE1,ID1)
        ELSEIF(IE1.EQ.IE2) THEN
          D1=DBIN(ID1)
          EFRAC=( EPROF(IE1,ID1)*(D1+DX-DIST)
     &           +EPROF(IE1,ID2)*(DIST-D1) )*DXINV
        ELSEIF(ID1.EQ.ID2) THEN
          E1=EBIN(IE1)
          E2=EBIN(IE2)
          DE=E2-E1
          EFRAC=( EPROF(IE1,ID1)*(E2-ENER)
     &           +EPROF(IE2,ID1)*(ENER-E1) )/DE
        ELSE
          E1=EBIN(IE1)
          E2=EBIN(IE2)
          DE=E2-E1
          D1=DBIN(ID1)
          D2=DBIN(ID2)
          EFRAC=( EPROF(IE1,ID1)*(D2-DIST)*(E2-ENER)
     &           +EPROF(IE2,ID1)*(D2-DIST)*(ENER-E1)
     &           +EPROF(IE1,ID2)*(DIST-D1)*(E2-ENER)
     &           +EPROF(IE2,ID2)*(DIST-D1)*(ENER-E1) )*DXINV/DE
        ENDIF
        EFRAC=EFRAC*Scale
        RETURN
        END
C
C -----------------------------------------------------------------------
C
