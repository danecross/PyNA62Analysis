        REAL FUNCTION ECOR(ENER,X,Y,SCC)
C
C
C Correct energy (cluster -> photon)
C
C correct also for energy lost in hole at R=0
C no correction for leakage  at the ouside edgde for now...
c
c returns SCC = correction applied for space charge effects ...
C
      IMPLICIT NONE
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
      REAL ENER,X,Y,RADIUS,xc,yc,xcorr,ycorr,tmp,Deep
      REAL SpaceChargeCorr,SCC
      REAL DX,DY,DD,DIST

      ECOR=ENER
      Scc=1.
CCCC      if (energy_corr) then
c
c correction for E vs x.
c determined from 100 GeV scan using first 20 bursts of each run
c and asking T(burst)<0.25 s (no space charge effect)
c
         xc= x/XCELL + 0.5*float(NCELX-1)
         xc = xc-int(xc+0.5)

c
c X modulation correction when space charge
c
CCCC         if (space_corr.and.KL_run) then
CCCC           radius=sqrt(x**2+y**2)
CCCC           xcorr=SpaceChargeCorr(radius,xc)
CCCC           Scc=1./xcorr
CCCC         else
           Deep=Par(2)
   
           if (xc.le.par(6)) then
            tmp=exp(-(xc-par(6))/par(3))
            xcorr=par(1)+4*Deep*tmp/((1+tmp)**2)
           else
            tmp=exp(-(xc-par(6))/par(5))
            xcorr=par(4)+4*(par(1)+Deep-par(4))*tmp/((1+tmp)**2)
           endif
CCCC         endif
         ECOR=ECOR/xcorr
c
c correction for E vs y
c determined as in E vs x case
c
         yc= y/YCELL +0.5*float(NCELY-1)
         yc = yc-int(yc+0.5)
         ycorr = par2(1)+par2(2)*yc**2+par2(3)*yc**4
         ECOR=ECOR/ycorr
CCCC      endif
c
c energy scale
c
CCCC      if (scale_corr) then
         ECOR=ECOR*LkrEnergyScale
CCCC      endif
c
c leakage in hole and correction for leakage outside calo
c
CCCC      if (hole_corr) then
         RADIUS=SQRT(X**2+Y**2)
         IF (RADIUS.ge.14.and.RADIUS.le.20.) THEN
            ECOR=(EcHole(1)+EcHole(2)*RADIUS+
     &            EcHole(3)*RADIUS**2+EcHole(4)*RADIUS**3)*ECOR
         ENDIF
c 
c takes into account that there is only 126 active cells in x
c
         DX=EDGE_X-ABS(X)
         DY=EDGE_Y-ABS(Y)
         DD=EDGE_D-(ABS(X)+ABS(Y))

         DIST=MIN(DX,DY)
         DIST=MIN(DIST,DD)
     
         IF (DIST.GT.0.AND.DIST.LE.11.) THEN
           ECOR=(EcOut(1)+EcOut(2)*DIST+
     &           EcOut(3)*DIST**2+EcOut(4)*DIST**3)*ECOR
         ENDIF
        
CCCC      endif
      RETURN
      END
