        REAL FUNCTION CTOY(YC,XPOS,ENER)
C
C convert y barycenter in cell unit to real y position in cm
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
        INTEGER IY,IX
        REAL YC,ENER,YCC,THYP,XPOS,DXC,XC,A,B
C
        XC= XPOS/XCELL + 0.5*float(NCELX-1)
        IX = INT(XC+0.5)
        DXC = XC - FLOAT(IX)
C
        IY=INT(YC+0.5)
        YCC=YC-FLOAT(IY)
CCCC        if (ycal_corr) then
          if (abs(dxc).le.0.15) then
           A=AyCorr(1)
           B=ByCorr(1)
          elseif(abs(dxc).le.0.30) then
           A=AyCorr(2)
           B=ByCorr(2)
          else
           A=AyCorr(3)
           B=ByCorr(3)
          endif
          ycc=A*thyp(B*ycc)
CCCC        endif
        CTOY=YCELL*(YCC + IY-0.5*float(ncely-1))
        RETURN
        END
C
C --------------------------------------------------------------------
C
