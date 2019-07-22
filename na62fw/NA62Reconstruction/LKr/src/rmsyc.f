        REAL FUNCTION RMSYC(RMS,Y)
C
C correct RMS of cell for position dependence
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
        REAL RMS,Y,YC
        INTEGER IYC
C
CCCC        if (rms_corr) then
         IYC=INT(0.5*FLOAT(NCELY)+Y/YCELL)
         YC=Y-YCELL*(FLOAT(IYC)-0.5*FLOAT(NCELY-1))
         RMSYC=RMS/(1.+RmsYPar*YC**2)
CCCC        else
CCCC         RMSYC=RMS
CCCC        endif
        RETURN
        END
C
C ---------------------------------------------------------------------
C
