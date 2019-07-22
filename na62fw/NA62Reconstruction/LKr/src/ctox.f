        REAL FUNCTION CTOX(XC,ENER)
C
C convert x barycenter in cell unit to real x position in cm
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
       INTEGER DTYPE
       COMMON /LKRDATATYPE/ DTYPE 
C
C --------------------------------------------------------------
C
        INTEGER IX
        REAL XC,ENER,XCC,THYP,EL,A,B,C
      REAL D,E,F
c
c
        IX=INT(XC+0.5)
        XCC=XC-FLOAT(IX)    ! from -0.5 to 0.5 in cell units
C
CC        if (xcal_corr) then
           EL=Log(max(0.001,Ener))
           A=Axcorr(1)+Axcorr(2)*El
           B=BxCorr(1)+BxCorr(2)*El
           C=CxCorr(1)+CxCorr(2)*El
           D=Dxcorr(1)+Dxcorr(2)*El
           E=ExCorr(1)+ExCorr(2)*El
           F=FxCorr(1)+FxCorr(2)*El
           xcc=A*thyp(B*xcc+D*xcc**2)+C+E*xcc+F*xcc**2
CC        endif

        if (DTYPE.eq.1) then
          CTOX=XCELL*(XCC + IX-0.5*float(ncelx-1))
        else 
          CTOX=XCELL*(XCC + IX-0.5*float(ncelx))
        endif
        RETURN
        END
C
C ------------------------------------------------------------------
C
