        REAL FUNCTION EFRAC2(DX,DY)

        implicit none
        REAL DX,DY
      INTEGER ND2BIN,NP2BIN
      PARAMETER(ND2BIN=30,NP2BIN=5)
      REAL EPROF2
      COMMON/pro2CDE/EPROF2(NP2BIN,ND2BIN)
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
        REAL DELTAX,PHI,DIST
        DATA FIRST/.TRUE./
        REAL D1,D2
        INTEGER ID1,ID2,I2
        SAVE FIRST
        SAVE DELTAX
        REAL SCALE
        SAVE SCALE
        REAL PHI1,PHI2,PHI3,PHI4,PI
        PARAMETER(PI=3.14159)
        PARAMETER(PHI1=PI/8.)
        PARAMETER(PHI2=3.*PI/8.)
        PARAMETER(PHI3=5.*PI/8.)
        parameter(phi4=7.*pi/8.)

        IF (FIRST) THEN
          CALL PROFINI2
          DELTAX=3.0/FLOAT(ND2BIN)
          SCALE=1.204/LkrEnergyScale
          FIRST=.FALSE.
        ENDIF
C
        PHI=ATAN2(ABS(DY),DX)
        IF (PHI.LE.PHI1) THEN
            I2=1
        ELSEIF(PHI.LE.PHI2) THEN
            I2=2
        ELSEIF(PHI.LE.PHI3) THEN
            I2=3
        ELSEIF(PHI.LE.PHI4) THEN
            I2=4
        ELSE
            I2=5
        ENDIF
        DIST=SQRT(DX**2+DY**2)
C
        ID1 = INT((DIST-0.5*DELTAX)/DELTAX)+1
        ID2  =INT((DIST+0.5*DELTAX)/DELTAX)+1
        IF (ID1.LE.0) ID1=1
        IF (ID2.GT.ND2BIN) ID2=ND2BIN
        IF (ID1.GT.ND2BIN) ID1=ND2BIN
C
        IF (ID1.NE.ID2) THEN
           D1=DELTAX*(FLOAT(ID1)-0.5)
           D2=DELTAX*(FLOAT(ID2)-0.5)
           EFRAC2=EPROF2(I2,ID2)*(DIST-D1)+EPROF2(I2,ID1)*(D2-DIST)
           EFRAC2=EFRAC2/(D2-D1)
        ELSE
           EFRAC2=EPROF2(I2,ID1)
        ENDIF
        EFRAC2=EFRAC2*SCALE
        RETURN
        END
C
C ------------------------------------------------------------------------
C
