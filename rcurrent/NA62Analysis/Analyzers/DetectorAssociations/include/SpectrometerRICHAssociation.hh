#ifndef SpectrometerRICHAssociation_HH
#define SpectrometerRICHAssociation_HH

#include <stdlib.h>
#include "Analyzer.hh"
#include "SpectrometerRICHAssociationOutput.hh"

class SpectrometerRICHAssociation : public NA62Analysis::Analyzer {

public:

  explicit SpectrometerRICHAssociation(NA62Analysis::Core::BaseAnalysis *ba);
  ~SpectrometerRICHAssociation() {}
  void InitHist();
  void StartOfRunUser();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser();
  void EndOfBurstUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}
  void PrintMirrorConstants();

private:

  Double_t CooperRingFit
  (Int_t NPoints, Double_t* XPos, Double_t* YPos, Double_t* Center,
   Double_t* Sig_Center, Double_t &Radius, Double_t &Sig_Radius);

  void     ComputeLikelihoods(Int_t iTrack, SpectrometerRICHAssociationOutput* Result);
  Double_t RICHCrossing(Double_t xc1, Double_t yc1, Double_t r1, TVector2 c2, Double_t r2);

  TString  fRevision; ///< Software revision used to process the data
  Double_t XMoveJura;
  Double_t fTimeDiffRICHCHODSigma;  ///< Time half-window for RICH-CHOD association
  Double_t fTimeDiffRICHStrawSigma; ///< Time half-window for RICH-STRAW association
  Double_t fMirrorZ; ///< Z position of mirror plane [mm]
  Double_t fChodZPos;
  Double_t fFocalLength; ///< RICH focal length
  Double_t fRefIndex; ///< Run-dependent neon refractive index
  Double_t fMirrorPanelRadius;
  Double_t fPMTDiskRadius;
  Double_t fSpacialResolution;
  Double_t fElectronRingRadius; ///< Burst-dependent electron ring radius
  Double_t fElectronRingNhits;  ///< Burst-dependent mean number of hits on electron ring
  Double_t MirrorX;
  Double_t MirrorY;
  Double_t XMove[2];
  Double_t YMove[2];
  Double_t fExtraOffsetX[2];        ///< Not used: applied at Reco stage
  Double_t fExtraOffsetY[2];        ///< Not used: applied at Reco stage
  Double_t fMirrorPosition[20][2];  ///< Mirror centre potisions (sequential numbering)
  Double_t fMirrorAlignment[25][2]; ///< Mirror alignment parameters
  Int_t    fMirrorSide[25];         ///< 0=Jura mirror, 1=Saleve mirror, -999: mirror does not exist
  Int_t    fMirrorNumber[20];       ///< Mapping of standard mirror numbers to contiguous numbers
  Bool_t   CalibrationMode;
  Bool_t   fDebug;

  std::vector<SpectrometerRICHAssociationOutput> fContainer; ///< The output information container
};

#endif
