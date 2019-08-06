#ifndef SpectrometerRICHAssociationSingleRing_HH
#define SpectrometerRICHAssociationSingleRing_HH

#include "Analyzer.hh"
#include "GeometricAcceptance.hh"
#include "SpectrometerRICHAssociationOutputSimple.hh"
#include "SpectrometerRICHAssociationOutputSingleRing.hh"
#include "SpectrometerMUV3AssociationOutput.hh"
#include "SpectrometerLKrAssociationOutput.hh"
#include "SpectrometerCHODAssociationOutput.hh"
#include "SpectrometerRICHAssociationOutput.hh"
#include "TF1.h"
#include "RICHSingleRingTrkSeededFit.hh"

class SpectrometerRICHAssociationSingleRing : public NA62Analysis::Analyzer {

public:
  explicit SpectrometerRICHAssociationSingleRing(NA62Analysis::Core::BaseAnalysis *ba);
  ~SpectrometerRICHAssociationSingleRing();
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(int iEvent);
  void StartOfRunUser();
  void StartOfBurstUser();
  void EndOfBurstUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}
  Int_t TrackSingleRingMatching(Double_t,TRecoRICHEvent*,TRecoSpectrometerCandidate*);

  Double_t fMirrorPos[25][6][2];
  Double_t fMirrorOff[25][2];
  int LoadMirrorParameters();

  //Int_t MirrorSurface(Double_t,Double_t,Double_t,Bool_t);
  Int_t MirrorSurface(Double_t,Double_t);
protected:

  // RICH parameters
  Double_t fRICHMirrorZPos; ///< Z position of mirror plane [mm]
  Double_t fFocalLength;    ///< RICH focal length
  Double_t fRefIndex;       ///< RICH radiator refractive index

  TF1 *fPoisson;
  vector<SpectrometerRICHAssociationOutputSingleRing> fContainer;

  Double_t fSingleRingTime;
  Double_t fSingleRingDist;
  Double_t fSingleRingDTime;
  Double_t fSingleRingRadius;
  Double_t fSingleRingCenterX;
  Double_t fSingleRingCenterY;
  Double_t fSingleRingChi2;
  Int_t fSingleRingNHits;
  RICHSingleRingTrkSeededFit *fSingleRing;

  Double_t fMirrorPosition[20][2];  ///< Mirror centre potisions (sequential numbering)
  Double_t fMirrorAlignment[25][2]; ///< Mirror alignment parameters
  Int_t    fMirrorSide[25];   ///< 0=Jura mirror, 1=Saleve mirror, -999: mirror does not exist
  Int_t    fMirrorNumber[20]; ///< Mapping of standard mirror numbers to contiguous numbers
  Double_t fElectronRingRadius;
    
};

#endif
