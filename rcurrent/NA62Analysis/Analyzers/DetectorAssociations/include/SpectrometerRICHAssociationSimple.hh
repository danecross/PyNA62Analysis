// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-07-27
//
// ---------------------------------------------------------------

#ifndef SpectrometerRICHAssociationSimple_HH
#define SpectrometerRICHAssociationSimple_HH

#include "Analyzer.hh"
#include "SpectrometerRICHAssociationOutputSimple.hh"
#include "TF1.h"

class SpectrometerRICHAssociationSimple : public NA62Analysis::Analyzer {

public:
  explicit SpectrometerRICHAssociationSimple(NA62Analysis::Core::BaseAnalysis *ba);
  ~SpectrometerRICHAssociationSimple();
  void InitHist() {}
  void InitOutput();
  void DefineMCSimple() {}
  void Process(int iEvent);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser() {}
  void PostProcess() {}
  void DrawPlot() {}

private:
  // RICH paremeters
  Double_t fFocalLength;    ///< RICH focal length
  Double_t fRefIndex;       ///< RICH radiator refractive index
  Double_t fBetaThreshold;  ///< Cherenkov threshold velocity
  Double_t fThetaCMax;      ///< Maximum Cherenkov angle
  Double_t fMomThresholdEl; ///< Cherenkov threshold momentum for electrons
  Double_t fMomThresholdMu; ///< Cherenkov threshold momentum for muons
  Double_t fMomThresholdPi; ///< Cherenkov threshold momentum for pions

  // Search parameters
  Double_t fMaxMeanNHits; ///< Expected mean number of hits for a ring in acceptance for beta=1
  Double_t fRadiusMargin; ///< Half-width of the ring where RICH hits are searched, around the expected ring in a certain mass hypothesis

  TF1 *fPoisson;

  std::vector<SpectrometerRICHAssociationOutputSimple> fContainer;
};

#endif
