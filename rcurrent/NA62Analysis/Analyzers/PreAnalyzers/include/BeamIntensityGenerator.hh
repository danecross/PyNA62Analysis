// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2019-01-17
// ---------------------------------------------------------------

#ifndef BEAMINTENSITYGENERATOR_HH
#define BEAMINTENSITYGENERATOR_HH

// This analyzer can only be run as a pre-analyzer
#pragma pre-analyzer

#include "Analyzer.hh"
#include "TRandom2.h"

class BeamIntensityGenerator : public NA62Analysis::Analyzer {

public:

  explicit BeamIntensityGenerator(NA62Analysis::Core::BaseAnalysis *ba);
  ~BeamIntensityGenerator();
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void PostProcess() {}
  void StartOfBurstUser();
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void DrawPlot() {}

private:

  Double_t fMinBeamIntensity; ///< Min beam intensity [MHz]
  Double_t fMaxBeamIntensity; ///< Max beam intensity [MHz]
  TString  fBeamIntensityTemplateFileName;
  TString  fBeamIntensityTemplateHistoName;
  TH1*     fHBeamIntensityTemplate; ///< Histogram containing the beam intensity profile to be used
  Double_t fBeamIntensity;          ///< Simulated beam intensity (event-by-event)
  TRandom2 *fRandom;
  Bool_t fForcedOnData;
};
#endif
