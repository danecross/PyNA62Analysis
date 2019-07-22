// ---------------------------------------------------------------
// History:
//
// Handling of +ve and -ve halo particles added by Chris Parkinson, 2019-07-16
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2019-02-19
// ---------------------------------------------------------------

#ifndef PILEUPPARTICLEGENERATOR_HH
#define PILEUPPARTICLEGENERATOR_HH

// This analyzer can only be run as a pre-analyzer
#pragma pre-analyzer

#include "Analyzer.hh"
#include "TRandom2.h"

typedef std::vector< std::pair<Double_t, Int_t>> UpstreamParticles;

class PileupParticleGenerator : public NA62Analysis::Analyzer {

public:

  explicit PileupParticleGenerator(NA62Analysis::Core::BaseAnalysis *ba);
  ~PileupParticleGenerator();
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

  TRandom2 *fRandom;
  Double_t fTimeWindowWidth;
  Double_t fKaonToTotalRatioAtGTK3; ///< Kaon to total ratio at GTK3 ~ 6%
  Double_t fPionToTotalRatioAtGTK3; ///< Pion to total ratio at GTK3 ~ 70%
  Int_t    fNTracksGenerated;       ///< Number of tracks generated in this event
  Double_t fBeamIntensityFudgeFactor; ///< Factor to artificially increase/decrease the beam intensity
  Double_t fHaloFudgeFactor; ///< Factor to artificially increase/decrease the amount of halo particles

  UpstreamParticles fBeamParticles; ///< Vector containing time and ID of generated beam particles
  UpstreamParticles fHaloParticles; ///< Vector containing time and ID of generated halo particles
  Bool_t fForcedOnData; ///< Force the analyzer to run on data events too
  Int_t  fInTimeParticleID; ///< Force a pileup particle to be generated at t=0 of given ID
};
#endif
