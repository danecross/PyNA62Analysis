// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-01-26
//
// ---------------------------------------------------------------

#ifndef SPECTROMETERTRACKCORRECTIONS_HH
#define SPECTROMETERTRACKCORRECTIONS_HH

// This analyzer can only be run as a pre-analyzer
#pragma pre-analyzer

#include "Analyzer.hh"

class SpectrometerTrackCorrections : public NA62Analysis::Analyzer {

public:
  explicit SpectrometerTrackCorrections(NA62Analysis::Core::BaseAnalysis *ba);
  ~SpectrometerTrackCorrections() {}
  void InitHist() {}
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser();
  void EndOfRunUser() {}
  void EndOfJobUser() {}
  void PostProcess() {}
  void DrawPlot() {}

private:
  Double_t fExternalAlpha; ///< "Forced" alpha parameter passed from the command line
  Double_t fExternalBeta;  ///< "Forced" beta parameter passed from the command line
  Double_t fAlpha;         ///< The alpha constant for the current run
  Double_t fBeta;          ///< The beta constant for the current run
  Bool_t   fWarnOnce;      ///< Indicates if the corrections have been applied already
};

#endif
