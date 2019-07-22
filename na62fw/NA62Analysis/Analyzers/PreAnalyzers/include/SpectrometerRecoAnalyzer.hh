// ---------------------------------------------------------------
//
// History:
//
// Created by Michal Koval (michal.koval@cern.ch) 2019-01-08
//
// ---------------------------------------------------------------

#ifndef SPECTROMETERRECOANALYZER_H
#define SPECTROMETERRECOANALYZER_H

// This analyzer can only be run as a pre-analyzer
#pragma pre-analyzer

#include <memory>
#include "Analyzer.hh"
#include "SpectrometerRecoAlgorithm.hh"
#include "TRandom2.h"

class SpectrometerRecoAnalyzer : public NA62Analysis::Analyzer {

public:
  explicit SpectrometerRecoAnalyzer(NA62Analysis::Core::BaseAnalysis *ba);
  ~SpectrometerRecoAnalyzer() {}
  void InitHist();
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser();
  void EndOfBurstUser() {}
  void StartOfRunUser();
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}

private:
  TString fRecoConfPath; ///< $NA62RECOSOURCE/conf path
  TString fConfFileName; ///< Spectrometer config file path
  Bool_t fUseExoticsConfig; ///< Should the default Spectrometer.exotics.conf be used
  std::unique_ptr<SpectrometerRecoAlgorithm> fSpecReco;

  Bool_t fUpdateWireDistance;  
};

#endif /* SPECTROMETERRECOANALYZER_H */
