// ---------------------------------------------------------------
//
// History:
//
// Created by Mathieu Perrin-Terrin (mathieu.perrin-terrin@cern.ch) 2018-06-06
//
// ---------------------------------------------------------------

#ifndef FILTER_MUON_NEUTRINOS_HH
#define FILTER_MUON_NEUTRINOS_HH

#include "Analyzer.hh"

class TRecoMUV3Event;

class FilterMuonNeutrino : public NA62Analysis::Analyzer {

public:
  explicit FilterMuonNeutrino(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterMuonNeutrino() {}
  void InitHist() {}
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser() {}
  void PostProcess() {}
  void DrawPlot() {}

private:
  Int_t fDownscaling; ///< Output donwscaling factor
  TRecoMUV3Event * fMUV3Evt;
  double fTW;

};

#endif
