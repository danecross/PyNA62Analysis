// ---------------------------------------------------------
//
// History:
//
// Created by Silvia Martellotti (silvia.martellotti@cern.ch) 2019-03-14
// 
// ---------------------------------------------------------

#ifndef FILTERONETRACKNOTMUON_HH
#define FILTERONETRACKNOTMUON_HH

#include "Analyzer.hh"

class FilterOneTrackNotMuon : public NA62Analysis::Analyzer {

public:
  explicit FilterOneTrackNotMuon(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterOneTrackNotMuon() {}
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

};

#endif
