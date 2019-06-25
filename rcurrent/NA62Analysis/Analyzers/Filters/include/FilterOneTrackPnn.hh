// ---------------------------------------------------------
//
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2018-04-27
// 
// ---------------------------------------------------------

#ifndef FILTERONETRACKPNN_HH
#define FILTERONETRACKPNN_HH

#include "Analyzer.hh"

class FilterOneTrackPnn : public NA62Analysis::Analyzer {

public:
  explicit FilterOneTrackPnn(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterOneTrackPnn() {}
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
