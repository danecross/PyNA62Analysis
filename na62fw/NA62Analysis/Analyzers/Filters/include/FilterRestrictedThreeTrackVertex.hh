// ---------------------------------------------------------------
//
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2016-12-05
// 
// ---------------------------------------------------------------

#ifndef FILTERRESTRICTEDTHREETRACKVERTEX_HH
#define FILTERRESTRICTEDTHREETRACKVERTEX_HH

#include "Analyzer.hh"

class FilterRestrictedThreeTrackVertex : public NA62Analysis::Analyzer {

public:
  explicit FilterRestrictedThreeTrackVertex(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterRestrictedThreeTrackVertex() {}
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
