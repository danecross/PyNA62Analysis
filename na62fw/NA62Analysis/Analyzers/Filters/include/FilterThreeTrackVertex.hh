// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-03-27
//
// ---------------------------------------------------------------

#ifndef FILTERTHREETRACKVERTEX_HH
#define FILTERTHREETRACKVERTEX_HH

#include "Analyzer.hh"

class FilterThreeTrackVertex : public NA62Analysis::Analyzer {

public:
  explicit FilterThreeTrackVertex(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterThreeTrackVertex() {}
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
