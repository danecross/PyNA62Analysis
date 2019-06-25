// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2017-10-17
//
// ---------------------------------------------------------------

#ifndef FILTER_RestrictedMuon_HH
#define FILTER_RestrictedMuon_HH

#include "Analyzer.hh"

class FilterRestrictedMuon : public NA62Analysis::Analyzer {

public:
  explicit FilterRestrictedMuon(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterRestrictedMuon() {}
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
};

#endif
