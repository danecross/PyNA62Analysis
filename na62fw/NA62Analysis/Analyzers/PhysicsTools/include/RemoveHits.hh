// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2017-09-19
//
// ---------------------------------------------------------------

#ifndef REMOVEHITS_HH
#define REMOVEHITS_HH

#include "Analyzer.hh"

class RemoveHits : public NA62Analysis::Analyzer {

public:
  explicit RemoveHits(NA62Analysis::Core::BaseAnalysis *ba);
  ~RemoveHits() {}
  void InitHist() {}
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(Int_t) {}
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser() {}
  void PostProcess();
  void DrawPlot() {}
};

#endif
