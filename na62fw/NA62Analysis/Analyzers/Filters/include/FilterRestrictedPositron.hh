// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-12-10
// 
// ---------------------------------------------------------------

#ifndef FILTERRESTRICTEDPOSITRON_HH
#define FILTERRESTRICTEDPOSITRON_HH

#include "Analyzer.hh"

class FilterRestrictedPositron : public NA62Analysis::Analyzer {

public:
  explicit FilterRestrictedPositron(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterRestrictedPositron() {}
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
