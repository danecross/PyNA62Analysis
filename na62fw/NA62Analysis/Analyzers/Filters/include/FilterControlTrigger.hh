// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-07-12
//
// ---------------------------------------------------------------

#ifndef FILTER_CONTROL_TRIGGER_HH
#define FILTER_CONTROL_TRIGGER_HH

#include "Analyzer.hh"

class FilterControlTrigger : public NA62Analysis::Analyzer {

public:
  explicit FilterControlTrigger(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterControlTrigger() {}
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
};

#endif
