// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2017-10-31
//
// ---------------------------------------------------------------

#ifndef TRIGGERRATES_HH
#define TRIGGERRATES_HH

#include "Analyzer.hh"
#include "TriggerConditions.hh"

class TriggerRates : public NA62Analysis::Analyzer {

public:
  explicit TriggerRates(NA62Analysis::Core::BaseAnalysis *ba);
  ~TriggerRates() {}
  void InitHist() {}
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(Int_t) {}
  void ProcessSOBEvent() {}
  void ProcessEOBEvent();
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void PostProcess() {}
  void EndOfJobUser() {}
  void DrawPlot() {}

};
#endif
