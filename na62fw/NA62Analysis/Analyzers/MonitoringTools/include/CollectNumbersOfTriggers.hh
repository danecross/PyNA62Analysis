// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2016-03-17
//
// ---------------------------------------------------------------

#ifndef CollectNumbersOfTriggers_HH
#define CollectNumbersOfTriggers_HH

#include "Analyzer.hh"

class CollectNumbersOfTriggers : public NA62Analysis::Analyzer {

public:
  explicit CollectNumbersOfTriggers(NA62Analysis::Core::BaseAnalysis *ba);
  ~CollectNumbersOfTriggers() {}

  void InitOutput() {}
  void DefineMCSimple() {}
  void InitHist();
  void StartOfRunUser() {}
  void StartOfBurstUser() {}
  void Process(Int_t);
  void EndOfBurstUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}

private:

  std::ofstream fOutputStream;
};

#endif
