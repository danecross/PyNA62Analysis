// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-11-28
//
// ---------------------------------------------------------------

#ifndef FILTER_ALL_HH
#define FILTER_ALL_HH

#include "Analyzer.hh"

class FilterAll : public NA62Analysis::Analyzer {

public:
  explicit FilterAll(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterAll() {}
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
