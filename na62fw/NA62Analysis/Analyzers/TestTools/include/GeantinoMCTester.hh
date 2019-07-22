// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2018-10-24
//
// ---------------------------------------------------------------

#ifndef GEANTINOMCTESTER_HH
#define GEANTINOMCTESTER_HH

#include "Analyzer.hh"
#include "Event.hh"

class GeantinoMCTester : public Analyzer {

public:
  explicit GeantinoMCTester(NA62Analysis::Core::BaseAnalysis *ba);
  void InitHist();
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void ExportPlot() {}
  void DrawPlot() {}
};

#endif
