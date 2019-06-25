// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-04-08
//
// ---------------------------------------------------------------

#ifndef BEAMMCTESTER_HH
#define BEAMMCTESTER_HH

#include "Analyzer.hh"
#include "Event.hh"

class BeamMCTester : public Analyzer {

public:
  explicit BeamMCTester(NA62Analysis::Core::BaseAnalysis *ba);
  void InitHist();
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(int iEvent);
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
