// -------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) Dec 2015
//
// -------------------------------------------------------------

#ifndef PRINTKINEPARTS_HH
#define PRINTKINEPARTS_HH

#include "Analyzer.hh"
#include "Event.hh"

class PrintKineParts : public Analyzer {

public:
  explicit PrintKineParts(NA62Analysis::Core::BaseAnalysis *ba);
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
