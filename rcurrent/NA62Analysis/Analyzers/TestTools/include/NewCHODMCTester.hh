// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-07-14
//
// ---------------------------------------------------------------

#ifndef NEWCHODMCTESTER_HH
#define NEWCHODMCTESTER_HH

#include <stdlib.h>
#include "Analyzer.hh"
#include <TCanvas.h>

class TH1I;
class TH2F;
class TH3D;
class TGraph;
class TTree;

class NewCHODMCTester : public NA62Analysis::Analyzer {

public:
  explicit NewCHODMCTester(NA62Analysis::Core::BaseAnalysis *ba);
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
  void DrawPlot() {}

private:
  Double_t fZNewCHOD;
  void Publish() {}
};

#endif
