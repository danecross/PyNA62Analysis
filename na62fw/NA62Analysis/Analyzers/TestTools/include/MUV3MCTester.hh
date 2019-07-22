// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-04-30
//
// ---------------------------------------------------------------

#ifndef MUV3MCTESTER_HH
#define MUV3MCTESTER_HH

#include <stdlib.h>
#include "Analyzer.hh"
#include <TCanvas.h>

class TH1I;
class TH2F;
class TH3D;
class TGraph;
class TTree;

class MUV3MCTester : public NA62Analysis::Analyzer {

public:
  explicit MUV3MCTester(NA62Analysis::Core::BaseAnalysis *ba);
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
  void DrawPlot() {}

private:
  Double_t fZMUV3;
  void Publish() {}
};

#endif
