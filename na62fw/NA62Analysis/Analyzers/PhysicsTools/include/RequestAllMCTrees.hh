#ifndef REQUESTALLMCTREES_HH
#define REQUESTALLMCTREES_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class RequestAllMCTrees : public NA62Analysis::Analyzer {
public:
  explicit RequestAllMCTrees(NA62Analysis::Core::BaseAnalysis *ba);
  ~RequestAllMCTrees() {}
  void InitHist() {}
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(Int_t) {}
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser() {}
  void PostProcess() {}
  void DrawPlot() {}

protected:

};
#endif
