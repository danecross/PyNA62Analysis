#ifndef REQUESTALLRECOTREES_HH
#define REQUESTALLRECOTREES_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class RequestAllRecoTrees : public NA62Analysis::Analyzer {
public:
  explicit RequestAllRecoTrees(NA62Analysis::Core::BaseAnalysis *ba);
  ~RequestAllRecoTrees() {}
  void InitHist() {}
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(int) {}
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
