#ifndef LAVSPECIALDIGEST_HH
#define LAVSPECIALDIGEST_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

using namespace std;

class TH1I;
class TH2F;
class TGraph;
class TTree;

class LAVSpecialDigest : public Analyzer
{
public:
  explicit LAVSpecialDigest(BaseAnalysis *ba);
  void InitHist();
  void InitOutput();
  void DefineMCSimple();
  void Process(Int_t);
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser();
  void PostProcess();
  void ExportPlot();
  void DrawPlot();
  
private:
  
  void Publish(); ///< Deprecated
  
protected:
  
  double fGlobalOffsetTrig;
  double fGlobalOffsetCHOD;
  double fGlobalOffsetCedar;
  bool fUseCedarOnly;
  
};
#endif
