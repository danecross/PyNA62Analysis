#ifndef FILTERHNLVERTEX_HH
#define FILTERHNLVERTEX_HH

#include <stdlib.h>
#include <vector>
#include <iostream>
#include <string>
#include "Analyzer.hh"
#include "TwoLinesCDA.hh"
#include "PointLineDistance.hh"

class FilterHNLVertex : public NA62Analysis::Analyzer {

public:

  explicit FilterHNLVertex(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterHNLVertex();
  void InitHist() {};
  void DefineMCSimple() {};
  void InitOutput() {};
  void PreProcess() {};
  void Process(int iEvent);
  void PostProcess() {};
  void StartOfBurstUser() {};
  void EndOfBurstUser() {};
  void StartOfRunUser() {};
  void EndOfRunUser() {};
  void EndOfJobUser() {};
  void DrawPlot() {};

private:

  long int fControlTriggerCounter;
  Int_t fControlTriggerDS;
  TwoLinesCDA *fCDAcomp;  
  PointLineDistance *fDistcomp;
  std::vector<Int_t> fID;
  std::vector<std::string> fStream;
};
#endif
