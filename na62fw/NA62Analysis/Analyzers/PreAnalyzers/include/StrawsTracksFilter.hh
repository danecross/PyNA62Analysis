#ifndef STRAWSTRACKSFILTER_HH
#define STRAWSTRACKSFILTER_HH

// This analyzer can only be run as a pre-analyzer
#pragma pre-analyzer

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "StrawsTracksFilterAlgo.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class StrawsTracksFilter : public NA62Analysis::Analyzer {

public:
  explicit StrawsTracksFilter(NA62Analysis::Core::BaseAnalysis *ba);
  ~StrawsTracksFilter();
  void InitHist();
  void InitOutput();
  void DefineMCSimple();
  void Process(int iEvent);
  void PostProcess();
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser();
  void EndOfJobUser();
  void DrawPlot();
protected:
  StrawsTracksFilterAlgo *fAlgoFilter;

};
#endif
