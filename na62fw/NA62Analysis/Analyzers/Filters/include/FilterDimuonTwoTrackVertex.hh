// --------------------------------------------------------------- 
// History:
//
// Created by Viacheslav Duk (Viacheslav.Duk@cern.ch) 24.02.2018
//
// ---------------------------------------------------------------

#ifndef FILTERDIMUONTWOTRACKVERTEX_HH
#define FILTERDIMUONTWOTRACKVERTEX_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "TwoLinesCDA.hh"
#include "DownstreamTrack.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class FilterDimuonTwoTrackVertex : public NA62Analysis::Analyzer {

public:
  explicit FilterDimuonTwoTrackVertex(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterDimuonTwoTrackVertex();
  void InitHist();
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(Int_t);
  void PostProcess() {}
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void DrawPlot() {}

private:
  TwoLinesCDA *fCDAcomp;
  Bool_t IsGoodTrack(DownstreamTrack);
};
#endif
