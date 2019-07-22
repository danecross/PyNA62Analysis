// --------------------------------------------------------------- 
// History:
//
// Created by Viacheslav Duk (Viacheslav.Duk@cern.ch) 10.12.2016
//
// ---------------------------------------------------------------

#ifndef FILTERTWOTRACKVERTEXWITHLEPTON_HH
#define FILTERTWOTRACKVERTEXWITHLEPTON_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "TwoLinesCDA.hh"
#include "DownstreamTrack.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class FilterTwoTrackVertexWithLepton : public NA62Analysis::Analyzer {

public:
  explicit FilterTwoTrackVertexWithLepton(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterTwoTrackVertexWithLepton();
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
