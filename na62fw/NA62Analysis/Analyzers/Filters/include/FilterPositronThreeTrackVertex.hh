// ---------------------------------------------------------------
//
// History:
//
// Created by A. Shaikhiev (shaykhiev@inr.ru) 2017-03-06
// 
// ---------------------------------------------------------------

#ifndef FILTERPOSITRONTHREETRACKVERTEX_HH
#define FILTERPOSITRONTHREETRACKVERTEX_HH

#include "Analyzer.hh"
#include "TriggerConditions.hh"

class FilterPositronThreeTrackVertex : public NA62Analysis::Analyzer {

public:
  explicit FilterPositronThreeTrackVertex(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterPositronThreeTrackVertex() {}
  void InitHist() {}
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser() {}
  void PostProcess() {}
  void DrawPlot() {}

private:
  TriggerConditions* fTrigCond;
  Int_t fTrigMultiTrack, fTrigElectron;
};

#endif
