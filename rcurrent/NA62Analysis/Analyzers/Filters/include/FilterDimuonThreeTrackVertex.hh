// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2017-03-02
// 
// ---------------------------------------------------------------

#ifndef FILTERDIMUONTHREETRACKVERTEX_HH
#define FILTERDIMUONTHREETRACKVERTEX_HH

#include "Analyzer.hh"
#include "TriggerConditions.hh"

class FilterDimuonThreeTrackVertex : public NA62Analysis::Analyzer {

public:
  explicit FilterDimuonThreeTrackVertex(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterDimuonThreeTrackVertex() {}
  void InitHist() {}
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser();
  void EndOfRunUser() {}
  void EndOfJobUser() {}
  void PostProcess() {}
  void DrawPlot() {}

private:
  TriggerConditions* fTrigCond;
  Int_t fTrigMultiTrack, fTrigDimuon1, fTrigDimuon2;
};

#endif
