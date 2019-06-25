// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2017-03-05
// 
// ---------------------------------------------------------------

#ifndef FILTERMUONELECTRONTHREETRACKVERTEX_HH
#define FILTERMUONELECTRONTHREETRACKVERTEX_HH

#include "Analyzer.hh"
#include "TriggerConditions.hh"

class FilterMuonElectronThreeTrackVertex : public NA62Analysis::Analyzer {

public:
  explicit FilterMuonElectronThreeTrackVertex(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterMuonElectronThreeTrackVertex() {}
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
  Int_t fTrigMultiTrack, fTrigElectron, fTrigMuon1, fTrigMuon2, fTrigMuon3;
};

#endif
