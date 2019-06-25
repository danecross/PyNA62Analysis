// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-06-25
//
// ---------------------------------------------------------------

#ifndef MUV3DATAQUALITYMONITOR_HH
#define MUV3DATAQUALITYMONITOR_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class MUV3DataQualityMonitor : public NA62Analysis::Analyzer {

public:
  explicit MUV3DataQualityMonitor(NA62Analysis::Core::BaseAnalysis *ba);
  ~MUV3DataQualityMonitor() {}

  void InitOutput() {}
  void DefineMCSimple() {}
  void InitHist();
  void StartOfRunUser() {}
  void StartOfBurstUser() {}
  void Process(Int_t) {}
  void EndOfBurstUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}

private:
  TString fOutDirName;
  TString fDirName;
  TH1F *fHNEventsProcessedPerBurst;
  TH1F *fHNRecoHitsPerBurst;
  TH1F *fHNLooseCandidatesPerBurst;
  TH1F *fHNTightCandidatesPerBurst;
  TH1F *fHNRecoHits;
  TH1F *fHChannelProfile, *fHChannelProfileEOB, *fHTileOR, *fHTileAND;
  TH1F *fHCandidateTimeWrtReferenceNoTileT0;
  TH1F *fHCandidateTimeWrtReference;
  TH1F *fHCandidateAvgTimeWrtReference;
  TH2F *fHChannelProfileVsBurst, *fHChannelProfileVsBurstEOB;
  TH2F *fHNCandidatesVsL0TriggerBit;
  TH2F *fHNCandidatesVsNoL0TriggerBit;
  TH2F *fHTightCandidateProfileVsL0TriggerBit;
  TH2F *fHTightCandidateProfileVsNoL0TriggerBit;
  TH2F *fHRecoHitTimeWrtReferenceVsReadoutChannelNoT0;
  TH2F *fHRecoHitTimeWrtReferenceVsReadoutChannel;
  TH2F *fHCandidateTimeWrtReferenceNoTileT0VsTile;
  TH2F *fHCandidateTimeWrtReferenceVsTile;
  TH2F *fHCandidateAvgTimeWrtReferenceVsTile;
  TH1F *fHTotalPrimitiveCountsEOB;
  TH1F *fHErrorCountsEOB;
  std::vector<TH2F*> fHList;
};

#endif
