// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-09
//
// ---------------------------------------------------------------

#ifndef NewCHODDATAQUALITYMONITOR_HH
#define NewCHODDATAQUALITYMONITOR_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include <TStyle.h>

#include "TH1F.h"
#include "TProfile.h"
#include "TF1.h"
#include "TText.h"
#include "TLine.h"
#include "TArc.h"
#include "TLegend.h"

class NewCHODDataQualityMonitor : public NA62Analysis::Analyzer {

public:

  explicit NewCHODDataQualityMonitor(NA62Analysis::Core::BaseAnalysis *ba);
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
  void LoadNIMPaperRatesAndAcceptances();

private:

  TString fOutPDFFileName, fDirName;
  TH1F *fHChannelProfile, *fHChannelProfileEOB, *fHTileOR, *fHTileAND;
  TH2F *fHNRecoHitsVsL0TriggerBit, *fHNRecoHitsVsNoL0TriggerBit;
  TH2F *fHTightRecoHitProfileVsL0TriggerBit, *fHTightRecoHitProfileVsNoL0TriggerBit;
  TH2F *fHChannelProfileVsBurst, *fHChannelProfileVsBurstEOB;
  TH2F *fHTimeWrtReferenceVsReadoutChannelNoT0;
  TH2F *fHTimeWrtReferenceVsReadoutChannel;

  TH1F *fHNEventsProcessedPerBurst;
  TH1F *fHTileAsymmetry, *fHTileAsymmetryEOB;
  TH1F *fHTotalPrimitiveCountsEOB, *fHErrorCountsEOB;
  TH2F *fHTightRecoHitTimeWrtReferenceVsTile;
  TH2F *fHLooseRecoHitTimeWrtReferenceVsTile;
  TH1F *fHTightRecoHitTimeWrtReference, *fHLooseRecoHitTimeWrtReference;

  std::vector<TH2F*> fHList;
};

#endif
