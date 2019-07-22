#ifndef GIGATRACKERDATAQUALITYMONITOR_HH
#define GIGATRACKERDATAQUALITYMONITOR_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "TriggerConditions.hh"
#include <TGraphErrors.h>
#include <TPad.h>
#include <TLegend.h>
#include <TCanvas.h>

class TH1I;
class TH2F;
class TGraph;
class TTree;

class TRecoGigaTrackerEvent;

class GigaTrackerDataQualityMonitor : public NA62Analysis::Analyzer {
public:
  explicit GigaTrackerDataQualityMonitor(NA62Analysis::Core::BaseAnalysis *ba);
  ~GigaTrackerDataQualityMonitor();
  void InitHist();
  void InitOutput();
  void DefineMCSimple();
  void StartOfBurstUser();
  void ProcessEOBEvent();
  void Process(Int_t);
  void PostProcess();
  void EndOfJobUser();
  void EndOfBurstUser();
  void DrawPlot();
protected:
  TriggerConditions *fTriggerConditions;
  TString fDirName;
  Bool_t fReadingData;

  Int_t fBurstID = 0;
  Double_t fEffThreshold = 0;
  Int_t fArgonionCountsMin = 0;
  Double_t fArgonionCounts = 0.;
  Int_t fNControlTriggersMin = 0;
  Int_t fNControl = 0;
  Int_t fNGTKBadEvents = 0;
  Double_t fNExpected[3] = {0., 0., 0.}; // {k2pi, k3pi, all events}
  Double_t fNSelected[3] = {0., 0., 0.}; // {k2pi, k3pi, events without gtk error}

  TRecoGigaTrackerEvent* fGigaTrackerEvent;

  TGraphErrors * fHArgonionCountsVsBurstID;
  TGraphErrors * fHGTKCriticalEventsVsBurstID;
  TGraphErrors * fHControlTriggersVsBurstID;
  TGraphErrors * fHEfficiencyVsBurstID[3];
  TGraphErrors * fHNExpectedEventsVsBurstID[3];

  TH2D * fHHitMap_all[3],
    * fHTimeProfile_all[3];

  TH1F * fHGTKCandidateTimeDiff[2],
    * fHAbsTimeProfile_all[3],
    * fHToT_all[3],
    * fHTime_all[3];

  TH2D * fHDecoderErrors,
    * fHDecoderErrors_all,
    * fHDigiTimeRawFine_all;

  TH2F * fHDigiTimeRawFineVsROChannel_all;

};
#endif
