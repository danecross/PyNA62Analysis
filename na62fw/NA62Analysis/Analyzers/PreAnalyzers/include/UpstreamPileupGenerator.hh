// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2018-01-30
// ---------------------------------------------------------------

#ifndef UpstreamPileupGenerator_HH
#define UpstreamPileupGenerator_HH

// This analyzer can only be run as a pre-analyzer
#pragma pre-analyzer

#include "Analyzer.hh"
#include "TRandom2.h"
#include "TF1.h"
#include "CedarReconstruction.hh"

class TRecoCedarEvent;
class TRecoGigaTrackerEvent;

class UpstreamPileupGenerator : public NA62Analysis::Analyzer {

public:
  explicit UpstreamPileupGenerator(NA62Analysis::Core::BaseAnalysis *ba);
  ~UpstreamPileupGenerator();
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser();
  void EndOfBurstUser() {}
  void StartOfRunUser();
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess();
  void ProcessEOBEvent() {}
  void DrawPlot() {}
  void Print();

private:

  void ReadGtkHitLibrary();
  void AddTrack(Double_t, Int_t, Int_t);
  void EvaluateInstantaneousIntensity();

  Bool_t   fEnabled;                    ///< Pileup generation enabled?
  Double_t fTimeWindowWidth;            ///< Width of the time window [ns]
  Double_t fPixelTimeResolution;        ///< GTK hit time resolution [ns]
  Double_t fGigaTrackerHitInefficiency; ///< Single-hit inefficiency (~4%)
  Double_t fCedarMeanNHitsPerKaon;      ///< Mean number of hits in a Cedar kaon candidate
  Double_t fCedarMeanNHitsPerPion;      ///< Mean number of hits in a Cedar pion candidate
  Double_t fCedarMeanNHitsPerProton;    ///< Mean number of hits in a Cedar proton candidate

  CedarReconstruction* fCedarReco;
  Double_t fCedarPMTTime_min;
  Double_t fCedarPMTTime_max;
  TF1*     fCedarTimeResponse; ///< Cedar PMT time response pdf function
  Bool_t   fKaonGenerated;     ///< Is a beam kaon generated in this event?

  TRandom2 *fRandom;
  std::vector<std::vector<Int_t>> fHitLibrary;
  Bool_t fForcedOnData;
  Int_t  fGenerateInTimePion; ///< Generate exactly one in-time beam pion? 0=no; otherwise the value (1-60000) determines the algorithm used to select a pion from the library.
  Double_t fInTimePionTimeWindow; ///< Half-width of the time window for in-tie pion generation; negative value = exactly in-time
  TRecoCedarEvent* fCedarEvent;
  TRecoGigaTrackerEvent* fGTKEvent;
};

#endif
