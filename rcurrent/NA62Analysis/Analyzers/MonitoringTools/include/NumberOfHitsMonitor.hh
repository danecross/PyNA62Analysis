// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-04-04
// Modified by Viacheslav Duk (Viacheslav.Duk@cern.ch) 28.11.2017
//
// ---------------------------------------------------------------

#ifndef NUMBER_OF_HITS_MONITOR_HH
#define NUMBER_OF_HITS_MONITOR_HH

#include "Analyzer.hh"
#include <stdlib.h>
#include <TCanvas.h>
#include <TLegend.h>
#include <TStyle.h>

class NumberOfHitsMonitor : public NA62Analysis::Analyzer {

public:
  explicit NumberOfHitsMonitor(NA62Analysis::Core::BaseAnalysis *ba);
  ~NumberOfHitsMonitor() {}

  void InitOutput() {}
  void DefineMCSimple() {}
  void InitHist();
  void StartOfRunUser() {}
  void StartOfBurstUser();
  void Process(Int_t);
  void EndOfBurstUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}
  void BuildPDFReport();

private:

  Int_t   fRunNumber;   ///< Run number: do not read multiple runs with the same job
  Bool_t  fReadingData; ///< Reading data or my own output?
  Int_t   fMaxNBursts;  ///< Number of bins in the histograms of counts vs burst ID, default = 5000

  std::vector<TString> fDetectorNames; ///< Detector names
  std::vector<Bool_t>  fDetectorProducesCandidates; ///< Do detectors produce candidates? If not, the corresponding checks are skipped
  UInt_t fNDetectors;

  Double_t fMinPhysicsEventsInBurst; ///< Min number of physics triggers in a good burst
  Double_t fMinControlEventsInBurst; ///< Min number of control triggers in a good burst
  std::vector<Double_t> fMinHitsPerControlTrigger;       ///< To classify burst as bad
  std::vector<Double_t> fMaxHitsPerControlTrigger;       ///< To classify burst as bad
  std::vector<Double_t> fMinCandidatesPerControlTrigger; ///< To classify burst as bad
  std::vector<Double_t> fMaxCandidatesPerControlTrigger; ///< To classify burst as bad
  std::vector<Bool_t>   fBadBurst;
  std::vector<TString>  fDetectorNameBadPeriod; ///< To read bad burst periods
  std::vector<Int_t>    fFirstBurstBadPeriod;   ///< To read bad burst periods
  std::vector<Int_t>    fLastBurstBadPeriod;    ///< To read bad burst periods

  TH1F *fHTotalEventsPerBurst;
  TH1F *fHPhysicsEventsPerBurst;
  TH1F *fHControlEventsPerBurst;
  std::vector<TH1F*>fHHitsPerBurst;              ///< Number of hits per burst in each subdetector
  std::vector<TH1F*>fHCandidatesPerBurst;        ///< Number of candidates per burst in each subdetector
  std::vector<TH1F*>fHHitsPerControlEvent;       ///< Number of hits per control event in each subdetector
  std::vector<TH1F*>fHCandidatesPerControlEvent; ///< Number of candidates per control event in each subdetector
  std::vector<TH1F*>fHHitsInTimePerBurst;              ///< Number of hits (|t|<10ns) per burst in each subdetector
  std::vector<TH1F*>fHCandidatesInTimePerBurst;        ///< Number of candidates (|t|<10ns) per burst in each subdetector
  std::vector<TH1F*>fHHitsInTimePerControlEvent;       ///< Number of hits (|t|<10ns) per control event in each subdetector
  std::vector<TH1F*>fHCandidatesInTimePerControlEvent; ///< Number of candidates (|t|<10ns) per control event in each subdetector
};

#endif
