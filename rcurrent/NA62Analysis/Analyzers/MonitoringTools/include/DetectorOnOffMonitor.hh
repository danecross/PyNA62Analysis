// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-02-05
//
// ---------------------------------------------------------------

#ifndef DETECTOR_ON_OFF_MONITOR_HH
#define DETECTOR_ON_OFF_MONITOR_HH

#include "Analyzer.hh"
#include <stdlib.h>
#include <TCanvas.h>
#include <TStyle.h>

class DetectorOnOffMonitor : public NA62Analysis::Analyzer {

public:
  explicit DetectorOnOffMonitor(NA62Analysis::Core::BaseAnalysis *ba);
  ~DetectorOnOffMonitor() {}

  void InitOutput() {}
  void DefineMCSimple() {}
  void InitHist();
  void StartOfRunUser() {}
  void StartOfBurstUser() {}
  void Process(Int_t);
  void EndOfBurstUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}

private:
  Bool_t fReadingData; ///< Reading data or my own output?
  TH1F *fHRecoHitsInDetectors;
  TH1F *fHCandidatesInDetectors;
};

#endif
