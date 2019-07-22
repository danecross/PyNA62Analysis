// ----------------------
// Created by Francesco Brizioli (francesco.brizioli@cern.ch)
// April 2019
// ----------------------
/// \author Francesco Brizioli (francesco.brizioli@cern.ch)
// ------------------------------------------------------------

#ifndef LKRFINECALIBRATIONSYMM_HH
#define LKRFINECALIBRATIONSYMM_HH

#include "Analyzer.hh"
#include "SpectrometerTrackVertex.hh"
#include <stdlib.h>
#include <TCanvas.h>
#include <TLegend.h>
#include <TStyle.h>
#include <TGraphErrors.h>
#include "GigaTrackerRecoAlgorithm.hh"
#include "SpectrometerGigaTrackerMatchingTool.hh"
#include "TRecoSAVEvent.hh"
#include <TProfile.h>
#include <TH2.h>

class LKrFineCalibrationSymm : public NA62Analysis::Analyzer {

public:
  explicit  LKrFineCalibrationSymm(NA62Analysis::Core::BaseAnalysis *ba);
  ~LKrFineCalibrationSymm();
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser() {}
  void EndOfJobUser();
  void BuildPDFReport();
  void PostProcess() {}
  void DrawPlot() {}
  void PrintStatisticsPerBurst();
  void CreateBadBurstList() {}

private:

  Int_t fTriggerMask; /// < Definition of the data sample by L0 trigger mask
  Bool_t fReadingData; /// < Reading data or my own output? 

  Int_t fNIter;
  TString  fOutPDFFileName;
  TString fLKrPi0CalibFileName;
  TH2D* fHRsVsE;
  TGraphErrors* fGRsVsE;
  TH2D* fHRsNewVsE;
  TGraphErrors* fGRsNewVsE;
  TH2D* fHRsOldVsE;
  TGraphErrors* fGRsOldVsE;
  Double_t fPreviousFitPar[8];
  Double_t fPreviousFitRange[2];
  Double_t fNewFitPar[8];
  Double_t fNewFitRange[2];
  Double_t fFitEdge;
};
#endif

