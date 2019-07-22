// ----------------------
// Created by Francesco Brizioli (francesco.brizioli@cern.ch)
// April 2019
// ----------------------
/// \author Francesco Brizioli (francesco.brizioli@cern.ch)
// ------------------------------------------------------------

#ifndef LKRFINECALIBRATIONASYMM_HH
#define LKRFINECALIBRATIONASYMM_HH

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

class LKrFineCalibrationAsymm : public NA62Analysis::Analyzer {

public:
  explicit  LKrFineCalibrationAsymm(NA62Analysis::Core::BaseAnalysis *ba);
  ~LKrFineCalibrationAsymm();
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser();
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
  Bool_t fMakeExtrapolationOutOfRange;
  TH2D* fHRaVsE;
  TGraphErrors* fGRaVsE;
  Double_t fPreviousFitPar[8];
  Double_t fPreviousFitRange[2];
  Double_t fNewFitPar[8];
  Double_t fNewFitRange[2];
  Double_t fFitEdge;
};
#endif

