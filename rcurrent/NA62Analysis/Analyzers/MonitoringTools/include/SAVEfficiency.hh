// ------------------------------------------------------------
// K2pi Selection for IRC and SAC efficiency studies
// It creates bad burst list and pdf report for IRC SAC and SAV
// ------------------
// Created by Francesco Brizioli (francesco.brizioli@cern.ch)
// March 2019
// ------------------------------------------------------------

#ifndef SAVEFFICIENCY_HH
#define SAVEFFICIENCY_HH

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

class SAVEfficiency : public NA62Analysis::Analyzer {

public:
  explicit  SAVEfficiency(NA62Analysis::Core::BaseAnalysis *ba);
  ~SAVEfficiency();
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void BuildPDFReport();
  void PostProcess() {}
  void DrawPlot() {}
  void PrintStatisticsPerBurst();
  void CreateBadBurstList();

  Int_t SAVCreamMatching(TRecoSAVEvent*, Double_t, Double_t, Double_t );

private:

  Int_t fTriggerMask; /// < Definition of the data sample by L0 trigger mask
  Bool_t fReadingData; /// < Reading data or my own output?
  Int_t fBurstID;
  Int_t fRunID;

  SpectrometerGigaTrackerMatchingTool* fSG;
  GigaTrackerRecoAlgorithm* fGTKAlgo;

  TString  fOutPDFFileName;

  Double_t fSAVDeltaTime;
  Int_t fNSelectedTriggers;
  Int_t fNSelectedTriggersMin;
  Int_t fIRCExpectedMin;
  Int_t fSACExpectedMin;
  Int_t fExpectedIRC;
  Int_t fExpectedSAC;
  Double_t fExpectedIRCErr;
  Double_t fExpectedSACErr;
  Int_t fSignalInIRC;
  Int_t fSignalInSAC;
  Double_t fIRCEfficiency;
  Double_t fSACEfficiency;
  Double_t fIRCEfficiencyErr;
  Double_t fSACEfficiencyErr;
  Int_t fSignalInIRCCream;
  Int_t fSignalInSACCream;
  Double_t fIRCCreamEfficiency;
  Double_t fSACCreamEfficiency;
  Double_t fIRCCreamEfficiencyErr;
  Double_t fSACCreamEfficiencyErr;

  TGraphErrors* fHNSelectedTriggersVsBurstID;
  TGraphErrors* fHIRCExpectedVsBurstID;
  TGraphErrors* fHSACExpectedVsBurstID;
  TGraphErrors* fHIRCEfficiencyVsBurstID;
  TGraphErrors* fHSACEfficiencyVsBurstID;
  TGraphErrors* fHIRCCreamEfficiencyVsBurstID;
  TGraphErrors* fHSACCreamEfficiencyVsBurstID;
  Double_t fIRCEfficiencyThreshold;
  Double_t fSACEfficiencyThreshold;
  Double_t fIRCCreamEfficiencyThreshold;
  Double_t fSACCreamEfficiencyThreshold;

};
#endif

