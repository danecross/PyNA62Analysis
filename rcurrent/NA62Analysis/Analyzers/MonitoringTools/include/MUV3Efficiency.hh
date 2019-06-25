// ---------------------------------------------------------------
//
// History:
//
// Created by Lubos Bician (lubos.bician@cern.ch) 2016-02-16
//
// ---------------------------------------------------------------

#ifndef MUV3EFFICIENCY_HH
#define MUV3EFFICIENCY_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include <TGraphErrors.h>
#include <TStyle.h>
#include "DownstreamTrack.hh"
#include "MUV3Geometry.hh"
#include "GeometricAcceptance.hh"

class MUV3Efficiency : public NA62Analysis::Analyzer {

public:

  explicit MUV3Efficiency(NA62Analysis::Core::BaseAnalysis *ba);
  ~MUV3Efficiency();
  void InitHist();
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser();
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}

private:

  TCanvas* fCanvas;
  Bool_t   fReadingData;
  Int_t    fMaxNBursts;
  TH2F* fhTileVsBurstID;
  TH2F* fhExpected;
  TH2F* fhMatched;
  TH2F* fhEfficiency;
  TH1F* fhMatchedVsBurstID;
  TH1F* fhExpectedVsBurstID;
  TH2F* fhDeltaT;
  TH1F* fhDeltaTTriggerMUV3;
  TH1F* fhEfficiency1DLowMom;
  TH1F* fhEfficiency1DHighMom;
  TGraphErrors* fhEffVsBurstID;
  TString fOutPDFFileName;
  MUV3Geometry* fMUV3Geometry;

  void GenerateInefficiencyPlot(TH1F*, TString, Bool_t);

  Double_t fCHODZPos;
  Double_t fMUV1ZPos;
  Double_t fMUV2ZPos;
  Double_t fMUV3ZPos;
  Double_t fMUV3Rmin;
  Double_t fMUV3Rmax;

  UInt_t fTriggerID;
  Bool_t fTriggerOK;
  Bool_t fUseControlTrigger;
  Bool_t fKmu2Enabled;
  Bool_t fStrictSelection;
  enum enumEventType {kHalo, kKmu2};
  void checkTrigger();
};
#endif
