// ---------------------------------------------------------
//
// History:
//
// Created by Lubos Bician (lubos.bician@cern.ch) 2016-07-21
//
// ---------------------------------------------------------

#ifndef NEWCHODEFFICIENCY_HH
#define NEWCHODEFFICIENCY_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include <TGraphErrors.h>
#include <TStyle.h>
#include "DownstreamTrack.hh"
#include "GeometricAcceptance.hh"
#include "NewCHODGeometry.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class NewCHODEfficiency : public NA62Analysis::Analyzer {

public:

  explicit NewCHODEfficiency(NA62Analysis::Core::BaseAnalysis *ba);
  ~NewCHODEfficiency();
  void InitHist();
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}

private:

  TCanvas* fCanvas;
  Bool_t fReadingData;
  Int_t fMaxNBursts;  ///< Number of bins in the histograms of counts vs burst ID, default = 5000
  TH2F* fhExpected;
  TH2F* fhMatched;
  TH2F* fhEfficiency;
  TH1F* fhMatchedVsBurstID;
  TH1F* fhExpectedVsBurstID;
  TH2F* fhDeltaT;
  TH1F* fhDeltaTCHODNewCHOD;
  TGraphErrors* fhEffVsBurstID;
  TString fOutPDFFileName;
  NewCHODGeometry* fNewCHODGeometry;

  Double_t fCHODZPos;
  Double_t fNewCHODZPos;
  Double_t fNewCHODRmin;
  Double_t fNewCHODRmax;
};
#endif
