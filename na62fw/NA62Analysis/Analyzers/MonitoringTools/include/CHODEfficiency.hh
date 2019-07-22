// ---------------------------------------------------------------
//
// History:
//
// Created by Viacheslav Duk (Viacheslav.Duk@cern.ch)
// and Riccardo Lollini (riccardo.lollini@cern.ch) 05.2017
//
// ---------------------------------------------------------------

#ifndef CHODEFFICIENCY_HH
#define CHODEFFICIENCY_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include <TGraphErrors.h>

class TH1I;
class TH2F;
class TGraph;
class TTree;

class TRecoCHODCandidate;

class CHODEfficiency : public NA62Analysis::Analyzer
{
public:
  explicit CHODEfficiency(NA62Analysis::Core::BaseAnalysis *ba);
  ~CHODEfficiency();
  void InitHist();
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(Int_t);
  void PostProcess() {}
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void ExportPlot();
  void DrawPlot() {}

private:
  Bool_t fReadingData;

  TRecoCHODCandidate * fCHODCandidate;

  Double_t fXBins[33] = {-1210.,-1110.,-1010.,-910.,-810.,-710.,-650.,-580.5,-520.,-450.5,-390.,
			 -320.5,-260.,-190.5,-130.,-60.5,0.,60.5,130.,190.5,260.,320.5,
			 390.,450.5,520.,580.5,650.,710.,810.,910.,1010.,1110.,1210.};
  Double_t fYBins[33] = {-1210.,-1110.,-1010.,-910.,-810.,-710.,-650.,-580.5,-520.,-450.5,-390.,
			 -320.5,-260.,-190.5,-130.,-60.5,0.,60.5,130.,190.5,260.,320.5,
			 390.,450.5,520.,580.5,650.,710.,810.,910.,1010.,1110.,1210.};

  Double_t fXOffset = -3.82;
  Double_t fYOffset = -1.17;

  Double_t fMinEff = 0.9;
  Double_t fMinStat = 100;

  Int_t    fBurstID;

  TH2F* fHIllumination;

  TGraphErrors* fHEfficiencyVsBurstID;
  TGraphErrors* fHNExpectedVsBurstID;
  TGraphErrors* fHEfficiencySingleTrackVsBurstID;
  TGraphErrors* fHNExpectedSingleTrackVsBurstID;
  TGraphErrors* fHEfficiencyNoShowerVsBurstID;
  TGraphErrors* fHNExpectedNoShowerVsBurstID;
  TGraphErrors* fHEfficiencySingleTrackNoShowerVsBurstID;
  TGraphErrors* fHNExpectedSingleTrackNoShowerVsBurstID;

  TGraphErrors* fHNMatchedSingleTrackNoShowerVsBurstID;

  Double_t fNMatchedPerBurst;
  Double_t fNExpectedPerBurst;
  Double_t fNMatchedSingleTrackPerBurst;
  Double_t fNExpectedSingleTrackPerBurst;
  Double_t fNMatchedNoShowerPerBurst;
  Double_t fNExpectedNoShowerPerBurst;
  Double_t fNMatchedSingleTrackNoShowerPerBurst;
  Double_t fNExpectedSingleTrackNoShowerPerBurst;

  TH1F * fHMomentumNumerator;
  TH1F * fHMomentumDenominator;
  TH1F * fHMomentumSingleTrackNumerator;
  TH1F * fHMomentumSingleTrackDenominator;

  TH1F * fHMomentumNoShowerNumerator;
  TH1F * fHMomentumNoShowerDenominator;
  TH1F * fHMomentumSingleTrackNoShowerNumerator;
  TH1F * fHMomentumSingleTrackNoShowerDenominator;

  TH1F * fHVertexNumerator;
  TH1F * fHVertexDenominator;
  TH1F * fHVertexSingleTrackNumerator;
  TH1F * fHVertexSingleTrackDenominator;

  TH1F * fHVertexNoShowerNumerator;
  TH1F * fHVertexNoShowerDenominator;
  TH1F * fHVertexSingleTrackNoShowerNumerator;
  TH1F * fHVertexSingleTrackNoShowerDenominator;

  TH2F * fHEfficiency2DNumerator;
  TH2F * fHEfficiency2DDenominator;
  TH2F * fHEfficiency2DSingleTrackNumerator;
  TH2F * fHEfficiency2DSingleTrackDenominator;

  TH2F * fHEfficiency2DNoShowerNumerator;
  TH2F * fHEfficiency2DNoShowerDenominator;
  TH2F * fHEfficiency2DSingleTrackNoShowerNumerator;
  TH2F * fHEfficiency2DSingleTrackNoShowerDenominator;

  TH1F * fHMomentum;
  TH1F * fHMomentumSingleTrack;

  TH1F * fHMomentumNoShower;
  TH1F * fHMomentumSingleTrackNoShower;

  TH1F * fHVertex;
  TH1F * fHVertexSingleTrack;

  TH1F * fHVertexNoShower;
  TH1F * fHVertexSingleTrackNoShower;

  TH2F * fHEfficiency2D;
  TH2F * fHEfficiency2DSingleTrack;

  TH2F * fHEfficiency2DNoShower;
  TH2F * fHEfficiency2DSingleTrackNoShower;

  TCanvas * fCanvas;
  TString fOutputPDFFileName;

protected:

};
#endif
