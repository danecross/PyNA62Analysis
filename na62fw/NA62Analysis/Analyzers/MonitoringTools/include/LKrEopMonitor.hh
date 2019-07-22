// ---------------------------------------------------------
//
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-01-30
//
// ---------------------------------------------------------

#ifndef LKREOPMONITOR_HH
#define LKREOPMONITOR_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include <TStyle.h>
#include <TGraphErrors.h>

class TH1I;
class TH2F;
class TGraph;
class TTree;

class LKrEopMonitor : public NA62Analysis::Analyzer {

  public:

    explicit LKrEopMonitor(NA62Analysis::Core::BaseAnalysis *ba);
    ~LKrEopMonitor();
    void InitHist();
    void InitOutput() {}
    void DefineMCSimple() {}
    void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
    void Process(Int_t);
    void StartOfBurstUser();
    void EndOfBurstUser();
    void StartOfRunUser() {}
    void EndOfRunUser() {}
    void EndOfJobUser();
    void PostProcess() {}
    void DrawPlot() {}
    void ComputeLKrStrawALignmentConstants();
    void CreateBadBurstList();
    void BuildPDFReport();

  private:

    std::pair<std::pair<Double_t,Double_t>,std::pair<Double_t,Double_t>> MeanEoPFit();
    std::pair<std::pair<Double_t,Double_t>,std::pair<Double_t,Double_t>> EoPVsPFit();

    TCanvas* fCanvas;
    TCanvas* fc_dx_y; ///< A canvas for plotting LKr-Straw alignment results
    TCanvas* fc_dy_x; ///< A canvas for plotting LKr-Straw alignment results
    Int_t    fBadCellsMask;
    Bool_t   fReadingData;
    Double_t fEoPEfficiencyThreshold;          ///< Threshold to define the E/p bad bursts (Bad if eff(E/p)<Threshold)
    Double_t fMeanEoPThreshold;                ///< Threshold to define the E/p bad bursts (Bad if mean E/p<Threshold)
    Int_t    fMaxNCellsInDeadClusterThreshold; ///< Threshold to define the maximum allowed number of undetected dead cells
    Int_t    fNManyHitsThreshold;              ///< Threshold to define the events with many hits
    Double_t fDeltaEoPForEfficiency;           ///< MaxDeltaE/p to define E/p matched events (|E/p-1|<MaxDeltaE/p)
    Double_t fArgonionCountsMin;               ///< Minimum ArgonionCounts to consider the burst non-empty
    Double_t fNSelectedTriggersMin;            ///< Minimum NSelectedTriggers to consider the burst non-empty
    Double_t fDeltaEoPPBin;                    ///< DeltaP: for E/p in momentum bins with a width DeltaP
    TString  fOutPDFFileName;

    Int_t    fBurstID;
    Double_t fArgonionCounts;
    Int_t    fNTriggers;
    Int_t    fNSelectedTriggers;
    Double_t fNEoPMatchedPerBurst;
    Double_t fNEoPExpectedPerBurst;
    Double_t fNEoPMatchedStrictPerBurst;
    Double_t fNEoPExpectedStrictPerBurst;
    Int_t    fNHotCellsFromHitMap;
    Int_t    fNHotCellsFromQualityWarnings;
    Int_t    fNHotCellsFromPedestals;
    Int_t    fNDeadCellsFromHitMap;
    Int_t    fMaxNCellsInDeadCluster;

    Int_t    fNBitFlipCells;
    Int_t    fNEventsWithManyHits;

    // Histograms and graphs
    TH1F*         fHNHits;                          // from the reconstructed file
    TH1F*         fHNQualityWarningsVsROChannel;    // from the reconstructed file
    TH2F*         fHEoPMatched;                     // Vs (x,y)
    TH2F*         fHEoPExpected;                    // Vs (x,y)
    TH2F*         fHEoPEfficiency;                  // Vs (x,y)
    TH1F*         fHEoP;
    TH2F*         fHEoPVsP;
    TH2F*         fHEoPVsTrkClsTime;
    TH2F*         fHEoPVsChannel;
    TH2F*         fHdx_vs_y;
    TH2F*         fHdy_vs_x;
    TGraphErrors* fHEoPEfficiencyVsBurstID;
    TGraphErrors* fHEoPEfficiencyStrictVsBurstID;
    TGraphErrors* fHMeanEoPVsBurstID;
    TGraphErrors* fHSigmaEoPVsBurstID;
    TGraphErrors* fHEoPFitPar0VsBurstID;
    TGraphErrors* fHEoPFitPar1VsBurstID;
    TGraphErrors* fHNEoPExpectedVsBurstID;
    TGraphErrors* fHNEoPExpectedStrictVsBurstID;
    TGraphErrors* fHArgonionCountsVsBurstID;
    TGraphErrors* fHNTriggersVsBurstID;
    TGraphErrors* fHNSelectedTriggersVsBurstID;
    TGraphErrors* fHNHotCellsFromHitMapVsBurstID;
    TGraphErrors* fHNHotCellsFromQualityWarningsVsBurstID;
    TGraphErrors* fHNHotCellsFromPedestalsVsBurstID;
    TGraphErrors* fHNDeadCellsFromHitMapVsBurstID;
    TGraphErrors* fHMaxNCellsInDeadClusterVsBurstID;
    TGraphErrors* fHNBitFlipCellsVsBurstID;
    TGraphErrors* fHNEventsWithManyHitsVsBurstID;
    TGraphErrors* fGEoPVsP;
    TGraphErrors* fGdx_vs_y;
    TGraphErrors* fGdy_vs_x;
    TF1*          fFdx_vs_y;
    TF1*          fFdy_vs_x;
};
#endif
