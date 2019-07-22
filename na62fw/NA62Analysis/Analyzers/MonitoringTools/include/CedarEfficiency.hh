// ---------------------------------------------------------
//
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-12-24
//
// ---------------------------------------------------------

#ifndef CEDAREFFICIENCY_HH
#define CEDAREFFICIENCY_HH

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

class CedarEfficiency : public NA62Analysis::Analyzer {

  public:

    explicit CedarEfficiency(NA62Analysis::Core::BaseAnalysis *ba);
    ~CedarEfficiency();
    void InitHist();
    void InitOutput() {}
    void DefineMCSimple() {}
    void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
    void Process(Int_t);
    void StartOfBurstUser();
    void EndOfBurstUser();
    void StartOfRunUser();
    void EndOfRunUser() {}
    void EndOfJobUser();
    void PostProcess() {}
    void DrawPlot() {}
    void CreateBadBurstList();
    void BuildPDFReport();

  private:

    TCanvas* fCanvas;
    Bool_t   fReadingData;
    Double_t fEfficiencyThreshold;      ///< Threshold to define the KTAG bad bursts (Bad if eff(NSectorsForEfficiency)<Threshold)
    Int_t    fNSectorsForEfficiency;    ///< Condition to define the KTAG bad bursts
    Double_t fMaxCedarDecayDeltaT;      ///< Max time difference (ns) for the decay-CedarCandidate association, default = 3 ns
    Double_t fArgonionCountsMin;        ///< Minimum ArgonionCounts to consider the burst non-empty
    Double_t fNSelectedTriggersMin;     ///< Minimum NSelectedTriggers to consider the burst non-empty
    TString  fOutPDFFileName;

    Int_t    fBurstID;
    Double_t fDecayTime;
    Double_t fEventTimeStamp;
    Double_t fArgonionCounts;
    Int_t    fNTriggers;
    Int_t    fNSelectedTriggers;
    Double_t ** fNMatchedPerBurst;      // for each NCoincidences, for each selection
    Double_t *  fNExpectedPerBurst;     // for each selection

    // Histograms and graphs
    TH1F**   fHMatched;                          // VsNCoincidences, for each selection
    TH1F**   fHExpected;                         // VsNCoincidences, for each selection
    TH1F**   fHEfficiency;                       // VsNCoincidences, for each selection
    TH1F***  fHMatchedVsDecayTime;               // for each NCoincidences, for each selection
    TH1F**   fHExpectedVsDecayTime;              // for each selection
    TH1F***  fHEfficiencyVsDecayTime;            // for each NCoincidences, for each selection
    TH1F***  fHMatchedVsEventTimeStamp;          // for each NCoincidences, for each selection
    TH1F**   fHExpectedVsEventTimeStamp;         // for each selection
    TH1F***  fHEfficiencyVsEventTimeStamp;       // for each NCoincidences, for each selection
    TGraphErrors*** fHEfficiencyVsBurstID;       // for each NCoincidences, for each selection
    TGraphErrors** fHNExpectedVsBurstID;         // for each selection
    TGraphErrors** fHNExpectedNormVsBurstID;     // for each selection
    TGraphErrors* fHArgonionCountsVsBurstID;
    TGraphErrors* fHNTriggersVsBurstID;
    TGraphErrors* fHNSelectedTriggersVsBurstID;

    // Cedar response
    TH2F** fHCedarDecayDeltaT;
    TH1F** fHCedarNHits;
    TH1F** fHCedarNSectors;

    // Selection definitions for internal use only
    enum Selection{ //bit 0: K2pi, bit 1: K3pi
      K2pi=0,
      K3pi,
      NSELECTIONS //used to count the number of implemented selections
    };

    TString* fSelectionLabels;
};
#endif
