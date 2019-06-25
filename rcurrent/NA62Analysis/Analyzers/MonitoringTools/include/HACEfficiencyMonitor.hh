#ifndef HACEFFICIENCYMONITOR_HH
#define HACEFFICIENCYMONITOR_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TGraphErrors.h>
#include "GeometricAcceptance.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class HACEfficiencyMonitor : public NA62Analysis::Analyzer {
public:
  explicit HACEfficiencyMonitor(NA62Analysis::Core::BaseAnalysis *ba);
  ~HACEfficiencyMonitor();
  void InitHist();
  void InitOutput(){}
  void DefineMCSimple(){}
  void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
  void Process(int iEvent);
  void PostProcess(){}
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser(){}
  void EndOfRunUser(){}
  void EndOfJobUser();
  void DrawPlot(){}
  void BuildPFDReport();
  void CreateBadBurstList();

public:
  Int_t IsGoodStrawCandidate(TRecoSpectrometerCandidate *fCand);
  Int_t IsGoodCHODCandidate(TRecoSpectrometerCandidate *fCand, Double_t *fTime1);
  Int_t IsGoodKTAGCandidate(Double_t fTime1, Double_t *fKTAGTime);
  Int_t IsGoodGTKCandidate(TVector3 postrack, TLorentzVector ptrack, Double_t fTime1, TLorentzVector *kaonmom, TVector3 *kaonpos);
  Int_t IsInAcceptanceDownstream(TVector3 momentum, TVector3 position, Int_t charge);

  TLorentzVector Get4Momentum(Double_t pmom, Double_t thetax, Double_t thetay, Double_t mass);
  TVector3 MultiTrackVertexSimple(Int_t, TLorentzVector*, TVector3*, Double_t*);
  TVector3 Propagate(Int_t, TVector3 *, TVector3 *, Double_t *, Double_t, Int_t);
protected:
  TRecoCedarEvent *fCedarEvent;
  TRecoGigaTrackerEvent *fGigaTrackerEvent;
  TRecoCHODEvent *fCHODEvent;
  TRecoSpectrometerEvent *fSpectrometerEvent;
  TRecoHACEvent *fHACEvent;

private:
  TCanvas *fCanvas;
  TString fOutPDFFileName;
  Bool_t fReadingData;
  Double_t fArgonionCountsMin; ///< Minimum ArgonionCounts to consider the burst non-empty     
  Double_t fNSelectedTriggersMin; ///< Minimum NSelectedTriggers to consider the burst non-empty
  Double_t fEfficiencyThreshold;

  Int_t fBurstID;
  Double_t fArgonionCounts;
  Int_t fNTriggers;
  Int_t fNSelectedTriggers;
  Double_t fNMatchedPerBurst;
  Double_t fNExpectedPerBurst;
     
  TH2F* fHMatched_front; // Vs (x,y)
  TH2F* fHMatched_middle; // Vs (x,y)
  TH2F* fHMatched_back; // Vs (x,y)

  TH2F* fHExpected_front; // Vs (x,y)
  TH2F* fHExpected_middle; // Vs (x,y)
  TH2F* fHExpected_back; // Vs (x,y)

  TH2F* fHEfficiency_front; // Vs (x,y)
  TH2F* fHEfficiency_middle; // Vs (x,y)
  TH2F* fHEfficiency_back; //Vs (x,y)         
  
  TGraphErrors* fHEfficiencyVsBurstID;
  TGraphErrors* fHExpectedVsBurstID;
  TGraphErrors* fHArgonionCountsVsBurstID;
  TGraphErrors* fHNTriggersVsBurstID;
  TGraphErrors* fHNSelectedTriggersVsBurstID;
};
#endif
