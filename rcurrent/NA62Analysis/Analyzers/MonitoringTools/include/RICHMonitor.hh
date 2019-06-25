// ---------------------------------------------------------------
//
// History:
//
// Created by Roberta Volpe (roberta.volpe@cern.ch) December 2016
//
// ---------------------------------------------------------------

#ifndef RICHMONITOR_HH
#define RICHMONITOR_HH

#include "Analyzer.hh"
#include "SpectrometerTrackVertex.hh"
#include "GeometricAcceptance.hh"
#include "DownstreamTrack.hh"
#include "BeamParameters.hh"
#include "TwoLinesCDA.hh"
#include <stdlib.h>
#include <TCanvas.h>
#include <TLegend.h>
#include <TStyle.h>

class TRecoRICHEvent;

class RICHMonitor : public NA62Analysis::Analyzer {

public:
  explicit RICHMonitor(NA62Analysis::Core::BaseAnalysis *ba);
  ~RICHMonitor();
  void InitHist();
  void InitOutput(){};
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser() {}
  void EndOfJobUser();
  void BuildPDFReport();
  void PostProcess() {}
  void DrawPlot() {}

private:

  Int_t   fRunNumber;
  TString fInputFileName;
  TwoLinesCDA  *fCDAcomp;

  Int_t        fTriggerMask; ///< Definition of the data sample by L0 trigger mask
  Bool_t       fReadingData; ///< Reading data or my own output?
  Double_t     fMaxNBursts;  ///< Number of bins in the histograms of counts vs burst ID, default = 5000
  Double_t fMinEventsInBurst;

  TH2F *fRecoHitTimeWrtReferenceVsReadoutChannelNoT0;
  TH2F *fRecoHitTimeWrtReferenceVsReadoutChannel;
  TH2F *fRecoHitTimeWrtReferenceVsSeqChannelNoT0;
  TH2F *fRecoHitTimeWrtReferenceVsSeqChannel;

  TH2F *fDigiTimeRawFineVsROChannel;
  TH1F *fHTimeRawJUP_PerBurst;
  TH1F *fHTimeRawJDW_PerBurst;
  TH1F *fHTimeRawSUP_PerBurst;
  TH1F *fHTimeRawSDW_PerBurst;
  TH1F *fHTimeRawMUL_PerBurst;

  TH1F *fHSigmaTimeRawJUP_PerBurst;
  TH1F *fHSigmaTimeRawJDW_PerBurst;
  TH1F *fHSigmaTimeRawSUP_PerBurst;
  TH1F *fHSigmaTimeRawSDW_PerBurst;
  TH1F *fHSigmaTimeRawMUL_PerBurst;

  TH1F *fHTotalEventsPerBurst;
  TH1F *fHPhysicsEventsPerBurst;
  TH1F *fHControlEventsPerBurst;
  TH1F *fhNSCHitsPerBurst;
  TH1F *fhNPMHitsPerBurst;
  TH1F *fhNPMTimeCandPerBurst;
  TH1F *fhNPMTimeCandPerBurst_phys;
  TH1F *fhNSCHitsInTimePerBurst;
  TH1F *fhNPMHitsInTimePerBurst;
  TH1F *fhNSCHitsInTimePerBurst_phys;
  TH1F *fhNPMHitsInTimePerBurst_phys;
  TH1F *fhNPMTimeCandInTimePerBurst;
  TH1F *fhNPMTimeCandInTimePerBurst_phys;
  TH1F *fhNSCHitsPerBurst_phys;
  TH1F *fhNPMHitsPerBurst_phys;
  TH1F *fHNSCHitsPerControlEvent;
  TH1F *fHNPMHitsPerControlEvent;
  TH1F *fHNPMTimeCandPerControlEvent;
  TH1F *fHNSCHitsInTimePerControlEvent;
  TH1F *fHNPMHitsInTimePerControlEvent;
  TH1F *fHNSCHitsPerPhysicsEvent;
  TH1F *fHNPMHitsPerPhysicsEvent;
  TH1F *fHNPMTimeCandPerPhysicsEvent;
  TH1F *fHNSCHitsInTimePerPhysicsEvent;
  TH1F *fHNPMHitsInTimePerPhysicsEvent;
  TH1F *fHNPMTimeCandInTimePerControlEvent;
  TH1F *fHNPMTimeCandInTimePerPhysicsEvent;
  TH1F *fhNElecRing_vs_BurstID;
  TH1F *fhNPiRing_vs_BurstID;
  TH1F *fhNMuRing_vs_BurstID;
  TH1F *fhNElecTrack_vs_BurstID;
  TH1F *fhNPiTrack_vs_BurstID;
  TH1F *fhNMuTrack_vs_BurstID;
  TH1F *fhNSCHitsPerBurstID;
  TH1F *fhNPMHitsPerBurstID;
  TH1F *fhPMHitQualityBadBurst;
  TH1F *fhSCHitQualityBadBurst;
  TH1F *fhPhysEvWith0SCHitsPerBurst;
  TH1F *fhPhysEvWith0SCHitsPerBurst_EvQual0;
  TH1F *fhPhysEvWith0SCHitsPerBurst_EvQualNot0;
  TH1F *fhNPMTimeCandPerBurstID;
  TH1F *fhEffiElRing_vs_BurstID;
  TH1F *fhEffiMuRing_vs_BurstID;
  TH1F *fhEffiPiRing_vs_BurstID;
  TH1F* fhNRICHEventPMHits;
  TH1F* fhNRICHEventSCHits;
  TH1F* fhNRICHEventPMHits_phys;
  TH1F* fhNRICHEventSCHits_phys;
  TH1F* fhPMHitQuality;
  TH1F* fhSCHitQuality;
  TH1F* fhNRICHTimeCand;
  TH1F* fhNRICHRingCand;
  TH1F* fhRICHTimeCandNhits;
  TH1F* fhTimeCandTime;
  TH1F* fhRingCandTime;
  TH1F* fhNRICHSingleRingCand;
  TH1F* fhDeltaT_t1t2;
  TH1F* fhDeltaT_RICHCand_CedarCand;
  TH1F* fhDeltaT_CedarCand_RICHHit;
  TH2F *fhDeltaT_RICHHitRichCand_vs_SeqID;
  TH2F *fhDeltaT_RICHHitCedarCand_vs_SeqID;

  TH1F* fhPAllPiTracks;
  TH1F* fhPPiTracks;
  TH1F* fhPAllMuTracks;
  TH1F* fhPMuTracks;
  TH1F* fhPAllElecTracks;
  TH1F* fhPElecTracks;

  TH1F *fhSingleRing_Dist;
  TH1F *fhMuRing_RICHMass;
  TH1F *fhPiRing_RICHMass;
  TH1F *fhElecRing_RICHMass;
  TH1F *fhPEvtSelTracks;
  TH1F *fhSingleRing_RICHMass;

  TH1F *fhPiRing_Radius;
  TH1F *fhPiRing_nHits;
  TH2F *fhPiRing_nHits_vs_p;
  TH1F *fhMuRing_Radius;
  TH1F *fhMuRing_nHits;
  TH2F *fhMuRing_nHits_vs_p;
  TH1F *fhElecRing_Radius;
  TH1F *fhElecRing_nHits;
  TH2F *fhElecRing_nHits_vs_p;

  TH2F *fhSingleRing_p_vs_RICHMass;
  TH2F *fhPiRing_p_vs_RICHMass;
  TH2F *fhMuRing_p_vs_RICHMass;
  TH2F *fhElecRing_p_vs_RICHMass;

  TH2F *fhSingleRing_Radius_vs_p;
  TH2F *fhPiRing_Radius_vs_p;
  TH2F *fhMuRing_Radius_vs_p;
  TH2F *fhElecRing_Radius_vs_p;
  TRecoRICHEvent *fRICHEvent;
  TRecoSpectrometerCandidate* fSpectrometerCand;

};
#endif

