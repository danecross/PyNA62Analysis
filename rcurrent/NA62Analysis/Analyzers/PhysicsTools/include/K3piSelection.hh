// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-02-19
//
// ---------------------------------------------------------------

#ifndef K3PISELECTION_HH
#define K3PISELECTION_HH

#include "Analyzer.hh"
#include "TriggerConditions.hh"
#include "L0PrimitiveHandler.hh"
#include "SpectrometerGigaTrackerMatchingTool.hh"
#include "TGraphErrors.h"

class K3piSelection : public NA62Analysis::Analyzer {

public:
  explicit K3piSelection(NA62Analysis::Core::BaseAnalysis *ba);
  ~K3piSelection();
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}
  void BuildPDFReport();
  void ProcessEOBEvent();

private:

  Double_t BinomialError(Double_t, Double_t);

  TriggerConditions *fTriggerConditions;
  L0PrimitiveHandler *fPrimitiveHandler;
  SpectrometerGigaTrackerMatchingTool *fSG;

  Bool_t   fReadingData; ///< Reading data or my own output?
  Int_t    fMaxNBursts;  ///< Number of bins in the histograms of counts vs burst ID, default = 5000
  Int_t    fMinRunID;    ///< Minimum run ID for histograms vs runID
  Int_t    fMaxRunID;    ///< Maximum run ID for histograms vs runID
  Bool_t   fPrintFlux;   ///< Print flux and proton-on-target information for each burst in --histo mode?
  Bool_t   fFastSimulation; ///< Processing fast simulation output or not?

  // Parameters for kaon flux and POT computation
  Double_t fZAcceptedMin;     ///< Lower z limit for K3pi selection
  Double_t fZAcceptedMax;     ///< Upper z limit for K3pi selection
  Double_t fZCedar;           ///< z position of the Cedar front [mm]
  Double_t fZGTK3;            ///< z position of GTK3 station [mm]
  Double_t fZFiducialMin;     ///< Standard lower FV limit for 3-track analyses [mm]
  Double_t fZFiducialMax;     ///< Standard upper FV limit for 3-track analyses [mm]
  Double_t fZLKr;             ///< LKr face Z position [mm]
  Double_t fZArgonion;        ///< Argonion counter Z position [mm]
  Double_t fKaonMeanPath;     ///< Kaon mean kaon path at 75 GeV/c
  Double_t fPionMeanPath;     ///< Pion mean kaon path at 75 GeV/c
  Double_t fFVCedarConv;      ///< Conversion of K+ rate at Cedar front to FV start
  Double_t fArgonionFVConvK;  ///< Conversion of K+ rate at FV start to Argonion
  Double_t fArgonionFVConvPi; ///< Conversion of pi+ rate at FV start to Argonion
  Double_t fDecayProb;        ///< Decay probability in the fiducial volume
  Double_t fAcceptance;       ///< Acceptance of this selection
  Double_t fBRK3pi;           ///< BR(K3pi)
  Double_t fPOT_to_Kaon;      ///< Protons-on-target to kaons at FV entrance conversion

  // Beam composition at the FV entrance and at the Argonion counter
  Double_t fPionFractionFV;
  Double_t fProtonFractionFV;
  Double_t fKaonFractionFV;
  Double_t fPionFractionArgonion;
  Double_t fProtonFractionArgonion;
  Double_t fKaonFractionArgonion;
  Double_t fKaon_to_Argonion; ///< Kaons at FV entrance to argonion count conversion

  Double_t fSpillLength; ///< Effective spill length [s]
  Double_t fMinTimeInSpill, fMaxTimeInSpill; ///< in each burst
  Int_t    fDownscaling1;      ///< Downscaling factor of the control trigger
  Int_t    fDownscaling2;      ///< Downscaling factor of the multi-track trigger (RICH*QX)
  Int_t    fTriggerMultiTrack; ///< ID of the multi-track trigger (RICH*QX)
  Int_t    fStartOfRunTime;    ///< Unix timestamp of the first burst of the run

  TH1F *fHZtrue;
  TH1F *fHMass;
  TH1F *fHMomentum;
  TH1F *fHdxdz;
  TH1F *fHdydz;
  TH1F *fHx; ///< Histogram of kaon x position at z=101.8m
  TH1F *fHy; ///< Histogram of kaon y position at z=101.8m
  TH1F *fHBurstID; ///< Number of bursts fonnd (correct also when reading list with several runs)
  TH1F *fHEventsPerBurst; ///< Total number of events in each burst
  TH1F *fHPhysicsEventsPerBurst; ///< Number of physics trigger events in each burst
  TH1F *fHControlEventsPerBurst; ///< Number of control trigger events in each burst
  TH1F *fHK3piEventsPerBurstControlTrigger; ///< Number of selected K3pi events (control trigger)
  TH1F *fHK3piEventsPerBurstControlTriggerQM0; ///< Number of selected K3pi events (control trigger, quality mask = 0)
  TH1F *fHK3piEventsPerBurstMultiTrackTrigger; ///< Number of selected K3pi events (multi-track trigger)
  TH1F *fHKaonRateKTAG; ///< Kaon rate at FV entrance [MHz] computed by Reco from KTAG rate
  TH1F *fHKaonRateArgonion; ///< Kaon rate at FV entrance [MHz] computed from Argonion counts

  TH1F *fTrigAllRICH, *fTrigEffRICH, *fTrigEfficiencyRICH;
  TH1F *fTrigAllQX, *fTrigEffQX, *fTrigEfficiencyQX;
  TH1F *fTrigL1All, *fTrigL1KTAG, *fTrigL1LAV, *fTrigL1STRAW;
  TH1F *fTrigL1EfficiencyKTAG, *fTrigL1EfficiencySTRAW;
  TH1F *fEmulatedL0QX;  ///< Histogram of the emulated QX efficiency numerator vs run number
  TH1F *fEmulatedL0All; ///< Histogram of the emulated QX efficiency denominator vs run number

  TF1  *fFMomentum;
  TF1  *fFdxdz;
  TF1  *fFdydz;

  TGraphErrors *fGraph_K3pi_CTL;
  TGraphErrors *fGraph_K3pi_MUL;
  TGraph *fGraph_K3pi_KTAG;
  TGraph *fGraph_K3pi_Argo;

  // Outputs
  Bool_t   fEventSelected;  ///< Did the event pass K3pi selection?
  Int_t    fVertexID;       ///< ID of the SpectrometerTrackVertex selected as K3pi vertex
  TVector3 fVertexPosition; ///< Vertex position
  TVector3 fTotalMomentum;  ///< Total momentum of the three tracks
  Double_t fM3pi;           ///< 3pi invariant mass
  Double_t fK3piTime;       ///< Mean time of CHOD candidates associated to the vertex tracks
  Int_t fTrackIndex0;
  Int_t fTrackIndex1;
  Int_t fTrackIndex2;
};

#endif
