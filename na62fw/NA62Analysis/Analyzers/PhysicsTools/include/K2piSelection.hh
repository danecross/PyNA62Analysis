// ---------------------------------------------------------------
//
// History:
//
// Updated by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2017-10-27
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2016-08-08
//
// ---------------------------------------------------------------

#ifndef K2PISELECTION_HH
#define K2PISELECTION_HH

#include "Analyzer.hh"
#include "SpectrometerTrackVertex.hh"
#include "LAVMatching.hh"
#include "SAVMatching.hh"
#include "Pi0Selection.hh"
#include <stdlib.h>
#include <TCanvas.h>
#include <TLegend.h>
#include <TStyle.h>
#include "TriggerConditions.hh"

class K2piSelection : public NA62Analysis::Analyzer {

public:
  explicit K2piSelection(NA62Analysis::Core::BaseAnalysis *ba);
  ~K2piSelection();
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser();
  void EndOfRunUser() {}
  void EndOfJobUser();
  void BuildPDFReport();
  void PostProcess() {}
  void DrawPlot() {}
  void PrintStatisticsPerBurst();
private:

  TriggerConditions *fTriggerConditions;

  Int_t fTriggerMask; ///< Definition of the data sample by L0 trigger mask
  Bool_t fReadingData; ///< Reading data or my own output?
  Double_t fMaxNBursts;  ///< Number of bins in the histograms of counts vs burst ID, default = 5000
  Int_t fDownscalingCtrl; ///< Downscaling factor of the control trigger
  Int_t    fStartOfRunTime;    ///< Unix timestamp of the first burst of the run
  TH1F* fHPhysicsEventsPerBurst;
  TH1F* fHK2piEventsPerBurst;
  TH1F* fHK2piEventsPerBurstControlTrigger; ///< Number of selected K2pi events (control trigger)
  TH1F* fHK2piEventsPerBurstControlTriggerQM0; ///< Number of selected K2pi events (control trigger, quality mask = 0)
  TH1F* fHMass;
  TH1F* fHEOP;
  TH1F* fHZvertex;
 
  // Parameters
  Bool_t fUseGTK;
  Double_t fTimeWindowIRC;
  Double_t fTimeWindowSAC;
  Double_t fCutMinPionMass;
  Double_t fCutMaxPionMass;

  // Parameters for kaon flux and POT computation
  Double_t fZFiducialMin;     ///< Standard lower FV limit for 3-track analyses [mm]
  Double_t fZFiducialMax;     ///< Standard upper FV limit for 3-track analyses [mm]
  Double_t fKaonMeanPath;     ///< Kaon mean kaon path at 75 GeV/c
  Double_t fPionMeanPath;     ///< Pion mean kaon path at 75 GeV/c
  Double_t fDecayProb;        ///< Decay probability in the fiducial volume
  Double_t fAcceptance;       ///< Acceptance of this selection
  Double_t fBRK2pi;           ///< BR(K2pi)
  Double_t fPOT_to_Kaon;      ///< Protons-on-target to kaons at FV entrance conversion

  // Outputs
  Bool_t   fEventSelected;
  Double_t fK2piTime;
  Int_t    fK2piTrackID;
  TLorentzVector fK2piPionFourMomentum;
  TLorentzVector fK2piKaonFourMomentum;
  TVector3 fK2piVertexPosition;
  Pi0SelectionOutput fPi0SelectionOutput ;
};
#endif

