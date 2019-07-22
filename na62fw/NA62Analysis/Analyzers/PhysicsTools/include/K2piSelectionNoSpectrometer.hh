// ---------------------------------------------------------------
//
// History:
//
// Created by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2018-11-12
//
// ---------------------------------------------------------------

#ifndef K2PISELECTIONNOSPECTROMETER_HH
#define K2PISELECTIONNOSPECTROMETER_HH

#include "Analyzer.hh"
#include "SpectrometerTrackVertex.hh"
#include "LAVMatching.hh"
#include "SAVMatching.hh"
#include <stdlib.h>
#include <TCanvas.h>
#include <TLegend.h>
#include <TStyle.h>
#include "TriggerConditions.hh"

class K2piSelectionNoSpectrometer : public NA62Analysis::Analyzer {

public:
  explicit K2piSelectionNoSpectrometer(NA62Analysis::Core::BaseAnalysis *ba);
  ~K2piSelectionNoSpectrometer();
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser();
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}
private:

  TriggerConditions *fTriggerConditions;

  Int_t fTriggerMask; ///< Definition of the data sample by L0 trigger mask
  Bool_t fReadingData; ///< Reading data or my own output?
  Double_t fMaxNBursts;  ///< Number of bins in the histograms of counts vs burst ID, default = 5000
  Int_t fDownscalingCtrl; ///< Downscaling factor of the control trigger

  // Parameters
  Bool_t fUseGTK;
  Double_t fCutTimeDiffMuonPi0;
  Double_t fCutTimeDiffMUV1Pi0;
  Double_t fCutMinEnergyOfMUV1Hit;
  Double_t fCutTimeDiffMUV2Pi0;
  Double_t fCutMinEnergyOfMUV2Hit;
  Int_t fCutMinNHitsMUV12;
  Double_t fCutTimeDiffLKr;
  Double_t fTimeWindowIRC;
  Double_t fTimeWindowSAC;
  Double_t fCutMinPionMass;
  Double_t fCutMaxPionMass;

  // Outputs
  Bool_t   fEventSelected;
  Double_t fK2piTime;
  Int_t    fK2piTrackID;
  TLorentzVector fK2piPionFourMomentum;
  TVector3 fK2piVertexPosition;
};
#endif

