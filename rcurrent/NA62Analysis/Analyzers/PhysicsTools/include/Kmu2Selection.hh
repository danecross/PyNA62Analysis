// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-03-25
//
// ---------------------------------------------------------------

#ifndef KMU2SELECTION_HH
#define KMU2SELECTION_HH

#include "Analyzer.hh"
#include "SpectrometerTrackVertex.hh"
#include "GeometricAcceptance.hh"
#include <stdlib.h>
#include <TCanvas.h>
#include <TLegend.h>
#include <TStyle.h>

class Kmu2Selection : public NA62Analysis::Analyzer {

public:
  explicit Kmu2Selection(NA62Analysis::Core::BaseAnalysis *ba);
  ~Kmu2Selection();
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void BuildPDFReport();
  void PostProcess() {}
  void DrawPlot() {}
  void PrintStatisticsPerBurst();
  
private:
  Int_t        fTriggerMask; ///< Definition of the data sample by L0 trigger mask
  Bool_t       fReadingData; ///< Reading data or my own output? 
  Double_t     fMaxNBursts;  ///< Number of bins in the histograms of counts vs burst ID, default = 5000
  TH1F* fHPhysicsEventsPerBurst;
  TH1F* fHKmu2EventsPerBurst;
  TH1F* fHMass;
  TH1F* fHEOP;
  TH1F* fHZvertex;

  // Outputs
  Bool_t fEventSelected;
  Double_t fMuonMomentum;
};
#endif
