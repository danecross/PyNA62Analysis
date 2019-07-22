// ---------------------------------------------------------
//
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-01-25
// Updated by Nicolas Lurkin (nicolas.lurkin@cern.ch) 2018-11-27
//
// ---------------------------------------------------------

#ifndef KE3SELECTION_HH
#define KE3SELECTION_HH

#include "Analyzer.hh"
#include "VertexLSF.hh"

class Ke3Selection : public NA62Analysis::Analyzer {

public:
  explicit Ke3Selection(NA62Analysis::Core::BaseAnalysis *ba);
  ~Ke3Selection();
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
  Bool_t       fGTKEnabled;  ///< Do we use GTK with additional selection
  Double_t     fMaxNBursts;  ///< Number of bins in the histograms of counts vs burst ID, default = 5000
  TH1F* fHPhysicsEventsPerBurst;
  TH1F* fHKe3EventsPerBurst;
  TH1F* fHMass;
  TH1F* fHEOP;
  TH1F* fHZvertex;
  TH1F* fHPtot;
  TH1F* fHPttot;
  TH1F* fHEOP_Selected;
  VertexLSF fVertexLSF;

  // Outputs
  Bool_t   fEventSelected;
  Double_t fKe3Time;
  Int_t    fKe3TrackID;

};
#endif

