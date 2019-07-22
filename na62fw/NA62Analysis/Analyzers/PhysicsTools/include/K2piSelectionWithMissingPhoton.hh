// ------------------------------------------------------------
// K2pi Selection with  missing photon in LKr
// Created by Francesco Brizioli (francesco.brizioli@cern.ch)
// March 2019
// ------------------------------------------------------------

#ifndef K2PISELECTIONWITHMISSINGPHOTON_HH
#define K2PISELECTIONWITHMISSINGPHOTON_HH

#include "Analyzer.hh"
#include <stdlib.h>
#include "GigaTrackerRecoAlgorithm.hh"
#include "SpectrometerGigaTrackerMatchingTool.hh"

struct PhotonCandidate{
  Int_t ClusterID;
  TLorentzVector FourMomentum;
  Double_t Time;
};

struct K2piSelectionWithMissingPhotonOutput {
  TLorentzVector MissingPhotonFourMomentum ;
  TVector3 DecayVertex ;
  Double_t EventTime ;
};

class K2piSelectionWithMissingPhoton : public NA62Analysis::Analyzer {

public:
  explicit  K2piSelectionWithMissingPhoton(NA62Analysis::Core::BaseAnalysis *ba);
  ~K2piSelectionWithMissingPhoton();
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void BuildPDFReport() {}
  void PostProcess();
  void DrawPlot() {}
  void PrintStatisticsPerBurst() {}
  void CreateBadBurstList() {}

private:

  Int_t fTriggerMask; /// < Definition of the data sample by L0 trigger mask
  Bool_t fReadingData; /// < Reading data or my own output? 
  Int_t fBurstID;
  Int_t fRunID;

  SpectrometerGigaTrackerMatchingTool* fSG;
  GigaTrackerRecoAlgorithm* fGTKAlgo;
  
  // analyzer output definition
  K2piSelectionWithMissingPhotonOutput fSelectedK2piWithMissingPhoton ;

};
#endif

