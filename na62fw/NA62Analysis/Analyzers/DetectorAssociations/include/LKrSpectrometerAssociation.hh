// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-11-24
//
// ---------------------------------------------------------

#ifndef LKRSPECASSO_HH
#define LKRSPECASSO_HH

#include "Analyzer.hh"
#include "LKrSpectrometerAssociationOutput.hh"
#include "GeometricAcceptance.hh"

class LKrSpectrometerAssociation : public NA62Analysis::Analyzer {

public:
  explicit LKrSpectrometerAssociation(NA62Analysis::Core::BaseAnalysis *ba);
  ~LKrSpectrometerAssociation() {}
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}

private:
  std::vector<LKrSpectrometerAssociationOutput> fContainer;
  Double_t fMaxClusterTrackDistance; ///< Maximum distance to match cluster to track [mm]
  Double_t fMaxClusterTrackDeltaTime; ///< Maximum time difference to match cluster to track [ns]
};

#endif
