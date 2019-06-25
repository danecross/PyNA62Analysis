// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-09-15
//
// ---------------------------------------------------------------

#ifndef SPECLKRASSO_HH
#define SPECLKRASSO_HH

#include "Analyzer.hh"
#include "SpectrometerLKrAssociationOutput.hh"
#include "GeometricAcceptance.hh"

class SpectrometerLKrAssociation : public NA62Analysis::Analyzer {

public:
  explicit SpectrometerLKrAssociation(NA62Analysis::Core::BaseAnalysis *ba);
  ~SpectrometerLKrAssociation() {}
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
  std::vector<SpectrometerLKrAssociationOutput> fContainer;
  Double_t fMaxTrackClusterDistance; ///< Maximum distance to match track to cluster [mm]
  Double_t fMaxTrackClusterDeltaTime; ///< Maximum time difference to match track to cluster [ns]
  Double_t fZLKr; ///< LKr front plane Z coordinate [mm], obtained from GeometricAcceptance class
  Bool_t   fMonitoringHistograms; ///< Produce additional monitoring histgrams or not?
};

#endif
