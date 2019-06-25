// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-10-28
//
// ---------------------------------------------------------------

#ifndef SPECMUV12ASSO_HH
#define SPECMUV12ASSO_HH

#include "Analyzer.hh"
#include "SpectrometerMUV12AssociationOutput.hh"
#include "GeometricAcceptance.hh"

class SpectrometerMUV12Association : public NA62Analysis::Analyzer {

public:
  explicit SpectrometerMUV12Association(NA62Analysis::Core::BaseAnalysis *ba);
  ~SpectrometerMUV12Association() {}
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
  std::vector<SpectrometerMUV12AssociationOutput> fContainer;
  Double_t fMaxTrackClusterDistance1; ///< Maximum distance to match track to cluster in MUV1 [mm]
  Double_t fMaxTrackClusterDistance2; ///< Maximum distance to match track to cluster in MUV2 [mm]
  Double_t fZMUV1; ///< MUV1 front plane Z coordinate [mm], read from GeometricAcceptance class
  Double_t fZMUV2; ///< MUV2 front plane Z coordinate [mm], read from GeometricAcceptance class
};

#endif
