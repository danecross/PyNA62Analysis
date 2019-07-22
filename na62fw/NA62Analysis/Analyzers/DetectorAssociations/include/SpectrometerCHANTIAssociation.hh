// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2018-01-28
//
// ---------------------------------------------------------

#ifndef SPECCHANTIASSO_HH
#define SPECCHANTIASSO_HH

#include "Analyzer.hh"
#include "SpectrometerCHANTIAssociationOutput.hh"
#include "GeometricAcceptance.hh"

class SpectrometerCHANTIAssociation : public NA62Analysis::Analyzer {

public:
  explicit SpectrometerCHANTIAssociation(NA62Analysis::Core::BaseAnalysis *ba);
  ~SpectrometerCHANTIAssociation() {}
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

  std::vector<SpectrometerCHANTIAssociationOutput> fContainer;
  Double_t fMaxTrackCandidateDistance; ///< Maximum distance to match cluster to track [mm]

  Double_t fZCHANTI;       ///< Z coordinate of the CHANTI front face
  Double_t fSize;          ///< Half of the transverse size of CHANTI
  Double_t fSizeHoleX;     ///< Half of the transverse x size of CHANTI hole
  Double_t fSizeHoleY;     ///< Half of the transverse y size of CHANTI hole
};

#endif
