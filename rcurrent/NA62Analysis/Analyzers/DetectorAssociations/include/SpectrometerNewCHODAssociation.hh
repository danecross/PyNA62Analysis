// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-07-13
//
// ---------------------------------------------------------------

#ifndef SPECNEWCHODASSO_HH
#define SPECNEWCHODASSO_HH

#include "Analyzer.hh"
#include "SpectrometerNewCHODAssociationOutput.hh"
#include "NewCHODGeometry.hh"
#include "GeometricAcceptance.hh"

class SpectrometerNewCHODAssociation : public NA62Analysis::Analyzer {

public:
  explicit SpectrometerNewCHODAssociation(NA62Analysis::Core::BaseAnalysis *ba);
  ~SpectrometerNewCHODAssociation();
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
  std::vector<SpectrometerNewCHODAssociationOutput> fContainer;
  NewCHODGeometry *fNewCHODGeometry;

  Double_t fScaleFactor; ///< Scale factor to modify the NewCHOD search radius
  Double_t fZNewCHOD;    ///< Z coordinate of the NewCHOD front face
  Double_t fRmin;        ///< Radius of the central hole of the NewCHOD
  Double_t fRmax;        ///< Outer radius of the NewCHOD
};

#endif
