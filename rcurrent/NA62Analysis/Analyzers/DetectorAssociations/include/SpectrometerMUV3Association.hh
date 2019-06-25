// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-07-09
//
// ---------------------------------------------------------------

#ifndef SPECMUV3ASSO_HH
#define SPECMUV3ASSO_HH

#include "Analyzer.hh"
#include "SpectrometerMUV3AssociationOutput.hh"
#include "GeometricAcceptance.hh"
#include "TRandom2.h"

class SpectrometerMUV3Association : public NA62Analysis::Analyzer {

public:
  explicit SpectrometerMUV3Association(NA62Analysis::Core::BaseAnalysis *ba);
  ~SpectrometerMUV3Association();
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser();
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}

private:
  std::vector<SpectrometerMUV3AssociationOutput> fContainer;
  Double_t fScaleFactor; ///< Scale factor to modify the MUV3 search radius
  Double_t fZMUV3;       ///< Z coordinate of the MUV3 front face
  Double_t fSize;        ///< Half of the transverse size of MUV3
  Double_t fRmin;        ///< Radius of the central hole of MUV3
  Double_t fInefficiencyforMC; ///< MUV3 inefficiency to be emulated for MC
  TRandom2 *fRandom;
};

#endif
