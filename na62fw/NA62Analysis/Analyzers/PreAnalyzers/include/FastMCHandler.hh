// ---------------------------------------------------------------
// History:
//
// Created by Viacheslav Duk (Viacheslav.Duk@cern.ch) 15.06.2018
// --------------------------------------------------------------- 

#ifndef FASTMCHANDLER_HH
#define FASTMCHANDLER_HH

// This analyzer can only be run as a pre-analyzer
#pragma pre-analyzer

#include "Analyzer.hh"
#include "MUV3Geometry.hh"
#include "NewCHODGeometry.hh"
#include "TRandom2.h"

class FastMCHandler : public NA62Analysis::Analyzer {

public:
  explicit FastMCHandler(NA62Analysis::Core::BaseAnalysis *ba);
  ~FastMCHandler();
  void InitHist() {}
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void PostProcess();
  void StartOfBurstUser();
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser() {}
  void DrawPlot() {}

private:
  Double_t fZMUV3, fZNewCHOD, fZLKr;
  Double_t fMultScatteringParameter; ///< Multuple scattering effect in MUV3 plane [MeV*mm]
  MUV3Geometry* fMUV3Geometry;
  NewCHODGeometry* fNewCHODGeometry;
  Bool_t   fFastSimulation; ///< Is a fast-simulated sample being processsed?
  TRandom2 *fRandom;  
  Bool_t fGenerateMUV3HitsFromPions;
  Bool_t fGenerateMuonMultipleScattering;
  Double_t GenerateDistanceToTile(Double_t, Double_t);
};
#endif
