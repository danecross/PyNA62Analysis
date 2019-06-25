// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-01-22
//
// ---------------------------------------------------------------

#ifndef LKRCLUSTERCORRECTIONS_HH
#define LKRCLUSTERCORRECTIONS_HH

// This analyzer can only be run as a pre-analyzer
#pragma pre-analyzer

#include "Analyzer.hh"
#include "LKrGeometry.hh"

class LKrClusterCorrections : public NA62Analysis::Analyzer {

public:
  explicit LKrClusterCorrections(NA62Analysis::Core::BaseAnalysis *ba);
  ~LKrClusterCorrections();
  void InitHist();
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser();
  void EndOfBurstUser() {}
  void StartOfRunUser();
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}

private:
  Bool_t fWarnOnce;
  Int_t  fFineCalibrationEnabled; ///< Controllable by a command-line argument: 0 = disabled, 1 = enabled (E/p based), 2 = enabled (pi0 mass based)

  TString  fLKrPi0CalibFileName;
  Double_t fFitPar[8];
  Double_t fFitRange[2];

  LKrGeometry *fLKrGeo;
  Double_t fRMSScaleFactor; ///< LKr cluster RMS scale factor: (v1.0.2+)/(older reco versions)
  Double_t fdX, fdY, fPhi; ///< LKr alignment corrections
  TString fRevision; ///< Revision used to reconstruct the data
};

#endif
