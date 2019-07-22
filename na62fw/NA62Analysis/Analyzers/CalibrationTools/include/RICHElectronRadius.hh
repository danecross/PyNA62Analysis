// ---------------------------------------------------------------
//
// History:
//
// Created by Viacheslav Duk (Viacheslav.Duk@cern.ch) 16.09.2016
// Revised by Viacheslav Duk (Viacheslav.Duk@cern.ch) 15.03.2018
//
// ---------------------------------------------------------------
#ifndef RICHELECTRONRADIUS_HH
#define RICHELECTRONRADIUS_HH

#include "SpectrometerTrackVertex.hh"
#include "GeometricAcceptance.hh"

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include <TStyle.h>

class TH1I;
class TH2F;
class TGraph;
class TTree;

class RICHElectronRadius : public NA62Analysis::Analyzer {

public:
  explicit RICHElectronRadius(NA62Analysis::Core::BaseAnalysis *ba);
  ~RICHElectronRadius();
  void InitHist();
  void InitOutput() {}
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

  Bool_t fReadingData;
  TH1F*  fHElectronRadius;

};
#endif
