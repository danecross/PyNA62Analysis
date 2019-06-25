// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-03-05
//
// ---------------------------------------------------------

#ifndef PHOTONVETOHANDLER_HH
#define PHOTONVETOHANDLER_HH

#include "Analyzer.hh"
#include "LAVMatching.hh"
#include "SAVMatching.hh"
#include "LKrMatching.hh"

class PhotonVetoHandler : public NA62Analysis::Analyzer {

public:
  explicit PhotonVetoHandler(NA62Analysis::Core::BaseAnalysis *ba);
  ~PhotonVetoHandler();
  void InitHist() {}
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t) {}
  void StartOfBurstUser();
  void EndOfBurstUser() {}
  void StartOfRunUser();
  void EndOfRunUser() {}
  void EndOfJobUser() {}
  void PostProcess() {}
  void DrawPlot() {}

  // Parameters
  TString fLAVNoisyChannelsFileName;

  // Outputs
  LAVMatching *fLAVMatching;
  SAVMatching *fSAVMatching;
  LKrMatching *fLKrMatching;
};

#endif
