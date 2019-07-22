// ---------------------------------------------------------------
//
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-11-14
//
// ---------------------------------------------------------------

#ifndef REMOVEMUV3CORNERTILES_HH
#define REMOVEMUV3CORNERTILES_HH

// This analyzer can only be run as a pre-analyzer
#pragma pre-analyzer

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class RemoveMUV3CornerTiles : public NA62Analysis::Analyzer {

public:

  // default analyzer methods
  explicit RemoveMUV3CornerTiles(NA62Analysis::Core::BaseAnalysis *ba);
  ~RemoveMUV3CornerTiles();
  void InitHist();
  void InitOutput();
  void DefineMCSimple();
  void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
  void Process(int iEvent);
  void PostProcess();
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser();
  void EndOfJobUser();
  void DrawPlot();

  // user methods
  Bool_t IsCorner(Int_t Channel);
};
#endif
