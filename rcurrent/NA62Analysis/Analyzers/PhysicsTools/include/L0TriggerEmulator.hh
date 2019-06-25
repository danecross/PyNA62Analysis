// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-10-10
//
// ---------------------------------------------------------

#ifndef L0TRIGGEREMULATOR_H
#define L0TRIGGEREMULATOR_H

#include "VL0Emulator.hh"

class L0TriggerEmulator : public VL0Emulator {

public:

  explicit L0TriggerEmulator(NA62Analysis::Core::BaseAnalysis *ba);
  ~L0TriggerEmulator() {};

  void InitHist();
  void Process(int iEvent);
  void PostProcess();
  void EndOfJobUser();

  // reimplemented from VL0Emulator
  void FillTimes();
  void Simple();
  void Detailed();
  void SetPrimitiveIDs(ClusVec::iterator clustit);
  void GenerateAccidentals();

  // L0TriggerEmulator specific 
  void Configure();
  
  Double_t fL0TPLAV;
  Double_t fL0TPCalo;
  Double_t fL0TPMUV3;
  Double_t fL0TPNewCHOD;
};

#endif
