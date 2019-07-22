// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-10-10
//
// ---------------------------------------------------------

#ifndef L0CALOEMULATOR_H
#define L0CALOEMULATOR_H

#include "VL0Emulator.hh"

class L0CaloEmulator : public VL0Emulator {

public:

  explicit L0CaloEmulator(NA62Analysis::Core::BaseAnalysis *ba);
  ~L0CaloEmulator() {};

  void FillTimes();
  void Simple();
  void Detailed();
  void SetPrimitiveIDs(ClusVec::iterator clustit);
  void GenerateAccidentals();
};

#endif
