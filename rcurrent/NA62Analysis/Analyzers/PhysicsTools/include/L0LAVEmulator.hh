// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-10-10
//
// ---------------------------------------------------------

#ifndef L0LAVEMULATOR_H
#define L0LAVEMULATOR_H

#include "VL0Emulator.hh"

class L0LAVEmulator : public VL0Emulator {

public:

  explicit L0LAVEmulator(NA62Analysis::Core::BaseAnalysis *ba);
  ~L0LAVEmulator() {};

  void FillTimes();
  void Simple();
  void Detailed();
  void SetPrimitiveIDs(ClusVec::iterator clustit);
  void GenerateAccidentals();
  
  //LAV specific
  void LAVPrimitiveMerging(ClusVec& clusters);
  void AddHitToSplit(ClusVec& SplitClusters, HitVec::iterator hitit);
};

#endif
