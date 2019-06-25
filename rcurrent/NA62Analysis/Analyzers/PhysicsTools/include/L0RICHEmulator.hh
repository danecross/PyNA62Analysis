// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-10-10
//
// ---------------------------------------------------------

#ifndef L0RICHEMULATOR_H
#define L0RICHEMULATOR_H

#include "VL0Emulator.hh"

class L0RICHEmulator : public VL0Emulator {

public:

  explicit L0RICHEmulator(NA62Analysis::Core::BaseAnalysis *ba);
  L0RICHEmulator(NA62Analysis::Core::BaseAnalysis *ba, TString DetectorName);
  ~L0RICHEmulator() {};

  void Simple();
  void Detailed();
  void SetPrimitiveIDs(ClusVec::iterator clustit);
  void ClusterAlgo(HitVec& hits, ClusVec& result);
  void GenerateAccidentals();

  virtual void FillTimes(); // reimplemented by L0CHODEmulator

  //RICH specific
  void MultiplicityCut(ClusVec& clusters, Int_t Threshold);
  void SetTimesToAverage(ClusVec& clusters);
  void AddHitToSplit(ClusVec& SplitClusters, HitVec::iterator hitit);

  Int_t fPPThreshold;
  Int_t fSLThreshold;
  Double_t fFracPP[3];
};

#endif
