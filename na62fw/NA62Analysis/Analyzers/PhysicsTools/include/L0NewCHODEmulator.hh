// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-10-10
//
// ---------------------------------------------------------

#ifndef L0NEWCHODEMULATOR_H
#define L0NEWCHODEMULATOR_H

#include "L0MUV3Emulator.hh"
#include "TRecoNewCHODHit.hh"

class L0NewCHODEmulator : public L0MUV3Emulator {

public:

  explicit L0NewCHODEmulator(NA62Analysis::Core::BaseAnalysis *ba);
  ~L0NewCHODEmulator();

  void InitHist();
  void InitOutput();

  // reimplemented from L0NewCHODEmulator
  void ReadT0s();
  void FillTimes();
  void SetPrimitiveIDs(ClusVec::iterator clustit);

  // NewCHOD specific functions
  Double_t GetRawTime(TRecoNewCHODHit* hit);
  void AddHitToSplit(ClusVec& SplitClusters, HitVec::iterator hitit);
  void GenerateAccidentals();
  void ParseInputFile(TString fname);
  void SplitOccupancy();

  // NewCHOD specific variables
  TH1F* fQuadrantHist;
  Int_t fEventOccupancy;
  Int_t fSplitOccupancy;
};

#endif
