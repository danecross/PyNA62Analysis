// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-10-10
//
// ---------------------------------------------------------

#ifndef L0MUV3EMULATOR_H
#define L0MUV3EMULATOR_H

#include "VL0Emulator.hh"

class L0MUV3Emulator : public VL0Emulator {

public:

  explicit  L0MUV3Emulator(NA62Analysis::Core::BaseAnalysis *ba);
  L0MUV3Emulator(NA62Analysis::Core::BaseAnalysis *ba, TString DetectorName);
  virtual ~L0MUV3Emulator();

  void Simple();
  void Detailed();

  // reimplemented by L0NewCHOD emulator
  virtual void FillTimes();
  virtual void SetPrimitiveIDs(ClusVec::iterator clustit);  
  virtual void GenerateAccidentals();

  // MUV3 specific functions
  void AddHitToSplit(ClusVec& SplitClusters, HitVec::iterator hitit);

  Double_t fFracTight;
  Double_t fFracInner;
  Double_t fFracQuads[3];
  Double_t* fAccidentals;

  TFile* fFile; 
  TH2F*  fHist;  
};

#endif
