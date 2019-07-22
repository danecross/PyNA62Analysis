// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-10-10
//
// ---------------------------------------------------------

#ifndef L0MUV3EMULATOR_H
#define L0MUV3EMULATOR_H

#include "VL0Emulator.hh"
#include "TRecoMUV3Candidate.hh"

class L0MUV3Emulator : public VL0Emulator {

public:

  explicit  L0MUV3Emulator(NA62Analysis::Core::BaseAnalysis *ba);
  L0MUV3Emulator(NA62Analysis::Core::BaseAnalysis *ba, TString DetectorName);
  virtual ~L0MUV3Emulator();

  void StartOfRunUser();
  void StartOfBurstUser();

  void Process(Int_t);

  void Simple();
  void Detailed();
  void ReadT0s();

  // reimplemented by L0NewCHOD emulator
  virtual void FillTimes();
  virtual void SetPrimitiveIDs(ClusVec::iterator clustit);  
  virtual void GenerateAccidentals();

  // MUV3 specific functions
  Double_t GetRawTime(TRecoMUV3Candidate* cand);
  void AddHitToSplit(ClusVec& SplitClusters, HitVec::iterator hitit);

  Double_t fFracTight;
  Double_t fFracInner;
  Double_t fFracQuads[3];
  Double_t* fAccidentals;

  TFile* fFile; 
  TH2F*  fHist;  

  // T0 corrections
  std::vector<Double_t> fT0Map;
  std::map<Int_t, Double_t> fTDT0Map;
  Double_t fBurstTriggerDriftT0;
  Double_t fEventTriggerDriftT0;
};

#endif
