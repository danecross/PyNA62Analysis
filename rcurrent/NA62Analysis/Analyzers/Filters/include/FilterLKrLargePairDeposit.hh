// ---------------------------------------------------------------
//
// History:
//
// Created by Jurgen Engelfried (Jurgen.Engelfried@cern.ch) 2019-03-25
//
// ---------------------------------------------------------------

#ifndef FILTER_LKR_LARGE_PAIR_DEPOSIT_HH
#define FILTER_LKR_LARGE_PAIR_DEPOSIT_HH

#include "Analyzer.hh"

class FilterLKrLargePairDeposit : public NA62Analysis::Analyzer {

public:
  explicit FilterLKrLargePairDeposit(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterLKrLargePairDeposit() {}
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
  Int_t fDownscaling; ///< Output donwscaling factor
  Double_t fMinimumEnergy;  ///< Energy threshold for filtering
  Double_t fInTime;  // Timing cut for intime clusters

};

#endif
