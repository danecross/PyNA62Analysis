// ------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-03
//
// ------------------------------------------------------------------

#include "T0Evaluation.hh"

#ifndef NewCHODT0_H
#define NewCHODT0_H 1

class NewCHODT0 : public T0Evaluation {

public:

  explicit NewCHODT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~NewCHODT0() {}

  void RequestUserHistograms();
  void GenerateUserPDFReport();

private:

  TH1F *fHTileDeltaTime[152];
  TH2F *fHTileT1T2[152];
  TH1D *fHTileTightRecoHit[152];
  TH2F *fHTileTight;

  Bool_t fAllTileHistogramsFound;
};

#endif
