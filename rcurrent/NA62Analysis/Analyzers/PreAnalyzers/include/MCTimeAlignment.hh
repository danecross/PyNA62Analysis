// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2018-03-02
// ---------------------------------------------------------------

#ifndef MCTimeAlignment_HH
#define MCTimeAlignment_HH

// This analyzer can only be run as a pre-analyzer
#pragma pre-analyzer

#include "Analyzer.hh"

class MCTimeAlignment : public NA62Analysis::Analyzer {

public:
  explicit MCTimeAlignment(NA62Analysis::Core::BaseAnalysis *ba);
  ~MCTimeAlignment() {}
  void InitHist() {}
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser();
  void EndOfRunUser() {}
  void EndOfJobUser() {}
  void PostProcess() {}
  void ProcessEOBEvent() {}
  void DrawPlot() {}

private:
  Bool_t   fWarnOnce;
  Double_t fCedarOffset; ///< Time offset to be applied to Cedar hits and candidates [ns]
  Double_t fRICHOffset;  ///< Time offset to be applied to RICH hits and candidates [ns]
  TString  fMCRevision;  ///< MC revision: time offsets depend on it
};

#endif
