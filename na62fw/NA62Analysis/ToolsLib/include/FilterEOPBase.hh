// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-03-31
//
// ---------------------------------------------------------------

#ifndef FILTER_EOPBASE_HH
#define FILTER_EOPBASE_HH

#include "Analyzer.hh"

class FilterEOPBase : public NA62Analysis::Analyzer {

public:
  FilterEOPBase(NA62Analysis::Core::BaseAnalysis *ba, std::string Name);
  ~FilterEOPBase() {}
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void PostProcess() {}
  void DrawPlot() {}

protected:
  Int_t    fMinNTracks; ///< Minimum number of "good" tracks with x1<E/p<x2 to produce a positive decision
  Double_t fMinEOP;     ///< Minimum E/p of a track to be "good" track
  Double_t fMaxEOP;     ///< Maximum E/p of a track to be "good" track
  Double_t fMinZvertex; ///< Min Zvertex wrt nominal beam axis to be "good" track

  // Outputs
  Bool_t fEventSelected;
};

#endif
