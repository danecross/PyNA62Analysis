// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-07-09
//
// Adopted for the CHOD by Viacheslav Duk (Viacheslav.Duk@cern.ch) 26.02.2016
//
// ---------------------------------------------------------------

#ifndef SPECTROMETERCHODASSOCIATION_HH
#define SPECTROMETERCHODASSOCIATION_HH

#include "Analyzer.hh"
#include "SpectrometerCHODAssociationOutput.hh"

class SpectrometerCHODAssociation : public NA62Analysis::Analyzer {

public:
  explicit SpectrometerCHODAssociation(NA62Analysis::Core::BaseAnalysis *ba);
  ~SpectrometerCHODAssociation() {}
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser();
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}

private:
  std::vector<SpectrometerCHODAssociationOutput> fOutput;
  Double_t fSlabWidth[128];
  TString fAlignmentFileName;
  Double_t fOffsetX;
  Double_t fOffsetY;

  void LoadOffsets();
};

#endif
