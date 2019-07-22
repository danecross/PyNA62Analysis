#ifndef PnnKinematicTails_AdditionalTimeAlignment_HH
#define PnnKinematicTails_AdditionalTimeAlignment_HH

// This analyzer can only be run as a pre-analyzer
#pragma pre-analyzer

#include "Analyzer.hh"

class PnnKinematicTails_AdditionalTimeAlignment : public NA62Analysis::Analyzer {

public:
  explicit PnnKinematicTails_AdditionalTimeAlignment(NA62Analysis::Core::BaseAnalysis *ba);
  ~PnnKinematicTails_AdditionalTimeAlignment() {}
  void InitHist();
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser();
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void ProcessEOBEvent() {}
  void DrawPlot() {}

private:
  bool fReadingData;
  Bool_t   fWarnOnce;
  Double_t fCedarOffset; ///< Time offset to be applied to Cedar hits and candidates [ns]
  Double_t fLKrOffset; ///< Time offset to be applied to LKr hits and candidates [ns]
  Double_t fSpectrometerOffset; ///< Time offset to be applied to Spectrometer candidates [ns]
  Double_t fGTKOffset;  ///< Time offset to be applied to GTK hits [ns]
  TString  fRecoRevision;  ///< Reco revision: time offsets depend on it
};

#endif
