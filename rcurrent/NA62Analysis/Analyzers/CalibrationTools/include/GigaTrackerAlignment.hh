// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2017-12-11
//
// ---------------------------------------------------------------

#ifndef GigaTrackerAlignment_HH
#define GigaTrackerAlignment_HH

#include <TGraphErrors.h>
#include "Analyzer.hh"
#include "GigaTrackerRecoAlgorithm.hh"

class GigaTrackerAlignment : public NA62Analysis::Analyzer {

public:
  explicit GigaTrackerAlignment(NA62Analysis::Core::BaseAnalysis *ba);
  ~GigaTrackerAlignment();
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
  void BuildPDFReport();
  void ProcessEOBEvent() {}

private:

  GigaTrackerRecoAlgorithm* fGTKReco;

  Bool_t   fFineCorrections; ///< Computing fine corrections or (x,y) offsets? Default = true.
  Bool_t   fReadingData;
  Double_t fZGTK1;
  Double_t fZGTK2;
  Double_t fZTrim5;
  Double_t fZGTK3;
  Double_t fTrim5PT;       ///< PT kick of Trim5 magnet [MeV/c]
  Double_t fK3piMomentum;  ///< Kaon momentum reconstructed from K3pi [MeV/c]
  Double_t fK3pidxdz;      ///< Kaon dx/dz reconstructed from K3pi
  Double_t fK3pidydz;      ///< Kaon dy/dz reconstructed from K3pi
  Double_t fK3piPos[3][2]; ///< Kaon (x,y) positions at GTK1-3 planes reconstructed from K3pi [mm]

  Double_t fPull[5]; ///< Contributions to GTK/K3pi chi2 from the 5 variables
  Double_t Chi2_GTK_K3pi(TRecoGigaTrackerCandidate*);

  // For step-2
  TH1F *fHdx1, *fHdy1, *fHdx2, *fHdy2, *fHdx3, *fHdy3, *fHdPx, *fHdPy, *fHPrat;
  TF1  *fFdx1, *fFdy1, *fFdx2, *fFdy2, *fFdx3, *fFdy3, *fFdPx, *fFdPy, *fFPrat;
  TH2F *fHdxdz_vs_x, *fHdydz_vs_y;
  TH2F *fHStationEfficiency;
  TGraphErrors *fGdxdz_vs_x, *fGdydz_vs_y;
  TF1  *fFdxdz_vs_x, *fFdydz_vs_y;

  Double_t fInt[20], fZero[20], fEff[3][20], fdEff[3][20];
};

#endif
