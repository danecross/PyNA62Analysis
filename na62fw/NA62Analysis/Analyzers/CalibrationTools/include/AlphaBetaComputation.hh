#ifndef ALPHABETACOMPUTATION_HH
#define ALPHABETACOMPUTATION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TF2.h>
#include <TGraphErrors.h>

class TH2I;
class TGraph;
class TTree;

class AlphaBetaComputation : public NA62Analysis::Analyzer {

public:
  explicit AlphaBetaComputation(NA62Analysis::Core::BaseAnalysis *ba);
  ~AlphaBetaComputation() {}
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

  TLorentzVector getParticle(TVector3 trmom, int part);
  TLorentzVector threeParticle(TLorentzVector part1,TLorentzVector part2,TLorentzVector part3);
  Double_t fit2DError(TF2* fit,Int_t opt);
  void determineBestAlphaBetaPerJob();
  void saveBurstStabilityPlots();
  //void assignHistograms(Int_t iEvent);
  void ReadDataModeTree();
  void makePDFReport();

  //! Data or Histo
  Bool_t fReadingData; 

  //! Struct for stability plots (same format as tree
  struct treeInfo{
    Int_t RunID;
    Int_t BurstID;
    Int_t NK3PI;
    Double_t Alpha;
    Double_t Beta;
    Double_t AlphaError;
    Double_t BetaError;
  };
  std::vector<treeInfo> SubJobVector;

  //! Variables 
  Int_t fRunID;
  Int_t fBurstID;
  Int_t fNumberK3PI;
  Double_t fAlpha, fBeta, fChiSq, fNDF;
  Double_t fAlphaError, fBetaError;
  Int_t nK3PI = 0;
  Double_t fSF;

  //! For Histogram definitions
  Double_t fAMax, fBMax, fAMin, fBMin;
  Double_t fAStep, fBStep;
  Double_t fABins, fBBins, fMBins, fTBins;
  Double_t fIMBins;

  //! Histograms and monitoring plots
  TH2I* fHMass;
  std::vector<TH2F*> fVChiGrid;
  std::vector<TGraphErrors*> fVMassMom;
  std::vector<TGraphErrors*> fVRunAlphaStability,fVRunBetaStability;

};
#endif
