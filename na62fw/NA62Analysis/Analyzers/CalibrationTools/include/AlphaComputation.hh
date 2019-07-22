#ifndef ALPHACOMPUTATION_HH
#define ALPHACOMPUTATION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TH1I;
class TH2F;
class TGraph;
class TGraphErrors;
class TF1;
class TTree;
class TVector3;

class AlphaComputation : public NA62Analysis::Analyzer {
public:
    explicit AlphaComputation(NA62Analysis::Core::BaseAnalysis *ba);
    ~AlphaComputation();
    void InitHist();
    void InitOutput();
    void DefineMCSimple();
    void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
    void Process(int iEvent);
    void PostProcess();
    void StartOfBurstUser();
    void EndOfBurstUser();
    void StartOfRunUser();
    void EndOfRunUser();
    void EndOfJobUser();
    void DrawPlot();

    void DetermineAlpha();
    void PDFReport();
    TGraph* TGraphAxisStyle(TGraph* g, const TString &tx, const TString &ty,const Int_t &mcol, const Int_t &lcol);
protected:

    // Data or Histo mode
    Bool_t fReadingData;

    // Variables
    Int_t fNAlphaScan;
    Double_t fBeta, fAlpha, fAStep;
    Double_t fAlphaInit;

    TVector3 fNegativeMomentum;

    std::vector<TH2F*> fHMass;
    std::vector<TGraphErrors*> fHMass_vs_P;
    TGraphErrors* fMKGraph;
    TGraphErrors* fChi2Graph;

};
#endif
