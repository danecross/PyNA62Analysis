#ifndef BETACOMPUTATION_HH
#define BETACOMPUTATION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>

class TH1I;
class TH1D;
class TH2F;
class TGraph;
class TGraphErrors;
class TF1;
class TTree;
class TVector3;

class BetaComputation : public NA62Analysis::Analyzer {
public:
    explicit BetaComputation(NA62Analysis::Core::BaseAnalysis *ba);
    ~BetaComputation();
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

    void DetermineBeta();
    void PDFReport();
protected:

    // Data or Histo mode
    Bool_t fReadingData;

    // Variables
    Int_t fNBetaScan;
    Double_t fBeta, fBStep;
    Double_t fBetaError;
    Double_t fBetaInit;

    TVector3 fNegativeMomentum;

    std::vector<TH2F*> fHMass;
    TGraphErrors* fMKGraph;
    TH1D* fBetaHisto;

};
#endif
