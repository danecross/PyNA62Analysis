#ifndef NEWCHODSEARCHRADIUSCOMPUTATION_HH
#define NEWCHODSEARCHRADIUSCOMPUTATION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TF1.h>

//class TF1;
class TH1I;
class TH2F;
class TGraph;
class TTree;

class NewCHODSearchRadiusComputation : public NA62Analysis::Analyzer {
  public:
    explicit NewCHODSearchRadiusComputation(NA62Analysis::Core::BaseAnalysis *ba);
    ~NewCHODSearchRadiusComputation();
    void InitHist();
    void InitOutput();
    void DefineMCSimple();
    void Process(int iEvent);
    void StartOfBurstUser();
    void EndOfBurstUser();
    void StartOfRunUser();
    void EndOfJobUser();
    void EndOfRunUser();
    void PostProcess();
    void DrawPlot();

  private:
    int fMCKaonID;
    int fMCMuonID;
};
#endif
