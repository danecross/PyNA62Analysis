#ifndef JITTERSCHECK_HH
#define JITTERSCHECK_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

enum Jitt {kCREAMUp=0, kCREAMLeft=0, kCREAMSeed=1, kCREAMRight=2, kCREAMDown=2};

class JittersCheck : public NA62Analysis::Analyzer
{
  public:
    explicit JittersCheck(NA62Analysis::Core::BaseAnalysis *ba);
    ~JittersCheck();
    void InitHist();
    void InitOutput();
    void DefineMCSimple();
    void PostProcess();
    void StartOfBurstUser();
    void ProcessSpecialTriggerUser(int, unsigned int);
    void Process(int iEvent);
    void EndOfBurstUser();
    void StartOfRunUser();
    void EndOfRunUser();
    void EndOfJobUser();
    void DrawPlot();
    void GeneratePDF();
    Bool_t CREAMHasJitter(Int_t crate, Int_t slot);
    Double_t GetAsymmetry(Int_t Crate, Int_t Slot,Int_t i, Int_t j);
    Bool_t IsExternalCREAM(Int_t Crate, Int_t Slot);
    void CheckCREAM(Int_t Crate, Int_t Slot);
  protected:
    TH2F *NegL;
    TH2F *PosL;
    TH2F *NegR;
    TH2F *PosR;
    TH2F *NEntriesL;
    TH2F *NEntriesR;
    TH2F *DTLeft;
    TH2F *DTRight;
    TH2F* fHNEntriesNegTail[3][3];
    TH2F* fHNEntriesPosTail[3][3];
    TH2F* fHDeltaT[3][3];
    TH2F* fHNPipEntries[3][3];
};
#endif
