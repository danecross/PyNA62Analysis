#ifndef CROCUSSWAPSFINDER_HH
#define CROCUSSWAPSFINDER_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "MCSimple.hh"
#include <TCanvas.h>

class TH1I;
class TH2F;
class TGraph;
class TTree;


class CrocusSwapsFinder : public NA62Analysis::Analyzer
{
  public:
    explicit CrocusSwapsFinder(NA62Analysis::Core::BaseAnalysis *ba);
    ~CrocusSwapsFinder();
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
    void DetectSwapsWithCrocus(std::ofstream& AdditionalOutputFile);
    void ReadReferenceFile();
  protected:

    UInt_t fMaxCrateID;
    UInt_t fMaxSlotID;
    Int_t* fCrateRemap; 
    Int_t* fSlotRemap; 
    TH3F *fNHitsSwapWithCrocus    ;
    TH3F *fNHitsRefSwapWithCrocus ;
    Int_t    fNCreamsCheckedWithCrocus;
    Int_t    fNTriggersForCrocus;
    std::vector<std::pair<UInt_t,UInt_t> > fSwapsFoundWithCrocus; // crate,slot
    TString fCrocusReferenceFileName;
    Int_t fFirstRunID; 
    Int_t fLastRunID; 
    Int_t fFirstBurstID; 
    Int_t fLastBurstID; 
  
};
#endif
