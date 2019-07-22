#ifndef THREETRACKMCEFFICIENCY_HH
#define THREETRACKMCEFFICIENCY_HH

#include <stdlib.h>
#include <vector>
#include <array>
#include <utility>
#include <map>
#include "Analyzer.hh"
#include <TCanvas.h>
#include <TString.h>

class TH1I;
class TH2F;
class TGraph;
class TTree;
class TEfficiency;

class ThreeTrackMCEfficiency : public NA62Analysis::Analyzer
{
public:
    explicit ThreeTrackMCEfficiency(NA62Analysis::Core::BaseAnalysis *ba);
    ~ThreeTrackMCEfficiency();
    void InitHist();
    void InitOutput();
    void DefineMCSimple(){}
    void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
    void Process(int);
    void PostProcess();
    void StartOfBurstUser();
    void EndOfBurstUser();
    void StartOfRunUser();
    void EndOfRunUser();
    void EndOfJobUser();
    void DrawPlot();
private:
    std::pair<TString, TString> GetPartName(Int_t pdgID, Int_t charge) const;
    bool fReadingTree;
    TString fOutputPDFFileName;
    TH1F *fHkaonDecayed, *fHNtracksInAcc, *fHZvtxAll, *fHZvtxAcc;
    std::map<std::pair<Int_t, Int_t>, TH1F*> fHmomAll, fHmomAcc;
    std::map<std::pair<Int_t, Int_t>, TH1F*> fHeffMom, fHeffMom4Ch;
    TH1F* fHeffEvent, *fHeffEventDen;
    std::array<TH2F*, 4> fHxyNoMatch;
    std::array<TH2F*, 4> fHxy3Chambers;
    std::array<TH2F*, 4> fHxyMiss;
    // 3 auxiliary constant variables for efficient histo booking
    const std::vector<Int_t> fCharges{1, -1};
    const std::vector<Int_t> fPDGids{0, 211, -13, -11};
};
#endif
