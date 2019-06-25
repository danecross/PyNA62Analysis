#ifndef HACDATAQUALITYMONITOR_HH
#define HACDATAQUALITYMONITOR_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TGraphErrors.h>
#include "TString.h"
#include "GeometricAcceptance.hh"
#include "VertexLSF.hh"
#include "LAVMatching.hh"
#include "SAVMatching.hh"
#include "K3piWithMissingPionInfo.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class HACDataQualityMonitor : public NA62Analysis::Analyzer {
public:
    explicit HACDataQualityMonitor(NA62Analysis::Core::BaseAnalysis *ba);
    ~HACDataQualityMonitor();
    void InitHist();

    void InitOutput();

    void DefineMCSimple() {}
    void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
    void Process(int iEvent);

    void PostProcess() {}
    void StartOfBurstUser();
    void EndOfBurstUser();

    void StartOfRunUser() {}

    void EndOfRunUser() {}
    void EndOfJobUser();

    void DrawPlot() {}
    void BuildPFDReport();
    void CreateBadBurstList();

public:
    void MatchMissingPion(const K3piWithMissingPionInfo &missingPion);
    Double_t ComputeCandidateCharge(const Int_t idMatch);
    Int_t IsInAcceptanceDownstream(TVector3 momentum, TVector3 position, Int_t charge);
    TVector3 Propagate(const TVector3 &position, const TVector3 &momentum, const Int_t &charge, 
    const Double_t &zEnd, const Int_t &flagInMag, Double_t &thetaXafter);

protected:
    TRecoHACEvent *fHACEvent;
    TRecoHACCandidate *fCandidate;

private:
    const char *fDetectorsName[10];
    int fDetectorsID[7];
    Double_t fZdetectors[10];

    Double_t fXstartHAC, fXstopHAC;
    Double_t fYstartHAC, fYstopHAC;
    Double_t fZstartHAC;

    std::vector<K3piWithMissingPionInfo> fMissingPion;
    
    Double_t fZmag[2]; 
    Double_t fDmag[2]; 
    Double_t fBmag[2];
private:
    TCanvas *fCanvas;
    TString fOutPDFFileName;
    Bool_t fReadingData;
    Double_t fArgonionCountsMin; ///< Minimum ArgonionCounts to consider the burst non-empty     
    Double_t fNSelectedTriggersMin; ///< Minimum NSelectedTriggers to consider the burst non-empty
    Double_t fEfficiencyThreshold;

    Int_t fBurstID;
    Double_t fArgonionCounts;
    Int_t fNTriggers;
    Int_t fNSelectedTriggers;
    Double_t fNMatchedPerBurst;
    Double_t fNExpectedPerBurst;

    TH2D* fHMatched_front; // Vs (x,y)
    TH2D* fHMatched_middle; // Vs (x,y)
    TH2D* fHMatched_back; // Vs (x,y)

    TH2D* fHExpected_front; // Vs (x,y)
    TH2D* fHExpected_middle; // Vs (x,y)
    TH2D* fHExpected_back; // Vs (x,y)

    TH2D* fHEfficiency_front; // Vs (x,y)
    TH2D* fHEfficiency_middle; // Vs (x,y)
    TH2D* fHEfficiency_back; //Vs (x,y)  
    
    TH1D *fHTiming;
    TH1D *fHCharge;

    TGraphErrors* fHEfficiencyVsBurstID;
    TGraphErrors* fHExpectedVsBurstID;
    TGraphErrors* fHArgonionCountsVsBurstID;
    TGraphErrors* fHNTriggersVsBurstID;
    TGraphErrors* fHNSelectedTriggersVsBurstID;

    Double_t fTimeWindow;
};
#endif
