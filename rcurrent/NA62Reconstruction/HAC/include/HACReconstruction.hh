#ifndef HACReconstruction_H
#define HACReconstruction_H 1

#include "NA62VReconstruction.hh"
#include "TRecoHACCandidate.hh"
#include "TRecoHACHit.hh"
#include "TDCEvent.hh"

#include "TFile.h"
#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TGraph.h"
#include "TVector.h"
#include "TGraphErrors.h"

#include "THACEvent.hh"
#include "TMultiGraph.h"
#include <vector>

class HACReconstruction : public NA62VReconstruction {
public:

    HACReconstruction(TFile*, TString);
    ~HACReconstruction();
    void ParseConfFile(TString);

    virtual void Init(NA62VReconstruction*);
    virtual void StartOfBurst();
    virtual void EndOfBurst();

    virtual TRecoVEvent * ProcessEvent(TDetectorVEvent*, Event*);
    virtual TDetectorVEvent * Trigger(TDetectorVEvent*, Event*);
    virtual void EndProcessing();
    virtual void FillTimes(Double_t);

    void InitHistograms();
    void SaveHistograms();
    void ResetHistograms();
    void DeleteHistograms();
    void ReadSlewCorrParams();
    void ReadThresholds();


public:

    TRecoHACCandidate * GetCandidate() {return fCandidate;};
    void SetCandidate(TRecoHACCandidate * value) {fCandidate = value;};

    // HASC histograms

    TH2F *fHEdgeFlagvsChID;
    TH1F *fHChID;
    TH1F *fHModuleID;
    TH2F *fHRawLeadTimevsChID;
    TH2F *fHRawTrailTimevsChID;
    TH2F *fHLeadTimevsChID;
    TH2F *fHTrailTimevsChID;
    TH2F *fHToTvsSiPMid;
    TH2F *fHModulevsSection;
    TH2F *fHTimevsSiPMid;
    TH2F *fHSlewCorrTimevsSiPMid;
    TH2F *fHChargevsSiPMid;
    TH2F *fHGoodMap;
    TH2F *fHBadMap;
    
    TH1F *GetHChID() {return fHChID;};
    TH2F *GetHEdgeFlagvsChID() {return fHEdgeFlagvsChID;};
    TH1F *GetHModuleID() {return fHModuleID;};
    TH2F *GetHModulevsSection() {return fHModulevsSection;};
    TH2F *GetHTimevsSiPMid() {return fHTimevsSiPMid;};
    TH2F *GetHChargevsSiPMid() {return fHChargevsSiPMid;};
    
    TString GetSignalParametersFileName() {return fSignalParametersFileName;};
    
    std::vector<Double_t> GetChargeToTOffset() {return fChargeToTOffset;};
    std::vector<Double_t> GetChargeToTConstant() {return fChargeToTConstant;};
    std::vector<Double_t> GetChargeToTSlope() {return fChargeToTSlope;};
    
    void GroupDigis(TClonesArray& Digis);
    void CreateRecoStructure(std::vector<Double_t> &leadTimes, std::vector<Double_t> &trailTimes, std::vector<Int_t> &thresholdID, Int_t channelID);
    void FillRecoHit(TRecoHACHit *recoHit);
    void SlewCorrection(TRecoHACHit *recoHit/*, Int_t fNThresholds, Int_t fChannelID*/);
    Double_t SlewCorrection(const Double_t &tot, const Int_t &chID);
    void ComputeCharge(TRecoHACHit *recoHit);
    Double_t ComputeArea(Double_t* times, Double_t *voltages);
    
    Double_t *GetThresholdValues() {return fThresholdValues;};
private:

    TRecoHACCandidate * fCandidate;
    TDCEvent* fTdcEvent; // Used as a buffer for the FillTimes 
    TString fSlewingCorrFileName;
    TString fThresholdsFileName;
    Bool_t fEnableSlewingCorr;
    Double_t fSlewCorrParamThr0[90][7]; // Lead Time Slew correction parameters
    Double_t fSlewCorrParamThr1[90][6]; // Lead Time Slew correction parameters
    Double_t fThresholdValues[360];
    //Parameters for digital signal generation
    TString fSignalParametersFileName;
    Double_t fTimeWindow;
    //Particle rate evaluation
    std::vector<Double_t> fNSelectedCandidatesInTimeSlots; 
    //Charge relation to ToT is an exponential for each threshold.
    //Charge(ToT) = Offset + Constant * Exp(ToT*Slope)
    std::vector<Double_t> fChargeToTOffset;
    std::vector<Double_t> fChargeToTConstant;
    std::vector<Double_t> fChargeToTSlope;
};
#endif
