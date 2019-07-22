#ifndef SACReconstruction_H
#define SACReconstruction_H 1

#include "NA62VReconstruction.hh"

#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TGraph.h"
#include "TGraphErrors.h"

class SACReconstruction : public NA62VReconstruction {

  public:

    SACReconstruction(TFile*, TString);
    ~SACReconstruction();
    void ParseConfFile(TString);

    virtual void Init(NA62VReconstruction*);
    virtual void StartOfBurst();
    virtual void EndOfBurst();

    virtual TRecoVEvent * ProcessEvent(TDetectorVEvent*, Event*);
    virtual TRecoVEvent * ProcessSOBEvent(TDetectorVEvent*, Event*);
    virtual TRecoVEvent * ProcessEOBEvent(TDetectorVEvent*, Event*);
    virtual TDetectorVEvent * Trigger(TDetectorVEvent*, Event*);
    virtual void EndProcessing();
    virtual void FillTimes(Double_t);

    void InitHistograms();
    void SaveHistograms();
    void ResetHistograms();
    void DeleteHistograms();
    void RecoHitAnalyse();

    // SAC histograms
    TH1F         *GetHRateBurst         (){return fHRateBurst;}
    TH2F         *GetHRateRun           (){return fHRateRun;}
    TH2F         *GetHHitToTAll         (){return fHHitToTAll;}
    TH2F         *GetHHitToT            (){return fHHitToT;}
    TH2F         *GetHHitToTOrdered     (){return fHHitToTOrdered;}
    TH2F         *GetHHitToTHighThr     (){return fHHitToTHighThr;}
    TH2F         *GetHHitToTLowThr      (){return fHHitToTLowThr;}
    TH2F         *GetHDigiToTVsROChannel(){return fHDigiToTVsROChannel;}
    TH2F         *GetHHitToTLowThrIfHighThrExists                    (){return fHHitToTLowThrIfHighThrExists;}
    TH2F         *GetHHitGeoChannelVsEdgeMask                        (){return fHHitGeoChannelVsEdgeMask;}

    TGraph** GetGRateChLow () {return fGRateChLow ;};
    TGraph** GetGRateChHigh() {return fGRateChHigh;};
    TGraph** GetGRateChLowNorm () {return fGRateChLowNorm ;};
    TGraph** GetGRateChHighNorm() {return fGRateChHighNorm;};
    TGraphErrors** GetGHitRateChNorm() {return fGHitRateChNorm;};
    Int_t *GetNHitsBurst(){ return  fNHitsBurst;};

  private:

    Int_t fNHitsBurst[4];

    Double_t fLowThreshold;
    Double_t fHighThreshold;
    Double_t fDtLeadingMax;

    TH1F         *fHRateBurst;
    TH2F         *fHRateRun;
    TH2F         *fHDigiToTVsROChannel;
    TH2F         *fHHitToT;
    TH2F         *fHHitToTAll; //Only those with 4 thresholds workd
    TH2F         *fHHitToTOrdered;
    TH2F         *fHHitToTLowThr;
    TH2F         *fHHitToTHighThr;
    TH2F         *fHHitToTLowThrIfHighThrExists;
    TH2F         *fHCorrectedLeadingTimeWrtReferenceVsSlewingSlope[4];
    TH2F         *fHCorrectedTrailingTimeWrtReferenceVsSlewingSlope[4];
    TH2F         *fHHitGeoChannelVsEdgeMask;

    TH2F         *fHHiThrTOTVsLeadingTime[4];
    TH2F *fHEdgeMaskVsFirstTime[4];
    TH2F *fHLowThrTOTVsLeadingTime[4];
    TGraph *fGRateChLow[4];
    TGraph *fGRateChHigh[4];
    TGraph *fGRateChLowNorm[4];
    TGraph *fGRateChHighNorm[4];

    TGraphErrors *fGHitRateChNorm[4];

};
#endif
