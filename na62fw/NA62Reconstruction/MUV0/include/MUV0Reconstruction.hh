#ifndef MUV0Reconstruction_H
#define MUV0Reconstruction_H 1

#include "NA62VReconstruction.hh"
#include "MUV0Geometry.hh"

#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TGraph.h"
#include "TGraphErrors.h"

class MUV0Reconstruction : public NA62VReconstruction {

  public:

    MUV0Reconstruction(TFile*, TString);
    ~MUV0Reconstruction();
    void ParseConfFile(TString);
    void ParseReconstructionSettingsFile(TString);

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
    Double_t GetNRecoHitsPerBurst() {return fNRecoHitsPerBurst;}

    // MUV0 histograms
    TH1I* GetHNDigis()                     {return fHNDigis;}
    TH1I* GetHNRecoHits()                  {return fHNRecoHits;}
    TH1I* GetHHitMap()                     {return fHHitMap;}
    TH1I* GetHHitMapEOBLowThr()            {return fHHitMapEOBLowThr;}
    TH1I* GetHHitMapEOBHighThr()           {return fHHitMapEOBHighThr;}
    TH1F* GetHTime(Int_t ChanID)           {return fHTime[ChanID];}
    TH1F* GetHTimeNoT0(Int_t ChanID)       {return fHTimeNoT0[ChanID];}
    TH2F* GetHIllumination()		   {return fHIllumination;}
    TH2F* GetHHitToTHighThr()		   {return fHHitToTHighThr;}
    TH2F* GetHHitToTLowThr()		   {return fHHitToTLowThr;}
    TH2F* GetHHitToTLowThrIfHighThrExists(){return fHHitToTLowThrIfHighThrExists;}
    TH2F* GetHHitGeoChannelVsEdgeMask()	   {return fHHitGeoChannelVsEdgeMask;}

  private:

    TString  fReconstructionSettingsFileName;
    Double_t fLowThreshold;
    Double_t fHighThreshold;
    Double_t fDtLeadingMax;
    Double_t fNRecoHitsPerBurst;
    MUV0Geometry* fMUV0Geometry;

    // MUV0 histograms
    TH1I* fHNDigis;
    TH1I* fHNRecoHits;
    TH1I* fHHitMap;
    TH1I* fHHitMapEOBLowThr;
    TH1I* fHHitMapEOBHighThr;
    TH1F* fHTime[9];
    TH1F* fHTimeNoT0[9];
    TH2F* fHIllumination;
    TH2F* fHDigiToTVsROChannel;
    TH2F* fHHitToTLowThr;
    TH2F* fHHitToTHighThr;
    TH2F* fHHitToTLowThrIfHighThrExists;
    TH2F* fHCorrectedLeadingTimeWrtReferenceVsSlewingSlope[9];
    TH2F* fHCorrectedTrailingTimeWrtReferenceVsSlewingSlope[9];
    TH2F* fHHitGeoChannelVsEdgeMask;
    TH2F* fHHiThrTOTVsLeadingTime[9];
    TH2F* fHEdgeMaskVsFirstTime[9];
    TH2F* fHLowTOTVsHighTOT[9];
};
#endif
