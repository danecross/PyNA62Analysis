#ifndef LAVReconstruction_H
#define LAVReconstruction_H 1

#define MAXBLOCKMAP 130000
#define MAXBLOCKS 2500
#define MAXHITSPERBLOCK 1000

#include "LAVGeometry.hh"
#include "LAVConfiguration.hh"
#include "LAVCalibration.hh"

#include "LAVRecoBlock.hh"

#include "NA62VReconstruction.hh"
#include "TRecoLAVCandidate.hh"

#include "LAVClusterMaker.hh"
#include "LAVTrackFinder.hh"

#include "TFile.h"
#include <TH1D.h>
#include <TH2F.h>
#include <TH2F.h>
#include <fstream>

class LAVReconstruction : public NA62VReconstruction
{
    public:

        LAVReconstruction(TFile*, TString);
        ~LAVReconstruction();
        
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

    public:

        TRecoLAVCandidate *  GetCandidate() { return fCandidate; };
        void                 SetCandidate(TRecoLAVCandidate * value)      { fCandidate = value;                   };

        TH1D *GetHNLAVHit(Int_t StationID)                                { return fHNHit[StationID-1];   };   // retrieve hit histogram; input index ranges from 1 to 12
        TH1D *GetHNLAVGoodHit(Int_t StationID)                            { return fHNGoodHit[StationID-1]; }; // retrieve hit histogram; input index ranges from 1 to 12
        TH1D *GetHNRecoGoodHit(Int_t StationID)                           { return fHNGoodRecoHitsPerStation[StationID-1]; };
  //Get EOB Plots for the Online Monitor
  TH1D *GetEOBLAVOccupancy(Int_t StationID) {return fHEOBChannelOccupancy[StationID-1];};
  TH1D *GetEOBRateVsLAV() {return fHEOBRateVsLAV;};
  //Get the Phi distribution plot for the Online Monitor
  TH2F *GetPhiDistributionOnPhys() {return fHPhiDistributionOnPhys;};
  TH2F *GetPhiDistributionOffPhys() {return fHPhiDistributionOffPhys;};

// Reconstruction operation modes

  void SetMakeLAVClusters(Int_t val){fMakeLAVClusters = val;}
  Int_t GetLAVClusterMethod(){return fMakeLAVClusters;}
  void SetMakeLAVTracks(Int_t val){fMakeLAVTracks = val;}
  Int_t GetLAVTrackMethod(){return fMakeLAVTracks;}



    private:
        void Clear();
  LAVGeometry* fGeometryInstance;
  Int_t fTotalEventsPerBurst;
  
  Int_t fBlockID[MAXBLOCKS];
  Int_t fIFiredBlock[MAXBLOCKMAP];
  Int_t fNFiredBlocks;
  std::vector<LAVRecoBlock*> fLAVRecoBlocks;
  Double_t fGoodHitTimes[MAXBLOCKMAP];

  LAVCalibration* fCalib;
  Double_t fResidualSlewPars[2][MAXPARSPERTHRESHOLD];
  Bool_t fT0available;
  Int_t fApplyResidualSlewingCorrections;
  Bool_t fResidualSlewingAvailable;
  Double_t fLowThreshold;
  Double_t fHighThreshold;

  TRecoLAVCandidate * fCandidate;
  LAVClusterMaker* fLAVClusterMaker;
  LAVTrackFinder* fLAVTrackFinder;

  Int_t fStatus;
  ofstream fLAVDeadCh;
  ofstream fLAVDeadChEOB;
  ofstream fLAVNoisyCh;
  ofstream fLAVNoisyChEOB;
  ofstream fLAVBadBurst;
  ofstream fLAVBadBurstEOB;

  TH2F * fHRecoHitTimeWrtReferenceVsROChannelCT;
  TH2F * fHRecoHitTimeWrtReferenceVsROChannelNoT0CT;
  TH2F * fHRecoHitTimeWrtReferenceVsROChannelNoT0PrimCT;
  TH1D ** fHNHitLow;
  TH1D ** fHNGoodHitLow;
  TH1D ** fHNHitHigh;
  TH1D ** fHNGoodHitHigh;
  TH2F ** fHTOTVsChannelLow;
  TH2F ** fHTOTVsChannelHigh;

  TH1D ** fHNGoodRecoHitsPerStation;
  TH1D ** fHNBadRecoHitsPerStation;
  TH1D ** fHNHit; // array of histograms; index ranges from 0 to 11
  TH2F ** fHEdgeMask;
  TH1D ** fHNGoodHit;
  TH1D ** fHNBadHit;
  TH2F ** fHTimeVsChannel;
  TH2F ** fHdTStartVsChannel;
  TH2F ** fHdVMaxVsChannel;
  TH2F ** fHDeltaTimeVsChannel;
  TH1D ** fHNHitFreq;
  TH1D ** fHNGoodHitFreq;
  TH1D ** fHNBadHitFreq;
  TH2F ** fHLAVWindow;
  
//  TH2F* fHRecoHitTimeWrtReferenceVsROChannelNoResidualSlew;
//  TH2F* f HRecoHitTimeWrtReferenceVsROChannel;
//  TH2F* fHRecoHitTimeWrtReferenceVsROChannelNoT0;
  TH2F* fHRecoHitTimeWrtReferenceVsBurst;
  TH2F* fHRecoHitdTimeHighWrtdTLead;  
  TH2F* fHRecoHitdTimeLowWrtTOTLow;
  TH2F* fHRecoHitTimeWrtReferenceVsdTimeSL;
  TH2F* fHRecoHitTimeWrtReferenceVsdTimeHL;
  TH2F* fHRecoHitTimeWrtReferenceVsdTimeSH;
  TH2F* fHRecoHitTimeWrtReferenceVsRefTime;
  //EOB Plots:
  TH1D ** fHEOBChannelOccupancy;
  TH1D ** fHEOBChannelFrequency;
  TH1D ** fHEOBTDCRate;
  TH1D* fHEOBRateVsLAV;
  TH2F* fHPhiDistributionOnPhys;
  TH2F* fHPhiDistributionOffPhys;
  TH1D* fHTRecoHits;  

  Bool_t fMakeLAVT0s;
  Int_t fMakeLAVClusters;
  Int_t fMakeLAVTracks;
  Int_t fMakeRecoHistos;
  Int_t fPreferredTimeAlgorithm;
  Int_t fThresholdMethod;
  Double_t fInitialTime;
  Double_t fTotalTime;
  Double_t fBurstTime;

  void TransferClusterInformation(TRecoLAVCandidate*, LAVCluster*);


};

#endif
