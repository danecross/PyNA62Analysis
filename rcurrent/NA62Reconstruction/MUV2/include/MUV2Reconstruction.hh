#ifndef MUV2Reconstruction_H
#define MUV2Reconstruction_H 1

#include "NA62VReconstruction.hh"
#include "TRecoMUV2Candidate.hh"
#include "TRecoMUV2Hit.hh"
#include "MUV2HitsCluster.hh"

#include "MUV2Geometry.hh"

#include "TFile.h"
#include "TVector3.h"
#include "TObjArray.h"
#include "TH1.h"
#include "TH2.h"
#include "TF1.h"
#include "TGraphErrors.h"
#include "TMinuit.h"

#include "TMUV2Event.hh"

class MUV2Reconstruction : public NA62VReconstruction {

  public:

    MUV2Reconstruction(TFile*, TString);
    virtual ~MUV2Reconstruction();

    virtual void Init(NA62VReconstruction*);
    virtual void StartOfBurst();
    virtual void EndOfBurst();

    void ParseConfFile(TString ConfFileName);

    virtual TRecoVEvent * ProcessEvent(TDetectorVEvent*, Event*);
    virtual TDetectorVEvent * Trigger(TDetectorVEvent*, Event*);
    virtual void EndProcessing();
    virtual void FillTimes(Double_t);

    Int_t GetBirksEnable(){ return fBirksEnable; };

    void InitHistograms();
    void SaveHistograms();
    void ResetHistograms();

  public:

    TRecoMUV2Candidate *  GetCandidate()                                     { return fCandidate;                    };
    void                  SetCandidate(TRecoMUV2Candidate * value)           { fCandidate = value;                   };

    Double_t GetT0 (Int_t Side, Int_t Channel){ return fMUV2T0[Side][Channel-1]; }
    Double_t GetTPosDelay (Int_t Channel, Double_t Position);
    Double_t GetEq (Int_t Side, Int_t Channel){ return fMUV2QEq[Side][Channel-1]; }
    Double_t GetQPosAttenuation (Int_t Channel, Double_t Position);


    TDirectory* GetPlotDir () { return GetOrMakeDir(fHistoFile,"MUV2Monitor"); }

  protected:
    static Double_t SinglePeakFunction(Double_t x, Double_t * pars);
    static void ChiSquareSinglePeak(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);
    static Double_t DoublePeakFunction(Double_t x, Double_t * pars);
    static void ChiSquareDoublePeak(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);

  private:

    //Base elements of the class
    TRecoMUV2Candidate *fCandidate;

    //Variables for Digi to Reco Hit
    TMinuit * fFitter;

    Double_t fMUV2FitDataX[32];
    Double_t fMUV2FitDataErrorX[32];
    Double_t fMUV2FitDataY[32];
    Double_t fMUV2FitDataErrorY[32];
    Double_t fPars[7], fParErrors[7];
    Double_t fPiSquared;

    //Clusterization algorithm variables
    std::vector <TRecoMUV2Hit*> fHitOnChannel[4][22];
    std::vector <MUV2HitsCluster*> fSideCluster[4];
    MUV2Geometry *fGeomInstance;

    //Time Alignment
    Double_t fMUV2T0[4][22];
    Double_t fMeanT0;
    Double_t fMUV2TimeDep_par0[4][22];
    Double_t fMUV2TimeDep_par1[4][22];
    Double_t fMUV2TimeDep_par2[4][22];
    TF1 *fTPosDependency;

    //Calibration
    Double_t fMUV2QEq[4][22];
    Double_t *fMUV2QEq_pars[4][22];
    TF1 *fQPosDependency;
    Int_t fQPosNParameters;
    Double_t fCalibrationParameter;

    //Parameters from the Config File
    Int_t fStopAfterEmptyChannels;
    Double_t fChannelClusterTimeCut;
    Double_t fSideClusterTimeCut;
    Bool_t fSaveRefused;
    Bool_t fSaveUnderflow;
    Bool_t fSaveSaturated;

    //parameters for digitization
    Bool_t fBirksEnable;

    //Histograms Declaration
    Int_t fLastNsamp, fFirstSlot, fLastSlot;

    TH2I * fHCREAMFlags;
    TH2F * fHHitTime;
    TH2F * fHChannelsHitMap;
    TH1I * fHChannelsOccupancy;
    TH2F * fHQVSChannel;
    TH2F * fHQVSChannel_L0intime;
    TH1F * fHHitTime_L0time;
    TH2F * fHAmplitudeVSChannel;
    TH2F * fHSigmaVSChannel;
    TH2F * fHISigBase;
    TH2F * fHHitMap;
    TH2F * fHClusterSeedChargeVsChannel;
    TH1F * fHShowerWidth;
    TH2F * fHShowerHitMap;
    TH1D * fHSideClusterTimeSpread;
    TH2F * fHSideClusterTimeSpreadVSChannelHit;
    TH1D * fHClusterLayerTimeDiff;
    TH2F * fHSeedEnergy;
    TH2F * fHSeedVSNHits;
    TH2F * fHHitTimeVSBurst;
    TH2F * fHHitTimeVSChannel;
    TH2I * fHNHitsVSBurst;
    TH2F * fHBaselinesvsChannel;

    TH1D * fHDistDerBaseline;
    TH1D * fHBaselines[4][22];

};
#endif
