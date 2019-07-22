#ifndef MUV1Reconstruction_H
#define MUV1Reconstruction_H 1

#include "NA62VReconstruction.hh"
#include "TRecoMUV1Candidate.hh"
#include "TRecoMUV1Hit.hh"
#include "MUV1HitsCluster.hh"

#include "MUV1Geometry.hh"

#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TF1.h"
#include "TMinuit.h"

class MUV1Reconstruction : public NA62VReconstruction {

  public:

    MUV1Reconstruction(TFile*, TString);
    virtual ~MUV1Reconstruction();

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

    TRecoMUV1Candidate *  GetCandidate()                                     { return fCandidate;                    };
    void                  SetCandidate(TRecoMUV1Candidate * value)           { fCandidate = value;                   };

    Double_t GetCalibration () { return fCalibrationParameter; }
    Double_t GetMeanT0() { return fMeanT0; }

    Double_t GetT0 (Int_t Side, Int_t Channel){ return fMUV1T0[Side][Channel-1]; }
    Double_t GetTPosDelay (Int_t Channel, Double_t Position);
    Double_t GetEq (Int_t Side, Int_t Channel){ return fMUV1QEq[Side][Channel-1]; }
    Double_t GetQPosAttenuation (Int_t Channel, Double_t Position);

    TDirectory* GetPlotDir () { return GetOrMakeDir(fHistoFile,"MUV1Monitor"); }
    
    Bool_t IsClusterAllowedAtCenter(Int_t vchannel, Int_t hchannel, Int_t quadrant);
    Bool_t IsClusterAllowedAtCorner(Int_t vchannel, Int_t hchannel, Int_t quadrant);

  protected:
    static Double_t SinglePeakFunction(Double_t x, Double_t * pars);
    static void ChiSquareSinglePeak(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);
    static Double_t DoublePeakFunction(Double_t x, Double_t * pars);
    static void ChiSquareDoublePeak(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);

  private:

    //Base element of the class
    TRecoMUV1Candidate *fCandidate;

    //Variables for Digi to Reco Hit
    TMinuit * fFitter;

    Double_t fMUV1FitDataX[32];
    Double_t fMUV1FitDataErrorX[32];
    Double_t fMUV1FitDataY[32];
    Double_t fMUV1FitDataErrorY[32];
    Double_t fPars[7], fParErrors[7];
    Double_t fPiSquared;

    //Clusterization algorithm variables
    std::vector <TRecoMUV1Hit*> fHitOnChannel[4][44];
    std::vector <MUV1HitsCluster*> fSideCluster[4];
    MUV1Geometry *fGeomInstance;

    //Time Alignment
    Double_t fMUV1T0[4][44];
    Double_t fMeanT0;
    Double_t fMUV1TimeDep_par0[4][44];
    Double_t fMUV1TimeDep_par1[4][44];
    TF1 *fTPosDependency;

    //Calibration
    Double_t fMUV1QEq[4][44];
    Double_t *fMUV1QEq_pars[4][44];
    TF1 *fQPosDependency;
    Int_t fQPosNParameters;
    Double_t fCalibrationParameter;

    //Parameters from the Config File
    Int_t fStopAfterEmptyChannels;
    Double_t fChannelClusterTimeCut;
    Double_t fSideClusterTimeCut;
    Bool_t fSaveRefused;
    Bool_t fSaveSaturated;
    Bool_t fSaveUnderflow;

    //parameters for digitization
    Bool_t fBirksEnable;

    //Parameters for geometry
    Bool_t fCentralClusterFlags[2][2];
    Bool_t fCornerClusterFlags[3][3];

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
    TH1D * fHTotalEnergy;

    TH1D * fHDistDerBaseline;
    TH1D * fHBaselines[4][44];

};
#endif
