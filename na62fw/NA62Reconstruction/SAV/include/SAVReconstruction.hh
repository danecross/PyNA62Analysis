#ifndef SAVReconstruction_H
#define SAVReconstruction_H 1

#include "NA62VReconstruction.hh"

#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TMinuit.h"

class SAVReconstruction : public NA62VReconstruction {

  public:

    SAVReconstruction(TFile*, TString);
    virtual ~SAVReconstruction();

    virtual void Init(NA62VReconstruction*);
    virtual void StartOfBurst();
    virtual void EndOfBurst();

    void ParseConfFile(TString ConfFileName);

    virtual TRecoVEvent * ProcessEvent(TDetectorVEvent*, Event*);
    virtual TDetectorVEvent * Trigger(TDetectorVEvent*, Event*);
    virtual void EndProcessing();
    virtual void FillTimes(Double_t);

    void InitHistograms();
    void SaveHistograms();
    void ResetHistograms();

  public:

    TDirectory* GetPlotDir () { return GetOrMakeDir(fHistoFile,"SAVMonitor"); }

  protected:
      static Double_t SinglePeakFunction(Double_t x, Double_t * pars);
      static void ChiSquareSinglePeak(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);
      static Double_t DoublePeakFunction(Double_t x, Double_t * pars);
      static void ChiSquareDoublePeak(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);
    

  private:

    // Objects for hit fitting
    TMinuit * fFitter;
    Double_t fSAVFitDataX[32];
    Double_t fSAVFitDataErrorX[32];
    Double_t fSAVFitDataY[32];
    Double_t fSAVFitDataErrorY[32];
    Double_t fPars[7], fParErrors[7];


    // Parameters for Config File
    Bool_t fSaveSaturated;
    Bool_t fSaveUnderflow;
    
    //Time Alignment
    Double_t fSAVT0[8];
    Double_t fMeanT0;
    
    //Calibration
    Double_t fSACchannelEq[4];
    Double_t fSAVcalibration[2][5];

    //Histograms Declaration
    Int_t fLastNsamp, fFirstSlot, fLastSlot;

    // Hit histo
    TH2I * fHCREAMFlags;
    TH2F * fHHitTime;
    TH1I * fHChannelsOccupancy;
    TH2F * fHAmplitudeVSChannel;
    TH2F * fHBaselinesVSChannel;
    TH2F * fHEnergyVSChannel;
    TH2F * fHEnergyVSChannel_L0intime;
    TH1D * fHHitTime_L0time;
    TH2F * fHHitTimeVSBurst;
    TH2F * fHHitTimeVSChannel;
    TH2I * fHNHitsVSBurst;
  
    // Candidate histo
    TH1D * fHHitTimeDist[2];
    TH1D * fHCandidateHitTime[2];
    TH2F * fHCandidateMap[2];
    TH1I * fHNHits[2];
    TH1D * fHCandidateEnergy[2];
  

};
#endif
