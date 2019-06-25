#ifndef LKrParameters_H
#define LKrParameters_H 1

#include "TVector3.h"
#include "TROOT.h"
#include "Riostream.h"
#include <iostream>
#include <string> 
#include "LKrReconstruction.hh"
#include "LKrCommon.hh"
using namespace std;

class LKrParameters {

  public:

    LKrParameters();
    static LKrParameters* GetInstance();

  private:

    static LKrParameters* fInstance; ///< Instance of the class.

  public:
    void Fill(LKrReconstruction*);
    Bool_t IsDeadCell(Int_t,Int_t);

  public:
    // Configuration file
    Int_t GetDecodingParam(Int_t i) { return fDecodingParam[i]; };
    void SetDecodingParam(Int_t i, Int_t val) { fDecodingParam[i] = val; };
    Int_t GetDigiFilterPreProcess() { return fDigiFilterPreProcess; };
    void SetDigiFilterPreProcess(Int_t val) { fDigiFilterPreProcess=val; };
    Int_t GetNSamplePedestals() { return fNSamplePedestals; };
    void SetNSamplePedestals(Int_t val) { fNSamplePedestals=val; };
    Int_t GetNSampleNoise() { return fNSampleNoise; };
    void SetNSampleNoise(Int_t val) { fNSampleNoise=val; };
    Int_t GetSampleTimeLow() { return fSampleTimeLow; };
    void SetSampleTimeLow(Int_t val) { fSampleTimeLow=val; };
    Int_t GetSampleTimeHigh() { return fSampleTimeHigh; };
    void SetSampleTimeHigh(Int_t val) { fSampleTimeHigh=val; };
    Double_t GetSeedEnergyCut() { return fSeedEnergyCut; };
    void SetSeedEnergyCut(Double_t val) { fSeedEnergyCut=val;};
    Double_t GetSeedECutRatio() { return fSeedECutRatio; };
    void SetSeedECutRatio(Double_t val) { fSeedECutRatio=val;};
    Double_t GetClusterXCorr(Int_t i) { return fClusterXCorr[i]; };
    void SetClusterXCorr(Int_t i, Double_t val) { fClusterXCorr[i]=val; };
    Double_t GetClusterYCorr(Int_t i) { return fClusterYCorr[i]; };
    void SetClusterYCorr(Int_t i, Double_t val) { fClusterYCorr[i]=val; };
    Double_t GetClusterEvsXCorr(Int_t i) { return fClusterEvsXCorr[i]; };
    void SetClusterEvsXCorr(Int_t i, Double_t val) { fClusterEvsXCorr[i]=val; };
    Double_t GetClusterEvsYCorr(Int_t i) { return fClusterEvsYCorr[i]; };
    void SetClusterEvsYCorr(Int_t i, Double_t val) { fClusterEvsYCorr[i]=val; };
    Double_t GetClusterRMSCorr(Int_t i) { return fClusterRMSCorr[i]; };
    void SetClusterRMSCorr(Int_t i, Double_t val) { fClusterRMSCorr[i]=val; };
    Double_t GetClusterHoleCorr(Int_t i) { return fClusterHoleCorr[i]; };
    void SetClusterHoleCorr(Int_t i, Double_t val) { fClusterHoleCorr[i]=val; };
    Double_t GetClusterOutCorr(Int_t i) { return fClusterOutCorr[i]; };
    void SetClusterOutCorr(Int_t i, Double_t val) { fClusterOutCorr[i]=val; };
    Double_t GetEnergyScale() { return fEnergyScale; };
    void SetEnergyScale(Double_t val) { fEnergyScale=val; };
    Int_t GetTimePulNent() { return fTimePulNent; };
    void SetTimePulNent(Int_t val) { fTimePulNent=val; };
    Double_t GetTimePulStep() { return fTimePulStep; };
    void SetTimePulStep(Double_t val) { fTimePulStep=val; };
    Int_t GetTimePulNpeak() { return fTimePulNpeak; };
    void SetTimePulNpeak(Int_t val) { fTimePulNpeak=val; };
    Double_t GetTimePulScale() { return fTimePulScale; };
    void SetTimePulScale(Double_t val) { fTimePulScale=val; };
    Double_t GetTimePBase() { return fTimePBase; };
    void SetTimePBase(Double_t val) { fTimePBase=val; };
    Int_t GetZSAlgorithm() { return fZSAlgorithm; };
    void SetZSAlgorithm(Int_t val) { fZSAlgorithm=val; };
    Int_t GetOutputHits() { return fOutputHits; };
    void SetOutputHits(Int_t val) { fOutputHits=val; };
    Int_t GetPedestalsEvaluation() { return fPedestalsEvaluation; };
    void SetPedestalsEvaluation(Int_t val) { fPedestalsEvaluation=val; };

    // Database 
    Double_t GetPedSigma(Int_t ix, Int_t iy) {return fPedSigma[ix][iy]; };
    Int_t GetPedStat(Int_t ix,Int_t iy) { return (Int_t)fPedStat[ix][iy]; };
    Double_t GetPedRef(Int_t ix,Int_t iy) { return fPedRef[ix][iy]; };
    Int_t GetCalStat(Int_t ix,Int_t iy) { return (Int_t)fCalStat[ix][iy]; };
    unsigned int GetCellSta(Int_t ix,Int_t iy) { return (unsigned int)fCellSta[ix][iy]; };
    Double_t GetCellKe3(Int_t ix,Int_t iy) { return fCellKe3[ix][iy]; };
    Double_t GetCellT0(Int_t ix,Int_t iy) { return fCellT0[ix][iy]; };
    Double_t GetCalOffset(Int_t ix,Int_t iy,Int_t ig) { return fCalOffset[ix][iy][ig]; };
    Double_t GetCalSteig(Int_t ix,Int_t iy,Int_t ig) { return fCalSteig[ix][iy][ig]; };
    Double_t GetTimePulCons(Int_t i) { return fTimePulCons[i]; };
    Double_t GetTimePulLine(Int_t i) { return fTimePulLine[i]; };

    // Database files
    TString GetPedFileName() { return fPedFileName; };
    TString GetSlopeileName() { return fSlopeFileName; };
    TString GetRefShapeFileName() { return fRefShapeFileName; };
    TString GetDigFiltConstFileName() { return fDigFiltConstFileName; };
    TString GetT0FileName() { return fT0FileName; };
    TString GetKe3CorrFileName() { return fKe3CorrFileName; };
    TString GetPulConsFileName() { return fPulConsFileName; };
    TString GetPulLineFileName() { return fPulLineFileName; };

    void SetPedFileName(TString val) { fPedFileName=val; };
    void SetSlopeFileName(TString val) { fSlopeFileName=val; };
    void SetRefShapeFileName(TString val) { fRefShapeFileName=val; };
    void SetDigFiltConstFileName(TString val) { fDigFiltConstFileName=val; };
    void SetT0FileName(TString val) { fT0FileName=val; };
    void SetKe3CorrFileName(TString val) { fKe3CorrFileName=val; };
    void SetPulConsFileName(TString val) { fPulConsFileName=val; };
    void SetPulLineFileName(TString val) { fPulLineFileName=val; };
    void DefineDataType(bool);

    // functions needed for CellClusT0
    void SetTriggerType(UInt_t);
    void SetTriggerDriftT0(Double_t);

  private:
    Bool_t fIsRawData;
    Int_t fDecodingParam[10];
    Int_t fDigiFilterPreProcess;
    Int_t fNSamplePedestals;
    Int_t fNSampleNoise;
    Int_t fSampleTimeLow;
    Int_t fSampleTimeHigh;
    Double_t fSeedEnergyCut;
    Double_t fSeedECutRatio;
    Double_t fClusterXCorr[12];
    Double_t fClusterYCorr[6];
    Double_t fClusterEvsXCorr[6];
    Double_t fClusterEvsYCorr[3];
    Double_t fClusterRMSCorr[2];
    Double_t fClusterHoleCorr[4];
    Double_t fClusterOutCorr[4];
    Double_t fEnergyScale;
    Int_t fTimePulNent;
    Double_t fTimePulStep;
    Int_t fTimePulNpeak;
    Double_t fTimePulScale;
    Double_t fTimePBase;
    Int_t fZSAlgorithm;
    Int_t fOutputHits;
    Int_t fPedestalsEvaluation;

  private:
    Double_t fPedSigma[128][128];
    Double_t fPedStat[128][128];
    Double_t fPedRef[128][128];
    Double_t fCalStat[128][128];
    Double_t fCalOffset[128][128][4];
    Double_t fCalSteig[128][128][4];
    Double_t fCellSta[128][128];
    Double_t fCellKe3[128][128];
    Double_t fCellT0[128][128];
    Double_t fTimePulCons[3000];
    Double_t fTimePulLine[3000];

  private:      // Database files
    TString fPedFileName;
    TString fSlopeFileName;
    TString fRefShapeFileName;
    TString fDigFiltConstFileName;
    TString fT0FileName;
    TString fKe3CorrFileName;
    TString fPulConsFileName;
    TString fPulLineFileName;

  private:
    void FillCommon(LKrReconstruction*);
    void FillDigiFilter(const char *,const char *);
    void FillDigiFilter_mc(const char *,const char *);
    void FillClusCorr();

    LKRdigifilter* fLKRdigifilter;
};
#endif
