#ifndef LKrReconstruction_H
#define LKrReconstruction_H 1

#include "TLKrDigi.hh"
#include "NA62VReconstruction.hh"
#include "TRecoLKrCandidate.hh"
#include "CREAMRawDecoder.hh"
#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TLKrEvent.hh"

class LKrParameters;
class LKrDigiManager;
class LKrGeometry;
class LKrCommon;
class FADCEvent;

class LKrReconstruction : public NA62VReconstruction {

    public:

        LKrReconstruction(TFile*, TString);
        ~LKrReconstruction();

        virtual void Init(NA62VReconstruction*);
        virtual void StartOfBurst();
        virtual void EndOfBurst();

        virtual TRecoVEvent * ProcessEvent(TDetectorVEvent*, Event*);
        virtual TDetectorVEvent * Trigger(TDetectorVEvent*, Event*);
        void ParseConfFile(TString);
        virtual void EndProcessing();
        virtual void FillTimes(Double_t);

        void InitHistograms();
        void SaveHistograms();
        Int_t GetHwType()            { return fHwType;  }
        void  SetHwType(Int_t value) { fHwType = value; }

        void UpdateLKrMonitorHisto(); 

    public:

        TRecoLKrCandidate * GetCandidate()                          { return fCandidate;          };
        void                SetCandidate(TRecoLKrCandidate * value) { fCandidate = value;         };
        void                SetStationT0(Int_t i, Double_t value)   { if(fStationsT0 && i<fNStations) fStationsT0[i] = value; }; // Needed for LKr digitizer

        // Online Monitoring 
        TH2F * GetHHitMap() {return fHHitMap;};
        TH2F * GetHHitMapCrateSlot() {return fHHitMapCrateSlot;};
        TH2F * GetHHitMapEnergy() {return fHHitMapEnergy;};
        TH2F * GetHHitMapEnergyCrateSlot() {return fHHitMapEnergyCrateSlot;};
        TH2F * GetHPedestalXY()  {return fHPedestal_xy;};
        TH2F * GetHZSCounter()  {return fHZSCounter;};
        TH2F * GetHZSCounterOOB()  {return fHZSCounterOOB;};
        TH2F * GetHSigma()  {return fHSigma;};
        TH2F * GetHMaxSampleVsL0Mask()  {return fHMaxSampleVsL0Mask;};
        TH1F * GetHMaxSample() {return fHMaxSample;};
        TH1F * GetHNHits()     {return fHNHits; };
        TH1F * GetHNClusters()     {return fHNClusters; };
        TH1F * GetHClusterEnergy() {return fHClusterEnergy; };
        TH2F * GetHClusterXY()     {return fHClusterXY; };
        TH2F * GetHClusterTimeVsEnergy()     {return fHClusterTimeVsEnergy; };
        TH2F * GetHCellTimeVsEnergy()     {return fHCellTimeVsEnergy; };
        TH2F * GetHCellTotalEnergy()     {return fHCellTotalEnergy; };
        TH2F * GetHTotalEnergyVsL0Mask()     {return fHTotalEnergyVsL0Mask; };
        TH2F * GetHCellTotalEnergyVsPrimBits() {return fHCellTotalEnergyVsPrimBits; };
        TH2F * GetHTotalEnergyVsPrimBits() {return fHTotalEnergyVsPrimBits; };
        TH2F * GetHHitMapPrimBitsCellsEff(UInt_t iBit)    { if(iBit<16) return fHHitMapPrimBitsCellsEff[iBit]; else return 0; };
        TH2F * GetHHitMapPrimBitsClustersEff(UInt_t iBit) { if(iBit<16) return fHHitMapPrimBitsClustersEff[iBit]; else return 0; };
        TH1F * GetHEnergyPrimBitsCellsEff(UInt_t iBit)    { if(iBit<16) return fHEnergyPrimBitsCellsEff[iBit]; else return 0; };
        TH1F * GetHEnergyPrimBitsClustersEff(UInt_t iBit) { if(iBit<16) return fHEnergyPrimBitsClustersEff[iBit]; else return 0; };

   private:
        UInt_t GetPrimitiveBits();

        TRecoLKrCandidate * fCandidate;
        Double_t fClusterTimeOffset;
        Double_t fTotCellEnergy;

        // T0 Histos
        TH2F * fHRecoHitTimeWrtReferenceVsCell,     ///<Histogram to monitor T0 corrections (128*x+y)
             * fHRecoHitTimeWrtReferenceVsCellNoT0; ///<Histogram to evaluate T0 corrections (128*x+y)

        // Online Monitoring
        Double_t fThrEnergy[16];
        TH2F * fHHitMap;
        TH2F * fHHitMapEnergy;
        TH2F * fHHitMapCrateSlot;
        TH2F * fHHitMapEnergyCrateSlot;
        TH2F * fHPedestal_xy;
        TH2F * fHZSCounter;
        TH2F * fHZSCounterOOB;
        TH2F * fHSigma;
        TH1F * fHPedestal;
        TH1F * fHMaxSample;
        TH2F * fHMaxSampleVsL0Mask;
        TH1F * fHADCCountLow;
        TH1F * fHADCCountAll;
        TH1F * fHNHits;
        TH1F * fHNClusters;
        TH1F * fHClusterEnergy;
        TH2F * fHClusterXY;
        TH2F * fHClusterTimeVsEnergy;
        TH2F * fHClusterTimeVsEnergy_over;
        TH2F * fHCellTimeVsEnergy;
        TH2F * fHCellTotalEnergy;
        TH2F * fHClusVsCellTotE;
        TH2F * fHCellXYEzero;
        TH2F * fHTotalEnergyVsL0Mask;
        TH2F * fHCellTotalEnergyVsPrimBits;
        TH2F * fHTotalEnergyVsPrimBits;
        TH2F * fHHitMapPrimBitsCellsNum[16];
        TH2F * fHHitMapPrimBitsCellsDen[16];
        TH2F * fHHitMapPrimBitsCellsEff[16];
        TH2F * fHHitMapPrimBitsClustersNum[16];
        TH2F * fHHitMapPrimBitsClustersDen[16];
        TH2F * fHHitMapPrimBitsClustersEff[16];
        TH1F * fHEnergyPrimBitsCellsNum[16];
        TH1F * fHEnergyPrimBitsCellsDen[16];
        TH1F * fHEnergyPrimBitsCellsEff[16];
        TH1F * fHEnergyPrimBitsClustersNum[16];
        TH1F * fHEnergyPrimBitsClustersDen[16];
        TH1F * fHEnergyPrimBitsClustersEff[16];

        enum LKrHwType {
          kCPD = 1,       //200x format with CPDs and DC
          kSLM = 2,       //2011-2012 SLM format (CPD data)
          kCREAM=3        //Final CREAM format
        };
        Int_t fHwType;
        Int_t iFlagRec;
        Int_t fNClusters;
        Int_t fNClustersAboveThr;

        LKrParameters *fPar;
        LKrGeometry *fGeo;
        LKrCommon *fCom;
        LKrDigiManager *fLKrDigiManager;
        FADCEvent *fFADCEvent;
        FADCEvent *fFADCEventEnergy;

        // Automatic correction of LKr full jitters
        std::vector<LKrJitter> fJitters;
        void ReadJitterFile();
        Double_t GetJitterCorrection(TLKrDigi *Digi);

        void EndEvent(Int_t);
        void FillOutputHits();
        void FillOutput(); 
        void ApplyEnergyCorrections();
        Double_t CorrectedEnergyData(Int_t NCells, Double_t E0);
        Double_t CorrectedEnergyMC(Int_t NCells, Double_t E0);
};

#endif
