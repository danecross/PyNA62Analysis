//
//  CREAMRawDecoder.hh
//
//
//  Created by riccardo aliberti on 06/10/14.
//
//

#ifndef CREAMRawDecoder_H
#define CREAMRawDecoder_H 1

#include "NA62VRawDecoder.hh"
#include "TH3.h"
#include "FADCEvent.hh"
#include "FADCVHit.hh"
#include "TSpecialTriggerEvent.hh"
#include "EventHeader.hh"

#include "TClonesArray.h"
#include <vector>
#include <fstream>

struct LKrFossil{
  Int_t Crate;
  Int_t Slot;
  Int_t Ch;
  Double_t Energy;
  Int_t ix;
  Int_t iy;
};

struct LKrJitter{
  Int_t Crate;
  Int_t Slot;
  Int_t NEntries;
  Double_t MaxAsym;
  Double_t MinAsym;
  Double_t MaxRatio;
  Double_t MinRatio;
  Double_t JittTime;
  Int_t TimeSlotsShift;
};

enum Jitt {kCREAMUp=0, kCREAMLeft=0, kCREAMSeed=1, kCREAMRight=2, kCREAMDown=2};
enum OutFiles {kReal=0, kAnom=1, kOCTANE=2};

class CREAMRawDecoder : public NA62VRawDecoder {

  public:

    explicit CREAMRawDecoder(NA62VReconstruction*);
    ~CREAMRawDecoder();
    virtual TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);
    void ParseRawDecoderSettingsFile(TString);
    virtual void EndProcessing();
    void StartOfBurst();
    void EndOfBurst();

    //Swap finders
    void FillSwapsWithCalibTriggersInfo(UInt_t,UInt_t,UInt_t,UInt_t,UInt_t);
    void FillSwapsWithCrocusInfo(UInt_t,UInt_t,UInt_t,UInt_t,UInt_t);
    void DetectSwapsWithCalibTriggers();
    void DetectSwapsWithCrocus(std::ofstream&);
    //Jitter analysis
    void JitterAnalysis();
    void FossilChecker(Int_t flag); // 0: ZSFossils, 1: Needles in physics triggers
    Double_t GetAsymmetry(Int_t Crate, Int_t Slot,Int_t i, Int_t j, Bool_t UseCoarseT0); 
    Int_t GetAdjacentCREAM(Int_t Crate, Int_t Slot);
    Double_t GetPhysicalSlot(Int_t ProgressiveSlot) { return (ProgressiveSlot <8) ? ProgressiveSlot+3 : ProgressiveSlot+5;}
    void SeedPipDeltaT(FADCVHit *seed, FADCVHit *pip);
    Bool_t CREAMHasJitter(Int_t CrateID, Int_t ProgressiveSlotID, Double_t &JittTime, Bool_t UseCoarseT0);
    Bool_t HasEnoughStatistics(Int_t crate, Int_t slot, Int_t row, Int_t col, Bool_t UseCoarseT0);
    LKrJitter InitializeJitter(Int_t crate, Int_t slot, Bool_t UseCoarseT0);
    TH1D* GetHNQualityWarningsVsROChannel() { return fHNQualityWarningsVsROChannel; }
    TH2F* GetHPedestals()    { return fHPedestals;    }
    TH2F* GetHPedestalsRMS() { return fHPedestalsRMS; }
    TH2F* GetHNCriticalErrorsCrateSlot() { return fHNCriticalErrorsCrateSlot; }
    TH2F* GetHNQualityWarningsCrateSlot(){ return fHNQualityWarningsCrateSlot;}
    TH2F* GetHMaxSampleXYCalib()         { return fHMaxSampleXYCalib;         }
    TH2F* GetHMaxSampleCrateSlotCalib()  { return fHMaxSampleCrateSlotCalib;  }
    TH2F* GetHZSCounterXY()              { return fHZSCounterXY;              }
    TH2F* GetHZSCounterXYCalib()         { return fHZSCounterXYCalib;         }
    Int_t GetNCriticalErrorTypes()       { return (Int_t)CREAMDecoderErr::CREAM_BAD_SAMPLES; } //number of critical errors currently defined

  private:

    Int_t fLatency;
    UInt_t fMaxCrateID;
    UInt_t fMaxSlotID;
    Int_t* fCrateRemap; 
    Int_t* fSlotRemap; 

    // variables for swap reading
    TString  fSwapInputFileName;
    std::vector<std::pair<UInt_t,UInt_t> > fSwapInput; // crate,slot
    // variables for swap detection
    Bool_t   fSwapDetectionEnabled;
    // variables for swap detection (CalibTriggers)
    Int_t    fNEntriesPerPatternSwapWithCalibTriggers[2];
    Int_t*** fNEntriesSwapWithCalibTriggers; 
    Int_t**  fNTotalEntriesSwapWithCalibTriggers; 
    Int_t**  fNDetectedSwapsWithCalibTriggers; 
    Int_t    fNCreamsCheckedWithCalibTriggers;
    std::vector<std::pair<UInt_t,UInt_t> > fSwapsFoundWithCalibTriggers; // crate,slot
    // variables for swap detection (Crocus)
    TString  fCrocusReferenceFileName;
    TH3F* fNHitsSwapWithCrocus;
    TH3F* fNHitsRefSwapWithCrocus;
    Int_t    fNCreamsCheckedWithCrocus;
    Int_t    fNTriggersForCrocus;
    std::vector<std::pair<UInt_t,UInt_t> > fSwapsFoundWithCrocus; // crate,slot
    enum CREAMDecoderErr{
      CREAM_UNDERFLOWBIN       =0, // UNDERFLOW defined in cmath
      ///// critical errors
      CREAM_EVENTNUMBER_MISMATCH , //critical
      CREAM_BLOCKPAYLOAD_FATAL   , //critical
      CREAM_NSAMPLES_MISMATCH    , //critical
      CREAM_TRIGWORD_MISMATCH    , //critical
      CREAM_BAD_SPECIAL_TRIGGER  , //critical (not implemented)
      CREAM_DATA_TYPE            , //critical (not implemented)
      CREAM_BAD_DATA_SIZE        , //critical (not implemented)
      CREAM_BLOCKTS_MISMATCH     , //critical
      CREAM_BAD_TRIGGERTS        , //critical (not implemented)
      CREAM_BAD_SLOT             , //critical (not implemented)
      CREAM_L1_ERROR             , //critical
      CREAM_WRONG_CHECKSUM       , //critical
      CREAM_BAD_SAMPLES          , //critical
      ///// non-critical errors
      CREAM_BLOCKPAYLOAD_MISMATCH,
      CREAM_PAYLOADCHS_MISMATCH  ,
      CREAM_WRONG_L0RQ           ,
      CREAM_REPEATED_WORD        , //(not implemented)
      CREAM_NOT_TIME_ORDER       , //(not implemented)
      CREAM_MASKED_CH            ,
      CREAM_OSCILLATING_CH       ,
      CREAM_OVERFLOWBIN            // OVERFLOW defined in cmath 
    }; 

    std::vector<TString> fCREAMDecoderErrString;
    // Histos for data corruption checks
    TH1D* fHNQualityWarningsVsROChannel;
    TH2F* fHNQualityWarningsCrateSlot;
    TH2F* fHPedestals;
    TH2F* fHPedestalsRMS;
    TH2F* fHPedestalVsROChannel;
    TH2F* fHPedestalWrtRefVsROChannel;
    TH2F* fHPedestalBadRefXYNum;
    TH2F* fHPedestalBadRefXYDen;
    TH2F* fHPedestalBadRefXYEff;
    TH2F* fHNCriticalErrorsCrateSlot;

    // Histos for LKr bad cells study (LKr only!)
    Double_t fHitEnergyThr; // used for the filling of fHHitMapEnergyAboveThr
    TH2F* fHZSCounterXY;
    TH2F* fHPedestalXY;
    TH1D* fHHitEnergy;
    TH2F* fHZSCounterXYCalib;
    TH2F* fHPedestalXYCalib;
    TH1D* fHHitEnergyCalib;
    TH2F* fHMaxSampleXYCalib;
    TH2F* fHMaxSampleCrateSlotCalib;
    TH2F* fHHitMapEnergyAboveZS;
    TH2F* fHHitMapEnergyAboveThr;
    TH2F* fHNQualityWarningsXY;
    // Histos for JitterAnalysis (LKr only) the elements of the arrays are: Right,Up,Left,Down
    TH2F* fHNEntriesNegTail[3][3];
    TH2F* fHNEntriesPosTail[3][3];
    TH2F* fHNEntriesNegTailOCTANE[3][3];
    TH2F* fHNEntriesPosTailOCTANE[3][3];
    TH2F* fHDeltaT[3][3];
    TH2F* fHDeltaTOCTANE[3][3];
    TH2F* fHNPipEntries[3][3];
    TH2F* fHSeedPosition;
    //Fossil checker
    TH2F *fHFossilCellPosition;
    TH2F *fHFossilCREAMPosition;
    std::vector<LKrFossil> fLKrZSFossils;
    std::vector<LKrFossil> fLKrNeedles;
    const Double_t fJitterThreshold = 5.;
};
#endif
