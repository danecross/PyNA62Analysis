// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson) 2017-05-31
// With help from Dario Soldi, Francesco Gonnella, Radoslav Marchevski
// ---------------------------------------------------------------

#ifndef L0PRIMITIVEHANDLER_HH
#define L0PRIMITIVEHANDLER_HH

#include <map>
#include "L0TPData.hh"
#include "L0DetectorBits.hh"
#include "L0TPSpecialTrigger.hh"
#include "BaseAnalysis.hh"

typedef std::vector<Int_t> PrimInfo;

class L0PrimitiveHandler{
public:

  static L0PrimitiveHandler* GetInstance();
  void SetData(L0TPData*, Int_t);
  void SetRunID(Int_t);
  void SetData(L0TPData*);

  void NewInputFile(TString fname);

  void SetDebug(Bool_t);
  Bool_t GetDebug();

  Bool_t CheckEmulatedPrimitives(TString, Int_t);
  Bool_t CheckPrimitives(TString Name, Int_t reftime, Bool_t isEmulate=false);
  // Bool_t CheckPrimitives(TString, Double_t, Bool_t);
  Bool_t CheckCondition(Int_t);
  Bool_t IsPrimitiveInSlot(Int_t L0Detector, Int_t slot);

  void PrintDetectorBits();
  void PrintKnownBitNames();
  void PrintKnownBitNames(Int_t);
  void PrintDetectorKey();
  TString GetL0DetectorName(Int_t);
  void GetL0Masks(L0TPSpecialTrigger* L0TPST,
		  std::vector<TString>& MaskNames,
		  std::vector<Int_t>& Downscales);
  Int_t GetL0TPGranularity(Int_t L0Detector);

  Int_t GetTriggerTime(Int_t);
  Int_t GetL0TriggerTime();
  Int_t GetPrimitiveID(Int_t, Int_t);
  Int_t GetPrimitiveCorrectedFT(Int_t, Int_t, Int_t);
  Int_t GetPrimitiveDT(Int_t, Int_t, Int_t);
  Int_t GetPrimitiveAbsDT(Int_t, Int_t, Int_t);    
  PrimInfo GetPrimitiveInfo(Int_t, Int_t, Int_t);
  Bool_t GetClosestPrimitiveInfo(Int_t Ref, Int_t L0Detector, PrimInfo& a);

  // Functions for L0Emulators
  void DeclareL0Emulators(BaseAnalysis* ba, Int_t A=-1, Int_t B=-1, Int_t C=-1, Int_t D=-1, Int_t E=-1, Int_t F=-1, Int_t G=-1);
  void ConfigureL0Emulators();
  void GetEmulatorConfig();

  void SetL0TPCut(Int_t L0Detector, Int_t cut);
  void ResetL0TPConfig();
    
private:

  L0PrimitiveHandler();
  ~L0PrimitiveHandler();

  void ParseInputFile(TString fname);
  void DecodeItem(TString item, Int_t &L0Detector, Int_t &Bit);

  Bool_t HasEmulatedBit(TString, Int_t, Int_t);
  Bool_t HasPrimitiveBit(Int_t, Int_t, Int_t);

  Int_t GetBit(TString, Int_t);
  TString GetBitName(Int_t, Int_t);
  Int_t GetL0Detector(TString);

  void GetL0TPConfig(Int_t L0Detector, Int_t &bit, Int_t &cut);
  void BuildL0TPInfo();

  Bool_t CheckL0Detector(Int_t);
  Bool_t CheckRefTime(Int_t);
  Bool_t CheckL0Slot(Int_t);
  Bool_t CheckPrimBit(Int_t);
  Bool_t CheckL0TPInfo(Int_t, Int_t);

  Bool_t fDebug;
  Int_t fRunNumber;
  L0TPData* fL0TPData;

  std::map<TString, Int_t> fBitNameToDetectorID;
  std::vector<L0DetectorBits> fAllDetectorBits;
  TString detkey[9];
  std::vector< std::pair<Int_t,Int_t> > fL0TPInfo;

  Bool_t fEnabledEmulators[7];
  std::vector<TString> fL0EmulatorConfig;

  BaseAnalysis* fBase;
};

#endif
