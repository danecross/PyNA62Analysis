#ifndef VL0EMULATOR_HH
#define VL0EMULATOR_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include <map>
#include <random>

class TH1I;
class TH2F;
class TGraph;
class TTree;

#include "EmulatedL0Primitive.hh"

typedef std::map<UInt_t, HitVec > HitMap;
typedef std::vector<EmulatedL0Primitive> ClusVec;

class VL0Emulator : public NA62Analysis::Analyzer{
public:
  VL0Emulator(NA62Analysis::Core::BaseAnalysis *ba, TString DetectorName);
  ~VL0Emulator();
  
  // Standard methods
  virtual void InitHist(); // Now virtual, allows SD specific implementation
  void InitOutput();
  void DefineMCSimple() {}
  void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
  void Process(Int_t);
  void PostProcess();
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser() {}
  void EndOfRunUser();
  void EndOfJobUser();
  void DrawPlot();

  // pure virtual L0 emulator-specific methods
  virtual void FillTimes()=0;
  virtual void Simple()=0;
  virtual void Detailed()=0;
  virtual void GenerateAccidentals()=0;
  virtual void SetPrimitiveIDs(ClusVec::iterator clustit)=0;
  
  // real L0 emulator-specific methods
  void EventTimes();
  HitMap SplitAlgo(HitVec& input, Int_t SplitSize);
  HitMap SplitByPP(HitVec& input);
  void ShuffleAlgo(HitVec& input);
  void SplitClear(HitMap::iterator it);
  void SplitLimit(HitMap::iterator it);
  void InsertAlgo(ClusVec& clusters, ClusVec::iterator clustit, HitVec::iterator hitit);
  void L0TPOverwriting();
  void BuildPrimitives();
  void SortAlgo(ClusVec& clusters);
  void PartialSort(ClusVec& clusters);
  
  void SetAllEdges();

  // wrappers for SplitAlgo
  HitMap SplitSlots(HitVec& input);
  HitMap Split100(HitVec& input);
  HitMap SplitFrames(HitVec& input);
  
protected:

  Int_t   fL0Detector;
  TString fDetectorName;

  Bool_t fDebug;
  Bool_t fActive;
  Bool_t fSimple;
  Int_t  fRunPeriod;

  Int_t    fShuffleAlgo;
  Int_t    fShuffleSeed;
  Double_t fShuffleFrac;
  
  Int_t    fFineTimeBit;
  Double_t fL0Window;
  Int_t    fClusteringWindow;

  Bool_t   fGenEventTime;
  Int_t    fGenAlgo;
  Int_t    fGenAccidentals;
  Double_t fGTKIntensity;
  Double_t fEventSize;
  Int_t    fGenSeed;

  Double_t fEventTimeStamp;
  Double_t fL0Reference;

  HitVec  fTimes;
  ClusVec fEventClusters;

  TRandom3* fRanGen;
  std::mt19937 fShuffler;

  Int_t fClusterModuleSize;
  
private:
  
};

#endif
