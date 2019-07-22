// ---------------------------------------------------------------
//
// History:
//
// Handling of +ve and -ve halo particles added by Chris Parkinson, 2019-07-16
// Pileup in LKr added by Michele Corvino 2019-04-30
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2019-02-07
//
// ---------------------------------------------------------------

#ifndef DOWNSTREAMPILEUPGENERATOR_HH
#define DOWNSTREAMPILEUPGENERATOR_HH

// This analyzer can only be run as a pre-analyzer
#pragma pre-analyzer

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include "TRandom2.h"
#include "MUV3Geometry.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class TRecoSpectrometerEvent;
class TRecoNewCHODEvent;
class TRecoMUV3Event;
class TRecoLKrEvent;
class TRecoLAVEvent;
class TRecoIRCEvent;
class TRecoSACEvent;
class TRecoMUV3Candidate;
class TRecoNewCHODHit;
class TRecoLAVHit;
class TRecoIRCHit;
class TRecoSACHit;

namespace dpg{
  union SupplInfo{ Int_t i; Float_t f; SupplInfo() : i(0) {}; SupplInfo(Float_t v) : f(v) {}; SupplInfo(Int_t v) : i(v) {} };
  template <int N>
  union SupplInfoArr{ Int_t i[N]; Float_t f[N]; };
  //ChannelID, Time, Other1, Other2, Other3
  typedef std::tuple<Int_t, Float_t, SupplInfo, SupplInfo, SupplInfo> pileuphit;
  template <int N, int M>
  struct DetLib {
      Int_t fNHits;
      Int_t fChannelID[N];
      Float_t fTime[N];
      SupplInfoArr<N> fOther1;
      SupplInfoArr<N> fOther2;
      SupplInfoArr<N> fOther3;
      TBranch* fBranches[M];
  };

}

class DownstreamPileupGenerator : public NA62Analysis::Analyzer
{
public:
  explicit DownstreamPileupGenerator(NA62Analysis::Core::BaseAnalysis *ba);
  ~DownstreamPileupGenerator();
  void InitHist();
  void InitOutput();
  void DefineMCSimple();
  void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
  void Process(int iEvent);
  void PostProcess();
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser();
  void EndOfJobUser();
  void DrawPlot();

protected:
  void ReadHitLibrary();
  template <int N, int M>
  std::vector<dpg::pileuphit> LoadDetectorEntry(int i, dpg::DetLib<N, M>& lib){
      std::vector<dpg::pileuphit> entry;
      dpg::SupplInfo def;
      for(unsigned k=0; k<M; ++k) lib.fBranches[k]->GetEntry(i);

      entry.resize(lib.fNHits);
      for(Int_t k=0; k<lib.fNHits; ++k)
          entry[k] = dpg::pileuphit(lib.fChannelID[k], lib.fTime[k], lib.fOther1.f[k], lib.fOther2.f[k], lib.fOther3.f[k]);
      return entry;
  }
  UInt_t DecodePID(Int_t particleID);
  UInt_t KaonDecay(std::vector<Double_t>& kaonDecays);

  void GeneratePileup(Double_t time, UInt_t lib);
  void GenerateSpectrometerPileup(TRecoSpectrometerEvent*, Double_t time, const std::vector<dpg::pileuphit>& pileup);
  void GenerateNewCHODPileup     (TRecoNewCHODEvent*     , Double_t time, const std::vector<dpg::pileuphit>& pileup, Int_t lib);
  void GenerateMUV3Pileup        (TRecoMUV3Event*        , Double_t time, const std::vector<dpg::pileuphit>& pileup, Int_t lib);
  void GenerateLKrPileup         (TRecoLKrEvent*         , Double_t time, const std::vector<dpg::pileuphit>& pileup);
  void GenerateLAVPileup         (TRecoLAVEvent*         , Double_t time, const std::vector<dpg::pileuphit>& pileup);
  void GenerateIRCPileup         (TRecoIRCEvent*         , Double_t time, const std::vector<dpg::pileuphit>& pileup);
  void GenerateSACPileup         (TRecoSACEvent*         , Double_t time, const std::vector<dpg::pileuphit>& pileup);
  
  template<typename EventType, typename HitType> void ApplyDeadtime();
  template<typename EventType, typename HitType> void ChannelPlot(TString name);
  void FillChannelPlot(TRecoMUV3Candidate* cand, TString name);
  void FillChannelPlot(TRecoNewCHODHit* cand, TString name);

  Bool_t fGenerateSpectrometerPileup; ///< Should pileup in the Spectrometer be generated?
  Bool_t fGenerateNewCHODPileup;      ///< Should pileup in the NewCHOD be generated?
  Bool_t fGenerateMUV3Pileup;         ///< Should pileup in the MUV3 be generated?
  Bool_t fGenerateLKrPileup;         ///< Should pileup in the LKr be generated?
  Bool_t fGenerateLAVPileup;         ///< Should pileup in the LAV be generated?
  Bool_t fGenerateIRCPileup;         ///< Should pileup in the IRC be generated?
  Bool_t fGenerateSACPileup;         ///< Should pileup in the SAC be generated?

  Bool_t fGenerateHaloPileup;        ///< Should pileup from halo particles be generated?

  Bool_t fApplyMUV3Deadtime;         ///< Should apply MUV3 deadtime?
  Bool_t fApplyNewCHODDeadtime;      ///< Should apply NewCHOD deadtime?
  Double_t fCFDDeadtime;             ///< Duration of CFD deadtime (for muv3, newchod)

  Double_t fKaonDecayFraction;       ///< fraction of beam kaons that will decay in 102.425 - 180.000m
  Double_t fKaonEDRDecayFraction;    ///< fraction of beam kaons that will decay in 102.425 - 265.000m
  Double_t fPionEDRDecayFraction;    ///< fraction of beam pions that will decay in 102.425 - 265.000m
  Double_t fHaloPositiveFraction;    ///< Fraction of positively charged beam halo

  std::vector<Double_t> fKaonDecays; ///< kaon decay branching fractions.
  Double_t fTotalKaonDecay;          ///< Total kaon branching fraction of 6 main modes.

  Bool_t fUseEDR; ///< Generate beam decays in the extended decay region?
  std::vector<Double_t> fKaonEDRDecays; ///< kaon decay branching fractions for EDR.

  TRandom2* fRandom; ///< local random number generator.
  MUV3Geometry *fMUV3Geo; ///< Pointer to MUV3 geometry class.

  const UInt_t fNLibraries; ///< hardcoded number of libraries to read.
  const UInt_t fNDetectors; ///< hardcoded number of detectors to read.

  std::vector<TString> fLibNames;

  // Libraries(11) -> Events(~10^5) -> Detector(3) -> Hits(~100) -> pileuphit
  std::vector<std::vector<std::vector<std::vector<dpg::pileuphit>>>> fHitLibrary;
  Bool_t fForcedOnData;
  TRecoSpectrometerEvent* fSpectrometerEvent;
  TRecoNewCHODEvent* fNewCHODEvent;
  TRecoMUV3Event* fMUV3Event;
  TRecoLKrEvent* fLKrEvent; 
  TRecoLAVEvent* fLAVEvent; 
};
#endif
