// ---------------------------------------------------------
// History:
//
// Pileup in LKr added by Michele Corvino 2019-04-30
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2019-04-23
//
// ---------------------------------------------------------
#ifndef BUILDDPGHITLIBRARY_HH
#define BUILDDPGHITLIBRARY_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>

class TRecoSpectrometerEvent;
class TRecoMUV3Event;
class TRecoNewCHODEvent;
class TRecoLAVEvent;
class TRecoIRCEvent;
class TRecoSACEvent;
class TRecoLKrEvent;

class BuildDPGHitLibrary : public NA62Analysis::Analyzer
{
public:
  explicit BuildDPGHitLibrary(NA62Analysis::Core::BaseAnalysis *ba);
  ~BuildDPGHitLibrary();
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
  template <int N>
  union SupplInfo{ Int_t i[N]; Float_t f[N]; };

  template <int N>
  struct DetLib {
      Int_t fNHits;
      Int_t fChannelID[N];
      Float_t fTime[N];
      SupplInfo<N> fOther1;
      SupplInfo<N> fOther2;
      SupplInfo<N> fOther3;
  };

  void AddDetectorHit(TRecoSpectrometerEvent *event);
  void AddDetectorHit(TRecoMUV3Event *event);
  void AddDetectorHit(TRecoNewCHODEvent *event);
  void AddDetectorHit(TRecoLAVEvent *event);
  void AddDetectorHit(TRecoIRCEvent *event);
  void AddDetectorHit(TRecoSACEvent *event);
  void AddDetectorHit(TRecoLKrEvent *event);

  template <class TEvent, int N>
    void AddDetectorHits(DetLib<N> &lib){
        TEvent* event = GetEvent<TEvent>("Reco");
        Int_t hits = event->GetNHits();
        lib.fNHits = hits;
        if(hits==0) // if the event is empty
          return ;
        else if(hits<N)
          AddDetectorHit(event);
        else{
          std::cout << " WARNING LARGE " << event->ClassName() << " N= " << hits << std::endl;
          return ;
        }
    }
  template <class TEvent, int N>
    void AddDetectorCandidates(DetLib<N> &lib){
        TEvent* event = GetEvent<TEvent>("Reco");
        Int_t candidates = event->GetNCandidates();
        lib.fNHits = candidates;
        if(candidates==0) // if the event is empty
          return ;
        else if(candidates<N)
          AddDetectorHit(event);
        else{
          std::cout << " WARNING LARGE " << event->ClassName() << " N= " << candidates << std::endl;
          return ;
        }
    }


  TFile* fRootFile;
  TTree* fSpecTree;
  TString fOutputName;

  DetLib<2000> fLib_spectrometer;
  DetLib<500>  fLib_muv3;
  DetLib<500>  fLib_newchod;
  DetLib<500>  fLib_lav;
  DetLib<20>   fLib_irc;
  DetLib<15>   fLib_sac;
  DetLib<25>   fLib_lkr;
};
#endif
