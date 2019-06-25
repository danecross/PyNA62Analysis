#ifndef LAVTRACKFINDER_H
#define LAVTRACKFINDER_H 1

#include "TRecoLAVEvent.hh"
#include "TRecoLAVHit.hh"
#include "LAVCluster.hh"
#include <vector>
#include <iostream>
using namespace std;

class LAVTrackFinder
{
public:
  LAVTrackFinder(); 
  ~LAVTrackFinder();
  void Init(TRecoLAVEvent*); // event
  void FindTracks(TRecoLAVEvent*);
  void Clear();

// Interface
  void SetPrintLevel(Int_t val){fPrintLevel = val;}
  void SetPhiBinWidth(Double_t val){fPhiBinWidth = val; fNPhiBins = (2*TMath::Pi())/fPhiBinWidth;}
  void SetTimeBinWidth(Double_t val){fTimeBinWidth = val;}
  Int_t GetNClusters() {return fNClusters;}
  LAVCluster* GetCluster(Int_t); // input: cluster index

private:

  Int_t fInitStatus;
  Int_t fNClusters;
  Int_t fPrintLevel;   // verbose level: default 0 (silent); if set to 1, print verbosely

  Double_t fPhiBinWidth; // phi binning: default 0.17453 (10 degrees)
  Int_t fNPhiBins;
  Double_t fTimeBinWidth;// time binning:default 2 ns

  Bool_t HitTrackQuality(TRecoLAVHit*); // quality of the hits to be included in the tracks

  vector <LAVCluster*> fLAVClusters;
  std::vector<Double_t> fPhiRecoHit[12];
  std::vector<Double_t> fTRecoHit[12];
  std::vector<Int_t> fIRecoHit[12];
  Double_t fTMinStation[12];
  Double_t fTMaxStation[12];
};
#endif
