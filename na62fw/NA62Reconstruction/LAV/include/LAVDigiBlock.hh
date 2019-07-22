#ifndef LAVDigiBlock_H
#define LAVDigiBlock_H 1

#include <iostream>
#include <vector>
#include "TLAVHit.hh"
#include "TRandom3.h"

using namespace std;

class LAVDigiBlock{

public:

  LAVDigiBlock(Int_t, Int_t); // BlockID, nOpticalPhotons
  ~LAVDigiBlock();

  void SetHitTime(Double_t HitTime){fHitTime = HitTime;}
  Double_t GetHitTime(){return fHitTime;}

  void SetMCHit(TLAVHit* Hit){fMCHit = Hit;}
  TLAVHit* GetMCHit(){return fMCHit;}

  void SetOpticalPhotonElement(Int_t i, Double_t T, Double_t E){fOpticalPhotonTime[i] = T; fOpticalPhotonEnergy[i] = E;}
  void SetOpticalPhotonTimeRange(Double_t TMin, Double_t TMax){fOpticalPhotonTMin = TMin; fOpticalPhotonTMax = TMax;}


  Int_t GetNOpticalPhotons(){return fNOpticalPhotons;}
  Int_t GetNPhotoElectrons(){return fNPhotoElectrons;} // after applying the Quantum efficiency at the photocathode
  Double_t GetOpticalPhotonTimeElement(Int_t i){return fOpticalPhotonTime[i];}
  Double_t GetOpticalPhotonEnergyElement(Int_t i){return fOpticalPhotonEnergy[i];}

  Double_t GetOpticalPhotonTimeMin(){return fOpticalPhotonTMin;}
  Double_t GetOpticalPhotonTimeMax(){return fOpticalPhotonTMax;}
  Double_t GetPhotoElectronTMin(){ return fPhotoElectronTMin; }
  Double_t GetPhotoElectronTMax(){ return fPhotoElectronTMax; }


  Double_t GetHitOffset(){return fTOffset;}
  Int_t GetNBins(){return fNBins;}
  Double_t GetTimeRange(){return fTRange;}

  Double_t GetTimeBegin(){return fTOffset;}
  Double_t GetTimeBin(){return fBinWidth;}
  Double_t GetTimeEnd(){return fTOffset + fTRange;}

  void SetSignalElement(Int_t, Double_t);
  Double_t GetSignalElement(Int_t);
  Double_t GetGeneratedSignalElement(Int_t);

  
  Int_t GetBlockID(){return fBlockID;}

  void Add(LAVDigiBlock*); // merge intervals from different hits of the same 

  Int_t ApplyPMT(TRandom3*); // generate last dynode photoelectron distribution and return number of time bins
  void Digitize(); // generate signal and digitize it

  Double_t GetTSignalMax()         { return fTSignalMax; }       // To use only if Digitize has been called, otherwise the result is not relaiable
  Double_t GetSignalPeak()         { return fSignalPeak; }       // To use only if Digitize has been called, otherwise the result is not relaiable
  Double_t GetCharge()            { return fCharge; }          // To use only if Digitize has been called, otherwise the result is not relaiable

  Int_t GetNEdgesLow(){return fEdgesLow.size();}
  Int_t GetNEdgesHigh(){return fEdgesHigh.size();}

  Double_t GetEdgeLowElement(Int_t i){return fEdgesLow.at(i);} // maybe add a protection on the input, later on
  Double_t GetEdgeHighElement(Int_t i){return fEdgesHigh.at(i);}
  Double_t GetEdgeElement(Int_t i, Int_t ithr){if (ithr==0) {return GetEdgeLowElement(i);} else {return GetEdgeHighElement(i);} }

  Int_t GetEdgeTypeLowElement(Int_t i){return fEdgeTypesLow.at(i);} // maybe add a protection on the input, later on
  Int_t GetEdgeTypeHighElement(Int_t i){return fEdgeTypesHigh.at(i);}
  Int_t GetEdgeTypeElement(Int_t i, Int_t ithr){if (ithr==0) {return GetEdgeTypeLowElement(i);} else {return GetEdgeTypeHighElement(i);} }

  void Print();
  Double_t GetRiseTime();

private:

  Int_t fBlockID;

  Int_t fNOpticalPhotons;
  Int_t fNPhotoElectrons;
  Double_t fPhotoElectronTMin;
  Double_t fPhotoElectronTMax;

  Double_t* fOpticalPhotonTime;
  Double_t* fOpticalPhotonEnergy;
  Double_t fOpticalPhotonTMin;
  Double_t fOpticalPhotonTMax;

  Double_t fHitTime;
  TLAVHit* fMCHit;

  Double_t fTOffset;
  Double_t fTRange;
  Double_t fBinWidth;

  Double_t fImpedence   ;
  Double_t fTau         ;
  Int_t fNtau           ;
  Double_t fHyTimeC     ;
  Double_t fLeThLow     ;
  Double_t fTrThLow     ;
  Double_t fLeThHigh    ;
  Double_t fTrThHigh    ;
  Double_t fNorm        ;

  Int_t fNBins;

  Double_t* fSignal; // Block Last Dynode Electron Distribution
  Double_t* fGeneratedSignal; // Block Last Dynode Electron Distribution

  std::vector<Float_t> fEdgesLow    ;
  std::vector<Int_t>   fEdgeTypesLow;
  std::vector<Float_t> fEdgesHigh   ;
  std::vector<Int_t>   fEdgeTypesHigh;
  Double_t        fTSignalMax;
  Double_t        fSignalPeak;

  Double_t fCharge;
};

#endif
