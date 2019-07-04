// Created by Vito Palladino 17/1/2010
// Modified by E.Leonardi 2014-06-13

#ifndef LAVPMT_hh
#define LAVPMT_hh 1

#include "TH1D.h"
#include <vector>
#include "LAVGeometry.hh"
#include "LAVPhotocathode.hh"
#include "TRandom3.h"

class LAVpmt{

public:

  explicit LAVpmt(TRandom3*);
  ~LAVpmt();

  void SetChannelID(Int_t);

  void AddPhotonElement(Double_t, Double_t); // Time, Energy

  Double_t  GetRange()  { return fTimeRange; }
  Double_t  GetOffset() { return fTimeOffset; }
  Int_t     GetNBins()  { return fTimeNBins; }

  Bool_t SignalIsPresent() { return !fNoSignal; }

  void Process();

  Double_t GetLastDynodeElectronDistribution(Int_t bin) { return fSignalShape[bin]; }
  Double_t GetPhotoElectronTMin(){ return fPhotoElectronTMin; }
  Double_t GetPhotoElectronTMax(){ return fPhotoElectronTMax; }
  Int_t GetNPhotoElectrons() {return fNPhotoElectrons;}

private:

  void Clear();

  void ApplyExactDynode(Int_t);
  void ApplyHistoDynode(Int_t);

  void GenerateSignal();

private:

  TRandom3* fRandom;
  Bool_t IsFirstElement;
  Int_t fBlockGlobalID; // PMT is attached to this block

  LAVGeometry* fGeometry;

  Double_t fTransitTime;
  Double_t fTransitTimeSpread;
  Double_t fTau;
  Int_t fNsigmaTTS;
  Int_t fNtauSignal;

  LAVPhotocathode* fPhotocathode;

  std::vector<Double_t> fPrimaryPhotons;
  std::vector<Double_t> fPhotoElectrons;
  std::vector<Double_t> fPhotoElectronsOut;

  Double_t fPhotoElectronTMin;
  Double_t fPhotoElectronTMax;
  Int_t fNPhotoElectrons;

  Bool_t fNoSignal;

  Double_t* fSignalShape;

  Double_t fTimeOffset;
  Double_t fTimeRange;
  Double_t fTimeBinWidth;
  Int_t    fTimeNBins;

  Int_t fNDynodes;
  Int_t fFFTLimit;
  Bool_t fFFTon;
};


#endif
