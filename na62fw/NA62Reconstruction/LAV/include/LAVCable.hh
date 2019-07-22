//
// Created by Vito Palladino 2/5/2011
//

#ifndef LAVCable_HH
#define LAVCable_HH 1

#include "TH1D.h"

#include "vector"

using namespace std;

class LAVCable{

public:

  LAVCable( Double_t CableLength, Double_t L, Double_t C, Double_t G, Double_t R, Double_t MaxFreq); 
  // CableLength in meters, L in Henry/m, C in Farad/m, G in Siemens/m, R in Ohm/m, MaxFreq in Hertz

  void SetSignal(Double_t* SignalIN, Double_t Offset, Double_t BinWidth, Int_t Nbins) { fSignalIN = SignalIN; fOffset = Offset; fBinWidth = BinWidth; fNbins = Nbins;}

  Double_t* GetSignal(Double_t LeTh, Double_t TrTh);
  
  std::vector<Float_t> GetLeadingLow(Double_t LeTh, Double_t TrTh)  { if( fSignalOUT==0 ) GetSignal(LeTh, TrTh); return fLeading;  } // needs to call GetSignal
  std::vector<Float_t> GetTrailingLow(Double_t LeTh, Double_t TrTh) { if( fSignalOUT==0 ) GetSignal(LeTh, TrTh); return fTrailing; } // needs to call GetSignal

  Double_t GetSignalMax(Double_t LeTh, Double_t TrTh)        { if( fSignalOUT==0 ) GetSignal(LeTh, TrTh); return fSignalMax; }  // needs to call GetSignal
  Double_t GetSignalPeak(Double_t LeTh, Double_t TrTh)       { if( fSignalOUT==0 ) GetSignal(LeTh, TrTh); return fSignalPeak; } // needs to call GetSignal

  Double_t GetCharge(Double_t LeTh, Double_t TrTh)           { if( fSignalOUT==0 ) GetSignal(LeTh, TrTh); return fCharge; } // needs to call GetSignal

#ifdef __LAVFindSignalRiseTime__
  Double_t GetRiseTime(Double_t LeTh, Double_t TrTh)         { if( fSignalOUT==0 ) GetSignal(LeTh, TrTh); return fRiseTime; } // needs to call GetSignal
#endif  

  void Clear();
  
private:
  
  Double_t Attenuation(Double_t*, Double_t*);
  Double_t PhaseInward(Double_t*, Double_t*);
  
private:
  
  Int_t fNbins;
  
  Double_t fCableLength;
  Double_t fL, fR, fG, fC; 
  Double_t fMaxFreq;
  
  Double_t * fSignalIN, *fSignalOUT;

  Double_t fBinWidth;
  Double_t fOffset;

  std::vector<Float_t> fLeading, fTrailing;
  Double_t fSignalMax;
  Double_t fSignalPeak;
  Double_t fCharge;

#ifdef __LAVFindSignalRiseTime__
  Double_t fRiseTime;
#endif

  TH1D* fhSignalOUT;
};

#endif
