#ifndef LAVRecoHit_H
#define LAVRecoHit_H 1

#include "iostream"
#include "TROOT.h"
#include "TLAVDigi.hh"

using namespace std;

class LAVRecoHit{

public:

  explicit LAVRecoHit( Int_t ); // Pass in input the BlockID without threshold identifier
  ~LAVRecoHit(){}

  void SetLowThreshold(Double_t val, Double_t eval){fLowThreshold = val; fSigmaLowThreshold = eval;}
  void SetHighThreshold(Double_t val, Double_t eval){fHighThreshold = val; fSigmaHighThreshold = eval;}
  void SetHysteresis(Double_t val, Double_t eval){fHysteresis = val; fSigmaHysteresis = eval;}
  void SetRiseTime(Double_t val, Double_t eval){fRiseTime = val; fSigmaRiseTime = eval;}
  void SetTau(Double_t val, Double_t eval){fTau = val; fSigmaTau = eval;}
  void SetTimeErrorLow(Double_t val){fSigmaTLow = val;}
  void SetTimeErrorHigh(Double_t val){fSigmaTHigh = val;}

  void SetLeadingEdgeLow(Double_t edgeTime){fLeadingEdgeLow = edgeTime    ; fEdgeMask |= 1;}
  void SetLeadingEdgeHigh(Double_t edgeTime){fLeadingEdgeHigh = edgeTime  ; fEdgeMask |= 2;}
  void SetTrailingEdgeHigh(Double_t edgeTime){fTrailingEdgeHigh = edgeTime; fEdgeMask |= 4;}
  void SetTrailingEdgeLow(Double_t edgeTime){fTrailingEdgeLow = edgeTime  ; fEdgeMask |= 8;}

  void SetDigiLeadingEdgeLow  (TLAVDigi* digi){fDigiLeadingEdgeLow = digi;}
  void SetDigiLeadingEdgeHigh (TLAVDigi* digi){fDigiLeadingEdgeHigh = digi;}
  void SetDigiTrailingEdgeHigh(TLAVDigi* digi){fDigiTrailingEdgeHigh = digi;}
  void SetDigiTrailingEdgeLow (TLAVDigi* digi){fDigiTrailingEdgeLow = digi;}
  
  Double_t GetLeadingEdgeLow(){if (fEdgeMask & 1) {return fLeadingEdgeLow;} else {return 0;}}
  Double_t GetLeadingEdgeHigh(){if (fEdgeMask & 2) {return fLeadingEdgeHigh;} else {return 0;}}
  Double_t GetTrailingEdgeHigh(){if (fEdgeMask & 4) {return fTrailingEdgeHigh;} else {return 0;}}
  Double_t GetTrailingEdgeLow(){if (fEdgeMask & 8) {return fTrailingEdgeLow;} else {return 0;}}

  TLAVDigi* GetDigiLeadingEdgeLow(){if (fEdgeMask & 1) {return fDigiLeadingEdgeLow;} else {return NULL;}}
  TLAVDigi* GetDigiLeadingEdgeHigh(){if (fEdgeMask & 2) {return fDigiLeadingEdgeHigh;} else {return NULL;}}
  TLAVDigi* GetDigiTrailingEdgeHigh(){if (fEdgeMask & 4) {return fDigiTrailingEdgeHigh;} else {return NULL;}}
  TLAVDigi* GetDigiTrailingEdgeLow(){if (fEdgeMask & 8) {return fDigiTrailingEdgeLow;} else {return NULL;}}

  Int_t GetEdgeMask(){return fEdgeMask;}

  void HitReconstruct();

  Double_t* GetTStartResults();
  Double_t* GetTMaxResults();
  Double_t* GetVMaxResults();
  Double_t* GetChargeResults();

  Double_t GetResidualSlewingCorrection(Int_t, Double_t*, Int_t, Double_t*, Int_t);

  void Print();

private:
  Double_t fLowThreshold;
  Double_t fSigmaLowThreshold;
  Double_t fHighThreshold;
  Double_t fSigmaHighThreshold;
  Double_t fHysteresis;
  Double_t fSigmaHysteresis;
  Double_t fRiseTime;
  Double_t fSigmaRiseTime;
  Double_t fTau;
  Double_t fSigmaTau;
  Double_t fSigmaTLow;
  Double_t fSigmaTHigh;

  Int_t fBlockID;
  Int_t fEdgeMask; 

  TLAVDigi* fDigiLeadingEdgeLow;
  TLAVDigi* fDigiTrailingEdgeLow;
  TLAVDigi* fDigiLeadingEdgeHigh;
  TLAVDigi* fDigiTrailingEdgeHigh;

  Double_t fLeadingEdgeLow;
  Double_t fTrailingEdgeLow;
  Double_t fLeadingEdgeHigh;
  Double_t fTrailingEdgeHigh;

  Double_t fTStartResults[6];    // value1,error1, value2,error2, ...
  Double_t fTMaxResults[6];      // value1,error1, value2,error2, ...
  Double_t fVMaxResults[10];     // value1,error1, value2,error2, ...
  Double_t fChargeResults[10];   // value1,error1, value2,error2, ...

// For each quantity below: [0] = Value; [1] = Error

  Double_t fTStartTOTLow[2];   // time of the signal start from the TOT Low threshold
  Double_t fTStartTOTHigh[2];  // time of the signal start from the TOT High threshold
  Double_t fTStartSlope[2];    // time of the signal start after slewing corrections from the slope of the leading (no RiseTime assumed)

  Double_t fTMaxTOTLow[2];   // time of the maximum of the signal from the TOT Low threshold (RiseTime assumed)
  Double_t fTMaxTOTHigh[2];  // time of the maximum of the signal from the TOT High threshold (RiseTime assumed)
  Double_t fTMaxSlope[2];    // time of the maximum of the signal after slewing corrections from the slope of the leading  (RiseTime assumed)

  Double_t fVMaxTOTLow[2];   // maximum of the signal from the TOT Low threshold 
  Double_t fVMaxTOTHigh[2];  // maximum of the signal from the TOT High threshold
  Double_t fVMaxSlope[2];    // maximum of the signal after slewing corrections from the slope of the leading (bad if time delta is too smaal)
  Double_t fVMaxSlopeTrailingLow[2];     // maximum of the signal for large slope of the leading time + low threshold trailing
  Double_t fVMaxSlopeTrailingHigh[2];    // maximum of the signal for large slope of the leading time + high threshold trailing 

  Double_t fChargeTOTLow[2];   // charge of the signal after corrections from the TOT Low threshold
  Double_t fChargeTOTHigh[2];  // charge of the signal after corrections from the TOT High threshold
  Double_t fChargeSlope[2];    // charge of the signal after slewing corrections from the slope of the leading
  Double_t fChargeSlopeTrailingLow[2];     // charge of the signal for large slope of the leading time + low threshold trailing
  Double_t fChargeSlopeTrailingHigh[2];    // charge of the signal for large slope of the leading time + high threshold trailing 

  Double_t fTStartBest[2]; // best estimate of the starting time
  Double_t fTMaxBest[2];   // best estimate of the peak time
  Double_t fVMaxBest[2];   // best estimate of the maximum voltage
  Double_t fChargeBest[2]; // best estimate of the integrated charge

  Double_t* VMaxFromTOT(Double_t, Double_t, Double_t); // leadingTime, trailingTime, threshold
  Double_t* TMaxFromTOT(Double_t, Double_t, Double_t, Double_t); // leadingTime, threshold, vmax, error_vmax
  Double_t* ChargeFromVMax(Double_t, Double_t); // vmax, error_vmax
  Double_t WAsymptotic(Double_t); // gives the Lambertian zero-branch for a given positive input, using the asymptotic expansion
  Double_t Lambertian0(Double_t); // gives the Lambertian zero-branch for a given positive input, using the Fritch's iteration method

};

#endif

