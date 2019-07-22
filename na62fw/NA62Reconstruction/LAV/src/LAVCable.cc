
//
// Created by Vito Palladino 2/5/2011
//


#include "LAVCable.hh"
#include "LAVGeometry.hh"

#include "TMath.h"
#include "TComplex.h"
#include "TVirtualFFT.h"
#include "TString.h"


LAVCable::LAVCable( Double_t CableLength, Double_t L, Double_t C, Double_t G, Double_t R, Double_t MaxFreq ) :
  fCableLength(CableLength),
  fL(L),
  fR(R),
  fG(G),
  fC(C),
  fMaxFreq(MaxFreq),
  fSignalIN(nullptr),
  fSignalOUT(nullptr),
  fBinWidth(.0),
  fOffset(.0),
  fSignalMax(.0),
  fSignalPeak(.0),
  fCharge(.0),
  fhSignalOUT(nullptr)
{
  Clear();

}


Double_t* LAVCable::GetSignal(Double_t LeTh, Double_t TrTh){ 

  if( fSignalOUT==0 && fNbins!=0){

    fSignalOUT = new Double_t[fNbins];

    Double_t para[] = {fCableLength,fL, fC, fR, fG};

    TVirtualFFT* SignalFFT = TVirtualFFT::FFT(1, &fNbins, "R2C ES");
    SignalFFT->SetPoints(fSignalIN);
    SignalFFT->Transform();

    Double_t* Re2Transform = new Double_t[fNbins];
    Double_t* Im2Transform = new Double_t[fNbins];

    for(Int_t i=0; i<fNbins; i++){
      Double_t Re, Im;
      SignalFFT->GetPointComplex(i, Re, Im);
      TComplex Point(Re, Im);

      Double_t x[] = {fMaxFreq*(i+0.5)/fNbins};

      TComplex S2Transform(Point.Rho()*Attenuation(x,para), Point.Theta()+PhaseInward(x,para), 1);
      Re2Transform[i] = S2Transform.Re();
      Im2Transform[i] = S2Transform.Im();
    }

    TVirtualFFT* aFFT = TVirtualFFT::FFT(1, &fNbins, "C2R ES");
    aFFT->SetPointsComplex(Re2Transform,Im2Transform);
    aFFT->Transform();
    aFFT->GetPoints(fSignalOUT);
  
    delete[] Re2Transform;
    delete[] Im2Transform; 
  
    fSignalOUT[0] = fSignalOUT[0]/fNbins;
    fCharge = fSignalOUT[0];

    Double_t SignalMax = fSignalOUT[0];
    Int_t iMax = 0;

    for(Int_t i=1; i<fNbins; i++){    
    
      fSignalOUT[i] = fSignalOUT[i]/fNbins;

      if( fSignalOUT[i] >= LeTh && fSignalOUT[i-1] < LeTh )
	fLeading.push_back( i*fBinWidth+fOffset );   
      if( fSignalOUT[i] <= TrTh && fSignalOUT[i-1] > TrTh )
	fTrailing.push_back( i*fBinWidth+fOffset );
    
      if (fSignalOUT[i] > SignalMax) {
	SignalMax = fSignalOUT[i];
	iMax = i;
      }
    
      fCharge += fSignalOUT[i];
      
    }
    fCharge = fCharge/50.*fBinWidth*1.e3;
    fSignalMax = iMax*fBinWidth + fOffset;
    fSignalPeak = SignalMax;

#ifdef __LAVFindSignalRiseTime__

    Double_t RiseLevel[2] = {SignalMax*0.1, SignalMax*0.9};
    for (Int_t i = 1; i < iMax; i++){

      if( fSignalOUT[i] >= RiseLevel[0] && fSignalOUT[i-1] < RiseLevel[0])
	fRiseTime = -i;

      if( fSignalOUT[i] >= RiseLevel[1] && fSignalOUT[i-1] < RiseLevel[1])
	fRiseTime += i;    
    }
    fRiseTime = fRiseTime*fBinWidth;

#endif  
  }

  return fSignalOUT;

}


Double_t LAVCable::Attenuation(Double_t* x, Double_t* par){

  // [0] cable length
  // [1] L
  // [2] C
  // [3] R
  // [4] G

  Double_t a = TMath::Power(x[0],2)*par[1]*par[2]*((par[3]/(par[1]*x[0]))*(par[4]/(par[2]*x[0]))-1);
  Double_t b = TMath::Power(x[0],2)*par[1]*par[2]*((par[3]/(par[1]*x[0]))+(par[4]/(par[2]*x[0])));
  Double_t phi = TMath::ATan2(b,a)*0.5;

  return TMath::Exp(-TMath::Power((a*a+b*b),0.25)*TMath::Cos(phi)*par[0]);

}


Double_t LAVCable::PhaseInward(Double_t* x, Double_t* par){

  // [0] cable length
  // [1] L
  // [2] C
  // [3] R
  // [4] G

  Double_t a = TMath::Power(x[0],2)*par[1]*par[2]*((par[3]/(par[1]*x[0]))*(par[4]/(par[2]*x[0]))-1);
  Double_t b = TMath::Power(x[0],2)*par[1]*par[2]*((par[3]/(par[1]*x[0]))+(par[4]/(par[2]*x[0])));
  Double_t phi = TMath::ATan2(b,a)*0.5;

  return -TMath::Power((a*a+b*b),0.25)*TMath::Sin(phi)*par[0];

}

void LAVCable::Clear(){

  fLeading.clear();
  fTrailing.clear();

  if( fSignalOUT!=0) {
    delete[] fSignalOUT;
    fSignalOUT = 0;
  }

  fNbins=0;

}
