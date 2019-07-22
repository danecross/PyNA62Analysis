// TLAVHit.cc
// --------------------------------------------------------------
// History:
//
// 2015-03-19 T. Spadaro (tommaso.spadaro@lnf.infn.it) promoting c++ variables to root types whenever possible
// 2015-01-22 First implementation of LAVChannelID and revision by T. Spadaro and E. Leonardi
// 2009-03-02 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - add LAV specific hit information
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-24
//
// --------------------------------------------------------------
#include "TLAVHit.hh"

ClassImp(TLAVHit)

 #include "Riostream.h"

TLAVHit::TLAVHit() : TDetectorVHit() {}

Int_t TLAVHit::EncodeChannelID(){
  fChannelID = LAVChannelID::EncodeChannelID();
  return fChannelID;
}

void TLAVHit::DecodeChannelID(){
  LAVChannelID::DecodeChannelID(fChannelID);
}

void TLAVHit::Print(Option_t * /*option*/) const{
  TDetectorVHit::Print();
  std::cout << "Persistent LAV hit "	 
	    << " Optical Photons " << fPhotonsNumber <<std::endl;
  
//  for (unsigned Int_t i=0; i< (Int_t) fPhotonsNumber; i++){
//    cout << "photon " << i << " E = " << fPhotonsEnergy[i] << " T = " << fPhotonsTime[i] << endl;
//  }

  Double_t tmin=9999999;
  Double_t tmax=-9999999;

  for (Int_t i=0; i< (Int_t) fPhotonsNumber; i++){
    if (fPhotonsTime[i] < tmin) tmin = fPhotonsTime[i];
    if (fPhotonsTime[i] > tmax) tmax = fPhotonsTime[i];
  }
  std::cout << "--TLAVHit: Tmin = " << tmin << " Tmax = " << tmax << std::endl;  
}

void TLAVHit::Clear(Option_t* option){ 
  TDetectorVHit::Clear(option);
}
