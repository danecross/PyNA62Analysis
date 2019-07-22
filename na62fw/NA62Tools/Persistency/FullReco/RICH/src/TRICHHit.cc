// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#include "TRICHHit.hh"

ClassImp(TRICHHit)

TRICHHit::TRICHHit() : TDetectorVHit() {
}

TRICHHit::TRICHHit(Int_t iCh) : TDetectorVHit(iCh) {
}

void TRICHHit::Clear(Option_t* option){
  TDetectorVHit::Clear(option);
}

Int_t TRICHHit::EncodeChannelID() {
  fChannelID=RICHChannelID::EncodeChannelID();
  return fChannelID;
}

void TRICHHit::DecodeChannelID() {
  RICHChannelID::DecodeChannelID(fChannelID);
}
