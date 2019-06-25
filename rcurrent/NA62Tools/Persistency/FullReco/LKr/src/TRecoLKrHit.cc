// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoLKrHit.hh"

ClassImp(TRecoLKrHit)

TRecoLKrHit::TRecoLKrHit() : TRecoVHit(), LKrChannelID() {
  fGain=0;
  fPedestal=0;
}

Int_t TRecoLKrHit::EncodeChannelID() {
  fChannelID = LKrChannelID::EncodeChannelID();
  return fChannelID;
}

void TRecoLKrHit::DecodeChannelID() {
  LKrChannelID::DecodeChannelID(fChannelID);
}

void TRecoLKrHit::Clear(Option_t* option){
  TRecoVHit::Clear(option);
  LKrChannelID::Clear(option);
  fGain=0;
  fPedestal=0;
}
