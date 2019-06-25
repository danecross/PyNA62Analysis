// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-24
//
// --------------------------------------------------------------
#include "TLKrHit.hh"
ClassImp(TLKrHit)

TLKrHit::TLKrHit() : TDetectorVHit(), LKrChannelID()  {
////<<    fMicroCellData = 0;
  fNSlices = 128;
  fSlicesInX = 100;
  fSlicesInY = 50;
  fCurrent = 0;
}

TLKrHit::~TLKrHit(){
}

void TLKrHit::Clear(Option_t* option){
  TDetectorVHit::Clear(option);
  LKrChannelID::Clear(option);
  fNSlices = 128;
  fSlicesInX = 100;
  fSlicesInY = 50;
  fCurrent = 0;
}

Int_t TLKrHit::EncodeChannelID() {
  fChannelID = LKrChannelID::EncodeChannelID();
  return fChannelID;
}

void TLKrHit::DecodeChannelID() {
  LKrChannelID::DecodeChannelID(fChannelID);
}
