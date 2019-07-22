// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------
#include "TNewCHODHit.hh"

ClassImp(TNewCHODHit)

TNewCHODHit::TNewCHODHit() : TDetectorVHit(), NewCHODChannelID() {}

Int_t TNewCHODHit::EncodeChannelID() {
  fChannelID = NewCHODChannelID::EncodeChannelID();
  return fChannelID;
}

void TNewCHODHit::DecodeChannelID() {
  NewCHODChannelID::DecodeChannelID(fChannelID);
}

void TNewCHODHit::Clear(Option_t* option){
  TDetectorVHit::Clear(option);
  NewCHODChannelID::Clear(option);
}
