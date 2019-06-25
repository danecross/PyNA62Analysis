// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Updated: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-02
//
// --------------------------------------------------------------
#include "TRecoCHODHit.hh"

ClassImp(TRecoCHODHit)

TRecoCHODHit::TRecoCHODHit() : TRecoVHit(), CHODChannelID() {
  fTimeWidth = 0;
}

Int_t TRecoCHODHit::EncodeChannelID() {
  fChannelID = CHODChannelID::EncodeChannelID();
  return fChannelID;
}

void TRecoCHODHit::DecodeChannelID() {
  CHODChannelID::DecodeChannelID(fChannelID);
}

void TRecoCHODHit::Clear(Option_t* option) {
  TRecoVHit::Clear(option);
  CHODChannelID::Clear(option);
  fTimeWidth = 0;
}
