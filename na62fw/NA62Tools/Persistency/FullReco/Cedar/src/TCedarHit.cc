// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-24
//
// --------------------------------------------------------------

#include "TCedarHit.hh"

ClassImp(TCedarHit)

TCedarHit::TCedarHit() : TDetectorVHit(), CedarChannelID() {
}

Int_t TCedarHit::EncodeChannelID() {
  fChannelID = CedarChannelID::EncodeChannelID();
  return fChannelID;
}

void TCedarHit::DecodeChannelID() {
  CedarChannelID::DecodeChannelID(fChannelID);
}
void TCedarHit::Clear(Option_t* option) {
  TDetectorVHit::Clear(option);
  CedarChannelID::Clear(option);
}
