// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-24
//
// --------------------------------------------------------------
#include "TIRCHit.hh"

ClassImp(TIRCHit)

TIRCHit::TIRCHit() : TDetectorVHit(), IRCChannelID() {
}

Int_t TIRCHit::EncodeChannelID() {
  fChannelID = IRCChannelID::EncodeChannelID();
  return fChannelID;
}

void TIRCHit::DecodeChannelID() {
  IRCChannelID::DecodeChannelID(fChannelID);
}

void TIRCHit::Clear(Option_t* option) {
  TDetectorVHit::Clear(option);
  IRCChannelID::Clear(option);
}
