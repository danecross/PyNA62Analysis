// --------------------------------------------------------------
// History:
//
// Created by T Spadaro (tommaso.spadaro@cern.ch) 2015-05-14
//
// --------------------------------------------------------------
#include "TIRCDigi.hh"

ClassImp(TIRCDigi)

TIRCDigi::TIRCDigi() : TDCVHit(), IRCChannelID() {}

TIRCDigi::TIRCDigi(TVHit* Hit) : TDCVHit(Hit), IRCChannelID() {}

Int_t TIRCDigi::EncodeChannelID() {
  fChannelID = IRCChannelID::EncodeChannelID();
  return fChannelID;
}

void TIRCDigi::DecodeChannelID() {
  Int_t geoChannel = fChannelID%1000;
  IRCChannelID::DecodeChannelID(geoChannel);
}

Int_t TIRCDigi::Compare(const TObject *obj) const {
  return TDCVHit::Compare(obj);
}

Int_t TIRCDigi::GetCorrespondingLowHighChannelId()const {
  return (!GetThresholdType())*1000+(fChannelID%1000);
}

void TIRCDigi::Clear(Option_t* option) {
  TDCVHit::Clear(option);
  IRCChannelID::Clear(option);
}
