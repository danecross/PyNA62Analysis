// --------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-09-14
//
// --------------------------------------------------------------
#include "THACDigi.hh"

ClassImp(THACDigi)

THACDigi::THACDigi() : TDCVHit(), HACChannelID() {}

THACDigi::THACDigi(TVHit* Hit) : TDCVHit(Hit), HACChannelID() {}

Int_t THACDigi::EncodeChannelID() {
  fChannelID = HACChannelID::EncodeChannelID();
  return fChannelID;
}

void THACDigi::DecodeChannelID() {
  Int_t geoChannel = fChannelID%100;
  HACChannelID::DecodeChannelID(geoChannel);
}

Int_t THACDigi::Compare(const TObject *obj) const {
  return TDCVHit::Compare(obj);
}

void THACDigi::Clear(Option_t* option) {
  TDCVHit::Clear(option);
  HACChannelID::Clear(option);
}
