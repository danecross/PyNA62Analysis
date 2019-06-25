// --------------------------------------------------------------
// History:
//
// Created by T Spadaro (tommaso.spadaro@cern.ch) 2015-05-14
//
// --------------------------------------------------------------
#include "TSACDigi.hh"

ClassImp(TSACDigi)

TSACDigi::TSACDigi() : TDCVHit(), SACChannelID() {}

TSACDigi::TSACDigi(TVHit* Hit) : TDCVHit(Hit), SACChannelID() {}

Int_t TSACDigi::EncodeChannelID() {
  fChannelID = SACChannelID::EncodeChannelID();
  return fChannelID;
}

void TSACDigi::DecodeChannelID() {
  Int_t geoChannel = fChannelID%1000;
  SACChannelID::DecodeChannelID(geoChannel);
}

Int_t TSACDigi::Compare(const TObject *obj) const {
  return TDCVHit::Compare(obj);
}

Int_t TSACDigi::GetCorrespondingLowHighChannelId()const {
  return (!GetThresholdType())*1000+(fChannelID%1000);
}

void TSACDigi::Clear(Option_t* option){
  TDCVHit::Clear(option);
  SACChannelID::Clear(option);
}
