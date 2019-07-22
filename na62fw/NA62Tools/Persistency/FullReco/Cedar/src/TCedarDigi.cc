//----------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-19
//
//----------------------------------------------------------------

#include "TCedarDigi.hh"

ClassImp(TCedarDigi)

TCedarDigi::TCedarDigi() : TDCVHit(), CedarChannelID() {}

TCedarDigi::TCedarDigi(TVHit* Hit) : TDCVHit(Hit), CedarChannelID() {}

Int_t TCedarDigi::EncodeChannelID() {
  fChannelID = CedarChannelID::EncodeChannelID();
  return fChannelID;
}

void TCedarDigi::DecodeChannelID() {
  CedarChannelID::DecodeChannelID(fChannelID);
}

Int_t TCedarDigi::Compare(const TObject *obj) const {
  return TDCVHit::Compare(obj);
}

void TCedarDigi::Clear(Option_t * option){
  TDCVHit::Clear(option);
  CedarChannelID::Clear(option);
}
