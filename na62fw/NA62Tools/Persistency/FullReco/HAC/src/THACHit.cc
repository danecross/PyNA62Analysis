#include "THACHit.hh"

ClassImp(THACHit)

THACHit::THACHit() : TDetectorVHit(), HACChannelID() {
}

Int_t THACHit::EncodeChannelID() {
  fChannelID = HACChannelID::EncodeChannelID();
  return fChannelID;
}

void THACHit::DecodeChannelID() {
  HACChannelID::DecodeChannelID(fChannelID);
}

void THACHit::Clear(Option_t* option) {
  TDetectorVHit::Clear(option);
  HACChannelID::Clear(option);
}
