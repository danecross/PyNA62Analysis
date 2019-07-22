#include "TMUV0Hit.hh"

ClassImp(TMUV0Hit)

TMUV0Hit::TMUV0Hit() : TDetectorVHit(), MUV0ChannelID() {
}

Int_t TMUV0Hit::EncodeChannelID() {
  fChannelID = MUV0ChannelID::EncodeChannelID();
  return fChannelID;
}

void TMUV0Hit::DecodeChannelID() {
  MUV0ChannelID::DecodeChannelID(fChannelID);
}

void TMUV0Hit::Clear(Option_t* option){
  TDetectorVHit::Clear(option);
  MUV0ChannelID::Clear(option);
}
