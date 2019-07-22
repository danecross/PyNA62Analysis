// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Updated: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-02
//
// --------------------------------------------------------------

#include "TRecoMUV3Hit.hh"

ClassImp(TRecoMUV3Hit)

TRecoMUV3Hit::TRecoMUV3Hit() : TRecoVHit(), MUV3ChannelID() {
  fDetectedEdge = 0;
  fLeadingTime = fTrailingTime = 0.0;
  fLeadingTimeNoT0 = fTrailingTimeNoT0 = fTimeNoT0 = 0.0;
  fROChannelID = 0;
}

void TRecoMUV3Hit::Clear(Option_t* option){
  TRecoVHit::Clear(option);
  MUV3ChannelID::Clear(option);
  fDetectedEdge = 0;
  fLeadingTime = fTrailingTime = 0.0;
  fLeadingTimeNoT0 = fTrailingTimeNoT0 = fTimeNoT0 = 0.0;
  fROChannelID = 0;
}

Int_t TRecoMUV3Hit::EncodeChannelID() {
  fChannelID = MUV3ChannelID::EncodeChannelID();
  return fChannelID;
}

void TRecoMUV3Hit::DecodeChannelID() {
  MUV3ChannelID::DecodeChannelID(fChannelID);
}
