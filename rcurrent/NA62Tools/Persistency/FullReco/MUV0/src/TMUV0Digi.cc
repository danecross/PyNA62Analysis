// --------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-07-16
// Updated by E Goudzovski (eg@hep.ph.bham.ac.uk) 2016-11-05
//
// --------------------------------------------------------------

#include "TMUV0Digi.hh"

ClassImp(TMUV0Digi)

TMUV0Digi::TMUV0Digi() : TDCVHit(), MUV0ChannelID() {}

Int_t TMUV0Digi::EncodeChannelID() {
  fChannelID = MUV0ChannelID::EncodeChannelID();
  return fChannelID;
}

void TMUV0Digi::DecodeChannelID() {
  MUV0ChannelID::DecodeChannelID(fChannelID);
}

void TMUV0Digi::Clear(Option_t* option){
  TDCVHit::Clear(option);
  MUV0ChannelID::Clear(option);
}
